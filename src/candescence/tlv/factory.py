"""
Purpose: Factory class for TLV VAE experiments
Author: Hallett Lab
Date: 2026-01-27

Provides Factory class for configuring and running VAE experiments.
"""

import hashlib
import json
from datetime import datetime
from pathlib import Path
from typing import Any, Callable, Dict, Optional

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
from torch.optim.lr_scheduler import ExponentialLR
from torch.utils.data import DataLoader

matplotlib.use('Agg')

from candescence.core.config import TLVConfig
from candescence.core.logging_config import get_logger
from candescence.tlv.architectures import (
    Tendrils,
    c_VAE,
    cond_uc_VAE,
    cond_uc_VAE_attention,
    cond_uc_VAE_spatial,
    cruc_VAE,
    cruc_VAE_conv,
    crutch_VAE,
    multi_cond_tendril_VAE,
    rc_vae,
    tendril_VAE,
    uc_VAE,
    uc_VAE_multistage,
)
from candescence.tlv.data import FullDataset
from candescence.tlv.losses import (
    get_kl_log_cosh_loss_tendril,
    get_kl_mse_loss_tendril,
)
from candescence.tlv.training import LossLogger, SkipLogger, VAETrainer
from candescence.tlv.utilities import adjust_process_priority

logger = get_logger("candescence.tlv.factory")

# torch.set_num_interop_threads() can only be called once per process,
# before any parallel work starts.  On the second call PyTorch's C++
# runtime throws c10::Error → std::terminate, killing the process.
# This flag ensures we only attempt the call once.
_INTEROP_THREADS_CONFIGURED = False


class Factory:
    """
    Factory class for configuring and running VAE experiments.

    Manages experiment configuration, dataset creation, model training,
    and inference workflows.
    """

    def __init__(self, config: TLVConfig, **kwargs) -> None:
        """
        Initialize Factory with configuration.

        Args:
            config: TLVConfig object with experiment paths.
            **kwargs: Override default arguments.
        """
        self.config = config

        # Architecture mapping
        self.architecture_map = {
            'c_vae': c_VAE,
            'uc_vae': uc_VAE,
            'cond_uc_vae': cond_uc_VAE,
            'cond_uc_vae_attention': cond_uc_VAE_attention,
            'cond_uc_vae_spatial': cond_uc_VAE_spatial,
            'rc_vae': rc_vae,
            'uc_vae_multistage': uc_VAE_multistage,
            'cruc_vae': cruc_VAE,
            'cruc_vae_conv': cruc_VAE_conv,
            'crutch_vae': crutch_VAE,
            'tendril_vae': tendril_VAE,
            'multi_cond_tendril_vae': multi_cond_tendril_VAE,
        }

        # Apply overrides
        for key, value in kwargs.items():
            if hasattr(config, key):
                setattr(config, key, value)
            else:
                setattr(config, key, value)

        # Set device
        if not hasattr(config, 'device'):
            config.device = torch.device(
                "cuda:0" if torch.cuda.is_available() else "cpu"
            )

        # System settings
        process_priority = getattr(config, 'process_priority', 19)
        adjust_process_priority(process_priority)

        training_seed = getattr(config, 'training_seed', 9954)
        torch.manual_seed(training_seed)

        global _INTEROP_THREADS_CONFIGURED
        num_threads = getattr(config, 'num_threads', 10)
        # set_num_threads is safe to call multiple times.
        torch.set_num_threads(num_threads)
        # set_num_interop_threads can only be called ONCE per process.
        # A second call triggers c10::Error at the C++ level which calls
        # std::terminate — this kills the process before Python's
        # except handler can run, so we must prevent the call entirely.
        if not _INTEROP_THREADS_CONFIGURED:
            try:
                torch.set_num_interop_threads(num_threads)
                _INTEROP_THREADS_CONFIGURED = True
            except RuntimeError as e:
                logger.debug(f"Could not set interop thread count: {e}")
                _INTEROP_THREADS_CONFIGURED = True  # don't retry

        # Set conditional dimension
        conditional_vars = getattr(config, 'conditional_variables', None)
        config.cond_dim = len(conditional_vars) if conditional_vars else 0

        # Prepare strategy
        self._prepare_strategy()

        # Instance variables
        self.vae: Optional[torch.nn.Module] = None
        self.dataset: Optional[FullDataset] = None
        self.trainer: Optional[VAETrainer] = None
        self.optimizer_vae: Optional[torch.optim.Optimizer] = None
        self.optimizer_film: Optional[torch.optim.Optimizer] = None
        self.scheduler_vae: Optional[torch.optim.lr_scheduler._LRScheduler] = None
        self.tendrils: Optional[Tendrils] = None

        logger.info(f"Factory initialized for experiment: {config.experiment_name}")

    def load_dataset(self) -> None:
        """Create and load the dataset."""
        self.dataset = FullDataset(self.config)
        self._check_conditional_variables()
        logger.info(f"Dataset loaded with {len(self.dataset)} samples")

    def _get_architecture_args(self) -> Dict[str, Any]:
        """
        Build arguments dictionary for architecture instantiation.

        Architectures still expect the legacy dict format. This method
        converts TLVConfig attributes to the expected dictionary.
        """
        args = self.config.get_arguments_dict()

        # Add additional attributes that architectures expect
        args['_cond_dim'] = getattr(self.config, 'cond_dim', 0)
        args['latent_dim'] = getattr(self.config, 'latent_dim', 128)
        args['intermediate_dim'] = getattr(self.config, 'intermediate_dim', 256)
        args['leaky_relu_slope'] = getattr(self.config, 'leaky_relu_slope', 0.02)
        args['image_dimension'] = getattr(self.config, 'image_dimension', 128)
        args['kernel_size'] = getattr(self.config, 'kernel_size', 3)
        args['strategy'] = getattr(self.config, 'strategy', 0)
        # cond_uc_vae / cond_uc_vae_attention / cond_uc_vae_spatial all
        # access arguments['conditional_variables'] directly (no .get()),
        # so it must be present even when empty.
        args['conditional_variables'] = (
            getattr(self.config, 'conditional_variables', None) or []
        )
        # Strategy 9.6/9.7: SE/spatial attention bottleneck width.
        args['reduction_ratio'] = getattr(self.config, 'reduction_ratio', 16)

        # Strategies 15 & 16: per-variable FiLM cond dims. Hue dim = number of
        # HSV variables selected. Day/media/plate_phys dims = fitted category
        # vocab sizes. plate_phys = physical plate-day identity
        # (plate + media + day) added in Strategy 16 to absorb the ~9%
        # conditional-R² plate batch effect together with the media×day
        # interaction in a single embedding.
        hsv_set = {'average_hue', 'average_saturation', 'average_value'}
        cond_vars = getattr(self.config, 'conditional_variables', []) or []
        args['_cond_dim_hue'] = sum(1 for v in cond_vars if v in hsv_set)
        args['_cond_dim_day'] = len(getattr(self.config, 'day_categories', []) or [])
        args['_cond_dim_media'] = len(getattr(self.config, 'media_categories', []) or [])
        args['_cond_dim_plate_phys'] = len(
            getattr(self.config, 'plate_phys_categories', []) or []
        )

        # Strategy 16 drops the day FiLM pathway. The cond_keys list is
        # persisted on the config (by _prepare_strategy) and on args.json
        # (by _save_arguments) so the model loader can reconstruct the same
        # architecture at inference time.
        cond_keys = getattr(self.config, 'cond_keys', None)
        if cond_keys is not None:
            args['_cond_keys'] = list(cond_keys)

        # Tendril parameters (needed by TendrilTrainer and tendril loss fns)
        args['tendril_batch_size'] = getattr(
            self.config, 'tendril_batch_size', 256
        )
        args['tendril_lr'] = getattr(self.config, 'tendril_lr', 1e-6)
        args['tendril_weight_decay'] = getattr(
            self.config, 'tendril_weight_decay', 1.5e-3
        )
        args['tendril_latent_dim'] = getattr(
            self.config, 'tendril_latent_dim', 128
        )
        args['tendril_num_epochs'] = getattr(
            self.config, 'tendril_num_epochs', 50
        )
        args['tendril_MSE_weight'] = getattr(
            self.config, 'tendril_mse_weight', 1.0
        )
        args['tendril_KL_weight'] = getattr(
            self.config, 'tendril_kl_weight', 1.0
        )
        args['tendril_log_cosh_weight'] = getattr(
            self.config, 'tendril_log_cosh_weight', 1.0
        )
        args['tendril_loss_fn'] = self._resolve_tendril_loss_fn()

        return args

    def _resolve_tendril_loss_fn(self) -> Callable:
        """
        Resolve tendril loss function string to callable.

        Returns
        -------
        Callable
            The tendril loss function.

        Raises
        ------
        ValueError
            If the loss function string is not recognized.
        """
        loss_name = getattr(self.config, 'tendril_loss_function', 'log_cosh')

        tendril_loss_map: Dict[str, Callable] = {
            'mse': get_kl_mse_loss_tendril,
            'log_cosh': get_kl_log_cosh_loss_tendril,
        }

        if loss_name not in tendril_loss_map:
            raise ValueError(
                f"Unknown tendril loss function: '{loss_name}'. "
                f"Available: {list(tendril_loss_map.keys())}"
            )

        logger.info(f"Tendril loss function resolved: {loss_name}")
        return tendril_loss_map[loss_name]

    def prepare_vae(self) -> None:
        """Prepare VAE model and optimizers."""
        architecture = getattr(self.config, 'architecture', 'c_vae')

        if architecture not in self.architecture_map:
            raise ValueError(f"Architecture '{architecture}' not implemented")

        vae_class = self.architecture_map[architecture]
        # Pass arguments dict for backward compatibility with architectures
        args = self._get_architecture_args()
        self.vae = vae_class(args).to(self.config.device)

        # Separate parameters for FiLM and VAE
        film_params = []
        vae_params = []

        for name, param in self.vae.named_parameters():
            if 'film' in name.lower():
                film_params.append(param)
            else:
                vae_params.append(param)

        vae_lr = getattr(self.config, 'vae_lr', 1e-4)
        film_lr = getattr(self.config, 'film_lr', 5e-4)
        # Defensive: getattr returns None when the attribute exists but is
        # set to None (e.g. Streamlit can route an unrendered FiLM LR widget
        # through as None for conditional strategies). torch.optim.Adam
        # validates `0.0 <= lr` and TypeErrors on None.
        if film_lr is None:
            film_lr = 5e-4
        if vae_lr is None:
            vae_lr = 1e-4
        weight_decay = getattr(self.config, 'weight_decay', 1.5e-3)
        if weight_decay is None:
            weight_decay = 1.5e-3

        self.optimizer_vae = torch.optim.Adam(
            vae_params, lr=vae_lr, weight_decay=weight_decay
        )

        if film_params:
            self.optimizer_film = torch.optim.Adam(
                film_params, lr=film_lr, weight_decay=weight_decay
            )
        else:
            self.optimizer_film = None

        self.scheduler_vae = ExponentialLR(self.optimizer_vae, gamma=0.95)

        logger.info(f"VAE prepared: {architecture}")

    def set_training_dataloader(self) -> None:
        """Create training and validation dataloaders."""
        batch_size = getattr(self.config, 'batch_size', 256)

        self.train_dataloader = DataLoader(
            self.dataset.train_dataset,
            batch_size=batch_size,
            shuffle=True
        )
        self.validation_dataloader = DataLoader(
            self.dataset.validation_dataset,
            batch_size=batch_size,
            shuffle=False
        )
        self.test_dataloader = DataLoader(
            self.dataset.test_dataset,
            batch_size=batch_size,
            shuffle=False
        )

        logger.info(f"DataLoaders created with batch_size={batch_size}")

    def train_model(self) -> None:
        """Train the VAE model."""
        if self.vae is None:
            raise RuntimeError("VAE not prepared. Call prepare_vae() first.")

        self.vae.to(self.config.device)

        self.trainer = VAETrainer.train_vae(
            self.vae,
            self.train_dataloader,
            self.validation_dataloader,
            self.optimizer_vae,
            self.optimizer_film,
            self.scheduler_vae,
            self.config
        )

        # Save arguments
        self._save_arguments()

        logger.info("Training complete")

    def load_model(self) -> None:
        """Load trained model weights."""
        model_path = self.config.models_path / "model.pth"
        state = torch.load(model_path, map_location=self.config.device)
        self.vae.load_state_dict(state)
        self.vae.to(self.config.device)
        logger.info(f"Model loaded from {model_path}")

    def _save_arguments(self) -> None:
        """Save serializable arguments to JSON."""
        args_path = self.config.models_path / "args.json"

        serializable = {}
        for key in dir(self.config):
            if key.startswith('_'):
                continue
            value = getattr(self.config, key)
            if callable(value):
                continue
            try:
                json.dumps(value)
                serializable[key] = value
            except (TypeError, OverflowError):
                serializable[key] = str(value)

        with open(args_path, 'w') as f:
            json.dump(serializable, f, indent=2)

        logger.info(f"Arguments saved to {args_path}")

    def _check_conditional_variables(self) -> None:
        """Verify conditional variables exist in dataset."""
        conditional_vars = getattr(self.config, 'conditional_variables', None) or []
        df_columns = self.dataset.target_df.columns

        for var in conditional_vars:
            if var not in df_columns:
                raise ValueError(
                    f"Conditional variable '{var}' not in DataFrame. "
                    f"Available: {list(df_columns)}"
                )

    def _prepare_strategy(self) -> None:
        """Configure strategy-specific settings."""
        strategy = getattr(self.config, 'strategy', 0)

        logger.info(f"Preparing strategy: {strategy}")

        if strategy in (0, 1, 11, 12):
            self.config.conditional_variables = None
            self.config.adjustment_variables = None
            self.config.augmentation_variables = None
            self.config.adjust_images = False
            self.config.augment_images = False

            if strategy == 0:
                self.config.architecture = 'c_vae'
            elif strategy == 1:
                self.config.architecture = 'uc_vae'
            elif strategy == 11:
                self.config.architecture = 'rc_vae'
            elif strategy == 12:
                self.config.architecture = 'uc_vae_multistage'

        elif strategy in (7, 8):
            if not getattr(self.config, 'conditional_variables', None):
                raise ValueError(
                    "Strategy 7/8 requires conditional_variables"
                )
            self.config.architecture = 'cond_uc_vae'

        elif strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16, 17):
            if not getattr(self.config, 'conditional_variables', None):
                raise ValueError(
                    f"Strategy {strategy} requires conditional_variables"
                )

            if strategy in (14, 17):
                # Strategy 17 = Invariant Tendril VAE: identical architecture to
                # Strategy 14; the difference is a VFAE-style invariance penalty
                # applied during stage-2 tendril training (see tendril_sub.py).
                self.config.architecture = 'tendril_vae'
            elif strategy in (15, 16):
                # Strategies 15 & 16: per-variable FiLM on outer Tendril VAE.
                # Strategy 15 stacks FiLM for hue + day + media. Strategy 16
                # drops day as a standalone FiLM key (its main effect on hue
                # is ~0%) and adds plate_phys — the physical plate-day
                # identity (plate + media + day) — to absorb the ~9%
                # conditional-R² plate batch effect plus the small but real
                # media×day interaction (~0.2% of hue variance) in a single
                # embedding. Both strategies require at least one HSV
                # variable in conditional_variables.
                hsv = {'average_hue', 'average_saturation', 'average_value'}
                cond_vars = set(getattr(self.config, 'conditional_variables', []) or [])
                if not (cond_vars & hsv):
                    raise ValueError(
                        f"Strategy {strategy} requires at least one HSV variable "
                        "(average_hue/saturation/value) in conditional_variables"
                    )
                self.config.architecture = 'multi_cond_tendril_vae'
                # Persist which FiLM pathways are active so inference can
                # reconstruct the same architecture from args.json.
                if strategy == 16:
                    self.config.cond_keys = ['hue', 'media', 'plate_phys']
                else:
                    self.config.cond_keys = ['hue', 'day', 'media']
            elif strategy == 9.6:
                # Strategy 9.6: cond-ucVAE with channel-attention
                # squeeze-excitation gates on skip connections (Methods §
                # methods:attention_layers). Used for the adjustment-section
                # nuisance-ablation experiments that were originally trained
                # in candescence_master/.../adjustment_only.ipynb.
                self.config.architecture = 'cond_uc_vae_attention'
            elif strategy == 9.7:
                # Strategy 9.7: cond-ucVAE with spatial-attention gates on
                # skip connections.
                self.config.architecture = 'cond_uc_vae_spatial'
            elif strategy == 13:
                self.config.architecture = 'crutch_vae'
            else:
                self.config.architecture = 'cond_uc_vae'

            # Do not force augment_images off: Streamlit / config may set True to build
            # HSV-jitter partner rows in target_df for decoder conditioning (learning
            # set still uses real rows; see FullDataset._create_splits).

            if getattr(self.config, 'augment_images', False):
                av = getattr(self.config, 'augmentation_variables', None)
                if not av:
                    cond = getattr(self.config, 'conditional_variables', None) or [
                        'average_hue'
                    ]
                    self.config.augmentation_variables = list(cond)
                if getattr(self.config, 'augmentation_spread', None) is None:
                    self.config.augmentation_spread = 0.1

            # Default True when the attribute is unset (e.g. scripts without this key).
            # Streamlit sets an explicit bool for strategies 13/14, so this default does
            # not override the training UI.
            if not hasattr(self.config, 'augment_decoder_images') or \
               getattr(self.config, 'augment_decoder_images', None) is None:
                self.config.augment_decoder_images = True

    def prepare_tendril_vaes(self) -> None:
        """
        Prepare and train tendril VAEs on encoder skip connections.

        This is Stage 2 of Strategy 14. After the primary VAE has been
        trained, this method reads skip connection data collected by
        the SkipLogger, then creates and trains a secondary tendril VAE
        for each encoder layer.

        Raises
        ------
        RuntimeError
            If trainer or skip data is not available.
        """
        if self.trainer is None or self.trainer.skip_logger is None:
            raise RuntimeError(
                "Trainer with skip_logger required. "
                "Call train_model() first."
            )

        train_skips = self.trainer.skip_logger.skips['train']
        validation_skips = self.trainer.skip_logger.skips['validation']

        if not train_skips:
            raise RuntimeError(
                "No skip connection data found. Ensure strategy is 14 "
                "and training completed successfully."
            )

        logger.info("Preparing tendril VAEs...")

        args = self._get_architecture_args()
        self.tendrils = Tendrils(args)

        sorted_keys = sorted(train_skips.keys())

        for key in sorted_keys:
            logger.info(
                f"Adding tendril VAE for layer {key}, "
                f"shape: {train_skips[key].shape}"
            )

            self.tendrils.add_tendril(
                key,
                train_dataset=train_skips[key],
                validation_dataset=validation_skips[key]
            )

            logger.info(f"Training tendril VAE for layer {key}...")
            self.tendrils.train_tendril(key)

        logger.info(
            f"All tendril VAEs trained. Layers: {sorted_keys}"
        )

    def run_experiment(self) -> None:
        """
        Run complete experiment pipeline.

        For strategy 14 (tendril_vae), this includes Stage 2:
        collecting skip connections and training tendril VAEs.
        """
        logger.info("Starting experiment pipeline")

        self.load_dataset()
        self.prepare_vae()
        self.set_training_dataloader()
        self.train_model()

        strategy = getattr(self.config, 'strategy', 0)
        if strategy == 14:
            self.prepare_tendril_vaes()

        logger.info("Experiment complete")

    def inference_with_learning_set(self, output_filename: str = 'default') -> Any:
        """
        Run inference on the learning set.

        Args:
            output_filename: Output filename for results.

        Returns:
            Inference object.
        """
        if not hasattr(self, 'trainer') or self.trainer is None:
            self._init_inference_trainer()

        file_path = self.config.analyses_path / f'{output_filename}.pkl'
        self.trainer.add_inference_object(self.dataset.target_df, 'learning')
        infer = self.trainer.get_inference_object('learning')
        infer.save()
        return infer

    def _init_inference_trainer(self) -> None:
        """Initialize trainer for inference only."""
        if not hasattr(self, 'dataset') or self.dataset is None:
            self.load_dataset()

        train_dl = getattr(self, 'train_dataloader', None)
        val_dl = getattr(self, 'validation_dataloader', None)

        if val_dl is None:
            val_dl = DataLoader(
                self.dataset,
                batch_size=1,
                num_workers=0,
                shuffle=False
            )

        self.trainer = VAETrainer(
            self.vae,
            train_dl,
            val_dl,
            None,
            None,
            None,
            self.config
        )

        logger.info("Trainer initialized for inference")

    def save_tendrils(self, tendrils: Any) -> None:
        """
        Save trained tendrils to the models directory.

        Args:
            tendrils: A Tendrils instance with trained tendril models.
        """
        tendrils_path = self.config.models_path / "tendrils"
        tendrils.save(tendrils_path)
        logger.info(f"Tendrils saved to {tendrils_path}")

    def get_config_summary(self) -> Dict[str, Any]:
        """
        Get summary of experiment configuration.

        Returns:
            Dictionary of configuration values.
        """
        return {
            'experiment_name': self.config.experiment_name,
            'save_name': self.config.save_name,
            'strategy': getattr(self.config, 'strategy', None),
            'architecture': getattr(self.config, 'architecture', None),
            'latent_dim': getattr(self.config, 'latent_dim', None),
            'batch_size': getattr(self.config, 'batch_size', None),
            'number_epochs': getattr(self.config, 'number_epochs', None),
            'device': str(self.config.device),
        }
