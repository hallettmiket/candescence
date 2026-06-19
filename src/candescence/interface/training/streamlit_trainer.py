"""
Purpose: Streamlit-compatible VAE trainer with callback-based progress reporting
Author: Hallett Lab
Date: 2026-02-01

Provides StreamlitTrainer that wraps VAETrainer functionality with callbacks
for real-time progress updates in Streamlit.
"""

import gc
import io
import time
from datetime import datetime
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Tuple, Union

import lpips
import matplotlib
matplotlib.use('Agg')
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from torch.utils.data import DataLoader

from candescence.core.logging_config import get_logger
from candescence.tlv.data.dataset import FullDataset
from candescence.tlv.losses import (
    SSIMLoss,
    get_kl_recon_cond_loss,
    get_kl_recon_loss,
    get_kl_mse_loss_tendril,
    get_kl_log_cosh_loss_tendril,
)
from candescence.tlv.utilities import convert_rgb_transformed_2_rgb

logger = get_logger("candescence.interface.training.streamlit_trainer")


def _batchify_cond(cond):
    """Add a leading batch dim to a 1-D tensor, or each tensor in a dict."""
    if isinstance(cond, dict):
        return {k: (v.unsqueeze(0) if v.dim() == 1 else v) for k, v in cond.items()}
    return cond.unsqueeze(0) if cond.dim() == 1 else cond


def _serialize_cond(cond):
    """Convert a tensor or dict-of-tensors cond to JSON-serializable lists."""
    if cond is None:
        return None
    if isinstance(cond, dict):
        return {k: v.detach().cpu().numpy().tolist() for k, v in cond.items()}
    return cond.detach().cpu().numpy().tolist()


class _NaNLossError(RuntimeError):
    """Raised when training loss becomes NaN/inf, signaling numerical divergence."""
    pass


class StreamlitTrainer:
    """
    VAE trainer adapted for Streamlit with callback-based progress reporting.

    Wraps VAETrainer functionality but adds:
    - Progress callbacks after each epoch
    - Reconstruction callbacks for visualization
    - Stop/pause checking for user control
    - Returns training summary dictionary

    Attributes:
        factory: Factory instance with prepared VAE and dataloaders
        config: TLVConfig with training parameters
        progress_callback: Called after each epoch with metrics dict
        reconstruction_callback: Called periodically with reconstruction images
        stop_check: Returns True if training should stop
        pause_check: Returns True if training should pause
    """

    def __init__(
        self,
        factory: Any,
        progress_callback: Optional[Callable[[Dict[str, Any]], None]] = None,
        reconstruction_callback: Optional[Callable[[np.ndarray, int], None]] = None,
        stop_check: Optional[Callable[[], bool]] = None,
        pause_check: Optional[Callable[[], bool]] = None,
        tendril_progress_callback: Optional[Callable[[Dict[str, Any]], None]] = None,
    ) -> None:
        """
        Initialize StreamlitTrainer.

        Args:
            factory: Factory instance with loaded dataset and prepared VAE
            progress_callback: Called with metrics dict after each epoch
            reconstruction_callback: Called with reconstruction images periodically
            stop_check: Callable returning True if training should stop
            pause_check: Callable returning True if training should pause
            tendril_progress_callback: Called with metrics dict after each tendril epoch
        """
        self.factory = factory
        self.config = factory.config
        self.vae = factory.vae
        self.train_dataloader = factory.train_dataloader
        self.validation_dataloader = factory.validation_dataloader
        self.optimizer_vae = factory.optimizer_vae
        self.optimizer_film = factory.optimizer_film
        self.scheduler = factory.scheduler_vae

        # Callbacks
        self.progress_callback = progress_callback or (lambda x: None)
        self.reconstruction_callback = reconstruction_callback or (lambda x, y: None)
        self.stop_check = stop_check or (lambda: False)
        self.pause_check = pause_check or (lambda: False)
        self.tendril_progress_callback = tendril_progress_callback or (lambda x: None)

        # Setup device
        self.device = torch.device(
            self.config.device if torch.cuda.is_available() else "cpu"
        )
        self.vae.to(self.device)

        # Setup loss function
        self._setup_loss_function()
        self._setup_loss_model()

        # Fixed indices for reconstruction visualization
        self._fixed_indices: Optional[np.ndarray] = None
        # Single fixed index for the spectrum figure (so it shows the same
        # image across reporting epochs and progress is visible).
        self._fixed_spectrum_index: Optional[int] = None

        # Estimate function for conditional loss
        self.estimate_fn = FullDataset.estimate_background_image_tensor_helper

        logger.info(f"StreamlitTrainer initialized on {self.device}")

    def _setup_loss_function(self) -> None:
        """Setup loss function based on architecture."""
        architecture = getattr(self.config, 'architecture', 'c_vae')

        if architecture in (
            'cond_uc_vae', 'cond_uc_vae_attention', 'cond_uc_vae_spatial',
            'tendril_vae', 'multi_cond_tendril_vae',
        ):
            self.loss_fn = get_kl_recon_cond_loss
        else:
            self.loss_fn = get_kl_recon_loss

    def _setup_loss_model(self) -> None:
        """Setup perceptual loss model if needed."""
        loss_function = getattr(self.config, 'vae_loss_function', 'mse')

        if loss_function == 'lpips':
            self.loss_model = lpips.LPIPS(net='vgg').to(self.device)
        elif loss_function == 'ssim':
            self.loss_model = SSIMLoss().to(self.device)
        else:
            self.loss_model = None

    def _get_arguments_dict(self) -> Dict[str, Any]:
        """Get arguments dictionary for loss functions."""
        return {
            'strategy': getattr(self.config, 'strategy', 0),
            'architecture': getattr(self.config, 'architecture', 'c_vae'),
            'vae_loss_function': getattr(self.config, 'vae_loss_function', 'mse'),
            'kl_weight': getattr(self.config, 'kl_weight', 1.0),
            'MSE_weight': getattr(self.config, 'mse_weight', 1.0),
            'LPIPS_weight': getattr(self.config, 'lpips_weight', 1.0),
            'SSIM_weight': getattr(self.config, 'ssim_weight', 1.0),
            'conditional_loss_weight': getattr(self.config, 'conditional_loss_weight', 1.0),
            'cycle_length': getattr(self.config, 'cycle_length', 10),
            'conditional_variables': getattr(self.config, 'conditional_variables', []),
            'image_dimension': getattr(self.config, 'image_dimension', 128),
            'grayscale': getattr(self.config, 'grayscale', False),
            'grayscale_background_normalize': getattr(self.config, 'grayscale_background_normalize', False),
            'grayscale_bg_target': getattr(self.config, 'grayscale_bg_target', 0.5),
            'grayscale_bg_border': int(getattr(self.config, 'grayscale_bg_border', 12)),
        }

    def _get_tendril_arguments_dict(self) -> Dict[str, Any]:
        """Get arguments dictionary for tendril training."""
        loss_fn_name = getattr(self.config, 'tendril_loss_fn', 'MSE')
        if loss_fn_name == 'Log-Cosh':
            tendril_loss_fn = get_kl_log_cosh_loss_tendril
        else:
            tendril_loss_fn = get_kl_mse_loss_tendril

        recon_weight = getattr(self.config, 'tendril_recon_weight', 1.0)

        # Strategy 16 conditions the inner tendrils on both media and
        # plate_phys to absorb media-level and plate-day-level batch
        # effects that visibly cluster in the mini-scatters when tendrils
        # are unconditioned. plate_phys = plate:media:day, so this also
        # absorbs the small media×day interaction. Order matches the outer
        # VAE's (hue, media, plate_phys) stack for consistency across
        # scopes.
        strategy = getattr(self.config, 'strategy', 0)
        tendril_cond_keys: Tuple[str, ...] = ()
        tendril_cond_dims: Dict[str, int] = {}
        if strategy == 16:
            media_cats = list(
                getattr(self.config, 'media_categories', []) or []
            )
            plate_phys_cats = list(
                getattr(self.config, 'plate_phys_categories', []) or []
            )
            keys: List[str] = []
            dims: Dict[str, int] = {}
            if media_cats:
                keys.append('media')
                dims['media'] = len(media_cats)
            if plate_phys_cats:
                keys.append('plate_phys')
                dims['plate_phys'] = len(plate_phys_cats)
            tendril_cond_keys = tuple(keys)
            tendril_cond_dims = dims

        return {
            'DEVICE': self.device,
            'MODELS': str(self.config.models_path),
            'tendril_batch_size': getattr(self.config, 'tendril_batch_size', 256),
            'tendril_lr': getattr(self.config, 'tendril_lr', 1e-4),
            'tendril_weight_decay': getattr(self.config, 'tendril_weight_decay', 1.5e-3),
            'tendril_latent_dim': getattr(self.config, 'tendril_latent_dim', 128),
            'tendril_num_epochs': getattr(self.config, 'tendril_num_epochs', 50),
            'tendril_loss_fn': tendril_loss_fn,
            'tendril_MSE_weight': recon_weight,
            'tendril_log_cosh_weight': recon_weight,
            'tendril_KL_weight': getattr(self.config, 'tendril_kl_weight', 1.0),
            'tendril_cond_keys': tendril_cond_keys,
            'tendril_cond_dims': tendril_cond_dims,
            # VFAE-style invariance penalty (Strategy 17). 0 weights → no penalty.
            'tendril_invariance_plate_weight': getattr(self.config, 'tendril_invariance_plate_weight', 0.0),
            'tendril_invariance_hsv_weight': getattr(self.config, 'tendril_invariance_hsv_weight', 0.0),
            'tendril_invariance_pheno_weight': getattr(self.config, 'tendril_invariance_pheno_weight', 0.0),
            'tendril_invariance_kernel': getattr(self.config, 'tendril_invariance_kernel', 'hsic'),
        }

    def _prepare_batch_inputs(
        self,
        batch: Any,
        which: str = 'train'
    ) -> Tuple[torch.Tensor, Optional[torch.Tensor], Optional[torch.Tensor],
               Optional[torch.Tensor], Optional[torch.Tensor]]:
        """Prepare batch inputs for forward pass."""
        encoder_cond = None
        decoder_cond = None
        cond = None
        indices = None

        if isinstance(batch, (tuple, list)):
            if len(batch) == 4:
                x, encoder_cond, decoder_cond, indices = batch
            elif len(batch) == 3:
                x, cond, indices = batch
            elif len(batch) == 2:
                x, indices = batch
            else:
                x = batch[0] if len(batch) == 1 else batch
        else:
            x = batch

        # Strategies 13-16: the dataset already produces the correct
        # decoder_cond — on augmented rows it's the parent's hue (denoising
        # target), on non-augmented rows it equals encoder_cond. No
        # NaN-sentinel lookup is needed here.
        return x, encoder_cond, decoder_cond, cond, indices

    def _decide_image_for_loss_comparison(
        self,
        x: torch.Tensor,
        indices_into_df: torch.Tensor,
        which: str = 'train',
        which_image: str = 'transformed_image',
    ) -> torch.Tensor:
        """Pick the image to compare ``x_hat`` against.

        For strategies 13-16 with ``augment_images=True``, the encoder
        input is an augmented image and the loss target is its real parent
        (denoising direction — pushes the latent toward HSV invariance).
        Otherwise the target is just ``x``.
        """
        strategy = getattr(self.config, 'strategy', 0)
        augment_images = getattr(self.config, 'augment_images', False)

        if strategy not in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16, 17) or not augment_images:
            return x.to(self.device)

        if which == 'train':
            learner = self.train_dataloader.dataset.meta_df_subset.iloc[indices_into_df]
            target_df = self.train_dataloader.dataset.target_df
        else:
            learner = self.validation_dataloader.dataset.meta_df_subset.iloc[indices_into_df]
            target_df = self.validation_dataloader.dataset.target_df

        if isinstance(learner, pd.Series):
            learner = learner.to_frame().T

        target_dict = target_df.set_index('id').to_dict('index')
        new_images = []
        for _, learner_row in learner.iterrows():
            parent_id = learner_row['parent_id']
            if pd.isnull(parent_id):
                raise ValueError(
                    f"Expected augmented row, but parent_id is null for id={learner_row['id']}"
                )
            parent_row = target_dict.get(parent_id, None)
            if parent_row is None:
                raise ValueError(f"Parent image not found for parent_id={parent_id}")
            parent_img = parent_row[which_image]
            if not isinstance(parent_img, torch.Tensor):
                parent_img = torch.tensor(parent_img)
            new_images.append(parent_img.to(self.device))

        return torch.stack(new_images)

    def _move_cond_to_device(
        self,
        cond: Union[torch.Tensor, Dict[str, torch.Tensor]],
    ) -> Union[torch.Tensor, Dict[str, torch.Tensor]]:
        """Move a tensor or dict-of-tensors cond onto ``self.device`` as float."""
        if isinstance(cond, dict):
            return {k: v.to(self.device).float() for k, v in cond.items()}
        return cond.to(self.device).float()

    def _forward_vae_with_conditions(
        self,
        x: torch.Tensor,
        encoder_cond: Optional[Union[torch.Tensor, Dict[str, torch.Tensor]]] = None,
        decoder_cond: Optional[Union[torch.Tensor, Dict[str, torch.Tensor]]] = None,
        cond: Optional[torch.Tensor] = None
    ) -> Tuple:
        """Forward pass through VAE with conditions (tensor or dict)."""
        x = x.to(self.device).float()

        if encoder_cond is not None:
            encoder_cond = self._move_cond_to_device(encoder_cond)
            decoder_cond = self._move_cond_to_device(decoder_cond)
            return self.vae(x, [encoder_cond, decoder_cond])
        elif cond is not None:
            cond = self._move_cond_to_device(cond)
            return self.vae(x, cond)
        else:
            return self.vae(x)

    def _training_epoch(self, epoch: int) -> Tuple[float, float, float, float]:
        """
        Execute one training epoch.

        Returns:
            Tuple of (avg_loss, avg_kl, avg_recon, avg_cond)
        """
        self.vae.train()
        total_loss = 0.0
        total_kl = 0.0
        total_recon = 0.0
        total_cond = 0.0

        architecture = getattr(self.config, 'architecture', 'c_vae')

        for batch in self.train_dataloader:
            x, encoder_cond, decoder_cond, cond, indices = self._prepare_batch_inputs(
                batch, which='train'
            )

            if isinstance(x, list):
                x = torch.stack(x)

            x = x.to(self.device).float()
            self.optimizer_vae.zero_grad()
            if self.optimizer_film is not None:
                self.optimizer_film.zero_grad()

            # Forward pass
            if architecture in ('tendril_vae', 'multi_cond_tendril_vae'):
                x_hat, z, mu, logvar, skip_connections = self._forward_vae_with_conditions(
                    x, encoder_cond, decoder_cond, cond
                )
            else:
                x_hat, z, mu, logvar = self._forward_vae_with_conditions(
                    x, encoder_cond, decoder_cond, cond
                )

            x_comparison = self._decide_image_for_loss_comparison(
                x, indices_into_df=indices, which='train'
            )

            # Compute loss
            if architecture in (
                'cond_uc_vae', 'cond_uc_vae_attention', 'cond_uc_vae_spatial',
                'tendril_vae', 'multi_cond_tendril_vae',
            ):
                loss, kl_loss, recon_loss, cond_loss = self.loss_fn(
                    x_comparison, x_hat, z, mu, logvar,
                    self._get_arguments_dict(), epoch, self.loss_model, self.estimate_fn
                )
            else:
                loss, kl_loss, recon_loss = self.loss_fn(
                    x_comparison, x_hat, z, mu, logvar,
                    self._get_arguments_dict(), epoch, self.loss_model
                )
                cond_loss = torch.tensor(0.0)

            # NaN guard: stop training cleanly if loss becomes NaN/inf
            if torch.isnan(loss) or torch.isinf(loss):
                nan_sources = []
                if torch.isnan(kl_loss) or torch.isinf(kl_loss):
                    nan_sources.append("kl_loss")
                if torch.isnan(recon_loss) or torch.isinf(recon_loss):
                    nan_sources.append("recon_loss")
                if hasattr(cond_loss, 'item') and (torch.isnan(cond_loss) or torch.isinf(cond_loss)):
                    nan_sources.append("cond_loss")
                if torch.isnan(x_hat).any():
                    nan_sources.append("x_hat")
                if torch.isnan(mu).any():
                    nan_sources.append("mu")
                lv = logvar if not isinstance(logvar, list) else logvar[0]
                if torch.isnan(lv).any():
                    nan_sources.append("logvar")
                logger.error(
                    f"NaN/inf detected in loss at epoch {epoch}. "
                    f"Sources: {nan_sources or ['unknown']}. Stopping training."
                )
                raise _NaNLossError(
                    f"Training diverged at epoch {epoch}: loss became NaN/inf "
                    f"(sources: {', '.join(nan_sources) or 'unknown'})"
                )

            loss.backward()

            # Gradient clipping to prevent explosive updates
            nn.utils.clip_grad_norm_(self.vae.parameters(), max_norm=1.0)

            if self.optimizer_film is not None:
                self.optimizer_film.step()
            self.optimizer_vae.step()

            total_loss += loss.item()
            total_kl += kl_loss.item()
            total_recon += recon_loss.item()
            total_cond += cond_loss.item() if hasattr(cond_loss, 'item') else cond_loss

        num_batches = len(self.train_dataloader)
        return (
            total_loss / num_batches,
            total_kl / num_batches,
            total_recon / num_batches,
            total_cond / num_batches
        )

    def _validation_epoch(self, epoch: int) -> Tuple[float, float, float, float]:
        """
        Execute one validation epoch.

        Returns:
            Tuple of (avg_loss, avg_kl, avg_recon, avg_cond)
        """
        self.vae.eval()
        total_loss = 0.0
        total_kl = 0.0
        total_recon = 0.0
        total_cond = 0.0

        architecture = getattr(self.config, 'architecture', 'c_vae')

        with torch.no_grad():
            for batch in self.validation_dataloader:
                x, encoder_cond, decoder_cond, cond, indices = self._prepare_batch_inputs(
                    batch, which='validation'
                )

                if isinstance(x, list):
                    x = torch.stack(x)

                x = x.to(self.device).float()

                # Forward pass
                if architecture in ('tendril_vae', 'multi_cond_tendril_vae'):
                    x_hat, z, mu, logvar, skip_connections = self._forward_vae_with_conditions(
                        x, encoder_cond, decoder_cond, cond
                    )
                else:
                    x_hat, z, mu, logvar = self._forward_vae_with_conditions(
                        x, encoder_cond, decoder_cond, cond
                    )

                x_comparison = self._decide_image_for_loss_comparison(
                    x, indices_into_df=indices, which='validation'
                )

                # Compute loss
                if architecture in (
                    'cond_uc_vae', 'cond_uc_vae_attention', 'cond_uc_vae_spatial',
                    'tendril_vae', 'multi_cond_tendril_vae',
                ):
                    loss, kl_loss, recon_loss, cond_loss = self.loss_fn(
                        x_comparison, x_hat, z, mu, logvar,
                        self._get_arguments_dict(), epoch, self.loss_model, self.estimate_fn
                    )
                else:
                    loss, kl_loss, recon_loss = self.loss_fn(
                        x_comparison, x_hat, z, mu, logvar,
                        self._get_arguments_dict(), epoch, self.loss_model
                    )
                    cond_loss = torch.tensor(0.0)

                total_loss += loss.item()
                total_kl += kl_loss.item()
                total_recon += recon_loss.item()
                total_cond += cond_loss.item() if hasattr(cond_loss, 'item') else cond_loss

        num_batches = len(self.validation_dataloader)
        return (
            total_loss / num_batches,
            total_kl / num_batches,
            total_recon / num_batches,
            total_cond / num_batches
        )

    def select_fixed_reconstruction_indices(self, n_images: int = 8) -> List[int]:
        """
        Select fixed indices for reconstruction visualization.

        These indices are used throughout training to show consistent
        reconstruction improvement.

        Args:
            n_images: Number of images to select

        Returns:
            List of indices into validation dataset
        """
        np.random.seed(42)  # Reproducible selection
        dataset_size = len(self.validation_dataloader.dataset)
        n_images = min(n_images, dataset_size)
        indices = np.random.choice(dataset_size, size=n_images, replace=False)
        self._fixed_indices = indices
        return indices.tolist()

    def generate_reconstructions(self, include_hsv: bool = True) -> Dict[str, Any]:
        """
        Generate reconstruction images for fixed indices with HSV metadata.

        Args:
            include_hsv: Whether to compute and include HSV values

        Returns:
            Dictionary with:
            - 'images': Array of shape (2, n_images, H, W, C) with originals and reconstructions
            - 'original_hsv': List of dicts with H, S, V for each original (0-1 scale)
            - 'reconstructed_hsv': List of dicts with H, S, V for each reconstruction (0-1 scale)
            - 'encoder_cond': List of encoder conditioning values used (0-1 scale)
            - 'decoder_cond': List of decoder conditioning values used (0-1 scale)
        """
        if self._fixed_indices is None:
            self.select_fixed_reconstruction_indices()

        self.vae.eval()
        originals = []
        reconstructions = []
        original_hsv_list = []
        reconstructed_hsv_list = []
        encoder_cond_list = []
        decoder_cond_list = []

        architecture = getattr(self.config, 'architecture', 'c_vae')

        with torch.no_grad():
            for idx in self._fixed_indices:
                item = self.validation_dataloader.dataset[idx]

                # Get image based on dataset format. The dataset already
                # returns the correct decoder_cond (parent's hue on augmented
                # rows, encoder_cond on real rows) — no sentinel handling here.
                if len(item) == 4:
                    img, encoder_cond, decoder_cond, _ = item
                elif len(item) == 3:
                    img, cond, _ = item
                    encoder_cond = cond
                    decoder_cond = cond
                else:
                    img = item[0]
                    encoder_cond = None
                    decoder_cond = None

                # Store conditioning values (model scale 0-1). For dict
                # conds (Strategy 15) serialize each variable separately.
                if encoder_cond is not None:
                    encoder_cond_list.append(_serialize_cond(encoder_cond))
                    decoder_cond_list.append(_serialize_cond(decoder_cond))
                else:
                    encoder_cond_list.append(None)
                    decoder_cond_list.append(None)

                img = img.unsqueeze(0).to(self.device).float()

                if encoder_cond is not None:
                    encoder_cond = _batchify_cond(encoder_cond)
                    encoder_cond = self._move_cond_to_device(encoder_cond)
                if decoder_cond is not None:
                    decoder_cond = _batchify_cond(decoder_cond)
                    decoder_cond = self._move_cond_to_device(decoder_cond)

                # Forward pass
                if encoder_cond is not None:
                    rec_img, *_ = self.vae(img, [encoder_cond, decoder_cond])
                else:
                    rec_img, *_ = self.vae(img)

                # Convert to displayable format
                orig = convert_rgb_transformed_2_rgb(img.cpu().squeeze())
                rec = convert_rgb_transformed_2_rgb(rec_img.cpu().squeeze())

                originals.append(orig)
                reconstructions.append(rec)

                # Compute HSV values from images
                if include_hsv:
                    orig_hsv = self._compute_image_hsv(orig)
                    rec_hsv = self._compute_image_hsv(rec)
                    original_hsv_list.append(orig_hsv)
                    reconstructed_hsv_list.append(rec_hsv)

        # Stack into array: (2, n_images, H, W, C)
        originals_arr = np.stack(originals, axis=0)
        reconstructions_arr = np.stack(reconstructions, axis=0)
        images = np.stack([originals_arr, reconstructions_arr], axis=0)

        result = {
            'images': images,
            'original_hsv': original_hsv_list,
            'reconstructed_hsv': reconstructed_hsv_list,
            'encoder_cond': encoder_cond_list,
            'decoder_cond': decoder_cond_list,
        }

        # When grayscale is active, reload the original color images from disk
        if getattr(self.config, 'grayscale', False):
            from PIL import Image
            color_originals = []
            dataset = self.validation_dataloader.dataset
            image_dim = getattr(self.config, 'image_dimension', 128)
            raw_path = getattr(self.config, 'raw_images_path', None)
            for idx in self._fixed_indices:
                try:
                    row = dataset.meta_df_subset.iloc[idx]
                    file_path = Path(raw_path) / row['filename']
                    img = Image.open(file_path).convert('RGB')
                    img = img.resize((image_dim, image_dim), Image.LANCZOS)
                    color_originals.append(np.array(img))
                except Exception:
                    color_originals.append(originals_arr[len(color_originals)])
            result['color_originals'] = np.stack(color_originals, axis=0)

        return result

    def generate_fixed_cond_reconstructions(
        self,
        fixed_cond: torch.Tensor,
        include_hsv: bool = True
    ) -> Dict[str, Any]:
        """
        Generate reconstruction images using fixed decoder conditioning.

        This allows visualizing how well the model translates images to a
        target hue/saturation/value during training.

        Args:
            fixed_cond: Fixed conditioning tensor for decoder (shape: [1, cond_dim])
            include_hsv: Whether to compute and include HSV values

        Returns:
            Dictionary with:
            - 'images': Array of shape (2, n_images, H, W, C) with originals and reconstructions
            - 'original_hsv': List of dicts with H, S, V for each original (0-1 scale)
            - 'reconstructed_hsv': List of dicts with H, S, V for each reconstruction (0-1 scale)
            - 'encoder_cond': List of encoder conditioning values used (0-1 scale)
            - 'decoder_cond': List of fixed decoder conditioning values (0-1 scale)
            - 'target_cond': The fixed conditioning values used for all images
        """
        if self._fixed_indices is None:
            self.select_fixed_reconstruction_indices()

        self.vae.eval()
        originals = []
        reconstructions = []
        original_hsv_list = []
        reconstructed_hsv_list = []
        encoder_cond_list = []
        decoder_cond_list = []

        # Prepare fixed conditioning tensor
        if fixed_cond.dim() == 1:
            fixed_cond = fixed_cond.unsqueeze(0)
        fixed_cond = fixed_cond.to(self.device).float()

        with torch.no_grad():
            for idx in self._fixed_indices:
                item = self.validation_dataloader.dataset[idx]

                # Get image based on dataset format
                if len(item) == 4:
                    img, encoder_cond, _, _ = item
                elif len(item) == 3:
                    img, encoder_cond, _ = item
                else:
                    img = item[0]
                    encoder_cond = None

                # Store conditioning values (serializer handles dict and tensor).
                if encoder_cond is not None:
                    encoder_cond_list.append(_serialize_cond(encoder_cond))
                else:
                    encoder_cond_list.append(None)
                decoder_cond_list.append(fixed_cond.cpu().numpy().tolist()[0])

                img = img.unsqueeze(0).to(self.device).float()

                if encoder_cond is not None:
                    encoder_cond = _batchify_cond(encoder_cond)
                    encoder_cond = self._move_cond_to_device(encoder_cond)

                    if isinstance(encoder_cond, dict):
                        # Strategy 15: fixed_cond is an HSV tensor covering only
                        # the 'hue' entry. Keep per-sample day/media for the
                        # decoder side so identity info doesn't change.
                        decoder_cond_batch = {
                            **encoder_cond,
                            'hue': fixed_cond,
                        }
                        rec_img, *_ = self.vae(img, [encoder_cond, decoder_cond_batch])
                    else:
                        rec_img, *_ = self.vae(img, [encoder_cond, fixed_cond])
                else:
                    rec_img, *_ = self.vae(img)

                # Convert to displayable format
                orig = convert_rgb_transformed_2_rgb(img.cpu().squeeze())
                rec = convert_rgb_transformed_2_rgb(rec_img.cpu().squeeze())

                originals.append(orig)
                reconstructions.append(rec)

                # Compute HSV values from images
                if include_hsv:
                    orig_hsv = self._compute_image_hsv(orig)
                    rec_hsv = self._compute_image_hsv(rec)
                    original_hsv_list.append(orig_hsv)
                    reconstructed_hsv_list.append(rec_hsv)

        # Stack into array: (2, n_images, H, W, C)
        originals_arr = np.stack(originals, axis=0)
        reconstructions_arr = np.stack(reconstructions, axis=0)
        images = np.stack([originals_arr, reconstructions_arr], axis=0)

        return {
            'images': images,
            'original_hsv': original_hsv_list,
            'reconstructed_hsv': reconstructed_hsv_list,
            'encoder_cond': encoder_cond_list,
            'decoder_cond': decoder_cond_list,
            'target_cond': fixed_cond.cpu().numpy().tolist()[0],
        }

    def generate_spectrum_figure(self, k: int = 2) -> Optional[Dict[str, Any]]:
        """Build the FiLM-spectrum diagnostic figure as PNG bytes.

        Holds the encoder's reparameterization noise fixed (RNG-state replay)
        and decodes ``k`` swept values per conditional channel. Returns:

          - ``'png_bytes'``: PNG image bytes (for st.image / st.download_button)
          - ``'index'``: validation index used
          - ``'epoch'``: caller fills this in
          - ``'k'``: number of swept values per channel

        Returns ``None`` for non-conditional strategies (nothing to vary).
        """
        strategy = getattr(self.config, 'strategy', 0)
        if strategy not in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16, 17):
            return None

        if self._fixed_spectrum_index is None:
            self._fixed_spectrum_index = int(np.random.choice(
                len(self.validation_dataloader.dataset), 1
            )[0])
        idx = self._fixed_spectrum_index

        item = self.validation_dataloader.dataset[idx]
        if len(item) != 4:
            return None
        img, encoder_cond, decoder_cond, _ = item

        img = img.unsqueeze(0).to(self.device).float()
        encoder_cond = self._move_cond_to_device(_batchify_cond(encoder_cond))
        decoder_cond = self._move_cond_to_device(_batchify_cond(decoder_cond))

        conditional_vars = list(getattr(self.config, 'conditional_variables', []))
        new_values = [v / (k + 1) for v in range(1, k + 1)]

        self.vae.eval()
        with torch.no_grad():
            rng_state = torch.get_rng_state()
            cuda_rng_state = (
                torch.cuda.get_rng_state(self.device) if torch.cuda.is_available() else None
            )

            def _forward(d_cond):
                torch.set_rng_state(rng_state)
                if cuda_rng_state is not None:
                    torch.cuda.set_rng_state(cuda_rng_state, self.device)
                out = self.vae(img, [encoder_cond, d_cond])
                return out[0]

            rec_img = _forward(decoder_cond)

            varied: Dict[str, List[Tuple[float, torch.Tensor]]] = {
                'average_hue': [], 'average_saturation': [], 'average_value': [],
            }
            for var_name in ('average_hue', 'average_saturation', 'average_value'):
                if var_name not in conditional_vars:
                    continue
                pos = conditional_vars.index(var_name)
                for new_val in new_values:
                    d_var = self._override_hue_channel(decoder_cond, pos, new_val)
                    varied[var_name].append((new_val, _forward(d_var)))

            indices_t = torch.tensor([idx])
            x_target = self._decide_image_for_loss_comparison(
                img, indices_into_df=indices_t, which='validation',
                which_image='transformed_image',
            ).to(self.device).float()

        png_bytes = self._render_spectrum_png(
            img, rec_img, varied, x_target,
            encoder_cond, decoder_cond, conditional_vars, k,
        )
        return {'png_bytes': png_bytes, 'index': idx, 'k': k}

    @staticmethod
    def _override_hue_channel(d_cond, pos: int, new_val: float):
        """Return a copy of ``d_cond`` with the ``pos``-th hue channel set to ``new_val``.

        For dict conds (Strategy 15/16), only the ``'hue'`` entry is modified.
        """
        if isinstance(d_cond, dict):
            new_hue = d_cond['hue'].clone()
            new_hue[..., pos] = new_val
            return {**d_cond, 'hue': new_hue}
        out = d_cond.clone()
        out[..., pos] = new_val
        return out

    def _render_spectrum_png(
        self, img, rec_img, varied, x_target,
        encoder_cond, decoder_cond, conditional_vars, k,
    ) -> bytes:
        """Render the 5-row spectrum figure and return its PNG bytes."""
        def _hue_vec(c):
            return c['hue'].squeeze(0) if isinstance(c, dict) else c.squeeze(0)

        enc_hue = _hue_vec(encoder_cond).detach().cpu().numpy()
        dec_hue = _hue_vec(decoder_cond).detach().cpu().numpy()

        fig = plt.figure(figsize=(8, 9))
        outer = gridspec.GridSpec(5, 1, hspace=0.55)

        label_kw = dict(
            fontsize=8, color='black', ha='center', va='top',
            bbox=dict(facecolor='white', edgecolor='gray', alpha=0.85, pad=2),
        )

        gs0 = gridspec.GridSpecFromSubplotSpec(1, 2, subplot_spec=outer[0], wspace=0.1)
        ax_in = fig.add_subplot(gs0[0])
        ax_in.imshow(convert_rgb_transformed_2_rgb(img.cpu().squeeze()))
        ax_in.axis('off')
        ax_in.set_title('Augmented input', fontsize=10)
        ax_in.text(
            0.5, -0.05, f"Encoder hue: {np.round(enc_hue, 3)}",
            transform=ax_in.transAxes, **label_kw,
        )

        ax_rec = fig.add_subplot(gs0[1])
        ax_rec.imshow(convert_rgb_transformed_2_rgb(rec_img.cpu().squeeze()))
        ax_rec.axis('off')
        ax_rec.set_title('Reconstruction', fontsize=10)

        row_specs = [
            (1, 'average_hue', 'Varying H', 'H'),
            (2, 'average_saturation', 'Varying S', 'S'),
            (3, 'average_value', 'Varying V', 'V'),
        ]
        for row_idx, var_name, title, letter in row_specs:
            if var_name not in conditional_vars:
                continue
            entries = varied[var_name]
            gs = gridspec.GridSpecFromSubplotSpec(1, k, subplot_spec=outer[row_idx], wspace=0.1)
            for j, (val, vimg) in enumerate(entries):
                ax = fig.add_subplot(gs[j])
                ax.imshow(convert_rgb_transformed_2_rgb(vimg.cpu().squeeze()))
                ax.axis('off')
                if j == 0:
                    ax.set_title(title, fontsize=10)
                ax.text(
                    0.5, -0.05, f"{letter}={val:.3f}",
                    transform=ax.transAxes, **label_kw,
                )

        gs4 = gridspec.GridSpecFromSubplotSpec(1, 1, subplot_spec=outer[4])
        ax_t = fig.add_subplot(gs4[0])
        ax_t.imshow(convert_rgb_transformed_2_rgb(x_target.cpu().squeeze()))
        ax_t.axis('off')
        ax_t.set_title('Real (loss target)', fontsize=10)
        ax_t.text(
            0.5, -0.05, f"Decoder hue: {np.round(dec_hue, 3)}",
            transform=ax_t.transAxes, **label_kw,
        )

        buf = io.BytesIO()
        fig.savefig(buf, format='png', dpi=150, bbox_inches='tight')
        plt.close(fig)
        buf.seek(0)
        return buf.getvalue()

    def save_reconstruction_compare(
        self, recon_data: Dict[str, Any], epoch: int
    ) -> Optional[Path]:
        """Save reconstruction comparison grid to ``config.analyses_path``.

        Uses the same filename convention as ``VAETrainer.reconstruction_compare``.
        """
        images = recon_data.get('images')
        if images is None or len(images) == 0:
            return None

        originals = images[0]
        reconstructions = images[1]
        n_images = len(originals)

        fig = plt.figure(figsize=(16, 4.5))
        for i in range(n_images):
            ax = fig.add_subplot(2, n_images, i + 1)
            ax.imshow(originals[i])
            ax.axis('off')
            if i == n_images // 2:
                ax.set_title('Original Images')

            ax = fig.add_subplot(2, n_images, i + n_images + 1)
            ax.imshow(reconstructions[i])
            ax.axis('off')
            if i == n_images // 2:
                ax.set_title('Reconstructed Images')

        analyses_path = self.config.analyses_path
        analyses_path.mkdir(parents=True, exist_ok=True)
        training_stage = getattr(self.config, 'training_stage', 'NA')
        fname = f"reconstruct-compare_stage{training_stage}_epoch-{epoch + 1}.png"
        out_path = analyses_path / fname
        fig.savefig(out_path, dpi=150, bbox_inches='tight')
        plt.close(fig)
        logger.info(f"Saved reconstruction compare to {out_path}")
        return out_path

    def save_spectrum_figure(
        self, spectrum: Dict[str, Any], epoch: int
    ) -> Optional[Path]:
        """Save FiLM-spectrum diagnostic to ``config.analyses_path``.

        Uses the same filename convention as
        ``VAETrainer.reconstruction_across_spectrum``.
        """
        png_bytes = spectrum.get('png_bytes')
        if not png_bytes:
            return None

        analyses_path = self.config.analyses_path
        analyses_path.mkdir(parents=True, exist_ok=True)
        idx = spectrum.get('index', 0)
        fname = f"spectrum-reconstruct-compare_epoch-{epoch + 1}-image-{idx}.png"
        out_path = analyses_path / fname
        out_path.write_bytes(png_bytes)
        logger.info(f"Saved spectrum compare to {out_path}")
        return out_path

    def _compute_image_hsv(self, img_array, border_width: int = 12) -> Dict[str, float]:
        """
        Compute HSV values from image border.

        Args:
            img_array: Image array (H, W, C) in 0-1 or 0-255 range, or PIL Image
            border_width: Width of border to sample

        Returns:
            Dict with 'hue', 'saturation', 'value' in normalized scale (0-1)
        """
        try:
            from PIL import Image

            # Convert PIL Image to numpy array if needed
            if isinstance(img_array, Image.Image):
                img_array = np.array(img_array)

            # Ensure image is in 0-255 range for PIL
            if img_array.max() <= 1.0:
                img_array = (img_array * 255).astype(np.uint8)
            else:
                img_array = img_array.astype(np.uint8)

            img = Image.fromarray(img_array, mode='RGB')
            hsv_img = img.convert('HSV')
            width, height = img.size

            # Extract border HSV values
            hsv_values = []
            for x in range(width):
                for y in range(min(border_width, height)):
                    hsv_values.append(hsv_img.getpixel((x, y)))
                for y in range(max(0, height - border_width), height):
                    hsv_values.append(hsv_img.getpixel((x, y)))

            for y in range(border_width, height - border_width):
                for x in range(min(border_width, width)):
                    hsv_values.append(hsv_img.getpixel((x, y)))
                for x in range(max(0, width - border_width), width):
                    hsv_values.append(hsv_img.getpixel((x, y)))

            if hsv_values:
                hsv_array = np.array(hsv_values)
                # Circular mean for hue (compute in radians, then normalize to 0-1)
                hue_rad = hsv_array[:, 0] * 2 * np.pi / 255
                avg_hue = np.arctan2(np.sin(hue_rad).mean(), np.cos(hue_rad).mean())
                avg_hue = (avg_hue / (2 * np.pi)) % 1.0  # Normalize to 0-1

                return {
                    'hue': float(avg_hue),
                    'saturation': float(hsv_array[:, 1].mean() / 255.0),  # Normalize to 0-1
                    'value': float(hsv_array[:, 2].mean() / 255.0),  # Normalize to 0-1
                }
        except Exception as e:
            logger.warning(f"Failed to compute HSV: {e}")

        return {'hue': 0.0, 'saturation': 0.0, 'value': 0.0}

    def save_model(self) -> Path:
        """
        Save trained model to config.models_path.

        Returns:
            Path to saved model file
        """
        models_path = self.config.models_path
        models_path.mkdir(parents=True, exist_ok=True)
        model_path = models_path / "model.pth"
        torch.save(self.vae.state_dict(), model_path)
        logger.info(f"Model saved to {model_path}")
        return model_path

    def train_with_callbacks(self) -> Dict[str, Any]:
        """
        Run training loop with callbacks for progress reporting.

        Returns:
            Training summary dictionary with final metrics and status
        """
        number_epochs = getattr(self.config, 'number_epochs', 100)
        report_periodicity = getattr(self.config, 'report_periodicity', 5)

        start_time = datetime.now()
        best_val_loss = float('inf')
        best_epoch = 0

        # Select fixed indices for reconstruction at start
        self.select_fixed_reconstruction_indices()

        logger.info(f"Starting training for {number_epochs} epochs")

        try:
            for epoch in range(number_epochs):
                epoch_start = time.time()

                # Check for stop request
                if self.stop_check():
                    logger.info(f"Training stopped at epoch {epoch}")
                    return self._build_summary(
                        status="stopped",
                        epochs_completed=epoch,
                        best_epoch=best_epoch,
                        best_val_loss=best_val_loss,
                        start_time=start_time
                    )

                # Check for pause request
                while self.pause_check():
                    time.sleep(0.1)
                    if self.stop_check():
                        return self._build_summary(
                            status="stopped",
                            epochs_completed=epoch,
                            best_epoch=best_epoch,
                            best_val_loss=best_val_loss,
                            start_time=start_time
                        )

                # Training epoch
                train_loss, train_kl, train_recon, train_cond = self._training_epoch(epoch)
                val_loss, val_kl, val_recon, val_cond = self._validation_epoch(epoch)

                # Update learning rate
                self.scheduler.step()

                # Track best epoch
                if val_loss < best_val_loss:
                    best_val_loss = val_loss
                    best_epoch = epoch

                epoch_time = time.time() - epoch_start
                elapsed = (datetime.now() - start_time).total_seconds()

                # Progress callback
                self.progress_callback({
                    "epoch": epoch,
                    "total_epochs": number_epochs,
                    "train_loss": train_loss,
                    "val_loss": val_loss,
                    "train_kl": train_kl,
                    "val_kl": val_kl,
                    "train_recon": train_recon,
                    "val_recon": val_recon,
                    "train_cond": train_cond,
                    "val_cond": val_cond,
                    "best_epoch": best_epoch,
                    "best_val_loss": best_val_loss,
                    "epoch_time": epoch_time,
                    "elapsed_seconds": elapsed,
                })

                # Reconstruction callback periodically
                if (epoch + 1) % report_periodicity == 0:
                    recon_data = self.generate_reconstructions(include_hsv=True)

                    # FiLM-spectrum diagnostic figure (conditional strategies only)
                    spectrum = self.generate_spectrum_figure()
                    if spectrum is not None:
                        recon_data['spectrum'] = spectrum

                    # Also generate fixed-hue reconstructions if configured
                    use_fixed = getattr(self.config, 'use_fixed_decoder', False)
                    fixed_values = getattr(self.config, 'conditional_decoder_fixed_values', None)
                    cond_vars = getattr(self.config, 'conditional_variables', None)

                    if use_fixed and fixed_values and cond_vars:
                        fixed_cond_list = []
                        for var in cond_vars:
                            if var in fixed_values:
                                fixed_cond_list.append(fixed_values[var])
                            else:
                                # Default to 0.5 if not specified
                                fixed_cond_list.append(0.5)

                        fixed_cond = torch.tensor([fixed_cond_list], dtype=torch.float32)
                        fixed_recon_data = self.generate_fixed_cond_reconstructions(
                            fixed_cond, include_hsv=True
                        )
                        recon_data['fixed_hue_data'] = fixed_recon_data

                    self.save_reconstruction_compare(recon_data, epoch)
                    if spectrum is not None:
                        self.save_spectrum_figure(spectrum, epoch)

                    self.reconstruction_callback(recon_data, epoch)

                logger.info(
                    f"Epoch {epoch + 1}/{number_epochs} - "
                    f"Loss: {train_loss:.4f} - Val: {val_loss:.4f} - "
                    f"Best: {best_val_loss:.4f} @ ep{best_epoch}"
                )

                # Clear GPU cache periodically
                if (epoch + 1) % 10 == 0:
                    torch.cuda.empty_cache()

            # Training completed (Phase 1)
            logger.info("Phase 1 (outer VAE) training completed successfully")

            # Phase 2: Tendril training for strategy 14
            architecture = getattr(self.config, 'architecture', 'c_vae')
            tendril_summary = {}
            if architecture in ('tendril_vae', 'multi_cond_tendril_vae'):
                tendril_summary = self._train_tendrils()

            summary = self._build_summary(
                status="completed",
                epochs_completed=number_epochs,
                best_epoch=best_epoch,
                best_val_loss=best_val_loss,
                start_time=start_time
            )
            if tendril_summary:
                summary["tendril_summary"] = tendril_summary
            return summary

        except Exception as e:
            # Guard: never swallow Streamlit control-flow exceptions.
            # On older Streamlit versions RerunException inherits from
            # Exception; catching it here would prevent the script from
            # actually rerunning and leave the app in a broken state.
            if type(e).__name__ in ("RerunException", "StopException", "RerunData"):
                raise
            logger.exception("Training failed with error")
            return self._build_summary(
                status="error",
                epochs_completed=epoch if 'epoch' in locals() else 0,
                best_epoch=best_epoch,
                best_val_loss=best_val_loss,
                start_time=start_time,
                error_message=str(e)
            )

    def _train_tendrils(self) -> Dict[str, Any]:
        """
        Run Phase 2: compute skip connections and train tendril VAEs.

        Returns:
            Dictionary with tendril training summary.
        """
        from candescence.tlv.training.skip_logger import SkipLogger
        from candescence.tlv.architectures.tendril_sub import Tendrils

        phase2_start = time.time()

        # Notify via tendril callback that Phase 2 is starting
        self.tendril_progress_callback({
            "phase": "tendril",
            "status": "computing_skips",
            "message": "Computing skip connections for tendril training...",
        })

        # Strategy 17: collect per-image nuisance when any invariance weight > 0.
        cfg = self.factory.config
        invariance_active = any(
            float(getattr(cfg, k, 0.0) or 0.0) > 0
            for k in (
                'tendril_invariance_plate_weight',
                'tendril_invariance_hsv_weight',
                'tendril_invariance_pheno_weight',
            )
        )
        if invariance_active:
            logger.info("Invariance penalty ACTIVE (Strategy 17) — collecting nuisance.")

        # Compute skip connections (VAE must stay on GPU for encoder forward passes)
        skip_logger = SkipLogger(self.vae, self.device, collect_nuisance=invariance_active)

        logger.info("Computing skip connections for training set...")
        skip_logger.compute_skip_connections(self.train_dataloader, 'train')
        logger.info("Computing skip connections for validation set...")
        skip_logger.compute_skip_connections(self.validation_dataloader, 'validation')

        # Free outer VAE GPU memory before tendril training to avoid OOM.
        # The outer VAE, optimizer, scheduler, and loss model can use 10+ GiB.
        # Tendril training needs similar peak memory for its own models and batches.
        self.optimizer_vae = None
        self.factory.optimizer_vae = None
        if self.optimizer_film is not None:
            self.optimizer_film = None
            self.factory.optimizer_film = None
        if self.scheduler is not None:
            self.scheduler = None
            self.factory.scheduler_vae = None
        if hasattr(self, 'loss_model') and self.loss_model is not None:
            self.loss_model = None
        self.vae.to('cpu')
        if torch.cuda.is_available():
            gc.collect()
            torch.cuda.empty_cache()
        logger.info("Freed outer VAE GPU memory for tendril training")

        # Build tendril arguments and create Tendrils container
        tendril_args = self._get_tendril_arguments_dict()

        # === DIAGNOSTIC: Log tendril training parameters ===
        logger.info("=== TENDRIL TRAINING PARAMETERS ===")
        logger.info(f"  tendril_lr: {tendril_args.get('tendril_lr')}")
        logger.info(f"  tendril_weight_decay: {tendril_args.get('tendril_weight_decay')}")
        logger.info(f"  tendril_batch_size: {tendril_args.get('tendril_batch_size')}")
        logger.info(f"  tendril_latent_dim: {tendril_args.get('tendril_latent_dim')}")
        logger.info(f"  tendril_num_epochs: {tendril_args.get('tendril_num_epochs')}")
        logger.info(f"  tendril_KL_weight: {tendril_args.get('tendril_KL_weight')}")
        logger.info(f"  tendril_MSE_weight: {tendril_args.get('tendril_MSE_weight')}")
        logger.info(f"  tendril_log_cosh_weight: {tendril_args.get('tendril_log_cosh_weight')}")
        logger.info(f"  tendril_loss_fn: {tendril_args.get('tendril_loss_fn', 'NOT SET').__name__ if callable(tendril_args.get('tendril_loss_fn')) else tendril_args.get('tendril_loss_fn')}")
        logger.info("===================================")

        tendrils = Tendrils(arguments=tendril_args)

        # Add tendrils for each layer. Strategy 14/15 inner tendrils run
        # unconditioned (FiLM only on the outer VAE). Strategy 16 adds a
        # plate_phys FiLM pathway to the inner tendril so the plate batch
        # effect can be regressed out of the tendril latent too.
        train_skips = skip_logger.skips['train']
        val_skips = skip_logger.skips['validation']
        layer_keys = sorted(train_skips.keys())
        total_tendrils = len(layer_keys)

        # Build per-key cond dicts aligned with the skip tensors. The
        # SkipLogger populated ``conds[phase]`` while computing skips,
        # so row order matches skip tensor row order. Keep only the
        # keys the inner tendril actually consumes (e.g. plate_phys).
        tendril_cond_keys = tendrils.cond_keys
        train_cond = None
        val_cond = None
        if tendril_cond_keys:
            src_train = skip_logger.conds.get('train') or {}
            src_val = skip_logger.conds.get('validation') or {}
            if not isinstance(src_train, dict) or not isinstance(src_val, dict):
                raise RuntimeError(
                    "Tendril cond_keys are set but SkipLogger did not "
                    "collect per-key cond tensors. This usually means the "
                    "outer VAE is not running a Strategy 15/16 dict-cond "
                    "batch format."
                )
            missing = [k for k in tendril_cond_keys if k not in src_train]
            if missing:
                raise RuntimeError(
                    f"SkipLogger is missing cond keys {missing} required "
                    "for tendril FiLM. Check that the dataset builds them."
                )
            train_cond = {k: src_train[k] for k in tendril_cond_keys}
            val_cond = {k: src_val[k] for k in tendril_cond_keys}

        logger.info(f"Creating {total_tendrils} tendrils for layers: {layer_keys}")

        # === DIAGNOSTIC: Log skip connection statistics ===
        logger.info("=== SKIP CONNECTION STATISTICS (RAW) ===")
        for key in layer_keys:
            t_skip = train_skips[key]
            v_skip = val_skips[key]
            logger.info(f"  Layer {key} TRAIN: shape={list(t_skip.shape)}, "
                        f"min={t_skip.min().item():.6f}, max={t_skip.max().item():.6f}, "
                        f"mean={t_skip.mean().item():.6f}, std={t_skip.std().item():.6f}")
            logger.info(f"  Layer {key} VAL:   shape={list(v_skip.shape)}, "
                        f"min={v_skip.min().item():.6f}, max={v_skip.max().item():.6f}, "
                        f"mean={v_skip.mean().item():.6f}, std={v_skip.std().item():.6f}")
            # Check for degenerate data
            if t_skip.std().item() < 1e-6:
                logger.warning(f"  Layer {key} TRAIN: NEAR-ZERO VARIANCE - skip data may be degenerate!")
            if v_skip.std().item() < 1e-6:
                logger.warning(f"  Layer {key} VAL: NEAR-ZERO VARIANCE - skip data may be degenerate!")
        logger.info("=========================================")

        # Nuisance tensors (plate one-hot, HSV, morphology) row-aligned with the
        # skip tensors — same for every tendril (Strategy 17 invariance penalty).
        train_nuisance = skip_logger.nuisance.get('train')
        val_nuisance = skip_logger.nuisance.get('validation')

        for key in layer_keys:
            tendrils.add_tendril(
                str(key),
                train_skips[key],
                val_skips[key],
                train_cond=train_cond,
                validation_cond=val_cond,
                train_nuisance=train_nuisance,
                validation_nuisance=val_nuisance,
            )

        # Train each tendril with progress callback
        tendril_losses = {}
        num_epochs = tendril_args.get('tendril_num_epochs', 50)

        for idx, key in enumerate(layer_keys):
            str_key = str(key)
            logger.info(f"Training tendril for layer {key} ({idx + 1}/{total_tendrils})")

            def make_callback(current_idx, total, str_k):
                def callback(metrics):
                    self.tendril_progress_callback({
                        "phase": "tendril",
                        "status": "training",
                        "tendril_key": str_k,
                        "tendril_epoch": metrics["epoch"],
                        "tendril_total_epochs": metrics["total_epochs"],
                        "total_tendrils": total,
                        "current_tendril": current_idx,
                        "loss": metrics["loss"],
                        "kl_loss": metrics["kl_loss"],
                        "recon_loss": metrics["recon_loss"],
                    })
                return callback

            tendrils.train_tendril(
                str_key,
                progress_callback=make_callback(idx, total_tendrils, str_key)
            )
            # Get final loss from the trainer
            trainer = tendrils.get_tendril(str_key)
            tendril_losses[str_key] = getattr(trainer, 'final_loss', None)

        # Save tendrils
        self.factory.save_tendrils(tendrils)
        self.tendrils = tendrils

        phase2_time = time.time() - phase2_start
        logger.info(f"Phase 2 completed in {phase2_time:.1f}s")

        return {
            "num_tendrils": total_tendrils,
            "layer_keys": [str(k) for k in layer_keys],
            "tendril_losses": tendril_losses,
            "phase2_seconds": phase2_time,
        }

    def _build_summary(
        self,
        status: str,
        epochs_completed: int,
        best_epoch: int,
        best_val_loss: float,
        start_time: datetime,
        error_message: Optional[str] = None
    ) -> Dict[str, Any]:
        """Build training summary dictionary."""
        elapsed = (datetime.now() - start_time).total_seconds()

        return {
            "status": status,
            "epochs_completed": epochs_completed,
            "total_epochs": getattr(self.config, 'number_epochs', 100),
            "best_epoch": best_epoch,
            "best_val_loss": best_val_loss,
            "elapsed_seconds": elapsed,
            "error_message": error_message,
            "model_path": str(self.config.models_path / "model.pth"),
            "config_path": str(self.config.meta_info_path / "config.json"),
        }
