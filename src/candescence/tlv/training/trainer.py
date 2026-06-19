"""
Purpose: VAE trainer for TLV module
Author: Hallett Lab
Date: 2026-01-27

Provides VAETrainer class for training variational autoencoders.
"""

from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Tuple, Union

import lpips
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from torch.optim import Optimizer
from torch.optim.lr_scheduler import _LRScheduler
from torch.utils.data import DataLoader

from candescence.core.logging_config import get_logger
from candescence.tlv.losses import (
    SSIMLoss,
    get_kl_recon_cond_loss,
    get_kl_recon_loss,
    get_kl_recon_skip_cond_loss,
)
from candescence.tlv.data.dataset import FullDataset
from candescence.tlv.training.loss_logger import LossLogger
from candescence.tlv.training.skip_logger import SkipLogger
from candescence.tlv.utilities import convert_rgb_transformed_2_rgb

logger = get_logger("candescence.tlv.training.trainer")


def _batchify_cond(
    cond: Union[torch.Tensor, Dict[str, torch.Tensor]],
) -> Union[torch.Tensor, Dict[str, torch.Tensor]]:
    """Add a leading batch dim to a 1-D tensor, or to each tensor in a dict."""
    if isinstance(cond, dict):
        return {k: (v.unsqueeze(0) if v.dim() == 1 else v) for k, v in cond.items()}
    return cond.unsqueeze(0) if cond.dim() == 1 else cond


class VAETrainer:
    """
    Trainer class for VAE models.

    Handles training loops, validation, loss logging, and visualization.
    """

    def __init__(
        self,
        vae: nn.Module,
        train_dataloader: DataLoader,
        validation_dataloader: DataLoader,
        optimizer_vae: Optimizer,
        optimizer_film: Optional[Optimizer],
        scheduler: _LRScheduler,
        config: Any,
    ) -> None:
        """
        Initialize VAETrainer.

        Args:
            vae: The VAE model to train.
            train_dataloader: DataLoader for training data.
            validation_dataloader: DataLoader for validation data.
            optimizer_vae: Optimizer for VAE parameters.
            optimizer_film: Optional optimizer for FiLM parameters.
            scheduler: Learning rate scheduler.
            config: Configuration object with training parameters.
        """
        self.vae = vae
        self.train_dataloader = train_dataloader
        self.validation_dataloader = validation_dataloader
        self.optimizer_vae = optimizer_vae
        self.optimizer_film = optimizer_film
        self.scheduler = scheduler
        self.config = config

        self.device = torch.device(
            config.device if torch.cuda.is_available() else "cpu"
        )
        self.vae.to(self.device)

        self._setup_loss_function()
        self._setup_loss_model()

        self.loss_logger = LossLogger(config)
        self.skip_logger: Optional[SkipLogger] = None
        self._inference_objects: Dict[str, Any] = {}
        self._random_indices: Optional[np.ndarray] = None
        self._random_indices_spectrum: Optional[np.ndarray] = None

    def _setup_loss_function(self) -> None:
        """Setup loss function based on architecture."""
        architecture = getattr(self.config, 'architecture', 'c_vae')

        if architecture in ('cond_uc_vae', 'cruc_vae', 'tendril_vae',
                            'multi_cond_tendril_vae'):
            self.loss_fn = get_kl_recon_cond_loss
        elif architecture == 'crutch_vae':
            self.loss_fn = get_kl_recon_skip_cond_loss
        else:
            self.loss_fn = get_kl_recon_loss

        # Store estimate function for conditional loss computation
        self.estimate_fn = FullDataset.estimate_background_image_tensor_helper

    def _setup_loss_model(self) -> None:
        """Setup perceptual loss model."""
        loss_function = getattr(self.config, 'vae_loss_function', 'mse')

        if loss_function == 'lpips':
            self.loss_model = lpips.LPIPS(net='vgg').to(self.device)
        elif loss_function == 'ssim':
            self.loss_model = SSIMLoss().to(self.device)
        else:
            self.loss_model = None

    def _prepare_batch_inputs(
        self,
        batch: Union[torch.Tensor, Tuple]
    ) -> Tuple[torch.Tensor, Optional[torch.Tensor], Optional[torch.Tensor],
               Optional[torch.Tensor], Optional[torch.Tensor]]:
        """
        Prepare batch inputs for forward pass.

        Args:
            batch: Batch from dataloader.

        Returns:
            Tuple of (x, encoder_cond, decoder_cond, cond, indices).
        """
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

    def _forward_vae_with_conditions(
        self,
        x: torch.Tensor,
        encoder_cond: Optional[Union[torch.Tensor, Dict[str, torch.Tensor]]] = None,
        decoder_cond: Optional[Union[torch.Tensor, Dict[str, torch.Tensor]]] = None,
        cond: Optional[torch.Tensor] = None
    ) -> Tuple:
        """Forward pass through VAE with conditions.

        ``encoder_cond`` / ``decoder_cond`` can be either a plain tensor
        (Strategies 13 / 14 / etc.) or a dict of tensors (Strategy 15,
        keys ``'hue'``, ``'day'``, ``'media'``). Moves both onto the
        training device before calling the VAE.
        """
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

    def _move_cond_to_device(
        self,
        cond: Union[torch.Tensor, Dict[str, torch.Tensor]],
    ) -> Union[torch.Tensor, Dict[str, torch.Tensor]]:
        """Move a tensor or a dict of tensors onto ``self.device`` as float."""
        if isinstance(cond, dict):
            return {k: v.to(self.device).float() for k, v in cond.items()}
        return cond.to(self.device).float()

    def _decide_image_for_loss_comparison(
        self,
        x: torch.Tensor,
        indices_into_df: torch.Tensor,
        which: str = 'train',
        which_image: str = 'transformed_image'
    ) -> torch.Tensor:
        """
        Decide which image to use for loss comparison.

        Args:
            x: Input tensor.
            indices_into_df: Indices into dataset.
            which: 'train' or 'validation'.
            which_image: Column name for image.

        Returns:
            Image tensor for loss comparison.
        """
        strategy = getattr(self.config, 'strategy', 0)

        if strategy in (0, 1, 2, 3, 5, 7, 8, 11, 12):
            return x.to(self.device)

        elif strategy in (4, 6, 9):
            if which == 'train':
                learner = self.train_dataloader.dataset.meta_df_subset.iloc[indices_into_df]
                target_df = self.train_dataloader.dataset.target_df
            else:
                learner = self.validation_dataloader.dataset.meta_df_subset.iloc[indices_into_df]
                target_df = self.validation_dataloader.dataset.target_df

            target_dict = target_df.set_index('id').to_dict('index')
            new_images = []

            for i in range(len(indices_into_df)):
                parent_id = learner.iloc[i]['parent_id']
                if pd.isnull(parent_id):
                    raise ValueError(f"No parent is null at row {i}")
                parent_row = target_dict.get(parent_id, None)
                parent_img = parent_row[which_image]

                if not isinstance(parent_img, torch.Tensor):
                    parent_img = torch.tensor(parent_img)
                new_images.append(parent_img.to(self.device))

            return torch.stack(new_images)

        elif strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16, 17):
            # Denoising direction: when augmenting, the dataset iterates
            # over augmented rows (encoder input). The reconstruction
            # target is the canonical parent (real) image, so the latent
            # is pushed toward HSV invariance. When not augmenting, the
            # dataset iterates over real rows and the target is just x.
            augment_images = getattr(self.config, 'augment_images', False)
            if not augment_images:
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

        else:
            raise ValueError(f"Invalid strategy: {strategy}")

    def training_epoch(self, epoch: int) -> Tuple[float, float, float, float, float]:
        """
        Execute one training epoch.

        Args:
            epoch: Current epoch number.

        Returns:
            Tuple of (avg_loss, avg_kl, avg_recon, avg_cond, avg_skip).
        """
        self.vae.train()
        total_loss = 0.0
        total_kl = 0.0
        total_recon = 0.0
        total_cond = 0.0
        total_skip = 0.0

        architecture = getattr(self.config, 'architecture', 'c_vae')

        for batch_idx, batch in enumerate(self.train_dataloader):
            x, encoder_cond, decoder_cond, cond, indices = self._prepare_batch_inputs(batch)

            if isinstance(x, list):
                x = torch.stack(x)

            x = x.to(self.device).float()
            self.optimizer_vae.zero_grad()
            if self.optimizer_film is not None:
                self.optimizer_film.zero_grad()

            # Forward pass
            if architecture == 'crutch_vae':
                x_hat, z, mu, logvar, from_skip, to_skip = self._forward_vae_with_conditions(
                    x, encoder_cond, decoder_cond, cond
                )
            elif architecture in ('tendril_vae', 'multi_cond_tendril_vae'):
                x_hat, z, mu, logvar, skip_connections = self._forward_vae_with_conditions(
                    x, encoder_cond, decoder_cond, cond
                )
            else:
                x_hat, z, mu, logvar = self._forward_vae_with_conditions(
                    x, encoder_cond, decoder_cond, cond
                )

            x_comparison = self._decide_image_for_loss_comparison(
                x, indices_into_df=indices, which='train', which_image='transformed_image'
            )
            x_comparison = x_comparison.to(self.device).float()

            # Compute loss
            if architecture == 'crutch_vae':
                loss, kl_loss, recon_loss, cond_loss, skip_loss = self.loss_fn(
                    x_comparison, x_hat, z, mu, logvar, from_skip, to_skip,
                    self._get_arguments_dict(), epoch, self.loss_model, self.estimate_fn
                )
            elif architecture in ('cond_uc_vae', 'tendril_vae', 'multi_cond_tendril_vae'):
                loss, kl_loss, recon_loss, cond_loss = self.loss_fn(
                    x_comparison, x_hat, z, mu, logvar,
                    self._get_arguments_dict(), epoch, self.loss_model, self.estimate_fn
                )
                skip_loss = 0
            else:
                loss, kl_loss, recon_loss, *rest = self.loss_fn(
                    x_comparison, x_hat, z, mu, logvar,
                    self._get_arguments_dict(), epoch, self.loss_model
                )
                cond_loss = rest[0] if rest else 0
                skip_loss = 0

            loss.backward()
            if self.optimizer_film is not None:
                self.optimizer_film.step()
            self.optimizer_vae.step()

            total_loss += loss.item()
            total_kl += kl_loss.item()
            total_recon += recon_loss.item()
            total_cond += cond_loss.item() if hasattr(cond_loss, 'item') else cond_loss
            total_skip += skip_loss.item() if hasattr(skip_loss, 'item') else skip_loss

        num_batches = len(self.train_dataloader)
        return (
            total_loss / num_batches,
            total_kl / num_batches,
            total_recon / num_batches,
            total_cond / num_batches,
            total_skip / num_batches
        )

    def validation_epoch(self, epoch: int) -> Tuple[float, float, float, float, float]:
        """
        Execute one validation epoch.

        Args:
            epoch: Current epoch number.

        Returns:
            Tuple of (avg_loss, avg_kl, avg_recon, avg_cond, avg_skip).
        """
        self.vae.eval()
        total_loss = 0.0
        total_kl = 0.0
        total_recon = 0.0
        total_cond = 0.0
        total_skip = 0.0

        architecture = getattr(self.config, 'architecture', 'c_vae')

        with torch.no_grad():
            for batch_idx, batch in enumerate(self.validation_dataloader):
                x, encoder_cond, decoder_cond, cond, indices = self._prepare_batch_inputs(batch)

                if isinstance(x, list):
                    x = torch.stack(x)

                if architecture == 'crutch_vae':
                    x_hat, z, mu, logvar, from_skip, to_skip = self._forward_vae_with_conditions(
                        x, encoder_cond, decoder_cond, cond
                    )
                elif architecture == 'tendril_vae':
                    x_hat, z, mu, logvar, skip_connections = self._forward_vae_with_conditions(
                        x, encoder_cond, decoder_cond, cond
                    )
                else:
                    x_hat, z, mu, logvar = self._forward_vae_with_conditions(
                        x, encoder_cond, decoder_cond, cond
                    )

                x_comparison = self._decide_image_for_loss_comparison(
                    x, indices_into_df=indices, which='validation', which_image='transformed_image'
                )
                x_comparison = x_comparison.to(self.device).float()

                if architecture == 'crutch_vae':
                    loss, kl_loss, recon_loss, cond_loss, skip_loss = self.loss_fn(
                        x_comparison, x_hat, z, mu, logvar, from_skip, to_skip,
                        self._get_arguments_dict(), epoch, self.loss_model, self.estimate_fn
                    )
                elif architecture in ('cond_uc_vae', 'tendril_vae'):
                    loss, kl_loss, recon_loss, cond_loss = self.loss_fn(
                        x_comparison, x_hat, z, mu, logvar,
                        self._get_arguments_dict(), epoch, self.loss_model, self.estimate_fn
                    )
                    skip_loss = 0
                else:
                    loss, kl_loss, recon_loss, *rest = self.loss_fn(
                        x_comparison, x_hat, z, mu, logvar,
                        self._get_arguments_dict(), epoch, self.loss_model
                    )
                    cond_loss = rest[0] if rest else 0
                    skip_loss = 0

                total_loss += loss.item()
                total_kl += kl_loss.item()
                total_recon += recon_loss.item()
                total_cond += cond_loss.item() if hasattr(cond_loss, 'item') else cond_loss
                total_skip += skip_loss.item() if hasattr(skip_loss, 'item') else skip_loss

        num_batches = len(self.validation_dataloader)
        return (
            total_loss / num_batches,
            total_kl / num_batches,
            total_recon / num_batches,
            total_cond / num_batches,
            total_skip / num_batches
        )

    def _get_arguments_dict(self) -> Dict[str, Any]:
        """Convert config to arguments dictionary for loss functions."""
        return {
            'strategy': getattr(self.config, 'strategy', 0),
            'architecture': getattr(self.config, 'architecture', 'c_vae'),
            'vae_loss_function': getattr(self.config, 'vae_loss_function', 'mse'),
            'kl_weight': getattr(self.config, 'kl_weight', 1.0),
            'MSE_weight': getattr(self.config, 'mse_weight', 1.0),
            'LPIPS_weight': getattr(self.config, 'lpips_weight', 1.0),
            'SSIM_weight': getattr(self.config, 'ssim_weight', 1.0),
            'skip_weight': getattr(self.config, 'skip_weight', 1.0),
            'conditional_loss_weight': getattr(self.config, 'conditional_loss_weight', 1.0),
            'cycle_length': getattr(self.config, 'cycle_length', 10),
            'conditional_variables': getattr(self.config, 'conditional_variables', []),
            'image_dimension': getattr(self.config, 'image_dimension', 128),
            'grayscale': getattr(self.config, 'grayscale', False),
        }

    def train(self) -> None:
        """Main training loop."""
        self.skip_logger = SkipLogger(vae=self.vae, device=self.device)
        number_epochs = getattr(self.config, 'number_epochs', 100)
        report_periodicity = getattr(self.config, 'report_periodicity', 10)

        for epoch in range(number_epochs):
            self.skip_logger.reset()

            train_loss, train_kl, train_recon, train_cond, train_skip = self.training_epoch(epoch)
            val_loss, val_kl, val_recon, val_cond, val_skip = self.validation_epoch(epoch)

            self.scheduler.step()

            self.loss_logger.log_epoch(
                train_loss=train_loss,
                train_kl_loss=train_kl,
                train_recon_loss=train_recon,
                train_cond_loss=train_cond,
                train_skip_loss=train_skip,
                val_loss=val_loss,
                val_kl_loss=val_kl,
                val_recon_loss=val_recon,
                val_cond_loss=val_cond,
                val_skip_loss=val_skip
            )

            logger.info(
                f"Epoch {epoch + 1}/{number_epochs} - "
                f"Loss: {train_loss:.4f} - "
                f"RECON: {train_recon:.4f} - "
                f"KL: {train_kl:.4f} - "
                f"Cond: {train_cond:.4f}"
            )

            if (epoch + 1) % report_periodicity == 0:
                self.reconstruction_compare(n=10, epoch=epoch)
                strategy = getattr(self.config, 'strategy', 0)
                if strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16, 17):
                    self.reconstruction_across_spectrum(n=1, epoch=epoch)

        # Collect skip connections after training for tendril Stage 2.
        # Only needed for strategy 14 (tendril_vae) where the skip data
        # feeds secondary tendril VAE training. Called once after the
        # final epoch since log_epoch() overwrites previous data anyway.
        architecture = getattr(self.config, 'architecture', 'c_vae')
        if architecture == 'tendril_vae':
            logger.info(
                "Computing final skip connections for tendril training..."
            )
            self.skip_logger.compute_skip_connections(
                self.train_dataloader, 'train'
            )
            self.skip_logger.compute_skip_connections(
                self.validation_dataloader, 'validation'
            )
            logger.info("Skip connections computed for all layers.")

        # Plot losses and save model
        self.loss_logger.plot_all_losses()
        self._save_model()

    def _save_model(self) -> None:
        """Save trained model."""
        models_path = self.config.models_path
        models_path.mkdir(parents=True, exist_ok=True)
        model_path = models_path / "model.pth"
        torch.save(self.vae.state_dict(), model_path)
        logger.info(f"Model saved to {model_path}")

    def reconstruction_compare(self, n: int = 10, epoch: Optional[int] = None) -> None:
        """
        Compare original and reconstructed images.

        Args:
            n: Number of images to compare.
            epoch: Current epoch number.
        """
        if self._random_indices is None:
            # Clamp to the validation set size so small datasets (e.g. the
            # bundled sample) don't trip ``replace=False``.
            n_available = len(self.validation_dataloader.dataset)
            self._random_indices = np.random.choice(
                n_available, min(n, n_available), replace=False
            )
        random_indices = self._random_indices

        plt.figure(figsize=(16, 4.5))
        architecture = getattr(self.config, 'architecture', 'c_vae')

        for i, idx in enumerate(random_indices):
            item = self.validation_dataloader.dataset[idx]

            # Get image based on dataset format
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

            img = img.unsqueeze(0).to(self.device).float()

            if encoder_cond is not None:
                encoder_cond = _batchify_cond(encoder_cond)
                encoder_cond = self._move_cond_to_device(encoder_cond)
            if decoder_cond is not None:
                decoder_cond = _batchify_cond(decoder_cond)
                decoder_cond = self._move_cond_to_device(decoder_cond)

            self.vae.eval()
            with torch.no_grad():
                if encoder_cond is not None:
                    rec_img, *_ = self.vae(img, [encoder_cond, decoder_cond])
                else:
                    rec_img, *_ = self.vae(img)

            # Plot original
            orig = convert_rgb_transformed_2_rgb(img.cpu().squeeze())
            ax = plt.subplot(2, n, i + 1)
            plt.imshow(orig)
            ax.axis('off')
            if i == n // 2:
                ax.set_title('Original Images')

            # Plot reconstructed
            rec = convert_rgb_transformed_2_rgb(rec_img.cpu().squeeze())
            ax = plt.subplot(2, n, i + n + 1)
            plt.imshow(rec)
            ax.axis('off')
            if i == n // 2:
                ax.set_title('Reconstructed Images')

        # Save figure
        analyses_path = self.config.analyses_path
        analyses_path.mkdir(parents=True, exist_ok=True)

        training_stage = getattr(self.config, 'training_stage', 'NA')
        if epoch is not None:
            fname = f"reconstruct-compare_stage{training_stage}_epoch-{epoch+1}.png"
        else:
            fname = f"reconstruct-compare_stage{training_stage}.png"

        plt.savefig(analyses_path / fname)
        plt.show()

    def reconstruction_across_spectrum(
        self,
        n: int = 1,
        k: int = 2,
        epoch: Optional[int] = None,
    ) -> None:
        """Visualize FiLM responsiveness by sweeping decoder HSV conditions.

        For each of ``n`` validation images, holds the latent fixed (the
        encoder's z is reproduced via RNG-state replay) and decodes with
        ``k`` swept values per conditional channel. The 5-row figure shows:

          1. Augmented input (encoder input)  |  Reconstruction
          2. Reconstructions varying H        (k columns)
          3. Reconstructions varying S        (k columns)
          4. Reconstructions varying V        (k columns)
          5. Real parent image (loss target, what the recon is trained against)

        Rows 2-4 are skipped for channels not in
        ``config.conditional_variables``. Saved to ``ANALYSES`` as
        ``spectrum-reconstruct-compare_epoch-{N}-image-{idx}.png`` once
        per called epoch.
        """
        strategy = getattr(self.config, 'strategy', 0)
        if strategy not in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16, 17):
            raise ValueError(
                f"reconstruction_across_spectrum requires a conditional strategy, got {strategy}"
            )

        if self._random_indices_spectrum is None:
            self._random_indices_spectrum = np.random.choice(
                len(self.validation_dataloader.dataset), n, replace=False
            )
        random_indices = self._random_indices_spectrum

        conditional_vars = list(getattr(self.config, 'conditional_variables', []))
        new_values = [v / (k + 1) for v in range(1, k + 1)]
        analyses_path = self.config.analyses_path
        analyses_path.mkdir(parents=True, exist_ok=True)

        for idx in random_indices:
            item = self.validation_dataloader.dataset[idx]
            if len(item) != 4:
                # Conditional strategies always return 4-tuples; bail loudly otherwise.
                raise ValueError(
                    f"Expected 4-tuple from dataset for strategy {strategy}, got {len(item)}"
                )
            img, encoder_cond, decoder_cond, _ = item

            img = img.unsqueeze(0).to(self.device).float()
            encoder_cond = self._move_cond_to_device(_batchify_cond(encoder_cond))
            decoder_cond = self._move_cond_to_device(_batchify_cond(decoder_cond))

            self.vae.eval()
            with torch.no_grad():
                # Pin RNG so each forward redraws the SAME reparameterization
                # noise — the latent is then identical across the spectrum.
                rng_state = torch.get_rng_state()
                if torch.cuda.is_available():
                    cuda_rng_state = torch.cuda.get_rng_state(self.device)

                def _forward_with_decoder_cond(d_cond):
                    torch.set_rng_state(rng_state)
                    if torch.cuda.is_available():
                        torch.cuda.set_rng_state(cuda_rng_state, self.device)
                    out = self.vae(img, [encoder_cond, d_cond])
                    return out[0]  # x_hat is always first

                rec_img = _forward_with_decoder_cond(decoder_cond)

                # Build varied decoder conds, one channel at a time.
                varied = {'average_hue': [], 'average_saturation': [], 'average_value': []}
                for var_name in ('average_hue', 'average_saturation', 'average_value'):
                    if var_name not in conditional_vars:
                        continue
                    pos = conditional_vars.index(var_name)
                    for new_val in new_values:
                        d_cond_var = self._override_hue_channel(
                            decoder_cond, pos, new_val
                        )
                        varied[var_name].append((
                            new_val,
                            _forward_with_decoder_cond(d_cond_var),
                        ))

                # Real parent image (loss target under the denoising direction).
                indices_t = torch.tensor([idx])
                x_target = self._decide_image_for_loss_comparison(
                    img, indices_into_df=indices_t, which='validation',
                    which_image='transformed_image',
                ).to(self.device).float()

            self._plot_spectrum_grid(
                img, rec_img, varied, x_target,
                encoder_cond, decoder_cond, conditional_vars, k,
                idx=int(idx), epoch=epoch, analyses_path=analyses_path,
            )

    @staticmethod
    def _override_hue_channel(
        d_cond: Union[torch.Tensor, Dict[str, torch.Tensor]],
        pos: int,
        new_val: float,
    ) -> Union[torch.Tensor, Dict[str, torch.Tensor]]:
        """Return a copy of d_cond with the ``pos``-th hue channel set to ``new_val``.

        For dict conds (Strategy 15/16) only the ``'hue'`` entry is
        modified; day/media/plate_phys are sample identity and pass
        through unchanged.
        """
        if isinstance(d_cond, dict):
            new_hue = d_cond['hue'].clone()
            new_hue[..., pos] = new_val
            return {**d_cond, 'hue': new_hue}
        out = d_cond.clone()
        out[..., pos] = new_val
        return out

    def _plot_spectrum_grid(
        self,
        img: torch.Tensor,
        rec_img: torch.Tensor,
        varied: Dict[str, List[Tuple[float, torch.Tensor]]],
        x_target: torch.Tensor,
        encoder_cond: Union[torch.Tensor, Dict[str, torch.Tensor]],
        decoder_cond: Union[torch.Tensor, Dict[str, torch.Tensor]],
        conditional_vars: List[str],
        k: int,
        idx: int,
        epoch: Optional[int],
        analyses_path: Path,
    ) -> None:
        """Render and save the 5-row spectrum figure."""
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

        # Row 1: augmented input | reconstruction
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

        # Rows 2-4: varying H, S, V — letter is explicit, not derived from var name.
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

        # Row 5: real parent (loss target)
        gs4 = gridspec.GridSpecFromSubplotSpec(1, 1, subplot_spec=outer[4])
        ax_t = fig.add_subplot(gs4[0])
        ax_t.imshow(convert_rgb_transformed_2_rgb(x_target.cpu().squeeze()))
        ax_t.axis('off')
        ax_t.set_title('Real (loss target)', fontsize=10)
        ax_t.text(
            0.5, -0.05, f"Decoder hue: {np.round(dec_hue, 3)}",
            transform=ax_t.transAxes, **label_kw,
        )

        if epoch is not None:
            fname = f"spectrum-reconstruct-compare_epoch-{epoch + 1}-image-{idx}.png"
        else:
            fname = f"spectrum-reconstruct-compare_image-{idx}.png"
        plt.savefig(analyses_path / fname, dpi=150, bbox_inches='tight')
        plt.close(fig)

    @staticmethod
    def train_vae(
        vae: nn.Module,
        train_dataloader: DataLoader,
        validation_dataloader: DataLoader,
        optimizer_vae: Optimizer,
        optimizer_film: Optional[Optimizer],
        scheduler: _LRScheduler,
        config: Any
    ) -> 'VAETrainer':
        """
        Static method to create and run trainer.

        Args:
            vae: VAE model.
            train_dataloader: Training DataLoader.
            validation_dataloader: Validation DataLoader.
            optimizer_vae: VAE optimizer.
            optimizer_film: FiLM optimizer.
            scheduler: LR scheduler.
            config: Configuration object.

        Returns:
            Trained VAETrainer instance.
        """
        trainer = VAETrainer(
            vae, train_dataloader, validation_dataloader,
            optimizer_vae, optimizer_film, scheduler, config
        )
        trainer.train()
        return trainer
