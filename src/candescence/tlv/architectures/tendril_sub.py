"""
Purpose: Tendril sub-architecture for hierarchical latent learning
Author: Hallett Lab
Date: 2026-01-27
Input: Latent vectors from main VAE
Output: Further compressed latent representations

The Tendrils system creates secondary VAEs that learn on the latent space
of the main VAE, enabling hierarchical representation learning.
"""

import json
from pathlib import Path

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import TensorDataset, DataLoader
from typing import Any, Dict, List, Tuple, Optional, Callable, Union

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.architectures.tendril_sub")


def initialize_weights(m: nn.Module) -> None:
    """Initialize weights with Kaiming uniform for LeakyReLU."""
    if isinstance(m, (nn.Linear, nn.Conv2d, nn.ConvTranspose2d)):
        nn.init.kaiming_uniform_(m.weight, nonlinearity='leaky_relu')
        if m.bias is not None:
            nn.init.zeros_(m.bias)


class Tendrils:
    """
    Container for managing multiple TendrilLayerVAE instances.

    Each tendril learns a secondary VAE on top of the primary latent space,
    enabling hierarchical representation learning. Per-image normalization
    is applied to handle diverse latent distributions.

    Parameters
    ----------
    arguments : dict, optional
        Configuration dictionary containing tendril parameters
    """

    def __init__(self, arguments: Optional[dict] = None) -> None:
        self.arguments = arguments if arguments is not None else {}
        self.tendril_trainers: Dict[Union[int, str], "TendrilTrainer"] = {}
        # Active FiLM cond config for inner tendrils. Empty tuple means
        # inner tendrils run unconditioned (legacy behavior).
        self.cond_keys: Tuple[str, ...] = tuple(
            self.arguments.get('tendril_cond_keys', ()) or ()
        )
        self.cond_dims: Dict[str, int] = dict(
            self.arguments.get('tendril_cond_dims', {}) or {}
        )

    def normalize_per_image(self, tensor: torch.Tensor) -> torch.Tensor:
        """
        Normalize each image's latent representation independently.

        Parameters
        ----------
        tensor : torch.Tensor
            Input tensor of shape [num_images, latent_dim]

        Returns
        -------
        torch.Tensor
            Normalized tensor with same shape
        """
        min_vals = tensor.amin(dim=1, keepdim=True)
        max_vals = tensor.amax(dim=1, keepdim=True)

        epsilon = 1e-8
        normalized_tensor = (tensor - min_vals) / (max_vals - min_vals + epsilon)

        return normalized_tensor

    def create_dataloader(
        self,
        tensor: torch.Tensor,
        shuffle: bool = True,
        cond: Optional[Dict[str, torch.Tensor]] = None,
    ) -> DataLoader:
        """
        Create DataLoader from tensor, with optional cond tensors.

        Parameters
        ----------
        tensor : torch.Tensor
            Input tensor
        shuffle : bool
            Whether to shuffle data
        cond : dict, optional
            Map of cond key → tensor of shape ``(N, cond_dim)`` aligned
            row-wise with ``tensor``. When present each batch unpacks as
            ``(feat, *cond_tensors)`` in the order of ``self.cond_keys``.

        Returns
        -------
        DataLoader
            PyTorch DataLoader
        """
        batch_size = self.arguments.get('tendril_batch_size', 256)
        if cond and self.cond_keys:
            # Order cond tensors deterministically by self.cond_keys so the
            # trainer can unpack by position.
            cond_tensors = [cond[k] for k in self.cond_keys]
            dataset = TensorDataset(tensor, *cond_tensors)
        else:
            dataset = TensorDataset(tensor)
        dataloader = DataLoader(dataset, batch_size=batch_size, shuffle=shuffle)
        return dataloader

    def add_tendril(
        self,
        key: Union[int, str],
        train_dataset: torch.Tensor,
        validation_dataset: torch.Tensor,
        train_cond: Optional[Dict[str, torch.Tensor]] = None,
        validation_cond: Optional[Dict[str, torch.Tensor]] = None,
    ) -> None:
        """
        Add a new tendril VAE for a specific encoder layer.

        Parameters
        ----------
        key : int or str
            Identifier for this tendril (typically encoder layer index)
        train_dataset : torch.Tensor
            Training data (encoder feature maps)
        validation_dataset : torch.Tensor
            Validation data (encoder feature maps)
        train_cond, validation_cond : dict, optional
            Per-variable cond tensors aligned with ``train_dataset`` /
            ``validation_dataset``. Required when ``self.cond_keys`` is
            non-empty so the inner tendril can apply FiLM.
        """
        if key in self.tendril_trainers:
            raise ValueError(f"A TendrilLayerVAE with key '{key}' already exists.")

        if self.cond_keys and (train_cond is None or validation_cond is None):
            raise ValueError(
                "cond_keys is non-empty but train_cond / validation_cond "
                "were not provided."
            )

        vae = TendrilLayerVAE(
            input_shape=train_dataset[0].shape,
            latent_dim=self.arguments['tendril_latent_dim'],
            cond_keys=self.cond_keys,
            cond_dims=self.cond_dims,
        )

        norm_train_dataset = self.normalize_per_image(train_dataset)
        norm_val_dataset = self.normalize_per_image(validation_dataset)

        train_dataloader = self.create_dataloader(
            norm_train_dataset, shuffle=True, cond=train_cond,
        )
        validation_dataloader = self.create_dataloader(
            norm_val_dataset, shuffle=False, cond=validation_cond,
        )

        self.tendril_trainers[key] = TendrilTrainer(
            vae,
            train_dataloader=train_dataloader,
            validation_dataloader=validation_dataloader,
            arguments=self.arguments,
            cond_keys=self.cond_keys,
        )
        logger.info(
            f"Added tendril with key '{key}' "
            f"(cond_keys={self.cond_keys or '()'})"
        )

    def get_tendril(self, key: Union[int, str]) -> "TendrilTrainer":
        """Get a tendril trainer by key."""
        if key not in self.tendril_trainers:
            raise KeyError(f"No TendrilLayerVAE found with key '{key}'.")
        return self.tendril_trainers[key]

    def remove_tendril(self, key: Union[int, str]) -> None:
        """Remove a tendril by key."""
        if key not in self.tendril_trainers:
            raise KeyError(f"No TendrilLayerVAE found with key '{key}'.")
        del self.tendril_trainers[key]

    def list_tendrils(self) -> list:
        """List all tendril keys."""
        return list(self.tendril_trainers.keys())

    def train_tendril(
        self,
        key: Union[int, str],
        progress_callback: Optional[Callable] = None
    ) -> None:
        """Train a specific tendril.

        Parameters
        ----------
        key : int or str
            Tendril key to train
        progress_callback : callable, optional
            Called after each epoch with metrics dict
        """
        if key not in self.tendril_trainers:
            raise KeyError(f"No TendrilTrainer found for key '{key}'.")
        trainer = self.tendril_trainers[key]
        trainer.train(key, progress_callback=progress_callback)
        logger.info(f"Tendril '{key}' training completed.")

    def save(self, path: Path) -> None:
        """
        Save all tendril models and metadata to a directory.

        Creates one .pth file per tendril key plus a tendrils_meta.json
        listing keys, input shapes, and latent dims.

        Parameters
        ----------
        path : Path
            Directory to save tendrils into
        """
        path = Path(path)
        path.mkdir(parents=True, exist_ok=True)

        meta: Dict[str, Dict] = {}
        for key, trainer in self.tendril_trainers.items():
            model = trainer.model
            # Save state dict
            torch.save(model.state_dict(), path / f"{key}.pth")
            meta[key] = {
                "input_shape": list(model.input_shape),
                "latent_dim": model.fc_mu.out_features,
                # FiLM cond config (empty for pre-FiLM tendrils).
                "cond_keys": list(getattr(model, 'cond_keys', ()) or ()),
                "cond_dims": dict(getattr(model, 'cond_dims', {}) or {}),
            }

        with open(path / "tendrils_meta.json", "w") as f:
            json.dump(meta, f, indent=2)

        logger.info(f"Saved {len(meta)} tendrils to {path}")

    @classmethod
    def load(cls, path: Path, device: torch.device) -> "Tendrils":
        """
        Load tendrils from a saved directory (inference only, no dataloaders).

        Parameters
        ----------
        path : Path
            Directory containing tendrils_meta.json and .pth files
        device : torch.device
            Device to load models onto

        Returns
        -------
        Tendrils
            Reconstructed Tendrils instance with models in eval mode
        """
        path = Path(path)
        meta_path = path / "tendrils_meta.json"
        if not meta_path.exists():
            raise FileNotFoundError(f"No tendrils_meta.json found in {path}")

        with open(meta_path) as f:
            meta = json.load(f)

        tendrils = cls()
        for key, info in meta.items():
            input_shape = tuple(info["input_shape"])
            latent_dim = info["latent_dim"]
            # Restore FiLM cond config; older meta entries omit these
            # keys and the VAE reconstructs unconditioned.
            cond_keys = tuple(info.get("cond_keys", ()) or ())
            cond_dims = dict(info.get("cond_dims", {}) or {})

            vae = TendrilLayerVAE(
                input_shape=input_shape,
                latent_dim=latent_dim,
                cond_keys=cond_keys,
                cond_dims=cond_dims,
            )
            state_dict = torch.load(path / f"{key}.pth", map_location="cpu")
            vae.load_state_dict(state_dict)
            vae = vae.to(device)
            vae.eval()

            # Store as a lightweight TendrilTrainer-like object (inference only)
            trainer = _InferenceTendrilHolder(vae, info)
            tendrils.tendril_trainers[key] = trainer

        logger.info(f"Loaded {len(meta)} tendrils from {path}")
        return tendrils


class _InferenceTendrilHolder:
    """
    Lightweight stand-in for TendrilTrainer used when loading tendrils
    for inference only (no dataloaders or optimizer needed).

    Parameters
    ----------
    model : TendrilLayerVAE
        The loaded tendril VAE model in eval mode
    info : dict
        Metadata dict with input_shape, latent_dim, cond_keys, cond_dims
    """

    def __init__(self, model: "TendrilLayerVAE", info: dict) -> None:
        self.model = model
        self.input_shape = tuple(info["input_shape"])
        self.latent_dim = info["latent_dim"]
        self.cond_keys: Tuple[str, ...] = tuple(info.get("cond_keys", ()) or ())
        self.cond_dims: Dict[str, int] = dict(info.get("cond_dims", {}) or {})

    def forward(
        self,
        x: torch.Tensor,
        cond: Optional[Dict[str, torch.Tensor]] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """Run the loaded tendril VAE with optional FiLM conditioning."""
        return self.model(x, cond)


class ResidualBlock(nn.Module):
    """
    Residual block for deeper tendril architectures.

    Parameters
    ----------
    in_channels : int
        Input channels
    out_channels : int
        Output channels
    stride : int
        Convolution stride
    """

    def __init__(
        self,
        in_channels: int,
        out_channels: int,
        stride: int = 1
    ) -> None:
        super(ResidualBlock, self).__init__()
        self.conv1 = nn.Conv2d(in_channels, out_channels, kernel_size=3, stride=stride, padding=1)
        self.bn1 = nn.BatchNorm2d(out_channels)
        self.relu = nn.ReLU()
        self.conv2 = nn.Conv2d(out_channels, out_channels, kernel_size=3, stride=1, padding=1)
        self.bn2 = nn.BatchNorm2d(out_channels)

        # Shortcut connection
        self.shortcut = nn.Sequential()
        if stride != 1 or in_channels != out_channels:
            self.shortcut = nn.Sequential(
                nn.Conv2d(in_channels, out_channels, kernel_size=1, stride=stride),
                nn.BatchNorm2d(out_channels)
            )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """Forward pass with residual connection."""
        out = self.conv1(x)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.conv2(out)
        out = self.bn2(out)
        out += self.shortcut(x)
        out = self.relu(out)
        return out


class TendrilLayerVAE(nn.Module):
    """
    Secondary VAE for learning on latent space, with optional FiLM
    conditioning at inner encoder / decoder blocks.

    This VAE takes latent vectors as input and learns a further
    compressed representation. When ``cond_keys`` is non-empty it
    applies a :class:`_MultiFiLM` stack (from
    :mod:`candescence.tlv.architectures.tendril_vae_multi_cond`) after
    the middle encoder block and before the middle decoder block so
    that a batch-effect signal (e.g. ``plate_phys``) can be regressed
    out of the tendril latent without cluttering the outer-most
    (input-adjacent) layers.

    Parameters
    ----------
    input_shape : tuple
        Shape of input tensor (channels, height, width).
    latent_dim : int
        Dimension of the secondary latent space.
    dropout_rate : float
        Dropout rate for regularization.
    cond_keys : tuple of str
        Active FiLM cond keys. Default ``()`` preserves the
        pre-existing unconditioned architecture so old checkpoints
        still load.
    cond_dims : dict, optional
        Map of cond key → feature dim (size of the one-hot / value
        vector that will be fed in under that key). Required when
        ``cond_keys`` is non-empty.
    """

    # Indices inside the Sequential encoder/decoder at which FiLM fires.
    # Encoder layout: [Conv0, BN1, ReLU2, Drop3, Conv4, BN5, ReLU6, Drop7,
    #                  Conv8, BN9, ReLU10, Drop11]. We apply FiLM after
    # the second Conv block (index 5 = BN output, before ReLU) so the
    # 64-channel map is modulated before nonlinearity.
    _ENC_FILM_AFTER = 5
    # Decoder layout: [ConvT0, BN1, ReLU2, Drop3, ConvT4, BN5, ReLU6,
    #                  Drop7, ConvT8, Sigmoid9]. Apply FiLM after the
    # first transpose-conv block (index 1 = BN output) so the 64-channel
    # map is modulated before ReLU — symmetric with the encoder.
    _DEC_FILM_AFTER = 1

    def __init__(
        self,
        input_shape: Tuple[int, ...],
        latent_dim: int,
        dropout_rate: float = 0.3,
        cond_keys: Tuple[str, ...] = (),
        cond_dims: Optional[Dict[str, int]] = None,
    ) -> None:
        super(TendrilLayerVAE, self).__init__()
        self.input_shape = input_shape
        self.cond_keys: Tuple[str, ...] = tuple(cond_keys)
        self.cond_dims: Dict[str, int] = dict(cond_dims or {})

        # Encoder — kept as nn.Sequential so pre-FiLM checkpoints load
        # without state-dict key renames. When cond_keys is non-empty the
        # forward pass iterates the Sequential and inserts FiLM modulation
        # at the canonical positions (see _ENC_FILM_AFTER).
        self.encoder = nn.Sequential(
            nn.Conv2d(input_shape[0], 32, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(32),
            nn.ReLU(),
            nn.Dropout2d(dropout_rate),

            nn.Conv2d(32, 64, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(64),
            nn.ReLU(),
            nn.Dropout2d(dropout_rate),

            nn.Conv2d(64, 128, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(128),
            nn.ReLU(),
            nn.Dropout2d(dropout_rate),
        )

        # Latent space
        self.flatten = nn.Flatten()
        bottleneck_h = input_shape[1] // 8
        bottleneck_w = input_shape[2] // 8
        flat_size = 128 * bottleneck_h * bottleneck_w

        self.fc_mu = nn.Linear(flat_size, latent_dim)
        self.fc_logvar = nn.Linear(flat_size, latent_dim)
        self.fc_latent = nn.Linear(latent_dim, flat_size)

        # Decoder — same nn.Sequential convention as the encoder.
        self.decoder = nn.Sequential(
            nn.ConvTranspose2d(128, 64, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(64),
            nn.ReLU(),
            nn.Dropout2d(dropout_rate),

            nn.ConvTranspose2d(64, 32, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(32),
            nn.ReLU(),
            nn.Dropout2d(dropout_rate),

            nn.ConvTranspose2d(32, input_shape[0], kernel_size=4, stride=2, padding=1),
            nn.Sigmoid(),
        )

        # FiLM modules: applied inside the Sequential at the canonical
        # positions. Named ``film_*`` so the factory's FiLM optimizer
        # group picks them up.
        if self.cond_keys:
            # Import here to avoid a circular import at module load.
            from candescence.tlv.architectures.tendril_vae_multi_cond import (
                _MultiFiLM,
            )
            self.film_enc = _MultiFiLM(64, self.cond_dims, self.cond_keys)
            self.film_dec = _MultiFiLM(64, self.cond_dims, self.cond_keys)
        else:
            self.film_enc = None
            self.film_dec = None

    def reparameterize(
        self,
        mu: torch.Tensor,
        logvar: torch.Tensor
    ) -> torch.Tensor:
        """Reparameterization trick."""
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std

    def _run_block(
        self,
        block: nn.Sequential,
        x: torch.Tensor,
        film_after: Optional[int],
        film: Optional[nn.Module],
        cond: Optional[Dict[str, torch.Tensor]],
    ) -> torch.Tensor:
        """Apply a Sequential, optionally inserting FiLM after one child.

        When ``film`` is ``None`` or ``cond`` is ``None`` this reduces to
        ``block(x)`` — identical numerics to the pre-FiLM implementation,
        which is what preserves state-dict compatibility for tendrils
        trained before this file grew FiLM support.
        """
        if film is None or cond is None or film_after is None:
            return block(x)
        for i, layer in enumerate(block):
            x = layer(x)
            if i == film_after:
                x = film(x, cond)
        return x

    def forward(
        self,
        x: torch.Tensor,
        cond: Optional[Dict[str, torch.Tensor]] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass through tendril VAE.

        When ``cond_keys == ()`` (no FiLM) or ``cond is None`` the
        numerics are identical to the pre-FiLM implementation, so
        checkpoints trained without FiLM keep working. When FiLM is
        active, modulation is applied inside the encoder/decoder
        Sequentials at :attr:`_ENC_FILM_AFTER` / :attr:`_DEC_FILM_AFTER`.

        Returns
        -------
        x_hat : torch.Tensor
            Reconstructed input
        mu : torch.Tensor
            Latent mean
        logvar : torch.Tensor
            Latent log variance
        z : torch.Tensor
            Sampled latent vector
        """
        h = self._run_block(
            self.encoder, x, self._ENC_FILM_AFTER, self.film_enc, cond,
        )
        h_flat = self.flatten(h)

        mu = self.fc_mu(h_flat)
        logvar = self.fc_logvar(h_flat)
        z = self.reparameterize(mu, logvar)

        x_hat = self.decode(z, cond)
        return x_hat, mu, logvar, z

    def decode(
        self,
        z: torch.Tensor,
        cond: Optional[Dict[str, torch.Tensor]] = None,
    ) -> torch.Tensor:
        """Decode a tendril latent z to a reconstructed feature map.

        Kept as a public method so external callers
        (e.g. :meth:`model_loader.TLVModelWrapper.decode_tendril`)
        don't have to re-implement the FiLM-aware decoder path.
        """
        z_projected = self.fc_latent(z)
        z_projected = z_projected.view(
            -1, 128,
            self.input_shape[1] // 8,
            self.input_shape[2] // 8,
        )
        return self._run_block(
            self.decoder, z_projected,
            self._DEC_FILM_AFTER, self.film_dec, cond,
        )


class TendrilTrainer:
    """
    Trainer for TendrilLayerVAE.

    Parameters
    ----------
    model : TendrilLayerVAE
        The tendril VAE model
    train_dataloader : DataLoader
        Training data loader
    validation_dataloader : DataLoader
        Validation data loader
    arguments : dict
        Configuration dictionary
    """

    def __init__(
        self,
        model: TendrilLayerVAE,
        train_dataloader: DataLoader,
        validation_dataloader: DataLoader,
        arguments: dict,
        cond_keys: Tuple[str, ...] = (),
    ) -> None:
        self.arguments = arguments
        self.device = self.arguments['DEVICE']
        self.model = model.to(self.device)
        # Cached so ``.train()`` knows how to unpack each batch and
        # rebuild the cond dict the model expects.
        self.cond_keys: Tuple[str, ...] = tuple(cond_keys)

        actual_lr = arguments.get('tendril_lr', 1e-6)
        actual_wd = arguments.get('tendril_weight_decay', 0)
        logger.info(f"=== TendrilTrainer OPTIMIZER PARAMS: lr={actual_lr}, weight_decay={actual_wd} ===")
        if actual_lr == 1e-6:
            logger.warning("tendril_lr is at FALLBACK default 1e-6 — arguments dict may be missing 'tendril_lr'!")
        if actual_wd == 0:
            logger.warning("tendril_weight_decay is at FALLBACK default 0 — arguments dict may be missing 'tendril_weight_decay'!")

        self.optimizer = optim.Adam(
            self.model.parameters(),
            lr=actual_lr,
            weight_decay=actual_wd
        )

        self.loss_fn: Callable = self.arguments['tendril_loss_fn']
        self.train_dataloader = train_dataloader
        self.validation_dataloader = validation_dataloader

    def _unpack_cond(
        self, batch: Any,
    ) -> Optional[Dict[str, torch.Tensor]]:
        """Rebuild the per-key cond dict from a dataloader batch.

        Batches are tuples: ``(feat,)`` when there's no cond, or
        ``(feat, *cond_tensors)`` in the order of ``self.cond_keys``
        when cond is active. Returns ``None`` in the no-cond case so
        the VAE's ``forward`` skips the FiLM path.
        """
        if not self.cond_keys or len(batch) < 1 + len(self.cond_keys):
            return None
        cond_tensors = batch[1:1 + len(self.cond_keys)]
        return {
            k: t.to(self.device)
            for k, t in zip(self.cond_keys, cond_tensors)
        }

    def train(
        self,
        key: str,
        progress_callback: Optional[Callable] = None
    ) -> None:
        """
        Train the tendril VAE.

        Parameters
        ----------
        key : str
            Identifier for logging
        progress_callback : callable, optional
            Called after each epoch with metrics dict containing
            tendril_key, epoch, total_epochs, loss, kl_loss, recon_loss
        """
        self.model.train()
        num_epochs = self.arguments.get('tendril_num_epochs', 50)

        for epoch in range(num_epochs):
            total_loss = 0.0
            kl_loss_sum = 0.0
            recon_loss_sum = 0.0

            for batch in self.train_dataloader:
                x = batch[0].to(self.device)
                cond = self._unpack_cond(batch)

                x_hat, mu, logvar, _ = self.model(x, cond)

                loss, kl_loss, recon_loss = self.loss_fn(
                    x=x,
                    x_hat=x_hat,
                    mu=mu,
                    logvar=logvar,
                    arguments=self.arguments,
                    epoch=epoch
                )

                self.optimizer.zero_grad()
                loss.backward()
                self.optimizer.step()

                total_loss += loss.item()
                kl_loss_sum += kl_loss if isinstance(kl_loss, float) else kl_loss.item()
                recon_loss_sum += recon_loss if isinstance(recon_loss, float) else recon_loss.item()

            avg_loss = total_loss / len(self.train_dataloader)
            avg_kl = kl_loss_sum / len(self.train_dataloader)
            avg_recon = recon_loss_sum / len(self.train_dataloader)

            # === DIAGNOSTIC: Log latent space health ===
            # Use last batch's mu/logvar as representative sample
            logger.info(
                f"Tendril '{key}' Epoch [{epoch + 1}/{num_epochs}] "
                f"Loss: {avg_loss:.4f} KL: {avg_kl:.4f} RECON: {avg_recon:.4f} | "
                f"mu_mean: {mu.mean().item():.4f} mu_std: {mu.std().item():.4f} "
                f"logvar_mean: {logvar.mean().item():.4f}"
            )
            if logvar.mean().item() < -10:
                logger.warning(
                    f"Tendril '{key}' Epoch [{epoch + 1}/{num_epochs}]: "
                    f"logvar very negative ({logvar.mean().item():.2f}) — "
                    f"LATENT COLLAPSE detected (posterior ≈ prior)"
                )

            if progress_callback:
                progress_callback({
                    "tendril_key": key,
                    "epoch": epoch,
                    "total_epochs": num_epochs,
                    "loss": avg_loss,
                    "kl_loss": avg_kl,
                    "recon_loss": avg_recon,
                })

        # Store final loss for summary
        self.final_loss = avg_loss


# The FiLM-conditioned tendril sub-VAEs that previously implemented
# Strategy 15 (CondTendrilLayerVAE / CondTendrils / CondTendrilTrainer) have
# been retired. The new Strategy 15 adds per-variable FiLM on the outer VAE
# (see candescence.tlv.architectures.tendril_vae_multi_cond) and reuses the
# plain Tendrils container above. Old code is preserved in
# obsolete/strategy_15_cond_tendrils/cond_tendrils.py.
