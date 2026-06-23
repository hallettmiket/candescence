"""
Purpose: Model loading utilities for the Candescence interface
Author: Hallett Lab
Date: 2026-01-28

Provides TLVModelWrapper to adapt TLV VAE models to the CandescenceModel
interface, and functions to load models from saved checkpoints.
"""

import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import torch

from candescence.core.logging_config import get_logger
from candescence.interface.base import CandescenceModel

logger = get_logger("candescence.interface.model_loader")


def _cond_to_device(cond, device):
    """Move a tensor or dict-of-tensors cond onto *device* as float.

    Strategy 15 encoders take a dict ``{'hue', 'day', 'media'}``; all other
    conditional strategies take a single tensor. Returns ``None`` unchanged.
    """
    if cond is None:
        return None
    if isinstance(cond, dict):
        return {k: v.to(device).float() for k, v in cond.items()}
    return cond.to(device).float()


class TLVModelWrapper(CandescenceModel):
    """
    Wrapper to make TLV VAE models conform to CandescenceModel interface.

    Handles the complexity of different TLV strategies (0, 1, 7, 8, 9, 14, etc.)
    and provides a unified encode/decode interface.

    The wrapper caches skip connections between encode and decode calls,
    which is necessary for U-Net style architectures.

    Example:
        >>> wrapper = TLVModelWrapper(vae, config)
        >>> z = wrapper.encode(images, conditioning)
        >>> reconstructed = wrapper.decode(z, conditioning)
    """

    def __init__(
        self,
        vae: torch.nn.Module,
        config: Any,
        metadata: Optional[Dict] = None
    ) -> None:
        """
        Initialize the wrapper.

        Args:
            vae: The VAE model (encoder + decoder)
            config: TLVConfig or similar configuration object
            metadata: Optional metadata dictionary
        """
        self._vae = vae
        self._config = config
        self._metadata = metadata or {}
        self._strategy = getattr(config, 'strategy', 0)
        self._latent_dim = getattr(config, 'latent_dim', 128)
        self._device = getattr(config, 'device', torch.device('cpu'))

        # Cache for skip connections (needed between encode/decode)
        self._last_skip: Optional[Any] = None
        self._last_fmap_shape: Optional[Tuple] = None

        # Tendrils (optional, loaded separately)
        self._tendrils: Optional[Any] = None

        # Ensure model is in eval mode
        self._vae.eval()

        logger.debug(
            f"TLVModelWrapper initialized: strategy={self._strategy}, "
            f"latent_dim={self._latent_dim}"
        )

    def encode(
        self,
        x: torch.Tensor,
        cond=None,
    ) -> torch.Tensor:
        """
        Encode input to latent space.

        Handles different TLV strategies which have different encoder signatures.

        Args:
            x: Input tensor of shape (B, C, H, W)
            cond: Optional conditioning. A tensor for Strategies 7/8/9/13/14,
                a dict of tensors (``{'hue', 'day', 'media'}``) for
                Strategy 15, or ``None`` for unconditional strategies.

        Returns:
            Latent representation of shape (B, latent_dim)
        """
        self._vae.encoder.eval()

        with torch.no_grad():
            x = x.to(self._device)
            cond = _cond_to_device(cond, self._device)

            if self._strategy in (14, 14.1, 15, 16, 17):
                # Tendril VAE: encoder(x, cond) -> (z, mu, logvar, skip)
                z, mu, logvar, skip = self._vae.encoder(x, cond)
                self._last_skip = skip
                return z

            elif self._strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13):
                # Conditional with separate encoder/decoder conditioning
                # Encoder takes [cond_enc, cond_dec] but we use same for both
                cond_list = [cond, cond] if cond is not None else None
                z, mu, logvar, skip, fmap_shape = self._vae.encoder(x, cond_list)
                self._last_skip = skip
                self._last_fmap_shape = fmap_shape
                return z

            elif self._strategy in (7, 8, 9):
                # Conditional VAE
                z, mu, logvar, skip, fmap_shape = self._vae.encoder(x, cond)
                self._last_skip = skip
                self._last_fmap_shape = fmap_shape
                return z

            elif self._strategy in (1, 12):
                # U-Net VAE (no conditioning)
                z, mu, logvar, skip, fmap_shape = self._vae.encoder(x)
                self._last_skip = skip
                self._last_fmap_shape = fmap_shape
                return z

            else:
                # Fallback: try basic encoder call
                out = self._vae.encoder(x)
                if isinstance(out, tuple):
                    z = out[0]
                    if len(out) > 3:
                        self._last_skip = out[3]
                    if len(out) > 4:
                        self._last_fmap_shape = out[4]
                else:
                    z = out
                return z

    def encode_full(
        self,
        x: torch.Tensor,
        cond=None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Encode input, returning z, mu, and logvar for diagnostics.

        Args:
            x: Input tensor of shape (B, C, H, W)
            cond: Optional conditioning (tensor or dict — see ``encode``).

        Returns:
            Tuple of (z, mu, logvar)
        """
        self._vae.encoder.eval()

        with torch.no_grad():
            x = x.to(self._device)
            cond = _cond_to_device(cond, self._device)

            if self._strategy in (14, 14.1, 15, 16, 17):
                z, mu, logvar, skip = self._vae.encoder(x, cond)
                self._last_skip = skip
                return z, mu, logvar

            elif self._strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13):
                cond_list = [cond, cond] if cond is not None else None
                z, mu, logvar, skip, fmap_shape = self._vae.encoder(x, cond_list)
                self._last_skip = skip
                self._last_fmap_shape = fmap_shape
                return z, mu, logvar

            elif self._strategy in (7, 8, 9):
                z, mu, logvar, skip, fmap_shape = self._vae.encoder(x, cond)
                self._last_skip = skip
                self._last_fmap_shape = fmap_shape
                return z, mu, logvar

            elif self._strategy in (1, 12):
                z, mu, logvar, skip, fmap_shape = self._vae.encoder(x)
                self._last_skip = skip
                self._last_fmap_shape = fmap_shape
                return z, mu, logvar

            else:
                out = self._vae.encoder(x)
                if isinstance(out, tuple) and len(out) >= 3:
                    z, mu, logvar = out[0], out[1], out[2]
                    if len(out) > 3:
                        self._last_skip = out[3]
                    if len(out) > 4:
                        self._last_fmap_shape = out[4]
                    return z, mu, logvar
                else:
                    raise RuntimeError(
                        f"Encoder for strategy {self._strategy} does not return "
                        "mu and logvar; encode_full() not supported."
                    )

    def decode(
        self,
        z: torch.Tensor,
        cond=None,
    ) -> torch.Tensor:
        """
        Decode latent vector to output.

        Uses cached skip connections from the last encode call.

        Args:
            z: Latent tensor of shape (B, latent_dim)
            cond: Optional conditioning (tensor or dict — see ``encode``).

        Returns:
            Reconstructed output of shape (B, C, H, W)
        """
        self._vae.decoder.eval()

        with torch.no_grad():
            z = z.to(self._device)
            cond = _cond_to_device(cond, self._device)

            if self._strategy in (14, 14.1, 15, 16, 17):
                # Tendril VAE: decoder(z, skip, cond)
                return self._vae.decoder(z, self._last_skip, cond)

            elif self._strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13):
                # Conditional with feature map shape
                return self._vae.decoder(
                    z, self._last_skip, self._last_fmap_shape, cond
                )

            elif self._strategy in (7, 8, 9):
                # Conditional VAE
                return self._vae.decoder(
                    z, self._last_skip, self._last_fmap_shape, cond
                )

            elif self._strategy in (1, 12):
                # U-Net VAE
                return self._vae.decoder(z, self._last_skip, self._last_fmap_shape)

            else:
                # Fallback
                if self._last_skip is not None:
                    return self._vae.decoder(
                        z, self._last_skip, self._last_fmap_shape
                    )
                return self._vae.decoder(z)

    def get_latent_dim(self) -> int:
        """Return latent dimension."""
        return self._latent_dim

    @property
    def model_info(self) -> Dict[str, Any]:
        """Return model metadata."""
        info = {
            'name': self._metadata.get('name', 'TLV Model'),
            'version': self._metadata.get('version', 'unknown'),
            'training_date': self._metadata.get('training_date', 'unknown'),
            'description': self._metadata.get('description', ''),
            'architecture': getattr(self._config, 'architecture', 'unknown'),
            'strategy': self._strategy,
            'latent_dim': self._latent_dim,
            'is_production': self._metadata.get('is_production', False),
            'has_tendrils': self.has_tendrils(),
        }
        if self.has_tendrils():
            info['tendril_info'] = self.get_tendril_info()
        return info

    @property
    def device(self) -> torch.device:
        """Return device."""
        return self._device

    def to(self, device: torch.device) -> "TLVModelWrapper":
        """Move model to device."""
        self._device = device
        self._vae = self._vae.to(device)
        return self

    def eval(self) -> "TLVModelWrapper":
        """Set to evaluation mode."""
        self._vae.eval()
        return self

    def get_last_skip(self) -> Optional[List[torch.Tensor]]:
        """Return the cached skip connections from the last encode call."""
        return self._last_skip

    def decode_with_skip(
        self,
        z: torch.Tensor,
        skip: List[torch.Tensor],
        cond: Optional[torch.Tensor] = None
    ) -> torch.Tensor:
        """
        Decode latent vector using explicit skip connections.

        This allows decoding with interpolated skip connections rather than
        the cached ones from the last encode call.

        Args:
            z: Latent tensor of shape (B, latent_dim)
            skip: List of skip connection tensors [x1, x2, x3, x4]
            cond: Optional conditioning tensor

        Returns:
            Reconstructed output of shape (B, C, H, W)
        """
        self._vae.decoder.eval()

        with torch.no_grad():
            z = z.to(self._device)
            if cond is not None:
                cond = cond.to(self._device).float()

            if self._strategy in (14, 14.1, 15, 16, 17):
                # Tendril VAE: decoder(z, skip, cond)
                return self._vae.decoder(z, skip, cond)

            elif self._strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13):
                return self._vae.decoder(z, skip, self._last_fmap_shape, cond)

            elif self._strategy in (7, 8, 9):
                return self._vae.decoder(z, skip, self._last_fmap_shape, cond)

            elif self._strategy in (1, 12):
                return self._vae.decoder(z, skip, self._last_fmap_shape)

            else:
                if skip is not None:
                    return self._vae.decoder(z, skip, self._last_fmap_shape)
                return self._vae.decoder(z)

    def encode_and_decode(
        self,
        x: torch.Tensor,
        cond_encode: Any = None,
        cond_decode: Any = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Encode ``x`` with ``cond_encode`` then decode with ``cond_decode``.

        Snapshots and restores ``self._last_skip`` / ``self._last_fmap_shape``
        so outer callers that relied on a prior encode are not disturbed.
        ``cond_decode`` defaults to ``cond_encode`` — pass them as
        different dicts (or one as a dict and one as a modified copy) to
        probe the FiLM conditioning pathway independently of the
        encoder/decoder.

        Args:
            x: Input tensor of shape (B, C, H, W).
            cond_encode: Conditioning passed to the encoder (tensor or
                dict for multi-cond strategies).
            cond_decode: Conditioning passed to the decoder. Defaults to
                ``cond_encode``.

        Returns:
            Tuple of ``(recon, z, mu, logvar)``.
        """
        if cond_decode is None:
            cond_decode = cond_encode

        prior_skip = self._last_skip
        prior_fmap = self._last_fmap_shape
        try:
            z, mu, logvar = self.encode_full(x, cond_encode)
            recon = self.decode(z, cond_decode)
        finally:
            self._last_skip = prior_skip
            self._last_fmap_shape = prior_fmap

        return recon, z, mu, logvar


    # ------------------------------------------------------------------
    # Tendril methods
    # ------------------------------------------------------------------

    def has_tendrils(self) -> bool:
        """Return True if tendril sub-VAEs are loaded."""
        return self._tendrils is not None and len(self._tendrils.list_tendrils()) > 0

    def list_tendril_keys(self) -> List[str]:
        """Return ordered list of tendril keys (e.g. ['x1', 'x2', 'x3', 'x4'])."""
        if self._tendrils is None:
            return []
        return self._tendrils.list_tendrils()

    def get_tendril_latent_dim(self, key: str) -> int:
        """Return the latent dimension for a specific tendril."""
        trainer = self._tendrils.get_tendril(key)
        return trainer.latent_dim

    def get_tendril_info(self) -> Dict[str, Dict]:
        """Return metadata about all loaded tendrils."""
        if self._tendrils is None:
            return {}
        info = {}
        for key in self.list_tendril_keys():
            trainer = self._tendrils.get_tendril(key)
            info[key] = {
                "input_shape": list(trainer.input_shape)
                if hasattr(trainer, 'input_shape')
                else list(trainer.model.input_shape),
                "latent_dim": trainer.latent_dim
                if hasattr(trainer, 'latent_dim')
                else trainer.model.fc_mu.out_features,
            }
        return info

    def encode_tendril(
        self,
        key: str,
        skip_feature: torch.Tensor,
        cond: Optional[Any] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Encode a skip connection feature map through the corresponding tendril VAE.

        Args:
            key: Tendril key (e.g. 'x1')
            skip_feature: Feature map tensor from the encoder skip connection
            cond: Conditioning input. For unconditioned tendrils this is
                ignored. For conditioned tendrils (Strategy 16) this
                should be the outer primary cond dict; this method
                filters it down to the tendril's own cond_keys.

        Returns:
            Tuple of (x_hat, mu, logvar, z)
        """
        trainer = self._tendrils.get_tendril(key)
        trainer.model.eval()
        tendril_cond_keys = tuple(
            getattr(trainer, 'cond_keys', None)
            or getattr(trainer.model, 'cond_keys', ())
            or ()
        )
        tendril_cond: Optional[Dict[str, torch.Tensor]] = None
        if tendril_cond_keys and isinstance(cond, dict):
            tendril_cond = {
                k: cond[k] for k in tendril_cond_keys if k in cond
            }
            # If the caller provided no cond entries for the active keys,
            # fall back to None so the VAE runs its no-FiLM path.
            if not tendril_cond:
                tendril_cond = None
        with torch.no_grad():
            # Normalize per-image — must match training normalization.
            normalized = self._tendrils.normalize_per_image(skip_feature)
            x_hat, mu, logvar, z = trainer.model(normalized, tendril_cond)
        return x_hat, mu, logvar, z

    def decode_tendril(
        self,
        key: str,
        z_tendril: torch.Tensor,
        cond: Optional[Any] = None,
    ) -> torch.Tensor:
        """
        Decode a tendril latent vector back to a feature map.

        Args:
            key: Tendril key
            z_tendril: Tendril latent vector
            cond: Conditioning input. For unconditioned tendrils this is
                ignored. For conditioned tendrils (Strategy 16) this
                should be the outer primary cond dict; this method
                filters it down to the tendril's own cond_keys and
                forwards it through the FiLM-aware decoder path.

        Returns:
            Reconstructed feature map
        """
        trainer = self._tendrils.get_tendril(key)
        trainer.model.eval()
        tendril_cond_keys = tuple(
            getattr(trainer, 'cond_keys', None)
            or getattr(trainer.model, 'cond_keys', ())
            or ()
        )
        tendril_cond: Optional[Dict[str, torch.Tensor]] = None
        if tendril_cond_keys and isinstance(cond, dict):
            tendril_cond = {
                k: cond[k] for k in tendril_cond_keys if k in cond
            }
            if not tendril_cond:
                tendril_cond = None
        with torch.no_grad():
            x_hat = trainer.model.decode(z_tendril, tendril_cond)
        return x_hat

    def decode_with_tendril_modification(
        self,
        key: str,
        z_tendril: torch.Tensor,
        base_skip: List[torch.Tensor],
        z_primary: torch.Tensor,
        cond: Optional[torch.Tensor] = None,
    ) -> torch.Tensor:
        """
        Decode using modified skip connections from a tendril latent vector.

        Replaces the skip connection at the tendril's level with the
        tendril-decoded feature map, then decodes through the main VAE.

        Args:
            key: Tendril key (e.g. 'x1')
            z_tendril: Tendril latent vector to decode
            base_skip: Base skip connections [x1, x2, x3, x4]
            z_primary: Primary latent vector for the main decoder
            cond: Optional conditioning tensor

        Returns:
            Reconstructed image
        """
        tendril_keys = self.list_tendril_keys()
        tendril_idx = tendril_keys.index(key)

        reconstructed_feature = self.decode_tendril(key, z_tendril)
        modified_skip = list(base_skip)
        modified_skip[tendril_idx] = reconstructed_feature

        return self.decode_with_skip(z_primary, modified_skip, cond)

    def encode_all_latent_spaces(
        self,
        x: torch.Tensor,
        cond: Optional[torch.Tensor] = None,
    ) -> Dict[str, torch.Tensor]:
        """
        Encode input through primary and all tendril encoders.

        Args:
            x: Input images
            cond: Optional conditioning tensor

        Returns:
            Dict mapping space name to latent vectors,
            e.g. {"primary": z, "x1": z_t1, ...}
        """
        z = self.encode(x, cond)
        result = {"primary": z}

        if self.has_tendrils():
            skip = self.get_last_skip()
            for i, key in enumerate(self.list_tendril_keys()):
                _, _, _, z_t = self.encode_tendril(key, skip[i], cond=cond)
                result[key] = z_t

        return result


def load_tlv_model(
    model_path: Union[str, Path],
    config_path: Optional[Union[str, Path]] = None,
    device: Optional[torch.device] = None,
    metadata: Optional[Dict] = None
) -> TLVModelWrapper:
    """
    Load a TLV model from checkpoint.

    Args:
        model_path: Path to model.pth file
        config_path: Optional path to config.json or args.json
        device: Device to load model on (auto-detects if None)
        metadata: Optional metadata to attach

    Returns:
        TLVModelWrapper instance

    Raises:
        FileNotFoundError: If model file doesn't exist
        RuntimeError: If model loading fails
    """
    model_path = Path(model_path)
    if not model_path.exists():
        raise FileNotFoundError(f"Model not found: {model_path}")

    # Determine device (use cuda:0 to avoid conflicts with training on other GPUs)
    if device is None:
        device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

    # Try to load config
    config = _load_config(model_path.parent, config_path)
    config.device = device

    # Create VAE architecture
    vae = _create_vae_from_config(config)

    # Load weights: use CPU first to avoid multi-GPU device conflicts,
    # then move model to target device
    state_dict = torch.load(model_path, map_location="cpu")
    vae.load_state_dict(state_dict)
    vae = vae.to(device)
    vae.eval()

    # Update config device
    config.device = device

    # Load metadata if available
    if metadata is None:
        metadata = _load_metadata(model_path.parent)

    logger.info(f"Loaded TLV model from {model_path}")

    wrapper = TLVModelWrapper(vae, config, metadata)

    # Load tendrils if available
    tendrils_path = model_path.parent / "tendrils"
    if tendrils_path.exists() and (tendrils_path / "tendrils_meta.json").exists():
        try:
            from candescence.tlv.architectures.tendril_sub import Tendrils
            tendrils = Tendrils.load(tendrils_path, device)
            wrapper._tendrils = tendrils
            logger.info(f"Loaded tendrils from {tendrils_path}: {tendrils.list_tendrils()}")
        except Exception as e:
            logger.warning(f"Failed to load tendrils from {tendrils_path}: {e}")

    return wrapper


def _load_config(model_dir: Path, config_path: Optional[Path] = None) -> Any:
    """Load configuration from directory."""
    from candescence.core.config import TLVConfig

    # Prefer args.json — it carries the full TLVConfig (incl. Strategy 15
    # day/media category vocabularies). config.json is a minimal fallback.
    if config_path is None:
        for name in ['args.json', 'config.json']:
            candidate = model_dir / name
            if candidate.exists():
                config_path = candidate
                break

    if config_path is not None and Path(config_path).exists():
        with open(config_path) as f:
            config_dict = json.load(f)

        # Create a minimal config
        config = TLVConfig.__new__(TLVConfig)
        for key, value in config_dict.items():
            setattr(config, key, value)

        return config

    # Default config
    logger.warning("No config found, using defaults")
    config = TLVConfig.__new__(TLVConfig)
    config.strategy = 14
    config.architecture = 'tendril_vae'
    config.latent_dim = 128
    config.cond_dim = 1
    return config


def _create_vae_from_config(config: Any) -> torch.nn.Module:
    """Create VAE architecture from config."""
    from candescence.tlv.architectures import (
        tendril_VAE, uc_VAE, c_VAE, cond_uc_VAE, multi_cond_tendril_VAE,
        cond_uc_VAE_attention, cond_uc_VAE_spatial,
    )

    architecture = getattr(config, 'architecture', 'tendril_vae')

    hsv_set = {'average_hue', 'average_saturation', 'average_value'}
    cond_vars = getattr(config, 'conditional_variables', []) or []

    # Build args dict for architecture
    args = {
        'latent_dim': getattr(config, 'latent_dim', 128),
        'intermediate_dim': getattr(config, 'intermediate_dim', 256),
        '_cond_dim': getattr(config, 'cond_dim', 1),
        'leaky_relu_slope': getattr(config, 'leaky_relu_slope', 0.02),
        'image_dimension': getattr(config, 'image_dimension', 128),
        'kernel_size': getattr(config, 'kernel_size', 3),
        'strategy': getattr(config, 'strategy', 14),
        'DEVICE': getattr(config, 'device', 'cuda:0'),
        # Strategies 15 & 16 per-variable cond dims. When loading a
        # pre-Strategy-15 checkpoint these keys are simply unused.
        '_cond_dim_hue': sum(1 for v in cond_vars if v in hsv_set),
        '_cond_dim_day': len(getattr(config, 'day_categories', []) or []),
        '_cond_dim_media': len(getattr(config, 'media_categories', []) or []),
        '_cond_dim_plate_phys': len(
            getattr(config, 'plate_phys_categories', []) or []
        ),
    }
    # Strategy 16 drops the day FiLM pathway; persisted on args.json via
    # ``cond_keys``. Falling back to ``None`` preserves Strategy 15's
    # default (hue+day+media) for older checkpoints.
    cond_keys = getattr(config, 'cond_keys', None)
    if cond_keys is not None:
        args['_cond_keys'] = list(cond_keys)

    architecture_map = {
        'tendril_vae': tendril_VAE,
        'multi_cond_tendril_vae': multi_cond_tendril_VAE,
        'uc_vae': uc_VAE,
        'c_vae': c_VAE,
        'cond_uc_vae': cond_uc_VAE,
        'cond_uc_vae_attention': cond_uc_VAE_attention,
        'cond_uc_vae_spatial': cond_uc_VAE_spatial,
    }
    # cond_uc_vae* architectures access args['conditional_variables']
    # directly (no .get()), so we must surface it from config.
    args['conditional_variables'] = (
        getattr(config, 'conditional_variables', None) or []
    )
    args['reduction_ratio'] = getattr(config, 'reduction_ratio', 16)

    if architecture not in architecture_map:
        logger.warning(f"Unknown architecture {architecture}, using tendril_vae")
        architecture = 'tendril_vae'

    vae_class = architecture_map[architecture]
    return vae_class(args)


def _load_metadata(model_dir: Path) -> Dict[str, Any]:
    """Load or generate model metadata."""
    metadata_path = model_dir / 'metadata.json'

    if metadata_path.exists():
        with open(metadata_path) as f:
            return json.load(f)

    # Generate from directory structure
    return {
        'name': model_dir.name,
        'version': '1.0.0',
        'training_date': 'unknown',
        'description': f'Model from {model_dir}',
        'is_production': 'production' in str(model_dir).lower(),
    }
