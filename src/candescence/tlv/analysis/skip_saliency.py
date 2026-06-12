"""Gradient-based saliency maps for skip connections and tendril latents.

Computes vanilla saliency (|∂target/∂x|) showing which input pixels most
affect a chosen skip level's activation or a tendril's latent μ.

Because :meth:`TLVModelWrapper.encode` wraps everything in
``torch.no_grad()``, these functions call the underlying
``model._vae.encoder`` directly inside ``torch.enable_grad()``.
"""

from typing import Optional

import numpy as np
import torch


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def compute_skip_saliency(
    model: "TLVModelWrapper",
    images: np.ndarray,
    conditioning: Optional[np.ndarray],
    image_idx: int,
    layer_idx: int = 0,
    target: str = "norm",
) -> np.ndarray:
    """Compute input-pixel saliency w.r.t. a skip connection layer.

    Args:
        model: Loaded :class:`TLVModelWrapper`.
        images: ``(N, C, H, W)`` float32 array.
        conditioning: ``(N, cond_dim)`` or ``None``.
        image_idx: Which image to compute saliency for.
        layer_idx: Skip layer index (0 = x1 … 3 = x4).
        target: Scalar target derived from the skip tensor.
            ``"norm"`` — L2 norm of all activations.
            ``"mean"`` — mean of all activations.

    Returns:
        ``(H, W)`` float32 saliency map (absolute gradient magnitude).
    """
    device = model.device
    vae = model._vae
    strategy = model._strategy

    x = torch.tensor(
        images[image_idx : image_idx + 1], dtype=torch.float32,
    ).to(device)

    cond = None
    if conditioning is not None:
        cond = torch.tensor(
            conditioning[image_idx : image_idx + 1], dtype=torch.float32,
        ).to(device)

    # We need gradients through the encoder
    was_training = vae.encoder.training
    vae.encoder.eval()

    with torch.enable_grad():
        x = x.detach().requires_grad_(True)

        encoder_out = _call_encoder(vae.encoder, x, cond, strategy)
        skip_list = _extract_skip(encoder_out, strategy)
        skip = skip_list[layer_idx]

        scalar = _target_scalar(skip, target)
        scalar.backward()

        grad = x.grad.detach()  # (1, C, H, W)

    if not was_training:
        vae.encoder.eval()

    # Absolute gradient summed over channels → (H, W)
    saliency = grad.abs().squeeze(0).sum(dim=0).cpu().numpy()  # (H, W)
    return saliency.astype(np.float32)


def compute_tendril_mu_saliency(
    model: "TLVModelWrapper",
    images: np.ndarray,
    conditioning: Optional[np.ndarray],
    image_idx: int,
    tendril_key: str,
    target: str = "norm",
) -> np.ndarray:
    """Compute input-pixel saliency w.r.t. a tendril encoder's μ.

    This forward-passes through the main encoder to get the skip, then
    through the tendril encoder to get μ, and backpropagates to the input.

    Args:
        model: Loaded :class:`TLVModelWrapper`.
        images: ``(N, C, H, W)`` float32 array.
        conditioning: ``(N, cond_dim)`` or ``None``.
        image_idx: Which image.
        tendril_key: e.g. ``"x1"``.
        target: ``"norm"`` or ``"mean"`` of μ.

    Returns:
        ``(H, W)`` float32 saliency map.
    """
    device = model.device
    vae = model._vae
    strategy = model._strategy

    x = torch.tensor(
        images[image_idx : image_idx + 1], dtype=torch.float32,
    ).to(device)

    cond = None
    if conditioning is not None:
        cond = torch.tensor(
            conditioning[image_idx : image_idx + 1], dtype=torch.float32,
        ).to(device)

    tendril_keys = model.list_tendril_keys()
    tendril_idx = tendril_keys.index(tendril_key)
    tendril_vae = model._tendrils.get_tendril(tendril_key)

    was_training_enc = vae.encoder.training
    was_training_tendril = tendril_vae.training if hasattr(tendril_vae, 'training') else False
    vae.encoder.eval()
    if hasattr(tendril_vae, 'eval'):
        tendril_vae.eval()

    with torch.enable_grad():
        x = x.detach().requires_grad_(True)

        # Forward through main encoder
        encoder_out = _call_encoder(vae.encoder, x, cond, strategy)
        skip_list = _extract_skip(encoder_out, strategy)
        skip_feature = skip_list[tendril_idx]

        # Forward through tendril encoder
        tendril_out = _call_tendril_encoder(tendril_vae, skip_feature, cond, strategy)
        mu = tendril_out  # (B, latent_dim)

        scalar = _target_scalar(mu, target)
        scalar.backward()

        grad = x.grad.detach()

    if not was_training_enc:
        vae.encoder.eval()
    if hasattr(tendril_vae, 'eval') and not was_training_tendril:
        tendril_vae.eval()

    saliency = grad.abs().squeeze(0).sum(dim=0).cpu().numpy()
    return saliency.astype(np.float32)


# ---------------------------------------------------------------------------
# Internal: strategy-aware encoder calls
# ---------------------------------------------------------------------------


def _call_encoder(encoder, x, cond, strategy):
    """Call the encoder with the correct signature for the strategy."""
    if strategy in (14, 15, 16):
        return encoder(x, cond)
    elif strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13):
        cond_list = [cond, cond] if cond is not None else None
        return encoder(x, cond_list)
    elif strategy in (7, 8, 9):
        return encoder(x, cond)
    elif strategy in (1, 12):
        return encoder(x)
    else:
        return encoder(x)


def _extract_skip(encoder_out, strategy):
    """Extract the skip list from an encoder output tuple."""
    if strategy in (14, 15, 16):
        # (z, mu, logvar, skip)
        return encoder_out[3]
    else:
        # (z, mu, logvar, skip, fmap_shape) or similar
        return encoder_out[3]


def _call_tendril_encoder(tendril_vae, skip_feature, cond, strategy):
    """Forward skip_feature through a tendril encoder, returning μ.

    The tendril VAE's encoder is a small net: conv → flatten → fc_mu / fc_logvar.
    We call the full forward to get μ — the second element of the return tuple
    from ``encode_tendril``, which internally does:
    ``(x_hat, mu, logvar, z) = tendril.forward(skip_feature, ...)``.
    """
    # TendrilLayerVAE .forward(x) — both Strategy 14 and 15 use plain
    # (unconditioned) inner tendrils.
    out = tendril_vae(skip_feature)

    # out is typically (x_hat, mu, logvar, z)
    if isinstance(out, tuple) and len(out) >= 2:
        return out[1]  # mu
    return out


def _target_scalar(tensor: torch.Tensor, target: str) -> torch.Tensor:
    """Reduce a tensor to a scalar target for backpropagation."""
    if target == "mean":
        return tensor.mean()
    # Default: L2 norm
    return tensor.norm()
