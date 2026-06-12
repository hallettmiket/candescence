"""Latent traversals in tendril (skip) space.

Walk along a single tendril latent dimension, decode each step back to a
full image via ``decode_with_tendril_modification``, and optionally return
the intermediate feature maps for visualisation.
"""

from typing import List, Optional, Tuple

import numpy as np
import torch
from sklearn.decomposition import PCA


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def tendril_latent_walk(
    model: "TLVModelWrapper",
    image_idx: int,
    images: np.ndarray,
    conditioning: Optional[np.ndarray],
    tendril_key: str,
    dim: int,
    n_steps: int = 5,
    step_size: float = 1.0,
    *,
    return_feature_maps: bool = False,
) -> Tuple[List[np.ndarray], List[float], Optional[List[np.ndarray]]]:
    """Walk along one tendril latent dimension and decode back to pixel space.

    Args:
        model: Loaded :class:`TLVModelWrapper` instance.
        image_idx: Index into *images* / *conditioning* arrays.
        images: ``(N, C, H, W)`` image array (float32, [0,1]).
        conditioning: ``(N, cond_dim)`` or ``None``.
        tendril_key: e.g. ``'x1'``, ``'x2'``, etc.
        dim: Tendril latent dimension to traverse (0-based).
        n_steps: Steps per side (total = ``2 * n_steps + 1``).
        step_size: Step size in latent units.
        return_feature_maps: If *True*, also return the tendril-decoded
            feature maps (before main decoder) for each step.

    Returns:
        ``(decoded_images, offsets, feature_maps)``

        - *decoded_images*: list of ``(C, H, W)`` numpy arrays.
        - *offsets*: list of float offset values (low → high).
        - *feature_maps*: list of ``(C, Hf, Wf)`` arrays or ``None``.
    """
    device = model.device

    # Prepare input tensors
    img_tensor = torch.tensor(
        images[image_idx : image_idx + 1], dtype=torch.float32,
    ).to(device)
    cond_tensor = None
    if conditioning is not None:
        cond_tensor = torch.tensor(
            conditioning[image_idx : image_idx + 1], dtype=torch.float32,
        ).to(device)

    # Encode: get primary z and skip connections
    with torch.no_grad():
        z_primary = model.encode(img_tensor, cond_tensor)
        skip = model.get_last_skip()

        # Encode the relevant skip through the tendril
        tendril_keys = model.list_tendril_keys()
        tendril_idx = tendril_keys.index(tendril_key)
        skip_feature = skip[tendril_idx]
        _, mu_tendril, _, _ = model.encode_tendril(
            tendril_key, skip_feature, cond_tensor,
        )

    base_mu = mu_tendril.cpu().numpy().squeeze()  # (latent_dim,)

    offsets = [
        round(-n_steps * step_size + i * step_size, 4)
        for i in range(2 * n_steps + 1)
    ]

    decoded_images: List[np.ndarray] = []
    feature_maps: Optional[List[np.ndarray]] = [] if return_feature_maps else None

    with torch.no_grad():
        for off in offsets:
            z = base_mu.copy()
            z[dim] += off
            z_t = torch.tensor(z, dtype=torch.float32).unsqueeze(0).to(device)

            # Full image via main decoder with modified skip
            decoded = model.decode_with_tendril_modification(
                tendril_key, z_t, skip, z_primary, cond_tensor,
            )
            decoded_images.append(decoded.cpu().numpy().squeeze())

            if return_feature_maps:
                fmap = model.decode_tendril(tendril_key, z_t, cond_tensor)
                feature_maps.append(fmap.cpu().numpy().squeeze())

    return decoded_images, offsets, feature_maps


# ---------------------------------------------------------------------------
# Visualisation helpers (shared with skip_channel_stats)
# ---------------------------------------------------------------------------


def skip_channels_to_rgb(
    feature_map: np.ndarray,
    method: str = "pca",
) -> np.ndarray:
    """Project a multi-channel feature map to an RGB image for display.

    Args:
        feature_map: ``(C, H, W)`` array.
        method: ``"pca"`` (PCA to 3 components) or ``"norm"`` (L2 norm
            across channels → grayscale).

    Returns:
        ``(H, W, 3)`` uint8 image in [0, 255].
    """
    c, h, w = feature_map.shape

    if method == "norm":
        # L2 norm across channels → single-channel heatmap
        norm_map = np.linalg.norm(feature_map, axis=0)  # (H, W)
        norm_map = _minmax_scale(norm_map)
        return _gray_to_rgb(norm_map)

    # PCA to 3 components
    flat = feature_map.reshape(c, h * w).T  # (H*W, C)
    n_components = min(3, c, h * w)
    pca = PCA(n_components=n_components)
    projected = pca.fit_transform(flat)  # (H*W, 3)
    rgb = np.zeros((h * w, 3), dtype=np.float64)
    rgb[:, :n_components] = projected[:, :n_components]

    # Per-channel min-max scale to [0, 255]
    for ch in range(3):
        rgb[:, ch] = _minmax_scale(rgb[:, ch])

    return (rgb.reshape(h, w, 3) * 255).astype(np.uint8)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _minmax_scale(arr: np.ndarray) -> np.ndarray:
    """Scale array to [0, 1]."""
    lo, hi = float(arr.min()), float(arr.max())
    if hi - lo < 1e-12:
        return np.zeros_like(arr)
    return (arr - lo) / (hi - lo)


def _gray_to_rgb(gray: np.ndarray) -> np.ndarray:
    """Convert (H, W) float [0, 1] to (H, W, 3) uint8."""
    g = (gray * 255).astype(np.uint8)
    return np.stack([g, g, g], axis=-1)
