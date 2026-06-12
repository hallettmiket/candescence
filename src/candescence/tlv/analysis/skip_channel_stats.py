"""Per-channel statistics and PCA summary for skip connection feature maps.

Encodes a batch of images, collects skip tensors on CPU, and computes
per-channel variance, spatial entropy, dead-channel fraction, and PCA
effective dimensionality for each skip level.
"""

from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import torch
from sklearn.decomposition import PCA


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def compute_skip_channel_stats(
    model: "TLVModelWrapper",
    images: np.ndarray,
    conditioning: Optional[np.ndarray],
    *,
    batch_size: int = 32,
    max_samples: Optional[int] = None,
) -> Dict[int, pd.DataFrame]:
    """Compute per-channel statistics for each skip level.

    Follows the same GPU-safe pattern as
    :meth:`SkipLogger.compute_skip_connections`: encode in small batches,
    move skip tensors to CPU immediately, and call
    ``torch.cuda.empty_cache()`` between batches.

    Args:
        model: Loaded :class:`TLVModelWrapper`.
        images: ``(N, C, H, W)`` float32 array.
        conditioning: ``(N, cond_dim)`` or ``None``.
        batch_size: Images per forward pass.
        max_samples: Cap on the number of images to process.

    Returns:
        Dict mapping layer index (0–3) to a DataFrame with one row per
        channel and columns: ``mean``, ``variance``, ``spatial_entropy``,
        ``dead`` (bool, variance < 1e-8).
    """
    device = model.device
    n = len(images)
    if max_samples is not None:
        n = min(n, max_samples)

    # Collect skip tensors on CPU layer-by-layer
    skip_accum: Dict[int, List[torch.Tensor]] = {}

    with torch.no_grad():
        for start in range(0, n, batch_size):
            end = min(start + batch_size, n)
            x = torch.tensor(images[start:end], dtype=torch.float32).to(device)

            cond = None
            if conditioning is not None:
                cond = torch.tensor(
                    conditioning[start:end], dtype=torch.float32,
                ).to(device)

            model.encode(x, cond)
            skip_list = model.get_last_skip()

            for layer_idx, skip_tensor in enumerate(skip_list):
                if layer_idx not in skip_accum:
                    skip_accum[layer_idx] = []
                skip_accum[layer_idx].append(skip_tensor.detach().cpu())

            if torch.cuda.is_available():
                torch.cuda.empty_cache()

    # Concatenate and compute stats
    results: Dict[int, pd.DataFrame] = {}
    for layer_idx in sorted(skip_accum.keys()):
        cat = torch.cat(skip_accum[layer_idx], dim=0).numpy()  # (N, C, H, W)
        results[layer_idx] = _channel_stats(cat)

    return results


def skip_pca_summary(
    model: "TLVModelWrapper",
    images: np.ndarray,
    conditioning: Optional[np.ndarray],
    layer_idx: int,
    *,
    batch_size: int = 32,
    max_samples: Optional[int] = None,
    max_components: int = 32,
) -> np.ndarray:
    """PCA explained-variance curve for the spatially-pooled channels of a skip level.

    For each image, the skip tensor at *layer_idx* is global-average-pooled
    over spatial dims → ``(C,)`` vector.  PCA is fit on the ``(N, C)``
    matrix and the explained-variance ratios are returned.

    Args:
        model: Loaded :class:`TLVModelWrapper`.
        images: ``(N, C, H, W)`` float32 array.
        conditioning: ``(N, cond_dim)`` or ``None``.
        layer_idx: Which skip layer (0–3).
        batch_size: Images per forward pass.
        max_samples: Cap on number of images.
        max_components: Max PCA components.

    Returns:
        1-D array of explained-variance ratios (length ≤ *max_components*).
    """
    device = model.device
    n = len(images)
    if max_samples is not None:
        n = min(n, max_samples)

    pooled_batches: List[np.ndarray] = []

    with torch.no_grad():
        for start in range(0, n, batch_size):
            end = min(start + batch_size, n)
            x = torch.tensor(images[start:end], dtype=torch.float32).to(device)

            cond = None
            if conditioning is not None:
                cond = torch.tensor(
                    conditioning[start:end], dtype=torch.float32,
                ).to(device)

            model.encode(x, cond)
            skip_list = model.get_last_skip()
            skip = skip_list[layer_idx].detach().cpu().numpy()  # (B, C, H, W)
            pooled = skip.mean(axis=(2, 3))  # (B, C)
            pooled_batches.append(pooled)

            if torch.cuda.is_available():
                torch.cuda.empty_cache()

    pooled_all = np.concatenate(pooled_batches, axis=0)  # (N, C)
    n_components = min(max_components, pooled_all.shape[0] - 1, pooled_all.shape[1])
    if n_components < 1:
        return np.array([1.0])
    pca = PCA(n_components=n_components)
    pca.fit(pooled_all)
    return pca.explained_variance_ratio_


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _channel_stats(skip: np.ndarray) -> pd.DataFrame:
    """Compute per-channel stats from a ``(N, C, H, W)`` numpy array.

    Returns DataFrame with columns: mean, variance, spatial_entropy, dead.
    """
    n, c, h, w = skip.shape
    means = skip.mean(axis=(0, 2, 3))       # (C,)
    variances = skip.var(axis=(0, 2, 3))     # (C,)

    # Spatial entropy: for each channel, build histogram of activations
    # across all images and spatial positions, then compute Shannon H.
    entropies = np.empty(c, dtype=np.float64)
    for ch in range(c):
        vals = skip[:, ch, :, :].ravel()
        entropies[ch] = _discretised_entropy(vals)

    dead = variances < 1e-8

    return pd.DataFrame({
        "channel": np.arange(c),
        "mean": means,
        "variance": variances,
        "spatial_entropy": entropies,
        "dead": dead,
    }).set_index("channel")


def _discretised_entropy(vals: np.ndarray, n_bins: int = 64) -> float:
    """Shannon entropy of a discretised activation distribution."""
    counts, _ = np.histogram(vals, bins=n_bins)
    probs = counts / max(counts.sum(), 1)
    probs = probs[probs > 0]
    return float(-np.sum(probs * np.log2(probs)))
