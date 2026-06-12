"""Pairwise distance matrices over the image space.

Used by the latent explorer's Mantel-vs-image analysis to compare a latent
embedding's geometry against:

- raw pixel space (flattened images, Euclidean)
- hand-crafted feature space (output of
  :func:`candescence.tlv.analysis.image_feature_table.compute_image_features_batch`)

Both functions return dense (N, N) symmetric distance matrices with zero
diagonal so they can be passed directly to
:func:`candescence.tlv.analysis.latent_space_metrics.mantel_latent_vs_matrix`.
"""

from typing import Optional

import numpy as np
from scipy.spatial.distance import pdist, squareform


def _flatten_to_nd(images: np.ndarray) -> np.ndarray:
    """Reshape any (N, ...) image batch to (N, D) float64 in [0, 1].

    Accepts ``(N, C, H, W)``, ``(N, H, W, C)``, ``(N, H, W)``. uint8 inputs
    are scaled by 1/255; float inputs > 1 are also scaled.
    """
    if images.ndim < 2:
        raise ValueError(f"images must have ndim >= 2; got {images.shape}")
    arr = images.astype(np.float64, copy=False)
    if arr.dtype == np.uint8 or arr.max() > 1.5:
        arr = arr / 255.0
    return arr.reshape(arr.shape[0], -1)


def raw_pixel_distance_matrix(
    images: np.ndarray,
    *,
    mode: str = "auto",
) -> np.ndarray:
    """Pairwise Euclidean distance over flattened pixels.

    Parameters
    ----------
    images : (N, ...) image batch.
    mode : kept for forward compatibility; only ``"auto"`` (default,
        Euclidean over flattened pixels) is currently supported.

    Returns
    -------
    Symmetric ``(N, N) float32`` distance matrix with zero diagonal.
    """
    if mode != "auto":
        raise ValueError(f"unsupported mode '{mode}'")
    flat = _flatten_to_nd(images)
    D = squareform(pdist(flat, metric="euclidean"))
    np.fill_diagonal(D, 0.0)
    return ((D + D.T) / 2.0).astype(np.float32)


def feature_distance_matrix(
    images: np.ndarray,
    *,
    standardize: bool = True,
) -> np.ndarray:
    """Pairwise Euclidean distance over hand-crafted image features.

    Uses
    :func:`candescence.tlv.analysis.image_feature_table.compute_image_features_batch`
    and (by default) z-score standardises each feature column before
    computing distances so heterogeneous scales don't dominate.

    Parameters
    ----------
    images : (N, ...) image batch accepted by ``compute_image_features_batch``.
    standardize : if True, z-score features column-wise before pdist.

    Returns
    -------
    Symmetric ``(N, N) float32`` distance matrix with zero diagonal.
    """
    from candescence.tlv.analysis.image_feature_table import (
        compute_image_features_batch,
    )

    feat_df = compute_image_features_batch(images)
    feats = feat_df.to_numpy(dtype=np.float64)
    if standardize:
        mu = feats.mean(axis=0, keepdims=True)
        sd = feats.std(axis=0, ddof=0, keepdims=True)
        sd[sd == 0] = 1.0
        feats = (feats - mu) / sd
    D = squareform(pdist(feats, metric="euclidean"))
    np.fill_diagonal(D, 0.0)
    return ((D + D.T) / 2.0).astype(np.float32)
