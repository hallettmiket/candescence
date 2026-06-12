"""Compute cheap per-image feature vectors from an (N, C, H, W) or (N, H, W, C) tensor.

Features are aligned row-for-row with the embedding / metadata arrays so they
can be correlated with PC scores or used as covariates.
"""

from typing import List, Tuple

import numpy as np
import pandas as pd


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

FEATURE_NAMES: List[str] = [
    "luminance_mean",
    "luminance_std",
    "channel_r_mean",
    "channel_g_mean",
    "channel_b_mean",
    "colony_area_frac",
    "bbox_aspect_ratio",
    "centroid_offset",
]


def compute_image_features_batch(
    images: np.ndarray,
    *,
    threshold_percentile: float = 25.0,
) -> pd.DataFrame:
    """Extract a small feature table from a batch of images.

    Args:
        images: Array of shape ``(N, C, H, W)`` or ``(N, H, W, C)`` with
            values in [0, 1] float or [0, 255] uint8.  Grayscale ``(N, 1, H, W)``
            / ``(N, H, W, 1)`` is also accepted.
        threshold_percentile: Percentile of luminance used as a simple
            foreground threshold (lower = more foreground).

    Returns:
        DataFrame with *N* rows and columns from :data:`FEATURE_NAMES`.
    """
    imgs = _to_nhwc_float(images)
    n = imgs.shape[0]

    gray = _luminance(imgs)  # (N, H, W)

    lum_mean = gray.mean(axis=(1, 2))
    lum_std = gray.std(axis=(1, 2))

    if imgs.shape[-1] >= 3:
        ch_r = imgs[:, :, :, 0].mean(axis=(1, 2))
        ch_g = imgs[:, :, :, 1].mean(axis=(1, 2))
        ch_b = imgs[:, :, :, 2].mean(axis=(1, 2))
    else:
        ch_r = ch_g = ch_b = lum_mean

    area_frac = np.empty(n, dtype=np.float64)
    bbox_ar = np.empty(n, dtype=np.float64)
    cent_off = np.empty(n, dtype=np.float64)

    for i in range(n):
        af, ar, co = _mask_features(gray[i], threshold_percentile)
        area_frac[i] = af
        bbox_ar[i] = ar
        cent_off[i] = co

    return pd.DataFrame(
        {
            "luminance_mean": lum_mean,
            "luminance_std": lum_std,
            "channel_r_mean": ch_r,
            "channel_g_mean": ch_g,
            "channel_b_mean": ch_b,
            "colony_area_frac": area_frac,
            "bbox_aspect_ratio": bbox_ar,
            "centroid_offset": cent_off,
        }
    )


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _to_nhwc_float(images: np.ndarray) -> np.ndarray:
    """Normalise to (N, H, W, C) float64 in [0, 1]."""
    arr = np.asarray(images, dtype=np.float64)
    if arr.max() > 1.5:
        arr = arr / 255.0

    if arr.ndim == 3:
        # (N, H, W) — single-channel
        arr = arr[:, :, :, np.newaxis]

    # Detect (N, C, H, W) vs (N, H, W, C) by channel dimension
    if arr.ndim == 4 and arr.shape[1] in (1, 3) and arr.shape[3] not in (1, 3):
        arr = arr.transpose(0, 2, 3, 1)

    return arr


def _luminance(imgs_nhwc: np.ndarray) -> np.ndarray:
    """Convert (N, H, W, C) to grayscale (N, H, W) via rec-601 weights."""
    if imgs_nhwc.shape[-1] == 1:
        return imgs_nhwc[:, :, :, 0]
    r = imgs_nhwc[:, :, :, 0]
    g = imgs_nhwc[:, :, :, 1]
    b = imgs_nhwc[:, :, :, 2]
    return 0.2989 * r + 0.5870 * g + 0.1140 * b


def _mask_features(
    gray: np.ndarray,
    threshold_percentile: float,
) -> Tuple[float, float, float]:
    """Compute colony-mask proxy features for a single (H, W) grayscale image.

    Returns:
        (area_fraction, bbox_aspect_ratio, centroid_offset)
    """
    h, w = gray.shape
    thresh = np.percentile(gray, threshold_percentile)
    mask = gray >= thresh  # foreground = at or above threshold

    n_fg = int(mask.sum())
    area_frac = n_fg / max(h * w, 1)

    if n_fg == 0:
        return 0.0, 1.0, 0.0

    ys, xs = np.nonzero(mask)

    # Bounding-box aspect ratio
    bbox_h = float(ys.max() - ys.min() + 1)
    bbox_w = float(xs.max() - xs.min() + 1)
    bbox_ar = max(bbox_h, bbox_w) / max(min(bbox_h, bbox_w), 1.0)

    # Centroid offset from image centre (normalised by half-diagonal)
    cy = ys.mean()
    cx = xs.mean()
    half_diag = np.sqrt(h**2 + w**2) / 2.0
    offset = np.sqrt((cy - h / 2.0) ** 2 + (cx - w / 2.0) ** 2) / max(half_diag, 1e-9)

    return area_frac, bbox_ar, offset
