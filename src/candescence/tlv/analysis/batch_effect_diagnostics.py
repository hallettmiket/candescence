"""Batch-effect diagnostics for latent-space analysis.

Decomposes latent variance into contributions from biological factors
(media / morphology) versus technical factors (plate / lighting / hue)
to distinguish hue-driven from morphology-driven representations.

Designed for the Candescence fully-crossed corpus (plates x media).
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional

import numpy as np
from sklearn.cluster import KMeans
from sklearn.linear_model import LinearRegression
from sklearn.metrics import adjusted_rand_score, silhouette_score
from sklearn.preprocessing import OneHotEncoder


# ---------------------------------------------------------------------------
# Variance partitioning
# ---------------------------------------------------------------------------

@dataclass
class VarianceResult:
    """Results from sequential R² variance partitioning."""

    r2_media: float
    r2_plate: float
    r2_hue: float  # NaN if hue not provided
    r2_media_plate: float
    r2_full: float  # media + plate + hue (or media + plate if no hue)
    unique_media: float
    unique_plate: float
    unique_hue: float  # NaN if hue not provided
    residual: float
    n_samples: int
    n_pcs: int


def variance_partitioning(
    X: np.ndarray,
    media: np.ndarray,
    plate: np.ndarray,
    hue: Optional[np.ndarray] = None,
    n_pcs: int = 10,
) -> VarianceResult:
    """Partition latent variance among media, plate, and hue.

    Parameters
    ----------
    X : (N, D) embedding matrix or PCA scores.
    media : (N,) categorical media labels.
    plate : (N,) categorical plate labels.
    hue : (N,) numeric hue values (optional).
    n_pcs : number of PCs to use (columns of X if D > n_pcs).

    Returns
    -------
    VarianceResult
    """
    X = np.asarray(X, dtype=np.float64)
    media = np.asarray(media).reshape(-1, 1)
    plate = np.asarray(plate).reshape(-1, 1)

    # Truncate to top PCs
    if X.shape[1] > n_pcs:
        X = X[:, :n_pcs]
    n = len(X)

    enc = OneHotEncoder(sparse_output=False, drop="first", handle_unknown="error")

    M = enc.fit_transform(media)
    P = enc.fit_transform(plate)

    def _r2(features: np.ndarray) -> float:
        if features.shape[1] == 0:
            return 0.0
        reg = LinearRegression().fit(features, X)
        return float(reg.score(features, X))

    r2_media = _r2(M)
    r2_plate = _r2(P)

    MP = np.hstack([M, P])
    r2_media_plate = _r2(MP)

    if hue is not None:
        hue = np.asarray(hue, dtype=np.float64).reshape(-1, 1)
        finite_mask = np.isfinite(hue.ravel())
        if finite_mask.sum() < n * 0.5:
            hue = None

    if hue is not None:
        H = hue
        r2_hue = _r2(H)
        MPH = np.hstack([M, P, H])
        r2_full = _r2(MPH)
        unique_hue = r2_full - r2_media_plate
    else:
        r2_hue = float("nan")
        r2_full = r2_media_plate
        unique_hue = float("nan")

    # Unique contributions (type III–style)
    if hue is not None:
        PH = np.hstack([P, H])
        MH = np.hstack([M, H])
        unique_media = r2_full - _r2(PH)
        unique_plate = r2_full - _r2(MH)
    else:
        unique_media = r2_media_plate - r2_plate
        unique_plate = r2_media_plate - r2_media

    residual = 1.0 - r2_full

    return VarianceResult(
        r2_media=round(r2_media, 4),
        r2_plate=round(r2_plate, 4),
        r2_hue=round(r2_hue, 4) if np.isfinite(r2_hue) else float("nan"),
        r2_media_plate=round(r2_media_plate, 4),
        r2_full=round(r2_full, 4),
        unique_media=round(max(unique_media, 0), 4),
        unique_plate=round(max(unique_plate, 0), 4),
        unique_hue=round(max(unique_hue, 0), 4) if np.isfinite(unique_hue) else float("nan"),
        residual=round(max(residual, 0), 4),
        n_samples=n,
        n_pcs=X.shape[1],
    )


# ---------------------------------------------------------------------------
# Within-plate separation
# ---------------------------------------------------------------------------

@dataclass
class WithinPlateResult:
    """Per-plate media separation metrics."""

    plate: str
    silhouette: float
    ari: float
    n_samples: int
    n_media: int
    media_labels: np.ndarray = field(repr=False)
    coords_2d: Optional[np.ndarray] = field(default=None, repr=False)


def within_plate_separation(
    X: np.ndarray,
    media: np.ndarray,
    plate: np.ndarray,
    target_plate: str,
    coords_2d: Optional[np.ndarray] = None,
) -> WithinPlateResult:
    """Measure media separation within a single plate.

    Parameters
    ----------
    X : (N, D) embeddings.
    media : (N,) media labels.
    plate : (N,) plate labels.
    target_plate : which plate to analyse.
    coords_2d : (N, 2) optional 2D projection for visualisation.

    Returns
    -------
    WithinPlateResult
    """
    mask = np.asarray(plate) == target_plate
    X_p = np.asarray(X)[mask]
    media_p = np.asarray(media)[mask]

    unique_media = np.unique(media_p)
    n_media = len(unique_media)
    n = len(X_p)

    if n < 3 or n_media < 2:
        return WithinPlateResult(
            plate=target_plate,
            silhouette=float("nan"),
            ari=float("nan"),
            n_samples=n,
            n_media=n_media,
            media_labels=media_p,
            coords_2d=coords_2d[mask] if coords_2d is not None else None,
        )

    k = min(n_media, n - 1)
    km_labels = KMeans(n_clusters=k, random_state=42, n_init=10).fit_predict(X_p)

    sil = float(silhouette_score(X_p, media_p))
    ari = float(adjusted_rand_score(media_p, km_labels))

    return WithinPlateResult(
        plate=target_plate,
        silhouette=round(sil, 4),
        ari=round(ari, 4),
        n_samples=n,
        n_media=n_media,
        media_labels=media_p,
        coords_2d=coords_2d[mask] if coords_2d is not None else None,
    )


# ---------------------------------------------------------------------------
# Cross-plate consistency
# ---------------------------------------------------------------------------

@dataclass
class CrossPlateResult:
    """Per-media plate mixing metrics."""

    media: str
    silhouette_plate: float
    n_samples: int
    n_plates: int
    plate_labels: np.ndarray = field(repr=False)
    coords_2d: Optional[np.ndarray] = field(default=None, repr=False)


def cross_plate_consistency(
    X: np.ndarray,
    media: np.ndarray,
    plate: np.ndarray,
    target_media: str,
    coords_2d: Optional[np.ndarray] = None,
) -> CrossPlateResult:
    """Measure plate mixing within a single media type.

    Low silhouette(plate) → plates intermix → morphology-driven (good).
    High silhouette(plate) → plates separate → hue-driven (bad).

    Parameters
    ----------
    X : (N, D) embeddings.
    media : (N,) media labels.
    plate : (N,) plate labels.
    target_media : which media to analyse.
    coords_2d : (N, 2) optional 2D projection for visualisation.

    Returns
    -------
    CrossPlateResult
    """
    mask = np.asarray(media) == target_media
    X_m = np.asarray(X)[mask]
    plate_m = np.asarray(plate)[mask]

    unique_plates = np.unique(plate_m)
    n_plates = len(unique_plates)
    n = len(X_m)

    if n < 3 or n_plates < 2:
        return CrossPlateResult(
            media=target_media,
            silhouette_plate=float("nan"),
            n_samples=n,
            n_plates=n_plates,
            plate_labels=plate_m,
            coords_2d=coords_2d[mask] if coords_2d is not None else None,
        )

    sil = float(silhouette_score(X_m, plate_m))

    return CrossPlateResult(
        media=target_media,
        silhouette_plate=round(sil, 4),
        n_samples=n,
        n_plates=n_plates,
        plate_labels=plate_m,
        coords_2d=coords_2d[mask] if coords_2d is not None else None,
    )
