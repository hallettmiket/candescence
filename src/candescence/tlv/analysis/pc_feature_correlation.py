"""Pearson and partial correlation between PC scores and per-image features.

Partial correlation removes the linear effect of a categorical covariate
(typically *media*) via OLS residuals, following the same pattern used in
``scripts/hue_media_confounding_full.py``.
"""

from typing import Optional, Tuple

import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def pearson_matrix(
    scores: np.ndarray,
    features: np.ndarray,
) -> np.ndarray:
    """Element-wise Pearson *r* between PC columns and feature columns.

    Args:
        scores: ``(N, K)`` array — e.g. first *K* PCA scores.
        features: ``(N, F)`` array — per-image feature columns.

    Returns:
        ``(K, F)`` array of Pearson *r* values.
    """
    scores = np.asarray(scores, dtype=np.float64)
    features = np.asarray(features, dtype=np.float64)
    k = scores.shape[1]
    f = features.shape[1]
    out = np.empty((k, f), dtype=np.float64)
    for j in range(k):
        for i in range(f):
            out[j, i] = _safe_pearson(scores[:, j], features[:, i])
    return out


def partial_corr_matrix(
    scores: np.ndarray,
    features: np.ndarray,
    covariate: np.ndarray,
) -> np.ndarray:
    """Partial correlation *r(PC_j, feature_i | covariate)* via OLS residuals.

    Args:
        scores: ``(N, K)`` PC scores.
        features: ``(N, F)`` image features.
        covariate: ``(N,)`` categorical array (e.g. media labels).

    Returns:
        ``(K, F)`` array of partial *r* values.
    """
    scores = np.asarray(scores, dtype=np.float64)
    features = np.asarray(features, dtype=np.float64)
    D = _covariate_dummies(covariate)
    k = scores.shape[1]
    f = features.shape[1]
    out = np.empty((k, f), dtype=np.float64)

    # Pre-residualise features (shared across PCs)
    feat_resid = np.empty_like(features)
    for i in range(f):
        feat_resid[:, i] = _residualise(features[:, i], D)

    for j in range(k):
        score_resid = _residualise(scores[:, j], D)
        for i in range(f):
            out[j, i] = _safe_pearson(score_resid, feat_resid[:, i])
    return out


def pc_feature_correlation(
    scores: np.ndarray,
    features: pd.DataFrame,
    *,
    n_pcs: int = 8,
    covariate: Optional[np.ndarray] = None,
) -> Tuple[pd.DataFrame, Optional[pd.DataFrame]]:
    """Convenience driver: Pearson + optional partial correlation table.

    Args:
        scores: ``(N, K_max)`` full PCA scores.
        features: DataFrame with *N* rows (from
            :func:`~candescence.tlv.analysis.image_feature_table.compute_image_features_batch`).
        n_pcs: Number of leading PCs to include.
        covariate: Optional categorical vector for partial correlation.

    Returns:
        ``(pearson_df, partial_df)`` — each a DataFrame with index
        ``PC1 … PC{n_pcs}`` and feature-name columns.  ``partial_df`` is
        ``None`` when *covariate* is not supplied.
    """
    k = min(n_pcs, scores.shape[1])
    S = scores[:, :k]
    F = features.values

    pc_labels = [f"PC{j+1}" for j in range(k)]
    feat_names = list(features.columns)

    r_mat = pearson_matrix(S, F)
    pearson_df = pd.DataFrame(r_mat, index=pc_labels, columns=feat_names)

    partial_df = None
    if covariate is not None:
        pr_mat = partial_corr_matrix(S, F, covariate)
        partial_df = pd.DataFrame(pr_mat, index=pc_labels, columns=feat_names)

    return pearson_df, partial_df


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _safe_pearson(x: np.ndarray, y: np.ndarray) -> float:
    """Pearson *r* that returns 0.0 for constant inputs."""
    x = np.asarray(x, dtype=np.float64)
    y = np.asarray(y, dtype=np.float64)
    if x.std() < 1e-12 or y.std() < 1e-12:
        return 0.0
    return float(np.corrcoef(x, y)[0, 1])


def _covariate_dummies(covariate: np.ndarray) -> np.ndarray:
    """One-hot encode a categorical vector, dropping the first level."""
    return pd.get_dummies(
        pd.Series(covariate), drop_first=True,
    ).values.astype(np.float64)


def _residualise(y: np.ndarray, D: np.ndarray) -> np.ndarray:
    """OLS residuals of *y* regressed on design matrix *D*.

    Returns *y* unchanged when *D* has no columns (e.g. single-level covariate
    after ``drop_first``).
    """
    if D.shape[1] == 0:
        return y.copy()
    reg = LinearRegression().fit(D, y)
    return y - reg.predict(D)
