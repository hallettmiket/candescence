"""Pure-function latent-space analysis helpers.

Designed for use in the Streamlit explorer where only raw embedding
matrices and metadata DataFrames are available (no Inference object).

Includes:
- Silhouette analysis (sklearn)
- Mantel test (scikit-bio, optional)
- k-NN label purity
- Within/between-class distance effect size

Distance computation mirrors ``LatentEmbedding`` in
``candescence.tlv.inference.latent_embedding``: Euclidean pdist,
Pearson correlation for Mantel, 999 default permutations.
"""

from dataclasses import dataclass, field
from typing import Optional, Tuple

import numpy as np
from scipy.spatial.distance import pdist, squareform
from sklearn.metrics import silhouette_samples, silhouette_score
from sklearn.neighbors import NearestNeighbors


# ---------------------------------------------------------------------------
# Silhouette
# ---------------------------------------------------------------------------

@dataclass
class SilhouetteResult:
    """Container for silhouette analysis output."""

    overall_score: float
    sample_scores: np.ndarray
    labels: np.ndarray
    mask: np.ndarray  # bool mask of rows actually used (after NaN/unlabelled filtering)


def silhouette_metrics(
    X: np.ndarray,
    labels: np.ndarray,
    *,
    metric: str = "euclidean",
    exclude_unlabelled: bool = False,
    unlabelled_values: Optional[set] = None,
) -> SilhouetteResult:
    """Compute silhouette scores for *X* grouped by *labels*.

    Parameters
    ----------
    X : (N, D) embedding matrix.
    labels : (N,) categorical labels (strings, ints, etc.).
    metric : distance metric forwarded to sklearn.
    exclude_unlabelled : drop rows whose label is in *unlabelled_values*.
    unlabelled_values : values treated as unlabelled.
        Defaults to ``{None, "", "unlabelled", "nan"}``.

    Returns
    -------
    SilhouetteResult

    Raises
    ------
    ValueError
        If fewer than 2 label groups remain after filtering, or N < 2.
    """
    X = np.asarray(X, dtype=np.float64)
    labels = np.asarray(labels, dtype=object)

    if len(X) != len(labels):
        raise ValueError(
            f"X ({len(X)}) and labels ({len(labels)}) must have same length"
        )

    if unlabelled_values is None:
        unlabelled_values = {None, "", "unlabelled", "nan"}

    # Build mask
    mask = np.ones(len(labels), dtype=bool)
    for i, lab in enumerate(labels):
        if lab is None or (isinstance(lab, float) and np.isnan(lab)):
            if exclude_unlabelled:
                mask[i] = False
            else:
                labels[i] = "unlabelled"
        elif str(lab).strip().lower() in unlabelled_values:
            if exclude_unlabelled:
                mask[i] = False
            else:
                labels[i] = "unlabelled"

    X_filt = X[mask]
    labels_filt = labels[mask]

    unique = np.unique(labels_filt)
    if len(unique) < 2:
        raise ValueError(
            f"Silhouette requires at least 2 label groups; got {len(unique)} "
            f"after filtering ({list(unique)})."
        )
    if len(X_filt) < 2:
        raise ValueError("Need at least 2 samples after filtering.")

    overall = float(silhouette_score(X_filt, labels_filt, metric=metric))
    per_sample = silhouette_samples(X_filt, labels_filt, metric=metric)

    return SilhouetteResult(
        overall_score=overall,
        sample_scores=per_sample,
        labels=labels_filt,
        mask=mask,
    )


# ---------------------------------------------------------------------------
# Mantel
# ---------------------------------------------------------------------------

@dataclass
class MantelResult:
    """Container for Mantel test output."""

    statistic: float
    p_value: float
    n_samples: int
    subsampled: bool
    permutations: int


class SkbioUnavailableError(ImportError):
    """Raised when scikit-bio is not installed."""


def mantel_latent_vs_matrix(
    X_latent: np.ndarray,
    D_other: np.ndarray,
    *,
    permutations: int = 999,
    max_n: Optional[int] = None,
    seed: int = 42,
) -> "MantelResult":
    """Mantel test: latent pairwise distances vs an arbitrary distance matrix.

    Parameters
    ----------
    X_latent : (N, D) embedding matrix.
    D_other : (N, N) symmetric distance matrix to correlate against.
    permutations : permutations for Mantel p-value.
    max_n : if N > max_n, randomly subsample matching rows/cols (fixed seed).
    seed : RNG seed.

    Notes
    -----
    Builds the latent distance matrix on the fly using the same Euclidean
    pdist + symmetrisation conventions as :func:`mantel_latent_vs_numeric`.
    """
    try:
        from skbio.stats.distance import mantel as skbio_mantel
    except ImportError as exc:
        raise SkbioUnavailableError(
            "scikit-bio is required for the Mantel test. "
            "Install with: pip install 'candescence[analysis]' or pip install scikit-bio"
        ) from exc

    X = np.asarray(X_latent, dtype=np.float64)
    D = np.asarray(D_other, dtype=np.float64)

    if X.ndim != 2:
        raise ValueError(f"X_latent must be 2-D; got shape {X.shape}")
    if D.ndim != 2 or D.shape[0] != D.shape[1]:
        raise ValueError(f"D_other must be square; got shape {D.shape}")
    if D.shape[0] != X.shape[0]:
        raise ValueError(
            f"X_latent rows ({X.shape[0]}) must equal D_other size ({D.shape[0]})"
        )
    if not np.all(np.isfinite(X)):
        raise ValueError("X_latent contains non-finite values.")
    if not np.all(np.isfinite(D)):
        raise ValueError("D_other contains non-finite values.")
    if not np.allclose(D, D.T, atol=1e-6):
        raise ValueError("D_other must be symmetric.")
    if X.shape[0] < 3:
        raise ValueError(
            f"Need at least 3 samples; got {X.shape[0]}."
        )

    subsampled = False
    if max_n is not None and X.shape[0] > max_n:
        rng = np.random.default_rng(seed)
        idx = rng.choice(X.shape[0], size=max_n, replace=False)
        idx.sort()
        X = X[idx]
        D = D[np.ix_(idx, idx)]
        subsampled = True

    n = X.shape[0]

    D_latent = squareform(pdist(X, metric="euclidean"))
    np.fill_diagonal(D_latent, 0.0)
    D_latent = np.round(D_latent, decimals=8)
    D_latent = (D_latent + D_latent.T) / 2
    D_latent = D_latent.astype(np.float32)

    D_other_sym = ((D + D.T) / 2.0).astype(np.float32)
    np.fill_diagonal(D_other_sym, 0.0)

    stat, p, _ = skbio_mantel(
        D_latent, D_other_sym,
        method="pearson",
        permutations=permutations,
    )

    return MantelResult(
        statistic=float(stat),
        p_value=float(p),
        n_samples=n,
        subsampled=subsampled,
        permutations=permutations,
    )


def mantel_latent_vs_numeric(
    X: np.ndarray,
    y: np.ndarray,
    *,
    permutations: int = 999,
    max_n: Optional[int] = None,
    seed: int = 42,
) -> MantelResult:
    """Mantel test: latent pairwise distances vs numeric trait distances.

    Mirrors the distance pipeline in
    ``LatentEmbedding.compute_mantel`` (Euclidean pdist, Pearson method)
    but operates on **copies** so no shared state is mutated.

    Parameters
    ----------
    X : (N, D) embedding matrix.
    y : (N,) numeric trait values.
    permutations : number of permutations for p-value.
    max_n : if N > max_n, randomly subsample to *max_n* rows (fixed *seed*).
        ``None`` means no subsampling.
    seed : RNG seed for reproducible subsampling.

    Returns
    -------
    MantelResult

    Raises
    ------
    SkbioUnavailableError
        If scikit-bio is not installed.
    ValueError
        If inputs are malformed (wrong shape, < 3 samples, < 2 unique y, NaN).
    """
    try:
        from skbio.stats.distance import mantel as skbio_mantel
    except ImportError as exc:
        raise SkbioUnavailableError(
            "scikit-bio is required for the Mantel test. "
            "Install with: pip install 'candescence[analysis]' or pip install scikit-bio"
        ) from exc

    X = np.asarray(X, dtype=np.float64)
    y = np.asarray(y, dtype=np.float64)

    if X.ndim != 2:
        raise ValueError(f"X must be 2-D; got shape {X.shape}")
    if y.ndim != 1:
        raise ValueError(f"y must be 1-D; got shape {y.shape}")
    if len(X) != len(y):
        raise ValueError(
            f"X ({len(X)}) and y ({len(y)}) must have same length"
        )

    # Drop rows with NaN in y or any NaN in X
    finite_mask = np.isfinite(y) & np.all(np.isfinite(X), axis=1)
    X = X[finite_mask]
    y = y[finite_mask]

    if len(X) < 3:
        raise ValueError(
            f"Need at least 3 samples with finite values; got {len(X)}."
        )
    if len(np.unique(y)) < 2:
        raise ValueError("y must have at least 2 unique finite values.")

    # Optional subsampling
    subsampled = False
    if max_n is not None and len(X) > max_n:
        rng = np.random.default_rng(seed)
        idx = rng.choice(len(X), size=max_n, replace=False)
        idx.sort()
        X = X[idx]
        y = y[idx]
        subsampled = True

    n = len(X)

    # Latent distance matrix (copy-based, no mutation of caller data)
    D_latent = squareform(pdist(X, metric="euclidean"))
    np.fill_diagonal(D_latent, 0.0)
    D_latent = np.round(D_latent, decimals=8)
    D_latent = (D_latent + D_latent.T) / 2
    D_latent = D_latent.astype(np.float32)

    # Property distance matrix
    D_prop = squareform(pdist(y[:, np.newaxis], metric="euclidean"))
    np.fill_diagonal(D_prop, 0.0)
    D_prop = D_prop.astype(np.float32)

    stat, p, _ = skbio_mantel(
        D_latent, D_prop,
        method="pearson",
        permutations=permutations,
    )

    return MantelResult(
        statistic=float(stat),
        p_value=float(p),
        n_samples=n,
        subsampled=subsampled,
        permutations=permutations,
    )


# ---------------------------------------------------------------------------
# k-NN label purity
# ---------------------------------------------------------------------------

@dataclass
class KNNPurityResult:
    """Container for k-NN label purity output."""

    mean_purity: float
    per_sample_purity: np.ndarray
    mask: np.ndarray
    n_samples: int
    k_used: int
    n_classes: int


def _filter_labels(
    X: np.ndarray,
    labels: np.ndarray,
    *,
    exclude_unlabelled: bool = False,
    unlabelled_values: Optional[set] = None,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Shared label filtering logic (mirrors silhouette_metrics).

    Returns (X_filtered, labels_filtered, bool_mask).
    """
    X = np.asarray(X, dtype=np.float64)
    labels = np.asarray(labels, dtype=object)

    if len(X) != len(labels):
        raise ValueError(
            f"X ({len(X)}) and labels ({len(labels)}) must have same length"
        )

    if unlabelled_values is None:
        unlabelled_values = {None, "", "unlabelled", "nan"}

    mask = np.ones(len(labels), dtype=bool)
    for i, lab in enumerate(labels):
        if lab is None or (isinstance(lab, float) and np.isnan(lab)):
            if exclude_unlabelled:
                mask[i] = False
            else:
                labels[i] = "unlabelled"
        elif str(lab).strip().lower() in unlabelled_values:
            if exclude_unlabelled:
                mask[i] = False
            else:
                labels[i] = "unlabelled"

    return X[mask], labels[mask], mask


def knn_label_purity(
    X: np.ndarray,
    labels: np.ndarray,
    *,
    k: int = 10,
    metric: str = "euclidean",
    exclude_unlabelled: bool = False,
    unlabelled_values: Optional[set] = None,
) -> KNNPurityResult:
    """Fraction of each point's *k* nearest neighbors sharing its label.

    Parameters
    ----------
    X : (N, D) embedding matrix.
    labels : (N,) categorical labels.
    k : number of neighbors to consider.
    metric : distance metric for neighbor search.
    exclude_unlabelled : drop rows whose label is in *unlabelled_values*.
    unlabelled_values : values treated as unlabelled.
        Defaults to ``{None, "", "unlabelled", "nan"}``.

    Returns
    -------
    KNNPurityResult

    Raises
    ------
    ValueError
        If fewer than 2 label groups remain after filtering.
    """
    X_filt, labels_filt, mask = _filter_labels(
        X, labels,
        exclude_unlabelled=exclude_unlabelled,
        unlabelled_values=unlabelled_values,
    )

    n = len(X_filt)
    unique = np.unique(labels_filt)
    if len(unique) < 2:
        raise ValueError(
            f"k-NN purity requires at least 2 label groups; got {len(unique)} "
            f"after filtering ({list(unique)})."
        )

    # Clamp k to available neighbors
    k_used = min(k, max(1, n - 1))

    # Fit NN (k+1 to account for self-match)
    nn = NearestNeighbors(n_neighbors=k_used + 1, metric=metric)
    nn.fit(X_filt)
    _, indices = nn.kneighbors(X_filt)

    # Normalize labels to strings for comparison
    str_labels = np.array([str(l) for l in labels_filt])

    per_sample = np.empty(n, dtype=np.float64)
    for i in range(n):
        # Drop self from neighbor list
        neigh_idx = indices[i]
        neigh_idx = neigh_idx[neigh_idx != i][:k_used]
        if len(neigh_idx) == 0:
            per_sample[i] = 0.0
        else:
            per_sample[i] = np.mean(str_labels[neigh_idx] == str_labels[i])

    return KNNPurityResult(
        mean_purity=float(np.mean(per_sample)),
        per_sample_purity=per_sample,
        mask=mask,
        n_samples=n,
        k_used=k_used,
        n_classes=len(unique),
    )


# ---------------------------------------------------------------------------
# Within / between class distance effect
# ---------------------------------------------------------------------------

@dataclass
class WithinBetweenResult:
    """Container for within/between-class distance comparison."""

    mean_within: float
    mean_between: float
    ratio: float  # mean_between / mean_within
    cohens_d: float  # effect size
    n_within_pairs: int
    n_between_pairs: int
    n_samples: int


def within_between_distance_effect(
    X: np.ndarray,
    labels: np.ndarray,
    *,
    metric: str = "euclidean",
    exclude_unlabelled: bool = False,
    unlabelled_values: Optional[set] = None,
    max_pairs: int = 100_000,
    random_state: int = 42,
) -> WithinBetweenResult:
    """Compare within-class to between-class pairwise distances.

    Avoids O(N^2) by subsampling pairs up to *max_pairs* each for
    within-class and between-class.

    Parameters
    ----------
    X : (N, D) embedding matrix.
    labels : (N,) categorical labels.
    metric : distance metric (scipy.spatial.distance).
    exclude_unlabelled : drop unlabelled rows.
    unlabelled_values : values treated as unlabelled.
    max_pairs : max pairs to sample for each of within/between.
    random_state : RNG seed for reproducible subsampling.

    Returns
    -------
    WithinBetweenResult

    Raises
    ------
    ValueError
        If fewer than 2 label groups remain after filtering.
    """
    from scipy.spatial.distance import cdist

    X_filt, labels_filt, mask = _filter_labels(
        X, labels,
        exclude_unlabelled=exclude_unlabelled,
        unlabelled_values=unlabelled_values,
    )

    str_labels = np.array([str(l) for l in labels_filt])
    unique = np.unique(str_labels)
    if len(unique) < 2:
        raise ValueError(
            f"Within/between analysis requires at least 2 label groups; "
            f"got {len(unique)} after filtering ({list(unique)})."
        )

    rng = np.random.default_rng(random_state)
    n = len(X_filt)

    # Build index arrays per class
    class_indices = {u: np.where(str_labels == u)[0] for u in unique}

    # --- Sample within-class pairs ---
    within_dists = []
    # Collect all possible within-class pairs, then subsample
    within_pairs_list = []
    for u, idx in class_indices.items():
        if len(idx) < 2:
            continue
        # Generate pair indices within this class
        n_u = len(idx)
        n_possible = n_u * (n_u - 1) // 2
        if n_possible <= max_pairs // len(unique):
            # Take all pairs for this class
            ii, jj = np.triu_indices(n_u, k=1)
            within_pairs_list.append(np.column_stack([idx[ii], idx[jj]]))
        else:
            # Subsample
            n_sample = max_pairs // len(unique)
            a = rng.integers(0, n_u, size=n_sample)
            b = rng.integers(0, n_u - 1, size=n_sample)
            b[b >= a] += 1
            within_pairs_list.append(np.column_stack([idx[a], idx[b]]))

    if not within_pairs_list:
        return WithinBetweenResult(
            mean_within=float('nan'), mean_between=float('nan'),
            ratio=float('nan'), cohens_d=float('nan'),
            n_within_pairs=0, n_between_pairs=0, n_samples=n,
        )

    within_pairs = np.concatenate(within_pairs_list)
    if len(within_pairs) > max_pairs:
        idx_sub = rng.choice(len(within_pairs), size=max_pairs, replace=False)
        within_pairs = within_pairs[idx_sub]

    # Compute within distances
    diff_w = X_filt[within_pairs[:, 0]] - X_filt[within_pairs[:, 1]]
    if metric == "euclidean":
        within_d = np.linalg.norm(diff_w, axis=1)
    else:
        within_d = np.array([
            float(cdist(X_filt[within_pairs[i:i+1, 0]],
                        X_filt[within_pairs[i:i+1, 1]], metric=metric)[0, 0])
            for i in range(len(within_pairs))
        ])

    # --- Sample between-class pairs ---
    n_between_sample = min(max_pairs, n * (n - 1) // 2)
    a_idx = rng.integers(0, n, size=n_between_sample)
    b_idx = rng.integers(0, n - 1, size=n_between_sample)
    b_idx[b_idx >= a_idx] += 1
    # Keep only between-class
    between_mask = str_labels[a_idx] != str_labels[b_idx]
    a_between = a_idx[between_mask]
    b_between = b_idx[between_mask]

    if len(a_between) == 0:
        return WithinBetweenResult(
            mean_within=float(np.mean(within_d)),
            mean_between=float('nan'),
            ratio=float('nan'), cohens_d=float('nan'),
            n_within_pairs=len(within_d), n_between_pairs=0,
            n_samples=n,
        )

    diff_b = X_filt[a_between] - X_filt[b_between]
    if metric == "euclidean":
        between_d = np.linalg.norm(diff_b, axis=1)
    else:
        between_d = np.array([
            float(cdist(X_filt[a_between[i:i+1]],
                        X_filt[b_between[i:i+1]], metric=metric)[0, 0])
            for i in range(len(a_between))
        ])

    mean_w = float(np.mean(within_d))
    mean_b = float(np.mean(between_d))
    ratio = mean_b / mean_w if mean_w > 0 else float('nan')

    # Cohen's d
    pooled_std = np.sqrt(
        (np.var(within_d, ddof=1) + np.var(between_d, ddof=1)) / 2.0
    )
    cohens_d = (mean_b - mean_w) / pooled_std if pooled_std > 0 else float('nan')

    return WithinBetweenResult(
        mean_within=mean_w,
        mean_between=mean_b,
        ratio=ratio,
        cohens_d=cohens_d,
        n_within_pairs=len(within_d),
        n_between_pairs=len(a_between),
        n_samples=n,
    )
