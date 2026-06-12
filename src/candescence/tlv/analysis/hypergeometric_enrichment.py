"""Hypergeometric enrichment testing for cluster–label associations.

Tests whether discovered clusters are enriched for morphology labels
beyond what random chance would predict, using the hypergeometric
distribution (Fisher's exact test, one-sided).

For each (cluster, label) pair:
    N = total samples
    K = global count of that label
    n = cluster size
    k = observed label count in cluster
    p = P[X >= k | Hypergeom(N, K, n)]   (one-sided enrichment)
"""

from typing import Optional

import numpy as np
import pandas as pd
from scipy.stats import hypergeom


def compute_hypergeometric_enrichment(
    labels: np.ndarray,
    clusters: np.ndarray,
    *,
    exclude_unlabelled: bool = False,
    unlabelled_values: Optional[set] = None,
    min_count: int = 1,
) -> pd.DataFrame:
    """Compute hypergeometric enrichment p-values for every (cluster, label) pair.

    Parameters
    ----------
    labels : array-like of shape (n_samples,)
        Class labels (e.g. morphology grades). NaN / None treated as
        ``"unlabelled"`` unless *exclude_unlabelled* is True.
    clusters : array-like of shape (n_samples,)
        Cluster assignments (integer or string).
    exclude_unlabelled : bool
        If True, rows whose label is in *unlabelled_values* are dropped
        before testing.
    unlabelled_values : set or None
        Values considered unlabelled.  Defaults to
        ``{None, np.nan, "", "unlabelled", "nan"}``.
    min_count : int
        Minimum observed count *k* to include a row in the output.

    Returns
    -------
    pd.DataFrame
        Columns: cluster, label, N, K, n, k, expected, enrichment_ratio,
        p_value, q_value (BH-adjusted).
    """
    labels = np.asarray(labels, dtype=object)
    clusters = np.asarray(clusters, dtype=object)

    if len(labels) != len(clusters):
        raise ValueError(
            f"labels ({len(labels)}) and clusters ({len(clusters)}) must have same length"
        )

    # Normalise missing labels
    if unlabelled_values is None:
        unlabelled_values = {None, "", "unlabelled", "nan"}

    sentinel = "unlabelled"
    clean_labels = []
    for lab in labels:
        if lab is None or (isinstance(lab, float) and np.isnan(lab)):
            clean_labels.append(sentinel)
        elif str(lab).strip().lower() in unlabelled_values:
            clean_labels.append(sentinel)
        else:
            clean_labels.append(str(lab))
    clean_labels = np.array(clean_labels)

    # Optional exclusion
    if exclude_unlabelled:
        keep = clean_labels != sentinel
        clean_labels = clean_labels[keep]
        clusters = clusters[keep]

    N = len(clean_labels)
    if N == 0:
        return _empty_result()

    unique_clusters = np.unique(clusters)
    unique_labels = np.unique(clean_labels)

    if len(unique_labels) < 2:
        return _empty_result()

    # Global label counts
    label_counts = {lab: int((clean_labels == lab).sum()) for lab in unique_labels}

    rows = []
    for cl in unique_clusters:
        mask = clusters == cl
        n = int(mask.sum())
        cluster_labels = clean_labels[mask]

        for lab in unique_labels:
            K = label_counts[lab]
            k = int((cluster_labels == lab).sum())
            if k < min_count:
                continue

            expected = n * K / N
            enrichment_ratio = k / expected if expected > 0 else np.inf
            # P[X >= k] = sf(k - 1) for discrete distribution
            p_value = float(hypergeom.sf(k - 1, N, K, n))

            rows.append({
                "cluster": cl,
                "label": lab,
                "N": N,
                "K": K,
                "n": n,
                "k": k,
                "expected": round(expected, 2),
                "enrichment_ratio": round(enrichment_ratio, 3),
                "p_value": p_value,
            })

    if not rows:
        return _empty_result()

    df = pd.DataFrame(rows)
    df["q_value"] = adjust_pvalues_bh(df["p_value"].values)
    return df


def adjust_pvalues_bh(pvals: np.ndarray) -> np.ndarray:
    """Benjamini–Hochberg FDR correction.

    Parameters
    ----------
    pvals : 1-d array of raw p-values.

    Returns
    -------
    np.ndarray of adjusted q-values (same length, same order).
    """
    pvals = np.asarray(pvals, dtype=float)
    n = len(pvals)
    if n == 0:
        return pvals.copy()

    order = np.argsort(pvals)
    ranked = np.empty(n, dtype=float)
    ranked[order] = np.arange(1, n + 1)

    q = pvals * n / ranked
    # Enforce monotonicity (step-up)
    q_sorted_idx = np.argsort(pvals)[::-1]
    q_sorted = q[q_sorted_idx]
    for i in range(1, n):
        if q_sorted[i] > q_sorted[i - 1]:
            q_sorted[i] = q_sorted[i - 1]
    q[q_sorted_idx] = q_sorted
    return np.minimum(q, 1.0)


def summarize_cluster_enrichment(
    enrichment_df: pd.DataFrame,
    significance_threshold: float = 0.05,
) -> pd.DataFrame:
    """Per-cluster summary: top enriched label and its statistics.

    Parameters
    ----------
    enrichment_df : DataFrame from :func:`compute_hypergeometric_enrichment`.
    significance_threshold : q-value cutoff.

    Returns
    -------
    pd.DataFrame
        One row per cluster with columns: cluster, top_label, k, n,
        enrichment_ratio, q_value, significant.
    """
    if enrichment_df.empty:
        return pd.DataFrame(columns=[
            "cluster", "top_label", "k", "n",
            "enrichment_ratio", "q_value", "significant",
        ])

    rows = []
    for cl, grp in enrichment_df.groupby("cluster"):
        best = grp.loc[grp["enrichment_ratio"].idxmax()]
        rows.append({
            "cluster": cl,
            "top_label": best["label"],
            "k": best["k"],
            "n": best["n"],
            "enrichment_ratio": best["enrichment_ratio"],
            "q_value": best["q_value"],
            "significant": best["q_value"] < significance_threshold,
        })
    return pd.DataFrame(rows)


def _empty_result() -> pd.DataFrame:
    """Return an empty DataFrame with the expected schema."""
    return pd.DataFrame(columns=[
        "cluster", "label", "N", "K", "n", "k",
        "expected", "enrichment_ratio", "p_value", "q_value",
    ])
