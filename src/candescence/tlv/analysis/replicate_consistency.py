"""Quantitative replicate-consistency metrics for latent embeddings.

Given a set of *replicate-pair* distances (same biological replicate, different
imaging) and *random-pair* distances (different replicates), compute a single
per-space score that summarises whether the embedding places replicates closer
together than random pairs.

These helpers are model-agnostic: they take precomputed distance arrays and
return scalars. Pair construction lives in
``candescence.tlv.inference.tendril_replicate_diagnostics``
(``build_replicate_pairs``, ``build_nonrep_pairs``).
"""

from dataclasses import dataclass
from typing import Sequence

import numpy as np
import pandas as pd


@dataclass
class ReplicateConsistencyResult:
    """Container for one replicate-consistency summary."""

    auroc: float
    cohens_d: float
    mean_rep: float
    mean_rand: float
    n_rep_pairs: int
    n_rand_pairs: int


def replicate_random_auroc(
    rep_dists: np.ndarray, rand_dists: np.ndarray
) -> float:
    """AUROC for distinguishing replicate pairs from random pairs by distance.

    Lower distance should indicate "is a replicate pair", so we use
    ``-distance`` as the score (replicate = positive class).

    Returns ``nan`` if either array is empty or all-NaN, or if both arrays
    contain only a single unique combined value (sklearn requires variation).
    """
    rep = np.asarray(rep_dists, dtype=np.float64)
    rand = np.asarray(rand_dists, dtype=np.float64)
    rep = rep[np.isfinite(rep)]
    rand = rand[np.isfinite(rand)]
    if len(rep) == 0 or len(rand) == 0:
        return float("nan")

    from sklearn.metrics import roc_auc_score

    y_true = np.concatenate([np.ones(len(rep)), np.zeros(len(rand))])
    y_score = np.concatenate([-rep, -rand])
    if np.unique(y_score).size < 2:
        return float("nan")
    return float(roc_auc_score(y_true, y_score))


def replicate_random_cohens_d(
    rep_dists: np.ndarray, rand_dists: np.ndarray
) -> float:
    """Pooled-std Cohen's d for ``rand - rep`` (positive = replicates closer).

    Returns ``nan`` if either group has < 2 samples or pooled std is zero.
    """
    rep = np.asarray(rep_dists, dtype=np.float64)
    rand = np.asarray(rand_dists, dtype=np.float64)
    rep = rep[np.isfinite(rep)]
    rand = rand[np.isfinite(rand)]
    if len(rep) < 2 or len(rand) < 2:
        return float("nan")
    var_rep = np.var(rep, ddof=1)
    var_rand = np.var(rand, ddof=1)
    pooled = np.sqrt((var_rep + var_rand) / 2.0)
    if pooled <= 0 or not np.isfinite(pooled):
        return float("nan")
    return float((np.mean(rand) - np.mean(rep)) / pooled)


def replicate_consistency_summary(
    rep_df: pd.DataFrame,
    rand_df: pd.DataFrame,
    layers: Sequence[str] = ("x0", "x1", "x2", "x3"),
) -> pd.DataFrame:
    """One-row-per-layer table of replicate-vs-random distance metrics.

    Parameters
    ----------
    rep_df, rand_df
        DataFrames produced by
        :func:`compute_intermediate_distances_for_pairs` (one row per pair,
        one distance column per layer).  Any *layers* not present in the
        frames are skipped.
    layers
        Layer column names to summarise.

    Returns
    -------
    DataFrame with columns
    ``layer, mean_rep, mean_rand, auroc, cohens_d, n_rep_pairs, n_rand_pairs``.
    """
    rows = []
    for layer in layers:
        if layer not in rep_df.columns or layer not in rand_df.columns:
            continue
        rep_vals = rep_df[layer].to_numpy(dtype=np.float64)
        rand_vals = rand_df[layer].to_numpy(dtype=np.float64)
        rep_finite = rep_vals[np.isfinite(rep_vals)]
        rand_finite = rand_vals[np.isfinite(rand_vals)]
        rows.append({
            "layer": layer,
            "mean_rep": float(np.mean(rep_finite)) if rep_finite.size else float("nan"),
            "mean_rand": float(np.mean(rand_finite)) if rand_finite.size else float("nan"),
            "auroc": replicate_random_auroc(rep_vals, rand_vals),
            "cohens_d": replicate_random_cohens_d(rep_vals, rand_vals),
            "n_rep_pairs": int(rep_finite.size),
            "n_rand_pairs": int(rand_finite.size),
        })
    return pd.DataFrame(rows)
