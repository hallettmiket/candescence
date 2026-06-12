"""Rank-change analysis across latent spaces (tendril / skip layers).

Pure-function reimplementation of the rank-change statistics from
``candescence_master/projects/tlv/vae/inference.py``
(``compute_nonrep_rank_change_stats``, ``compute_replicate_rank_change_stats``).

Operates on pre-computed embedding matrices — no VAE forward pass needed.
Complexity is O(P * L * D) where P = pairs, L = spaces, D = latent dim.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd


@dataclass
class RankChangeResult:
    """Container for rank-change analysis across latent spaces.

    Attributes
    ----------
    overall : dict
        Aggregate statistics: mean_rank_var, median_rank_var, mean_rank_range,
        n_pairs, n_spaces.
    pair_summary : pd.DataFrame
        Per-pair metrics: rank_var, rank_std, rank_range, mean_abs_delta,
        plus rank_<space> and delta columns.
    moves : pd.DataFrame
        Per-transition summary: columns are transition name,
        Mean |Δrank|, Median |Δrank|, Std |Δrank|, Mean Δrank.
    rank_table : pd.DataFrame
        Raw rank matrix (pairs × spaces).
    """

    overall: dict
    pair_summary: pd.DataFrame
    moves: pd.DataFrame
    rank_table: pd.DataFrame


def pairwise_distances_for_index_pairs(
    X: np.ndarray,
    pairs: np.ndarray,
    metric: str = "euclidean",
) -> np.ndarray:
    """Compute distances for specific index pairs.

    Parameters
    ----------
    X : (N, D) embedding matrix.
    pairs : (P, 2) integer index array (0-based).
    metric : distance metric. Only ``"euclidean"`` is optimized;
        others fall back to scipy.

    Returns
    -------
    (P,) distance array.
    """
    pairs = np.asarray(pairs, dtype=int)
    if pairs.ndim != 2 or pairs.shape[1] != 2:
        raise ValueError(f"pairs must be (P, 2); got shape {pairs.shape}")

    diff = X[pairs[:, 0]] - X[pairs[:, 1]]
    if metric == "euclidean":
        return np.linalg.norm(diff, axis=1)
    else:
        from scipy.spatial.distance import cdist
        return np.array([
            float(cdist(X[pairs[i:i+1, 0]], X[pairs[i:i+1, 1]], metric=metric)[0, 0])
            for i in range(len(pairs))
        ])


def ranks_from_distances(distances: np.ndarray) -> np.ndarray:
    """Convert distances to 1-based ordinal ranks (ascending: rank 1 = closest).

    Matches the master implementation: ``np.argsort`` with stable sort,
    then assign ranks 1..P by sorted order (ties get consecutive ranks).

    Parameters
    ----------
    distances : (P,) finite distance array.

    Returns
    -------
    (P,) integer ranks (1-based).
    """
    distances = np.asarray(distances, dtype=np.float64)
    order = np.argsort(distances, kind="mergesort")  # stable, matches master
    ranks = np.empty_like(order)
    ranks[order] = np.arange(1, len(distances) + 1)
    return ranks


def rank_change_across_spaces(
    embeddings_by_space: Dict[str, np.ndarray],
    pairs: Optional[np.ndarray] = None,
    *,
    n_pairs: int = 2000,
    metric: str = "euclidean",
    random_state: int = 42,
) -> RankChangeResult:
    """Compute rank-change statistics for pairs across multiple latent spaces.

    For each pair of points, computes pairwise distance in every space,
    ranks the pairs within each space, then measures how ranks shift
    across spaces.

    Parameters
    ----------
    embeddings_by_space : dict mapping space name → (N, D_i) array.
        All arrays must have the same N (row-aligned). Keys define
        the space ordering (insertion order).
    pairs : (P, 2) int array of index pairs, or ``None`` to generate
        random pairs.
    n_pairs : number of random pairs when *pairs* is ``None``.
        Capped at N*(N-1)/2. Default 2000.
    metric : distance metric (default ``"euclidean"``).
    random_state : RNG seed for pair generation.

    Returns
    -------
    RankChangeResult
    """
    space_names = list(embeddings_by_space.keys())
    if len(space_names) < 2:
        raise ValueError(
            f"Need at least 2 spaces for rank-change analysis; got {len(space_names)}."
        )

    # Validate row counts match
    Ns = [embeddings_by_space[s].shape[0] for s in space_names]
    if len(set(Ns)) != 1:
        raise ValueError(
            f"All spaces must have the same number of rows; got {dict(zip(space_names, Ns))}."
        )
    N = Ns[0]

    # Generate or validate pairs
    if pairs is None:
        rng = np.random.default_rng(random_state)
        max_possible = N * (N - 1) // 2
        n_pairs = min(n_pairs, max_possible)
        # Generate unique random pairs
        pair_set = set()
        while len(pair_set) < n_pairs:
            batch = max(n_pairs - len(pair_set), 256)
            a = rng.integers(0, N, size=batch)
            b = rng.integers(0, N - 1, size=batch)
            b[b >= a] += 1
            for ai, bi in zip(a, b):
                pair_set.add((min(ai, bi), max(ai, bi)))
                if len(pair_set) >= n_pairs:
                    break
        pairs = np.array(sorted(pair_set), dtype=int)
    else:
        pairs = np.asarray(pairs, dtype=int)
        if pairs.ndim != 2 or pairs.shape[1] != 2:
            raise ValueError(f"pairs must be (P, 2); got shape {pairs.shape}")

    P = len(pairs)

    # Build rank matrix: (P, L)
    rank_cols = {}
    dist_cols = {}
    for sname in space_names:
        X = np.asarray(embeddings_by_space[sname], dtype=np.float64)
        dists = pairwise_distances_for_index_pairs(X, pairs, metric=metric)
        ranks = ranks_from_distances(dists)
        rank_cols[sname] = ranks
        dist_cols[sname] = dists

    rank_matrix = np.column_stack([rank_cols[s] for s in space_names])  # (P, L)

    # Per-pair statistics (matching master: ddof=0)
    rank_var = np.var(rank_matrix, axis=1, ddof=0)
    rank_std = np.std(rank_matrix, axis=1, ddof=0)
    rank_range = np.ptp(rank_matrix, axis=1)

    # Build pair_summary DataFrame
    summary_data = {
        'pair_idx_a': pairs[:, 0],
        'pair_idx_b': pairs[:, 1],
    }
    for sname in space_names:
        summary_data[f'dist_{sname}'] = dist_cols[sname]
        summary_data[f'rank_{sname}'] = rank_cols[sname]

    # Consecutive deltas
    delta_cols = {}
    for i in range(len(space_names) - 1):
        s_from = space_names[i]
        s_to = space_names[i + 1]
        delta_name = f'delta_{s_from}_to_{s_to}'
        delta = rank_cols[s_to].astype(np.float64) - rank_cols[s_from].astype(np.float64)
        delta_cols[delta_name] = delta
        summary_data[delta_name] = delta

    summary_data['rank_var'] = rank_var
    summary_data['rank_std'] = rank_std
    summary_data['rank_range'] = rank_range

    # Mean absolute delta across all transitions
    if delta_cols:
        abs_deltas = np.column_stack([np.abs(d) for d in delta_cols.values()])
        summary_data['mean_abs_delta'] = np.mean(abs_deltas, axis=1)

    pair_summary = pd.DataFrame(summary_data)

    # Transitions (moves) table — mirrors compute_replicate_rank_change_stats
    moves_rows = []
    for i in range(len(space_names) - 1):
        s_from = space_names[i]
        s_to = space_names[i + 1]
        delta = rank_cols[s_to].astype(np.float64) - rank_cols[s_from].astype(np.float64)
        abs_delta = np.abs(delta)
        moves_rows.append({
            'transition': f'{s_from} → {s_to}',
            'Mean |Δrank|': float(np.mean(abs_delta)),
            'Median |Δrank|': float(np.median(abs_delta)),
            'Std |Δrank|': float(np.std(abs_delta, ddof=0)),
            'Mean Δrank': float(np.mean(delta)),
        })
    moves = pd.DataFrame(moves_rows)

    # Rank table (pairs × spaces)
    rank_table = pd.DataFrame(rank_matrix, columns=space_names)

    # Overall summary (mirrors compute_nonrep_rank_change_stats)
    overall = {
        'n_pairs': P,
        'n_spaces': len(space_names),
        'mean_rank_var': float(np.mean(rank_var)),
        'median_rank_var': float(np.median(rank_var)),
        'std_rank_var': float(np.std(rank_var, ddof=0)),
        'mean_rank_std': float(np.mean(rank_std)),
        'mean_rank_range': float(np.mean(rank_range)),
        'median_rank_range': float(np.median(rank_range)),
    }

    return RankChangeResult(
        overall=overall,
        pair_summary=pair_summary,
        moves=moves,
        rank_table=rank_table,
    )
