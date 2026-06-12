"""Replicate and tendril layer diagnostics ported from candescence_master.

Pure-function module — no Inference object dependency.  Operates on
a DataFrame (with ``id``, ``my_rep``, ``transformed_image`` columns),
a VAE encoder, and an optional device.

Ported from ``candescence_master/projects/tlv/vae/inference.py``
(functions: compute_intermediate_distances_for_pairs,
compute_replicate_rank_matrices_per_layer, compute_nonrep_rank_*,
compute_replicate_rank_change_stats, plot_replicate_distance_distribution_per_layer,
inspect_distance_peaks_for_layer, plot_intermediate_layer_outliers,
show_pair_reconstructions_and_distances_intermediate,
get_tendril_latent_embedding / get_real_tendril_embedding).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import (
    Any,
    Callable,
    Dict,
    List,
    Optional,
    Sequence,
    Tuple,
    Union,
)

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.tlv.inference.tendril_replicate_diagnostics")


# ═══════════════════════════════════════════════════════════════════════
# Helper: pair builders
# ═══════════════════════════════════════════════════════════════════════

def build_replicate_pairs(
    df: pd.DataFrame,
    rep_col: str = "my_rep",
) -> List[Tuple[Any, Any]]:
    """Return (id_a, id_b) for every size-2 replicate group.

    Parameters
    ----------
    df : DataFrame with ``id`` and *rep_col* columns.
    rep_col : column holding replicate group identifiers.

    Returns
    -------
    List of (id_a, id_b) tuples sorted by group.
    """
    pairs: List[Tuple[int, int]] = []
    for _, grp in df.groupby(rep_col):
        if len(grp) == 2:
            ids = grp["id"].tolist()
            pairs.append((ids[0], ids[1]))
    return pairs


def build_nonrep_pairs(
    df: pd.DataFrame,
    n_pairs: int,
    rep_col: str = "my_rep",
    random_state: int = 42,
    max_trials: int = 200_000,
) -> List[Tuple[Any, Any]]:
    """Sample random non-replicate (id_a, id_b) pairs.

    Parameters
    ----------
    n_pairs : target count.
    max_trials : cap on rejection sampling iterations.

    Returns
    -------
    List of (id_a, id_b) tuples (may be shorter than *n_pairs* if
    rejection sampling hits the trial limit).
    """
    rng = np.random.default_rng(random_state)
    ids = df["id"].values
    reps = df.set_index("id")[rep_col]
    seen: set = set()
    pairs: List[Tuple[Any, Any]] = []
    for _ in range(max_trials):
        if len(pairs) >= n_pairs:
            break
        a, b = rng.choice(ids, size=2, replace=False)
        key = tuple(sorted([str(a), str(b)]))
        if key in seen:
            continue
        if reps[a] == reps[b]:
            continue
        seen.add(key)
        pairs.append((a, b))
    return pairs


# ═══════════════════════════════════════════════════════════════════════
# Helper: image / tensor fetching
# ═══════════════════════════════════════════════════════════════════════

def _fetch_tensor_by_id(
    df: pd.DataFrame,
    id_value,
    col: str = "transformed_image",
) -> torch.Tensor:
    """Return a CHW float32 [0, 1] tensor for *id_value*.

    Looks up *col* in *df*; handles ``torch.Tensor``, ``np.ndarray``,
    and PIL Images.
    """
    row = df.loc[df["id"] == id_value]
    if row.empty:
        raise KeyError(f"id {id_value} not found in df")
    obj = row.iloc[0][col]

    if isinstance(obj, torch.Tensor):
        t = obj.float()
        if t.max() > 1.0:
            t = t / 255.0
        return t

    if isinstance(obj, np.ndarray):
        t = torch.from_numpy(obj).float()
        if t.ndim == 3 and t.shape[-1] in (1, 3):
            t = t.permute(2, 0, 1)
        if t.max() > 1.0:
            t = t / 255.0
        return t

    # PIL Image fallback
    try:
        arr = np.array(obj).astype(np.float32) / 255.0
        return torch.from_numpy(arr).permute(2, 0, 1)
    except Exception as exc:
        raise TypeError(
            f"Cannot convert column '{col}' (type={type(obj)}) to tensor"
        ) from exc


def _fetch_rgb_image(
    df: pd.DataFrame,
    id_value,
    col: str = "rgb_image",
) -> np.ndarray:
    """Return HWC uint8 RGB array for *id_value*."""
    row = df.loc[df["id"] == id_value]
    if row.empty:
        raise KeyError(f"id {id_value} not found in df")
    obj = row.iloc[0][col]

    if isinstance(obj, np.ndarray):
        return obj.astype(np.uint8) if obj.dtype != np.uint8 else obj

    if isinstance(obj, torch.Tensor):
        arr = obj.detach().cpu().numpy()
        if arr.ndim == 3 and arr.shape[0] in (1, 3):
            arr = arr.transpose(1, 2, 0)
        if arr.max() <= 1.0:
            arr = (arr * 255).astype(np.uint8)
        return arr.astype(np.uint8)

    # PIL Image
    try:
        return np.array(obj.convert("RGB"), dtype=np.uint8)
    except Exception:
        return np.array(obj, dtype=np.uint8)


# ═══════════════════════════════════════════════════════════════════════
# Core: intermediate layer distances
# ═══════════════════════════════════════════════════════════════════════

def _build_conditioning(
    df: pd.DataFrame,
    id_value,
    strategy: int,
    cond_vars: Sequence[str] = ("average_hue", "average_saturation", "average_value"),
) -> Optional[torch.Tensor]:
    """Build a (1, cond_dim) conditioning tensor for *id_value*."""
    if strategy in (0, 1, 2, 3, 4, 5, 6, 11, 12):
        return None
    row = df.loc[df["id"] == id_value].iloc[0]
    vals = []
    for v in cond_vars:
        if v in row.index:
            vals.append(float(row[v]) / 255.0)
    if not vals:
        return None
    return torch.tensor([vals], dtype=torch.float32)


def compute_intermediate_distances_for_pairs(
    df: pd.DataFrame,
    vae: nn.Module,
    pairs: List[Tuple[Any, Any]],
    *,
    device: torch.device = torch.device("cpu"),
    layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
    pool_hw: int = 8,
    strategy: int = 14,
    image_col: str = "transformed_image",
    conditioning: Optional[np.ndarray] = None,
) -> pd.DataFrame:
    """Compute per-layer L2 distances for given pairs.

    Forwards each unique image once through the encoder, extracts skip
    features at each layer, optionally pools, then computes pairwise L2
    distances.

    Parameters
    ----------
    df : DataFrame with ``id`` and *image_col*.
    vae : VAE module whose ``.encoder(x, cond)`` returns
        ``(z, mu, logvar, skip_list)``.
    pairs : list of ``(id_a, id_b)``.
    device : compute device.
    layers : layer names (mapped to skip indices 0–3).
    pool_hw : adaptive-average-pool target size (0 = no pooling).
    strategy : training strategy number (controls conditioning).
    image_col : column holding image tensors.
    conditioning : optional ``(N, cond_dim)`` array aligned row-for-row
        with *df*.  When provided, this is used directly instead of
        :func:`_build_conditioning` (avoids dimension mismatches when
        the model was trained with fewer conditioning variables).

    Returns
    -------
    DataFrame with columns ``id_a``, ``id_b``, plus one distance column
    per layer name.
    """
    layer_idx_map = {f"x{i}": i for i in range(4)}

    # Build id → row-index mapping for conditioning lookup
    id_to_row: Dict[Any, int] = {}
    if conditioning is not None:
        for row_idx, id_val in enumerate(df["id"].values):
            id_to_row[id_val] = row_idx

    # Gather unique IDs
    unique_ids = set()
    for a, b in pairs:
        unique_ids.add(a)
        unique_ids.add(b)
    unique_ids = sorted(unique_ids, key=str)

    # Forward each unique ID, store skip features
    vae.eval()
    feats: Dict[Any, Dict[str, torch.Tensor]] = {}  # id -> {layer: tensor}

    with torch.no_grad():
        for uid in unique_ids:
            img = _fetch_tensor_by_id(df, uid, col=image_col).unsqueeze(0).to(device)
            if conditioning is not None and uid in id_to_row:
                cond_row = conditioning[id_to_row[uid]]
                cond = torch.tensor(cond_row, dtype=torch.float32).unsqueeze(0).to(device)
            else:
                cond = _build_conditioning(df, uid, strategy)
                if cond is not None:
                    cond = cond.to(device)

            if strategy in (14, 15, 16):
                _, _, _, skip_list = vae.encoder(img, cond)
            elif strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13):
                cond_list = [cond, cond] if cond is not None else None
                _, _, _, skip_list, _ = vae.encoder(img, cond_list)
            elif strategy in (7, 8, 9):
                _, _, _, skip_list, _ = vae.encoder(img, cond)
            else:
                enc_out = vae.encoder(img) if cond is None else vae.encoder(img, cond)
                if isinstance(enc_out, tuple) and len(enc_out) >= 4:
                    skip_list = enc_out[3]
                else:
                    skip_list = enc_out[3] if isinstance(enc_out, tuple) else []

            id_feats: Dict[str, torch.Tensor] = {}
            for lname in layers:
                idx = layer_idx_map.get(lname, int(lname) if lname.isdigit() else 0)
                if idx < len(skip_list):
                    feat = skip_list[idx]
                    if pool_hw and pool_hw > 0 and feat.ndim == 4:
                        feat = F.adaptive_avg_pool2d(feat, (pool_hw, pool_hw))
                    id_feats[lname] = feat.detach().cpu().flatten()
            feats[uid] = id_feats

            if torch.cuda.is_available():
                torch.cuda.empty_cache()

    # Compute distances
    records: List[Dict[str, Any]] = []
    for a, b in pairs:
        rec: Dict[str, Any] = {"id_a": a, "id_b": b}
        for lname in layers:
            fa = feats.get(a, {}).get(lname)
            fb = feats.get(b, {}).get(lname)
            if fa is not None and fb is not None:
                rec[lname] = float(torch.norm(fa - fb, p=2))
            else:
                rec[lname] = float("nan")
        records.append(rec)

    return pd.DataFrame(records)


# ═══════════════════════════════════════════════════════════════════════
# Ranking helpers
# ═══════════════════════════════════════════════════════════════════════

def _rank_column(values: np.ndarray) -> np.ndarray:
    """1-based ordinal ranks (ascending, stable mergesort)."""
    finite = np.isfinite(values)
    ranks = np.full_like(values, np.nan, dtype=np.float64)
    finite_vals = values[finite]
    order = np.argsort(finite_vals, kind="mergesort")
    r = np.empty_like(order)
    r[order] = np.arange(1, len(finite_vals) + 1)
    ranks[finite] = r
    return ranks


# ═══════════════════════════════════════════════════════════════════════
# Replicate rank matrices per layer
# ═══════════════════════════════════════════════════════════════════════

@dataclass
class ReplicateRankResult:
    """Output of :func:`compute_replicate_rank_matrices_per_layer`."""
    rank_table: pd.DataFrame        # (P, L) — rank per layer
    pair_summary: pd.DataFrame      # id_a, id_b, my_rep, dist_*, rank_*
    replicate_pairs: List[Tuple[int, int]]


def compute_replicate_rank_matrices_per_layer(
    df: pd.DataFrame,
    vae: nn.Module,
    *,
    device: torch.device = torch.device("cpu"),
    layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
    pool_hw: int = 8,
    strategy: int = 14,
    image_col: str = "transformed_image",
    rep_col: str = "my_rep",
) -> ReplicateRankResult:
    """Build per-layer rank table for replicate pairs.

    For each layer independently, ranks all replicate-pair distances from
    1 (closest) to P (farthest).
    """
    pairs = build_replicate_pairs(df, rep_col=rep_col)
    if len(pairs) < 2:
        raise ValueError(
            f"Need at least 2 replicate pairs; found {len(pairs)}."
        )

    dist_df = compute_intermediate_distances_for_pairs(
        df, vae, pairs,
        device=device, layers=layers, pool_hw=pool_hw,
        strategy=strategy, image_col=image_col,
    )

    # Build rank table
    rank_data = {}
    for lname in layers:
        rank_data[lname] = _rank_column(dist_df[lname].values)
    rank_table = pd.DataFrame(rank_data)

    # Pair summary
    id2rep = df.set_index("id")[rep_col]
    pair_summary = dist_df.copy()
    pair_summary["my_rep"] = [id2rep.get(a, None) for a, _ in pairs]
    for lname in layers:
        pair_summary[f"dist_{lname}"] = dist_df[lname]
        pair_summary[f"rank_{lname}"] = rank_data[lname]

    return ReplicateRankResult(
        rank_table=rank_table,
        pair_summary=pair_summary,
        replicate_pairs=pairs,
    )


# ═══════════════════════════════════════════════════════════════════════
# Non-replicate rank matrices per layer
# ═══════════════════════════════════════════════════════════════════════

@dataclass
class NonrepRankResult:
    """Output of :func:`compute_nonrep_rank_matrices_per_layer`."""
    rank_table: pd.DataFrame
    pair_summary: pd.DataFrame
    nonrep_pairs: List[Tuple[int, int]]


def compute_nonrep_rank_matrices_per_layer(
    df: pd.DataFrame,
    vae: nn.Module,
    *,
    device: torch.device = torch.device("cpu"),
    layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
    pool_hw: int = 8,
    strategy: int = 14,
    image_col: str = "transformed_image",
    rep_col: str = "my_rep",
    n_pairs: Optional[int] = None,
    random_state: int = 42,
) -> NonrepRankResult:
    """Build per-layer rank table for random non-replicate pairs."""
    if n_pairs is None:
        n_pairs = len(build_replicate_pairs(df, rep_col=rep_col))
    if n_pairs < 2:
        raise ValueError("Need n_pairs >= 2.")

    pairs = build_nonrep_pairs(
        df, n_pairs=n_pairs, rep_col=rep_col, random_state=random_state,
    )

    dist_df = compute_intermediate_distances_for_pairs(
        df, vae, pairs,
        device=device, layers=layers, pool_hw=pool_hw,
        strategy=strategy, image_col=image_col,
    )

    rank_data = {}
    for lname in layers:
        rank_data[lname] = _rank_column(dist_df[lname].values)
    rank_table = pd.DataFrame(rank_data)

    id2rep = df.set_index("id")[rep_col]
    pair_summary = dist_df.copy()
    pair_summary["my_rep_a"] = [id2rep.get(a, None) for a, _ in pairs]
    pair_summary["my_rep_b"] = [id2rep.get(b, None) for _, b in pairs]
    for lname in layers:
        pair_summary[f"dist_{lname}"] = dist_df[lname]
        pair_summary[f"rank_{lname}"] = rank_data[lname]

    return NonrepRankResult(
        rank_table=rank_table,
        pair_summary=pair_summary,
        nonrep_pairs=pairs,
    )


# ═══════════════════════════════════════════════════════════════════════
# Rank change statistics
# ═══════════════════════════════════════════════════════════════════════

@dataclass
class RankChangeStatsResult:
    """Output of :func:`compute_rank_change_stats`."""
    rank_table: pd.DataFrame        # original (P, L) rank matrix
    per_pair: pd.DataFrame          # augmented with var/std/range/delta cols
    by_layer: pd.DataFrame          # per-layer summary stats
    moves: pd.DataFrame             # per-transition |Δrank| stats
    volatility: Dict[str, float]    # overall summary
    overall_text: str               # formatted text report


def compute_rank_change_stats(
    rank_table: pd.DataFrame,
    pair_summary: Optional[pd.DataFrame] = None,
) -> RankChangeStatsResult:
    """Compute per-pair and per-transition rank-change statistics.

    Mirrors ``compute_replicate_rank_change_stats`` from master.
    Works for both replicate and non-replicate rank tables.

    Parameters
    ----------
    rank_table : (P, L) DataFrame of ranks (1-based).
    pair_summary : optional augmented DataFrame (id_a, id_b, …).
    """
    layer_cols = list(rank_table.columns)
    R = rank_table[layer_cols].values.astype(np.float64)  # (P, L)
    P, L = R.shape

    # ── Per-pair metrics ────────────────────────────────────────────
    per_pair = pair_summary.copy() if pair_summary is not None else rank_table.copy()

    per_pair["mean_rank"] = np.nanmean(R, axis=1)
    per_pair["std_rank"] = np.nanstd(R, axis=1, ddof=0)
    per_pair["range_rank"] = np.nanmax(R, axis=1) - np.nanmin(R, axis=1)

    # Best / worst layer
    per_pair["best_layer"] = rank_table[layer_cols].idxmin(axis=1)
    per_pair["worst_layer"] = rank_table[layer_cols].idxmax(axis=1)

    # Normalised stability score (master formula)
    uniform_std = P / np.sqrt(12)
    per_pair["std_rank_norm"] = per_pair["std_rank"] / uniform_std if uniform_std > 0 else 0.0
    per_pair["stability_score"] = 1.0 - np.clip(per_pair["std_rank_norm"], 0, 1)

    # Consecutive deltas
    delta_cols = []
    for i in range(L - 1):
        col_from, col_to = layer_cols[i], layer_cols[i + 1]
        dname = f"Δ{col_from}→{col_to}"
        per_pair[dname] = R[:, i + 1] - R[:, i]
        delta_cols.append(dname)

    if delta_cols:
        abs_deltas = np.column_stack([np.abs(per_pair[c].values) for c in delta_cols])
        per_pair["mean_abs_delta"] = np.nanmean(abs_deltas, axis=1)
        per_pair["mean_delta"] = np.nanmean(
            np.column_stack([per_pair[c].values for c in delta_cols]), axis=1
        )

        # Ups / downs / flats
        signed = np.column_stack([per_pair[c].values for c in delta_cols])
        per_pair["ups"] = np.sum(signed > 0, axis=1)
        per_pair["downs"] = np.sum(signed < 0, axis=1)
        per_pair["flats"] = np.sum(signed == 0, axis=1)

    # Spearman trend (rank vs layer index)
    from scipy.stats import spearmanr
    trends = np.full(P, np.nan)
    for i in range(P):
        row = R[i]
        finite = np.isfinite(row)
        if finite.sum() >= 3:
            rho, _ = spearmanr(np.arange(L)[finite], row[finite])
            trends[i] = rho
    per_pair["spearman_trend"] = trends

    # ── Per-layer summary ───────────────────────────────────────────
    layer_stats: List[Dict[str, Any]] = []
    for lname in layer_cols:
        vals = rank_table[lname].dropna().values
        layer_stats.append({
            "layer": lname,
            "n_pairs": len(vals),
            "mean": float(np.mean(vals)),
            "median": float(np.median(vals)),
            "std": float(np.std(vals, ddof=0)),
            "min": float(np.min(vals)),
            "p10": float(np.percentile(vals, 10)),
            "p90": float(np.percentile(vals, 90)),
            "max": float(np.max(vals)),
        })
    by_layer = pd.DataFrame(layer_stats)

    # ── Transition moves table ──────────────────────────────────────
    moves_rows: List[Dict[str, Any]] = []
    for i in range(L - 1):
        col_from, col_to = layer_cols[i], layer_cols[i + 1]
        delta = R[:, i + 1] - R[:, i]
        ad = np.abs(delta)
        moves_rows.append({
            "transition": f"{col_from} → {col_to}",
            "Mean |Δrank|": float(np.nanmean(ad)),
            "Median |Δrank|": float(np.nanmedian(ad)),
            "Std |Δrank|": float(np.nanstd(ad, ddof=0)),
            "Mean Δrank": float(np.nanmean(delta)),
        })
    moves = pd.DataFrame(moves_rows)

    # ── Volatility summary ──────────────────────────────────────────
    std_vals = per_pair["std_rank"].values
    rng_vals = per_pair["range_rank"].values
    volatility = {
        "mean_of_std_rank": float(np.nanmean(std_vals)),
        "median_of_std_rank": float(np.nanmedian(std_vals)),
        "std_of_std_rank": float(np.nanstd(std_vals, ddof=0)),
        "mean_of_range_rank": float(np.nanmean(rng_vals)),
        "median_of_range_rank": float(np.nanmedian(rng_vals)),
        "std_of_range_rank": float(np.nanstd(rng_vals, ddof=0)),
    }

    # ── Text report ─────────────────────────────────────────────────
    lines = [
        f"Rank change stats: {P} pairs × {L} layers ({', '.join(layer_cols)})",
        "",
        "Per-layer summary:",
        by_layer.to_string(index=False),
        "",
        "Transition moves:",
        moves.to_string(index=False),
        "",
        "Volatility:",
    ]
    for k, v in volatility.items():
        lines.append(f"  {k}: {v:.4f}")

    return RankChangeStatsResult(
        rank_table=rank_table,
        per_pair=per_pair,
        by_layer=by_layer,
        moves=moves,
        volatility=volatility,
        overall_text="\n".join(lines),
    )


# ═══════════════════════════════════════════════════════════════════════
# Plotting: replicate vs random distance distributions per layer
# ═══════════════════════════════════════════════════════════════════════

def plot_replicate_distance_distribution_per_layer(
    rep_dist_df: pd.DataFrame,
    nonrep_dist_df: pd.DataFrame,
    layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
    bins: int = 50,
    title_prefix: str = "Layer",
) -> Dict[str, plt.Figure]:
    """Overlaid histograms of replicate vs random pair distances per layer.

    Parameters
    ----------
    rep_dist_df : output of ``compute_intermediate_distances_for_pairs``
        for replicate pairs.
    nonrep_dist_df : same for non-replicate pairs.

    Returns
    -------
    Dict mapping layer name → matplotlib Figure.
    """
    figs: Dict[str, plt.Figure] = {}
    for lname in layers:
        fig, ax = plt.subplots(figsize=(8, 4))
        rep_vals = rep_dist_df[lname].dropna().values
        non_vals = nonrep_dist_df[lname].dropna().values
        all_vals = np.concatenate([rep_vals, non_vals])
        lo, hi = float(all_vals.min()), float(all_vals.max())
        edges = np.linspace(lo, hi, bins + 1)

        ax.hist(rep_vals, bins=edges, alpha=0.6, label=f"Replicate (n={len(rep_vals)})", color="steelblue")
        ax.hist(non_vals, bins=edges, alpha=0.6, label=f"Random (n={len(non_vals)})", color="salmon")
        ax.set_xlabel("L2 distance")
        ax.set_ylabel("Count")
        ax.set_title(f"{title_prefix} {lname}")
        ax.legend()
        fig.tight_layout()
        figs[lname] = fig
    return figs


# ═══════════════════════════════════════════════════════════════════════
# Plotting: intermediate layer outliers
# ═══════════════════════════════════════════════════════════════════════

def plot_intermediate_layer_outliers(
    dist_df: pd.DataFrame,
    df: pd.DataFrame,
    layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
    n_outliers: int = 10,
    label: str = "Replicate",
    image_col: str = "rgb_image",
) -> Dict[str, Tuple[plt.Figure, plt.Figure]]:
    """Show left/right outlier pairs (most similar / most different) per layer.

    Returns dict ``{layer → (left_fig, right_fig)}`` where *left_fig* shows
    the ``n_outliers`` pairs with the smallest distance ("Most Similar") and
    *right_fig* shows the largest ("Most Different").

    Each figure arranges pairs in 2 rows of ``ceil(n_outliers / 2)`` pairs.
    Image-A is captioned ``A:{id}``, image-B is captioned ``B:{id}\\nd={dist}``.
    """
    figs: Dict[str, Tuple[plt.Figure, plt.Figure]] = {}
    for lname in layers:
        vals = dist_df[[lname, "id_a", "id_b"]].dropna(subset=[lname])
        if vals.empty:
            continue

        sorted_asc = vals.sort_values(lname)
        left = sorted_asc.head(n_outliers)
        right = sorted_asc.tail(n_outliers).iloc[::-1]

        n_left = min(n_outliers, len(left))
        n_right = min(n_outliers, len(right))
        if n_left == 0 and n_right == 0:
            continue

        left_fig = _outlier_grid(
            left, df, lname, n_left, image_col,
            title=f"{lname} — {label} Left Outliers (Most Similar)",
        )
        right_fig = _outlier_grid(
            right, df, lname, n_right, image_col,
            title=f"{lname} — {label} Right Outliers (Most Different)",
        )
        figs[lname] = (left_fig, right_fig)
    return figs


def _outlier_grid(
    subset: pd.DataFrame,
    df: pd.DataFrame,
    layer: str,
    n_pairs: int,
    image_col: str,
    title: str,
) -> plt.Figure:
    """Render a grid of image pairs for one side (left or right) of outliers.

    Layout: 2 rows, ``ceil(n_pairs / 2)`` pairs per row.  Each pair is two
    adjacent columns (image A + image B).
    """
    import math

    pairs_per_row = math.ceil(n_pairs / 2)
    n_rows = 2
    n_cols = pairs_per_row * 2  # A + B per pair

    fig, axes = plt.subplots(
        n_rows, n_cols,
        figsize=(2.5 * n_cols, 3 * n_rows),
    )
    if axes.ndim == 1:
        axes = axes.reshape(1, -1)

    fig.suptitle(title, fontsize=11)

    # Turn all axes off first
    for ax_row in axes:
        for ax in ax_row:
            ax.axis("off")

    pair_iter = subset.iterrows()
    for pair_idx in range(n_pairs):
        try:
            _, r = next(pair_iter)
        except StopIteration:
            break
        row = pair_idx // pairs_per_row
        col_in_row = pair_idx % pairs_per_row

        try:
            img_a = _fetch_rgb_image(df, r["id_a"], col=image_col)
            img_b = _fetch_rgb_image(df, r["id_b"], col=image_col)
        except Exception:
            continue

        ax_a = axes[row, col_in_row * 2]
        ax_b = axes[row, col_in_row * 2 + 1]

        ax_a.imshow(img_a)
        ax_a.set_title(f"A:{r['id_a']}", fontsize=7)
        ax_a.axis("off")

        ax_b.imshow(img_b)
        ax_b.set_title(f"B:{r['id_b']}\nd={r[layer]:.3f}", fontsize=7)
        ax_b.axis("off")

    fig.tight_layout()
    return fig


# ═══════════════════════════════════════════════════════════════════════
# Peak inspection
# ═══════════════════════════════════════════════════════════════════════

@dataclass
class PeakInfo:
    """One detected peak in a distance histogram."""
    bin_idx: int
    center: float
    width: float
    prominence: float
    example_pairs: List[Tuple[int, int, float]]  # (id_a, id_b, dist)


def inspect_distance_peaks_for_layer(
    dist_df: pd.DataFrame,
    df: pd.DataFrame,
    layer: str = "x1",
    bins: int = 60,
    n_examples_per_peak: int = 6,
    max_peaks: int = 3,
    image_col: str = "rgb_image",
) -> Dict[str, Any]:
    """Detect peaks in the distance histogram and select example pairs.

    Parameters
    ----------
    dist_df : output of :func:`compute_intermediate_distances_for_pairs`.
    df : full DataFrame (must contain *image_col* for visualization).

    Returns
    -------
    Dict with ``peaks`` (list of PeakInfo), ``counts``, ``edges``,
    and ``fig`` (matplotlib Figure of histogram + peak markers).
    """
    vals = dist_df[[layer, "id_a", "id_b"]].dropna(subset=[layer])
    distances = vals[layer].values
    ids_a = vals["id_a"].values
    ids_b = vals["id_b"].values

    counts, edges = np.histogram(distances, bins=bins)
    centers = (edges[:-1] + edges[1:]) / 2
    widths = edges[1:] - edges[:-1]

    # Find local maxima
    peak_indices: List[int] = []
    for i in range(1, len(counts) - 1):
        if counts[i] > counts[i - 1] and counts[i] >= counts[i + 1]:
            peak_indices.append(i)

    # Prominence: height above min of left/right valley
    prominences: List[float] = []
    for pi in peak_indices:
        left_min = np.min(counts[:pi]) if pi > 0 else 0
        right_min = np.min(counts[pi + 1:]) if pi < len(counts) - 1 else 0
        prominences.append(float(counts[pi]) - max(left_min, right_min))

    # Sort by prominence, keep top max_peaks
    ranked = sorted(zip(peak_indices, prominences), key=lambda x: -x[1])
    top_peaks = ranked[:max_peaks]

    peaks: List[PeakInfo] = []
    for pi, prom in top_peaks:
        lo, hi = edges[pi], edges[pi + 1]
        mask = (distances >= lo) & (distances < hi)
        candidates = list(zip(ids_a[mask], ids_b[mask], distances[mask]))
        rng = np.random.default_rng(0)
        if len(candidates) > n_examples_per_peak:
            idxs = rng.choice(len(candidates), n_examples_per_peak, replace=False)
            candidates = [candidates[j] for j in idxs]
        peaks.append(PeakInfo(
            bin_idx=pi,
            center=float(centers[pi]),
            width=float(widths[pi]),
            prominence=prom,
            example_pairs=[(a, b, float(d)) for a, b, d in candidates],
        ))

    # Plot
    fig, ax = plt.subplots(figsize=(10, 4))
    ax.bar(centers, counts, width=widths, alpha=0.7, color="steelblue", edgecolor="white")
    for pk in peaks:
        ax.axvline(pk.center, color="red", linestyle="--", alpha=0.7)
        ax.annotate(
            f"peak D={pk.center:.2f}",
            (pk.center, counts[pk.bin_idx]),
            textcoords="offset points", xytext=(5, 10), fontsize=8, color="red",
        )
    ax.set_xlabel("L2 distance")
    ax.set_ylabel("Count")
    ax.set_title(f"Distance distribution — {layer} (n={len(distances)} pairs)")
    fig.tight_layout()

    return {
        "peaks": peaks,
        "counts": counts,
        "edges": edges,
        "fig": fig,
    }


# ═══════════════════════════════════════════════════════════════════════
# Pair reconstruction grids
# ═══════════════════════════════════════════════════════════════════════

def show_pair_reconstructions_grid(
    dist_df: pd.DataFrame,
    df: pd.DataFrame,
    layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
    n_pairs: int = 10,
    title: str = "Pair distances",
    image_col: str = "rgb_image",
) -> plt.Figure:
    """Grid of image pairs with per-layer distances.

    Layout: rows = pairs, columns = [Image A, distance text, Image B].
    """
    subset = dist_df.head(n_pairs)
    n = len(subset)
    if n == 0:
        fig, ax = plt.subplots()
        ax.text(0.5, 0.5, "No pairs", ha="center", va="center")
        return fig

    fig, axes = plt.subplots(n, 3, figsize=(8, 2.5 * n))
    if n == 1:
        axes = axes.reshape(1, 3)

    fig.suptitle(title, fontsize=11, y=1.01)

    for row, (_, rec) in enumerate(subset.iterrows()):
        id_a, id_b = rec["id_a"], rec["id_b"]
        try:
            img_a = _fetch_rgb_image(df, id_a, col=image_col)
            img_b = _fetch_rgb_image(df, id_b, col=image_col)
        except Exception:
            for c in range(3):
                axes[row, c].axis("off")
            continue

        axes[row, 0].imshow(img_a)
        axes[row, 0].set_title(f"ID {id_a}", fontsize=8)
        axes[row, 0].axis("off")

        # Distance text
        dist_lines = [f"{l}: {rec.get(l, float('nan')):.3f}" for l in layers]
        axes[row, 1].text(
            0.5, 0.5, "\n".join(dist_lines),
            ha="center", va="center", fontsize=8, family="monospace",
        )
        axes[row, 1].axis("off")

        axes[row, 2].imshow(img_b)
        axes[row, 2].set_title(f"ID {id_b}", fontsize=8)
        axes[row, 2].axis("off")

    fig.tight_layout()
    return fig


# ═══════════════════════════════════════════════════════════════════════
# Tendril embedding helpers
# ═══════════════════════════════════════════════════════════════════════

def compute_tendril_latent_embeddings(
    df: pd.DataFrame,
    vae: nn.Module,
    tendrils: Any,
    *,
    device: torch.device = torch.device("cpu"),
    strategy: int = 14,
    image_col: str = "transformed_image",
    batch_size: int = 32,
) -> Dict[str, pd.DataFrame]:
    """Compute per-layer tendril latent embeddings for all images in *df*.

    Forwards each image through the main encoder to get skip features,
    then through each loaded tendril VAE to get tendril mu vectors.

    Parameters
    ----------
    tendrils : ``Tendrils`` container with trained models.

    Returns
    -------
    Dict mapping tendril key (e.g. ``"0"``, ``"1"``) → DataFrame
    with columns ``id, latent_0, latent_1, ...``.
    """
    vae.eval()
    tendril_keys = tendrils.list_tendrils()
    if not tendril_keys:
        return {}

    # Collect skip features per layer for all images
    ids = df["id"].tolist()
    layer_feats: Dict[str, List[torch.Tensor]] = {k: [] for k in tendril_keys}

    with torch.no_grad():
        for uid in ids:
            img = _fetch_tensor_by_id(df, uid, col=image_col).unsqueeze(0).to(device)
            cond = _build_conditioning(df, uid, strategy)
            if cond is not None:
                cond = cond.to(device)

            if strategy in (14, 15, 16):
                _, _, _, skip_list = vae.encoder(img, cond)
            elif strategy in (7, 8, 9, 9.5, 9.6, 9.7, 9.8, 9.9, 13):
                enc_out = vae.encoder(img, cond)
                skip_list = enc_out[3] if isinstance(enc_out, tuple) and len(enc_out) >= 4 else []
            else:
                enc_out = vae.encoder(img) if cond is None else vae.encoder(img, cond)
                skip_list = enc_out[3] if isinstance(enc_out, tuple) and len(enc_out) >= 4 else []

            for ki, tkey in enumerate(tendril_keys):
                if ki < len(skip_list):
                    layer_feats[tkey].append(skip_list[ki].detach().cpu())

            if torch.cuda.is_available():
                torch.cuda.empty_cache()

    # Forward through each tendril VAE
    result: Dict[str, pd.DataFrame] = {}
    for tkey in tendril_keys:
        feats_list = layer_feats[tkey]
        if not feats_list:
            continue
        all_feats = torch.cat(feats_list, dim=0)  # (N, C, H, W)

        trainer = tendrils.get_tendril(tkey)
        model = trainer.model
        model.eval()
        model_device = next(model.parameters()).device

        mus: List[torch.Tensor] = []
        with torch.no_grad():
            for start in range(0, len(all_feats), batch_size):
                batch = all_feats[start:start + batch_size]
                # Per-image normalization (same as training)
                batch = tendrils.normalize_per_image(batch)
                batch = batch.to(model_device)

                _, mu, _, _ = model(batch)

                mus.append(mu.detach().cpu())

        all_mu = torch.cat(mus, dim=0).numpy()  # (N, latent_dim)
        latent_cols = [f"latent_{i}" for i in range(all_mu.shape[1])]
        tdf = pd.DataFrame(all_mu, columns=latent_cols)
        tdf["id"] = ids[:len(tdf)]
        result[tkey] = tdf

    return result


def compute_real_layer_embeddings(
    df: pd.DataFrame,
    vae: nn.Module,
    *,
    device: torch.device = torch.device("cpu"),
    strategy: int = 14,
    image_col: str = "transformed_image",
    pool_hw: int = 8,
    layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
) -> Dict[str, pd.DataFrame]:
    """Extract actual intermediate encoder activations (skip features).

    Unlike tendril embeddings, these are the raw pooled+flattened encoder
    skip features — no tendril VAE forward pass.

    Returns
    -------
    Dict mapping layer name → DataFrame with columns ``id, latent_0, ...``.
    """
    layer_idx_map = {f"x{i}": i for i in range(4)}
    vae.eval()
    ids = df["id"].tolist()

    # Collect per-layer features
    layer_vecs: Dict[str, List[np.ndarray]] = {l: [] for l in layers}

    with torch.no_grad():
        for uid in ids:
            img = _fetch_tensor_by_id(df, uid, col=image_col).unsqueeze(0).to(device)
            cond = _build_conditioning(df, uid, strategy)
            if cond is not None:
                cond = cond.to(device)

            if strategy in (14, 15, 16):
                _, _, _, skip_list = vae.encoder(img, cond)
            elif strategy in (7, 8, 9, 9.5, 9.6, 9.7, 9.8, 9.9, 13):
                enc_out = vae.encoder(img, cond)
                skip_list = enc_out[3] if isinstance(enc_out, tuple) and len(enc_out) >= 4 else []
            else:
                enc_out = vae.encoder(img) if cond is None else vae.encoder(img, cond)
                skip_list = enc_out[3] if isinstance(enc_out, tuple) and len(enc_out) >= 4 else []

            for lname in layers:
                idx = layer_idx_map.get(lname, 0)
                if idx < len(skip_list):
                    feat = skip_list[idx]
                    if pool_hw and pool_hw > 0 and feat.ndim == 4:
                        feat = F.adaptive_avg_pool2d(feat, (pool_hw, pool_hw))
                    layer_vecs[lname].append(feat.flatten().cpu().numpy())

            if torch.cuda.is_available():
                torch.cuda.empty_cache()

    result: Dict[str, pd.DataFrame] = {}
    for lname in layers:
        vecs = layer_vecs[lname]
        if not vecs:
            continue
        mat = np.stack(vecs)
        cols = [f"latent_{i}" for i in range(mat.shape[1])]
        ldf = pd.DataFrame(mat, columns=cols)
        ldf["id"] = ids[:len(ldf)]
        result[lname] = ldf

    return result
