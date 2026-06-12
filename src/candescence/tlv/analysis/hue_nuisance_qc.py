"""Hue-nuisance quantification for latent-space quality control.

Fits within-media OLS regressions of latent embeddings on HSV colour
features, computes residuals, and compares PCA explained-variance ratios
before and after residualization.  This directly answers: "Is PC1 mostly
hue?" and "Does hue regression remove a large share of variance from
the dominant axis?"

Usage (CLI)::

    python scripts/quantify_hue_nuisance.py \\
        --embeddings run_mu.npy \\
        --metadata   run_metadata.csv \\
        --output-dir results/hue_qc

Usage (library)::

    from candescence.tlv.analysis.hue_nuisance_qc import (
        within_group_residuals, pca_report, HueNuisanceReport,
    )

    resid = within_group_residuals(M, media, hsv)
    raw_pca   = pca_report(M, media, hsv[:, 0], label="raw")
    clean_pca = pca_report(resid, media, hsv[:, 0], label="residual")

All functions are pure numpy / sklearn / pandas with **no** Streamlit
dependency.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.linear_model import LinearRegression

logger = logging.getLogger(__name__)

# Minimum samples per stratum to fit a 4-coefficient OLS (intercept + h,s,v).
_MIN_STRATUM_N = 10

# Number of PCA components to report.
_DEFAULT_N_PCS = 8


# ---------------------------------------------------------------------------
# Result containers
# ---------------------------------------------------------------------------

@dataclass
class StratumFitInfo:
    """Per-media-stratum OLS summary."""

    media: str
    n_samples: int
    mean_r2: float  # mean R² across latent dimensions
    skipped: bool  # True if n < _MIN_STRATUM_N


@dataclass
class PCAReportResult:
    """PCA summary for one embedding matrix."""

    label: str
    n_samples: int
    n_dims: int
    evr: np.ndarray  # explained-variance ratios for first n_pcs components
    cum_evr: np.ndarray
    pc1_hue_corr_overall: float  # Pearson r(PC1, hue) across all rows
    pc1_hue_corr_per_media: Dict[str, float]  # per-media Pearson r(PC1, hue)


@dataclass
class HueNuisanceReport:
    """Full report for one latent space."""

    space_name: str
    n: int
    n_dropped: int
    strata: List[StratumFitInfo]
    pca_raw: PCAReportResult
    pca_residual: PCAReportResult
    delta_evr_pc1: float  # evr_pc1_raw - evr_pc1_residual


# ---------------------------------------------------------------------------
# Core functions
# ---------------------------------------------------------------------------

def build_design_matrix(
    hue: np.ndarray,
    saturation: np.ndarray,
    value: np.ndarray,
) -> np.ndarray:
    """Stack normalised HSV columns into an (N, 3) design matrix.

    Parameters
    ----------
    hue, saturation, value : (N,) arrays on 0-255 storage scale.

    Returns
    -------
    (N, 3) float64 array with columns h/255, s/255, v/255.
    """
    h = np.asarray(hue, dtype=np.float64) / 255.0
    s = np.asarray(saturation, dtype=np.float64) / 255.0
    v = np.asarray(value, dtype=np.float64) / 255.0
    return np.column_stack([h, s, v])


def within_group_residuals(
    M: np.ndarray,
    media: np.ndarray,
    hsv: np.ndarray,
    min_n: int = _MIN_STRATUM_N,
) -> Tuple[np.ndarray, List[StratumFitInfo]]:
    """Fit per-media OLS  M_g ~ [1, h, s, v]  and return stacked residuals.

    Parameters
    ----------
    M : (N, D) embedding matrix (float).
    media : (N,) string media labels.
    hsv : (N, 3) design matrix (already on 0-1 scale, e.g. from
        ``build_design_matrix``).
    min_n : skip strata with fewer than this many samples.

    Returns
    -------
    residuals : (N, D) array — OLS residuals stacked across strata.
        For skipped strata the original rows are kept (no regression).
    strata_info : list of per-stratum fit summaries.
    """
    M = np.asarray(M, dtype=np.float64)
    hsv = np.asarray(hsv, dtype=np.float64)
    media = np.asarray(media)

    N, D = M.shape
    residuals = np.empty_like(M)
    strata_info: List[StratumFitInfo] = []

    for g in np.unique(media):
        mask = media == g
        n_g = int(mask.sum())

        if n_g < min_n:
            logger.warning(
                "Stratum '%s' has %d samples (< %d); skipping regression.",
                g, n_g, min_n,
            )
            residuals[mask] = M[mask]
            strata_info.append(StratumFitInfo(
                media=str(g), n_samples=n_g, mean_r2=float("nan"), skipped=True,
            ))
            continue

        X_g = hsv[mask]
        M_g = M[mask]
        reg = LinearRegression().fit(X_g, M_g)
        pred = reg.predict(X_g)
        residuals[mask] = M_g - pred

        # Mean R² across dimensions (cheap per-dim metric)
        ss_res = np.sum((M_g - pred) ** 2, axis=0)
        ss_tot = np.sum((M_g - M_g.mean(axis=0)) ** 2, axis=0)
        # Guard against zero-variance dimensions
        with np.errstate(divide="ignore", invalid="ignore"):
            r2_dims = 1.0 - ss_res / ss_tot
        r2_dims = np.where(np.isfinite(r2_dims), r2_dims, 0.0)
        mean_r2 = float(np.mean(r2_dims))

        strata_info.append(StratumFitInfo(
            media=str(g), n_samples=n_g, mean_r2=round(mean_r2, 4), skipped=False,
        ))

    return residuals, strata_info


def pca_report(
    M: np.ndarray,
    media: np.ndarray,
    hue_01: np.ndarray,
    label: str = "",
    n_pcs: int = _DEFAULT_N_PCS,
) -> PCAReportResult:
    """Run PCA and compute hue correlations on PC1.

    Parameters
    ----------
    M : (N, D) embedding matrix.
    media : (N,) media labels.
    hue_01 : (N,) hue on 0-1 scale.
    label : human-readable tag (e.g. "raw", "residual").
    n_pcs : number of components to fit.

    Returns
    -------
    PCAReportResult
    """
    M = np.asarray(M, dtype=np.float64)
    hue_01 = np.asarray(hue_01, dtype=np.float64)
    media = np.asarray(media)

    N, D = M.shape
    k = min(n_pcs, N, D)
    pca = PCA(n_components=k)
    scores = pca.fit_transform(M)

    evr = pca.explained_variance_ratio_
    cum_evr = np.cumsum(evr)

    # Overall Pearson r(PC1, hue)
    pc1 = scores[:, 0]
    overall_corr = _safe_pearson(pc1, hue_01)

    # Per-media Pearson r(PC1, hue)
    per_media: Dict[str, float] = {}
    for g in np.unique(media):
        mask = media == g
        per_media[str(g)] = _safe_pearson(pc1[mask], hue_01[mask])

    return PCAReportResult(
        label=label,
        n_samples=N,
        n_dims=D,
        evr=evr,
        cum_evr=cum_evr,
        pc1_hue_corr_overall=round(overall_corr, 4),
        pc1_hue_corr_per_media=per_media,
    )


# ---------------------------------------------------------------------------
# Plotting (optional, graceful degradation if matplotlib absent)
# ---------------------------------------------------------------------------

def plot_pc_scatter(
    M: np.ndarray,
    media: np.ndarray,
    title: str = "PCA",
    out_path: Optional[Path] = None,
    n_pcs: int = 2,
) -> Optional[Path]:
    """PC1 vs PC2 scatter coloured by media; save PNG if *out_path* given.

    Returns the path written, or None if matplotlib is unavailable.
    """
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        logger.warning("matplotlib not installed; skipping scatter plot.")
        return None

    M = np.asarray(M, dtype=np.float64)
    media = np.asarray(media)
    k = min(n_pcs, M.shape[0], M.shape[1])
    pca = PCA(n_components=k)
    scores = pca.fit_transform(M)

    fig, ax = plt.subplots(figsize=(7, 6))
    for g in np.unique(media):
        mask = media == g
        ax.scatter(scores[mask, 0], scores[mask, 1], label=g, s=8, alpha=0.5)
    ax.set_xlabel(f"PC1 ({pca.explained_variance_ratio_[0]:.1%})")
    ax.set_ylabel(f"PC2 ({pca.explained_variance_ratio_[1]:.1%})")
    ax.set_title(title)
    ax.legend(markerscale=3, fontsize=8)
    fig.tight_layout()

    if out_path is not None:
        out_path = Path(out_path)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(out_path, dpi=150)
        logger.info("Saved scatter plot to %s", out_path)
    plt.close(fig)
    return out_path


# ---------------------------------------------------------------------------
# End-to-end pipeline
# ---------------------------------------------------------------------------

def run_hue_nuisance_qc(
    M: np.ndarray,
    metadata: pd.DataFrame,
    space_name: str = "primary",
    n_pcs: int = _DEFAULT_N_PCS,
    output_dir: Optional[Path] = None,
) -> HueNuisanceReport:
    """Full hue-nuisance QC pipeline for a single latent space.

    Parameters
    ----------
    M : (N, D) embedding matrix (posterior mean *mu* preferred).
    metadata : DataFrame with columns ``media``, ``average_hue``,
        ``average_saturation``, ``average_value``.  Must have same row
        count / order as *M*.
    space_name : label for this latent space.
    n_pcs : components for PCA report.
    output_dir : if given, write CSV + optional PNGs here.

    Returns
    -------
    HueNuisanceReport
    """
    M = np.asarray(M, dtype=np.float64)
    required = {"media", "average_hue", "average_saturation", "average_value"}
    missing = required - set(metadata.columns)
    if missing:
        raise ValueError(f"metadata missing columns: {missing}")

    if len(metadata) != M.shape[0]:
        raise ValueError(
            f"Row count mismatch: M has {M.shape[0]} rows, "
            f"metadata has {len(metadata)} rows."
        )

    # --- Drop rows with missing media or NaN HSV ---
    hsv_cols = ["average_hue", "average_saturation", "average_value"]
    valid_mask = (
        metadata["media"].notna()
        & metadata[hsv_cols].notna().all(axis=1)
    )
    n_dropped = int((~valid_mask).sum())
    if n_dropped > 0:
        logger.info(
            "Dropping %d / %d rows with missing media or NaN HSV.",
            n_dropped, len(metadata),
        )
    M = M[valid_mask.values]
    metadata = metadata.loc[valid_mask].reset_index(drop=True)

    media = metadata["media"].values
    hsv = build_design_matrix(
        metadata["average_hue"].values,
        metadata["average_saturation"].values,
        metadata["average_value"].values,
    )
    hue_01 = hsv[:, 0]

    # --- Residualise ---
    residuals, strata = within_group_residuals(M, media, hsv)

    # --- PCA reports ---
    pca_raw = pca_report(M, media, hue_01, label="raw", n_pcs=n_pcs)
    pca_resid = pca_report(residuals, media, hue_01, label="residual", n_pcs=n_pcs)

    delta = float(pca_raw.evr[0] - pca_resid.evr[0])

    report = HueNuisanceReport(
        space_name=space_name,
        n=M.shape[0],
        n_dropped=n_dropped,
        strata=strata,
        pca_raw=pca_raw,
        pca_residual=pca_resid,
        delta_evr_pc1=round(delta, 4),
    )

    # --- Optional outputs ---
    if output_dir is not None:
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        _write_summary_csv(report, output_dir / f"summary_{space_name}.csv")
        plot_pc_scatter(
            M, media,
            title=f"{space_name} — raw",
            out_path=output_dir / f"scatter_raw_{space_name}.png",
        )
        plot_pc_scatter(
            residuals, media,
            title=f"{space_name} — residual",
            out_path=output_dir / f"scatter_residual_{space_name}.png",
        )

    return report


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _safe_pearson(x: np.ndarray, y: np.ndarray) -> float:
    """Pearson r, returning NaN for constant or tiny arrays."""
    if len(x) < 3:
        return float("nan")
    sx, sy = np.std(x), np.std(y)
    if sx == 0 or sy == 0:
        return float("nan")
    return float(np.corrcoef(x, y)[0, 1])


def _write_summary_csv(report: HueNuisanceReport, path: Path) -> None:
    """Write a one-row-per-metric summary CSV."""
    rows = []

    rows.append({"metric": "space", "value": report.space_name})
    rows.append({"metric": "n", "value": report.n})
    rows.append({"metric": "n_dropped_missing_hsv", "value": report.n_dropped})

    # Per-stratum mean R²
    for s in report.strata:
        tag = f"r2_mean_{s.media}"
        rows.append({
            "metric": tag,
            "value": s.mean_r2 if not s.skipped else "skipped",
        })

    # PCA raw
    for i, ev in enumerate(report.pca_raw.evr):
        rows.append({"metric": f"evr_pc{i+1}_raw", "value": round(float(ev), 4)})
    for i, cev in enumerate(report.pca_raw.cum_evr):
        rows.append({"metric": f"cum_evr_pc{i+1}_raw", "value": round(float(cev), 4)})

    # PCA residual
    for i, ev in enumerate(report.pca_residual.evr):
        rows.append({"metric": f"evr_pc{i+1}_residual", "value": round(float(ev), 4)})
    for i, cev in enumerate(report.pca_residual.cum_evr):
        rows.append({"metric": f"cum_evr_pc{i+1}_residual", "value": round(float(cev), 4)})

    rows.append({"metric": "delta_evr_pc1", "value": report.delta_evr_pc1})

    # Hue correlations
    rows.append({
        "metric": "pc1_hue_corr_raw_overall",
        "value": report.pca_raw.pc1_hue_corr_overall,
    })
    for g, r in report.pca_raw.pc1_hue_corr_per_media.items():
        rows.append({"metric": f"pc1_hue_corr_raw_{g}", "value": r})
    rows.append({
        "metric": "pc1_hue_corr_residual_overall",
        "value": report.pca_residual.pc1_hue_corr_overall,
    })
    for g, r in report.pca_residual.pc1_hue_corr_per_media.items():
        rows.append({"metric": f"pc1_hue_corr_residual_{g}", "value": r})

    df = pd.DataFrame(rows)
    df.to_csv(path, index=False)
    logger.info("Wrote summary CSV to %s", path)


def report_to_stdout(report: HueNuisanceReport) -> None:
    """Print a concise human-readable summary to stdout."""
    print(f"\n{'='*60}")
    print(f"Hue nuisance QC — {report.space_name}")
    print(f"{'='*60}")
    print(f"  N = {report.n}  (dropped {report.n_dropped} rows with missing HSV)")
    print()

    print("  Per-stratum mean R² (HSV → latent dims):")
    for s in report.strata:
        status = "SKIPPED" if s.skipped else f"R²={s.mean_r2:.4f}"
        print(f"    {s.media:>12s}  n={s.n_samples:5d}  {status}")
    print()

    print("  PCA explained-variance ratios:")
    print(f"    {'PC':>4s}  {'raw':>8s}  {'residual':>8s}  {'delta':>8s}")
    k = len(report.pca_raw.evr)
    for i in range(k):
        raw_ev = report.pca_raw.evr[i]
        res_ev = report.pca_residual.evr[i]
        d = raw_ev - res_ev
        print(f"    PC{i+1:d}   {raw_ev:7.4f}   {res_ev:7.4f}   {d:+7.4f}")
    print()

    print("  Pearson r(PC1, hue):")
    print(f"    overall  raw={report.pca_raw.pc1_hue_corr_overall:+.4f}"
          f"  residual={report.pca_residual.pc1_hue_corr_overall:+.4f}")
    for g in report.pca_raw.pc1_hue_corr_per_media:
        r_raw = report.pca_raw.pc1_hue_corr_per_media[g]
        r_res = report.pca_residual.pc1_hue_corr_per_media.get(g, float("nan"))
        print(f"    {g:>12s}  raw={r_raw:+.4f}  residual={r_res:+.4f}")
    print()
    print(f"  Delta EVR PC1: {report.delta_evr_pc1:+.4f}")
    print(f"{'='*60}\n")
