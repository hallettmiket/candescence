#!/usr/bin/env python3
"""Variance partitioning of edge-band hue between medium and plate.

Fits a linear mixed-effects model ``hue ~ C(media) + (1|plate)`` and
reports variance components, ICC, and a likelihood-ratio test for the
medium fixed effect.  Cross-checks against the OLS sequential R^2
decomposition in ``batch_effect_diagnostics.variance_partitioning``.
"""
from __future__ import annotations

import argparse
import json
import logging
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from PIL import Image as PILImage

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from candescence.core.settings import get_settings

_RAW = str(get_settings().raw_path)
_REFINED = str(get_settings().refined_path)

from candescence.tlv.analysis.batch_effect_diagnostics import variance_partitioning  # noqa: E402

logger = logging.getLogger(__name__)

IMAGE_DIR = Path(f"{_RAW}/tlv_images/all-final")
MODEL_DIR = Path(
    f"{_REFINED}/zoo/training_20260402/s14_run/models"
)
DEFAULT_OUTPUT = Path(
    f"{_REFINED}/zoo/training_20260402/s14_run/analyses/hue_medium_vs_plate"
)
MEDIA_MAP = {"spdr": "spider", "ctrl": "control"}


def _parse_plate_media(filename: str) -> tuple[str, str]:
    stem = Path(filename).stem
    parts = stem.split("_")
    plate = parts[0].lstrip("Pp") if parts else "unknown"
    media_raw = parts[1].lower() if len(parts) >= 2 else "unknown"
    return plate, MEDIA_MAP.get(media_raw, media_raw)


def _compute_hsv_edge(img_hsv_arr: np.ndarray, dim: int) -> tuple[float, float, float]:
    mask = np.zeros((dim, dim), dtype=bool)
    mask[:12, :] = True
    mask[-12:, :] = True
    mask[:, :12] = True
    mask[:, -12:] = True
    hue_vals = img_hsv_arr[..., 0][mask].astype(float)
    angles = hue_vals / 255.0 * 2 * np.pi
    mean_angle = np.arctan2(np.mean(np.sin(angles)), np.mean(np.cos(angles)))
    if mean_angle < 0:
        mean_angle += 2 * np.pi
    avg_hue = (mean_angle / (2 * np.pi)) * 255.0
    return avg_hue, float(img_hsv_arr[..., 1][mask].mean()), float(img_hsv_arr[..., 2][mask].mean())


def build_hue_table(image_dir: Path, dim: int, filtered: set[str] | None) -> pd.DataFrame:
    if filtered is not None:
        files = sorted(image_dir / fn for fn in filtered if (image_dir / fn).exists())
    else:
        files = sorted(image_dir.glob("*.bmp"))
    logger.info("Loading %d images from %s", len(files), image_dir)
    rows = []
    for i, p in enumerate(files):
        img_hsv = PILImage.open(p).convert("RGB").resize((dim, dim)).convert("HSV")
        h, s, v = _compute_hsv_edge(np.array(img_hsv), dim)
        plate, media = _parse_plate_media(p.name)
        rows.append((p.name, plate, media, h, s, v))
        if (i + 1) % 5000 == 0:
            logger.info("  %d / %d", i + 1, len(files))
    return pd.DataFrame(rows, columns=["filename", "plate", "media", "average_hue", "average_saturation", "average_value"])


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--image-dir", type=Path, default=IMAGE_DIR)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--hue-table", type=Path, default=None,
                        help="Reuse pre-computed hue table CSV instead of recomputing.")
    parser.add_argument("--dim", type=int, default=128)
    parser.add_argument("--use-filtered", action="store_true",
                        help="Restrict to filtered_images from s14_run args.json.")
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )
    args.output_dir.mkdir(parents=True, exist_ok=True)

    cache = args.output_dir / "hue_table.csv"
    if args.hue_table is not None:
        df = pd.read_csv(args.hue_table)
    elif cache.exists():
        logger.info("Loading cached hue table from %s", cache)
        df = pd.read_csv(cache)
    else:
        filtered = None
        if args.use_filtered:
            with open(MODEL_DIR / "args.json") as f:
                model_args = json.load(f)
            entries = model_args.get("filtered_images", [])
            if entries:
                filtered = {e["filename"] for e in entries}
                logger.info("Using %d filtered images", len(filtered))
        df = build_hue_table(args.image_dir, args.dim, filtered)
        df.to_csv(cache, index=False)
        logger.info("Wrote %s", cache)

    df["plate"] = df["plate"].astype(str)
    df["media"] = df["media"].astype(str)
    # Plate IDs are reused across media; each (plate, media) is a distinct
    # physical plate. Build a unique grouping label.
    df["plate_phys"] = df["plate"] + ":" + df["media"]
    logger.info("Rows=%d  plates=%d  media=%d", len(df), df["plate"].nunique(), df["media"].nunique())
    logger.info("Media counts:\n%s", df["media"].value_counts().to_string())
    logger.info("Plates per medium:\n%s", df.groupby("media")["plate"].nunique().to_string())

    # ---- Mixed-effects model ----
    import statsmodels.formula.api as smf

    full = smf.mixedlm("average_hue ~ C(media)", df, groups=df["plate_phys"]).fit(reml=False)
    null = smf.mixedlm("average_hue ~ 1", df, groups=df["plate_phys"]).fit(reml=False)
    full_reml = smf.mixedlm("average_hue ~ C(media)", df, groups=df["plate_phys"]).fit(reml=True)

    sigma2_plate = float(full_reml.cov_re.iloc[0, 0])
    sigma2_resid = float(full_reml.scale)
    icc = sigma2_plate / (sigma2_plate + sigma2_resid)

    # Marginal R^2 (fixed effects only) and conditional R^2 (fixed + random),
    # following Nakagawa & Schielzeth (2013).
    fe = full_reml.fe_params
    X = np.asarray(full_reml.model.exog)
    fixed_pred = X @ fe.values
    var_f = float(np.var(fixed_pred, ddof=0))
    r2_marginal = var_f / (var_f + sigma2_plate + sigma2_resid)
    r2_conditional = (var_f + sigma2_plate) / (var_f + sigma2_plate + sigma2_resid)

    # LR test
    lr_stat = 2 * (full.llf - null.llf)
    df_diff = int(full.df_modelwc - null.df_modelwc)
    from scipy.stats import chi2
    p_lrt = float(chi2.sf(lr_stat, df_diff)) if df_diff > 0 else float("nan")

    # ---- Sanity check: OLS sequential R^2 ----
    vp = variance_partitioning(
        X=df[["average_hue"]].to_numpy(dtype=float),
        media=df["media"].to_numpy(),
        plate=df["plate_phys"].to_numpy(),
        n_pcs=1,
    )

    # ---- Write outputs ----
    out = args.output_dir
    (out / "mixedlm_summary.txt").write_text(str(full_reml.summary()))

    pd.DataFrame({
        "component": ["sigma2_plate", "sigma2_resid", "ICC", "R2_marginal", "R2_conditional"],
        "value": [sigma2_plate, sigma2_resid, icc, r2_marginal, r2_conditional],
    }).to_csv(out / "variance_components.csv", index=False)

    fe_df = pd.DataFrame({
        "term": full_reml.fe_params.index,
        "estimate": full_reml.fe_params.values,
        "std_err": full_reml.bse_fe.values,
        "z": full_reml.tvalues[: len(full_reml.fe_params)].values,
        "p_value": full_reml.pvalues[: len(full_reml.fe_params)].values,
    })
    fe_df.to_csv(out / "medium_effects.csv", index=False)

    (out / "lrt_medium.txt").write_text(
        f"LR statistic = {lr_stat:.4f}\n"
        f"df = {df_diff}\n"
        f"p-value = {p_lrt:.4g}\n"
        f"full ll (ML) = {full.llf:.4f}\n"
        f"null ll (ML) = {null.llf:.4f}\n"
    )

    pd.DataFrame([{
        "r2_media": vp.r2_media,
        "r2_plate": vp.r2_plate,
        "r2_media_plate": vp.r2_media_plate,
        "unique_media": vp.unique_media,
        "unique_plate": vp.unique_plate,
        "shared_media_plate": vp.r2_media + vp.r2_plate - vp.r2_media_plate,
    }]).to_csv(out / "sanity_sequential_r2.csv", index=False)

    # Plot
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        import seaborn as sns
        fig, ax = plt.subplots(figsize=(10, 5))
        sns.stripplot(data=df, x="media", y="average_hue", hue="plate",
                      ax=ax, size=2, alpha=0.5, legend=False)
        ax.set_title("Edge-band hue by medium and plate")
        fig.tight_layout()
        fig.savefig(out / "hue_by_medium_plate.png", dpi=120)
        plt.close(fig)
    except Exception as e:
        logger.warning("Plot failed: %s", e)

    verdict = (
        f"VERDICT: plate ICC = {icc:.3f}  "
        f"(plate σ²={sigma2_plate:.3f}, resid σ²={sigma2_resid:.3f}); "
        f"medium fixed-effect marginal R² = {r2_marginal:.3f}; "
        f"plate+medium conditional R² = {r2_conditional:.3f}; "
        f"LRT for medium χ²({df_diff}) = {lr_stat:.2f}, p = {p_lrt:.3g}"
    )
    print("\n" + verdict)
    (out / "verdict.txt").write_text(verdict + "\n")


if __name__ == "__main__":
    main()
