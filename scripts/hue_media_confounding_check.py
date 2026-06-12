#!/usr/bin/env python3
"""Hue–media confounding analysis for TLV latent spaces.

For each model (A=s14_run, B=greyscale_s14):
  1. Per-media r(PC1_raw, hue) — from existing summary CSVs.
  2. ANOVA / R² for hue ~ media (metadata only, no GPU).
  3. PC1 ~ C(media) residual correlation with hue (needs encoding).

Usage::

    python scripts/hue_media_confounding_check.py [--device cuda:4] [--skip-encoding]
"""
from __future__ import annotations

import argparse
import json
import logging
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats
from sklearn.decomposition import PCA
from sklearn.linear_model import LinearRegression

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from candescence.core.settings import get_settings

logger = logging.getLogger(__name__)

_ZOO = get_settings().zoo_path

# ── Model configs ─────────────────────────────────────────────────────────
MODELS = {
    "A (s14_run)": {
        "model_dir": _ZOO / "training_20260402/s14_run/models",
        "summary_dir": _ZOO / "training_20260402/s14_run/analyses/hue_nuisance_qc",
        "grayscale": False,
    },
    "B (greyscale)": {
        "model_dir": _ZOO / "training_20260403/greyscale_s14/models",
        "summary_dir": _ZOO / "training_20260403/greyscale_s14/analyses/hue_nuisance_qc",
        "grayscale": True,
    },
}

IMAGE_DIR = get_settings().image_dir
MEDIA_MAP = {"spdr": "spider", "ctrl": "control"}
SPACES = ["primary", "0", "1", "2", "3"]


def _parse_media(filename: str) -> str:
    parts = Path(filename).stem.split("_")
    if len(parts) >= 2:
        raw = parts[1].lower()
        return MEDIA_MAP.get(raw, raw)
    return "unknown"


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
    avg_sat = float(img_hsv_arr[..., 1][mask].mean())
    avg_val = float(img_hsv_arr[..., 2][mask].mean())
    return avg_hue, avg_sat, avg_val


# ── Part 1: Read per-media correlations from existing CSVs ───────────────

def read_per_media_corrs(summary_dir: Path, space: str) -> dict:
    """Read per-media r(PC1_raw, hue) from summary CSV."""
    csv_path = summary_dir / f"summary_{space}.csv"
    df = pd.read_csv(csv_path)
    result = {}
    for _, row in df.iterrows():
        metric = row["metric"]
        if metric.startswith("pc1_hue_corr_raw_") and metric != "pc1_hue_corr_raw_overall":
            media_name = metric.replace("pc1_hue_corr_raw_", "")
            result[media_name] = float(row["value"])
        elif metric == "pc1_hue_corr_raw_overall":
            result["__overall__"] = float(row["value"])
    return result


# ── Part 2: hue ~ media ANOVA (no GPU needed) ────────────────────────────

def hue_media_anova(metadata: pd.DataFrame) -> dict:
    """One-way ANOVA / Kruskal-Wallis for hue ~ media; R² from OLS."""
    hue = metadata["average_hue"].values / 255.0
    media = metadata["media"].values

    # Group hue by media
    groups = {}
    for m in np.unique(media):
        groups[m] = hue[media == m]

    # Kruskal-Wallis (non-parametric, robust)
    group_arrays = [groups[m] for m in sorted(groups)]
    kw_stat, kw_p = stats.kruskal(*group_arrays)

    # R² from OLS: hue_01 ~ C(media) via dummy encoding
    media_dummies = pd.get_dummies(pd.Series(media), drop_first=True).values.astype(float)
    reg = LinearRegression().fit(media_dummies, hue)
    ss_res = np.sum((hue - reg.predict(media_dummies)) ** 2)
    ss_tot = np.sum((hue - hue.mean()) ** 2)
    r2 = 1.0 - ss_res / ss_tot

    # Per-media summary
    per_media = {}
    for m in sorted(groups):
        arr = groups[m]
        per_media[m] = {
            "n": len(arr),
            "mean_hue_01": float(np.mean(arr)),
            "sd_hue_01": float(np.std(arr)),
        }

    return {
        "kw_stat": float(kw_stat),
        "kw_p": float(kw_p),
        "r2_hue_media": float(r2),
        "per_media": per_media,
    }


# ── Part 3: PC1 residual check (needs encoding) ─────────────────────────

def pc1_residual_corr(
    pc1: np.ndarray, hue_01: np.ndarray, media: np.ndarray
) -> float:
    """corr(PC1 residuals after media, hue) — incremental hue info beyond media."""
    media_dummies = pd.get_dummies(pd.Series(media), drop_first=True).values.astype(float)
    reg = LinearRegression().fit(media_dummies, pc1)
    pc1_resid = pc1 - reg.predict(media_dummies)
    # Pearson
    if np.std(pc1_resid) == 0 or np.std(hue_01) == 0:
        return float("nan")
    return float(np.corrcoef(pc1_resid, hue_01)[0, 1])


def encode_model(model_dir: Path, is_grayscale: bool, device, batch_size: int = 256):
    """Load model, encode images, return {space: PC1_scores}, metadata DataFrame."""
    import torch
    from PIL import Image as PILImage
    from candescence.interface.model_loader import load_tlv_model

    with open(model_dir / "args.json") as f:
        model_args = json.load(f)

    filtered_entries = model_args.get("filtered_images", [])
    filtered_filenames = (
        {e["filename"] for e in filtered_entries} if filtered_entries else None
    )
    dim = model_args.get("image_dimension", 128)
    cond_vars = model_args.get("conditional_variables", ["average_hue"])

    model = load_tlv_model(model_dir / "model.pth", device=device)
    all_tendril_keys = model.list_tendril_keys() if model.has_tendrils() else []

    if filtered_filenames:
        image_files = sorted(
            IMAGE_DIR / fn for fn in filtered_filenames if (IMAGE_DIR / fn).exists()
        )
    else:
        image_files = sorted(IMAGE_DIR.glob("*.bmp"))

    n = len(image_files)
    logger.info("Loading %d images ...", n)

    images = np.empty((n, 3, dim, dim), dtype=np.float32)
    ids, media_list = [], []
    hsv_data = np.empty((n, 3), dtype=np.float64)

    for i, p in enumerate(image_files):
        img = PILImage.open(p).convert("RGB").resize((dim, dim))
        img_hsv = img.convert("HSV")
        hsv_data[i] = _compute_hsv_edge(np.array(img_hsv), dim)
        if is_grayscale:
            img = img.convert("L").convert("RGB")
        arr = np.array(img, dtype=np.float32) / 255.0
        images[i] = arr.transpose(2, 0, 1)
        ids.append(p.stem)
        media_list.append(_parse_media(p.name))
        if (i + 1) % 5000 == 0:
            logger.info("  loaded %d / %d", i + 1, n)

    conditioning = np.empty((n, len(cond_vars)), dtype=np.float32)
    for ci, var in enumerate(cond_vars):
        if var == "average_hue":
            conditioning[:, ci] = hsv_data[:, 0] / 255.0
        elif var == "average_saturation":
            conditioning[:, ci] = hsv_data[:, 1] / 255.0
        elif var == "average_value":
            conditioning[:, ci] = hsv_data[:, 2] / 255.0

    # Encode
    mu_lists = {"primary": []}
    for k in all_tendril_keys:
        mu_lists[k] = []

    bs = batch_size
    nb = (n + bs - 1) // bs
    logger.info("Encoding %d batches ...", nb)

    for b in range(nb):
        s, e = b * bs, min((b + 1) * bs, n)
        x = torch.from_numpy(images[s:e]).to(device)
        c = torch.from_numpy(conditioning[s:e]).to(device)
        with torch.no_grad():
            z, mu, logvar = model.encode_full(x, c)
            mu_lists["primary"].append(mu.cpu().numpy())
            if all_tendril_keys:
                skip = model.get_last_skip()
                for j, key in enumerate(all_tendril_keys):
                    _, mu_t, _, _ = model.encode_tendril(key, skip[j], cond=c)
                    mu_lists[key].append(mu_t.cpu().numpy())
        if (b + 1) % 20 == 0:
            logger.info("  batch %d / %d", b + 1, nb)

    # PCA → PC1 per space
    all_mu = {k: np.vstack(v).astype(np.float32) for k, v in mu_lists.items()}
    pc1_scores = {}
    for space, M in all_mu.items():
        pca = PCA(n_components=1)
        pc1_scores[space] = pca.fit_transform(M.astype(np.float64)).ravel()

    metadata = pd.DataFrame({
        "id": ids,
        "media": media_list,
        "average_hue": hsv_data[:, 0],
        "average_saturation": hsv_data[:, 1],
        "average_value": hsv_data[:, 2],
    })

    # Cleanup
    del model, images
    torch.cuda.empty_cache()

    return pc1_scores, metadata


def main():
    parser = argparse.ArgumentParser(
        description="Hue–media confounding check for TLV latent spaces."
    )
    parser.add_argument("--device", default="cuda:4")
    parser.add_argument("--batch-size", type=int, default=256)
    parser.add_argument(
        "--skip-encoding", action="store_true",
        help="Skip PC1 residual check (no GPU needed).",
    )
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )

    # We need metadata from at least one model to do hue~media ANOVA.
    # Both models use the same filtered_images, so metadata is identical.
    # Build it once from s14_run args.json (no GPU needed).
    ref_model_dir = MODELS["A (s14_run)"]["model_dir"]
    with open(ref_model_dir / "args.json") as f:
        model_args = json.load(f)

    filtered_entries = model_args.get("filtered_images", [])
    filtered_filenames = (
        {e["filename"] for e in filtered_entries} if filtered_entries else None
    )
    dim = model_args.get("image_dimension", 128)

    if filtered_filenames:
        image_files = sorted(
            IMAGE_DIR / fn for fn in filtered_filenames if (IMAGE_DIR / fn).exists()
        )
    else:
        image_files = sorted(IMAGE_DIR.glob("*.bmp"))

    n = len(image_files)
    logger.info("Building metadata for %d images (no GPU) ...", n)

    from PIL import Image as PILImage

    ids, media_list = [], []
    hsv_data = np.empty((n, 3), dtype=np.float64)
    for i, p in enumerate(image_files):
        img = PILImage.open(p).convert("RGB").resize((dim, dim))
        img_hsv = img.convert("HSV")
        hsv_data[i] = _compute_hsv_edge(np.array(img_hsv), dim)
        ids.append(p.stem)
        media_list.append(_parse_media(p.name))
        if (i + 1) % 5000 == 0:
            logger.info("  loaded %d / %d", i + 1, n)

    metadata = pd.DataFrame({
        "id": ids,
        "media": media_list,
        "average_hue": hsv_data[:, 0],
        "average_saturation": hsv_data[:, 1],
        "average_value": hsv_data[:, 2],
    })
    logger.info("Metadata ready. N=%d", len(metadata))

    # ── Part 2: hue ~ media ANOVA (same for both models) ─────────────
    anova_result = hue_media_anova(metadata)

    print("\n" + "=" * 72)
    print("Hue ~ Media association (shared dataset, N=%d)" % len(metadata))
    print("=" * 72)
    print(f"  Kruskal-Wallis H = {anova_result['kw_stat']:.1f},  "
          f"p = {anova_result['kw_p']:.2e}")
    print(f"  R²(hue_01 ~ C(media)) = {anova_result['r2_hue_media']:.4f}")
    print()
    print(f"  {'media':>10s}  {'n':>6s}  {'mean_hue':>10s}  {'sd_hue':>10s}")
    print("  " + "-" * 42)
    for m, info in sorted(anova_result["per_media"].items()):
        print(f"  {m:>10s}  {info['n']:6d}  "
              f"{info['mean_hue_01']:10.4f}  {info['sd_hue_01']:10.4f}")
    print("=" * 72 + "\n")

    # ── Parts 1 + 3: per model ────────────────────────────────────────
    hue_01 = metadata["average_hue"].values / 255.0
    media_arr = metadata["media"].values

    # Optionally encode for PC1 residual check
    pc1_by_model = {}
    if not args.skip_encoding:
        import torch
        device = torch.device(
            args.device if torch.cuda.is_available() else "cpu"
        )
        for label, cfg in MODELS.items():
            logger.info("Encoding model %s ...", label)
            pc1_scores, _ = encode_model(
                cfg["model_dir"], cfg["grayscale"], device, args.batch_size
            )
            pc1_by_model[label] = pc1_scores

    for label, cfg in MODELS.items():
        print("\n" + "#" * 72)
        print(f"# Model: {label}")
        print("#" * 72)

        rows = []
        for space in SPACES:
            corrs = read_per_media_corrs(cfg["summary_dir"], space)
            r_global = corrs.pop("__overall__", float("nan"))
            abs_rs = [abs(v) for v in corrs.values()]
            median_abs = float(np.median(abs_rs))
            q1, q3 = float(np.percentile(abs_rs, 25)), float(np.percentile(abs_rs, 75))

            # PC1 residual correlation
            r_pc1_resid = float("nan")
            if label in pc1_by_model and space in pc1_by_model[label]:
                r_pc1_resid = pc1_residual_corr(
                    pc1_by_model[label][space], hue_01, media_arr
                )

            rows.append({
                "space": space,
                "r_global": r_global,
                "median_abs_r_within": median_abs,
                "iqr_abs_r_within": f"[{q1:.3f}, {q3:.3f}]",
                "min_r_w": min(corrs.values()),
                "max_r_w": max(corrs.values()),
                "R2_hue_media": anova_result["r2_hue_media"],
                "p_KW": anova_result["kw_p"],
                "r_pc1_resid_media": r_pc1_resid,
            })

            # Per-media detail
            print(f"\n  Space: {space}  |  r_global = {r_global:+.4f}")
            for m in sorted(corrs):
                print(f"    {m:>10s}: r = {corrs[m]:+.4f}")
            print(f"    median|r| = {median_abs:.4f}  IQR = [{q1:.3f}, {q3:.3f}]"
                  f"  range = [{min(corrs.values()):+.4f}, {max(corrs.values()):+.4f}]")
            if not np.isnan(r_pc1_resid):
                print(f"    r(PC1_resid_media, hue) = {r_pc1_resid:+.4f}")

        # Summary table
        df = pd.DataFrame(rows)
        print(f"\n{'='*90}")
        print(f"Summary table — {label}")
        print(f"{'='*90}")
        header = (f"{'Space':>8s}  {'r_global':>9s}  {'med|r_w|':>9s}  "
                  f"{'IQR|r_w|':>15s}  {'min_r_w':>9s}  {'max_r_w':>9s}  "
                  f"{'R2_h~m':>8s}  {'p_KW':>10s}  {'r_PC1rm':>9s}")
        print(header)
        print("-" * 90)
        for _, r in df.iterrows():
            print(
                f"{r['space']:>8s}  {r['r_global']:+9.4f}  "
                f"{r['median_abs_r_within']:9.4f}  {r['iqr_abs_r_within']:>15s}  "
                f"{r['min_r_w']:+9.4f}  {r['max_r_w']:+9.4f}  "
                f"{r['R2_hue_media']:8.4f}  {r['p_KW']:10.2e}  "
                f"{r['r_pc1_resid_media']:+9.4f}"
            )
        print("=" * 90 + "\n")

    # Save outputs
    for label, cfg in MODELS.items():
        out_dir = cfg["summary_dir"]
        out_path = out_dir / "hue_media_confounding.csv"
        # rebuild rows for saving
        save_rows = []
        for space in SPACES:
            corrs = read_per_media_corrs(cfg["summary_dir"], space)
            r_global = corrs.pop("__overall__", float("nan"))
            abs_rs = [abs(v) for v in corrs.values()]

            r_pc1_resid = float("nan")
            if label in pc1_by_model and space in pc1_by_model[label]:
                r_pc1_resid = pc1_residual_corr(
                    pc1_by_model[label][space], hue_01, media_arr
                )

            save_rows.append({
                "space": space,
                "r_global": round(r_global, 4),
                "median_abs_r_within": round(float(np.median(abs_rs)), 4),
                "min_r_within": round(min(corrs.values()), 4),
                "max_r_within": round(max(corrs.values()), 4),
                "R2_hue_media": round(anova_result["r2_hue_media"], 4),
                "p_KW": anova_result["kw_p"],
                "r_pc1_resid_media": round(r_pc1_resid, 4),
            })
        pd.DataFrame(save_rows).to_csv(out_path, index=False)
        logger.info("Saved %s", out_path)


if __name__ == "__main__":
    main()
