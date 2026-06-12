#!/usr/bin/env python3
"""Full hue–media confounding diagnostic for TLV latent spaces.

For each model (A=s14_run, B=greyscale_s14), per space:
  1. Global and per-media r(PC1, hue) and r(PC2, hue).
  2. Partial correlation: r(PC1, hue | media) and r(PC2, hue | media).
  3. Nested regression: R²(PC1 ~ media), R²(PC1 ~ media + hue), ΔR², F-test.
  4. Per-media audit CSV with n, r_pc1_hue, r_pc2_hue per stratum.

Usage::

    python scripts/hue_media_confounding_full.py [--device cuda:4]
    python scripts/hue_media_confounding_full.py --skip-encoding  # metadata-only (no PC residuals)

Author: Candescence team
Date:   2026-04-04
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

_RAW = str(get_settings().raw_path)
_REFINED = str(get_settings().refined_path)

logger = logging.getLogger(__name__)

# ── Model configs ─────────────────────────────────────────────────────────
MODELS = {
    "A (s14_run)": {
        "model_dir": Path(
            f"{_REFINED}/zoo/training_20260402/s14_run/models"
        ),
        "summary_dir": Path(
            f"{_REFINED}/zoo/training_20260402/"
            "s14_run/analyses/hue_nuisance_qc"
        ),
        "output_dir": Path(
            f"{_REFINED}/zoo/training_20260402/"
            "s14_run/analyses/hue_media_confounding"
        ),
        "grayscale": False,
    },
    "B (greyscale)": {
        "model_dir": Path(
            f"{_REFINED}/zoo/training_20260403/greyscale_s14/models"
        ),
        "summary_dir": Path(
            f"{_REFINED}/zoo/training_20260403/"
            "greyscale_s14/analyses/hue_nuisance_qc"
        ),
        "output_dir": Path(
            f"{_REFINED}/zoo/training_20260403/"
            "greyscale_s14/analyses/hue_media_confounding"
        ),
        "grayscale": True,
    },
}

IMAGE_DIR = Path(f"{_RAW}/tlv_images/all-final")
MEDIA_MAP = {"spdr": "spider", "ctrl": "control"}
SPACES = ["primary", "0", "1", "2", "3"]
MIN_STRATUM_N = 10


# ── Shared helpers ────────────────────────────────────────────────────────

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


def _safe_pearson(x: np.ndarray, y: np.ndarray) -> float:
    """Pearson r, NaN for constant or tiny arrays."""
    if len(x) < 3:
        return float("nan")
    if np.std(x) == 0 or np.std(y) == 0:
        return float("nan")
    return float(np.corrcoef(x, y)[0, 1])


def _media_dummies(media: np.ndarray) -> np.ndarray:
    """One-hot encode media, drop first level (reference)."""
    return pd.get_dummies(pd.Series(media), drop_first=True).values.astype(np.float64)


# ── Encoding ──────────────────────────────────────────────────────────────

def encode_model(
    model_dir: Path, is_grayscale: bool, device, batch_size: int = 256
) -> tuple[dict[str, np.ndarray], dict[str, np.ndarray], pd.DataFrame]:
    """Encode all images; return {space: PC1}, {space: PC2}, metadata."""
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

    mu_lists: dict[str, list] = {"primary": []}
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

    all_mu = {k: np.vstack(v).astype(np.float64) for k, v in mu_lists.items()}

    # PCA → PC1 + PC2 per space
    pc1_scores, pc2_scores = {}, {}
    for space, M in all_mu.items():
        n_comp = min(2, M.shape[0], M.shape[1])
        pca = PCA(n_components=n_comp)
        scores = pca.fit_transform(M)
        pc1_scores[space] = scores[:, 0]
        pc2_scores[space] = scores[:, 1] if n_comp >= 2 else np.zeros(M.shape[0])

    metadata = pd.DataFrame({
        "id": ids,
        "media": media_list,
        "average_hue": hsv_data[:, 0],
        "average_saturation": hsv_data[:, 1],
        "average_value": hsv_data[:, 2],
    })

    del model, images
    torch.cuda.empty_cache()

    return pc1_scores, pc2_scores, metadata


# ── Analysis functions ────────────────────────────────────────────────────

def partial_corr(y: np.ndarray, x: np.ndarray, media: np.ndarray) -> float:
    """Partial correlation r(y, x | media) via OLS residuals."""
    D = _media_dummies(media)
    reg_y = LinearRegression().fit(D, y)
    reg_x = LinearRegression().fit(D, x)
    return _safe_pearson(y - reg_y.predict(D), x - reg_x.predict(D))


def nested_regression(
    pc: np.ndarray, hue_01: np.ndarray, media: np.ndarray
) -> dict:
    """Model A: PC ~ media. Model B: PC ~ media + hue. Return R²s, ΔR², F, p."""
    D = _media_dummies(media)
    n = len(pc)
    p_a = D.shape[1]  # predictors in model A (dummies only)

    # Model A: PC ~ media
    reg_a = LinearRegression().fit(D, pc)
    ss_res_a = np.sum((pc - reg_a.predict(D)) ** 2)
    ss_tot = np.sum((pc - pc.mean()) ** 2)
    r2_a = 1.0 - ss_res_a / ss_tot

    # Model B: PC ~ media + hue
    D_b = np.column_stack([D, hue_01])
    p_b = D_b.shape[1]
    reg_b = LinearRegression().fit(D_b, pc)
    ss_res_b = np.sum((pc - reg_b.predict(D_b)) ** 2)
    r2_b = 1.0 - ss_res_b / ss_tot

    delta_r2 = r2_b - r2_a

    # F-test for nested models (1 extra predictor: hue)
    df_extra = p_b - p_a  # = 1
    df_resid_b = n - p_b - 1  # -1 for intercept
    if df_resid_b > 0 and ss_res_b > 0:
        f_stat = ((ss_res_a - ss_res_b) / df_extra) / (ss_res_b / df_resid_b)
        p_val = 1.0 - stats.f.cdf(f_stat, df_extra, df_resid_b)
    else:
        f_stat = float("nan")
        p_val = float("nan")

    return {
        "r2_media": round(float(r2_a), 4),
        "r2_media_hue": round(float(r2_b), 4),
        "delta_r2": round(float(delta_r2), 4),
        "f_stat": round(float(f_stat), 2),
        "p_f": float(p_val),
    }


def per_media_corrs(
    pc1: np.ndarray,
    pc2: np.ndarray,
    hue_01: np.ndarray,
    media: np.ndarray,
    min_n: int = MIN_STRATUM_N,
) -> pd.DataFrame:
    """Per-media r(PC1, hue) and r(PC2, hue)."""
    rows = []
    for g in sorted(np.unique(media)):
        mask = media == g
        n_g = int(mask.sum())
        if n_g < min_n:
            logger.warning("Stratum '%s' n=%d < %d, skipping.", g, n_g, min_n)
            continue
        rows.append({
            "media": g,
            "n": n_g,
            "r_pc1_hue": round(_safe_pearson(pc1[mask], hue_01[mask]), 4),
            "r_pc2_hue": round(_safe_pearson(pc2[mask], hue_01[mask]), 4),
        })
    return pd.DataFrame(rows)


def hue_media_anova(metadata: pd.DataFrame) -> dict:
    """Kruskal-Wallis + R²(hue ~ media) + per-media stats."""
    hue = metadata["average_hue"].values / 255.0
    media = metadata["media"].values

    groups = {}
    for m in np.unique(media):
        groups[m] = hue[media == m]

    kw_stat, kw_p = stats.kruskal(*[groups[m] for m in sorted(groups)])

    D = _media_dummies(media)
    reg = LinearRegression().fit(D, hue)
    ss_res = np.sum((hue - reg.predict(D)) ** 2)
    ss_tot = np.sum((hue - hue.mean()) ** 2)
    r2 = 1.0 - ss_res / ss_tot

    per_media = {}
    for m in sorted(groups):
        arr = groups[m]
        per_media[m] = {"n": len(arr), "mean": float(np.mean(arr)), "sd": float(np.std(arr))}

    return {"kw_stat": float(kw_stat), "kw_p": float(kw_p), "r2": float(r2), "per_media": per_media}


# ── Main ──────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description="Full hue–media confounding diagnostic."
    )
    parser.add_argument("--device", default="cuda:4")
    parser.add_argument("--batch-size", type=int, default=256)
    parser.add_argument("--skip-encoding", action="store_true")
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )

    # ── Build metadata (no GPU) ───────────────────────────────────────
    ref_dir = MODELS["A (s14_run)"]["model_dir"]
    with open(ref_dir / "args.json") as f:
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
    logger.info("Building metadata for %d images ...", n)

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
    hue_01 = metadata["average_hue"].values / 255.0
    media_arr = metadata["media"].values
    logger.info("Metadata ready. N=%d", n)

    # ── Hue ~ media (shared) ─────────────────────────────────────────
    anova = hue_media_anova(metadata)
    print("\n" + "=" * 72)
    print(f"Hue ~ Media (N={n})")
    print("=" * 72)
    print(f"  Kruskal-Wallis H={anova['kw_stat']:.1f}  p={anova['kw_p']:.2e}")
    print(f"  R²(hue ~ media) = {anova['r2']:.4f}")
    print(f"\n  {'media':>10s}  {'n':>6s}  {'mean':>8s}  {'sd':>8s}")
    print("  " + "-" * 36)
    for m, info in sorted(anova["per_media"].items()):
        print(f"  {m:>10s}  {info['n']:6d}  {info['mean']:8.4f}  {info['sd']:8.4f}")
    print("=" * 72)

    # ── Encode models ─────────────────────────────────────────────────
    model_data: dict[str, dict] = {}  # label -> {space: (pc1, pc2)}

    if not args.skip_encoding:
        import torch
        device = torch.device(args.device if torch.cuda.is_available() else "cpu")
        for label, cfg in MODELS.items():
            logger.info("Encoding %s ...", label)
            pc1s, pc2s, _ = encode_model(
                cfg["model_dir"], cfg["grayscale"], device, args.batch_size
            )
            model_data[label] = {sp: (pc1s[sp], pc2s[sp]) for sp in pc1s}

    # ── Per-model analysis ────────────────────────────────────────────
    for label, cfg in MODELS.items():
        out_dir = cfg["output_dir"]
        out_dir.mkdir(parents=True, exist_ok=True)

        print(f"\n{'#'*72}")
        print(f"# {label}")
        print(f"{'#'*72}")

        summary_rows = []
        has_scores = label in model_data

        for space in SPACES:
            if has_scores and space in model_data[label]:
                pc1, pc2 = model_data[label][space]
            else:
                pc1, pc2 = None, None

            # ── Per-media correlations ────────────────────────────────
            if pc1 is not None:
                pm_df = per_media_corrs(pc1, pc2, hue_01, media_arr)
                pm_df.insert(0, "space", space)
                pm_df.to_csv(out_dir / f"per_media_{space}.csv", index=False)

                r_glob_pc1 = _safe_pearson(pc1, hue_01)
                r_glob_pc2 = _safe_pearson(pc2, hue_01)

                abs_r1 = pm_df["r_pc1_hue"].abs()
                abs_r2 = pm_df["r_pc2_hue"].abs()
                med_r1 = float(abs_r1.median())
                med_r2 = float(abs_r2.median())

                # ── Partial correlations ──────────────────────────────
                part_r1 = partial_corr(pc1, hue_01, media_arr)
                part_r2 = partial_corr(pc2, hue_01, media_arr)

                # ── Nested regression ─────────────────────────────────
                nest = nested_regression(pc1, hue_01, media_arr)

                # ── Detail print ──────────────────────────────────────
                print(f"\n  Space: {space}")
                print(f"    PC1: r_global={r_glob_pc1:+.4f}  "
                      f"median|r_within|={med_r1:.4f}  "
                      f"partial_r={part_r1:+.4f}")
                print(f"    PC2: r_global={r_glob_pc2:+.4f}  "
                      f"median|r_within|={med_r2:.4f}  "
                      f"partial_r={part_r2:+.4f}")
                print(f"    Nested: R²(media)={nest['r2_media']:.4f}  "
                      f"R²(media+hue)={nest['r2_media_hue']:.4f}  "
                      f"ΔR²={nest['delta_r2']:.4f}  "
                      f"F={nest['f_stat']:.1f}  p={nest['p_f']:.2e}")
                for _, row in pm_df.iterrows():
                    print(f"      {row['media']:>10s} n={row['n']:5d}  "
                          f"r_pc1={row['r_pc1_hue']:+.4f}  "
                          f"r_pc2={row['r_pc2_hue']:+.4f}")

                summary_rows.append({
                    "space": space,
                    "r_glob_PC1": round(r_glob_pc1, 4),
                    "med|r_w|_PC1": round(med_r1, 4),
                    "partial_r_PC1": round(part_r1, 4),
                    "r_glob_PC2": round(r_glob_pc2, 4),
                    "med|r_w|_PC2": round(med_r2, 4),
                    "partial_r_PC2": round(part_r2, 4),
                    "R2_media": nest["r2_media"],
                    "delta_R2_hue": nest["delta_r2"],
                    "p_F": nest["p_f"],
                })
            else:
                summary_rows.append({
                    "space": space,
                    "r_glob_PC1": float("nan"),
                    "med|r_w|_PC1": float("nan"),
                    "partial_r_PC1": float("nan"),
                    "r_glob_PC2": float("nan"),
                    "med|r_w|_PC2": float("nan"),
                    "partial_r_PC2": float("nan"),
                    "R2_media": float("nan"),
                    "delta_R2_hue": float("nan"),
                    "p_F": float("nan"),
                })

        # ── Summary table ─────────────────────────────────────────────
        sdf = pd.DataFrame(summary_rows)
        sdf.to_csv(out_dir / "confounding_summary.csv", index=False)

        print(f"\n{'='*120}")
        print(f"Summary — {label}")
        print(f"{'='*120}")
        hdr = (f"{'Space':>8s}  {'r_g PC1':>8s}  {'m|rw|PC1':>8s}  "
               f"{'part_r1':>8s}  {'r_g PC2':>8s}  {'m|rw|PC2':>8s}  "
               f"{'part_r2':>8s}  {'R²(med)':>8s}  {'ΔR²+hue':>8s}  "
               f"{'p_F':>10s}")
        print(hdr)
        print("-" * 120)
        for _, r in sdf.iterrows():
            p_str = f"{r['p_F']:.2e}" if not np.isnan(r["p_F"]) else "n/a"
            print(
                f"{r['space']:>8s}  {r['r_glob_PC1']:+8.4f}  "
                f"{r['med|r_w|_PC1']:8.4f}  {r['partial_r_PC1']:+8.4f}  "
                f"{r['r_glob_PC2']:+8.4f}  {r['med|r_w|_PC2']:8.4f}  "
                f"{r['partial_r_PC2']:+8.4f}  {r['R2_media']:8.4f}  "
                f"{r['delta_R2_hue']:8.4f}  {p_str:>10s}"
            )
        print("=" * 120 + "\n")

        logger.info("Outputs saved to %s", out_dir)


if __name__ == "__main__":
    main()
