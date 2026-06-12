#!/usr/bin/env python3
"""Run hue nuisance QC + biological label eval on greyscale_s14 model.

Combines both analyses in one pass (single image load + encode).
"""
from __future__ import annotations

import argparse
import json
import logging
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from PIL import Image as PILImage
from sklearn.metrics import balanced_accuracy_score, confusion_matrix, silhouette_score
from sklearn.model_selection import StratifiedKFold
from sklearn.neighbors import KNeighborsClassifier
from sklearn.preprocessing import LabelEncoder

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from candescence.core.settings import get_settings, legacy_refined_root

_RAW = str(get_settings().raw_path)
_REFINED = str(get_settings().refined_path)
_LEGACY_REFINED = str(legacy_refined_root())

from candescence.interface.model_loader import load_tlv_model
from candescence.tlv.analysis.hue_nuisance_qc import (
    build_design_matrix,
    report_to_stdout,
    run_hue_nuisance_qc,
    within_group_residuals,
)

logger = logging.getLogger(__name__)

MODEL_DIR = Path(
    f"{_REFINED}/zoo/training_20260403/greyscale_s14/models"
)
IMAGE_DIR = Path(f"{_RAW}/tlv_images/all-final")
MANUAL_LABELS_CSV = Path(
    f"{_LEGACY_REFINED}/candescence_master/projects/tlv/data_files/"
    "manually_labelled_images.csv"
)
OUTPUT_HUE_QC = Path(
    f"{_REFINED}/zoo/training_20260403/"
    "greyscale_s14/analyses/hue_nuisance_qc"
)
OUTPUT_BIO_EVAL = Path(
    f"{_REFINED}/zoo/training_20260403/"
    "greyscale_s14/analyses/hue_nuisance_qc/tendril_only_bio_eval"
)

MEDIA_MAP = {"spdr": "spider", "ctrl": "control"}
MIN_CLASS_N = 5


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


def _merge_manual_labels(metadata: pd.DataFrame) -> pd.DataFrame:
    labels_df = pd.read_csv(MANUAL_LABELS_CSV)
    labels_df = labels_df.rename(columns={"file_name": "filename"})
    metadata = metadata.copy()
    metadata["filename"] = metadata["id"] + ".bmp"
    merged = metadata.merge(labels_df[["filename", "morphology"]], on="filename", how="left")
    merged = merged.rename(columns={"morphology": "manual_formation"})
    merged["manual_formation"] = merged["manual_formation"].fillna("unlabelled")
    return merged


def _filter_labelled(M, labels, min_n=MIN_CLASS_N):
    mask = labels != "unlabelled"
    M, labels = M[mask], labels[mask]
    classes, counts = np.unique(labels, return_counts=True)
    keep = set(classes[counts >= min_n])
    if len(keep) < len(classes):
        logger.info("Dropping rare classes: %s", set(classes) - keep)
        m2 = np.isin(labels, list(keep))
        M, labels = M[m2], labels[m2]
    return M, labels


def _silhouette(M, labels):
    if len(np.unique(labels)) < 2 or len(M) < 3:
        return float("nan")
    return float(silhouette_score(M, labels))


def _knn_cv(M, labels, k=5, n_folds=5):
    le = LabelEncoder()
    y = le.fit_transform(labels)
    skf = StratifiedKFold(n_splits=n_folds, shuffle=True, random_state=42)
    accs, baccs = [], []
    for tr, te in skf.split(M, y):
        clf = KNeighborsClassifier(n_neighbors=k, metric="euclidean", weights="distance")
        clf.fit(M[tr], y[tr])
        yp = clf.predict(M[te])
        accs.append(float(np.mean(yp == y[te])))
        baccs.append(float(balanced_accuracy_score(y[te], yp)))
    return float(np.mean(accs)), float(np.mean(baccs))


def _confusion_df(M, labels, k=5):
    le = LabelEncoder()
    y = le.fit_transform(labels)
    clf = KNeighborsClassifier(n_neighbors=k, metric="euclidean", weights="distance")
    clf.fit(M, y)
    yp = clf.predict(M)
    cm = confusion_matrix(y, yp)
    return pd.DataFrame(cm, index=le.classes_, columns=le.classes_)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--device", default="cuda:4")
    parser.add_argument("--batch-size", type=int, default=256)
    parser.add_argument("--n-pcs", type=int, default=8)
    parser.add_argument("--model-dir", type=Path, default=MODEL_DIR)
    parser.add_argument("--output-hue-qc", type=Path, default=OUTPUT_HUE_QC)
    parser.add_argument("--output-bio-eval", type=Path, default=OUTPUT_BIO_EVAL)
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )
    device = torch.device(args.device if torch.cuda.is_available() else "cpu")

    # ── Config ────────────────────────────────────────────────────────
    model_dir = args.model_dir
    output_hue_qc = args.output_hue_qc
    output_bio_eval = args.output_bio_eval

    with open(model_dir / "args.json") as f:
        model_args = json.load(f)

    filtered_entries = model_args.get("filtered_images", [])
    filtered_filenames = {e["filename"] for e in filtered_entries} if filtered_entries else None
    dim = model_args.get("image_dimension", 128)
    cond_vars = model_args.get("conditional_variables", ["average_value"])
    is_grayscale = model_args.get("grayscale", False)
    logger.info("conditional_variables=%s  grayscale=%s", cond_vars, is_grayscale)

    # ── Load model ────────────────────────────────────────────────────
    logger.info("Loading model ...")
    model = load_tlv_model(model_dir / "model.pth", device=device)
    all_tendril_keys = model.list_tendril_keys() if model.has_tendrils() else []
    logger.info("Tendril keys: %s", all_tendril_keys)

    # ── Load images ───────────────────────────────────────────────────
    if filtered_filenames:
        image_files = sorted(IMAGE_DIR / fn for fn in filtered_filenames if (IMAGE_DIR / fn).exists())
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
            # Match training: L -> RGB (3 identical channels)
            img = img.convert("L").convert("RGB")
        arr = np.array(img, dtype=np.float32) / 255.0
        images[i] = arr.transpose(2, 0, 1)

        ids.append(p.stem)
        media_list.append(_parse_media(p.name))
        if (i + 1) % 5000 == 0:
            logger.info("  loaded %d / %d", i + 1, n)

    logger.info("All images loaded. Tensor shape: %s", images.shape)

    # ── Conditioning ──────────────────────────────────────────────────
    conditioning = np.empty((n, len(cond_vars)), dtype=np.float32)
    for ci, var in enumerate(cond_vars):
        if var == "average_hue":
            conditioning[:, ci] = hsv_data[:, 0] / 255.0
        elif var == "average_saturation":
            conditioning[:, ci] = hsv_data[:, 1] / 255.0
        elif var == "average_value":
            conditioning[:, ci] = hsv_data[:, 2] / 255.0

    # ── Encode (primary + all tendrils) ───────────────────────────────
    mu_lists = {"primary": []}
    for k in all_tendril_keys:
        mu_lists[k] = []

    bs = args.batch_size
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

    all_mu = {k: np.vstack(v).astype(np.float32) for k, v in mu_lists.items()}
    logger.info("Shapes: %s", {k: v.shape for k, v in all_mu.items()})

    # ── Metadata ──────────────────────────────────────────────────────
    metadata = pd.DataFrame({
        "id": ids,
        "media": media_list,
        "average_hue": hsv_data[:, 0],
        "average_saturation": hsv_data[:, 1],
        "average_value": hsv_data[:, 2],
    })

    # ═══════════════════════════════════════════════════════════════════
    # PART 1: Hue nuisance QC (all spaces)
    # ═══════════════════════════════════════════════════════════════════
    print("\n" + "#" * 72)
    print("# PART 1: Hue Nuisance QC")
    print("#" * 72)
    for space, M in sorted(all_mu.items()):
        report = run_hue_nuisance_qc(
            M=M, metadata=metadata, space_name=space,
            n_pcs=args.n_pcs, output_dir=output_hue_qc,
        )
        report_to_stdout(report)

    # ═══════════════════════════════════════════════════════════════════
    # PART 2: Bio label eval (tendrils only)
    # ═══════════════════════════════════════════════════════════════════
    print("\n" + "#" * 72)
    print("# PART 2: Biological Label Evaluation (tendrils only)")
    print("#" * 72)

    metadata_labelled = _merge_manual_labels(metadata)
    labels_all = metadata_labelled["manual_formation"].values

    media_arr = metadata["media"].values
    hsv_matrix = build_design_matrix(
        metadata["average_hue"].values,
        metadata["average_saturation"].values,
        metadata["average_value"].values,
    )

    # Only tendrils for bio eval
    tendril_mu = {k: v for k, v in all_mu.items() if k != "primary"}
    residuals = {}
    for space, M in tendril_mu.items():
        resid, _ = within_group_residuals(M, media_arr, hsv_matrix)
        residuals[space] = resid

    output_bio_eval.mkdir(parents=True, exist_ok=True)
    rows = []
    for space in sorted(tendril_mu.keys()):
        for variant, M in [("raw", tendril_mu[space]), ("residual", residuals[space])]:
            Mf, lf = _filter_labelled(M, labels_all)
            nl, nc = len(lf), len(np.unique(lf))
            if nl < 10 or nc < 2:
                continue
            sil = _silhouette(Mf, lf)
            acc, bacc = _knn_cv(Mf, lf)
            rows.append({
                "space": space, "variant": variant,
                "n_labelled": nl, "n_classes": nc,
                "silhouette": round(sil, 4),
                "knn_accuracy": round(acc, 4),
                "knn_balanced_accuracy": round(bacc, 4),
            })
            cm = _confusion_df(Mf, lf)
            cm.to_csv(output_bio_eval / f"confusion_{space}_{variant}.csv")
            logger.info("%s/%s: sil=%.4f acc=%.4f bacc=%.4f", space, variant, sil, acc, bacc)

    rdf = pd.DataFrame(rows)
    rdf.to_csv(output_bio_eval / "bio_label_eval_manual_formation.csv", index=False)

    print("\n" + "=" * 72)
    print("Biological label evaluation — manual_formation (greyscale_s14)")
    print("=" * 72)
    print(f"{'space':>10s}  {'variant':>10s}  {'n':>6s}  {'cls':>4s}  "
          f"{'silhouette':>10s}  {'kNN acc':>8s}  {'kNN bacc':>9s}")
    print("-" * 72)
    for _, r in rdf.iterrows():
        print(f"{r['space']:>10s}  {r['variant']:>10s}  "
              f"{r['n_labelled']:6d}  {r['n_classes']:4d}  "
              f"{r['silhouette']:10.4f}  {r['knn_accuracy']:8.4f}  "
              f"{r['knn_balanced_accuracy']:9.4f}")
    print("=" * 72 + "\n")

    logger.info("Done. Hue QC: %s  Bio eval: %s", output_hue_qc, output_bio_eval)


if __name__ == "__main__":
    main()
