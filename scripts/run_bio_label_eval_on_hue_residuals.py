#!/usr/bin/env python3
"""Evaluate biological separability on hue-corrected residual latents.

Loads model, encodes images, residualizes hue within media strata,
merges manual morphology labels, and computes silhouette + kNN metrics.

Usage::

    python scripts/run_bio_label_eval_on_hue_residuals.py [--device cuda:4]

Author: Candescence team
Date:   2026-04-03
"""

from __future__ import annotations

import argparse
import json
import logging
import sys
from pathlib import Path
from typing import Optional

import numpy as np
import pandas as pd
import torch
from PIL import Image as PILImage
from sklearn.metrics import (
    balanced_accuracy_score,
    confusion_matrix,
    silhouette_score,
)
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
    within_group_residuals,
)

logger = logging.getLogger(__name__)

# ── Paths ──────────────────────────────────────────────────────────────────
MODEL_DIR = Path(
    f"{_REFINED}/zoo/training_20260402/s14_run/models"
)
IMAGE_DIR = Path(f"{_RAW}/tlv_images/all-final")
MANUAL_LABELS_CSV = Path(
    f"{_LEGACY_REFINED}/candescence_master/projects/tlv/data_files/"
    "manually_labelled_images.csv"
)
DEFAULT_OUTPUT = Path(
    f"{_REFINED}/zoo/training_20260402/"
    "s14_run/analyses/hue_nuisance_qc/tendril_only_bio_eval"
)

MEDIA_MAP = {"spdr": "spider", "ctrl": "control"}

# Only encode these tendril keys (saves time vs all 4).
TARGET_TENDRIL_KEYS = {"0", "1", "2", "3"}

# Minimum samples per class to include in evaluation.
MIN_CLASS_N = 5


# ── Helpers ────────────────────────────────────────────────────────────────

def _parse_media(filename: str) -> str:
    parts = Path(filename).stem.split("_")
    if len(parts) >= 2:
        raw = parts[1].lower()
        return MEDIA_MAP.get(raw, raw)
    return "unknown"


def _compute_hsv_edge(
    img_hsv_arr: np.ndarray, dim: int
) -> tuple[float, float, float]:
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
    """Merge manual morphology labels from the canonical CSV."""
    labels_df = pd.read_csv(MANUAL_LABELS_CSV)
    labels_df = labels_df.rename(columns={"file_name": "filename"})

    # Build filename column from id (stem) + .bmp
    metadata = metadata.copy()
    metadata["filename"] = metadata["id"] + ".bmp"

    merged = metadata.merge(
        labels_df[["filename", "morphology"]],
        on="filename",
        how="left",
    )
    merged = merged.rename(columns={"morphology": "manual_formation"})
    merged["manual_formation"] = merged["manual_formation"].fillna("unlabelled")
    return merged


def _filter_labelled(
    M: np.ndarray, labels: np.ndarray, min_n: int = MIN_CLASS_N
) -> tuple[np.ndarray, np.ndarray]:
    """Drop unlabelled rows and classes with fewer than min_n samples."""
    mask = labels != "unlabelled"
    M, labels = M[mask], labels[mask]

    # Drop rare classes
    classes, counts = np.unique(labels, return_counts=True)
    keep_classes = set(classes[counts >= min_n])
    if len(keep_classes) < len(classes):
        dropped = set(classes) - keep_classes
        logger.info("Dropping rare classes (n < %d): %s", min_n, dropped)
        mask2 = np.isin(labels, list(keep_classes))
        M, labels = M[mask2], labels[mask2]

    return M, labels


def _compute_silhouette(M: np.ndarray, labels: np.ndarray) -> float:
    """Silhouette score, or NaN if not computable."""
    unique = np.unique(labels)
    if len(unique) < 2 or len(M) < 3:
        return float("nan")
    return float(silhouette_score(M, labels))


def _compute_knn_cv(
    M: np.ndarray,
    labels: np.ndarray,
    k: int = 5,
    n_folds: int = 5,
) -> tuple[float, float]:
    """Stratified k-fold kNN. Returns (accuracy, balanced_accuracy)."""
    le = LabelEncoder()
    y = le.fit_transform(labels)

    skf = StratifiedKFold(n_splits=n_folds, shuffle=True, random_state=42)
    accs, baccs = [], []

    for train_idx, test_idx in skf.split(M, y):
        clf = KNeighborsClassifier(n_neighbors=k, metric="euclidean", weights="distance")
        clf.fit(M[train_idx], y[train_idx])
        y_pred = clf.predict(M[test_idx])
        accs.append(float(np.mean(y_pred == y[test_idx])))
        baccs.append(float(balanced_accuracy_score(y[test_idx], y_pred)))

    return float(np.mean(accs)), float(np.mean(baccs))


def _confusion_df(
    M: np.ndarray, labels: np.ndarray, k: int = 5
) -> pd.DataFrame:
    """Full-data kNN confusion matrix as a DataFrame."""
    le = LabelEncoder()
    y = le.fit_transform(labels)
    clf = KNeighborsClassifier(n_neighbors=k, metric="euclidean", weights="distance")
    clf.fit(M, y)
    y_pred = clf.predict(M)
    cm = confusion_matrix(y, y_pred)
    classes = le.classes_
    return pd.DataFrame(cm, index=classes, columns=classes)


# ── Main ───────────────────────────────────────────────────────────────────

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Biological label evaluation on hue-corrected residuals."
    )
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--device", type=str, default="cuda:4")
    parser.add_argument("--batch-size", type=int, default=256)
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )

    device = torch.device(args.device if torch.cuda.is_available() else "cpu")

    # ── 1. Determine image set ─────────────────────────────────────────
    with open(MODEL_DIR / "args.json") as f:
        model_args = json.load(f)

    filtered_entries = model_args.get("filtered_images", [])
    if filtered_entries:
        filtered_filenames = {e["filename"] for e in filtered_entries}
        logger.info("Using %d filtered images from args.json", len(filtered_filenames))
    else:
        filtered_filenames = None

    dim = model_args.get("image_dimension", 128)
    cond_vars = model_args.get("conditional_variables", ["average_hue"])

    # ── 2. Load model ──────────────────────────────────────────────────
    logger.info("Loading model ...")
    model = load_tlv_model(MODEL_DIR / "model.pth", device=device)
    all_tendril_keys = model.list_tendril_keys() if model.has_tendrils() else []
    tendril_keys = [k for k in all_tendril_keys if k in TARGET_TENDRIL_KEYS]
    logger.info("Tendril keys to encode: %s", tendril_keys)

    # ── 3. Load images + HSV ───────────────────────────────────────────
    if filtered_filenames is not None:
        image_files = sorted(
            IMAGE_DIR / fn for fn in filtered_filenames if (IMAGE_DIR / fn).exists()
        )
    else:
        image_files = sorted(IMAGE_DIR.glob("*.bmp"))

    n_images = len(image_files)
    logger.info("Loading %d images ...", n_images)

    images = np.empty((n_images, 3, dim, dim), dtype=np.float32)
    ids = []
    hsv_data = np.empty((n_images, 3), dtype=np.float64)
    media_list = []

    for i, img_path in enumerate(image_files):
        img = PILImage.open(img_path).convert("RGB").resize((dim, dim))
        img_hsv = img.convert("HSV")
        hsv_data[i] = _compute_hsv_edge(np.array(img_hsv), dim)
        arr = np.array(img, dtype=np.float32) / 255.0
        images[i] = arr.transpose(2, 0, 1)
        ids.append(img_path.stem)
        media_list.append(_parse_media(img_path.name))
        if (i + 1) % 5000 == 0:
            logger.info("  loaded %d / %d", i + 1, n_images)

    logger.info("All images loaded.")

    # ── 4. Conditioning ────────────────────────────────────────────────
    conditioning = np.empty((n_images, len(cond_vars)), dtype=np.float32)
    for ci, var in enumerate(cond_vars):
        if var == "average_hue":
            conditioning[:, ci] = hsv_data[:, 0] / 255.0
        elif var == "average_saturation":
            conditioning[:, ci] = hsv_data[:, 1] / 255.0
        elif var == "average_value":
            conditioning[:, ci] = hsv_data[:, 2] / 255.0

    # ── 5. Encode ──────────────────────────────────────────────────────
    mu_lists: dict[str, list] = {}
    for key in tendril_keys:
        mu_lists[key] = []

    bs = args.batch_size
    n_batches = (n_images + bs - 1) // bs
    logger.info("Encoding %d batches ...", n_batches)

    for b in range(n_batches):
        s, e = b * bs, min((b + 1) * bs, n_images)
        x = torch.from_numpy(images[s:e]).to(device)
        c = torch.from_numpy(conditioning[s:e]).to(device)

        with torch.no_grad():
            # encode_full needed to populate skip connections for tendrils
            z, mu, logvar = model.encode_full(x, c)

            if tendril_keys:
                skip = model.get_last_skip()
                for j, key in enumerate(all_tendril_keys):
                    if key in TARGET_TENDRIL_KEYS:
                        _, mu_t, _, _ = model.encode_tendril(key, skip[j], cond=c)
                        mu_lists[key].append(mu_t.cpu().numpy())

        if (b + 1) % 20 == 0:
            logger.info("  batch %d / %d", b + 1, n_batches)

    all_mu = {k: np.vstack(v).astype(np.float32) for k, v in mu_lists.items()}
    logger.info("Encoding done. Shapes: %s", {k: v.shape for k, v in all_mu.items()})

    # ── 6. Build metadata + merge labels ───────────────────────────────
    metadata = pd.DataFrame({
        "id": ids,
        "media": media_list,
        "average_hue": hsv_data[:, 0],
        "average_saturation": hsv_data[:, 1],
        "average_value": hsv_data[:, 2],
    })
    metadata = _merge_manual_labels(metadata)

    label_counts = metadata["manual_formation"].value_counts()
    logger.info("Label distribution:\n%s", label_counts.to_string())

    # ── 7. Residualize ─────────────────────────────────────────────────
    media_arr = metadata["media"].values
    hsv_matrix = build_design_matrix(
        metadata["average_hue"].values,
        metadata["average_saturation"].values,
        metadata["average_value"].values,
    )

    residuals = {}
    for space, M in all_mu.items():
        resid, _ = within_group_residuals(M, media_arr, hsv_matrix)
        residuals[space] = resid

    # ── 8. Evaluate ────────────────────────────────────────────────────
    labels_all = metadata["manual_formation"].values
    results_rows = []
    output_dir = args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    for space in sorted(all_mu.keys()):
        for variant, M in [("raw", all_mu[space]), ("residual", residuals[space])]:
            M_filt, labels_filt = _filter_labelled(M, labels_all)
            n_labelled = len(labels_filt)
            classes = np.unique(labels_filt)
            n_classes = len(classes)

            if n_labelled < 10 or n_classes < 2:
                logger.warning(
                    "Skipping %s/%s: only %d labelled samples, %d classes.",
                    space, variant, n_labelled, n_classes,
                )
                continue

            sil = _compute_silhouette(M_filt, labels_filt)
            acc, bacc = _compute_knn_cv(M_filt, labels_filt)

            results_rows.append({
                "space": space,
                "variant": variant,
                "n_labelled": n_labelled,
                "n_classes": n_classes,
                "silhouette": round(sil, 4),
                "knn_accuracy": round(acc, 4),
                "knn_balanced_accuracy": round(bacc, 4),
            })

            # Confusion matrix
            cm_df = _confusion_df(M_filt, labels_filt)
            cm_path = output_dir / f"confusion_{space}_{variant}.csv"
            cm_df.to_csv(cm_path)

            logger.info(
                "%s/%s: sil=%.4f  acc=%.4f  bacc=%.4f  (n=%d, classes=%d)",
                space, variant, sil, acc, bacc, n_labelled, n_classes,
            )

    # ── 9. Write summary ──────────────────────────────────────────────
    results_df = pd.DataFrame(results_rows)
    csv_path = output_dir / "bio_label_eval_manual_formation.csv"
    results_df.to_csv(csv_path, index=False)
    logger.info("Wrote summary to %s", csv_path)

    # Print summary table
    print("\n" + "=" * 72)
    print("Biological label evaluation — manual_formation")
    print("=" * 72)
    print(
        f"{'space':>10s}  {'variant':>10s}  {'n':>6s}  {'cls':>4s}  "
        f"{'silhouette':>10s}  {'kNN acc':>8s}  {'kNN bacc':>9s}"
    )
    print("-" * 72)
    for _, row in results_df.iterrows():
        print(
            f"{row['space']:>10s}  {row['variant']:>10s}  "
            f"{row['n_labelled']:6d}  {row['n_classes']:4d}  "
            f"{row['silhouette']:10.4f}  {row['knn_accuracy']:8.4f}  "
            f"{row['knn_balanced_accuracy']:9.4f}"
        )
    print("=" * 72 + "\n")


if __name__ == "__main__":
    main()
