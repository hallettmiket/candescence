#!/usr/bin/env python3
"""Run hue nuisance QC on the s14_run model.

Loads model, computes embeddings + HSV from images, runs analysis.

Usage::

    python scripts/run_hue_nuisance_s14.py [--output-dir DIR] [--device DEVICE]
"""

from __future__ import annotations

import argparse
import json
import logging
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from PIL import Image as PILImage

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from candescence.core.settings import get_settings

_RAW = str(get_settings().raw_path)
_REFINED = str(get_settings().refined_path)

from candescence.interface.model_loader import load_tlv_model
from candescence.tlv.analysis.hue_nuisance_qc import (
    report_to_stdout,
    run_hue_nuisance_qc,
)

logger = logging.getLogger(__name__)

MODEL_DIR = Path(
    f"{_REFINED}/zoo/training_20260402/s14_run/models"
)
IMAGE_DIR = Path(f"{_RAW}/tlv_images/all-final")
DEFAULT_OUTPUT = Path(
    f"{_REFINED}/zoo/training_20260402/s14_run/analyses/hue_nuisance_qc"
)

# Media normalization (matches dataset.py / latent_explorer_app.py)
MEDIA_MAP = {"spdr": "spider", "ctrl": "control"}


def _parse_media(filename: str) -> str:
    """Extract and normalise media from filename like P11_control_day2_1-r1-c10.bmp."""
    parts = Path(filename).stem.split("_")
    if len(parts) >= 2:
        raw = parts[1].lower()
        return MEDIA_MAP.get(raw, raw)
    return "unknown"


def _compute_hsv_edge(img_hsv_arr: np.ndarray, dim: int) -> tuple[float, float, float]:
    """Compute edge-band HSV (circular mean for hue). Returns 0-255 scale."""
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


def main() -> None:
    parser = argparse.ArgumentParser(description="Hue nuisance QC for s14_run")
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--device", type=str, default="cuda:4")
    parser.add_argument("--batch-size", type=int, default=256)
    parser.add_argument("--n-pcs", type=int, default=8)
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )

    device = torch.device(args.device if torch.cuda.is_available() else "cpu")

    # --- Determine which images were used in training ---
    args_json = MODEL_DIR / "args.json"
    with open(args_json) as f:
        model_args = json.load(f)

    filtered_entries = model_args.get("filtered_images", [])
    if filtered_entries:
        filtered_filenames = {e["filename"] for e in filtered_entries}
        logger.info("Using %d filtered images from args.json", len(filtered_filenames))
    else:
        filtered_filenames = None
        logger.info("No filtered_images in args.json; using all images in %s", IMAGE_DIR)

    dim = model_args.get("image_dimension", 128)
    cond_vars = model_args.get("conditional_variables", ["average_hue"])

    # --- Load model ---
    logger.info("Loading model from %s onto %s ...", MODEL_DIR / "model.pth", device)
    model = load_tlv_model(MODEL_DIR / "model.pth", device=device)
    has_tendrils = model.has_tendrils()
    tendril_keys = model.list_tendril_keys() if has_tendrils else []
    logger.info("Model loaded. Tendrils: %s", tendril_keys if has_tendrils else "none")

    # --- Load images, compute HSV, build tensors ---
    if filtered_filenames is not None:
        image_files = sorted(IMAGE_DIR / fn for fn in filtered_filenames if (IMAGE_DIR / fn).exists())
    else:
        image_files = sorted(IMAGE_DIR.glob("*.bmp"))

    n_images = len(image_files)
    logger.info("Loading %d images from %s ...", n_images, IMAGE_DIR)

    images = np.empty((n_images, 3, dim, dim), dtype=np.float32)
    ids = []
    hsv_data = np.empty((n_images, 3), dtype=np.float64)
    media_list = []

    for i, img_path in enumerate(image_files):
        img = PILImage.open(img_path).convert("RGB").resize((dim, dim))
        img_hsv = img.convert("HSV")

        # HSV from edges
        hsv_data[i] = _compute_hsv_edge(np.array(img_hsv), dim)

        # Image tensor (C, H, W) normalised to [0, 1]
        arr = np.array(img, dtype=np.float32) / 255.0
        images[i] = arr.transpose(2, 0, 1)

        ids.append(img_path.stem)
        media_list.append(_parse_media(img_path.name))

        if (i + 1) % 5000 == 0:
            logger.info("  loaded %d / %d", i + 1, n_images)

    logger.info("All %d images loaded.", n_images)

    # --- Build conditioning tensor (0-1 scale) ---
    conditioning = np.empty((n_images, len(cond_vars)), dtype=np.float32)
    for ci, var in enumerate(cond_vars):
        if var == "average_hue":
            conditioning[:, ci] = hsv_data[:, 0] / 255.0
        elif var == "average_saturation":
            conditioning[:, ci] = hsv_data[:, 1] / 255.0
        elif var == "average_value":
            conditioning[:, ci] = hsv_data[:, 2] / 255.0

    # --- Encode: primary + tendrils ---
    mu_lists: dict[str, list] = {"primary": []}
    if has_tendrils:
        for key in tendril_keys:
            mu_lists[key] = []

    bs = args.batch_size
    n_batches = (n_images + bs - 1) // bs
    logger.info("Encoding %d batches (batch_size=%d) ...", n_batches, bs)

    for b in range(n_batches):
        s, e = b * bs, min((b + 1) * bs, n_images)
        x = torch.from_numpy(images[s:e]).to(device)
        c = torch.from_numpy(conditioning[s:e]).to(device)

        with torch.no_grad():
            z, mu, logvar = model.encode_full(x, c)
            mu_lists["primary"].append(mu.cpu().numpy())

            if has_tendrils:
                skip = model.get_last_skip()
                for j, key in enumerate(tendril_keys):
                    _, mu_t, _, _ = model.encode_tendril(key, skip[j], cond=c)
                    mu_lists[key].append(mu_t.cpu().numpy())

        if (b + 1) % 20 == 0:
            logger.info("  batch %d / %d", b + 1, n_batches)

    # Stack
    all_mu = {k: np.vstack(v).astype(np.float32) for k, v in mu_lists.items()}
    logger.info(
        "Encoding done. Shapes: %s",
        {k: v.shape for k, v in all_mu.items()},
    )

    # --- Build metadata DataFrame ---
    metadata = pd.DataFrame({
        "id": ids,
        "media": media_list,
        "average_hue": hsv_data[:, 0],
        "average_saturation": hsv_data[:, 1],
        "average_value": hsv_data[:, 2],
    })

    # --- Run hue nuisance QC for each space ---
    output_dir = args.output_dir
    for space_name, M in sorted(all_mu.items()):
        logger.info("Running hue nuisance QC for '%s' ...", space_name)
        report = run_hue_nuisance_qc(
            M=M,
            metadata=metadata,
            space_name=space_name,
            n_pcs=args.n_pcs,
            output_dir=output_dir,
        )
        report_to_stdout(report)

    logger.info("Done. Results in %s", output_dir)


if __name__ == "__main__":
    main()
