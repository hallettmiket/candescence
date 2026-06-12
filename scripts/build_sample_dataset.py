"""
Purpose: Build the small, in-repo sample TLV dataset used for tutorials/first run.
Author: Hallett Lab
Date: 2026-06-12
Input: The full raw image tree + manual morphology labels (resolved via settings).
Output: data/sample/images/*.png (a stratified subset, PNG to keep the repo small)
        and data/sample/manually_labelled_images.csv (matching file_name+morphology).

A new user who clones Candescence gets a handful of real colony images spanning
several morphologies and growth media, so the app runs out-of-the-box without
access to the lab VM. Selection is deterministic (fixed seed) for reproducibility.

Run:  python scripts/build_sample_dataset.py
"""

from __future__ import annotations

import logging
import sys
from pathlib import Path

import pandas as pd
from PIL import Image

# Make the package importable when run as a script.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from candescence.core.settings import get_settings  # noqa: E402

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger("build_sample_dataset")

# How many images to take per morphology class (capped by availability).
PER_MORPHOLOGY = {
    "smooth": 8,
    "light filament": 6,
    "wrinkle": 6,
    "heavy filament": 4,
    "ring": 4,
    "blank": 4,
    "star": 2,
    "cut": 1,
    "stippled": 1,
}
RANDOM_STATE = 9954


def _media_token(file_name: str) -> str:
    """Extract the growth-media token from a colony filename."""
    parts = file_name.split("_")
    return parts[1] if len(parts) > 1 else "unknown"


def build_sample() -> None:
    settings = get_settings()
    raw_dir = settings.image_dir
    labels_csv = settings.manual_labels_csv
    out_dir = settings.repo_path / "data" / "sample"
    out_images = out_dir / "images"
    out_images.mkdir(parents=True, exist_ok=True)

    labels = pd.read_csv(labels_csv)
    # Keep only labelled images that actually exist in the raw tree.
    existing = {p.name for p in raw_dir.glob("*.bmp")}
    labels = labels[labels["file_name"].isin(existing)].copy()
    labels["media"] = labels["file_name"].map(_media_token)
    logger.info("Labelled images present in raw tree: %d", len(labels))

    # Stratified, media-diverse, deterministic selection per morphology.
    selected_rows = []
    for morphology, k in PER_MORPHOLOGY.items():
        pool = labels[labels["morphology"] == morphology]
        if pool.empty:
            continue
        # Spread across media: sort by media, then take an even stride.
        pool = pool.sort_values(["media", "file_name"]).reset_index(drop=True)
        take = min(k, len(pool))
        idx = [round(i * (len(pool) - 1) / max(take - 1, 1)) for i in range(take)]
        selected_rows.append(pool.iloc[sorted(set(idx))])
    selected = pd.concat(selected_rows, ignore_index=True)
    logger.info("Selected %d images across %d morphologies",
                len(selected), selected["morphology"].nunique())

    # Convert each BMP -> PNG (halves the size) keeping the original stem.
    out_records = []
    for _, row in selected.iterrows():
        src = raw_dir / row["file_name"]
        if not src.exists():
            logger.warning("Missing source image, skipping: %s", src)
            continue
        png_name = Path(row["file_name"]).with_suffix(".png").name
        Image.open(src).convert("RGB").save(out_images / png_name, "PNG")
        out_records.append({"file_name": png_name, "morphology": row["morphology"]})

    out_csv = out_dir / "manually_labelled_images.csv"
    pd.DataFrame(out_records).to_csv(out_csv, index=False)

    total_kb = sum(p.stat().st_size for p in out_images.glob("*.png")) // 1024
    logger.info("Wrote %d images (%d KB) to %s", len(out_records), total_kb, out_images)
    logger.info("Wrote labels CSV: %s", out_csv)
    logger.info("Media spread: %s", selected["media"].value_counts().to_dict())


if __name__ == "__main__":
    build_sample()
