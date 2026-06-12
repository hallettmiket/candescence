"""
Purpose: Ensure the bundled sample dataset loads through the training pipeline.
Author: Hallett Lab
Date: 2026-06-12
Input: data/sample (PNG images + labels CSV) and FullDataset.
Output: Pytest assertions.

This guards two things at once: (1) the in-repo sample stays usable on a fresh
clone, and (2) FullDataset accepts a *string* raw_images_path (as the Streamlit
app supplies) and non-BMP (PNG) images.
"""

from __future__ import annotations

from pathlib import Path

import pytest

torch = pytest.importorskip("torch")
pytest.importorskip("torchvision")

from candescence.core.config import TLVConfig
from candescence.tlv.data.dataset import FullDataset

REPO_ROOT = Path(__file__).resolve().parents[3]
SAMPLE_DIR = REPO_ROOT / "data" / "sample"


@pytest.mark.skipif(
    not (SAMPLE_DIR / "images").is_dir(), reason="sample dataset not present"
)
def test_sample_dataset_loads_from_string_path_and_png() -> None:
    cfg = TLVConfig("sample_test", "run")
    # The app supplies these as plain strings; PNG (not BMP) images.
    cfg.raw_images_path = str(SAMPLE_DIR / "images")
    cfg.manual_labels_path = str(SAMPLE_DIR / "manually_labelled_images.csv")
    cfg.manually_labelled_path = cfg.manual_labels_path
    cfg.merge_manually_labelled = True
    cfg.restrict_to_day = None
    cfg.train_num, cfg.validation_num, cfg.test_num = 8, 2, 2
    cfg.architecture, cfg.strategy = "c_vae", 0

    ds = FullDataset(cfg)

    # Filenames parsed and morphology labels merged for every sample image.
    assert len(ds.meta_df) == 36
    assert (ds.meta_df["manual_formation"] != "unlabelled").all()
    # Multiple growth media represented (good for a latent-space demo).
    assert ds.meta_df["media"].nunique() >= 4
    # A train/val/test selection was built.
    assert len(ds) == 12
