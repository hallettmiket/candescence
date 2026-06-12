"""Minimal tests for bio label evaluation helpers."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

# Import the helpers directly from the script module
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent / "scripts"))

from run_bio_label_eval_on_hue_residuals import _parse_media, _merge_manual_labels


class TestParseMedia:
    def test_standard_filenames(self):
        assert _parse_media("P11_control_day2_1-r1-c10.bmp") == "control"
        assert _parse_media("Pl1_spider_day2_1-r5-c4.bmp") == "spider"
        assert _parse_media("P16_RPMI_day5_2-r5-c10.bmp") == "rpmi"
        assert _parse_media("Pwt_ctrl_day2_2-r8-c8.bmp") == "control"
        assert _parse_media("P11_ypd_day2_1-r1-c1.bmp") == "ypd"
        assert _parse_media("Pl1_spdr_day2_1-r3-c2.bmp") == "spider"
        assert _parse_media("P11_serum_day2_1-r1-c5.bmp") == "serum"


class TestMergeLabelAlignment:
    """Pure pandas merge alignment test (no file IO)."""

    def test_merge_preserves_row_order(self):
        """Labels merge correctly and unlabelled rows get filled."""
        metadata = pd.DataFrame({
            "id": ["img_A", "img_B", "img_C"],
            "media": ["ypd", "spider", "control"],
            "average_hue": [100.0, 150.0, 200.0],
            "average_saturation": [50.0, 60.0, 70.0],
            "average_value": [80.0, 90.0, 100.0],
        })

        # Simulate _merge_manual_labels logic without file IO
        labels_df = pd.DataFrame({
            "filename": ["img_A.bmp", "img_C.bmp"],
            "morphology": ["smooth", "wrinkle"],
        })

        metadata = metadata.copy()
        metadata["filename"] = metadata["id"] + ".bmp"
        merged = metadata.merge(
            labels_df[["filename", "morphology"]],
            on="filename",
            how="left",
        )
        merged = merged.rename(columns={"morphology": "manual_formation"})
        merged["manual_formation"] = merged["manual_formation"].fillna("unlabelled")

        assert list(merged["manual_formation"]) == ["smooth", "unlabelled", "wrinkle"]
        assert list(merged["id"]) == ["img_A", "img_B", "img_C"]
        assert len(merged) == 3
