"""
Purpose: Lock in HSV adjustment behaviour now that it is exposed in the GUI.
Author: Hallett Lab
Date: 2026-06-12
Input: FullDataset._adjust_hsv_helper and TLVConfig adjustment defaults.
Output: Pytest assertions.

The training wizard now exposes deterministic HSV adjustment (adjust_images +
target_hue/saturation/value). These tests pin the underlying transform and the
config defaults so the UI's promises hold.
"""

from __future__ import annotations

import pytest

torch = pytest.importorskip("torch")

from candescence.core.config import TLVConfig
from candescence.tlv.data.dataset import FullDataset


def test_config_has_adjustment_defaults() -> None:
    cfg = TLVConfig("exp", "run")
    assert cfg.adjust_images is False
    # Targets default to the real-cohort dataset means (HSV on a 0-1 scale).
    assert cfg.target_hue == pytest.approx(107.15 / 255.0)
    assert cfg.target_saturation == pytest.approx(49.07 / 255.0)
    assert cfg.target_value == pytest.approx(93.38 / 255.0)


def test_value_and_saturation_shift_with_clamp() -> None:
    hsv = torch.tensor([[[0.2]], [[0.4]], [[0.6]]])  # (H, S, V), 1x1 image
    out = FullDataset._adjust_hsv_helper(hsv, hue_factor=0.0,
                                         saturation_factor=0.5, value_factor=0.5)
    # Saturation 0.4 + 0.5 -> clamp(0.9); Value 0.6 + 0.5 -> clamp(1.0)
    assert out[1].item() == pytest.approx(0.9)
    assert out[2].item() == pytest.approx(1.0)


def test_hue_wraps_around() -> None:
    hsv = torch.tensor([[[0.9]], [[0.5]], [[0.5]]])
    out = FullDataset._adjust_hsv_helper(hsv, hue_factor=0.2,
                                         saturation_factor=0.0, value_factor=0.0)
    # Hue 0.9 + 0.2 = 1.1 -> wraps to 0.1
    assert out[0].item() == pytest.approx(0.1)


def test_zero_adjustment_is_identity() -> None:
    hsv = torch.tensor([[[0.3]], [[0.4]], [[0.7]]])
    out = FullDataset._adjust_hsv_helper(hsv, 0.0, 0.0, 0.0)
    assert torch.allclose(out, hsv)
