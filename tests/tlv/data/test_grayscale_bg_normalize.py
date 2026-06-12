"""Tests for apply_grayscale_background_normalize and _edge_band_mask."""

import numpy as np
import pytest
from PIL import Image

from candescence.tlv.data.dataset import (
    _edge_band_mask,
    apply_grayscale_background_normalize,
)


class TestEdgeBandMask:
    def test_shape(self):
        mask = _edge_band_mask(128, 128, border=12)
        assert mask.shape == (128, 128)
        assert mask.dtype == bool

    def test_edges_true_center_false(self):
        mask = _edge_band_mask(128, 128, border=12)
        # corners must be True
        assert mask[0, 0]
        assert mask[127, 127]
        # center must be False
        assert not mask[64, 64]

    def test_custom_border(self):
        mask = _edge_band_mask(64, 64, border=5)
        assert mask[4, 4]    # within border
        assert not mask[32, 32]  # center


class TestApplyGrayscaleBackgroundNormalize:
    def _make_grey_image(self, edge_val: int, center_val: int, dim: int = 128) -> Image.Image:
        """Create a 3-channel greyscale image with distinct edge and center values."""
        arr = np.full((dim, dim), center_val, dtype=np.uint8)
        mask = _edge_band_mask(dim, dim, border=12)
        arr[mask] = edge_val
        rgb = np.stack([arr, arr, arr], axis=-1)
        return Image.fromarray(rgb, mode='RGB')

    def test_bright_edge_shifts_to_target(self):
        img = self._make_grey_image(edge_val=200, center_val=100, dim=128)
        result = apply_grayscale_background_normalize(img, 128, target=0.5)
        arr = np.array(result).astype(np.float32) / 255.0
        mask = _edge_band_mask(128, 128, border=12)
        edge_mean = arr[:, :, 0][mask].mean()
        assert abs(edge_mean - 0.5) < 0.02

    def test_dark_edge_shifts_to_target(self):
        img = self._make_grey_image(edge_val=30, center_val=180, dim=128)
        result = apply_grayscale_background_normalize(img, 128, target=0.5)
        arr = np.array(result).astype(np.float32) / 255.0
        mask = _edge_band_mask(128, 128, border=12)
        edge_mean = arr[:, :, 0][mask].mean()
        assert abs(edge_mean - 0.5) < 0.02

    def test_uniform_image_no_nan(self):
        """sigma~0 case: all pixels the same value."""
        img = self._make_grey_image(edge_val=100, center_val=100, dim=128)
        result = apply_grayscale_background_normalize(img, 128, target=0.5)
        arr = np.array(result).astype(np.float32) / 255.0
        assert not np.any(np.isnan(arr))
        edge_mean = arr[:, :, 0][_edge_band_mask(128, 128)].mean()
        assert abs(edge_mean - 0.5) < 0.02

    def test_output_is_three_identical_channels(self):
        img = self._make_grey_image(edge_val=150, center_val=80, dim=128)
        result = apply_grayscale_background_normalize(img, 128, target=0.5)
        arr = np.array(result)
        np.testing.assert_array_equal(arr[:, :, 0], arr[:, :, 1])
        np.testing.assert_array_equal(arr[:, :, 0], arr[:, :, 2])

    def test_values_clipped_to_valid_range(self):
        # Edge is very bright; shifting to 0.5 pushes center above 1.0
        img = self._make_grey_image(edge_val=250, center_val=250, dim=128)
        result = apply_grayscale_background_normalize(img, 128, target=0.5)
        arr = np.array(result)
        assert arr.min() >= 0
        assert arr.max() <= 255

    def test_custom_target(self):
        img = self._make_grey_image(edge_val=100, center_val=50, dim=128)
        result = apply_grayscale_background_normalize(img, 128, target=0.3)
        arr = np.array(result).astype(np.float32) / 255.0
        edge_mean = arr[:, :, 0][_edge_band_mask(128, 128)].mean()
        assert abs(edge_mean - 0.3) < 0.02
