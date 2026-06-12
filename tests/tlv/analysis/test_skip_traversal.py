"""Tests for candescence.tlv.analysis.skip_traversal."""

import numpy as np
import pytest

from candescence.tlv.analysis.skip_traversal import (
    skip_channels_to_rgb,
    _minmax_scale,
)


class TestSkipChannelsToRgb:
    """Test the feature-map visualisation helper."""

    def test_pca_output_shape(self):
        fmap = np.random.default_rng(0).standard_normal((16, 8, 8)).astype(np.float32)
        rgb = skip_channels_to_rgb(fmap, method="pca")
        assert rgb.shape == (8, 8, 3)
        assert rgb.dtype == np.uint8

    def test_norm_output_shape(self):
        fmap = np.random.default_rng(1).standard_normal((32, 4, 4)).astype(np.float32)
        rgb = skip_channels_to_rgb(fmap, method="norm")
        assert rgb.shape == (4, 4, 3)
        assert rgb.dtype == np.uint8

    def test_norm_values_bounded(self):
        fmap = np.random.default_rng(2).standard_normal((8, 6, 6)).astype(np.float32)
        rgb = skip_channels_to_rgb(fmap, method="norm")
        assert rgb.min() >= 0
        assert rgb.max() <= 255

    def test_constant_feature_map(self):
        fmap = np.ones((4, 3, 3), dtype=np.float32)
        rgb = skip_channels_to_rgb(fmap, method="norm")
        # Constant map → all zero after minmax scale
        assert rgb.shape == (3, 3, 3)

    def test_single_channel(self):
        fmap = np.random.default_rng(3).standard_normal((1, 5, 5)).astype(np.float32)
        rgb = skip_channels_to_rgb(fmap, method="pca")
        assert rgb.shape == (5, 5, 3)

    def test_pca_deterministic(self):
        fmap = np.random.default_rng(4).standard_normal((8, 4, 4)).astype(np.float32)
        r1 = skip_channels_to_rgb(fmap, method="pca")
        r2 = skip_channels_to_rgb(fmap, method="pca")
        np.testing.assert_array_equal(r1, r2)


class TestMinmaxScale:
    def test_normal(self):
        arr = np.array([1.0, 2.0, 3.0])
        scaled = _minmax_scale(arr)
        np.testing.assert_allclose(scaled, [0.0, 0.5, 1.0])

    def test_constant(self):
        arr = np.array([5.0, 5.0, 5.0])
        scaled = _minmax_scale(arr)
        np.testing.assert_allclose(scaled, [0.0, 0.0, 0.0])

    def test_negative(self):
        arr = np.array([-3.0, 0.0, 3.0])
        scaled = _minmax_scale(arr)
        assert scaled.min() == pytest.approx(0.0)
        assert scaled.max() == pytest.approx(1.0)
