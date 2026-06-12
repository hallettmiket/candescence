"""Tests for candescence.tlv.analysis.image_feature_table."""

import numpy as np
import pandas as pd
import pytest

from candescence.tlv.analysis.image_feature_table import (
    FEATURE_NAMES,
    compute_image_features_batch,
)


def _random_images_nchw(n: int, c: int = 3, h: int = 32, w: int = 32) -> np.ndarray:
    rng = np.random.default_rng(42)
    return rng.random((n, c, h, w)).astype(np.float32)


def _random_images_nhwc(n: int, h: int = 32, w: int = 32, c: int = 3) -> np.ndarray:
    rng = np.random.default_rng(42)
    return rng.random((n, h, w, c)).astype(np.float32)


class TestComputeImageFeatures:
    """Core tests for compute_image_features_batch."""

    def test_output_shape_and_columns(self):
        imgs = _random_images_nchw(10)
        df = compute_image_features_batch(imgs)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 10
        assert list(df.columns) == FEATURE_NAMES

    def test_nhwc_input(self):
        imgs = _random_images_nhwc(8)
        df = compute_image_features_batch(imgs)
        assert len(df) == 8
        assert list(df.columns) == FEATURE_NAMES

    def test_uint8_input(self):
        rng = np.random.default_rng(99)
        imgs = rng.integers(0, 256, size=(5, 3, 16, 16), dtype=np.uint8)
        df = compute_image_features_batch(imgs)
        assert len(df) == 5
        # luminance_mean should be in [0, 1] after normalisation
        assert df["luminance_mean"].between(0, 1).all()

    def test_grayscale_single_channel(self):
        rng = np.random.default_rng(7)
        imgs = rng.random((6, 1, 24, 24)).astype(np.float32)
        df = compute_image_features_batch(imgs)
        assert len(df) == 6
        # channel means should equal luminance for single-channel
        np.testing.assert_allclose(
            df["channel_r_mean"].values,
            df["luminance_mean"].values,
            atol=1e-6,
        )

    def test_values_bounded(self):
        imgs = _random_images_nchw(12)
        df = compute_image_features_batch(imgs)
        assert df["luminance_mean"].between(0, 1).all()
        assert (df["luminance_std"] >= 0).all()
        assert (df["colony_area_frac"] >= 0).all()
        assert (df["colony_area_frac"] <= 1).all()
        assert (df["bbox_aspect_ratio"] >= 1.0).all()
        assert (df["centroid_offset"] >= 0).all()

    def test_uniform_image_features(self):
        """A constant-value image: area_frac ~1, aspect ~1, offset ~0."""
        imgs = np.full((1, 3, 32, 32), 0.5, dtype=np.float32)
        df = compute_image_features_batch(imgs, threshold_percentile=25.0)
        assert df["colony_area_frac"].iloc[0] == pytest.approx(1.0)
        assert df["luminance_std"].iloc[0] == pytest.approx(0.0, abs=1e-9)

    def test_deterministic(self):
        imgs = _random_images_nchw(5)
        df1 = compute_image_features_batch(imgs)
        df2 = compute_image_features_batch(imgs)
        pd.testing.assert_frame_equal(df1, df2)
