"""Tests for raw-pixel and feature-based image distance matrices."""

import numpy as np
import pytest

from candescence.tlv.analysis.image_distance_metrics import (
    feature_distance_matrix,
    raw_pixel_distance_matrix,
)


def _fake_images(n=8, c=3, h=16, w=16, seed=0):
    rng = np.random.default_rng(seed)
    return (rng.random((n, c, h, w)) * 255).astype(np.uint8)


class TestRawPixelDistanceMatrix:
    def test_shape_and_symmetry(self):
        imgs = _fake_images()
        D = raw_pixel_distance_matrix(imgs)
        assert D.shape == (8, 8)
        assert D.dtype == np.float32
        np.testing.assert_allclose(D, D.T, atol=1e-6)
        np.testing.assert_allclose(np.diag(D), 0.0, atol=1e-6)

    def test_offdiagonal_positive(self):
        imgs = _fake_images()
        D = raw_pixel_distance_matrix(imgs)
        off = D[~np.eye(D.shape[0], dtype=bool)]
        assert (off > 0).all()

    def test_identical_images_zero_distance(self):
        img = (np.ones((1, 3, 8, 8), dtype=np.uint8) * 100)
        imgs = np.repeat(img, 3, axis=0)
        D = raw_pixel_distance_matrix(imgs)
        np.testing.assert_allclose(D, 0.0, atol=1e-6)

    def test_uint8_and_float_agree(self):
        imgs_u8 = _fake_images()
        imgs_f = imgs_u8.astype(np.float32) / 255.0
        D1 = raw_pixel_distance_matrix(imgs_u8)
        D2 = raw_pixel_distance_matrix(imgs_f)
        np.testing.assert_allclose(D1, D2, atol=1e-5)

    def test_unsupported_mode_raises(self):
        with pytest.raises(ValueError):
            raw_pixel_distance_matrix(_fake_images(), mode="bogus")


class TestFeatureDistanceMatrix:
    def test_shape_and_symmetry(self):
        imgs = _fake_images()
        D = feature_distance_matrix(imgs)
        assert D.shape == (8, 8)
        assert D.dtype == np.float32
        np.testing.assert_allclose(D, D.T, atol=1e-6)
        np.testing.assert_allclose(np.diag(D), 0.0, atol=1e-6)

    def test_standardize_changes_scale(self):
        imgs = _fake_images()
        D_std = feature_distance_matrix(imgs, standardize=True)
        D_raw = feature_distance_matrix(imgs, standardize=False)
        # Values differ in magnitude when standardized
        assert not np.allclose(D_std, D_raw)
