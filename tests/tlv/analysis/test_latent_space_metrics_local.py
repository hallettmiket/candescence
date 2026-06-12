"""Tests for k-NN label purity and within/between distance effect."""

import numpy as np
import pytest

from candescence.tlv.analysis.latent_space_metrics import (
    KNNPurityResult,
    WithinBetweenResult,
    knn_label_purity,
    within_between_distance_effect,
)


# ---------------------------------------------------------------------------
# k-NN purity
# ---------------------------------------------------------------------------

class TestKNNLabelPurity:
    """Tests for knn_label_purity()."""

    def _make_blobs(self, rng, n=50, d=8, separation=10.0):
        """Two well-separated Gaussian blobs."""
        X = np.vstack([
            rng.normal(loc=0, scale=1, size=(n, d)),
            rng.normal(loc=separation, scale=1, size=(n, d)),
        ])
        labels = np.array(["A"] * n + ["B"] * n)
        return X, labels

    def test_well_separated_blobs_high_purity(self):
        rng = np.random.default_rng(0)
        X, labels = self._make_blobs(rng)
        result = knn_label_purity(X, labels, k=10)

        assert isinstance(result, KNNPurityResult)
        assert result.mean_purity > 0.95
        assert result.n_samples == 100
        assert result.n_classes == 2
        assert result.k_used == 10
        assert len(result.per_sample_purity) == 100

    def test_random_labels_low_purity(self):
        rng = np.random.default_rng(1)
        X = rng.normal(size=(100, 5))
        labels = rng.choice(["A", "B"], size=100)
        result = knn_label_purity(X, labels, k=10)

        # Random labels → purity ~ 0.5
        assert 0.3 < result.mean_purity < 0.7

    def test_exclude_unlabelled(self):
        rng = np.random.default_rng(2)
        X, labels = self._make_blobs(rng, n=30)
        labels = labels.astype(object)
        labels[0] = None
        labels[1] = "unlabelled"
        labels[2] = ""

        result = knn_label_purity(X, labels, k=5, exclude_unlabelled=True)
        assert result.mask.sum() == len(X) - 3
        assert result.n_samples == len(X) - 3

    def test_single_class_raises(self):
        X = np.random.default_rng(3).normal(size=(20, 3))
        labels = np.array(["A"] * 20)
        with pytest.raises(ValueError, match="at least 2"):
            knn_label_purity(X, labels)

    def test_length_mismatch_raises(self):
        with pytest.raises(ValueError, match="same length"):
            knn_label_purity(np.zeros((5, 2)), np.array(["A", "B"]))

    def test_k_clamped_for_small_n(self):
        rng = np.random.default_rng(4)
        X = rng.normal(size=(5, 2))
        labels = np.array(["A", "A", "B", "B", "B"])
        result = knn_label_purity(X, labels, k=100)
        assert result.k_used == 4  # min(100, 5-1)

    def test_purity_range(self):
        rng = np.random.default_rng(5)
        X, labels = self._make_blobs(rng)
        result = knn_label_purity(X, labels, k=5)
        assert all(0.0 <= p <= 1.0 for p in result.per_sample_purity)


# ---------------------------------------------------------------------------
# Within / between distance effect
# ---------------------------------------------------------------------------

class TestWithinBetweenDistanceEffect:
    """Tests for within_between_distance_effect()."""

    def _make_blobs(self, rng, n=50, d=8, separation=10.0):
        X = np.vstack([
            rng.normal(loc=0, scale=1, size=(n, d)),
            rng.normal(loc=separation, scale=1, size=(n, d)),
        ])
        labels = np.array(["A"] * n + ["B"] * n)
        return X, labels

    def test_separated_blobs_high_ratio(self):
        rng = np.random.default_rng(10)
        X, labels = self._make_blobs(rng, separation=20.0)
        result = within_between_distance_effect(X, labels)

        assert isinstance(result, WithinBetweenResult)
        assert result.ratio > 2.0  # between >> within
        assert result.cohens_d > 1.0  # large effect
        assert result.n_samples == 100

    def test_overlapping_blobs_low_ratio(self):
        rng = np.random.default_rng(11)
        X, labels = self._make_blobs(rng, separation=0.0)
        result = within_between_distance_effect(X, labels)

        # Overlapping → ratio close to 1
        assert 0.8 < result.ratio < 1.3

    def test_exclude_unlabelled(self):
        rng = np.random.default_rng(12)
        X, labels = self._make_blobs(rng, n=30)
        labels = labels.astype(object)
        labels[0] = None
        labels[1] = "unlabelled"

        result = within_between_distance_effect(
            X, labels, exclude_unlabelled=True,
        )
        assert result.n_samples == len(X) - 2

    def test_single_class_raises(self):
        X = np.random.default_rng(13).normal(size=(20, 3))
        labels = np.array(["A"] * 20)
        with pytest.raises(ValueError, match="at least 2"):
            within_between_distance_effect(X, labels)

    def test_length_mismatch_raises(self):
        with pytest.raises(ValueError, match="same length"):
            within_between_distance_effect(np.zeros((5, 2)), np.array(["A", "B"]))

    def test_positive_distances(self):
        rng = np.random.default_rng(14)
        X, labels = self._make_blobs(rng)
        result = within_between_distance_effect(X, labels)
        assert result.mean_within > 0
        assert result.mean_between > 0
