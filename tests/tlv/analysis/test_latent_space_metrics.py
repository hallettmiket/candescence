"""Tests for silhouette and Mantel latent-space metric helpers."""

import numpy as np
import pytest

from candescence.tlv.analysis.latent_space_metrics import (
    MantelResult,
    SilhouetteResult,
    SkbioUnavailableError,
    mantel_latent_vs_matrix,
    mantel_latent_vs_numeric,
    silhouette_metrics,
)
from scipy.spatial.distance import pdist, squareform


# ---------------------------------------------------------------------------
# Silhouette
# ---------------------------------------------------------------------------

class TestSilhouetteMetrics:
    """Tests for silhouette_metrics()."""

    def _make_blobs(self, rng, n=50, d=8, separation=10.0):
        """Two well-separated Gaussian blobs."""
        X = np.vstack([
            rng.normal(loc=0, scale=1, size=(n, d)),
            rng.normal(loc=separation, scale=1, size=(n, d)),
        ])
        # dtype=object so tests can later assign None / longer strings
        # (e.g. "unlabelled") without numpy's fixed-width dtype truncating
        # them to a single character.
        labels = np.array(["A"] * n + ["B"] * n, dtype=object)
        return X, labels

    def test_well_separated_blobs_high_score(self):
        rng = np.random.default_rng(0)
        X, labels = self._make_blobs(rng)
        result = silhouette_metrics(X, labels)

        assert isinstance(result, SilhouetteResult)
        assert -1 <= result.overall_score <= 1
        assert result.overall_score > 0.8  # well separated
        assert len(result.sample_scores) == len(X)
        assert result.mask.all()

    def test_random_labels_low_score(self):
        rng = np.random.default_rng(1)
        X = rng.normal(size=(100, 5))
        labels = rng.choice(["A", "B", "C"], size=100)
        result = silhouette_metrics(X, labels)

        assert -1 <= result.overall_score <= 1
        assert result.overall_score < 0.3

    def test_exclude_unlabelled(self):
        rng = np.random.default_rng(2)
        X, labels = self._make_blobs(rng, n=30)
        labels[0] = None
        labels[1] = "unlabelled"
        labels[2] = ""

        result = silhouette_metrics(X, labels, exclude_unlabelled=True)
        assert result.mask.sum() == len(X) - 3
        assert len(result.sample_scores) == len(X) - 3

    def test_unlabelled_kept_by_default(self):
        rng = np.random.default_rng(3)
        X, labels = self._make_blobs(rng, n=30)
        labels[0] = None

        result = silhouette_metrics(X, labels, exclude_unlabelled=False)
        # NaN label becomes "unlabelled" → 3 groups now
        assert "unlabelled" in result.labels

    def test_single_class_raises(self):
        X = np.random.default_rng(4).normal(size=(20, 3))
        labels = np.array(["A"] * 20)
        with pytest.raises(ValueError, match="at least 2"):
            silhouette_metrics(X, labels)

    def test_length_mismatch_raises(self):
        with pytest.raises(ValueError, match="same length"):
            silhouette_metrics(np.zeros((5, 2)), np.array(["A", "B"]))

    def test_score_in_range(self):
        rng = np.random.default_rng(5)
        X = rng.normal(size=(60, 4))
        labels = np.array(["X"] * 20 + ["Y"] * 20 + ["Z"] * 20)
        result = silhouette_metrics(X, labels)
        assert all(-1 <= s <= 1 for s in result.sample_scores)


# ---------------------------------------------------------------------------
# Mantel
# ---------------------------------------------------------------------------

class TestMantelLatentVsNumeric:
    """Tests for mantel_latent_vs_numeric().

    These tests require scikit-bio.  Marked with ``pytest.importorskip``
    so they are skipped gracefully when the package is absent.
    """

    @pytest.fixture(autouse=True)
    def _require_skbio(self):
        pytest.importorskip("skbio")

    def test_colinear_high_correlation(self):
        """Latent dim 1 is linearly related to y → high Mantel r."""
        rng = np.random.default_rng(10)
        n = 80
        y = rng.uniform(0, 10, size=n)
        X = np.column_stack([y + rng.normal(0, 0.1, n), rng.normal(size=n)])

        result = mantel_latent_vs_numeric(X, y)
        assert isinstance(result, MantelResult)
        assert result.statistic > 0.5
        assert result.p_value < 0.05
        assert result.n_samples == n
        assert result.subsampled is False

    def test_independent_low_correlation(self):
        """Random X and y → Mantel r ≈ 0."""
        rng = np.random.default_rng(11)
        n = 80
        X = rng.normal(size=(n, 4))
        y = rng.normal(size=n)

        result = mantel_latent_vs_numeric(X, y)
        assert abs(result.statistic) < 0.3

    def test_subsampling(self):
        rng = np.random.default_rng(12)
        n = 200
        y = rng.uniform(size=n)
        X = rng.normal(size=(n, 3))

        result = mantel_latent_vs_numeric(X, y, max_n=50)
        assert result.n_samples == 50
        assert result.subsampled is True

    def test_nan_rows_dropped(self):
        rng = np.random.default_rng(13)
        n = 50
        y = rng.uniform(size=n).astype(float)
        X = rng.normal(size=(n, 3))
        y[0] = np.nan
        y[1] = np.nan

        result = mantel_latent_vs_numeric(X, y)
        assert result.n_samples == n - 2

    def test_too_few_samples_raises(self):
        with pytest.raises(ValueError, match="at least 3"):
            mantel_latent_vs_numeric(
                np.array([[1.0], [2.0]]),
                np.array([1.0, 2.0]),
            )

    def test_constant_y_raises(self):
        X = np.random.default_rng(14).normal(size=(10, 2))
        y = np.ones(10)
        with pytest.raises(ValueError, match="at least 2 unique"):
            mantel_latent_vs_numeric(X, y)

    def test_shape_mismatch_raises(self):
        with pytest.raises(ValueError, match="same length"):
            mantel_latent_vs_numeric(np.zeros((5, 2)), np.zeros(3))

    def test_y_must_be_1d(self):
        with pytest.raises(ValueError, match="1-D"):
            mantel_latent_vs_numeric(np.zeros((5, 2)), np.zeros((5, 2)))


class TestMantelLatentVsMatrix:
    """Tests for the generic Mantel-vs-arbitrary-matrix variant."""

    @pytest.fixture(autouse=True)
    def _require_skbio(self):
        pytest.importorskip("skbio")

    def _matching_matrix(self, X):
        return squareform(pdist(X, metric="euclidean"))

    def test_self_distance_perfect_correlation(self):
        rng = np.random.default_rng(20)
        X = rng.normal(size=(40, 5))
        D = self._matching_matrix(X)
        result = mantel_latent_vs_matrix(X, D)
        assert isinstance(result, MantelResult)
        assert result.statistic == pytest.approx(1.0, abs=1e-4)
        assert result.n_samples == 40
        assert result.subsampled is False

    def test_unrelated_matrix_low_correlation(self):
        rng = np.random.default_rng(21)
        X = rng.normal(size=(60, 4))
        Y = rng.normal(size=(60, 4))
        D = self._matching_matrix(Y)
        result = mantel_latent_vs_matrix(X, D)
        assert abs(result.statistic) < 0.3

    def test_subsampling(self):
        rng = np.random.default_rng(22)
        X = rng.normal(size=(150, 4))
        D = self._matching_matrix(X)
        result = mantel_latent_vs_matrix(X, D, max_n=40)
        assert result.n_samples == 40
        assert result.subsampled is True
        # Even after subsampling, self-distance Mantel ≈ 1
        assert result.statistic == pytest.approx(1.0, abs=1e-4)

    def test_non_square_raises(self):
        X = np.random.default_rng(23).normal(size=(10, 3))
        with pytest.raises(ValueError, match="square"):
            mantel_latent_vs_matrix(X, np.zeros((10, 9)))

    def test_size_mismatch_raises(self):
        X = np.random.default_rng(24).normal(size=(10, 3))
        with pytest.raises(ValueError, match="must equal"):
            mantel_latent_vs_matrix(X, np.zeros((9, 9)))

    def test_non_finite_raises(self):
        X = np.random.default_rng(25).normal(size=(10, 3))
        D = self._matching_matrix(X)
        D[0, 1] = np.nan
        D[1, 0] = np.nan
        with pytest.raises(ValueError, match="non-finite"):
            mantel_latent_vs_matrix(X, D)

    def test_asymmetric_raises(self):
        X = np.random.default_rng(26).normal(size=(10, 3))
        D = self._matching_matrix(X)
        D[0, 1] += 1.0  # break symmetry
        with pytest.raises(ValueError, match="symmetric"):
            mantel_latent_vs_matrix(X, D)


class TestSkbioUnavailable:
    """Test graceful error when scikit-bio is missing."""

    def test_error_type(self):
        assert issubclass(SkbioUnavailableError, ImportError)
