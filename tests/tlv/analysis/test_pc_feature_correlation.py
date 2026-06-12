"""Tests for candescence.tlv.analysis.pc_feature_correlation."""

import numpy as np
import pandas as pd
import pytest

from candescence.tlv.analysis.pc_feature_correlation import (
    partial_corr_matrix,
    pc_feature_correlation,
    pearson_matrix,
)


def _synth_data(n: int = 200, k: int = 4, f: int = 3, seed: int = 0):
    """Generate synthetic scores, features, and a categorical covariate."""
    rng = np.random.default_rng(seed)
    scores = rng.standard_normal((n, k))
    features = rng.standard_normal((n, f))
    media = rng.choice(["RPMI", "YPD", "Spider"], size=n)
    feat_df = pd.DataFrame(
        features, columns=[f"feat_{i}" for i in range(f)]
    )
    return scores, features, feat_df, media


class TestPearsonMatrix:
    def test_shape(self):
        scores, features, _, _ = _synth_data(k=3, f=5)
        r = pearson_matrix(scores, features)
        assert r.shape == (3, 5)

    def test_self_correlation(self):
        """Correlating a matrix with itself should give 1 on the diagonal."""
        rng = np.random.default_rng(1)
        X = rng.standard_normal((100, 4))
        r = pearson_matrix(X, X)
        np.testing.assert_allclose(np.diag(r), 1.0, atol=1e-10)

    def test_bounded(self):
        scores, features, _, _ = _synth_data()
        r = pearson_matrix(scores, features)
        assert (np.abs(r) <= 1.0 + 1e-10).all()

    def test_constant_column_returns_zero(self):
        scores = np.random.default_rng(2).standard_normal((50, 2))
        features = np.column_stack([np.ones(50), scores[:, 0]])
        r = pearson_matrix(scores, features)
        assert r[0, 0] == pytest.approx(0.0)
        assert r[1, 0] == pytest.approx(0.0)


class TestPartialCorrMatrix:
    def test_shape(self):
        scores, features, _, media = _synth_data(k=2, f=4)
        pr = partial_corr_matrix(scores, features, media)
        assert pr.shape == (2, 4)

    def test_bounded(self):
        scores, features, _, media = _synth_data()
        pr = partial_corr_matrix(scores, features, media)
        assert (np.abs(pr) <= 1.0 + 1e-10).all()

    def test_no_covariate_effect_matches_pearson(self):
        """When covariate is constant, partial == Pearson."""
        scores, features, _, _ = _synth_data(n=100)
        media = np.array(["A"] * 100)
        r = pearson_matrix(scores, features)
        pr = partial_corr_matrix(scores, features, media)
        np.testing.assert_allclose(r, pr, atol=1e-6)

    def test_removes_confound(self):
        """Score and feature both driven by media → partial r should shrink."""
        rng = np.random.default_rng(7)
        n = 300
        media = rng.choice(["X", "Y", "Z"], size=n)
        media_effect = np.where(media == "X", 5.0, np.where(media == "Y", -3.0, 0.0))
        # Both score and feature share a media-driven component
        score = (media_effect + rng.standard_normal(n) * 0.5).reshape(-1, 1)
        feature = (media_effect + rng.standard_normal(n) * 0.5).reshape(-1, 1)

        r = pearson_matrix(score, feature)
        pr = partial_corr_matrix(score, feature, media)
        # Marginal r should be large (confounded); partial r should be much smaller
        assert abs(r[0, 0]) > 0.3
        assert abs(pr[0, 0]) < abs(r[0, 0])


class TestPcFeatureCorrelation:
    def test_driver_without_covariate(self):
        scores, _, feat_df, _ = _synth_data(k=6, f=3)
        pearson_df, partial_df = pc_feature_correlation(
            scores, feat_df, n_pcs=4
        )
        assert pearson_df.shape == (4, 3)
        assert list(pearson_df.index) == ["PC1", "PC2", "PC3", "PC4"]
        assert partial_df is None

    def test_driver_with_covariate(self):
        scores, _, feat_df, media = _synth_data(k=6, f=3)
        pearson_df, partial_df = pc_feature_correlation(
            scores, feat_df, n_pcs=4, covariate=media
        )
        assert pearson_df.shape == (4, 3)
        assert partial_df is not None
        assert partial_df.shape == (4, 3)

    def test_n_pcs_capped(self):
        scores, _, feat_df, _ = _synth_data(k=2, f=3)
        pearson_df, _ = pc_feature_correlation(scores, feat_df, n_pcs=10)
        # Should cap to available PCs (2)
        assert pearson_df.shape == (2, 3)
