"""Tests for hue_nuisance_qc module.

Uses synthetic data with a known linear hue component injected into
the latent space.  After residualization, PC1 explained variance
should drop and PC1-hue correlation should decrease.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from candescence.tlv.analysis.hue_nuisance_qc import (
    HueNuisanceReport,
    PCAReportResult,
    StratumFitInfo,
    build_design_matrix,
    pca_report,
    run_hue_nuisance_qc,
    within_group_residuals,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture()
def rng():
    return np.random.default_rng(42)


@pytest.fixture()
def synthetic_data(rng):
    """Create synthetic latent matrix with a large hue-driven component.

    Strategy:
    - Two media groups ("ypd", "spider"), 100 samples each.
    - HSV drawn uniformly; hue injected as a strong signal into dim-0.
    - Remaining dims are small noise.
    """
    n_per_group = 100
    n = n_per_group * 2
    d = 16

    media = np.array(["ypd"] * n_per_group + ["spider"] * n_per_group)

    # HSV on storage scale 0-255
    hue = rng.uniform(0, 255, size=n)
    sat = rng.uniform(50, 200, size=n)
    val = rng.uniform(50, 200, size=n)

    # Latent matrix: dim-0 dominated by hue, rest is noise
    M = rng.normal(0, 0.1, size=(n, d))
    M[:, 0] += 5.0 * (hue / 255.0)  # strong hue signal in dim 0

    metadata = pd.DataFrame({
        "media": media,
        "average_hue": hue,
        "average_saturation": sat,
        "average_value": val,
    })

    return M, metadata


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

class TestBuildDesignMatrix:
    def test_shape_and_scale(self, rng):
        h = rng.uniform(0, 255, 50)
        s = rng.uniform(0, 255, 50)
        v = rng.uniform(0, 255, 50)
        dm = build_design_matrix(h, s, v)
        assert dm.shape == (50, 3)
        assert dm.min() >= 0.0
        assert dm.max() <= 1.0

    def test_known_values(self):
        dm = build_design_matrix(
            np.array([0, 127.5, 255]),
            np.array([0, 127.5, 255]),
            np.array([0, 127.5, 255]),
        )
        np.testing.assert_allclose(dm[0], [0, 0, 0])
        np.testing.assert_allclose(dm[1], [0.5, 0.5, 0.5])
        np.testing.assert_allclose(dm[2], [1, 1, 1])


class TestWithinGroupResiduals:
    def test_residuals_shape(self, synthetic_data):
        M, meta = synthetic_data
        hsv = build_design_matrix(
            meta["average_hue"].values,
            meta["average_saturation"].values,
            meta["average_value"].values,
        )
        residuals, strata = within_group_residuals(M, meta["media"].values, hsv)
        assert residuals.shape == M.shape
        assert len(strata) == 2
        assert all(not s.skipped for s in strata)

    def test_small_stratum_skipped(self, rng):
        M = rng.normal(size=(5, 4))
        media = np.array(["a"] * 5)
        hsv = rng.uniform(size=(5, 3))
        residuals, strata = within_group_residuals(M, media, hsv, min_n=10)
        assert strata[0].skipped
        # Residuals should be the original M for skipped strata
        np.testing.assert_array_equal(residuals, M)

    def test_hue_variance_reduced(self, synthetic_data):
        """After residualization, dim-0 variance should drop substantially."""
        M, meta = synthetic_data
        hsv = build_design_matrix(
            meta["average_hue"].values,
            meta["average_saturation"].values,
            meta["average_value"].values,
        )
        residuals, _ = within_group_residuals(M, meta["media"].values, hsv)
        assert np.var(residuals[:, 0]) < 0.5 * np.var(M[:, 0])


class TestPCAReport:
    def test_basic(self, synthetic_data):
        M, meta = synthetic_data
        hue_01 = meta["average_hue"].values / 255.0
        rep = pca_report(M, meta["media"].values, hue_01, label="test", n_pcs=4)
        assert isinstance(rep, PCAReportResult)
        assert len(rep.evr) == 4
        assert rep.evr[0] > 0.3  # hue-dominated PC1 should be large
        assert abs(rep.pc1_hue_corr_overall) > 0.5

    def test_residual_decorrelates(self, synthetic_data):
        M, meta = synthetic_data
        hsv = build_design_matrix(
            meta["average_hue"].values,
            meta["average_saturation"].values,
            meta["average_value"].values,
        )
        hue_01 = hsv[:, 0]
        media = meta["media"].values

        residuals, _ = within_group_residuals(M, media, hsv)

        raw_rep = pca_report(M, media, hue_01, label="raw", n_pcs=4)
        res_rep = pca_report(residuals, media, hue_01, label="residual", n_pcs=4)

        # PC1 explained variance should drop
        assert res_rep.evr[0] < raw_rep.evr[0]
        # PC1–hue correlation should weaken
        assert abs(res_rep.pc1_hue_corr_overall) < abs(raw_rep.pc1_hue_corr_overall)


class TestRunHueNuisanceQC:
    def test_end_to_end(self, synthetic_data, tmp_path):
        M, meta = synthetic_data
        report = run_hue_nuisance_qc(
            M, meta, space_name="test", n_pcs=4, output_dir=tmp_path,
        )
        assert isinstance(report, HueNuisanceReport)
        assert report.n == 200
        assert report.n_dropped == 0
        assert report.delta_evr_pc1 > 0  # hue removal should reduce PC1
        # CSV written
        csv = tmp_path / "summary_test.csv"
        assert csv.exists()
        df = pd.read_csv(csv)
        assert "metric" in df.columns

    def test_missing_column_raises(self, synthetic_data):
        M, meta = synthetic_data
        meta_bad = meta.drop(columns=["average_hue"])
        with pytest.raises(ValueError, match="missing columns"):
            run_hue_nuisance_qc(M, meta_bad)

    def test_row_mismatch_raises(self, synthetic_data):
        M, meta = synthetic_data
        with pytest.raises(ValueError, match="Row count mismatch"):
            run_hue_nuisance_qc(M[:10], meta)

    def test_nan_rows_dropped(self, synthetic_data):
        M, meta = synthetic_data
        meta = meta.copy()
        meta.loc[0:4, "average_hue"] = np.nan
        report = run_hue_nuisance_qc(M, meta, space_name="nan_test")
        assert report.n_dropped == 5
        assert report.n == 195
