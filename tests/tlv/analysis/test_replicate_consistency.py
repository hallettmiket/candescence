"""Tests for replicate-consistency metrics."""

import math

import numpy as np
import pandas as pd
import pytest

from candescence.tlv.analysis.replicate_consistency import (
    replicate_consistency_summary,
    replicate_random_auroc,
    replicate_random_cohens_d,
)


class TestReplicateRandomAuroc:
    def test_perfect_separation_high_auroc(self):
        rep = np.array([0.1, 0.2, 0.15, 0.25])
        rand = np.array([1.0, 1.5, 2.0, 1.2])
        assert replicate_random_auroc(rep, rand) == pytest.approx(1.0)

    def test_inverted_returns_zero(self):
        # Replicate distances LARGER than random — model is anti-replicate
        rep = np.array([2.0, 3.0])
        rand = np.array([0.1, 0.2])
        assert replicate_random_auroc(rep, rand) == pytest.approx(0.0)

    def test_overlapping_around_half(self):
        rng = np.random.default_rng(0)
        rep = rng.normal(loc=1.0, scale=1.0, size=200)
        rand = rng.normal(loc=1.0, scale=1.0, size=200)
        auroc = replicate_random_auroc(rep, rand)
        assert 0.4 < auroc < 0.6

    def test_empty_returns_nan(self):
        assert math.isnan(replicate_random_auroc(np.array([]), np.array([1.0])))
        assert math.isnan(replicate_random_auroc(np.array([1.0]), np.array([])))

    def test_drops_nans(self):
        rep = np.array([0.1, np.nan, 0.2])
        rand = np.array([1.0, 1.5, np.nan])
        auroc = replicate_random_auroc(rep, rand)
        assert auroc == pytest.approx(1.0)


class TestReplicateRandomCohensD:
    def test_positive_when_replicates_closer(self):
        rep = np.array([0.1, 0.2, 0.15, 0.25])
        rand = np.array([1.0, 1.5, 2.0, 1.2])
        d = replicate_random_cohens_d(rep, rand)
        assert d > 1.0

    def test_zero_variance_returns_nan(self):
        # Both groups identical → pooled std = 0
        rep = np.array([0.5, 0.5, 0.5])
        rand = np.array([0.5, 0.5, 0.5])
        assert math.isnan(replicate_random_cohens_d(rep, rand))

    def test_too_few_samples_returns_nan(self):
        assert math.isnan(replicate_random_cohens_d(np.array([0.1]), np.array([1.0, 2.0])))
        assert math.isnan(replicate_random_cohens_d(np.array([0.1, 0.2]), np.array([1.0])))


class TestReplicateConsistencySummary:
    def test_per_layer_rows(self):
        rep_df = pd.DataFrame({
            "id_a": [1, 2], "id_b": [3, 4],
            "x0": [0.1, 0.2], "x1": [0.5, 0.6],
        })
        rand_df = pd.DataFrame({
            "id_a": [5, 6], "id_b": [7, 8],
            "x0": [1.0, 1.2], "x1": [1.5, 1.6],
        })
        summary = replicate_consistency_summary(rep_df, rand_df, layers=("x0", "x1"))
        assert list(summary["layer"]) == ["x0", "x1"]
        assert (summary["auroc"] > 0.9).all()
        assert (summary["cohens_d"] > 0).all()
        assert (summary["n_rep_pairs"] == 2).all()

    def test_skips_missing_layers(self):
        rep_df = pd.DataFrame({"x0": [0.1, 0.2]})
        rand_df = pd.DataFrame({"x0": [1.0, 1.2]})
        summary = replicate_consistency_summary(rep_df, rand_df, layers=("x0", "x1", "x2"))
        assert list(summary["layer"]) == ["x0"]
