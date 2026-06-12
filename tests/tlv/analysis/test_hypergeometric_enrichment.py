"""Tests for hypergeometric enrichment analysis."""

import numpy as np
import pandas as pd
import pytest

from candescence.tlv.analysis.hypergeometric_enrichment import (
    adjust_pvalues_bh,
    compute_hypergeometric_enrichment,
    summarize_cluster_enrichment,
)


class TestComputeHypergeometricEnrichment:
    """Tests for the core enrichment function."""

    def test_perfect_enrichment(self):
        """Cluster 0 contains only 'white', cluster 1 only 'hyphae'."""
        labels = np.array(["white"] * 50 + ["hyphae"] * 50)
        clusters = np.array([0] * 50 + [1] * 50)
        df = compute_hypergeometric_enrichment(labels, clusters)

        assert len(df) > 0
        # white in cluster 0 should be highly significant
        row = df[(df["cluster"] == 0) & (df["label"] == "white")]
        assert len(row) == 1
        assert row.iloc[0]["k"] == 50
        assert row.iloc[0]["enrichment_ratio"] == 2.0
        assert row.iloc[0]["q_value"] < 1e-10

    def test_random_labels_mostly_nonsignificant(self):
        """Random assignment should yield mostly non-significant results."""
        rng = np.random.default_rng(42)
        labels = rng.choice(["white", "opaque", "gray", "shmoo"], size=200)
        clusters = rng.choice([0, 1, 2, 3], size=200)
        df = compute_hypergeometric_enrichment(labels, clusters)

        significant = df[df["q_value"] < 0.05]
        # With random data, most should be non-significant
        assert len(significant) / len(df) < 0.2

    def test_missing_labels_become_unlabelled(self):
        """NaN and None labels are mapped to 'unlabelled'."""
        labels = np.array(["white", None, "hyphae", np.nan, "white"], dtype=object)
        clusters = np.array([0, 0, 1, 1, 1])
        df = compute_hypergeometric_enrichment(labels, clusters)

        assert "unlabelled" in df["label"].values

    def test_exclude_unlabelled(self):
        """When exclude_unlabelled=True, unlabelled rows are dropped."""
        labels = np.array(["white", None, "hyphae", "unlabelled", "white"], dtype=object)
        clusters = np.array([0, 0, 1, 1, 1])
        df = compute_hypergeometric_enrichment(
            labels, clusters, exclude_unlabelled=True
        )

        assert "unlabelled" not in df["label"].values
        # N should reflect excluded rows
        if len(df) > 0:
            assert df.iloc[0]["N"] == 3  # only white, hyphae, white

    def test_single_class_returns_empty(self):
        """If all labels are the same, return empty (need >= 2 classes)."""
        labels = np.array(["white"] * 20)
        clusters = np.array([0] * 10 + [1] * 10)
        df = compute_hypergeometric_enrichment(labels, clusters)

        assert len(df) == 0

    def test_min_count_filter(self):
        """Rows with k < min_count should be excluded."""
        labels = np.array(["white"] * 49 + ["hyphae"] * 1 + ["white"] * 50)
        clusters = np.array([0] * 50 + [1] * 50)
        df = compute_hypergeometric_enrichment(labels, clusters, min_count=3)

        # hyphae appears only once in cluster 0, should be filtered
        hyphae_rows = df[df["label"] == "hyphae"]
        for _, row in hyphae_rows.iterrows():
            assert row["k"] >= 3

    def test_length_mismatch_raises(self):
        """Mismatched array lengths should raise ValueError."""
        with pytest.raises(ValueError, match="same length"):
            compute_hypergeometric_enrichment(
                np.array(["a", "b"]), np.array([0, 1, 2])
            )

    def test_output_columns(self):
        """Verify the output DataFrame has all expected columns."""
        labels = np.array(["white", "hyphae"] * 25)
        clusters = np.array([0, 1] * 25)
        df = compute_hypergeometric_enrichment(labels, clusters)

        expected_cols = {
            "cluster", "label", "N", "K", "n", "k",
            "expected", "enrichment_ratio", "p_value", "q_value",
        }
        assert set(df.columns) == expected_cols

    def test_enrichment_ratio_correctness(self):
        """Verify enrichment_ratio = k / expected."""
        labels = np.array(["white"] * 30 + ["hyphae"] * 20 + ["white"] * 20 + ["hyphae"] * 30)
        clusters = np.array([0] * 50 + [1] * 50)
        df = compute_hypergeometric_enrichment(labels, clusters)

        for _, row in df.iterrows():
            if row["expected"] > 0:
                assert abs(row["enrichment_ratio"] - row["k"] / row["expected"]) < 0.01


class TestAdjustPvaluesBH:
    """Tests for Benjamini-Hochberg correction."""

    def test_single_pvalue(self):
        q = adjust_pvalues_bh(np.array([0.03]))
        assert len(q) == 1
        assert q[0] == pytest.approx(0.03)

    def test_monotonicity(self):
        """Adjusted p-values should be monotone w.r.t. raw p-values."""
        pvals = np.array([0.001, 0.01, 0.05, 0.1, 0.5])
        q = adjust_pvalues_bh(pvals)
        # Sorted raw -> sorted adjusted
        order = np.argsort(pvals)
        assert all(q[order[i]] <= q[order[i + 1]] for i in range(len(q) - 1))

    def test_never_exceeds_one(self):
        q = adjust_pvalues_bh(np.array([0.8, 0.9, 0.95, 0.99]))
        assert all(q <= 1.0)

    def test_empty_input(self):
        q = adjust_pvalues_bh(np.array([]))
        assert len(q) == 0

    def test_known_values(self):
        """Verify against hand-computed BH adjustment."""
        # 4 tests: raw p = [0.01, 0.04, 0.03, 0.20]
        # Sorted: [0.01, 0.03, 0.04, 0.20] at ranks 1,2,3,4
        # BH: [0.01*4/1, 0.03*4/2, 0.04*4/3, 0.20*4/4] = [0.04, 0.06, 0.053, 0.20]
        # After monotonicity enforcement: [0.04, 0.053, 0.053, 0.20]
        pvals = np.array([0.01, 0.04, 0.03, 0.20])
        q = adjust_pvalues_bh(pvals)
        assert q[0] == pytest.approx(0.04, abs=0.001)
        assert q[3] == pytest.approx(0.20, abs=0.001)


class TestSummarizeClusterEnrichment:
    """Tests for the per-cluster summary."""

    def test_summary_picks_top_enriched(self):
        labels = np.array(["white"] * 40 + ["hyphae"] * 10 + ["hyphae"] * 40 + ["white"] * 10)
        clusters = np.array([0] * 50 + [1] * 50)
        full_df = compute_hypergeometric_enrichment(labels, clusters)
        summary = summarize_cluster_enrichment(full_df)

        assert len(summary) == 2
        cl0 = summary[summary["cluster"] == 0].iloc[0]
        cl1 = summary[summary["cluster"] == 1].iloc[0]
        assert cl0["top_label"] == "white"
        assert cl1["top_label"] == "hyphae"
        assert cl0["significant"] is True or cl0["significant"] == True
        assert cl1["significant"] is True or cl1["significant"] == True

    def test_empty_input(self):
        empty = pd.DataFrame(columns=[
            "cluster", "label", "N", "K", "n", "k",
            "expected", "enrichment_ratio", "p_value", "q_value",
        ])
        summary = summarize_cluster_enrichment(empty)
        assert len(summary) == 0

    def test_significance_threshold(self):
        """With a very strict threshold, nothing should be significant."""
        labels = np.array(["white"] * 30 + ["hyphae"] * 20 + ["white"] * 20 + ["hyphae"] * 30)
        clusters = np.array([0] * 50 + [1] * 50)
        full_df = compute_hypergeometric_enrichment(labels, clusters)
        summary = summarize_cluster_enrichment(full_df, significance_threshold=1e-50)

        assert all(~summary["significant"])
