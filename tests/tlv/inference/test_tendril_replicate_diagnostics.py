"""Tests for tendril replicate diagnostics (no GPU required).

Uses a synthetic DataFrame and a mock VAE encoder that returns
deterministic skip features of known shape.
"""

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest
import torch
import torch.nn as nn

pytest.importorskip("psutil", reason="psutil required by candescence.tlv.utilities")

from candescence.tlv.inference.tendril_replicate_diagnostics import (
    PeakInfo,
    RankChangeStatsResult,
    ReplicateRankResult,
    build_nonrep_pairs,
    build_replicate_pairs,
    compute_intermediate_distances_for_pairs,
    compute_rank_change_stats,
    compute_replicate_rank_matrices_per_layer,
    inspect_distance_peaks_for_layer,
    plot_replicate_distance_distribution_per_layer,
    show_pair_reconstructions_grid,
)


# ── Mock encoder / VAE ────────────────────────────────────────────────

class _MockEncoder(nn.Module):
    """Encoder that returns fixed-shape skip features seeded by pixel sum."""

    def __init__(self, skip_channels=(32, 64, 128, 256), skip_hw=8):
        super().__init__()
        self.skip_channels = skip_channels
        self.skip_hw = skip_hw
        # Dummy parameter so .parameters() is non-empty
        self._dummy = nn.Parameter(torch.zeros(1))

    def forward(self, x, cond=None):
        B = x.shape[0]
        # Use pixel sum as a seed so different images → different skips
        seed = x.sum(dim=(1, 2, 3), keepdim=True)  # (B, 1, 1, 1)
        skips = []
        for c in self.skip_channels:
            feat = torch.randn(B, c, self.skip_hw, self.skip_hw) * 0.1 + seed
            skips.append(feat)
        z = torch.randn(B, 32)
        mu = torch.randn(B, 32)
        logvar = torch.zeros(B, 32)
        return z, mu, logvar, skips


class _MockVAE(nn.Module):
    def __init__(self):
        super().__init__()
        self.encoder = _MockEncoder()

    def forward(self, x, cond=None):
        return self.encoder(x, cond)


# ── Fixtures ──────────────────────────────────────────────────────────

@pytest.fixture
def mock_vae():
    return _MockVAE()


@pytest.fixture
def sample_df():
    """12 images in 6 replicate pairs, strategy 0 (no conditioning)."""
    n = 12
    rng = np.random.default_rng(42)
    imgs = [torch.randn(3, 32, 32) for _ in range(n)]
    df = pd.DataFrame({
        "id": list(range(n)),
        "my_rep": [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5],
        "transformed_image": imgs,
        "rgb_image": [np.random.randint(0, 255, (32, 32, 3), dtype=np.uint8) for _ in range(n)],
    })
    return df


# ── Tests: pair builders ──────────────────────────────────────────────

class TestBuildPairs:

    def test_replicate_pairs_count(self, sample_df):
        pairs = build_replicate_pairs(sample_df)
        assert len(pairs) == 6  # 6 size-2 groups

    def test_replicate_pairs_distinct_ids(self, sample_df):
        pairs = build_replicate_pairs(sample_df)
        for a, b in pairs:
            assert a != b

    def test_nonrep_pairs_different_rep(self, sample_df):
        pairs = build_nonrep_pairs(sample_df, n_pairs=10)
        reps = sample_df.set_index("id")["my_rep"]
        for a, b in pairs:
            assert reps[a] != reps[b]

    def test_nonrep_pairs_count_capped(self, sample_df):
        pairs = build_nonrep_pairs(sample_df, n_pairs=5)
        assert len(pairs) == 5


# ── Tests: intermediate distances ─────────────────────────────────────

class TestComputeIntermediateDistances:

    def test_output_shape(self, sample_df, mock_vae):
        pairs = build_replicate_pairs(sample_df)
        dist_df = compute_intermediate_distances_for_pairs(
            sample_df, mock_vae, pairs,
            strategy=0, pool_hw=4,
        )
        assert len(dist_df) == len(pairs)
        assert "id_a" in dist_df.columns
        assert "x0" in dist_df.columns
        assert "x3" in dist_df.columns

    def test_distances_finite(self, sample_df, mock_vae):
        pairs = build_replicate_pairs(sample_df)
        dist_df = compute_intermediate_distances_for_pairs(
            sample_df, mock_vae, pairs,
            strategy=0, pool_hw=4,
        )
        for layer in ("x0", "x1", "x2", "x3"):
            assert dist_df[layer].notna().all()
            assert (dist_df[layer] >= 0).all()

    def test_same_image_zero_distance(self, sample_df, mock_vae):
        """Pair of identical IDs should have distance ≈ 0."""
        pairs = [(0, 0)]
        dist_df = compute_intermediate_distances_for_pairs(
            sample_df, mock_vae, pairs,
            strategy=0, pool_hw=4,
        )
        for layer in ("x0", "x1", "x2", "x3"):
            assert dist_df[layer].iloc[0] == pytest.approx(0.0, abs=1e-5)


# ── Tests: rank matrices ─────────────────────────────────────────────

class TestReplicateRankMatrices:

    def test_rank_table_shape(self, sample_df, mock_vae):
        result = compute_replicate_rank_matrices_per_layer(
            sample_df, mock_vae, strategy=0, pool_hw=4,
        )
        assert isinstance(result, ReplicateRankResult)
        assert result.rank_table.shape == (6, 4)  # 6 pairs, 4 layers
        assert len(result.replicate_pairs) == 6

    def test_ranks_are_1_based(self, sample_df, mock_vae):
        result = compute_replicate_rank_matrices_per_layer(
            sample_df, mock_vae, strategy=0, pool_hw=4,
        )
        for col in result.rank_table.columns:
            vals = result.rank_table[col].dropna().values
            assert vals.min() >= 1
            assert vals.max() <= len(result.replicate_pairs)


# ── Tests: rank change stats ─────────────────────────────────────────

class TestRankChangeStats:

    def test_identical_ranks_zero_volatility(self):
        """Same rank in every layer → zero std/range."""
        rt = pd.DataFrame({
            "x0": [1.0, 2.0, 3.0],
            "x1": [1.0, 2.0, 3.0],
            "x2": [1.0, 2.0, 3.0],
        })
        result = compute_rank_change_stats(rt)
        assert isinstance(result, RankChangeStatsResult)
        assert result.volatility["mean_of_std_rank"] == pytest.approx(0.0)
        assert result.volatility["mean_of_range_rank"] == pytest.approx(0.0)

    def test_varying_ranks_nonzero_volatility(self):
        rt = pd.DataFrame({
            "x0": [1.0, 2.0, 3.0],
            "x1": [3.0, 1.0, 2.0],
        })
        result = compute_rank_change_stats(rt)
        assert result.volatility["mean_of_std_rank"] > 0
        assert result.volatility["mean_of_range_rank"] > 0

    def test_moves_table_columns(self):
        rt = pd.DataFrame({"x0": [1, 2], "x1": [2, 1], "x2": [1, 2]})
        result = compute_rank_change_stats(rt)
        assert len(result.moves) == 2  # two transitions
        assert "Mean |Δrank|" in result.moves.columns

    def test_per_pair_has_spearman(self):
        rt = pd.DataFrame({"a": [1, 2, 3, 4], "b": [2, 3, 4, 1], "c": [3, 4, 1, 2]})
        result = compute_rank_change_stats(rt)
        assert "spearman_trend" in result.per_pair.columns
        assert result.per_pair["spearman_trend"].notna().any()

    def test_stability_score_range(self):
        rt = pd.DataFrame({"x0": [1, 2, 3], "x1": [3, 1, 2]})
        result = compute_rank_change_stats(rt)
        scores = result.per_pair["stability_score"].values
        assert all(0 <= s <= 1 for s in scores)


# ── Tests: peak detection ─────────────────────────────────────────────

class TestInspectDistancePeaks:

    def test_finds_peaks_on_bimodal(self):
        """Bimodal distance distribution should yield 2 peaks."""
        rng = np.random.default_rng(0)
        n = 200
        # Two clusters of distances
        dists = np.concatenate([rng.normal(1.0, 0.2, n), rng.normal(3.0, 0.2, n)])
        ids_a = np.arange(len(dists))
        ids_b = np.arange(len(dists)) + 1000
        dist_df = pd.DataFrame({
            "id_a": ids_a, "id_b": ids_b, "x1": dists,
        })
        df = pd.DataFrame({
            "id": list(set(ids_a) | set(ids_b)),
            "rgb_image": [np.zeros((8, 8, 3), dtype=np.uint8)] * (len(set(ids_a) | set(ids_b))),
        })

        result = inspect_distance_peaks_for_layer(
            dist_df, df, layer="x1", bins=40, max_peaks=3,
        )
        assert "peaks" in result
        assert len(result["peaks"]) >= 2
        assert all(isinstance(p, PeakInfo) for p in result["peaks"])
        assert result["fig"] is not None
        plt.close(result["fig"])

    def test_empty_distances(self):
        dist_df = pd.DataFrame({"id_a": [], "id_b": [], "x1": []})
        df = pd.DataFrame({"id": [], "rgb_image": []})
        result = inspect_distance_peaks_for_layer(dist_df, df, layer="x1")
        assert len(result["peaks"]) == 0
        plt.close(result["fig"])


# ── Tests: plotting smoke tests ───────────────────────────────────────

class TestPlotSmoke:

    def test_distribution_plot_returns_figs(self, sample_df, mock_vae):
        pairs_rep = build_replicate_pairs(sample_df)
        pairs_non = build_nonrep_pairs(sample_df, n_pairs=6)
        dist_rep = compute_intermediate_distances_for_pairs(
            sample_df, mock_vae, pairs_rep, strategy=0, pool_hw=4,
        )
        dist_non = compute_intermediate_distances_for_pairs(
            sample_df, mock_vae, pairs_non, strategy=0, pool_hw=4,
        )
        figs = plot_replicate_distance_distribution_per_layer(
            dist_rep, dist_non, layers=("x0", "x1"),
        )
        assert "x0" in figs
        assert "x1" in figs
        for fig in figs.values():
            plt.close(fig)

    def test_pair_grid_returns_fig(self, sample_df, mock_vae):
        pairs = build_replicate_pairs(sample_df)
        dist_df = compute_intermediate_distances_for_pairs(
            sample_df, mock_vae, pairs, strategy=0, pool_hw=4,
        )
        fig = show_pair_reconstructions_grid(
            dist_df, sample_df, n_pairs=3,
        )
        assert fig is not None
        plt.close(fig)
