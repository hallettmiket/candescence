"""Tests for rank-change analysis across latent spaces."""

import numpy as np
import pandas as pd
import pytest

from candescence.tlv.analysis.tendril_rank_change import (
    RankChangeResult,
    pairwise_distances_for_index_pairs,
    rank_change_across_spaces,
    ranks_from_distances,
)


class TestPairwiseDistancesForIndexPairs:
    """Tests for pairwise_distances_for_index_pairs()."""

    def test_euclidean_basic(self):
        X = np.array([[0.0, 0.0], [3.0, 4.0], [1.0, 0.0]])
        pairs = np.array([[0, 1], [0, 2]])
        dists = pairwise_distances_for_index_pairs(X, pairs)
        np.testing.assert_allclose(dists, [5.0, 1.0])

    def test_invalid_shape_raises(self):
        X = np.eye(3)
        with pytest.raises(ValueError, match="\\(P, 2\\)"):
            pairwise_distances_for_index_pairs(X, np.array([0, 1, 2]))


class TestRanksFromDistances:
    """Tests for ranks_from_distances()."""

    def test_ascending_distances(self):
        d = np.array([1.0, 2.0, 3.0])
        ranks = ranks_from_distances(d)
        np.testing.assert_array_equal(ranks, [1, 2, 3])

    def test_descending_distances(self):
        d = np.array([3.0, 2.0, 1.0])
        ranks = ranks_from_distances(d)
        np.testing.assert_array_equal(ranks, [3, 2, 1])

    def test_1based(self):
        d = np.array([10.0, 5.0])
        ranks = ranks_from_distances(d)
        assert ranks.min() == 1
        assert ranks.max() == 2


class TestRankChangeAcrossSpaces:
    """Tests for rank_change_across_spaces()."""

    def test_identical_spaces_zero_variance(self):
        """Same embeddings in both spaces → rank variance should be 0."""
        rng = np.random.default_rng(0)
        X = rng.normal(size=(50, 4))
        result = rank_change_across_spaces(
            {'space_a': X, 'space_b': X.copy()},
            n_pairs=100,
        )
        assert isinstance(result, RankChangeResult)
        assert result.overall['mean_rank_var'] == pytest.approx(0.0)
        assert result.overall['mean_rank_range'] == pytest.approx(0.0)
        assert len(result.moves) == 1  # one transition
        assert result.moves['Mean |Δrank|'].iloc[0] == pytest.approx(0.0)

    def test_different_spaces_nonzero_variance(self):
        """Two independent random spaces → nonzero rank variance."""
        rng = np.random.default_rng(1)
        n = 30
        # Two completely independent embeddings
        X_a = rng.normal(size=(n, 4))
        X_b = rng.normal(size=(n, 4))

        result = rank_change_across_spaces(
            {'space_a': X_a, 'space_b': X_b},
            n_pairs=200,
            random_state=42,
        )
        assert result.overall['mean_rank_var'] > 0
        assert result.overall['mean_rank_range'] > 0
        assert result.moves['Mean |Δrank|'].iloc[0] > 0

    def test_output_shapes(self):
        rng = np.random.default_rng(2)
        n, n_pairs = 40, 150
        spaces = {
            'primary': rng.normal(size=(n, 5)),
            'tendril_0': rng.normal(size=(n, 3)),
            'tendril_1': rng.normal(size=(n, 4)),
        }
        result = rank_change_across_spaces(spaces, n_pairs=n_pairs)

        assert result.rank_table.shape == (n_pairs, 3)
        assert list(result.rank_table.columns) == ['primary', 'tendril_0', 'tendril_1']
        assert len(result.pair_summary) == n_pairs
        assert len(result.moves) == 2  # 2 transitions for 3 spaces
        assert result.overall['n_pairs'] == n_pairs
        assert result.overall['n_spaces'] == 3

    def test_explicit_pairs(self):
        rng = np.random.default_rng(3)
        X = rng.normal(size=(20, 3))
        pairs = np.array([[0, 1], [2, 3], [4, 5]])
        result = rank_change_across_spaces(
            {'a': X, 'b': X * 2},
            pairs=pairs,
        )
        assert result.overall['n_pairs'] == 3

    def test_single_space_raises(self):
        X = np.random.default_rng(4).normal(size=(10, 2))
        with pytest.raises(ValueError, match="at least 2 spaces"):
            rank_change_across_spaces({'only': X})

    def test_mismatched_N_raises(self):
        with pytest.raises(ValueError, match="same number of rows"):
            rank_change_across_spaces({
                'a': np.zeros((10, 2)),
                'b': np.zeros((15, 2)),
            })

    def test_pair_summary_has_delta_columns(self):
        rng = np.random.default_rng(5)
        n = 20
        spaces = {
            's1': rng.normal(size=(n, 3)),
            's2': rng.normal(size=(n, 3)),
            's3': rng.normal(size=(n, 3)),
        }
        result = rank_change_across_spaces(spaces, n_pairs=50)

        assert 'delta_s1_to_s2' in result.pair_summary.columns
        assert 'delta_s2_to_s3' in result.pair_summary.columns
        assert 'rank_var' in result.pair_summary.columns
        assert 'mean_abs_delta' in result.pair_summary.columns

    def test_moves_columns(self):
        rng = np.random.default_rng(6)
        n = 20
        spaces = {'a': rng.normal(size=(n, 2)), 'b': rng.normal(size=(n, 2))}
        result = rank_change_across_spaces(spaces, n_pairs=50)

        expected_cols = {'transition', 'Mean |Δrank|', 'Median |Δrank|',
                         'Std |Δrank|', 'Mean Δrank'}
        assert set(result.moves.columns) == expected_cols
