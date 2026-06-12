"""Tests for candescence.tlv.analysis.skip_channel_stats (pure helpers)."""

import numpy as np
import pandas as pd
import pytest

from candescence.tlv.analysis.skip_channel_stats import (
    _channel_stats,
    _discretised_entropy,
)


class TestChannelStats:
    """Test per-channel statistics on synthetic skip tensors."""

    def test_output_shape_and_columns(self):
        skip = np.random.default_rng(0).standard_normal((10, 8, 4, 4)).astype(np.float32)
        df = _channel_stats(skip)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 8  # 8 channels
        assert set(df.columns) == {"mean", "variance", "spatial_entropy", "dead"}

    def test_constant_channel_is_dead(self):
        skip = np.zeros((5, 3, 2, 2), dtype=np.float32)
        # Channel 1 gets some variance
        skip[:, 1, :, :] = np.random.default_rng(1).standard_normal((5, 2, 2))
        df = _channel_stats(skip)
        assert df.loc[0, "dead"] is np.True_
        assert df.loc[2, "dead"] is np.True_
        assert df.loc[1, "dead"] is np.False_

    def test_variance_nonnegative(self):
        skip = np.random.default_rng(2).standard_normal((20, 16, 4, 4)).astype(np.float32)
        df = _channel_stats(skip)
        assert (df["variance"] >= 0).all()

    def test_entropy_nonnegative(self):
        skip = np.random.default_rng(3).standard_normal((10, 4, 3, 3)).astype(np.float32)
        df = _channel_stats(skip)
        assert (df["spatial_entropy"] >= 0).all()

    def test_mean_reasonable(self):
        rng = np.random.default_rng(4)
        skip = rng.standard_normal((100, 2, 4, 4)).astype(np.float32)
        df = _channel_stats(skip)
        # With 100 samples of standard normal, mean should be close to 0
        assert abs(df.loc[0, "mean"]) < 0.5
        assert abs(df.loc[1, "mean"]) < 0.5


class TestDiscretisedEntropy:
    def test_constant_zero_entropy(self):
        vals = np.ones(100)
        # All values in one bin → only one non-zero prob → H = 0
        h = _discretised_entropy(vals, n_bins=10)
        assert h == pytest.approx(0.0)

    def test_uniform_max_entropy(self):
        vals = np.linspace(0, 1, 1000)
        h = _discretised_entropy(vals, n_bins=10)
        # Uniform distribution across 10 bins → H ≈ log2(10)
        assert h == pytest.approx(np.log2(10), abs=0.15)

    def test_nonnegative(self):
        vals = np.random.default_rng(5).standard_normal(500)
        h = _discretised_entropy(vals)
        assert h >= 0
