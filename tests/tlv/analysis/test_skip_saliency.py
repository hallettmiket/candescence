"""Tests for candescence.tlv.analysis.skip_saliency (pure helpers)."""

import numpy as np
import pytest
import torch

from candescence.tlv.analysis.skip_saliency import _target_scalar


class TestTargetScalar:
    """Test the reduction function used for backpropagation targets."""

    def test_norm(self):
        t = torch.tensor([3.0, 4.0])
        s = _target_scalar(t, "norm")
        assert s.item() == pytest.approx(5.0)

    def test_mean(self):
        t = torch.tensor([2.0, 4.0, 6.0])
        s = _target_scalar(t, "mean")
        assert s.item() == pytest.approx(4.0)

    def test_default_is_norm(self):
        t = torch.tensor([3.0, 4.0])
        s = _target_scalar(t, "anything_else")
        assert s.item() == pytest.approx(5.0)

    def test_gradient_flows(self):
        """Ensure the scalar retains grad_fn for backprop."""
        t = torch.randn(8, requires_grad=True)
        s = _target_scalar(t, "norm")
        s.backward()
        assert t.grad is not None
        assert t.grad.shape == t.shape
