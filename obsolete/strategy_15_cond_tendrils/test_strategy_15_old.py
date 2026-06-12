"""Tests for Strategy 15: FiLM-conditioned tendril sub-VAEs."""

import json
import tempfile
from pathlib import Path

import numpy as np
import pytest
import torch

from candescence.tlv.architectures.tendril_sub import (
    CondTendrilLayerVAE,
    CondTendrils,
    CondTendrilTrainer,
    TendrilLayerVAE,
    Tendrils,
)
from candescence.tlv.losses import get_kl_mse_loss_tendril


# ── Fixtures ────────────────────────────────────────────────────────────

@pytest.fixture
def input_shape():
    return (64, 16, 16)  # channels, height, width


@pytest.fixture
def latent_dim():
    return 32


@pytest.fixture
def cond_dim():
    return 3  # H, S, V


@pytest.fixture
def batch_size():
    return 4


@pytest.fixture
def device():
    return torch.device("cpu")


@pytest.fixture
def tendril_args(device):
    return {
        "DEVICE": device,
        "MODELS": "/tmp/test_models",
        "tendril_batch_size": 4,
        "tendril_lr": 1e-4,
        "tendril_weight_decay": 0,
        "tendril_latent_dim": 32,
        "tendril_num_epochs": 2,
        "tendril_loss_fn": get_kl_mse_loss_tendril,
        "tendril_MSE_weight": 1.0,
        "tendril_KL_weight": 1.0,
    }


# ── CondTendrilLayerVAE unit tests ──────────────────────────────────────

class TestCondTendrilLayerVAE:

    def test_forward_shapes(self, input_shape, latent_dim, cond_dim, batch_size):
        model = CondTendrilLayerVAE(input_shape, latent_dim, cond_dim)
        x = torch.randn(batch_size, *input_shape)
        cond = torch.randn(batch_size, cond_dim)

        x_hat, mu, logvar, z = model(x, cond)

        assert x_hat.shape == (batch_size, *input_shape)
        assert mu.shape == (batch_size, latent_dim)
        assert logvar.shape == (batch_size, latent_dim)
        assert z.shape == (batch_size, latent_dim)

    def test_cond_gradient_flows(self, input_shape, latent_dim, cond_dim, batch_size):
        """Verify that conditioning actually affects output (gradient check)."""
        model = CondTendrilLayerVAE(input_shape, latent_dim, cond_dim)
        x = torch.randn(batch_size, *input_shape)
        cond = torch.randn(batch_size, cond_dim, requires_grad=True)

        x_hat, mu, logvar, z = model(x, cond)
        loss = x_hat.sum()
        loss.backward()

        assert cond.grad is not None
        assert cond.grad.abs().sum() > 0, "cond has no gradient — FiLM is not connected"

    def test_different_cond_produces_different_output(self, input_shape, latent_dim, cond_dim):
        model = CondTendrilLayerVAE(input_shape, latent_dim, cond_dim)
        model.eval()

        x = torch.randn(1, *input_shape)
        cond_a = torch.zeros(1, cond_dim)
        cond_b = torch.ones(1, cond_dim)

        with torch.no_grad():
            out_a, _, _, _ = model(x, cond_a)
            out_b, _, _, _ = model(x, cond_b)

        assert not torch.allclose(out_a, out_b, atol=1e-6), \
            "Different conditioning should produce different outputs"

    def test_various_input_shapes(self, latent_dim, cond_dim, batch_size):
        """Verify the model works with different feature map sizes."""
        for shape in [(32, 8, 8), (128, 16, 16), (256, 32, 32)]:
            model = CondTendrilLayerVAE(shape, latent_dim, cond_dim)
            x = torch.randn(batch_size, *shape)
            cond = torch.randn(batch_size, cond_dim)
            x_hat, mu, logvar, z = model(x, cond)
            assert x_hat.shape == (batch_size, *shape)


# ── CondTendrils container tests ────────────────────────────────────────

class TestCondTendrils:

    def test_add_and_list(self, tendril_args, input_shape, cond_dim):
        tendrils = CondTendrils(arguments=tendril_args)
        n = 16
        train_data = torch.randn(n, *input_shape)
        val_data = torch.randn(n, *input_shape)
        train_cond = torch.randn(n, cond_dim)
        val_cond = torch.randn(n, cond_dim)

        tendrils.add_tendril("0", train_data, val_data, train_cond, val_cond)
        assert "0" in tendrils.list_tendrils()

    def test_train_loss_finite(self, tendril_args, input_shape, cond_dim):
        """Train for 2 epochs, verify loss is finite."""
        tendrils = CondTendrils(arguments=tendril_args)
        n = 16
        train_data = torch.randn(n, *input_shape)
        val_data = torch.randn(n, *input_shape)
        train_cond = torch.randn(n, cond_dim)
        val_cond = torch.randn(n, cond_dim)

        tendrils.add_tendril("layer0", train_data, val_data, train_cond, val_cond)
        tendrils.train_tendril("layer0")

        trainer = tendrils.get_tendril("layer0")
        assert trainer.final_loss is not None
        assert np.isfinite(trainer.final_loss)


# ── CondTendrilTrainer tests ────────────────────────────────────────────

class TestCondTendrilTrainer:

    def test_training_runs(self, tendril_args, input_shape, cond_dim, device):
        n = 16
        model = CondTendrilLayerVAE(input_shape, 32, cond_dim)
        train_ds = torch.utils.data.TensorDataset(
            torch.randn(n, *input_shape), torch.randn(n, cond_dim)
        )
        val_ds = torch.utils.data.TensorDataset(
            torch.randn(n, *input_shape), torch.randn(n, cond_dim)
        )
        train_dl = torch.utils.data.DataLoader(train_ds, batch_size=4)
        val_dl = torch.utils.data.DataLoader(val_ds, batch_size=4)

        trainer = CondTendrilTrainer(model, train_dl, val_dl, tendril_args)
        trainer.train("test_key")
        assert trainer.final_loss is not None


# ── Serialization round-trip ────────────────────────────────────────────

class TestCondTendrilSerialization:

    def test_save_load_roundtrip(self, tendril_args, input_shape, cond_dim, device):
        tendrils = CondTendrils(arguments=tendril_args)
        n = 16
        train_data = torch.randn(n, *input_shape)
        val_data = torch.randn(n, *input_shape)
        train_cond = torch.randn(n, cond_dim)
        val_cond = torch.randn(n, cond_dim)

        tendrils.add_tendril("0", train_data, val_data, train_cond, val_cond)
        tendrils.train_tendril("0")

        with tempfile.TemporaryDirectory() as tmpdir:
            save_path = Path(tmpdir) / "tendrils"
            tendrils.save(save_path)

            # Verify metadata contains conditioned flag
            with open(save_path / "tendrils_meta.json") as f:
                meta = json.load(f)
            assert meta["0"]["conditioned"] is True
            assert meta["0"]["cond_dim"] == cond_dim

            # Load and compare
            loaded = CondTendrils.load(save_path, device)
            assert loaded.list_tendrils() == ["0"]

            # Forward pass should produce same output
            test_input = torch.randn(1, *input_shape)
            test_cond = torch.randn(1, cond_dim)

            tendrils.get_tendril("0").model.eval()
            loaded.get_tendril("0").model.eval()

            with torch.no_grad():
                # Compare mu (deterministic) — x_hat differs due to stochastic reparameterize
                _, orig_mu, _, _ = tendrils.get_tendril("0").model(test_input, test_cond)
                _, load_mu, _, _ = loaded.get_tendril("0").model(test_input, test_cond)

            assert torch.allclose(orig_mu, load_mu, atol=1e-5)


# ── Dual cond (encoder/decoder split) ─────────────────────────────────

class TestDualCond:
    """Strategy 15 encoder/decoder FiLM conditioning mismatch."""

    def test_dual_cond_forward_differs_from_single(self, input_shape, latent_dim, cond_dim):
        """When cond_dec != cond_enc, output should differ from single-cond."""
        model = CondTendrilLayerVAE(input_shape, latent_dim, cond_dim)
        model.eval()

        x = torch.randn(1, *input_shape)
        cond_enc = torch.zeros(1, cond_dim)
        cond_dec = torch.ones(1, cond_dim)

        with torch.no_grad():
            out_single, mu_s, _, _ = model(x, cond_enc)  # cond_dec defaults to cond_enc
            out_dual, mu_d, _, _ = model(x, cond_enc, cond_dec)

        # Encoder path is identical → mu should match
        assert torch.allclose(mu_s, mu_d, atol=1e-5), \
            "Same encoder cond should produce same mu"
        # Decoder path differs → x_hat should NOT match
        assert not torch.allclose(out_single, out_dual, atol=1e-6), \
            "Different decoder cond should produce different reconstructions"

    def test_single_cond_backward_compat(self, input_shape, latent_dim, cond_dim, batch_size):
        """model(x, cond) still works (positional, no cond_dec)."""
        model = CondTendrilLayerVAE(input_shape, latent_dim, cond_dim)
        x = torch.randn(batch_size, *input_shape)
        cond = torch.randn(batch_size, cond_dim)

        x_hat, mu, logvar, z = model(x, cond)
        assert x_hat.shape == (batch_size, *input_shape)

    def test_cond_dec_none_equals_cond_enc(self, input_shape, latent_dim, cond_dim):
        """Passing cond_dec=None should produce the same encoder output (mu)."""
        model = CondTendrilLayerVAE(input_shape, latent_dim, cond_dim)
        model.eval()
        x = torch.randn(1, *input_shape)
        cond = torch.randn(1, cond_dim)

        with torch.no_grad():
            _, mu_none, logvar_none, _ = model(x, cond, cond_dec=None)
            _, mu_same, logvar_same, _ = model(x, cond, cond_dec=cond)

        # Encoder path is identical in both cases
        assert torch.allclose(mu_none, mu_same, atol=1e-6)
        assert torch.allclose(logvar_none, logvar_same, atol=1e-6)


class TestCondTendrilsDualCond:
    """CondTendrils container with decoder jitter conditioning."""

    def test_add_tendril_with_cond_dec(self, tendril_args, input_shape, cond_dim):
        tendrils = CondTendrils(arguments=tendril_args)
        n = 16
        train_data = torch.randn(n, *input_shape)
        val_data = torch.randn(n, *input_shape)
        train_cond = torch.randn(n, cond_dim)
        val_cond = torch.randn(n, cond_dim)
        train_cond_dec = torch.randn(n, cond_dim)
        val_cond_dec = torch.randn(n, cond_dim)

        tendrils.add_tendril(
            "0", train_data, val_data, train_cond, val_cond,
            train_cond_dec=train_cond_dec, val_cond_dec=val_cond_dec,
        )
        assert "0" in tendrils.list_tendrils()

    def test_train_with_cond_dec_loss_finite(self, tendril_args, input_shape, cond_dim):
        """Train 2 epochs with separate encoder/decoder cond — loss should be finite."""
        tendrils = CondTendrils(arguments=tendril_args)
        n = 16
        train_data = torch.randn(n, *input_shape)
        val_data = torch.randn(n, *input_shape)
        train_cond = torch.randn(n, cond_dim)
        val_cond = torch.randn(n, cond_dim)
        train_cond_dec = torch.randn(n, cond_dim)  # different from enc
        val_cond_dec = torch.randn(n, cond_dim)

        tendrils.add_tendril(
            "layer0", train_data, val_data, train_cond, val_cond,
            train_cond_dec=train_cond_dec, val_cond_dec=val_cond_dec,
        )
        tendrils.train_tendril("layer0")

        trainer = tendrils.get_tendril("layer0")
        assert trainer.final_loss is not None
        assert np.isfinite(trainer.final_loss)

    def test_dataloader_yields_3_tuple(self, tendril_args, input_shape, cond_dim):
        """DataLoader should yield (skip, cond_enc, cond_dec) tuples."""
        tendrils = CondTendrils(arguments=tendril_args)
        n = 8
        train_data = torch.randn(n, *input_shape)
        val_data = torch.randn(n, *input_shape)
        train_cond = torch.randn(n, cond_dim)
        val_cond = torch.randn(n, cond_dim)
        train_cond_dec = torch.randn(n, cond_dim)
        val_cond_dec = torch.randn(n, cond_dim)

        tendrils.add_tendril(
            "0", train_data, val_data, train_cond, val_cond,
            train_cond_dec=train_cond_dec, val_cond_dec=val_cond_dec,
        )
        trainer = tendrils.get_tendril("0")
        batch = next(iter(trainer.train_dataloader))
        assert len(batch) == 3, f"Expected 3-tuple, got {len(batch)}-tuple"


# ── Backward compatibility ─────────────────────────────────────────────

class TestBackwardCompatibility:
    """Ensure existing TendrilLayerVAE/Tendrils (Strategy 14) still work."""

    def test_unconditioned_tendril_forward(self, input_shape, latent_dim, batch_size):
        model = TendrilLayerVAE(input_shape, latent_dim)
        x = torch.randn(batch_size, *input_shape)
        x_hat, mu, logvar, z = model(x)

        assert x_hat.shape == (batch_size, *input_shape)
        assert mu.shape == (batch_size, latent_dim)

    def test_unconditioned_tendrils_container(self, input_shape):
        args = {
            "DEVICE": torch.device("cpu"),
            "tendril_batch_size": 4,
            "tendril_lr": 1e-4,
            "tendril_weight_decay": 0,
            "tendril_latent_dim": 32,
            "tendril_num_epochs": 1,
            "tendril_loss_fn": get_kl_mse_loss_tendril,
            "tendril_MSE_weight": 1.0,
            "tendril_KL_weight": 1.0,
        }
        tendrils = Tendrils(arguments=args)
        n = 8
        tendrils.add_tendril("0", torch.randn(n, *input_shape), torch.randn(n, *input_shape))
        tendrils.train_tendril("0")
        assert tendrils.get_tendril("0").final_loss is not None
