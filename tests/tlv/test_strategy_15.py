"""Tests for Strategy 15: per-variable FiLM Tendril VAE.

Covers the new ``multi_cond_tendril_VAE`` architecture, the dataset
dict-cond return path, and the factory/optimizer wiring that depends on
module names containing ``film`` for per-parameter learning rates.
"""

from __future__ import annotations

import sys
from types import SimpleNamespace
from typing import Dict

import pytest
import torch

from candescence.tlv.architectures.tendril_vae_multi_cond import (
    COND_KEYS,
    MultiCondDecoder,
    MultiCondEncoder,
    _MultiFiLM,
    multi_cond_tendril_VAE,
)
from candescence.tlv.data.dataset import _one_hot


# ── Fixtures ────────────────────────────────────────────────────────────


@pytest.fixture
def cond_dims() -> Dict[str, int]:
    # hue=1 (just average_hue selected), day=3 (2/5/NA), media=5
    return {"hue": 1, "day": 3, "media": 5}


@pytest.fixture
def arch_args(cond_dims) -> dict:
    return {
        "DEVICE": "cpu",
        "latent_dim": 32,
        "intermediate_dim": 64,
        "leaky_relu_slope": 0.02,
        "image_dimension": 128,
        "kernel_size": 3,
        "strategy": 15,
        "_cond_dim_hue": cond_dims["hue"],
        "_cond_dim_day": cond_dims["day"],
        "_cond_dim_media": cond_dims["media"],
    }


def _make_cond_batch(batch: int, cond_dims: Dict[str, int]) -> Dict[str, torch.Tensor]:
    return {k: torch.rand(batch, d) for k, d in cond_dims.items()}


# ── Architecture tests ─────────────────────────────────────────────────


def test_multi_film_module_shapes(cond_dims):
    """_MultiFiLM applies hue/day/media FiLM sequentially and preserves shape."""
    film = _MultiFiLM(feature_dim=16, cond_dims=cond_dims)
    x = torch.randn(4, 16, 8, 8)
    cond = _make_cond_batch(4, cond_dims)
    y = film(x, cond)
    assert y.shape == x.shape
    # Ensure each per-variable FiLM submodule exists.
    assert hasattr(film, "film_hue")
    assert hasattr(film, "film_day")
    assert hasattr(film, "film_media")


def test_vae_forward_end_to_end(arch_args, cond_dims):
    """Full forward: output image, z, mu, logvar, skip-connection list."""
    vae = multi_cond_tendril_VAE(arch_args)
    batch = 2
    x = torch.rand(batch, 3, 128, 128)
    cond = _make_cond_batch(batch, cond_dims)

    recon, z, mu, logvar, skips = vae(x, cond)

    assert recon.shape == x.shape
    assert z.shape == (batch, arch_args["latent_dim"])
    assert mu.shape == (batch, arch_args["latent_dim"])
    assert logvar.shape == (batch, arch_args["latent_dim"])
    assert len(skips) == 4
    # Channels at each skip level follow the encoder: 64, 128, 256, 512
    for skip, expected_c in zip(skips, [64, 128, 256, 512]):
        assert skip.shape[0] == batch
        assert skip.shape[1] == expected_c


def test_vae_accepts_separate_enc_dec_cond(arch_args, cond_dims):
    """Forward supports both ``cond`` and ``[enc_cond, dec_cond]`` forms."""
    vae = multi_cond_tendril_VAE(arch_args)
    x = torch.rand(2, 3, 128, 128)
    enc_cond = _make_cond_batch(2, cond_dims)
    dec_cond = _make_cond_batch(2, cond_dims)

    recon_shared, *_ = vae(x, enc_cond)
    recon_split, *_ = vae(x, [enc_cond, dec_cond])
    assert recon_shared.shape == recon_split.shape == x.shape


def test_film_parameters_are_named_for_optimizer_split(arch_args):
    """Factory's FiLM-vs-VAE optimizer split requires ``film`` in param names.

    See ``Factory.prepare_vae`` — we check ``'film' in name.lower()`` to
    route learning rates. Breakage here would mean Strategy 15 loses the
    fast FiLM learning rate.
    """
    vae = multi_cond_tendril_VAE(arch_args)
    film_names = [n for n, _ in vae.named_parameters() if "film" in n.lower()]
    # Per-variable FiLM modules × 4 encoder blocks + 4 decoder blocks = 24 modules.
    # Each FiLM module has gamma_layer + beta_layer, each with weight+bias.
    # Expect at least 4 parameters per module: 8 encoder + 8 decoder × 3 variables
    # × 2 sub-layers × 2 tensors = 96 tensors. Don't over-specify; just ensure a
    # substantial set is routed to FiLM.
    assert len(film_names) >= 8 * 3 * 4, (
        f"Expected many FiLM-tagged parameters, got {len(film_names)}"
    )


def test_encoder_and_decoder_accept_dict_conds(arch_args, cond_dims):
    """Encoder and decoder take cond dicts directly (no concatenation)."""
    enc = MultiCondEncoder(arch_args)
    # Lazy-init fc layers so we can call forward directly.
    dummy = torch.rand(1, 3, 128, 128)
    dummy_cond = _make_cond_batch(1, cond_dims)
    x1, x2, x3, x4 = enc.forward_conv(dummy, dummy_cond)
    enc.fc_mu = torch.nn.Linear(x4.numel(), arch_args["latent_dim"])
    enc.fc_logvar = torch.nn.Linear(x4.numel(), arch_args["latent_dim"])

    z, mu, logvar, skips = enc(dummy, dummy_cond)
    assert z.shape == (1, arch_args["latent_dim"])

    dec = MultiCondDecoder(arch_args)
    out = dec(z, skips, dummy_cond)
    assert out.shape == dummy.shape


# ── Dataset helper tests ───────────────────────────────────────────────


def test_one_hot_matches_string_categories():
    cats = ["2", "5", "NA"]
    assert torch.equal(_one_hot(2, cats), torch.tensor([1.0, 0.0, 0.0]))
    assert torch.equal(_one_hot("5", cats), torch.tensor([0.0, 1.0, 0.0]))
    assert torch.equal(_one_hot("NA", cats), torch.tensor([0.0, 0.0, 1.0]))


def test_one_hot_unknown_value_returns_zero_vector():
    cats = ["control", "spider", "ypd", "lees", "filament"]
    vec = _one_hot("unseen_medium", cats)
    assert vec.shape == (5,)
    assert torch.all(vec == 0.0)


# ── Trainer dict-cond plumbing ─────────────────────────────────────────


def test_batchify_and_move_cond_handles_dicts():
    """Trainer's dict helpers pass tensors through unchanged for dict-of-tensors conds."""
    # Avoid importing the full trainer (pulls in lpips / matplotlib). Import the
    # helper directly via its module. Skipped gracefully if optional deps absent.
    try:
        from candescence.tlv.training.trainer import _batchify_cond
    except Exception:  # pragma: no cover — optional deps
        pytest.skip("trainer module not importable in this environment")

    cond_1d = {
        "hue": torch.tensor([0.3]),
        "day": torch.tensor([1.0, 0.0, 0.0]),
        "media": torch.tensor([0.0, 1.0, 0.0, 0.0, 0.0]),
    }
    batched = _batchify_cond(cond_1d)
    assert batched["hue"].shape == (1, 1)
    assert batched["day"].shape == (1, 3)
    assert batched["media"].shape == (1, 5)


def test_trainer_set_decoder_conditions_dict_keeps_day_media(monkeypatch):
    """Only 'hue' is replaced by augmented lookup; day/media pass through."""
    try:
        from candescence.tlv.training.trainer import VAETrainer
    except Exception:
        pytest.skip("trainer module not importable in this environment")

    # Build a bare VAETrainer — avoid VAETrainer.__init__ (needs model etc.);
    # use object.__new__ + manually attach the method via unbound call.
    inst = object.__new__(VAETrainer)

    # Fake _set_decoder_conditions returns a 2x1 tensor of augmented hue.
    def fake_aug(indices, which="train"):
        return torch.tensor([[0.42], [0.84]])

    inst._set_decoder_conditions = fake_aug

    encoder_cond = {
        "hue": torch.tensor([[0.1], [0.2]]),
        "day": torch.tensor([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]]),
        "media": torch.tensor([[1.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 1.0, 0.0, 0.0]]),
    }
    indices = torch.tensor([0, 1])
    out = VAETrainer._set_decoder_conditions_dict(inst, encoder_cond, indices, "train")

    assert torch.allclose(out["hue"], torch.tensor([[0.42], [0.84]]))
    assert torch.allclose(out["day"], encoder_cond["day"])
    assert torch.allclose(out["media"], encoder_cond["media"])
