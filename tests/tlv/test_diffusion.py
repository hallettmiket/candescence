"""
Purpose: Lock in the ported TLV diffusion model (construction + inference).
Author: Hallett Lab
Date: 2026-06-12
Input: A synthetic small config (fast) and, if present, the real checkpoint.
Output: Pytest assertions.
"""

from __future__ import annotations

from pathlib import Path

import pytest

torch = pytest.importorskip("torch")
pytest.importorskip("omegaconf")
from omegaconf import OmegaConf

from candescence.tlv.diffusion import inference as diff_inf
from candescence.tlv.diffusion.loader import (
    LoadedDiffusionModel,
    _build_full_config,
    load_diffusion_model,
)
from candescence.core.settings import legacy_refined_root
from candescence.tlv.diffusion.model import TLVCondDiffVAE
from candescence.tlv.diffusion.schedule import build_schedule

_REAL_CKPT = (
    legacy_refined_root()
    / "candescence_master/projects/tlv/38-diffusion/DEBUG/models/ckpt_epoch_0075.pt"
)


def _tiny_loaded() -> LoadedDiffusionModel:
    """A small-channel diffusion model for fast inference tests."""
    cfg = OmegaConf.create({
        "data": {"input_channels": 1, "img_size": 128},
        # base channel must be divisible by 32 (the UNet's output GroupNorm).
        "model": {
            "encoder": {"latent_dim": 8, "channels": [32, 64, 128, 256], "use_norm": True},
            "unet": {"channels": [32, 64, 128, 256], "time_emb_dim": 64,
                     "sin_emb_dim": 32, "num_attn_heads": 2},
        },
        "diffusion": {"t_steps": 20, "beta_start": 1e-4, "beta_end": 2e-2,
                      "ddim_k": 3, "schedule_type": "linear"},
    })
    model = TLVCondDiffVAE(cfg).eval()
    dev = torch.device("cpu")
    betas, alphas, alpha_bars = build_schedule(cfg, dev)
    return LoadedDiffusionModel(model, cfg, dev, betas, alphas, alpha_bars, -1, "test")


def test_build_full_config_from_flat_bw_format() -> None:
    cfg = _build_full_config({"INPUT_CHANNELS": 1, "LATENT_DIM": 32, "T_STEPS": 500})
    assert cfg["data"]["input_channels"] == 1
    assert cfg["model"]["encoder"]["latent_dim"] == 32
    assert cfg["diffusion"]["t_steps"] == 500
    assert cfg["diffusion"]["ddim_k"] == 50


def test_construct_and_infer_shapes() -> None:
    loaded = _tiny_loaded()
    # generate
    imgs = diff_inf.generate(loaded, n=2, ddim_steps=3, seed=0)
    assert len(imgs) == 2 and imgs[0].shape == (128, 128)
    assert imgs[0].min() >= 0 and imgs[0].max() <= 1
    # encode
    z = diff_inf.encode(loaded, torch.randn(2, 1, 128, 128))
    assert z.shape == (2, 8)
    # interpolate
    seq = diff_inf.interpolate(
        loaded, torch.randn(1, 128, 128), torch.randn(1, 128, 128),
        n_steps=4, ddim_steps=3,
    )
    assert len(seq) == 4 and seq[0].shape == (128, 128)


@pytest.mark.skipif(not _REAL_CKPT.is_file(), reason="real diffusion checkpoint not present")
def test_load_real_checkpoint_and_generate() -> None:
    loaded = load_diffusion_model(str(_REAL_CKPT), device="cpu")
    assert loaded.input_channels == 1
    assert loaded.latent_dim == 32
    assert loaded.cfg.diffusion.t_steps == 500
    imgs = diff_inf.generate(loaded, n=1, ddim_steps=5, seed=0)
    assert imgs[0].shape == (128, 128)
