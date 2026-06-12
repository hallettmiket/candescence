"""
Purpose: Load a pretrained TLV conditional-diffusion checkpoint for inference.
Author: Hallett Lab (ported from Jose's 38-diffusion webapp model_service)
Date: 2026-06-12
Input: A checkpoint path (.pt) with keys {model, config, ...}.
Output: A LoadedDiffusionModel (model + diffusion schedule tensors), eval-ready.

Handles the two checkpoint config layouts seen on disk:
  - flat BW-1.0.0 notebook config (UPPERCASE keys: INPUT_CHANNELS, LATENT_DIM,
    T_STEPS) -> reconstructed into the full nested config with version-5
    architectural defaults;
  - the version-5 nested config (data/model/diffusion) -> used as-is.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Optional

import torch
from omegaconf import DictConfig, OmegaConf

from .model import TLVCondDiffVAE
from .schedule import build_schedule

# version-5 architectural defaults, used to rebuild minimal/flat configs.
_ENC_CHANNELS = [64, 128, 256, 512]
_UNET_CHANNELS = [64, 128, 256, 512]
_DEFAULT_IMG_SIZE = 128


def _build_full_config(raw: dict) -> dict:
    """Reconstruct a complete nested config from a checkpoint's raw config."""
    # Already the version-5 nested layout?
    if all(k in raw for k in ("data", "model", "diffusion")):
        cfg = OmegaConf.to_container(OmegaConf.create(raw), resolve=True)
        cfg.setdefault("diffusion", {})
        cfg["diffusion"].setdefault("ddim_k", 50)
        cfg["diffusion"].setdefault("schedule_type", "linear")
        cfg["diffusion"].setdefault("beta_start", 1e-4)
        cfg["diffusion"].setdefault("beta_end", 2e-2)
        return cfg

    # Flat BW-1.0.0 layout (accept upper- or lower-case keys).
    def pick(*names, default):
        for n in names:
            if n in raw:
                return raw[n]
        return default

    input_channels = pick("INPUT_CHANNELS", "input_channels", default=1)
    latent_dim = pick("LATENT_DIM", "latent_dim", default=32)
    t_steps = pick("T_STEPS", "t_steps", default=500)
    img_size = pick("IMG_SIZE", "img_size", default=_DEFAULT_IMG_SIZE)

    return {
        "data": {"input_channels": input_channels, "img_size": img_size},
        "model": {
            "encoder": {
                "latent_dim": latent_dim,
                "channels": _ENC_CHANNELS,
                "use_norm": True,
            },
            "unet": {
                "channels": _UNET_CHANNELS,
                "time_emb_dim": 256,
                "sin_emb_dim": 128,
                "num_attn_heads": 4,
            },
        },
        "diffusion": {
            "t_steps": t_steps,
            "beta_start": 1e-4,
            "beta_end": 2e-2,
            "ddim_k": 50,
            "schedule_type": "linear",
        },
    }


@dataclass
class LoadedDiffusionModel:
    """A pretrained diffusion model plus its precomputed diffusion schedule."""

    model: TLVCondDiffVAE
    cfg: DictConfig
    device: torch.device
    betas: torch.Tensor
    alphas: torch.Tensor
    alpha_bars: torch.Tensor
    epoch: int
    path: str

    @property
    def latent_dim(self) -> int:
        return int(self.cfg.model.encoder.latent_dim)

    @property
    def input_channels(self) -> int:
        return int(self.cfg.data.input_channels)

    @property
    def img_size(self) -> int:
        return int(self.cfg.data.img_size)


def load_diffusion_model(
    path: str, device: Optional[str] = None
) -> LoadedDiffusionModel:
    """
    Load a diffusion checkpoint and return an eval-ready :class:`LoadedDiffusionModel`.

    Prefers EMA weights when present and strips any ``_orig_mod.`` prefix left by
    ``torch.compile``.
    """
    dev = torch.device(
        device or ("cuda:0" if torch.cuda.is_available() else "cpu")
    )
    ckpt: dict[str, Any] = torch.load(path, map_location=dev, weights_only=False)

    raw_cfg = ckpt.get("config", {})
    if not isinstance(raw_cfg, dict):
        raw_cfg = OmegaConf.to_container(raw_cfg, resolve=True)
    cfg = OmegaConf.create(_build_full_config(raw_cfg))

    model = TLVCondDiffVAE(cfg).to(dev)

    ema = ckpt.get("ema")
    if isinstance(ema, dict) and ema and any(
        isinstance(v, torch.Tensor) for v in ema.values()
    ):
        state_dict = ema
    else:
        state_dict = ckpt["model"]
    if any(k.startswith("_orig_mod.") for k in state_dict):
        state_dict = {k.removeprefix("_orig_mod."): v for k, v in state_dict.items()}
    model.load_state_dict(state_dict)
    model.eval()

    betas, alphas, alpha_bars = build_schedule(cfg, dev)
    return LoadedDiffusionModel(
        model=model,
        cfg=cfg,
        device=dev,
        betas=betas,
        alphas=alphas,
        alpha_bars=alpha_bars,
        epoch=int(ckpt.get("epoch", -1)),
        path=str(path),
    )
