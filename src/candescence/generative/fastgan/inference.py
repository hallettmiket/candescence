"""
Purpose: In-process inference for the Varasana FastGAN — load a trained generator
         and synthesise single-cell images by sampling / interpolating in the
         latent space.
Author: Hallett Lab
Date: 2026-06-16
Input: A FastGAN checkpoint (``{g, d}`` from the published model) + a latent dim.
Output: PIL images of synthetic Candida cells.

Runs natively in the main environment (GPU when available) — no legacy worker.
The 256x256 generator output is in the tanh range; helpers map it to [0, 1].
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Union

import torch
import torchvision.transforms.functional as TF
from PIL import Image

from candescence.core.logging_config import get_logger
from candescence.generative.fastgan.models import Generator

logger = get_logger("candescence.generative.fastgan.inference")

PathLike = Union[str, Path]


@dataclass
class LoadedGenerator:
    """A loaded FastGAN generator ready for sampling."""

    model: Generator
    nz: int
    im_size: int
    device: torch.device


def _resolve_device(device: Optional[str]) -> torch.device:
    return torch.device(device or ("cuda:0" if torch.cuda.is_available() else "cpu"))


def _load_ema_params(model: Generator, params) -> None:
    """Copy a list of EMA parameter tensors into the model (FastGAN ``g_ema``)."""
    for p, new_p in zip(model.parameters(), params):
        p.data.copy_(new_p.to(p.device))


def load_generator(
    checkpoint: PathLike,
    *,
    nz: int = 256,
    im_size: int = 256,
    ngf: int = 64,
    nc: int = 3,
    device: Optional[str] = None,
    use_ema: bool = False,
) -> LoadedGenerator:
    """Load a FastGAN generator checkpoint for inference.

    Parameters
    ----------
    checkpoint:
        Path to the ``.pth`` checkpoint. Accepts the published ``{g, d}`` dict, a
        full ``{g, d, g_ema, ...}`` dict, or a bare generator ``state_dict``.
    nz, im_size, ngf, nc:
        Generator hyperparameters (defaults match the published Varasana model).
    use_ema:
        If the checkpoint carries ``g_ema`` weights, copy those in (often smoother
        samples than the raw generator).
    """
    dev = _resolve_device(device)
    ckpt = torch.load(checkpoint, map_location=dev, weights_only=False)

    model = Generator(ngf=ngf, nz=nz, nc=nc, im_size=im_size).to(dev)
    state = ckpt["g"] if isinstance(ckpt, dict) and "g" in ckpt else ckpt
    model.load_state_dict(state)
    if use_ema and isinstance(ckpt, dict) and ckpt.get("g_ema") is not None:
        _load_ema_params(model, ckpt["g_ema"])
    model.eval()
    logger.info("loaded FastGAN generator (nz=%d, im_size=%d) on %s", nz, im_size, dev)
    return LoadedGenerator(model=model, nz=nz, im_size=im_size, device=dev)


def _to_pil(image: torch.Tensor) -> Image.Image:
    """Map one ``(C, H, W)`` tanh-range tensor to a PIL image in [0, 1]."""
    return TF.to_pil_image(((image + 1) / 2).clamp(0, 1).cpu())


def _seed_rng(seed: Optional[int], device: torch.device) -> None:
    """Seed the global RNG so a run is reproducible.

    FastGAN's per-layer ``NoiseInjection`` draws from the global RNG during the
    forward pass, so the latent vector alone does not fix the image — seeding the
    global RNG here makes the same ``seed`` reproduce the same sample.
    """
    if seed is None:
        return
    torch.manual_seed(int(seed))
    if device.type == "cuda":
        torch.cuda.manual_seed_all(int(seed))


@torch.no_grad()
def sample(
    loaded: LoadedGenerator,
    n: int,
    *,
    seed: Optional[int] = None,
    batch_size: int = 16,
) -> List[Image.Image]:
    """Generate ``n`` synthetic cell images from random latent vectors.

    Passing ``seed`` makes the whole batch reproducible (latent + injected noise).
    """
    _seed_rng(seed, loaded.device)
    z = torch.randn(n, loaded.nz, device=loaded.device)
    images: List[Image.Image] = []
    for start in range(0, n, batch_size):
        out = loaded.model(z[start:start + batch_size])[0]  # (b, C, im, im)
        images.extend(_to_pil(t) for t in out)
    return images


@torch.no_grad()
def interpolate(
    loaded: LoadedGenerator,
    *,
    seed_a: Optional[int] = None,
    seed_b: Optional[int] = None,
    steps: int = 8,
) -> List[Image.Image]:
    """Linear latent walk between two seeds; returns ``steps`` images.

    The two endpoint latents are drawn from ``seed_a`` / ``seed_b`` (so each
    endpoint is reproducible); the walk itself is then deterministic.
    """
    steps = max(2, int(steps))
    z_a = _endpoint_latent(loaded, seed_a)
    z_b = _endpoint_latent(loaded, seed_b)
    images: List[Image.Image] = []
    for k in range(steps):
        t = k / (steps - 1)
        z = (1 - t) * z_a + t * z_b
        images.append(_to_pil(loaded.model(z)[0][0]))
    return images


def _endpoint_latent(loaded: LoadedGenerator, seed: Optional[int]) -> torch.Tensor:
    """One reproducible latent vector for an interpolation endpoint."""
    _seed_rng(seed, loaded.device)
    return torch.randn(1, loaded.nz, device=loaded.device)
