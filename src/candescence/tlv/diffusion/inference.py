"""
Purpose: Inference for the TLV diffusion model — encode, reconstruct, generate,
         interpolate. Framework-agnostic (returns numpy images), so the Streamlit
         app and notebooks can both use it.
Author: Hallett Lab (ported from Jose's 38-diffusion inference_service)
Date: 2026-06-12
Input: A LoadedDiffusionModel + image tensors / latent codes.
Output: numpy image arrays in [0, 1] — (H, W) grayscale or (H, W, 3) RGB.
"""

from __future__ import annotations

from typing import List, Optional

import numpy as np
import torch

from .loader import LoadedDiffusionModel
from .schedule import generate_ddim, q_sample, reconstruct_ddim


def _to_numpy_images(t: torch.Tensor) -> List[np.ndarray]:
    """(B, C, H, W) in [-1,1] or [0,1] -> list of [0,1] (H,W) or (H,W,3) arrays."""
    if t.min() < 0:
        t = (t + 1) / 2
    arr = t.clamp(0, 1).cpu().numpy()
    images: List[np.ndarray] = []
    for img in arr:
        if img.shape[0] == 1:
            images.append(img[0])  # grayscale (H, W)
        else:
            images.append(np.transpose(img, (1, 2, 0)))  # (H, W, 3)
    return images


def _cfg_with_steps(loaded: LoadedDiffusionModel, ddim_steps: Optional[int]):
    if ddim_steps is None:
        return loaded.cfg
    cfg = loaded.cfg.copy()
    cfg.diffusion.ddim_k = int(ddim_steps)
    return cfg


@torch.no_grad()
def encode(loaded: LoadedDiffusionModel, x0: torch.Tensor) -> np.ndarray:
    """Encode images to their semantic codes (posterior means). Returns (B, latent_dim)."""
    x0 = x0.to(loaded.device)
    _, mu, _, _ = loaded.model.encode(x0)
    return mu.cpu().numpy()


@torch.no_grad()
def reconstruct(
    loaded: LoadedDiffusionModel,
    x0: torch.Tensor,
    ddim_steps: Optional[int] = None,
) -> List[np.ndarray]:
    """Reconstruct images: encode -> noise to t=T-1 -> DDIM-denoise conditioned on z_sem."""
    x0 = x0.to(loaded.device)
    _, _, _, z_sem = loaded.model.encode(x0)
    t_steps = loaded.cfg.diffusion.t_steps
    t_max = torch.full((x0.shape[0],), t_steps - 1, device=loaded.device, dtype=torch.long)
    x_noisy, _ = q_sample(x0, t_max, loaded.alpha_bars)
    recon = reconstruct_ddim(
        loaded.model, z_sem, x_noisy, loaded.alpha_bars, _cfg_with_steps(loaded, ddim_steps)
    )
    return _to_numpy_images(recon)


@torch.no_grad()
def generate(
    loaded: LoadedDiffusionModel,
    n: int = 4,
    z_sem: Optional[torch.Tensor] = None,
    ddim_steps: Optional[int] = None,
    seed: Optional[int] = None,
) -> List[np.ndarray]:
    """Generate novel images. With no ``z_sem``, samples random semantic codes ~ N(0, I)."""
    if seed is not None:
        torch.manual_seed(seed)
    if z_sem is None:
        z = torch.randn(n, loaded.latent_dim, device=loaded.device)
    else:
        z = torch.as_tensor(z_sem, dtype=torch.float32, device=loaded.device)
    imgs = generate_ddim(loaded.model, z, loaded.alpha_bars, _cfg_with_steps(loaded, ddim_steps))
    return _to_numpy_images(imgs)


@torch.no_grad()
def interpolate(
    loaded: LoadedDiffusionModel,
    x_a: torch.Tensor,
    x_b: torch.Tensor,
    n_steps: int = 8,
    ddim_steps: Optional[int] = None,
) -> List[np.ndarray]:
    """Interpolate between two images in semantic-code space, generating each step."""
    pair = torch.stack([x_a, x_b]).to(loaded.device)
    _, mu, _, _ = loaded.model.encode(pair)
    z_a, z_b = mu[0], mu[1]
    fracs = torch.linspace(0, 1, n_steps, device=loaded.device)
    z = torch.stack([z_a * (1 - f) + z_b * f for f in fracs])
    imgs = generate_ddim(loaded.model, z, loaded.alpha_bars, _cfg_with_steps(loaded, ddim_steps))
    return _to_numpy_images(imgs)
