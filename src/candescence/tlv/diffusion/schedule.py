"""Diffusion schedule utilities and sampling functions."""

import math
from typing import Optional, Tuple

import torch
import torch.nn as nn
from omegaconf import DictConfig


def build_linear_schedule(
    T: int, beta_start: float, beta_end: float, device: torch.device
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Build linear beta schedule and compute cumulative alpha products.

    Returns (betas, alphas, alpha_bars) all on *device*.
    """
    betas = torch.linspace(beta_start, beta_end, T, dtype=torch.float32, device=device)
    alphas = 1.0 - betas
    alpha_bars = torch.cumprod(alphas, dim=0)
    return betas, alphas, alpha_bars


def build_cosine_schedule(
    T: int, device: torch.device, s: float = 0.008
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Cosine beta schedule (Nichol & Dhariwal, 2021).

    Returns (betas, alphas, alpha_bars) all on *device*.
    """
    steps = torch.arange(T + 1, dtype=torch.float64)
    f = torch.cos((steps / T + s) / (1 + s) * math.pi / 2) ** 2
    alpha_bars = (f / f[0]).to(torch.float32).to(device)
    # Clip betas to prevent singularities at end of schedule
    betas = torch.clamp(1 - alpha_bars[1:] / alpha_bars[:-1], max=0.999)
    alpha_bars = alpha_bars[1:]  # drop the extra leading 1.0
    alphas = 1.0 - betas
    return betas, alphas, alpha_bars


def build_schedule(
    cfg: DictConfig, device: torch.device
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """Build diffusion schedule from config. Dispatches to linear or cosine."""
    schedule_type = getattr(cfg.diffusion, "schedule_type", "linear")
    if schedule_type == "cosine":
        return build_cosine_schedule(cfg.diffusion.t_steps, device)
    return build_linear_schedule(
        cfg.diffusion.t_steps, cfg.diffusion.beta_start, cfg.diffusion.beta_end, device
    )


def q_sample(
    x0: torch.Tensor,
    t: torch.Tensor,
    alpha_bars: torch.Tensor,
    eps: Optional[torch.Tensor] = None,
) -> Tuple[torch.Tensor, torch.Tensor]:
    """Forward diffusion: q(x_t | x_0)."""
    if eps is None:
        eps = torch.randn_like(x0)
    ab = alpha_bars[t].view(-1, 1, 1, 1)
    x_t = torch.sqrt(ab) * x0 + torch.sqrt(1 - ab) * eps
    return x_t, eps


def predict_x0_from_eps(
    x_t: torch.Tensor,
    t: torch.Tensor,
    eps_pred: torch.Tensor,
    alpha_bars: torch.Tensor,
) -> torch.Tensor:
    """Predict x_0 from x_t and predicted noise."""
    ab = alpha_bars[t].view(-1, 1, 1, 1)
    ab_clamped = torch.clamp(ab, min=1e-8)
    return (x_t - torch.sqrt(1 - ab) * eps_pred) / torch.sqrt(ab_clamped)


# ---------------------------------------------------------------------------
# Generation / reconstruction helpers
# ---------------------------------------------------------------------------

@torch.no_grad()
def generate_ddim(
    model: nn.Module,
    z_sem: torch.Tensor,
    alpha_bars: torch.Tensor,
    cfg: DictConfig,
    K: int = None,
    shape: Tuple[int, ...] = None,
) -> torch.Tensor:
    """DDIM deterministic generation from pure noise."""
    if K is None:
        K = getattr(cfg.diffusion, "ddim_k", 50)  # Default to 50 steps if not specified
    B = z_sem.shape[0]
    if shape is None:
        shape = (B, cfg.data.input_channels, cfg.data.img_size, cfg.data.img_size)

    x_t = torch.randn(shape, device=z_sem.device)
    steps = torch.linspace(cfg.diffusion.t_steps - 1, 0, K, device=z_sem.device).long()

    for i in range(len(steps) - 1):
        t_cur = steps[i].repeat(B)
        t_next = steps[i + 1]
        eps_pred = model.predict_eps(x_t, t_cur, z_sem)
        x0_pred = predict_x0_from_eps(x_t, t_cur, eps_pred, alpha_bars)
        alpha_bar_next = alpha_bars[t_next]
        x_t = torch.sqrt(alpha_bar_next) * x0_pred + torch.sqrt(1 - alpha_bar_next) * eps_pred

    t0 = steps[-1].repeat(B)
    eps_pred = model.predict_eps(x_t, t0, z_sem)
    x0_hat = predict_x0_from_eps(x_t, t0, eps_pred, alpha_bars)
    return torch.clamp(x0_hat, -1.0, 1.0)


@torch.no_grad()
def generate_ddpm(
    model: nn.Module,
    z_sem: torch.Tensor,
    betas: torch.Tensor,
    alphas: torch.Tensor,
    alpha_bars: torch.Tensor,
    cfg: DictConfig,
    shape: Tuple[int, ...] = None,
) -> torch.Tensor:
    """DDPM stochastic generation from pure noise."""
    B = z_sem.shape[0]
    if shape is None:
        shape = (B, cfg.data.input_channels, cfg.data.img_size, cfg.data.img_size)

    x_t = torch.randn(shape, device=z_sem.device)
    for t_int in range(cfg.diffusion.t_steps - 1, -1, -1):
        t_batch = torch.full((B,), t_int, device=z_sem.device, dtype=torch.long)
        eps_pred = model.predict_eps(x_t, t_batch, z_sem)

        ab_cur = alpha_bars[t_int]
        ab_prev = alpha_bars[t_int - 1] if t_int > 0 else torch.tensor(1.0, device=z_sem.device)
        beta_t = 1 - (ab_cur / ab_prev)
        alpha_t = 1 - beta_t

        pred_mean = (1 / torch.sqrt(alpha_t)) * (
            x_t - (beta_t / torch.sqrt(1 - ab_cur)) * eps_pred
        )
        if t_int > 0:
            x_t = pred_mean + torch.sqrt(beta_t) * torch.randn_like(x_t)
        else:
            x_t = pred_mean

    return torch.clamp(x_t, -1.0, 1.0)


@torch.no_grad()
def reconstruct_ddim(
    model: nn.Module,
    z_sem: torch.Tensor,
    x_noisy: torch.Tensor,
    alpha_bars: torch.Tensor,
    cfg: DictConfig,
) -> torch.Tensor:
    """DDIM deterministic reconstruction from noised original. Returns [0, 1]."""
    x_t = x_noisy
    for t_int in range(cfg.diffusion.t_steps - 1, 0, -1):
        t = torch.full((x_t.shape[0],), t_int, device=x_t.device, dtype=torch.long)
        eps_pred = model.predict_eps(x_t, t, z_sem)
        x0_pred = predict_x0_from_eps(x_t, t, eps_pred, alpha_bars)
        tp = t_int - 1
        ab_tp = alpha_bars[tp].view(1, 1, 1, 1)
        x_t = torch.sqrt(ab_tp) * x0_pred + torch.sqrt(1 - ab_tp) * eps_pred

    t0 = torch.zeros((x_t.shape[0],), device=x_t.device, dtype=torch.long)
    eps_pred0 = model.predict_eps(x_t, t0, z_sem)
    x0_pred0 = predict_x0_from_eps(x_t, t0, eps_pred0, alpha_bars)
    return torch.clamp((x0_pred0 + 1) / 2, 0, 1)


@torch.no_grad()
def reconstruct_ddpm(
    model: nn.Module,
    z_sem: torch.Tensor,
    x_noisy: torch.Tensor,
    betas: torch.Tensor,
    alphas: torch.Tensor,
    alpha_bars: torch.Tensor,
    cfg: DictConfig,
) -> torch.Tensor:
    """DDPM stochastic reconstruction from noised original. Returns [0, 1]."""
    x_t = x_noisy
    for t_int in range(cfg.diffusion.t_steps - 1, -1, -1):
        t = torch.full((x_t.shape[0],), t_int, device=x_t.device, dtype=torch.long)
        eps_pred = model.predict_eps(x_t, t, z_sem)

        at = alphas[t_int]
        abt = alpha_bars[t_int]
        bt = betas[t_int]

        factor = (1 - at) / torch.sqrt(1 - abt)
        mu_t = (1 / torch.sqrt(at)) * (x_t - factor * eps_pred)

        if t_int > 0:
            x_t = mu_t + torch.sqrt(bt) * torch.randn_like(x_t)
        else:
            x_t = mu_t

    return torch.clamp((x_t + 1) / 2, 0, 1)
