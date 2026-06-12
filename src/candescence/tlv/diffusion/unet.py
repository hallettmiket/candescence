"""Diffusion UNet with time and semantic conditioning."""

import math
from typing import List

import torch
import torch.nn as nn
import torch.nn.functional as F

from .encoder import gn_groups


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def sinusoidal_timestep_embedding(
    t: torch.Tensor, dim: int, T: int
) -> torch.Tensor:
    """Sinusoidal timestep embeddings (Transformer / DDPM style)."""
    t_normalized = t.float() / (T - 1)
    half = dim // 2
    freqs = torch.exp(
        -math.log(10000) * torch.arange(0, half, device=t.device).float() / half
    )
    args = t_normalized[:, None] * freqs[None]
    emb = torch.cat([torch.sin(args), torch.cos(args)], dim=-1)
    if dim % 2 == 1:
        emb = F.pad(emb, (0, 1))
    return emb


# ---------------------------------------------------------------------------
# Building blocks
# ---------------------------------------------------------------------------

class AdaGroupNorm(nn.Module):
    """FiLM-style adaptive group normalization conditioned on semantic code."""

    def __init__(self, num_channels: int, latent_dim: int):
        super().__init__()
        self.num_channels = num_channels
        self.norm = nn.GroupNorm(gn_groups(num_channels), num_channels, affine=False)
        self.to_params = nn.Linear(latent_dim, 2 * num_channels)
        nn.init.zeros_(self.to_params.weight)
        nn.init.zeros_(self.to_params.bias)

    def forward(self, h: torch.Tensor, z_sem: torch.Tensor) -> torch.Tensor:
        B, C, H, W = h.shape
        assert C == self.num_channels, (
            f"AdaGN channel mismatch {C} vs {self.num_channels}"
        )
        h_norm = self.norm(h)
        params = self.to_params(z_sem)
        gamma, beta = params.chunk(2, dim=1)
        gamma = gamma.view(B, C, 1, 1)
        beta = beta.view(B, C, 1, 1)
        return h_norm * (1.0 + gamma) + beta


class TimeEmbedding(nn.Module):
    """Sinusoidal timestep encoding with MLP projection."""

    def __init__(self, sin_dim: int = 128, out_dim: int = 256, T: int = 100):
        super().__init__()
        self.sin_dim = sin_dim
        self.T = T
        self.mlp = nn.Sequential(
            nn.Linear(sin_dim, out_dim),
            nn.SiLU(),
            nn.Linear(out_dim, out_dim),
        )

    def forward(self, t: torch.Tensor) -> torch.Tensor:
        emb = sinusoidal_timestep_embedding(t, self.sin_dim, self.T)
        return self.mlp(emb)


class ResBlock(nn.Module):
    """Residual block with time and semantic conditioning via AdaGroupNorm."""

    def __init__(
        self, in_ch: int, out_ch: int, time_dim: int = 256, latent_dim: int = 32
    ):
        super().__init__()
        self.in_ch, self.out_ch = in_ch, out_ch
        self.conv1 = nn.Conv2d(in_ch, out_ch, 3, padding=1)
        self.adagn1 = AdaGroupNorm(out_ch, latent_dim)
        self.conv2 = nn.Conv2d(out_ch, out_ch, 3, padding=1)
        self.adagn2 = AdaGroupNorm(out_ch, latent_dim)
        self.time_proj = nn.Linear(time_dim, out_ch)
        self.act = nn.SiLU()
        self.shortcut = (
            nn.Identity() if in_ch == out_ch else nn.Conv2d(in_ch, out_ch, 1)
        )

    def forward(
        self, x: torch.Tensor, t_emb: torch.Tensor, z_sem: torch.Tensor
    ) -> torch.Tensor:
        h = self.conv1(x)
        h = self.adagn1(h, z_sem)
        h = self.act(h)
        tb = self.time_proj(t_emb).view(t_emb.shape[0], self.out_ch, 1, 1)
        h = h + tb
        h = self.conv2(h)
        h = self.adagn2(h, z_sem)
        h = self.act(h)
        return h + self.shortcut(x)


class SelfAttention(nn.Module):
    """
    Multi-head self-attention for spatial features.

    Uses F.scaled_dot_product_attention (Flash Attention on A100).
    """

    def __init__(self, channels: int, num_heads: int = 4):
        super().__init__()
        assert channels % num_heads == 0, "channels must be divisible by num_heads"
        self.channels = channels
        self.num_heads = num_heads
        self.head_dim = channels // num_heads

        self.norm = nn.GroupNorm(gn_groups(channels), channels, affine=True)
        self.to_q = nn.Conv2d(channels, channels, 1)
        self.to_k = nn.Conv2d(channels, channels, 1)
        self.to_v = nn.Conv2d(channels, channels, 1)
        self.to_out = nn.Conv2d(channels, channels, 1)

    def forward(self, h: torch.Tensor) -> torch.Tensor:
        B, C, H, W = h.shape
        hn = self.norm(h)

        q = self.to_q(hn)
        k = self.to_k(hn)
        v = self.to_v(hn)

        q = q.reshape(B, self.num_heads, self.head_dim, H * W).transpose(2, 3)
        k = k.reshape(B, self.num_heads, self.head_dim, H * W).transpose(2, 3)
        v = v.reshape(B, self.num_heads, self.head_dim, H * W).transpose(2, 3)

        out = F.scaled_dot_product_attention(q, k, v)
        out = out.transpose(2, 3).reshape(B, C, H, W)
        out = self.to_out(out)
        return h + out


# ---------------------------------------------------------------------------
# Full UNet
# ---------------------------------------------------------------------------

class DiffusionUNet(nn.Module):
    """
    U-Net for diffusion noise prediction.

    Encoder: 4 downsampling stages (128->64->32->16).
    Bottleneck: ResBlocks + Attention at 16x16.
    Decoder: 4 upsampling stages with skip connections.
    """

    def __init__(
        self,
        in_ch: int = 3,
        base_chs: List[int] = None,
        latent_dim: int = 32,
        time_dim: int = 256,
        sin_dim: int = 128,
        T: int = 500,
        num_heads: int = 4,
    ):
        super().__init__()
        base_chs = base_chs or [64, 128, 256, 512]
        self.in_ch = in_ch
        self.base_chs = base_chs
        self.T = T

        self.time_emb = TimeEmbedding(sin_dim, time_dim, T=T)

        # Down path
        self.down_blocks = nn.ModuleList()
        self.downsamples = nn.ModuleList()
        prev = in_ch
        down_resolutions = [128, 64, 32, 16]

        for i, ch in enumerate(base_chs):
            self.down_blocks.append(
                nn.ModuleList(
                    [
                        ResBlock(prev, ch, time_dim, latent_dim),
                        ResBlock(ch, ch, time_dim, latent_dim),
                        (
                            SelfAttention(ch, num_heads=num_heads)
                            if down_resolutions[i] in (16, 8)
                            else nn.Identity()
                        ),
                    ]
                )
            )
            prev = ch
            if i < len(base_chs) - 1:
                self.downsamples.append(
                    nn.Sequential(
                        nn.Conv2d(ch, ch, 4, stride=2, padding=1),
                        nn.GroupNorm(gn_groups(ch), ch, affine=True),
                        nn.SiLU(),
                    )
                )

        # Bottleneck
        self.mid1 = ResBlock(base_chs[-1], base_chs[-1], time_dim, latent_dim)
        self.mid_attn = SelfAttention(base_chs[-1], num_heads=num_heads)
        self.mid2 = ResBlock(base_chs[-1], base_chs[-1], time_dim, latent_dim)

        # Up path
        self.up_blocks = nn.ModuleList()
        self.upsamples = nn.ModuleList()
        rev_chs = list(reversed(base_chs))
        up_resolutions = [16, 32, 64, 128]

        for i, ch in enumerate(rev_chs):
            prev_ch = rev_chs[i - 1] if i > 0 else ch
            if i > 0:
                self.upsamples.append(
                    nn.Sequential(
                        nn.Conv2d(prev_ch, ch, 3, padding=1),
                        nn.GroupNorm(gn_groups(ch), ch, affine=True),
                        nn.SiLU(),
                    )
                )
            in_ch_rb1 = ch + ch
            self.up_blocks.append(
                nn.ModuleList(
                    [
                        ResBlock(in_ch_rb1, ch, time_dim, latent_dim),
                        ResBlock(ch, ch, time_dim, latent_dim),
                        (
                            SelfAttention(ch, num_heads=num_heads)
                            if up_resolutions[i] in (16, 8)
                            else nn.Identity()
                        ),
                    ]
                )
            )

        # Output head
        self.out_norm = nn.GroupNorm(32, base_chs[0])
        self.out_conv = nn.Conv2d(base_chs[0], in_ch, 3, padding=1)

    def forward(
        self, x_t: torch.Tensor, t: torch.Tensor, z_sem: torch.Tensor
    ) -> torch.Tensor:
        assert x_t.ndim == 4 and t.ndim == 1, f"x_t {x_t.shape}, t {t.shape}"
        t_emb = self.time_emb(t)

        hs = []
        h = x_t

        for i, blocks in enumerate(self.down_blocks):
            rb1, rb2, attn = blocks
            h = rb1(h, t_emb, z_sem)
            h = rb2(h, t_emb, z_sem)
            h = attn(h)
            hs.append(h)
            if i < len(self.downsamples):
                h = self.downsamples[i](h)

        h = self.mid1(h, t_emb, z_sem)
        h = self.mid_attn(h)
        h = self.mid2(h, t_emb, z_sem)

        for i, blocks in enumerate(self.up_blocks):
            rb1, rb2, attn = blocks
            skip = hs.pop()
            if i > 0:
                h = F.interpolate(h, scale_factor=2, mode="nearest")
                h = self.upsamples[i - 1](h)
            if h.shape[-2:] != skip.shape[-2:]:
                skip = F.interpolate(skip, size=h.shape[-2:], mode="nearest")
            h = torch.cat([h, skip], dim=1)
            h = rb1(h, t_emb, z_sem)
            h = rb2(h, t_emb, z_sem)
            h = attn(h)

        h = self.out_norm(h)
        h = F.silu(h)
        return self.out_conv(h)
