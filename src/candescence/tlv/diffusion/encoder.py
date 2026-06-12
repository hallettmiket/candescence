"""Semantic VAE Encoder."""

from typing import List, Tuple

import torch
import torch.nn as nn


def gn_groups(C: int) -> int:
    """Robust GroupNorm group chooser: <=32 and divides channels."""
    g = min(32, C)
    while C % g != 0:
        g -= 1
    return max(g, 1)


class SemanticEncoder(nn.Module):
    """
    VAE-style encoder that produces mean (mu) and log-variance (logvar).

    Architecture: 4 downsampling stages with conv blocks, each containing
    Conv -> GroupNorm -> ReLU -> Conv -> GroupNorm -> ReLU -> Downsample(stride=2).
    Final: Flatten -> FC to mu and logvar.
    """

    def __init__(
        self,
        in_ch: int,
        latent_dim: int,
        img_size: int,
        ch_schedule: List[int] = None,
        enc_norm: bool = True,
    ):
        super().__init__()
        ch_schedule = ch_schedule or [64, 128, 256, 512]
        self.in_ch = in_ch
        self.latent_dim = latent_dim
        self.enc_norm = enc_norm

        blocks = []
        prev = in_ch
        for ch in ch_schedule:
            g = gn_groups(ch)
            blocks.append(nn.Conv2d(prev, ch, 3, padding=1))
            if enc_norm:
                blocks.append(nn.GroupNorm(g, ch, affine=True))
            blocks.append(nn.ReLU(inplace=True))
            blocks.append(nn.Conv2d(ch, ch, 3, padding=1))
            if enc_norm:
                blocks.append(nn.GroupNorm(g, ch, affine=True))
            blocks.append(nn.ReLU(inplace=True))
            blocks.append(nn.Conv2d(ch, ch, 4, stride=2, padding=1))
            if enc_norm:
                blocks.append(nn.GroupNorm(g, ch, affine=True))
            blocks.append(nn.ReLU(inplace=True))
            prev = ch
        self.conv = nn.Sequential(*blocks)

        self.flatten_dim = ch_schedule[-1] * (img_size // 2 ** len(ch_schedule)) ** 2
        self.fc_mu = nn.Linear(self.flatten_dim, latent_dim)
        self.fc_logvar = nn.Linear(self.flatten_dim, latent_dim)

    def forward(
        self, x0: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        assert x0.ndim == 4, f"Expected (B,C,H,W), got {x0.shape}"
        h = self.conv(x0)
        h = h.flatten(1)
        mu = self.fc_mu(h)
        logvar = self.fc_logvar(h)

        if self.training:
            eps = torch.randn_like(mu)
            z = mu + torch.exp(0.5 * logvar) * eps
        else:
            z = mu
        return z, mu, logvar
