"""Top-level Conditional Diffusion VAE model."""

from typing import Tuple

import torch
import torch.nn as nn
from omegaconf import DictConfig

from .encoder import SemanticEncoder
from .unet import DiffusionUNet


class TLVCondDiffVAE(nn.Module):
    """
    Complete Conditional Diffusion VAE.

    Components:
      1. SemanticEncoder: Image -> (z, mu, logvar)
      2. DiffusionUNet:   (x_t, t, z_sem) -> eps_pred
    """

    def __init__(self, config: DictConfig):
        super().__init__()
        self.config = config

        self.encoder = SemanticEncoder(
            in_ch=config.data.input_channels,
            latent_dim=config.model.encoder.latent_dim,
            img_size=config.data.img_size,
            ch_schedule=list(config.model.encoder.channels),
            enc_norm=config.model.encoder.use_norm,
        )

        self.unet = DiffusionUNet(
            in_ch=config.data.input_channels,
            base_chs=list(config.model.unet.channels),
            latent_dim=config.model.encoder.latent_dim,
            time_dim=config.model.unet.time_emb_dim,
            sin_dim=config.model.unet.sin_emb_dim,
            T=config.diffusion.t_steps,
            num_heads=config.model.unet.num_attn_heads,
        )

    def encode(
        self, x0: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        z, mu, logvar = self.encoder(x0)
        z_sem = mu  # Use posterior mean as semantic code
        return z, mu, logvar, z_sem

    def predict_eps(
        self, x_t: torch.Tensor, t: torch.Tensor, z_sem: torch.Tensor
    ) -> torch.Tensor:
        return self.unet(x_t, t, z_sem)
