"""
Purpose: Conditional U-Net VAE with channel attention mechanisms
Author: Hallett Lab
Date: 2026-01-27
Input: Images and conditioning vectors
Output: Reconstructed images, latent representations

Adds Squeeze-and-Excitation (SE) attention blocks to skip connections
in the decoder, allowing the model to selectively weight feature channels.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from typing import Tuple, List, Union

from candescence.core.logging_config import get_logger
from .film import FiLM

logger = get_logger("candescence.architectures.cond_uc_vae_attention")


class AttentionBlock(nn.Module):
    """
    Squeeze-and-Excitation channel attention block.

    Parameters
    ----------
    feature_dim : int
        Number of input channels
    reduction_ratio : int
        Channel reduction ratio for the bottleneck MLP
    """

    def __init__(self, feature_dim: int, reduction_ratio: int = 16) -> None:
        super(AttentionBlock, self).__init__()
        self.global_avg_pool = nn.AdaptiveAvgPool2d(1)
        self.fc1 = nn.Linear(feature_dim, feature_dim // reduction_ratio, bias=False)
        self.fc2 = nn.Linear(feature_dim // reduction_ratio, feature_dim, bias=False)
        self.relu = nn.ReLU()
        self.sigmoid = nn.Sigmoid()

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """Apply channel attention."""
        b, c, _, _ = x.size()
        y = self.global_avg_pool(x).view(b, c)
        y = self.relu(self.fc1(y))
        y = self.sigmoid(self.fc2(y)).view(b, c, 1, 1)
        return x * y


class Encoder(nn.Module):
    """Conditional encoder with FiLM modulation (same as cond_uc_vae)."""

    def __init__(self, arguments: dict) -> None:
        super(Encoder, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        self.cond_dim = self.arguments['_cond_dim']

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])

        self.film1 = FiLM(64, self.cond_dim)
        self.film2 = FiLM(128, self.cond_dim)
        self.film3 = FiLM(256, self.cond_dim)
        self.film4 = FiLM(512, self.cond_dim)

        self.conv1 = nn.Conv2d(3, 64, 3, padding=1)
        self.batch1 = nn.BatchNorm2d(64)
        self.conv1_extra = nn.Conv2d(64, 64, 3, padding=1)
        self.batch1_extra = nn.BatchNorm2d(64)
        self.pool1 = nn.MaxPool2d(2, 2)

        self.conv2 = nn.Conv2d(64, 128, 3, padding=1)
        self.batch2 = nn.BatchNorm2d(128)
        self.conv2_extra = nn.Conv2d(128, 128, 3, padding=1)
        self.batch2_extra = nn.BatchNorm2d(128)
        self.pool2 = nn.MaxPool2d(2, 2)

        self.conv3 = nn.Conv2d(128, 256, 3, padding=1)
        self.batch3 = nn.BatchNorm2d(256)
        self.conv3_extra = nn.Conv2d(256, 256, 3, padding=1)
        self.batch3_extra = nn.BatchNorm2d(256)
        self.pool3 = nn.MaxPool2d(2, 2)

        self.conv4 = nn.Conv2d(256, 512, 3, padding=1)
        self.batch4 = nn.BatchNorm2d(512)
        self.conv4_extra = nn.Conv2d(512, 512, 3, padding=1)
        self.batch4_extra = nn.BatchNorm2d(512)

        self.flattened_size = None
        self.fc_mu = None
        self.fc_logvar = None

    def compute_flattened_size(self) -> None:
        """Compute flattened feature size and initialize FC layers."""
        dummy_input = torch.ones(
            1, 3,
            self.arguments['image_dimension'],
            self.arguments['image_dimension']
        ).to(self.device)
        dummy_cond = torch.ones(
            1, len(self.arguments['conditional_variables'])
        ).to(self.device)

        with torch.no_grad():
            _, _, _, x4 = self.forward_conv(dummy_input, dummy_cond)

        self.flattened_size = x4.shape[1] * x4.shape[2] * x4.shape[3]

        self.fc_mu = nn.Linear(
            self.flattened_size + self.cond_dim,
            self.arguments['latent_dim']
        ).to(self.device)
        self.fc_logvar = nn.Linear(
            self.flattened_size + self.cond_dim,
            self.arguments['latent_dim']
        ).to(self.device)

        nn.init.xavier_uniform_(self.fc_mu.weight)
        nn.init.xavier_uniform_(self.fc_logvar.weight)
        if self.fc_mu.bias is not None:
            nn.init.zeros_(self.fc_mu.bias)
        if self.fc_logvar.bias is not None:
            nn.init.zeros_(self.fc_logvar.bias)

    def forward_conv(
        self, x: torch.Tensor, cond: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """Forward pass through convolutional layers with FiLM modulation."""
        x = x.float()

        x1 = self.act(self.batch1(self.conv1(x)))
        x1 = self.film1(x1, cond)
        x1 = self.act(self.batch1_extra(self.conv1_extra(x1)))
        x1_pooled = self.pool1(x1)

        x2 = self.act(self.batch2(self.conv2(x1_pooled)))
        x2 = self.film2(x2, cond)
        x2 = self.act(self.batch2_extra(self.conv2_extra(x2)))
        x2_pooled = self.pool2(x2)

        x3 = self.act(self.batch3(self.conv3(x2_pooled)))
        x3 = self.film3(x3, cond)
        x3 = self.act(self.batch3_extra(self.conv3_extra(x3)))
        x3_pooled = self.pool3(x3)

        x4 = self.act(self.batch4(self.conv4(x3_pooled)))
        x4 = self.film4(x4, cond)
        x4 = self.act(self.batch4_extra(self.conv4_extra(x4)))

        return x1, x2, x3, x4

    def reparameterize(
        self, mu: torch.Tensor, logvar: torch.Tensor
    ) -> torch.Tensor:
        """Reparameterization trick."""
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std

    def forward(
        self, x: torch.Tensor, cond: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, List[torch.Tensor], Tuple[int, ...]]:
        """
        Forward pass through encoder.

        Returns z, mu, logvar, skip_connections, feature_map_shape
        """
        x = x.to(self.device).float()
        if isinstance(cond, (list, tuple)):
            cond = cond[0]

        cond = cond.to(self.device).float()
        x1, x2, x3, x4 = self.forward_conv(x, cond)

        feature_map_shape = x4.shape[1:]
        x_flat = torch.flatten(x4, start_dim=1)
        x_flat_cond = torch.cat([x_flat, cond], dim=1)

        mu = self.fc_mu(x_flat_cond)
        logvar = self.fc_logvar(x_flat_cond)
        logvar = torch.clamp(logvar, min=-10, max=10)
        z = self.reparameterize(mu, logvar)

        return z, mu, logvar, [x1, x2, x3, x4], feature_map_shape

    def encode(
        self, x: torch.Tensor, cond: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """Encode input to latent distribution parameters."""
        _, mu, logvar, _, _ = self.forward(x, cond)
        return mu, logvar


class Decoder(nn.Module):
    """
    Conditional decoder with attention-gated skip connections.

    Uses SE attention blocks on skip connections before concatenation,
    plus FiLM conditioning on the decoder pathway.
    """

    def __init__(self, arguments: dict, flattened_size: int) -> None:
        super(Decoder, self).__init__()
        self.arguments = arguments
        self.cond_dim = self.arguments['_cond_dim']

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])
        self.sigmoid = nn.Sigmoid()

        self.film3 = FiLM(256, self.cond_dim)
        self.film2 = FiLM(128, self.cond_dim)
        self.film1 = FiLM(64, self.cond_dim)

        # Attention gates for skip connections
        ratios = self.arguments.get('reduction_ratio', 16)
        if isinstance(ratios, (list, tuple)) and len(ratios) == 4:
            r4, r3, r2, r1 = ratios
        else:
            r4 = r3 = r2 = r1 = int(ratios)

        self.att4 = AttentionBlock(512, reduction_ratio=r4)
        self.att3 = AttentionBlock(256, reduction_ratio=r3)
        self.att2 = AttentionBlock(128, reduction_ratio=r2)
        self.att1 = AttentionBlock(64, reduction_ratio=r1)

        # Linear transformation with condition concatenation
        self.linear_part = nn.Sequential(
            nn.Linear(arguments['latent_dim'] + self.cond_dim, arguments['intermediate_dim']),
            self.act,
            nn.Linear(arguments['intermediate_dim'], flattened_size),
            self.act
        )

        self.unflatten = nn.Unflatten(dim=1, unflattened_size=(512, 6, 6))

        # Decoder blocks with skip connections
        self.convtrans3 = nn.ConvTranspose2d(512 + 512, 256, 3, stride=2, padding=1, output_padding=1)
        self.bn3 = nn.BatchNorm2d(256)

        self.convtrans2 = nn.ConvTranspose2d(256 + 256, 128, 3, stride=2, padding=1, output_padding=1)
        self.bn2 = nn.BatchNorm2d(128)

        self.convtrans1 = nn.ConvTranspose2d(128 + 128, 64, 3, stride=2, padding=1, output_padding=1)
        self.bn1 = nn.BatchNorm2d(64)

        self.final_conv = nn.ConvTranspose2d(64, 3, 3, stride=1, padding=1)

    def forward(
        self,
        z: torch.Tensor,
        skip_connections: List[torch.Tensor],
        feature_map_shape: Tuple[int, ...],
        cond: torch.Tensor
    ) -> torch.Tensor:
        """
        Forward pass with attention-gated skip connections.

        Parameters
        ----------
        z : torch.Tensor
            Latent vector
        skip_connections : list
            [x1, x2, x3, x4] from encoder
        feature_map_shape : tuple
            Shape to reshape to
        cond : torch.Tensor
            Conditioning vector
        """
        x1, x2, x3, x4 = skip_connections

        cond = cond.to(z.device).float()
        z_cond = torch.cat([z, cond], dim=1)
        x = self.linear_part(z_cond)
        x = x.view(-1, *feature_map_shape)

        # Decoder block 3 with attention-gated skip
        x4_g = self.att4(x4)
        x = torch.cat([x, x4_g], dim=1)
        x = self.act(self.bn3(self.convtrans3(x)))
        x = self.film3(x, cond)

        # Decoder block 2
        x = F.interpolate(x, size=(x3.shape[2], x3.shape[3]), mode='bilinear', align_corners=False)
        x3_g = self.att3(x3)
        x = torch.cat([x, x3_g], dim=1)
        x = self.act(self.bn2(self.convtrans2(x)))
        x = self.film2(x, cond)

        # Decoder block 1
        x = F.interpolate(x, size=(x2.shape[2], x2.shape[3]), mode='bilinear', align_corners=False)
        x2_g = self.att2(x2)
        x = torch.cat([x, x2_g], dim=1)
        x = self.act(self.bn1(self.convtrans1(x)))
        x = self.film1(x, cond)

        # Final output
        x = self.final_conv(x)
        x = self.sigmoid(x)

        return x


class cond_uc_VAE_attention(nn.Module):
    """
    Conditional U-Net VAE with Squeeze-and-Excitation attention.

    Adds channel attention blocks to skip connections in the decoder,
    allowing selective weighting of encoder features.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary. Supports 'reduction_ratio' (int or list of 4)
        for controlling attention bottleneck width per decoder level.
    """

    def __init__(self, arguments: dict) -> None:
        super(cond_uc_VAE_attention, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            self.arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )

        self.encoder = Encoder(self.arguments).to(self.device)
        self.encoder.compute_flattened_size()

        self.flattened_size = self.encoder.flattened_size
        self.decoder = Decoder(self.arguments, self.flattened_size).to(self.device)

    def forward(
        self,
        x: torch.Tensor,
        cond: Union[torch.Tensor, List[torch.Tensor]]
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass through VAE.

        Parameters
        ----------
        x : torch.Tensor
            Input images
        cond : torch.Tensor or list
            Conditioning vector, or [encoder_cond, decoder_cond]

        Returns
        -------
        recon, z, mu, logvar
        """
        x = x.to(self.device).float()

        if isinstance(cond, list) and len(cond) == 2:
            encoder_cond, decoder_cond = cond
        else:
            encoder_cond = cond
            decoder_cond = cond

        z, mu, logvar, skip_connections, feature_map_shape = self.encoder(x, encoder_cond)

        self.feature_map_shape = feature_map_shape
        recon = self.decoder(z, skip_connections, feature_map_shape, decoder_cond)

        return recon, z, mu, logvar
