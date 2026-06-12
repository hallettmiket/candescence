"""
Purpose: Conditional Redirected Hierarchical U-Net VAE (CRUtch VAE)
Author: Hallett Lab
Date: 2026-01-27
Input: Images and conditioning vectors
Output: Reconstructed images, hierarchical latent representations

Uses FiLM conditioning with hierarchical skip connections that are
reconstructed from per-level latent variables. Strategy #13.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from typing import Tuple, List, Union

from candescence.core.logging_config import get_logger
from .film import FiLM

logger = get_logger("candescence.architectures.crutch_vae")


def initialize_weights(m: nn.Module) -> None:
    """Xavier uniform initialization for linear and conv layers."""
    if isinstance(m, (nn.Linear, nn.Conv2d, nn.ConvTranspose2d)):
        nn.init.xavier_uniform_(m.weight)
        if m.bias is not None:
            nn.init.zeros_(m.bias)


class Encoder(nn.Module):
    """
    Hierarchical encoder producing per-level latent variables.

    Each encoder block produces a separate latent code (z1-z4),
    enabling hierarchical latent representations.
    """

    def __init__(self, arguments: dict) -> None:
        super(Encoder, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        self.cond_dim = self.arguments['_cond_dim']
        self.latent_dim = self.arguments['latent_dim']

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])

        # FiLM layers for conditioning
        self.film1 = FiLM(64, self.cond_dim)
        self.film2 = FiLM(128, self.cond_dim)
        self.film3 = FiLM(256, self.cond_dim)
        self.film4 = FiLM(512, self.cond_dim)

        # Convolutional layers
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

        # Per-level FC layers (initialized dynamically in crutch_VAE)
        self.fc_mu1 = self.fc_mu2 = self.fc_mu3 = self.fc_mu4 = None
        self.fc_logvar1 = self.fc_logvar2 = self.fc_logvar3 = self.fc_logvar4 = None

        self.apply(initialize_weights)

    def forward_conv(
        self, x: torch.Tensor, cond: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """Forward pass through convolutional layers with FiLM modulation."""
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
        """Reparameterization trick for VAE sampling."""
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std

    def forward(
        self, x: torch.Tensor, cond: torch.Tensor
    ) -> Tuple[List[torch.Tensor], List[torch.Tensor], List[torch.Tensor], List[torch.Tensor]]:
        """
        Forward pass producing hierarchical latent variables.

        Returns
        -------
        z_list : list of 4 tensors
            Sampled latent vectors per level
        mu_list : list of 4 tensors
            Means per level
        logvar_list : list of 4 tensors
            Log variances per level
        skip_list : list of 4 tensors
            Feature maps per level
        """
        x1, x2, x3, x4 = self.forward_conv(x, cond)

        x1_flat = torch.flatten(x1, start_dim=1)
        x2_flat = torch.flatten(x2, start_dim=1)
        x3_flat = torch.flatten(x3, start_dim=1)
        x4_flat = torch.flatten(x4, start_dim=1)

        mu1, logvar1 = self.fc_mu1(x1_flat), self.fc_logvar1(x1_flat) + 1e-8
        mu2, logvar2 = self.fc_mu2(x2_flat), self.fc_logvar2(x2_flat) + 1e-8
        mu3, logvar3 = self.fc_mu3(x3_flat), self.fc_logvar3(x3_flat) + 1e-8
        mu4, logvar4 = self.fc_mu4(x4_flat), self.fc_logvar4(x4_flat) + 1e-8

        # Stabilize latent parameters
        mu1 = torch.clamp(mu1, min=-10.0, max=10.0)
        mu2 = torch.clamp(mu2, min=-10.0, max=10.0)
        mu3 = torch.clamp(mu3, min=-10.0, max=10.0)
        mu4 = torch.clamp(mu4, min=-10.0, max=10.0)

        logvar1 = torch.clamp(logvar1, min=-10.0, max=10.0)
        logvar2 = torch.clamp(logvar2, min=-10.0, max=10.0)
        logvar3 = torch.clamp(logvar3, min=-10.0, max=10.0)
        logvar4 = torch.clamp(logvar4, min=-10.0, max=10.0)

        z1 = self.reparameterize(mu1, logvar1)
        z2 = self.reparameterize(mu2, logvar2)
        z3 = self.reparameterize(mu3, logvar3)
        z4 = self.reparameterize(mu4, logvar4)

        return (
            [z1, z2, z3, z4],
            [mu1, mu2, mu3, mu4],
            [logvar1, logvar2, logvar3, logvar4],
            [x1, x2, x3, x4]
        )

    def encode(
        self, x: torch.Tensor, cond: torch.Tensor
    ) -> Tuple[List[torch.Tensor], List[torch.Tensor]]:
        """Encode input to latent distribution parameters."""
        _, mu, logvar, _ = self.forward(x, cond)
        return mu, logvar


class Decoder(nn.Module):
    """
    Hierarchical decoder that reconstructs skip connections from latent variables.

    Each skip connection is reconstructed from its corresponding latent variable
    rather than being passed directly from the encoder.
    """

    def __init__(self, arguments: dict) -> None:
        super(Decoder, self).__init__()
        self.arguments = arguments
        self.latent_dim = self.arguments['latent_dim']
        self.cond_dim = self.arguments['_cond_dim']

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])
        self.sigmoid = nn.Sigmoid()

        # Project conditioning to latent dimension
        self.cond_projection = nn.Linear(self.cond_dim, self.latent_dim)

        # FiLM layers for conditioning
        self.film4 = FiLM(128, self.cond_dim)
        self.film3 = FiLM(64, self.cond_dim)
        self.film2 = FiLM(32, self.cond_dim)
        self.film1 = FiLM(16, self.cond_dim)

        # Reconstruct skip connections from latent variables
        self.reconstruct_skip4 = nn.ConvTranspose2d(self.latent_dim, 512, kernel_size=3, stride=1, padding=1)
        self.reconstruct_skip3 = nn.ConvTranspose2d(self.latent_dim, 256, kernel_size=3, stride=1, padding=1)
        self.reconstruct_skip2 = nn.ConvTranspose2d(self.latent_dim, 128, kernel_size=3, stride=1, padding=1)
        self.reconstruct_skip1 = nn.ConvTranspose2d(self.latent_dim, 64, kernel_size=3, stride=1, padding=1)

        # Transposed convolutional layers for upsampling
        self.fc = nn.Linear(self.latent_dim * 4, 64 * 16 * 16)
        self.convtrans4 = nn.ConvTranspose2d(64 + 512, 128, 3, stride=2, padding=1, output_padding=1)
        self.bn4 = nn.BatchNorm2d(128)
        self.convtrans4_extra = nn.Conv2d(128, 128, 3, padding=1)

        self.convtrans3 = nn.ConvTranspose2d(128 + 256, 64, 3, stride=2, padding=1, output_padding=1)
        self.bn3 = nn.BatchNorm2d(64)
        self.convtrans3_extra = nn.Conv2d(64, 64, 3, padding=1)

        self.convtrans2 = nn.ConvTranspose2d(64 + 128, 32, 3, stride=2, padding=1, output_padding=1)
        self.bn2 = nn.BatchNorm2d(32)
        self.convtrans2_extra = nn.Conv2d(32, 32, 3, padding=1)

        # No upsampling in Decoder Block 1
        self.convtrans1 = nn.ConvTranspose2d(32 + 64, 16, 3, stride=1, padding=1)
        self.bn1 = nn.BatchNorm2d(16)
        self.convtrans1_extra = nn.Conv2d(16, 16, 3, padding=1)

        self.convtrans_final = nn.ConvTranspose2d(16, 3, 3, stride=1, padding=1)
        self.apply(initialize_weights)

    def forward(
        self,
        z_list: List[torch.Tensor],
        cond: torch.Tensor
    ) -> Tuple[torch.Tensor, List[torch.Tensor]]:
        """
        Forward pass through hierarchical decoder.

        Parameters
        ----------
        z_list : list of 4 tensors
            Per-level latent variables [z1, z2, z3, z4]
        cond : torch.Tensor
            Conditioning vector

        Returns
        -------
        output : torch.Tensor
            Reconstructed image
        skips : list of 4 tensors
            Reconstructed skip connections
        """
        z1, z2, z3, z4 = z_list
        z_concat = torch.cat(z_list, dim=1)

        # Reconstruct skip connections from latent variables
        skip4 = self.reconstruct_skip4(z4.unsqueeze(-1).unsqueeze(-1))
        skip3 = self.reconstruct_skip3(z3.unsqueeze(-1).unsqueeze(-1))
        skip2 = self.reconstruct_skip2(z2.unsqueeze(-1).unsqueeze(-1))
        skip1 = self.reconstruct_skip1(z1.unsqueeze(-1).unsqueeze(-1))

        # Reshape latent vector to match bottleneck resolution
        x = self.fc(z_concat)
        x = x.view(-1, 64, 16, 16)

        # Decoder block 4
        skip4 = F.interpolate(skip4, size=x.shape[-2:], mode='bilinear', align_corners=False)
        x = torch.cat([x, skip4], dim=1)
        x = self.act(self.bn4(self.convtrans4(x)))
        x = self.film4(x, cond)

        # Decoder block 3
        skip3 = F.interpolate(skip3, size=x.shape[-2:], mode='bilinear', align_corners=False)
        x = torch.cat([x, skip3], dim=1)
        x = self.act(self.bn3(self.convtrans3(x)))
        x = self.act(self.convtrans3_extra(x))
        x = self.film3(x, cond)

        # Decoder block 2
        skip2 = F.interpolate(skip2, size=x.shape[-2:], mode='bilinear', align_corners=False)
        x = torch.cat([x, skip2], dim=1)
        x = self.act(self.bn2(self.convtrans2(x)))
        x = self.act(self.convtrans2_extra(x))
        x = self.film2(x, cond)

        # Decoder block 1 (no upsampling)
        skip1 = F.interpolate(skip1, size=x.shape[-2:], mode='bilinear', align_corners=False)
        x = torch.cat([x, skip1], dim=1)
        x = self.act(self.bn1(self.convtrans1(x)))
        x = self.act(self.convtrans1_extra(x))
        x = self.film1(x, cond)

        x = self.convtrans_final(x)
        x = self.sigmoid(x)

        return x, [skip1, skip2, skip3, skip4]


class crutch_VAE(nn.Module):
    """
    Conditional Redirected Hierarchical U-Net VAE (CRUtch VAE).

    Strategy #13. Uses hierarchical latent variables with per-level
    encoding and reconstructed skip connections. FiLM-conditioned.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary
    """

    def __init__(self, arguments: dict) -> None:
        super(crutch_VAE, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            self.arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        self.latent_dim = self.arguments['latent_dim']

        # Initialize the encoder
        self.encoder = Encoder(self.arguments).to(self.device)

        # Dynamically compute FC layer sizes
        dummy_input = torch.ones(
            1, 3,
            self.arguments['image_dimension'],
            self.arguments['image_dimension']
        ).to(self.device)
        dummy_cond = torch.ones(1, self.encoder.cond_dim).to(self.device)
        with torch.no_grad():
            x1, x2, x3, x4 = self.encoder.forward_conv(dummy_input, dummy_cond)

        self.encoder.fc_mu1 = nn.Linear(x1.numel() // x1.shape[0], self.latent_dim)
        self.encoder.fc_logvar1 = nn.Linear(x1.numel() // x1.shape[0], self.latent_dim)

        self.encoder.fc_mu2 = nn.Linear(x2.numel() // x2.shape[0], self.latent_dim)
        self.encoder.fc_logvar2 = nn.Linear(x2.numel() // x2.shape[0], self.latent_dim)

        self.encoder.fc_mu3 = nn.Linear(x3.numel() // x3.shape[0], self.latent_dim)
        self.encoder.fc_logvar3 = nn.Linear(x3.numel() // x3.shape[0], self.latent_dim)

        self.encoder.fc_mu4 = nn.Linear(x4.numel() // x4.shape[0], self.latent_dim)
        self.encoder.fc_logvar4 = nn.Linear(x4.numel() // x4.shape[0], self.latent_dim)

        # Initialize the hierarchical decoder
        self.decoder = Decoder(self.arguments).to(self.device)

    def forward(
        self,
        x: torch.Tensor,
        cond: Union[torch.Tensor, List[torch.Tensor]]
    ) -> Tuple[torch.Tensor, List[torch.Tensor], List[torch.Tensor], List[torch.Tensor], List[torch.Tensor], List[torch.Tensor]]:
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
        recon, z_list, mu_list, logvar_list, from_skip, to_skip
        """
        x = x.to(self.device).float()

        if isinstance(cond, list) and len(cond) == 2:
            encoder_cond, decoder_cond = cond
        else:
            encoder_cond = cond
            decoder_cond = cond

        z, mu, logvar, from_skip = self.encoder(x, encoder_cond)
        recon, to_skip = self.decoder(z, decoder_cond)

        return recon, z, mu, logvar, from_skip, to_skip
