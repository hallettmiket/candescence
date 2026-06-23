"""
Purpose: Tendril VAE architecture with hierarchical latent learning
Author: Hallett Lab
Date: 2026-01-27
Input: Images and conditioning vectors
Output: Reconstructed images, latent representations

The Tendril VAE is a hierarchical architecture that learns secondary VAEs
on the latent space to capture finer structure in the data.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from typing import Optional, Tuple, List, Union

from candescence.core.logging_config import get_logger
from .film import FiLM

logger = get_logger("candescence.architectures.tendril_vae")


def initialize_weights(m: nn.Module) -> None:
    """Initialize weights with Kaiming uniform for LeakyReLU."""
    if isinstance(m, (nn.Linear, nn.Conv2d, nn.ConvTranspose2d)):
        nn.init.kaiming_uniform_(m.weight, nonlinearity='leaky_relu')
        if m.bias is not None:
            nn.init.zeros_(m.bias)


class Encoder(nn.Module):
    """
    Tendril encoder with FiLM conditioning.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary
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

        self.fc_mu: Optional[nn.Linear] = None
        self.fc_logvar: Optional[nn.Linear] = None

        self.apply(initialize_weights)

    def forward_conv(
        self,
        x: torch.Tensor,
        cond: torch.Tensor
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
        self,
        mu: torch.Tensor,
        logvar: torch.Tensor
    ) -> torch.Tensor:
        """Reparameterization trick for VAE sampling."""
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std

    def forward(
        self,
        x: torch.Tensor,
        cond: Optional[torch.Tensor] = None
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, List[torch.Tensor]]:
        """Forward pass through encoder. ``cond`` defaults to ``None`` for the
        unconditional (de-FiLMed) tendril path; FiLM is identity when
        ``cond_dim == 0``."""
        x1, x2, x3, x4 = self.forward_conv(x, cond)

        x4_flat = torch.flatten(x4, start_dim=1)

        mu = self.fc_mu(x4_flat)
        logvar = torch.clamp(self.fc_logvar(x4_flat), min=-20, max=10)
        z = self.reparameterize(mu, logvar)

        return z, mu, logvar, [x1, x2, x3, x4]

    def encode(
        self,
        x: torch.Tensor,
        cond: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """Encode input to latent distribution parameters."""
        _, mu, logvar, _ = self.forward(x, cond)
        return mu, logvar


class Decoder(nn.Module):
    """
    Tendril decoder with FiLM conditioning.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary
    """

    def __init__(self, arguments: dict) -> None:
        super(Decoder, self).__init__()
        self.arguments = arguments
        self.latent_dim = self.arguments['latent_dim']
        self.cond_dim = self.arguments['_cond_dim']

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])
        self.sigmoid = nn.Sigmoid()

        # FiLM layers for conditioning
        self.film4 = FiLM(256, self.cond_dim)
        self.film3 = FiLM(128, self.cond_dim)
        self.film2 = FiLM(64, self.cond_dim)
        self.film1 = FiLM(32, self.cond_dim)

        # Linear projection from latent space
        bottleneck_size = (self.arguments['image_dimension'] // 8)
        self.fc = nn.Linear(self.latent_dim, 512 * bottleneck_size * bottleneck_size)

        # Transposed convolutions for upsampling
        self.convtrans4 = nn.ConvTranspose2d(512 + 512, 256, 3, stride=1, padding=1)
        self.bn4 = nn.BatchNorm2d(256)
        self.bn4_extra = nn.BatchNorm2d(256)
        self.convtrans4_extra = nn.ConvTranspose2d(256, 256, 3, stride=2, padding=1, output_padding=1)

        self.convtrans3 = nn.ConvTranspose2d(256 + 256, 128, 3, stride=2, padding=1, output_padding=1)
        self.bn3 = nn.BatchNorm2d(128)
        self.bn3_extra = nn.BatchNorm2d(128)
        self.convtrans3_extra = nn.Conv2d(128, 128, 3, padding=1)

        self.convtrans2 = nn.ConvTranspose2d(128 + 128, 64, 3, stride=2, padding=1, output_padding=1)
        self.bn2 = nn.BatchNorm2d(64)
        self.bn2_extra = nn.BatchNorm2d(64)
        self.convtrans2_extra = nn.Conv2d(64, 64, 3, padding=1)

        self.convtrans1 = nn.ConvTranspose2d(64 + 64, 32, 3, stride=1, padding=1)
        self.bn1 = nn.BatchNorm2d(32)
        self.bn1_extra = nn.BatchNorm2d(32)
        self.convtrans1_extra = nn.Conv2d(32, 32, 3, padding=1)

        self.convtrans_final = nn.ConvTranspose2d(32, 3, 3, stride=1, padding=1)

        self.apply(initialize_weights)

    def forward(
        self,
        z: torch.Tensor,
        skip_connections: List[torch.Tensor],
        cond: Optional[torch.Tensor] = None
    ) -> torch.Tensor:
        """Forward pass through decoder. ``cond`` defaults to ``None`` for the
        unconditional (de-FiLMed) tendril path."""
        skip1, skip2, skip3, skip4 = skip_connections

        # Project latent to bottleneck
        x = self.fc(z)
        x = x.view(-1, 512, 16, 16)

        # Decoder block 4
        x = torch.cat([x, skip4], dim=1)
        x = self.act(self.bn4(self.convtrans4(x)))
        x = self.film4(x, cond)
        x = self.act(self.bn4_extra(self.convtrans4_extra(x)))

        # Decoder block 3
        x = torch.cat([x, skip3], dim=1)
        x = self.act(self.bn3(self.convtrans3(x)))
        x = self.film3(x, cond)
        x = self.act(self.bn3_extra(self.convtrans3_extra(x)))

        # Decoder block 2
        x = torch.cat([x, skip2], dim=1)
        x = self.act(self.bn2(self.convtrans2(x)))
        x = self.act(self.bn2_extra(self.convtrans2_extra(x)))
        x = self.film2(x, cond)

        # Decoder block 1 (no upsampling)
        x = torch.cat([x, skip1], dim=1)
        x = self.act(self.bn1(self.convtrans1(x)))
        x = self.act(self.bn1_extra(self.convtrans1_extra(x)))
        x = self.film1(x, cond)

        x = self.convtrans_final(x)
        x = self.sigmoid(x)

        return x


class tendril_VAE(nn.Module):
    """
    Tendril VAE with hierarchical latent learning.

    This architecture supports learning secondary VAEs on top of the
    primary latent space to capture finer structure.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary
    """

    def __init__(self, arguments: dict) -> None:
        super(tendril_VAE, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            self.arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        self.latent_dim = self.arguments['latent_dim']

        # Initialize encoder
        self.encoder = Encoder(self.arguments).to(self.device)

        # Compute input size for FC layers
        dummy_input = torch.ones(
            1, 3,
            self.arguments['image_dimension'],
            self.arguments['image_dimension']
        ).to(self.device)
        dummy_cond = torch.ones(1, self.encoder.cond_dim).to(self.device)

        with torch.no_grad():
            x1, x2, x3, x4 = self.encoder.forward_conv(dummy_input, dummy_cond)

        # Initialize encoder FC layers
        self.encoder.fc_mu = nn.Linear(x4.numel() // x4.shape[0], self.latent_dim)
        self.encoder.fc_logvar = nn.Linear(x4.numel() // x4.shape[0], self.latent_dim)

        # Initialize decoder
        self.decoder = Decoder(self.arguments).to(self.device)

    def forward(
        self,
        x: torch.Tensor,
        cond: Union[torch.Tensor, List[torch.Tensor], None] = None
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor, List[torch.Tensor]]:
        """
        Forward pass through Tendril VAE.

        Parameters
        ----------
        x : torch.Tensor
            Input images
        cond : torch.Tensor or list
            Conditioning vector, or [encoder_cond, decoder_cond]

        Returns
        -------
        recon : torch.Tensor
            Reconstructed images
        z : torch.Tensor
            Latent vectors
        mu : torch.Tensor
            Latent means
        logvar : torch.Tensor
            Latent log variances
        skip_connections : list
            Feature maps for potential hierarchical processing
        """
        x = x.to(self.device).float()

        # Support separate encoder/decoder conditioning
        if isinstance(cond, list) and len(cond) == 2:
            encoder_cond, decoder_cond = cond
        else:
            encoder_cond = cond
            decoder_cond = cond

        z, mu, logvar, skip_connections = self.encoder(x, encoder_cond)
        recon = self.decoder(z, skip_connections, decoder_cond)

        return recon, z, mu, logvar, skip_connections
