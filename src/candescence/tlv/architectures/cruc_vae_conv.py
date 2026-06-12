"""
Purpose: CRUC VAE with configurable skip channel count
Author: Hallett Lab
Date: 2026-01-27
Input: Images and conditioning vectors
Output: Reconstructed images, latent representations

Variant of CRUC VAE that allows configuring the skip connection channel
count independently from the latent dimension via 'skip_channels' argument.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from typing import Tuple, List, Optional, Union

from candescence.core.logging_config import get_logger
from .film import FiLM

logger = get_logger("candescence.architectures.cruc_vae_conv")


class Encoder(nn.Module):
    """
    Conditional encoder with configurable skip channel reduction.

    Like CRUC encoder but uses skip_channels (defaulting to latent_dim)
    for the 1x1 conv reduction, allowing independent control.
    """

    def __init__(self, arguments: dict) -> None:
        super(Encoder, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        self.cond_dim = self.arguments['_cond_dim']
        self.latent_dim = self.arguments['latent_dim']
        self.skip_channels = self.arguments.get('skip_channels', self.latent_dim)

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

        # 1x1 convolutions to reduce to skip_channels
        self.reduce_64 = nn.Conv2d(64, self.skip_channels, kernel_size=1)
        self.reduce_128 = nn.Conv2d(128, self.skip_channels, kernel_size=1)
        self.reduce_256 = nn.Conv2d(256, self.skip_channels, kernel_size=1)
        self.reduce_512 = nn.Conv2d(512, self.skip_channels, kernel_size=1)

        # FC layers initialized dynamically
        self.fc_mu: Optional[nn.Linear] = None
        self.fc_logvar: Optional[nn.Linear] = None

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
        self,
        x: torch.Tensor,
        cond: Union[torch.Tensor, List[torch.Tensor]]
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, List[torch.Tensor], Optional[torch.Tensor]]:
        """
        Forward pass through encoder.

        Returns z, mu, logvar, skip_connections (latent-augmented), decoder_cond
        """
        if isinstance(cond, (list, tuple)):
            cond_enc, cond_dec = cond
        else:
            cond_enc, cond_dec = cond, None

        x1, x2, x3, x4 = self.forward_conv(x, cond_enc)

        # Reduce feature maps
        skip1 = self.reduce_64(x1)
        skip2 = self.reduce_128(x2)
        skip3 = self.reduce_256(x3)
        skip4 = self.reduce_512(x4)

        # Flatten and compute latent variables
        x_flat = torch.flatten(x4, start_dim=1)
        mu = self.fc_mu(x_flat)
        logvar = self.fc_logvar(x_flat)
        z = self.reparameterize(mu, logvar)

        # Augment skip connections with latent vector
        z_expanded = z.unsqueeze(-1).unsqueeze(-1)
        skip1 = torch.cat([skip1, z_expanded.expand(-1, -1, skip1.shape[2], skip1.shape[3])], dim=1)
        skip2 = torch.cat([skip2, z_expanded.expand(-1, -1, skip2.shape[2], skip2.shape[3])], dim=1)
        skip3 = torch.cat([skip3, z_expanded.expand(-1, -1, skip3.shape[2], skip3.shape[3])], dim=1)
        skip4 = torch.cat([skip4, z_expanded.expand(-1, -1, skip4.shape[2], skip4.shape[3])], dim=1)

        return z, mu, logvar, [skip1, skip2, skip3, skip4], cond_dec

    def encode(
        self, x: torch.Tensor, cond: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """Encode input to latent distribution parameters."""
        _, mu, logvar, _, _ = self.forward(x, cond)
        return mu, logvar


class Decoder(nn.Module):
    """Conditional decoder with configurable skip channel count."""

    def __init__(self, arguments: dict) -> None:
        super(Decoder, self).__init__()
        self.arguments = arguments
        self.latent_dim = self.arguments['latent_dim']
        self.cond_dim = self.arguments['_cond_dim']
        self.skip_channels = self.arguments.get('skip_channels', self.latent_dim)

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])
        self.sigmoid = nn.Sigmoid()

        # FiLM layers conditioned on projected latent
        self.film3 = FiLM(256, self.latent_dim)
        self.film2 = FiLM(128, self.latent_dim)
        self.film1 = FiLM(64, self.latent_dim)

        # Decoder layers (skip_channels + latent_dim for augmented skips)
        self.convtrans3 = nn.ConvTranspose2d(self.skip_channels + self.latent_dim, 256, 3, stride=2, padding=1, output_padding=1)
        self.bn3 = nn.BatchNorm2d(256)

        self.convtrans2 = nn.ConvTranspose2d(256 + self.skip_channels + self.latent_dim, 128, 3, stride=2, padding=1, output_padding=1)
        self.bn2 = nn.BatchNorm2d(128)

        self.convtrans1 = nn.ConvTranspose2d(128 + self.skip_channels + self.latent_dim, 64, 3, stride=2, padding=1, output_padding=1)
        self.bn1 = nn.BatchNorm2d(64)

        self.final_conv = nn.ConvTranspose2d(64, 3, 3, stride=1, padding=1)

        self.cond_projection = nn.Linear(self.cond_dim, self.latent_dim)

    def forward(
        self,
        z: torch.Tensor,
        skip_connections: List[torch.Tensor],
        *args: torch.Tensor
    ) -> torch.Tensor:
        """
        Forward pass through decoder.

        Parameters
        ----------
        z : torch.Tensor
            Latent vector
        skip_connections : list
            Latent-augmented skip connections
        *args : torch.Tensor
            Last argument is the conditioning vector
        """
        cond = args[-1]
        cond = self.cond_projection(cond)
        skip1, skip2, skip3, skip4 = skip_connections

        # Decoder block 3
        x = self.act(self.bn3(self.convtrans3(skip4)))
        x = self.film3(x, cond)

        # Decoder block 2
        x = torch.cat([x, skip3], dim=1)
        x = self.act(self.bn2(self.convtrans2(x)))
        x = self.film2(x, cond)

        # Decoder block 1
        x = torch.cat([x, skip2], dim=1)
        x = self.act(self.bn1(self.convtrans1(x)))
        x = self.film1(x, cond)

        # Final output
        x = self.final_conv(x)
        x = self.sigmoid(x)

        return x


class cruc_VAE_conv(nn.Module):
    """
    CRUC VAE with configurable skip channel count.

    Like cruc_VAE but allows setting skip_channels independently from
    latent_dim via the 'skip_channels' config key.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary
    """

    def __init__(self, arguments: dict) -> None:
        super(cruc_VAE_conv, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            self.arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )

        self.encoder = Encoder(self.arguments).to(self.device)

        # Dynamically compute the flattened size
        dummy_input = torch.ones(
            1, 3,
            self.arguments['image_dimension'],
            self.arguments['image_dimension']
        ).to(self.device)
        dummy_cond = torch.ones(1, self.encoder.cond_dim).to(self.device)
        with torch.no_grad():
            _, _, _, x4 = self.encoder.forward_conv(dummy_input, dummy_cond)
        flattened_size = x4.shape[1] * x4.shape[2] * x4.shape[3]

        self.encoder.fc_mu = nn.Linear(flattened_size, self.encoder.latent_dim).to(self.device)
        self.encoder.fc_logvar = nn.Linear(flattened_size, self.encoder.latent_dim).to(self.device)

        self.decoder = Decoder(self.arguments).to(self.device)
        self.feature_map_shape = None

    def forward(
        self,
        x: torch.Tensor,
        cond: Union[torch.Tensor, List[torch.Tensor]]
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass through VAE.

        Returns recon, z, mu, logvar
        """
        x = x.to(self.device).float()

        if isinstance(cond, list) and len(cond) == 2:
            encoder_cond, decoder_cond = cond
        else:
            encoder_cond = cond
            decoder_cond = cond

        z, mu, logvar, skip_connections, _ = self.encoder(x, encoder_cond)
        recon = self.decoder(z, skip_connections, decoder_cond)

        return recon, z, mu, logvar
