"""
Purpose: Convolutional VAE architecture (Strategy 0)
Author: Hallett Lab
Date: 2026-01-27
Input: Images (default 128x128)
Output: Reconstructed images, latent representations

This is the simplest VAE architecture -- no skip connections, no conditioning.
The encoder uses 4 conv blocks with max pooling, the decoder uses transposed
convolutions with upsampling.  Good baseline for comparison against more
complex architectures (uc_VAE, tendril_VAE).
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from typing import Tuple

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.architectures.c_vae")


class Encoder(nn.Module):
    """
    Convolutional encoder for c_VAE.

    Four conv blocks (each with two Conv2d + BatchNorm + LeakyReLU), three
    max-pool layers, then FC projections to mu and logvar.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary containing:
        - DEVICE: torch device string
        - leaky_relu_slope: negative slope for LeakyReLU
        - image_dimension: input image size (width = height)
        - latent_dim: latent space dimensionality
    """

    def __init__(self, arguments: dict) -> None:
        super(Encoder, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])

        # Block 1
        self.conv1 = nn.Conv2d(3, 64, 3, padding=1)
        self.batch1 = nn.BatchNorm2d(64)
        self.conv1_extra = nn.Conv2d(64, 64, 3, padding=1)
        self.batch1_extra = nn.BatchNorm2d(64)
        self.pool1 = nn.MaxPool2d(2, 2)

        # Block 2
        self.conv2 = nn.Conv2d(64, 128, 3, padding=1)
        self.batch2 = nn.BatchNorm2d(128)
        self.conv2_extra = nn.Conv2d(128, 128, 3, padding=1)
        self.batch2_extra = nn.BatchNorm2d(128)
        self.pool2 = nn.MaxPool2d(2, 2)

        # Block 3
        self.conv3 = nn.Conv2d(128, 256, 3, padding=1)
        self.batch3 = nn.BatchNorm2d(256)
        self.conv3_extra = nn.Conv2d(256, 256, 3, padding=1)
        self.batch3_extra = nn.BatchNorm2d(256)
        self.pool3 = nn.MaxPool2d(2, 2)

        # Block 4 (no pooling)
        self.conv4 = nn.Conv2d(256, 512, 3, padding=1)
        self.batch4 = nn.BatchNorm2d(512)
        self.conv4_extra = nn.Conv2d(512, 512, 3, padding=1)
        self.batch4_extra = nn.BatchNorm2d(512)

        # Compute conv output size dynamically from image_dimension
        img_dim = arguments.get('image_dimension', 128)
        dummy_input = torch.ones(1, 3, img_dim, img_dim)
        dummy_output = self._forward_conv(dummy_input)
        conv_output_size = dummy_output.shape[1] * dummy_output.shape[2] * dummy_output.shape[3]

        self.fc_mu = nn.Linear(conv_output_size, arguments['latent_dim'])
        self.fc_logvar = nn.Linear(conv_output_size, arguments['latent_dim'])

        # Xavier initialization for FC layers
        nn.init.xavier_uniform_(self.fc_mu.weight)
        nn.init.xavier_uniform_(self.fc_logvar.weight)
        if self.fc_mu.bias is not None:
            nn.init.zeros_(self.fc_mu.bias)
        if self.fc_logvar.bias is not None:
            nn.init.zeros_(self.fc_logvar.bias)

    def _forward_conv(self, x: torch.Tensor) -> torch.Tensor:
        """Forward pass through convolutional layers only."""
        x = x.float()

        x = self.act(self.batch1(self.conv1(x)))
        x = self.act(self.batch1_extra(self.conv1_extra(x)))
        x = self.pool1(x)

        x = self.act(self.batch2(self.conv2(x)))
        x = self.act(self.batch2_extra(self.conv2_extra(x)))
        x = self.pool2(x)

        x = self.act(self.batch3(self.conv3(x)))
        x = self.act(self.batch3_extra(self.conv3_extra(x)))
        x = self.pool3(x)

        x = self.act(self.batch4(self.conv4(x)))
        x = self.act(self.batch4_extra(self.conv4_extra(x)))
        return x

    def forward(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass through encoder.

        Returns
        -------
        z, mu, logvar
        """
        x = x.to(self.device).float()
        x = self._forward_conv(x)
        x = torch.flatten(x, start_dim=1)

        mu = self.fc_mu(x)
        logvar = self.fc_logvar(x)
        logvar = torch.clamp(logvar, min=-10, max=10)

        # Reparameterization trick
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        z = mu + eps * std

        return z, mu, logvar

    def encode(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """Encode input to latent distribution parameters (mu, logvar)."""
        _, mu, logvar = self.forward(x)
        return mu, logvar


class Decoder(nn.Module):
    """
    Transposed-convolution decoder for c_VAE.

    Maps latent vector z through a linear layer, reshapes to (512, 8, 8),
    then applies transposed convolutions with upsampling to reconstruct
    the image.  No skip connections -- reconstruction depends entirely
    on the latent code.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary containing:
        - latent_dim: latent space dimensionality
        - intermediate_dim: hidden layer size in the linear stage
        - leaky_relu_slope: negative slope for LeakyReLU
        - image_dimension: target output image size
    """

    def __init__(self, arguments: dict) -> None:
        super(Decoder, self).__init__()
        self.arguments = arguments
        self.target_size = arguments.get('image_dimension', 128)

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])
        self.sigmoid = nn.Sigmoid()

        output_size = 512 * 8 * 8
        self.linear_part = nn.Sequential(
            nn.Linear(self.arguments['latent_dim'], self.arguments['intermediate_dim']),
            self.act,
            nn.Linear(self.arguments['intermediate_dim'], output_size),
            self.act
        )

        self.conv1 = nn.ConvTranspose2d(512, 512, 3)
        self.batch1 = nn.BatchNorm2d(512)

        self.conv1_extra = nn.ConvTranspose2d(512, 256, 3)
        self.batch1_extra = nn.BatchNorm2d(256)

        self.up2 = nn.Upsample(scale_factor=2, mode='nearest')

        self.conv2 = nn.ConvTranspose2d(256, 256, 3)
        self.batch2 = nn.BatchNorm2d(256)

        self.conv2_extra = nn.ConvTranspose2d(256, 128, 3)
        self.batch2_extra = nn.BatchNorm2d(128)

        self.up3 = nn.Upsample(scale_factor=2, mode='nearest')

        self.conv3 = nn.ConvTranspose2d(128, 128, 3)
        self.batch3 = nn.BatchNorm2d(128)

        self.conv3_extra = nn.ConvTranspose2d(128, 64, 3)
        self.batch3_extra = nn.BatchNorm2d(64)

        self.up4 = nn.Upsample(scale_factor=2, mode='nearest')

        self.conv4 = nn.ConvTranspose2d(64, 64, 3)
        self.batch4 = nn.BatchNorm2d(64)

        self.conv4_extra = nn.ConvTranspose2d(64, 3, 3)
        self.batch4_extra = nn.BatchNorm2d(3)

        self.conv5_extra = nn.ConvTranspose2d(3, 3, 3)
        self.batch5_extra = nn.BatchNorm2d(3)

        self.conv6_extra = nn.ConvTranspose2d(3, 3, 3)

    def forward(self, z: torch.Tensor) -> torch.Tensor:
        """
        Decode latent vector to image reconstruction.

        The transposed-conv chain produces a 128x128 output.  If the target
        image dimension differs, the output is resized with bilinear
        interpolation as a final step.
        """
        x = self.linear_part(z)
        x = x.view(-1, 512, 8, 8)

        x = self.act(self.batch1(self.conv1(x)))
        x = self.act(self.batch1_extra(self.conv1_extra(x)))

        x = self.act(self.batch2(self.conv2(self.up2(x))))
        x = self.act(self.batch2_extra(self.conv2_extra(x)))

        x = self.act(self.batch3(self.conv3(self.up3(x))))
        x = self.act(self.batch3_extra(self.conv3_extra(x)))

        x = self.act(self.batch4(self.conv4(self.up4(x))))
        x = self.act(self.batch4_extra(self.conv4_extra(x)))
        x = self.act(self.batch5_extra(self.conv5_extra(x)))

        x = self.sigmoid(self.conv6_extra(x))

        # Resize if target dimension is not 128
        if x.shape[2] != self.target_size or x.shape[3] != self.target_size:
            x = F.interpolate(
                x, size=(self.target_size, self.target_size),
                mode='bilinear', align_corners=False
            )

        return x


class c_VAE(nn.Module):
    """
    Convolutional VAE (Strategy 0).

    Simplest VAE architecture: encoder compresses to a latent vector,
    decoder reconstructs from the latent vector alone (no skip connections,
    no conditioning).

    Parameters
    ----------
    arguments : dict
        Configuration dictionary containing DEVICE, latent_dim,
        intermediate_dim, leaky_relu_slope, image_dimension.
    """

    def __init__(self, arguments: dict) -> None:
        super(c_VAE, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            self.arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )

        self.encoder = Encoder(self.arguments)
        self.decoder = Decoder(self.arguments)

        self.to(self.device)

    def forward(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass through VAE.

        Returns
        -------
        recon, z, mu, logvar
        """
        x = x.to(self.device).float()
        z, mu, logvar = self.encoder(x)
        recon = self.decoder(z)
        return recon, z, mu, logvar

    def encode(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """Encode input to latent distribution parameters (mu, logvar)."""
        return self.encoder.encode(x)
