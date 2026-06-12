"""
Purpose: ResNet-based VAE architecture
Author: Hallett Lab
Date: 2026-01-27
Input: Images [batch, 3, H, W]
Output: Reconstructed images, latent representations

Uses a pretrained ResNet-152 backbone as encoder and a mirrored
transposed-convolution decoder. No conditioning — unconditional architecture.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
import torchvision.models as models
from torchvision.models import resnet152, ResNet152_Weights
from typing import Tuple

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.architectures.rc_vae")


class Encoder(nn.Module):
    """ResNet-152 encoder with convolutional latent heads."""

    def __init__(self, arguments: dict) -> None:
        super().__init__()
        self.arguments = arguments
        self.device = arguments['DEVICE']

        # Pretrained ResNet-152 backbone up through layer4 (no avgpool/fc)
        resnet = models.resnet152(weights=ResNet152_Weights.DEFAULT)
        # Keep 8x8 spatial by disabling the last down-sample
        resnet.layer4[0].conv2.stride = (1, 1)
        resnet.layer4[0].downsample[0].stride = (1, 1)

        # Chop off avgpool and fc
        self.backbone = nn.Sequential(*list(resnet.children())[:-2])

        # Convolutional latent heads
        C = 2048
        L = arguments['latent_dim']
        self.conv_mu = nn.Conv2d(C, L, kernel_size=1)
        self.conv_logvar = nn.Conv2d(C, L, kernel_size=1)

    def reparameterize(
        self, mu: torch.Tensor, logvar: torch.Tensor
    ) -> torch.Tensor:
        """Reparameterization trick for VAE sampling."""
        std = (0.5 * logvar).exp()
        eps = torch.randn_like(std)
        return mu + eps * std

    def forward(
        self, x: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass through encoder.

        Parameters
        ----------
        x : torch.Tensor
            Input images [batch, 3, H, W]

        Returns
        -------
        z : torch.Tensor
            Sampled latent maps [batch, L, 8, 8]
        mu_map : torch.Tensor
            Mean maps [batch, L, 8, 8]
        logvar_map : torch.Tensor
            Log variance maps [batch, L, 8, 8]
        """
        feats = self.backbone(x)
        mu_map = self.conv_mu(feats)
        logvar_map = self.conv_logvar(feats)
        z = self.reparameterize(mu_map, logvar_map)
        return z, mu_map, logvar_map


class TransposeBottleneck(nn.Module):
    """Mirrors torchvision Bottleneck block with ConvTranspose2d layers."""

    def __init__(self, enc_block: nn.Module) -> None:
        super().__init__()
        # Invert conv3 (1x1)
        self.conv3_t = nn.ConvTranspose2d(
            in_channels=enc_block.conv3.out_channels,
            out_channels=enc_block.conv2.out_channels,
            kernel_size=enc_block.conv3.kernel_size,
            stride=enc_block.conv3.stride,
            padding=enc_block.conv3.padding,
            output_padding=enc_block.conv3.stride[0] - 1 if any(enc_block.conv3.stride) else 0,
            bias=False
        )
        self.bn3 = nn.BatchNorm2d(enc_block.conv2.out_channels)

        # Invert conv2 (3x3)
        self.conv2_t = nn.ConvTranspose2d(
            in_channels=enc_block.conv2.out_channels,
            out_channels=enc_block.conv1.out_channels,
            kernel_size=enc_block.conv2.kernel_size,
            stride=enc_block.conv2.stride,
            padding=enc_block.conv2.padding,
            output_padding=enc_block.conv2.stride[0] - 1 if any(enc_block.conv2.stride) else 0,
            bias=False
        )
        self.bn2 = nn.BatchNorm2d(enc_block.conv1.out_channels)

        # Invert conv1 (1x1)
        self.conv1_t = nn.ConvTranspose2d(
            in_channels=enc_block.conv1.out_channels,
            out_channels=enc_block.conv1.in_channels,
            kernel_size=enc_block.conv1.kernel_size,
            stride=enc_block.conv1.stride,
            padding=enc_block.conv1.padding,
            output_padding=enc_block.conv1.stride[0] - 1 if any(enc_block.conv1.stride) else 0,
            bias=False
        )
        self.bn1 = nn.BatchNorm2d(enc_block.conv1.in_channels)

        # Invert downsample if present
        if enc_block.downsample is not None:
            ds = enc_block.downsample[0]
            self.down_t = nn.ConvTranspose2d(
                in_channels=ds.out_channels,
                out_channels=ds.in_channels,
                kernel_size=ds.kernel_size,
                stride=ds.stride,
                padding=ds.padding,
                output_padding=ds.stride[0] - 1,
                bias=False
            )
            self.bn_ds = nn.BatchNorm2d(ds.in_channels)
        else:
            self.down_t = None

        self.relu = nn.ReLU(inplace=True)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """Forward pass through transposed bottleneck block."""
        identity = x
        out = self.relu(self.bn3(self.conv3_t(x)))
        out = self.relu(self.bn2(self.conv2_t(out)))
        out = self.bn1(self.conv1_t(out))
        if self.down_t is not None:
            identity = self.bn_ds(self.down_t(x))
        out += identity
        return self.relu(out)


class ResNetDecoder(nn.Module):
    """Decoder that mirrors the ResNet-152 encoder with transposed convolutions."""

    def __init__(self, arguments: dict) -> None:
        super().__init__()
        latent_dim = arguments['latent_dim']
        resnet = resnet152(weights=ResNet152_Weights.DEFAULT)

        # Expand z from [B, latent_dim, 8, 8] to 2048 channels
        self.expand = nn.ConvTranspose2d(
            latent_dim, 2048,
            kernel_size=1, stride=1, padding=0, bias=False
        )
        self.bn0 = nn.BatchNorm2d(2048)
        self.relu = nn.ReLU(inplace=True)

        # Mirror each ResNet stage
        self.up_blocks = nn.ModuleList()
        for stage in [resnet.layer4, resnet.layer3, resnet.layer2, resnet.layer1]:
            for enc_block in reversed(list(stage)):
                self.up_blocks.append(TransposeBottleneck(enc_block))

        # Invert stem: maxpool and conv1
        stem_conv = resnet.conv1
        self.unconv1 = nn.ConvTranspose2d(
            stem_conv.out_channels, stem_conv.out_channels,
            kernel_size=stem_conv.kernel_size, stride=stem_conv.stride,
            padding=stem_conv.padding, output_padding=1, bias=False
        )
        self.bn_unconv1 = nn.BatchNorm2d(stem_conv.out_channels)

        # Final conv: 64 -> 3
        self.final = nn.Sequential(
            nn.Conv2d(64, 3, kernel_size=3, padding=1),
            nn.Sigmoid()
        )

    def forward(self, z: torch.Tensor) -> torch.Tensor:
        """
        Forward pass through decoder.

        Parameters
        ----------
        z : torch.Tensor
            Latent maps [batch, latent_dim, 8, 8]

        Returns
        -------
        torch.Tensor
            Reconstructed image [batch, 3, H, W]
        """
        x = self.relu(self.bn0(self.expand(z)))

        for block in self.up_blocks:
            x = block(x)

        x = self.relu(self.bn_unconv1(self.unconv1(x)))
        x = self.final(x)
        return x


class rc_vae(nn.Module):
    """
    ResNet-based Convolutional VAE.

    Uses a pretrained ResNet-152 backbone for encoding and a mirrored
    transposed-convolution decoder. Unconditional architecture (Strategy #11).

    Parameters
    ----------
    arguments : dict
        Configuration dictionary containing:
        - DEVICE: torch device string
        - latent_dim: latent space dimension
    """

    def __init__(self, arguments: dict) -> None:
        super(rc_vae, self).__init__()
        self.arguments = arguments

        self.encoder = Encoder(arguments)
        self.decoder = ResNetDecoder(arguments)

        self.to(arguments['DEVICE'])

    def forward(
        self, x: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Forward pass through VAE.

        Parameters
        ----------
        x : torch.Tensor
            Input images [batch, 3, H, W]

        Returns
        -------
        recon : torch.Tensor
            Reconstructed images
        z : torch.Tensor
            Sampled latent maps
        mu_map : torch.Tensor
            Mean maps
        logvar_map : torch.Tensor
            Log variance maps
        """
        B, C, H, W = x.shape

        z, mu_map, logvar_map = self.encoder(x)
        recon = self.decoder(z)

        # Force recon back to exactly (H, W)
        if recon.shape[-2:] != (H, W):
            recon = F.interpolate(
                recon, size=(H, W),
                mode='bilinear', align_corners=False
            )

        return recon, z, mu_map, logvar_map
