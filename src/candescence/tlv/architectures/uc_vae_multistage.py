"""
Purpose: Multi-stage U-Net VAE with progressive skip connection removal
Author: Hallett Lab
Date: 2026-01-27
Input: Images [batch, 3, H, W]
Output: Reconstructed images, latent representations

Unconditional U-Net VAE where skip connections can be progressively
removed across training stages (controlled by training_stage argument).
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from typing import Tuple, List

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.architectures.uc_vae_multistage")


class Encoder(nn.Module):
    """U-Net encoder with 4 conv blocks and skip connections."""

    def __init__(self, arguments: dict) -> None:
        super(Encoder, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])

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
        dummy_input = torch.randn(
            1, 3,
            self.arguments['image_dimension'],
            self.arguments['image_dimension']
        ).to(self.device)
        with torch.no_grad():
            _, _, _, x4 = self.forward_conv(dummy_input)
        self.flattened_size = x4.shape[1] * x4.shape[2] * x4.shape[3]

        self.fc_mu = nn.Linear(self.flattened_size, self.arguments['latent_dim']).to(self.device)
        self.fc_logvar = nn.Linear(self.flattened_size, self.arguments['latent_dim']).to(self.device)

        nn.init.xavier_uniform_(self.fc_mu.weight)
        nn.init.xavier_uniform_(self.fc_logvar.weight)
        if self.fc_mu.bias is not None:
            nn.init.zeros_(self.fc_mu.bias)
        if self.fc_logvar.bias is not None:
            nn.init.zeros_(self.fc_logvar.bias)

    def forward_conv(
        self, x: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """Forward pass through convolutional layers."""
        x = x.float()

        x1 = self.act(self.batch1(self.conv1(x)))
        x1 = self.act(self.batch1_extra(self.conv1_extra(x1)))
        x1_pooled = self.pool1(x1)

        x2 = self.act(self.batch2(self.conv2(x1_pooled)))
        x2 = self.act(self.batch2_extra(self.conv2_extra(x2)))
        x2_pooled = self.pool2(x2)

        x3 = self.act(self.batch3(self.conv3(x2_pooled)))
        x3 = self.act(self.batch3_extra(self.conv3_extra(x3)))
        x3_pooled = self.pool3(x3)

        x4 = self.act(self.batch4(self.conv4(x3_pooled)))
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
        self, x: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, List[torch.Tensor], Tuple[int, ...]]:
        """
        Forward pass through encoder.

        Returns
        -------
        z, mu, logvar, skip_connections, feature_map_shape
        """
        x = x.to(self.device).float()
        x1, x2, x3, x4 = self.forward_conv(x)

        feature_map_shape = x4.shape[1:]
        x_flat = torch.flatten(x4, start_dim=1)

        mu = self.fc_mu(x_flat)
        logvar = self.fc_logvar(x_flat)
        logvar = torch.clamp(logvar, min=-10, max=10)
        z = self.reparameterize(mu, logvar)

        return z, mu, logvar, [x1, x2, x3, x4], feature_map_shape

    def encode(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """Encode input to latent distribution parameters."""
        _, mu, logvar, _, _ = self.forward(x)
        return mu, logvar


class Decoder(nn.Module):
    """
    Multi-stage decoder with progressive skip connection removal.

    Stages control which skip connections are active:
    - Stage 1: all skips (x4, x3, x2, x1)
    - Stage 2: drop x1 skip
    - Stage 3: drop x2 skip
    - Stage 4: drop x3 skip
    - Stage 5: no skips
    """

    def __init__(self, arguments: dict, flattened_size: int) -> None:
        super(Decoder, self).__init__()
        self.arguments = arguments
        self.stage = int(arguments.get('training_stage', 1))
        self.device = torch.device(
            arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        w = float(arguments.get('skip_weight', 1.0))
        self.skip_weight = torch.tensor(w, device=self.device)
        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])
        self.sigmoid = nn.Sigmoid()

        # Linear transformation from latent space to feature map
        self.linear_part = nn.Sequential(
            nn.Linear(arguments['latent_dim'], arguments['intermediate_dim']),
            self.act,
            nn.Linear(arguments['intermediate_dim'], flattened_size),
            self.act
        )

        H, W = arguments['feature_map_shape'][1:]
        C = flattened_size // (H * W)

        # Block definitions: full (with skip) vs noskip for each level
        self.conv3_full = nn.ConvTranspose2d(C + 512, 256, 3, stride=2, padding=1, output_padding=1)
        self.conv3_noskip = nn.ConvTranspose2d(C, 256, 3, stride=2, padding=1, output_padding=1)
        self.bn3 = nn.BatchNorm2d(256)

        self.conv2_full = nn.ConvTranspose2d(256 + 256, 128, 3, stride=2, padding=1, output_padding=1)
        self.conv2_noskip = nn.ConvTranspose2d(256, 128, 3, stride=2, padding=1, output_padding=1)
        self.bn2 = nn.BatchNorm2d(128)

        self.conv1_full = nn.ConvTranspose2d(128 + 128, 64, 3, stride=2, padding=1, output_padding=1)
        self.conv1_noskip = nn.ConvTranspose2d(128, 64, 3, stride=2, padding=1, output_padding=1)
        self.bn1 = nn.BatchNorm2d(64)

        self.conv0_full = nn.ConvTranspose2d(64 + 64, 64, kernel_size=3, stride=1, padding=1)
        self.conv0_noskip = nn.ConvTranspose2d(64, 64, kernel_size=3, stride=1, padding=1)
        self.bn0 = nn.BatchNorm2d(64)

        self.final_conv = nn.ConvTranspose2d(64, 3, 3, stride=1, padding=1)

    def forward(
        self,
        z: torch.Tensor,
        skip_connections: List[torch.Tensor],
        feature_map_shape: Tuple[int, ...]
    ) -> torch.Tensor:
        """
        Forward pass with stage-dependent skip connections.

        Parameters
        ----------
        z : torch.Tensor
            Latent vector
        skip_connections : list
            [x1, x2, x3, x4] from encoder
        feature_map_shape : tuple
            Shape to reshape to
        """
        x1, x2, x3, x4 = skip_connections

        x = self.linear_part(z)
        x = x.view(-1, *feature_map_shape)

        # Block3: deepest skip (x4) — keep on stages 1-4, drop on 5
        if self.stage <= 4:
            inp3 = torch.cat([x, x4 * self.skip_weight], dim=1)
            x = self.conv3_full(inp3)
        else:
            x = self.conv3_noskip(x)
        x = self.act(self.bn3(x))

        # Block2: skip (x3) — keep on stages 1-3, drop on 4-5
        x = F.interpolate(x, size=(x3.shape[2], x3.shape[3]), mode='bilinear', align_corners=False)
        if self.stage <= 3:
            inp2 = torch.cat([x, x3 * self.skip_weight], dim=1)
            x = self.conv2_full(inp2)
        else:
            x = self.conv2_noskip(x)
        x = self.act(self.bn2(x))

        # Block1: skip (x2) — keep on stages 1-2, drop on 3-5
        x = F.interpolate(x, size=(x2.shape[2], x2.shape[3]), mode='bilinear', align_corners=False)
        if self.stage <= 2:
            inp1 = torch.cat([x, x2 * self.skip_weight], dim=1)
            x = self.conv1_full(inp1)
        else:
            x = self.conv1_noskip(x)
        x = self.act(self.bn1(x))

        # Block0: skip (x1) — keep only on stage 1, drop on 2-5
        x = F.interpolate(x, size=(x1.shape[2], x1.shape[3]), mode='bilinear', align_corners=False)
        if self.stage == 1:
            inp0 = torch.cat([x, x1 * self.skip_weight], dim=1)
            x = self.conv0_full(inp0)
        else:
            x = self.conv0_noskip(x)
        x = self.act(self.bn0(x))

        x = self.final_conv(x)
        return self.sigmoid(x)


class uc_VAE_multistage(nn.Module):
    """
    Multi-stage U-Net VAE with progressive skip connection removal.

    Unconditional architecture (Strategy #12) that allows training in stages
    where skip connections are progressively removed, forcing the model to
    rely more on the latent representation.

    Parameters
    ----------
    arguments : dict
        Configuration dictionary containing:
        - DEVICE: torch device string
        - latent_dim: latent space dimension
        - training_stage: which stage (1-5, controls skip removal)
        - skip_weight: weight for skip connections (0.0-1.0)
        - leaky_relu_slope: negative slope for LeakyReLU
        - intermediate_dim: FC hidden dimension
        - image_dimension: input image size
    """

    def __init__(self, arguments: dict) -> None:
        super(uc_VAE_multistage, self).__init__()
        self.arguments = arguments
        self.device = torch.device(
            self.arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )

        self.encoder = Encoder(self.arguments).to(self.device)
        self.encoder.compute_flattened_size()

        # Run dummy forward to grab feature_map_shape
        with torch.no_grad():
            dummy = torch.randn(
                1, 3,
                self.arguments['image_dimension'],
                self.arguments['image_dimension']
            ).to(self.device)
            _, _, _, _, feature_map_shape = self.encoder(dummy)
        self.arguments['feature_map_shape'] = feature_map_shape

        self.flattened_size = self.encoder.flattened_size
        self.decoder = Decoder(self.arguments, self.flattened_size).to(self.device)
        self.skip_weight = self.decoder.skip_weight

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
        recon, z, mu, logvar
        """
        x = x.to(self.device).float()
        z, mu, logvar, skip_connections, feature_map_shape = self.encoder(x)
        self.feature_map_shape = feature_map_shape
        recon = self.decoder(z, skip_connections, feature_map_shape)
        return recon, z, mu, logvar
