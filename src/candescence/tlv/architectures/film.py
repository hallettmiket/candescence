"""
Purpose: FiLM (Feature-wise Linear Modulation) conditioning layers
Author: Hallett Lab
Date: 2026-01-27
Input: Feature maps and conditioning vectors
Output: Modulated feature maps

FiLM layers are used to inject conditioning information (like hue, saturation)
into the VAE encoder and decoder feature maps. This allows the model to
generate images conditioned on specific properties.

Reference: Perez et al., "FiLM: Visual Reasoning with a General Conditioning Layer"
"""

import torch
import torch.nn as nn
from typing import Optional


class FiLM(nn.Module):
    """
    Feature-wise Linear Modulation layer for conditioning.

    Applies affine transformation to feature maps based on conditioning input:
        output = gamma * features + beta

    Where gamma and beta are learned from the conditioning vector.

    Parameters
    ----------
    feature_dim : int
        Number of feature channels to modulate
    cond_dim : int
        Dimension of the conditioning vector

    Examples
    --------
    >>> film = FiLM(feature_dim=64, cond_dim=3)
    >>> features = torch.randn(8, 64, 32, 32)  # [batch, channels, H, W]
    >>> cond = torch.randn(8, 3)  # [batch, cond_dim]
    >>> output = film(features, cond)
    >>> output.shape
    torch.Size([8, 64, 32, 32])
    """

    def __init__(self, feature_dim: int, cond_dim: int) -> None:
        super(FiLM, self).__init__()
        self.feature_dim = feature_dim
        self.cond_dim = cond_dim

        # Modulation layers: learn gamma (scale) and beta (shift)
        self.gamma_layer = nn.Linear(cond_dim, feature_dim)
        self.beta_layer = nn.Linear(cond_dim, feature_dim)

        # Xavier initialization for stable training
        nn.init.xavier_uniform_(self.gamma_layer.weight)
        nn.init.xavier_uniform_(self.beta_layer.weight)
        nn.init.zeros_(self.gamma_layer.bias)
        nn.init.zeros_(self.beta_layer.bias)

    def forward(
        self,
        x: torch.Tensor,
        cond: torch.Tensor
    ) -> torch.Tensor:
        """
        Apply FiLM modulation to feature maps.

        Parameters
        ----------
        x : torch.Tensor
            Feature maps of shape [batch, feature_dim, H, W]
        cond : torch.Tensor
            Conditioning vector of shape [batch, cond_dim]

        Returns
        -------
        torch.Tensor
            Modulated feature maps of same shape as input
        """
        # De-FiLMed mode: when there is no conditioning vector (cond_dim == 0)
        # FiLM is a no-op. Returning ``x`` unchanged is the correct identity
        # here -- a zero-width ``nn.Linear`` would emit only its (zero-init)
        # bias, which would otherwise scale/shift the feature map by zero and
        # annihilate the signal. ``cond`` may be ``None`` in this mode.
        if self.cond_dim == 0:
            return x

        # Compute scale and shift from conditioning
        # Shape: [batch, feature_dim] -> [batch, feature_dim, 1, 1]
        gamma = self.gamma_layer(cond).unsqueeze(-1).unsqueeze(-1)
        beta = self.beta_layer(cond).unsqueeze(-1).unsqueeze(-1)

        # Apply affine transformation
        return gamma * x + beta

    def extra_repr(self) -> str:
        return f'feature_dim={self.feature_dim}, cond_dim={self.cond_dim}'


class FiLMGenerator(nn.Module):
    """
    Generate FiLM parameters from a conditioning network.

    This is useful when you want a more complex transformation
    of the conditioning input before computing gamma/beta.

    Parameters
    ----------
    cond_dim : int
        Dimension of the conditioning vector
    hidden_dim : int
        Hidden layer dimension
    feature_dims : list of int
        Feature dimensions for each FiLM layer to generate
    """

    def __init__(
        self,
        cond_dim: int,
        hidden_dim: int,
        feature_dims: list[int]
    ) -> None:
        super(FiLMGenerator, self).__init__()
        self.cond_dim = cond_dim
        self.feature_dims = feature_dims

        # Shared conditioning network
        self.cond_net = nn.Sequential(
            nn.Linear(cond_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
        )

        # Per-layer gamma/beta generators
        self.gamma_layers = nn.ModuleList([
            nn.Linear(hidden_dim, dim) for dim in feature_dims
        ])
        self.beta_layers = nn.ModuleList([
            nn.Linear(hidden_dim, dim) for dim in feature_dims
        ])

        self._init_weights()

    def _init_weights(self) -> None:
        """Initialize weights with Xavier uniform."""
        for layer in self.gamma_layers:
            nn.init.xavier_uniform_(layer.weight)
            nn.init.zeros_(layer.bias)
        for layer in self.beta_layers:
            nn.init.xavier_uniform_(layer.weight)
            nn.init.zeros_(layer.bias)

    def forward(
        self,
        cond: torch.Tensor
    ) -> tuple[list[torch.Tensor], list[torch.Tensor]]:
        """
        Generate FiLM parameters for all layers.

        Parameters
        ----------
        cond : torch.Tensor
            Conditioning vector of shape [batch, cond_dim]

        Returns
        -------
        tuple of (list of gammas, list of betas)
            Each gamma/beta has shape [batch, feature_dim, 1, 1]
        """
        h = self.cond_net(cond)

        gammas = [
            layer(h).unsqueeze(-1).unsqueeze(-1)
            for layer in self.gamma_layers
        ]
        betas = [
            layer(h).unsqueeze(-1).unsqueeze(-1)
            for layer in self.beta_layers
        ]

        return gammas, betas
