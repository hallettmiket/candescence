"""
Purpose: Property regressor for latent space prediction
Author: Hallett Lab
Date: 2026-01-27
Input: Latent vectors [batch, latent_dim]
Output: Predicted property values [batch, 1]

Simple MLP regressor that predicts image properties (e.g., hue)
from VAE latent vectors. Used in adversarial VAE training.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.architectures.regressor")


class PropertyRegressor(nn.Module):
    """
    Property regressor head for predicting image properties from latent space.

    Predicts a single scalar property (e.g., hue) from the latent vector
    using a 3-layer MLP with sigmoid output (constrained to [0, 1]).

    Parameters
    ----------
    latent_dim : int
        Dimension of the input latent vector
    """

    def __init__(self, latent_dim: int) -> None:
        super(PropertyRegressor, self).__init__()
        self.latent_dim = latent_dim

        self.fc1 = nn.Linear(latent_dim, 128)
        self.fc2 = nn.Linear(128, 64)
        self.fc3 = nn.Linear(64, 1)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Forward pass of the network.

        Parameters
        ----------
        x : torch.Tensor
            Input tensor of shape [batch_size, latent_dim]

        Returns
        -------
        torch.Tensor
            Output tensor of shape [batch_size, 1] (predicted property)
        """
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = torch.sigmoid(self.fc3(x))
        return x
