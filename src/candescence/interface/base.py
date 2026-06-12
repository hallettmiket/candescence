"""
Purpose: Abstract base class for Candescence models in interface
Author: Hallett Lab
Date: 2026-01-28

Defines the CandescenceModel interface that all models must implement
to be used in the Candescence interface.
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, Optional, Tuple

import torch


class CandescenceModel(ABC):
    """
    Abstract base class for all Candescence models used in the interface.

    All models exposed to the interface must implement this interface.
    This ensures consistent behavior across TLV, Detection, and future models.

    The interface is inference-only - no training methods are included.

    Example:
        >>> model = load_model("tlv", model_id="tendril_production")
        >>> z = model.encode(images)
        >>> reconstructed = model.decode(z)
    """

    @abstractmethod
    def encode(
        self,
        x: torch.Tensor,
        cond: Optional[torch.Tensor] = None
    ) -> torch.Tensor:
        """
        Encode input to latent space.

        Args:
            x: Input tensor of shape (B, C, H, W)
            cond: Optional conditioning tensor

        Returns:
            Latent representation of shape (B, latent_dim)
        """
        pass

    @abstractmethod
    def decode(
        self,
        z: torch.Tensor,
        cond: Optional[torch.Tensor] = None
    ) -> torch.Tensor:
        """
        Decode latent vector to output.

        Args:
            z: Latent tensor of shape (B, latent_dim)
            cond: Optional conditioning tensor

        Returns:
            Reconstructed output of shape (B, C, H, W)
        """
        pass

    @abstractmethod
    def get_latent_dim(self) -> int:
        """
        Return dimensionality of latent space.

        Returns:
            Integer latent dimension (e.g., 128, 256)
        """
        pass

    @property
    @abstractmethod
    def model_info(self) -> Dict[str, Any]:
        """
        Return model metadata.

        Returns:
            Dictionary with keys:
                - name: Human-readable model name
                - version: Model version string
                - training_date: When the model was trained
                - description: Brief description
                - architecture: Architecture name (e.g., 'tendril_vae')
                - is_production: Whether this is a production model
        """
        pass

    @property
    @abstractmethod
    def device(self) -> torch.device:
        """
        Return the device the model is on.

        Returns:
            torch.device (cuda or cpu)
        """
        pass

    def to(self, device: torch.device) -> "CandescenceModel":
        """
        Move model to specified device.

        Args:
            device: Target device

        Returns:
            Self for chaining
        """
        raise NotImplementedError("Subclass must implement to()")

    def eval(self) -> "CandescenceModel":
        """
        Set model to evaluation mode.

        Returns:
            Self for chaining
        """
        raise NotImplementedError("Subclass must implement eval()")

    def encode_full(
        self,
        x: torch.Tensor,
        cond: Optional[torch.Tensor] = None
    ) -> Optional[Tuple["torch.Tensor", "torch.Tensor", "torch.Tensor"]]:
        """
        Encode input, returning z, mu, and logvar for diagnostics.

        Unlike encode() which returns only z, this method also returns the
        distribution parameters needed for posterior collapse diagnostics.

        Args:
            x: Input tensor of shape (B, C, H, W)
            cond: Optional conditioning tensor

        Returns:
            Tuple of (z, mu, logvar) or None if not supported
        """
        return None
