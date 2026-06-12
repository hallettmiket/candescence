"""
Candescence: Deep learning for Candida albicans morphology analysis.

This package provides tools for:
- Object detection of Candida morphologies (FCOS/MMDETECTION)
- VAE-based latent space analysis (TLV, Tendril VAE)
- Statistical analysis and visualization

Submodules:
- candescence.core: Shared utilities and configuration
- candescence.detection: FCOS-based morphology detection
- candescence.tlv: Tendril VAE and related architectures
- candescence.analysis: Statistical analysis and visualization
"""

__version__ = "0.1.0"

from candescence.core.config import TLVConfig
from candescence.core.morphology import (
    CLASS_NAMES,
    MorphologyClass,
    NUM_CLASSES,
)

__all__ = [
    "TLVConfig",
    "MorphologyClass",
    "CLASS_NAMES",
    "NUM_CLASSES",
    "__version__",
]
