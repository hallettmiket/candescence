"""
TLV (Trained Latent VAE) module for Candescence.

This module provides VAE architectures, training, and inference
capabilities for Candida albicans morphology analysis.
"""

# Lazy imports to avoid circular dependencies
__all__ = [
    "Factory",
    "VAETrainer",
    "Inference",
    "FullDataset",
    "LearningDataset",
    "LossLogger",
    "SkipLogger",
]


def __getattr__(name):
    """Lazy loading of submodules."""
    if name == "Factory":
        from candescence.tlv.factory import Factory
        return Factory
    if name == "VAETrainer":
        from candescence.tlv.training import VAETrainer
        return VAETrainer
    if name == "Inference":
        from candescence.tlv.inference import Inference
        return Inference
    if name == "FullDataset":
        from candescence.tlv.data import FullDataset
        return FullDataset
    if name == "LearningDataset":
        from candescence.tlv.data import LearningDataset
        return LearningDataset
    if name == "LossLogger":
        from candescence.tlv.training import LossLogger
        return LossLogger
    if name == "SkipLogger":
        from candescence.tlv.training import SkipLogger
        return SkipLogger
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
