"""Modern (torchvision) detection track: train + infer FCOS detectors in the main
environment on GPU, in-process. New detectors use this track; the legacy mmdet
detectors remain inference-only in the isolated worker.
"""

from candescence.detection.modern.dataset import PickleDetectionDataset
from candescence.detection.modern.inference import (
    LoadedModernModel,
    detect,
    load_modern_model,
)
from candescence.detection.modern.model import (
    ARCHITECTURE_BY_BACKBONE,
    MODERN_ARCHITECTURE,
    MODERN_ARCHITECTURE_R101,
    MODERN_ARCHITECTURES,
    SUPPORTED_BACKBONES,
    architecture_for_backbone,
    backbone_for_architecture,
    build_fcos,
)
from candescence.detection.modern.trainer import TrainResult, train_detector

__all__ = [
    "PickleDetectionDataset",
    "MODERN_ARCHITECTURE",
    "MODERN_ARCHITECTURE_R101",
    "MODERN_ARCHITECTURES",
    "ARCHITECTURE_BY_BACKBONE",
    "SUPPORTED_BACKBONES",
    "architecture_for_backbone",
    "backbone_for_architecture",
    "build_fcos",
    "train_detector",
    "TrainResult",
    "load_modern_model",
    "LoadedModernModel",
    "detect",
]
