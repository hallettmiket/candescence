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
from candescence.detection.modern.model import MODERN_ARCHITECTURE, build_fcos
from candescence.detection.modern.trainer import TrainResult, train_detector

__all__ = [
    "PickleDetectionDataset",
    "MODERN_ARCHITECTURE",
    "build_fcos",
    "train_detector",
    "TrainResult",
    "load_modern_model",
    "LoadedModernModel",
    "detect",
]
