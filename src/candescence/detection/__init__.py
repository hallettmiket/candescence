"""
Candescence Detection Module

FCOS-based object detection for Candida morphology classification.
Uses MMDETECTION framework with curriculum learning.
"""

from candescence.detection.inference.detector import CandidaDetector
from candescence.detection.inference.cell_extractor import CellExtractor

__all__ = [
    "CandidaDetector",
    "CellExtractor",
]
