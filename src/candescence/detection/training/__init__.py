"""
Purpose: Training utilities for Candida detection models
Author: Hallett Lab
Date: 2026-02-04

Provides curriculum learning trainer and log parsing for FCOS detection training.
"""

from candescence.detection.training.curriculum_trainer import (
    CurriculumConfig,
    CurriculumTrainer,
)
from candescence.detection.training.log_parser import (
    MMDetectionLogParser,
    find_latest_log_file,
    find_latest_checkpoint,
)

__all__ = [
    "CurriculumConfig",
    "CurriculumTrainer",
    "MMDetectionLogParser",
    "find_latest_log_file",
    "find_latest_checkpoint",
]
