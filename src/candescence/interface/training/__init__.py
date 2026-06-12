"""
Purpose: Training utilities for Candescence Streamlit interface
Author: Hallett Lab
Date: 2026-02-01

Provides training state management and Streamlit-compatible training wrappers
for both TLV (VAE) and Varasana (FCOS detection) training.
"""

from candescence.interface.training.training_state import (
    TrainingState,
    TrainingStateManager,
)
from candescence.interface.training.streamlit_trainer import StreamlitTrainer
from candescence.interface.training.detection_state import (
    DetectionTrainingState,
    DetectionStateManager,
)
from candescence.interface.training.detection_trainer import (
    DetectionConfig,
    StreamlitDetectionTrainer,
)

__all__ = [
    # TLV (VAE) training
    "TrainingState",
    "TrainingStateManager",
    "StreamlitTrainer",
    # Varasana (Detection) training
    "DetectionTrainingState",
    "DetectionStateManager",
    "DetectionConfig",
    "StreamlitDetectionTrainer",
]
