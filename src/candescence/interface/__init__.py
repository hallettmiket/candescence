"""
Candescence Interface Module.

Provides interactive exploration and training tools for Candescence models.

Components:
- CandescenceModel: Abstract interface for all models
- ModelRegistry: Discover and load trained models
- LatentExplorer: Interactive latent space visualization
- ImagePanel: Image display with metadata
- InterpolationTool: Latent space interpolation
- TrainingConfigPanel: Training configuration UI
- TrainingProgressPanel: Training progress visualization
- TrainingSummaryPanel: Post-training summary and save
- Data utilities: Preprocessing and quality assessment

Standalone Apps:
- latent_explorer_app: Interactive Streamlit app for latent space exploration
  Launch with: streamlit run src/candescence/interface/apps/latent_explorer_app.py

- training_app: Guided Tendril VAE training interface
  Launch with: streamlit run src/candescence/interface/apps/training_app.py
"""

from candescence.interface.base import CandescenceModel
from candescence.interface.model_loader import TLVModelWrapper, load_tlv_model
from candescence.interface.data_utils import (
    standardize_missing_values,
    clean_non_ascii,
    filter_labeled_samples,
    load_and_clean_metadata,
    load_morphology_labels,
    load_calb_master,
    get_calb_master_path,
    DataQualityReport,
)

__all__ = [
    "CandescenceModel",
    "TLVModelWrapper",
    "load_tlv_model",
    "standardize_missing_values",
    "clean_non_ascii",
    "filter_labeled_samples",
    "load_and_clean_metadata",
    "load_morphology_labels",
    "load_calb_master",
    "get_calb_master_path",
    "DataQualityReport",
]
