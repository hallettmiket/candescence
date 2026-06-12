"""
Purpose: Core infrastructure for unified Candescence interface
Author: Hallett Lab
Date: 2026-02-04

Provides shared theme, components, and registries for all Candescence Streamlit apps.
"""

from candescence.interface.core.theme import THEME, apply_theme, get_subproject_color
from candescence.core.model_zoo import ModelZoo, ZooEntry
from candescence.core.dataset_zoo import DatasetZoo, DatasetEntry

__all__ = [
    "THEME",
    "apply_theme",
    "get_subproject_color",
    "ModelZoo",
    "ZooEntry",
    "DatasetZoo",
    "DatasetEntry",
]
