"""
Purpose: MMDETECTION dataset adapters for Candida morphology detection
Author: Hallett Lab
Date: 2026-01-28
Input: Pickle annotation files with bounding boxes and labels
Output: MMDETECTION-compatible dataset objects

NOTE: This file must be copied to $MMDETECTION/mmdet/datasets/ and
registered with MMDETECTION for training. For inference, use the
CandidaDetector class which handles model loading directly.
"""

import pickle
from typing import Any, Dict, List

from mmdet.datasets.builder import DATASETS
from mmdet.datasets.custom import CustomDataset

from candescence.core.morphology import CLASS_NAMES, CLASS_TO_INDEX


@DATASETS.register_module()
class CandidaDataset(CustomDataset):
    """
    Full Candida morphology dataset with all 15 classes.

    Attributes:
        CLASSES: Tuple of class names in order
        cat2label: Mapping from class name to index
    """

    CLASSES = CLASS_NAMES

    def load_annotations(self, ann_file: str) -> List[Dict[str, Any]]:
        """
        Load annotations from pickle file.

        Args:
            ann_file: Path to pickle annotation file

        Returns:
            List of annotation dictionaries
        """
        self.cat2label = CLASS_TO_INDEX.copy()

        data = []
        with open(ann_file, "rb") as f:
            while True:
                try:
                    data.append(pickle.load(f))
                except EOFError:
                    break
        return data[0]

    def get_ann_info(self, idx: int) -> Dict[str, Any]:
        """Get annotation info for a single image."""
        return self.data_infos[idx]["ann"]


@DATASETS.register_module()
class FilamentousDataset(CustomDataset):
    """
    Dataset for all morphology classes including filamentous types.

    This is the primary dataset used for full curriculum training.
    """

    CLASSES = CLASS_NAMES

    def load_annotations(self, ann_file: str) -> List[Dict[str, Any]]:
        """Load annotations from pickle file."""
        self.cat2label = CLASS_TO_INDEX.copy()

        data = []
        with open(ann_file, "rb") as f:
            while True:
                try:
                    data.append(pickle.load(f))
                except EOFError:
                    break
        return data[0]

    def get_ann_info(self, idx: int) -> Dict[str, Any]:
        """Get annotation info for a single image."""
        return self.data_infos[idx]["ann"]


@DATASETS.register_module()
class CurriculumDataset(CustomDataset):
    """
    Dataset for curriculum learning stages.

    Supports progressive training from simpler to more complex morphologies.
    """

    CLASSES = CLASS_NAMES

    def load_annotations(self, ann_file: str) -> List[Dict[str, Any]]:
        """Load annotations from pickle file."""
        self.cat2label = CLASS_TO_INDEX.copy()

        data = []
        with open(ann_file, "rb") as f:
            while True:
                try:
                    data.append(pickle.load(f))
                except EOFError:
                    break
        return data[0]

    def get_ann_info(self, idx: int) -> Dict[str, Any]:
        """Get annotation info for a single image."""
        return self.data_infos[idx]["ann"]


@DATASETS.register_module()
class NonFilamentousDataset(CustomDataset):
    """
    Dataset for non-filamentous morphologies only.

    Classes: Yeast White, Budding White, Yeast Opaque, Budding Opaque,
             Yeast Gray, Budding Gray, Shmoo, Artifact, Unknown
    """

    CLASSES = (
        "Yeast White",
        "Budding White",
        "Yeast Opaque",
        "Budding Opaque",
        "Yeast Gray",
        "Budding Gray",
        "Shmoo",
        "Artifact",
        "Unknown",
    )

    def load_annotations(self, ann_file: str) -> List[Dict[str, Any]]:
        """Load annotations from pickle file."""
        self.cat2label = {name: idx for idx, name in enumerate(self.CLASSES)}

        data = []
        with open(ann_file, "rb") as f:
            while True:
                try:
                    data.append(pickle.load(f))
                except EOFError:
                    break
        return data[0]

    def get_ann_info(self, idx: int) -> Dict[str, Any]:
        """Get annotation info for a single image."""
        return self.data_infos[idx]["ann"]
