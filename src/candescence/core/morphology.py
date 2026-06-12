"""
Purpose: Shared morphology class definitions for Candescence
Author: Hallett Lab
Date: 2026-01-28
Input: None
Output: Morphology class constants and utilities

This module provides consistent morphology class definitions used across
all Candescence subprojects (detection, TLV, analysis).
"""

from dataclasses import dataclass
from enum import IntEnum
from typing import Dict, List, Tuple


class MorphologyClass(IntEnum):
    """
    Candida morphology classes.

    The ordering follows the curriculum learning progression:
    white → opaque → gray → shmoo → pseudohyphae → hyphae
    """
    YEAST_WHITE = 0
    BUDDING_WHITE = 1
    YEAST_OPAQUE = 2
    BUDDING_OPAQUE = 3
    YEAST_GRAY = 4
    BUDDING_GRAY = 5
    SHMOO = 6
    ARTIFACT = 7
    UNKNOWN = 8
    PSEUDOHYPHAE = 9
    HYPHAE = 10
    H_JUNCTION = 11
    P_JUNCTION = 12
    P_START = 13
    H_START = 14


# Class name mappings
CLASS_NAMES: Tuple[str, ...] = (
    "Yeast White",
    "Budding White",
    "Yeast Opaque",
    "Budding Opaque",
    "Yeast Gray",
    "Budding Gray",
    "Shmoo",
    "Artifact",
    "Unknown",
    "Pseudohyphae",
    "Hyphae",
    "H-junction",
    "P-junction",
    "P-Start",
    "H-Start",
)

# Bidirectional mappings
CLASS_TO_INDEX: Dict[str, int] = {name: idx for idx, name in enumerate(CLASS_NAMES)}
INDEX_TO_CLASS: Dict[int, str] = {idx: name for idx, name in enumerate(CLASS_NAMES)}

# Number of classes
NUM_CLASSES: int = len(CLASS_NAMES)


@dataclass
class CurriculumStage:
    """Definition of a curriculum learning stage."""
    name: str
    classes: List[str]
    cumulative_classes: List[str]


# Curriculum learning stages (cumulative)
CURRICULUM_STAGES: List[CurriculumStage] = [
    CurriculumStage(
        name="white",
        classes=["Yeast White", "Budding White"],
        cumulative_classes=["Yeast White", "Budding White"]
    ),
    CurriculumStage(
        name="opaque",
        classes=["Yeast Opaque", "Budding Opaque"],
        cumulative_classes=[
            "Yeast White", "Budding White",
            "Yeast Opaque", "Budding Opaque"
        ]
    ),
    CurriculumStage(
        name="gray",
        classes=["Yeast Gray", "Budding Gray"],
        cumulative_classes=[
            "Yeast White", "Budding White",
            "Yeast Opaque", "Budding Opaque",
            "Yeast Gray", "Budding Gray"
        ]
    ),
    CurriculumStage(
        name="shmoo",
        classes=["Shmoo"],
        cumulative_classes=[
            "Yeast White", "Budding White",
            "Yeast Opaque", "Budding Opaque",
            "Yeast Gray", "Budding Gray",
            "Shmoo"
        ]
    ),
    CurriculumStage(
        name="pseudohyphae",
        classes=["Pseudohyphae", "P-Start", "P-junction"],
        cumulative_classes=[
            "Yeast White", "Budding White",
            "Yeast Opaque", "Budding Opaque",
            "Yeast Gray", "Budding Gray",
            "Shmoo",
            "Pseudohyphae", "P-Start", "P-junction"
        ]
    ),
    CurriculumStage(
        name="hyphae",
        classes=["Hyphae", "H-Start", "H-junction"],
        cumulative_classes=[
            "Yeast White", "Budding White",
            "Yeast Opaque", "Budding Opaque",
            "Yeast Gray", "Budding Gray",
            "Shmoo",
            "Pseudohyphae", "P-Start", "P-junction",
            "Hyphae", "H-Start", "H-junction"
        ]
    ),
]


def get_curriculum_stage(stage_name: str) -> CurriculumStage:
    """
    Get curriculum stage by name.

    Args:
        stage_name: Name of the stage (white, opaque, gray, shmoo, pseudohyphae, hyphae)

    Returns:
        CurriculumStage object

    Raises:
        ValueError: If stage name is not found
    """
    for stage in CURRICULUM_STAGES:
        if stage.name == stage_name:
            return stage
    raise ValueError(f"Unknown curriculum stage: {stage_name}")


def get_class_indices(class_names: List[str]) -> List[int]:
    """
    Convert class names to indices.

    Args:
        class_names: List of class name strings

    Returns:
        List of class indices
    """
    return [CLASS_TO_INDEX[name] for name in class_names]


# Grade groupings for analysis
YEAST_GRADES = ["Yeast White", "Yeast Opaque", "Yeast Gray"]
BUDDING_GRADES = ["Budding White", "Budding Opaque", "Budding Gray"]
FILAMENTOUS_GRADES = ["Pseudohyphae", "Hyphae", "P-Start", "H-Start", "P-junction", "H-junction"]
NON_FILAMENTOUS_GRADES = YEAST_GRADES + BUDDING_GRADES + ["Shmoo"]
