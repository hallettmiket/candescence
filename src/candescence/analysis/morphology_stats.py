"""
Purpose: Statistical analysis of morphology detection results
Author: Hallett Lab
Date: 2026-01-28
Input: Detection results from CandidaDetector
Output: Statistical summaries and metrics

Provides tools for analyzing morphology distributions and detection quality.
"""

from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd

from candescence.core.logging_config import get_logger
from candescence.core.morphology import (
    CLASS_NAMES,
    FILAMENTOUS_GRADES,
    NON_FILAMENTOUS_GRADES,
)
from candescence.detection.inference.detector import Detection

logger = get_logger("candescence.analysis.morphology_stats")


@dataclass
class ImageStats:
    """Statistics for a single image."""
    image_path: Path
    total_detections: int
    class_counts: Dict[str, int]
    mean_confidence: float
    filamentous_ratio: float


class MorphologyStats:
    """
    Statistical analysis of morphology detections.

    Computes class distributions, confidence metrics, and
    morphology ratios across detection results.

    Example:
        >>> from candescence.detection import CandidaDetector
        >>> detector = CandidaDetector()
        >>> detections = detector.detect("image.jpg")
        >>> stats = MorphologyStats()
        >>> stats.add_detections("image.jpg", detections)
        >>> print(stats.summary())
    """

    def __init__(self) -> None:
        """Initialize the statistics collector."""
        self._detections: List[Tuple[Path, List[Detection]]] = []
        self._image_stats: List[ImageStats] = []

    def add_detections(
        self,
        image_path: Union[str, Path],
        detections: List[Detection],
    ) -> ImageStats:
        """
        Add detections from an image to the statistics.

        Args:
            image_path: Path to the source image
            detections: List of Detection objects

        Returns:
            ImageStats for the added image
        """
        image_path = Path(image_path)
        self._detections.append((image_path, detections))

        # Compute image statistics
        class_counts = Counter(d.label_name for d in detections)
        mean_conf = np.mean([d.score for d in detections]) if detections else 0.0

        # Compute filamentous ratio
        filamentous_count = sum(
            class_counts.get(c, 0) for c in FILAMENTOUS_GRADES
        )
        total = len(detections)
        filamentous_ratio = filamentous_count / total if total > 0 else 0.0

        stats = ImageStats(
            image_path=image_path,
            total_detections=len(detections),
            class_counts=dict(class_counts),
            mean_confidence=float(mean_conf),
            filamentous_ratio=filamentous_ratio,
        )
        self._image_stats.append(stats)

        return stats

    def summary(self) -> Dict:
        """
        Get summary statistics across all images.

        Returns:
            Dictionary with summary statistics
        """
        if not self._detections:
            return {"error": "No detections added"}

        all_detections = [d for _, dets in self._detections for d in dets]

        # Class distribution
        class_counts = Counter(d.label_name for d in all_detections)

        # Confidence statistics
        confidences = [d.score for d in all_detections]

        # Per-image statistics
        detections_per_image = [len(dets) for _, dets in self._detections]

        return {
            "total_images": len(self._detections),
            "total_detections": len(all_detections),
            "class_distribution": dict(class_counts),
            "confidence_mean": float(np.mean(confidences)) if confidences else 0.0,
            "confidence_std": float(np.std(confidences)) if confidences else 0.0,
            "confidence_min": float(np.min(confidences)) if confidences else 0.0,
            "confidence_max": float(np.max(confidences)) if confidences else 0.0,
            "detections_per_image_mean": float(np.mean(detections_per_image)),
            "detections_per_image_std": float(np.std(detections_per_image)),
            "filamentous_ratio": self._compute_filamentous_ratio(all_detections),
        }

    def to_dataframe(self) -> pd.DataFrame:
        """
        Convert detections to a pandas DataFrame.

        Returns:
            DataFrame with one row per detection
        """
        rows = []
        for image_path, detections in self._detections:
            for det in detections:
                rows.append({
                    "image": str(image_path),
                    "image_name": image_path.name,
                    "class": det.label_name,
                    "class_id": det.label,
                    "confidence": det.score,
                    "x1": det.x1,
                    "y1": det.y1,
                    "x2": det.x2,
                    "y2": det.y2,
                    "width": det.width,
                    "height": det.height,
                    "area": det.area,
                    "is_filamentous": det.label_name in FILAMENTOUS_GRADES,
                })

        return pd.DataFrame(rows)

    def image_summary_dataframe(self) -> pd.DataFrame:
        """
        Get per-image summary as DataFrame.

        Returns:
            DataFrame with one row per image
        """
        rows = []
        for stats in self._image_stats:
            row = {
                "image": str(stats.image_path),
                "image_name": stats.image_path.name,
                "total_detections": stats.total_detections,
                "mean_confidence": stats.mean_confidence,
                "filamentous_ratio": stats.filamentous_ratio,
            }
            # Add class counts
            for class_name in CLASS_NAMES:
                row[f"count_{class_name.replace(' ', '_')}"] = stats.class_counts.get(
                    class_name, 0
                )
            rows.append(row)

        return pd.DataFrame(rows)

    def class_distribution(self) -> pd.DataFrame:
        """
        Get class distribution summary.

        Returns:
            DataFrame with class counts and percentages
        """
        all_detections = [d for _, dets in self._detections for d in dets]
        class_counts = Counter(d.label_name for d in all_detections)
        total = len(all_detections)

        rows = []
        for class_name in CLASS_NAMES:
            count = class_counts.get(class_name, 0)
            rows.append({
                "class": class_name,
                "count": count,
                "percentage": 100.0 * count / total if total > 0 else 0.0,
                "is_filamentous": class_name in FILAMENTOUS_GRADES,
            })

        df = pd.DataFrame(rows)
        return df.sort_values("count", ascending=False).reset_index(drop=True)

    def _compute_filamentous_ratio(self, detections: List[Detection]) -> float:
        """Compute ratio of filamentous to total detections."""
        if not detections:
            return 0.0

        filamentous = sum(
            1 for d in detections if d.label_name in FILAMENTOUS_GRADES
        )
        return filamentous / len(detections)

    def save_summary(self, output_path: Union[str, Path]) -> None:
        """
        Save summary statistics to file.

        Args:
            output_path: Path to save summary (JSON or CSV based on extension)
        """
        import json

        output_path = Path(output_path)

        if output_path.suffix == ".json":
            with open(output_path, "w") as f:
                json.dump(self.summary(), f, indent=2)
        elif output_path.suffix == ".csv":
            self.to_dataframe().to_csv(output_path, index=False)
        else:
            raise ValueError(f"Unsupported format: {output_path.suffix}")

        logger.info(f"Saved summary to {output_path}")

    def clear(self) -> None:
        """Clear all collected statistics."""
        self._detections.clear()
        self._image_stats.clear()
