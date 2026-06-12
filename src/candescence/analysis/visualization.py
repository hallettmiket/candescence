"""
Purpose: Visualization tools for Candida detection and analysis
Author: Hallett Lab
Date: 2026-01-28
Input: Detection results and statistics
Output: Matplotlib figures and saved images

Provides plotting utilities for morphology analysis.
"""

from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union

import numpy as np

from candescence.core.logging_config import get_logger
from candescence.core.morphology import CLASS_NAMES, FILAMENTOUS_GRADES
from candescence.detection.inference.detector import Detection

logger = get_logger("candescence.analysis.visualization")


# Color palette for morphology classes (RGB normalized)
CLASS_COLORS: Dict[str, Tuple[float, float, float]] = {
    "Yeast White": (0.9, 0.9, 0.9),
    "Budding White": (0.8, 0.8, 0.8),
    "Yeast Opaque": (0.6, 0.8, 0.6),
    "Budding Opaque": (0.5, 0.7, 0.5),
    "Yeast Gray": (0.5, 0.5, 0.5),
    "Budding Gray": (0.4, 0.4, 0.4),
    "Shmoo": (0.8, 0.6, 0.2),
    "Artifact": (0.3, 0.3, 0.3),
    "Unknown": (0.2, 0.2, 0.2),
    "Pseudohyphae": (0.2, 0.6, 0.8),
    "Hyphae": (0.1, 0.4, 0.8),
    "H-junction": (0.3, 0.3, 0.9),
    "P-junction": (0.4, 0.7, 0.9),
    "P-Start": (0.5, 0.8, 0.9),
    "H-Start": (0.2, 0.5, 0.9),
}


class DetectionVisualizer:
    """
    Visualization tools for detection results.

    Provides methods for plotting class distributions, confidence
    histograms, and annotated images.

    Example:
        >>> viz = DetectionVisualizer()
        >>> fig = viz.plot_class_distribution(detections)
        >>> fig.savefig("distribution.png")
    """

    def __init__(self, figsize: Tuple[int, int] = (10, 6)) -> None:
        """
        Initialize the visualizer.

        Args:
            figsize: Default figure size (width, height)
        """
        self.figsize = figsize

    def plot_class_distribution(
        self,
        detections: List[Detection],
        title: str = "Morphology Class Distribution",
        show_percentages: bool = True,
    ) -> "matplotlib.figure.Figure":
        """
        Plot bar chart of class distribution.

        Args:
            detections: List of Detection objects
            title: Plot title
            show_percentages: Whether to show percentage labels

        Returns:
            Matplotlib figure
        """
        import matplotlib.pyplot as plt
        from collections import Counter

        class_counts = Counter(d.label_name for d in detections)
        total = len(detections)

        # Order by class definition
        classes = [c for c in CLASS_NAMES if class_counts.get(c, 0) > 0]
        counts = [class_counts[c] for c in classes]
        colors = [CLASS_COLORS.get(c, (0.5, 0.5, 0.5)) for c in classes]

        fig, ax = plt.subplots(figsize=self.figsize)
        bars = ax.bar(range(len(classes)), counts, color=colors, edgecolor='black')

        ax.set_xticks(range(len(classes)))
        ax.set_xticklabels(classes, rotation=45, ha='right')
        ax.set_xlabel("Morphology Class")
        ax.set_ylabel("Count")
        ax.set_title(title)

        if show_percentages:
            for bar, count in zip(bars, counts):
                pct = 100.0 * count / total if total > 0 else 0
                ax.text(
                    bar.get_x() + bar.get_width() / 2,
                    bar.get_height() + 0.5,
                    f"{pct:.1f}%",
                    ha='center',
                    va='bottom',
                    fontsize=8,
                )

        plt.tight_layout()
        return fig

    def plot_confidence_histogram(
        self,
        detections: List[Detection],
        bins: int = 20,
        title: str = "Detection Confidence Distribution",
    ) -> "matplotlib.figure.Figure":
        """
        Plot histogram of detection confidences.

        Args:
            detections: List of Detection objects
            bins: Number of histogram bins
            title: Plot title

        Returns:
            Matplotlib figure
        """
        import matplotlib.pyplot as plt

        confidences = [d.score for d in detections]

        fig, ax = plt.subplots(figsize=self.figsize)
        ax.hist(confidences, bins=bins, edgecolor='black', alpha=0.7)
        ax.axvline(
            np.mean(confidences),
            color='red',
            linestyle='--',
            label=f"Mean: {np.mean(confidences):.3f}",
        )
        ax.set_xlabel("Confidence Score")
        ax.set_ylabel("Count")
        ax.set_title(title)
        ax.legend()

        plt.tight_layout()
        return fig

    def plot_size_distribution(
        self,
        detections: List[Detection],
        bins: int = 20,
        title: str = "Detection Size Distribution",
    ) -> "matplotlib.figure.Figure":
        """
        Plot histogram of detection sizes (areas).

        Args:
            detections: List of Detection objects
            bins: Number of histogram bins
            title: Plot title

        Returns:
            Matplotlib figure
        """
        import matplotlib.pyplot as plt

        areas = [d.area for d in detections]

        fig, ax = plt.subplots(figsize=self.figsize)
        ax.hist(areas, bins=bins, edgecolor='black', alpha=0.7)
        ax.axvline(
            np.mean(areas),
            color='red',
            linestyle='--',
            label=f"Mean: {np.mean(areas):.0f}",
        )
        ax.set_xlabel("Bounding Box Area (pixels²)")
        ax.set_ylabel("Count")
        ax.set_title(title)
        ax.legend()

        plt.tight_layout()
        return fig

    def plot_filamentous_pie(
        self,
        detections: List[Detection],
        title: str = "Filamentous vs Non-Filamentous",
    ) -> "matplotlib.figure.Figure":
        """
        Plot pie chart of filamentous vs non-filamentous morphologies.

        Args:
            detections: List of Detection objects
            title: Plot title

        Returns:
            Matplotlib figure
        """
        import matplotlib.pyplot as plt

        filamentous = sum(1 for d in detections if d.label_name in FILAMENTOUS_GRADES)
        non_filamentous = len(detections) - filamentous

        fig, ax = plt.subplots(figsize=(8, 8))
        ax.pie(
            [non_filamentous, filamentous],
            labels=["Non-Filamentous", "Filamentous"],
            autopct="%1.1f%%",
            colors=[(0.7, 0.7, 0.7), (0.2, 0.5, 0.8)],
            explode=(0, 0.05),
        )
        ax.set_title(title)

        plt.tight_layout()
        return fig

    def annotate_image(
        self,
        image: Union[str, Path, np.ndarray],
        detections: List[Detection],
        threshold: float = 0.0,
        show_labels: bool = True,
        show_scores: bool = True,
        line_thickness: int = 2,
    ) -> np.ndarray:
        """
        Draw detection boxes on an image.

        Args:
            image: Image path or numpy array
            detections: List of Detection objects
            threshold: Minimum confidence to display
            show_labels: Whether to show class labels
            show_scores: Whether to show confidence scores
            line_thickness: Box line thickness

        Returns:
            Annotated image as numpy array
        """
        import cv2

        # Load image if path
        if isinstance(image, (str, Path)):
            img = cv2.imread(str(image))
        else:
            img = image.copy()

        for det in detections:
            if det.score < threshold:
                continue

            # Get color (BGR for OpenCV)
            color = CLASS_COLORS.get(det.label_name, (0.5, 0.5, 0.5))
            color_bgr = tuple(int(c * 255) for c in reversed(color))

            # Draw box
            x1, y1, x2, y2 = int(det.x1), int(det.y1), int(det.x2), int(det.y2)
            cv2.rectangle(img, (x1, y1), (x2, y2), color_bgr, line_thickness)

            # Draw label
            if show_labels or show_scores:
                label_parts = []
                if show_labels:
                    label_parts.append(det.label_name)
                if show_scores:
                    label_parts.append(f"{det.score:.2f}")
                label = " ".join(label_parts)

                (label_w, label_h), baseline = cv2.getTextSize(
                    label, cv2.FONT_HERSHEY_SIMPLEX, 0.5, 1
                )
                cv2.rectangle(
                    img,
                    (x1, y1 - label_h - baseline - 5),
                    (x1 + label_w, y1),
                    color_bgr,
                    -1,
                )
                cv2.putText(
                    img,
                    label,
                    (x1, y1 - baseline - 2),
                    cv2.FONT_HERSHEY_SIMPLEX,
                    0.5,
                    (255, 255, 255),
                    1,
                )

        return img

    def save_annotated_image(
        self,
        image: Union[str, Path, np.ndarray],
        detections: List[Detection],
        output_path: Union[str, Path],
        **kwargs,
    ) -> None:
        """
        Annotate and save an image.

        Args:
            image: Image path or numpy array
            detections: List of Detection objects
            output_path: Path to save annotated image
            **kwargs: Additional arguments for annotate_image
        """
        import cv2

        annotated = self.annotate_image(image, detections, **kwargs)
        cv2.imwrite(str(output_path), annotated)
        logger.info(f"Saved annotated image to {output_path}")

    def create_detection_montage(
        self,
        crops: List["CellCrop"],
        grid_size: Optional[Tuple[int, int]] = None,
        title: str = "Detected Cells",
    ) -> "matplotlib.figure.Figure":
        """
        Create a montage of detected cell crops.

        Args:
            crops: List of CellCrop objects from CellExtractor
            grid_size: (rows, cols) for the grid. Auto-computed if None.
            title: Plot title

        Returns:
            Matplotlib figure
        """
        import matplotlib.pyplot as plt

        n = len(crops)
        if n == 0:
            fig, ax = plt.subplots()
            ax.text(0.5, 0.5, "No crops", ha='center', va='center')
            return fig

        # Compute grid size
        if grid_size is None:
            cols = int(np.ceil(np.sqrt(n)))
            rows = int(np.ceil(n / cols))
        else:
            rows, cols = grid_size

        fig, axes = plt.subplots(rows, cols, figsize=(2 * cols, 2 * rows))
        axes = np.atleast_2d(axes)

        for idx, ax in enumerate(axes.flat):
            if idx < n:
                crop = crops[idx]
                if len(crop.image.shape) == 2:
                    ax.imshow(crop.image, cmap='gray')
                else:
                    ax.imshow(crop.image[:, :, ::-1])  # BGR to RGB
                ax.set_title(f"{crop.label_name}\n{crop.score:.2f}", fontsize=8)
            ax.axis('off')

        fig.suptitle(title)
        plt.tight_layout()
        return fig
