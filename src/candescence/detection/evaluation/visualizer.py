"""
Purpose: Visualization utilities for detection evaluation
Author: Hallett Lab
Date: 2026-02-04

Provides:
- PredictionVisualizer: Side-by-side GT vs prediction visualization
- SampleBrowser: Browse best/worst predictions, FP/FN
"""

from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import numpy as np

try:
    from PIL import Image, ImageDraw, ImageFont
except ImportError:
    Image = None
    ImageDraw = None
    ImageFont = None

try:
    import plotly.graph_objects as go
    import plotly.express as px
    from plotly.subplots import make_subplots
except ImportError:
    go = None
    px = None
    make_subplots = None

from candescence.detection.evaluation.evaluator import (
    Detection,
    EvaluationResults,
    GroundTruth,
    ImageEvaluation,
)


# Color palette for classes (distinct colors)
CLASS_COLORS = [
    "#FF6B6B",  # Red
    "#4ECDC4",  # Teal
    "#45B7D1",  # Blue
    "#96CEB4",  # Green
    "#FFEAA7",  # Yellow
    "#DDA0DD",  # Plum
    "#98D8C8",  # Mint
    "#F7DC6F",  # Gold
    "#BB8FCE",  # Purple
    "#85C1E9",  # Light Blue
    "#F8B500",  # Orange
    "#00CED1",  # Dark Cyan
    "#FF69B4",  # Hot Pink
    "#32CD32",  # Lime Green
    "#FFB6C1",  # Light Pink
]


class PredictionVisualizer:
    """
    Visualizes detection predictions against ground truth.

    Provides side-by-side comparison with:
    - Ground truth bounding boxes (solid lines)
    - Predicted bounding boxes (dashed lines)
    - Color-coded by class
    - IoU and confidence annotations

    Parameters
    ----------
    class_names : List[str]
        Names for each class
    font_size : int
        Font size for labels (default: 12)
    line_width : int
        Bounding box line width (default: 2)
    """

    def __init__(
        self,
        class_names: List[str],
        font_size: int = 12,
        line_width: int = 2,
    ):
        self.class_names = class_names
        self.font_size = font_size
        self.line_width = line_width

        # Try to load a font
        self._font = None
        if ImageFont is not None:
            try:
                self._font = ImageFont.truetype(
                    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
                    font_size
                )
            except (OSError, IOError):
                try:
                    self._font = ImageFont.load_default()
                except Exception:
                    pass

    def draw_ground_truth(
        self,
        image: np.ndarray,
        ground_truths: List[GroundTruth],
    ) -> np.ndarray:
        """
        Draw ground truth bounding boxes on image.

        Parameters
        ----------
        image : np.ndarray
            Image array (H, W, 3) in RGB
        ground_truths : List[GroundTruth]
            List of ground truth annotations

        Returns
        -------
        np.ndarray
            Annotated image
        """
        if Image is None:
            return image

        pil_image = Image.fromarray(image)
        draw = ImageDraw.Draw(pil_image)

        for gt in ground_truths:
            color = CLASS_COLORS[gt.label % len(CLASS_COLORS)]
            x1, y1, x2, y2 = gt.bbox

            # Draw solid box for ground truth
            draw.rectangle(
                [x1, y1, x2, y2],
                outline=color,
                width=self.line_width,
            )

            # Draw label
            label_text = self.class_names[gt.label] if gt.label < len(self.class_names) else f"class_{gt.label}"
            self._draw_label(draw, label_text, x1, y1, color)

        return np.array(pil_image)

    def draw_predictions(
        self,
        image: np.ndarray,
        predictions: List[Detection],
        show_confidence: bool = True,
    ) -> np.ndarray:
        """
        Draw predicted bounding boxes on image.

        Parameters
        ----------
        image : np.ndarray
            Image array (H, W, 3) in RGB
        predictions : List[Detection]
            List of detection predictions
        show_confidence : bool
            Whether to show confidence scores

        Returns
        -------
        np.ndarray
            Annotated image
        """
        if Image is None:
            return image

        pil_image = Image.fromarray(image)
        draw = ImageDraw.Draw(pil_image)

        for pred in predictions:
            color = CLASS_COLORS[pred.label % len(CLASS_COLORS)]
            x1, y1, x2, y2 = pred.bbox

            # Draw dashed box for predictions (using multiple small lines)
            dash_length = 5
            self._draw_dashed_rect(draw, x1, y1, x2, y2, color, dash_length)

            # Draw label with confidence
            label_text = self.class_names[pred.label] if pred.label < len(self.class_names) else f"class_{pred.label}"
            if show_confidence:
                label_text = f"{label_text}: {pred.score:.2f}"
            self._draw_label(draw, label_text, x1, y1, color, offset_y=-20)

        return np.array(pil_image)

    def draw_comparison(
        self,
        image: np.ndarray,
        ground_truths: List[GroundTruth],
        predictions: List[Detection],
        matches: List[Tuple[int, int, float]],
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Create side-by-side comparison images.

        Parameters
        ----------
        image : np.ndarray
            Original image (H, W, 3) in RGB
        ground_truths : List[GroundTruth]
            Ground truth annotations
        predictions : List[Detection]
            Detection predictions
        matches : List[Tuple[int, int, float]]
            Matches as (gt_idx, pred_idx, iou)

        Returns
        -------
        Tuple[np.ndarray, np.ndarray]
            (GT annotated image, Prediction annotated image)
        """
        gt_image = self.draw_ground_truth(image.copy(), ground_truths)
        pred_image = self.draw_predictions(image.copy(), predictions)

        return gt_image, pred_image

    def create_comparison_figure(
        self,
        image: np.ndarray,
        evaluation: ImageEvaluation,
    ) -> Optional[Any]:
        """
        Create Plotly figure with side-by-side comparison.

        Parameters
        ----------
        image : np.ndarray
            Original image
        evaluation : ImageEvaluation
            Evaluation results for this image

        Returns
        -------
        plotly.graph_objects.Figure or None
            Comparison figure
        """
        if go is None or make_subplots is None:
            return None

        gt_image, pred_image = self.draw_comparison(
            image,
            evaluation.ground_truths,
            evaluation.predictions,
            evaluation.matches,
        )

        fig = make_subplots(
            rows=1, cols=2,
            subplot_titles=["Ground Truth", "Prediction"],
            horizontal_spacing=0.02,
        )

        # Add GT image
        fig.add_trace(
            go.Image(z=gt_image),
            row=1, col=1
        )

        # Add prediction image
        fig.add_trace(
            go.Image(z=pred_image),
            row=1, col=2
        )

        fig.update_layout(
            height=400,
            margin=dict(l=10, r=10, t=40, b=10),
            showlegend=False,
        )

        # Hide axes
        fig.update_xaxes(visible=False)
        fig.update_yaxes(visible=False)

        return fig

    def _draw_dashed_rect(
        self,
        draw: Any,
        x1: float,
        y1: float,
        x2: float,
        y2: float,
        color: str,
        dash_length: int = 5,
    ) -> None:
        """Draw a dashed rectangle."""
        # Top edge
        self._draw_dashed_line(draw, x1, y1, x2, y1, color, dash_length)
        # Bottom edge
        self._draw_dashed_line(draw, x1, y2, x2, y2, color, dash_length)
        # Left edge
        self._draw_dashed_line(draw, x1, y1, x1, y2, color, dash_length)
        # Right edge
        self._draw_dashed_line(draw, x2, y1, x2, y2, color, dash_length)

    def _draw_dashed_line(
        self,
        draw: Any,
        x1: float,
        y1: float,
        x2: float,
        y2: float,
        color: str,
        dash_length: int,
    ) -> None:
        """Draw a dashed line."""
        length = ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** 0.5
        if length == 0:
            return

        dx = (x2 - x1) / length * dash_length
        dy = (y2 - y1) / length * dash_length

        x, y = x1, y1
        draw_on = True
        while (x - x1) * (x2 - x1) + (y - y1) * (y2 - y1) < length ** 2:
            next_x = min(x + dx, x2) if dx > 0 else max(x + dx, x2)
            next_y = min(y + dy, y2) if dy > 0 else max(y + dy, y2)

            if draw_on:
                draw.line(
                    [(x, y), (next_x, next_y)],
                    fill=color,
                    width=self.line_width
                )

            x, y = next_x, next_y
            draw_on = not draw_on

    def _draw_label(
        self,
        draw: Any,
        text: str,
        x: float,
        y: float,
        color: str,
        offset_y: int = 0,
    ) -> None:
        """Draw a label with background."""
        if self._font:
            bbox = draw.textbbox((x, y + offset_y), text, font=self._font)
        else:
            # Estimate bbox
            bbox = (x, y + offset_y, x + len(text) * 7, y + offset_y + 12)

        # Draw background
        draw.rectangle(
            [bbox[0] - 2, bbox[1] - 2, bbox[2] + 2, bbox[3] + 2],
            fill=color,
        )

        # Draw text
        draw.text(
            (x, y + offset_y),
            text,
            fill="white",
            font=self._font,
        )


class SampleBrowser:
    """
    Browse detection samples by various criteria.

    Provides access to:
    - Random samples
    - Best predictions (highest IoU)
    - Worst predictions (lowest IoU)
    - False positives
    - False negatives

    Parameters
    ----------
    results : EvaluationResults
        Evaluation results to browse
    image_dir : Path
        Directory containing images
    """

    def __init__(
        self,
        results: EvaluationResults,
        image_dir: Optional[Path] = None,
    ):
        self.results = results
        self.image_dir = Path(image_dir) if image_dir else None
        self._visualizer = PredictionVisualizer(results.class_names)

    def get_random_samples(self, n: int = 10, seed: int = 42) -> List[ImageEvaluation]:
        """Get random sample of image evaluations."""
        rng = np.random.default_rng(seed)
        indices = rng.choice(
            len(self.results.image_evaluations),
            size=min(n, len(self.results.image_evaluations)),
            replace=False
        )
        return [self.results.image_evaluations[i] for i in indices]

    def get_best_predictions(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with best predictions (highest average IoU)."""
        return self.results.get_best_predictions(n)

    def get_worst_predictions(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with worst predictions (lowest average IoU)."""
        return self.results.get_worst_predictions(n)

    def get_false_positive_samples(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with most false positives."""
        return self.results.get_false_positive_samples(n)

    def get_false_negative_samples(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with most false negatives."""
        return self.results.get_false_negative_samples(n)

    def load_image(self, image_path: str) -> Optional[np.ndarray]:
        """Load an image from path."""
        if Image is None:
            return None

        path = Path(image_path)
        if not path.exists() and self.image_dir:
            path = self.image_dir / path.name

        if not path.exists():
            return None

        try:
            img = Image.open(path).convert("RGB")
            return np.array(img)
        except Exception:
            return None

    def get_sample_with_visualization(
        self,
        evaluation: ImageEvaluation,
    ) -> Dict[str, Any]:
        """
        Get sample data with visualization.

        Returns
        -------
        Dict with keys:
            - image_path: str
            - gt_image: np.ndarray (annotated)
            - pred_image: np.ndarray (annotated)
            - evaluation: ImageEvaluation
            - metrics: Dict with IoU, TP, FP, FN counts
        """
        image = self.load_image(evaluation.image_path)

        result = {
            "image_path": evaluation.image_path,
            "evaluation": evaluation,
            "metrics": {
                "avg_iou": evaluation.avg_iou,
                "num_gt": len(evaluation.ground_truths),
                "num_pred": len(evaluation.predictions),
                "num_tp": evaluation.num_tp,
                "num_fp": evaluation.num_fp,
                "num_fn": evaluation.num_fn,
            },
        }

        if image is not None:
            gt_image, pred_image = self._visualizer.draw_comparison(
                image,
                evaluation.ground_truths,
                evaluation.predictions,
                evaluation.matches,
            )
            result["gt_image"] = gt_image
            result["pred_image"] = pred_image
            result["original_image"] = image

        return result


def create_confusion_matrix_figure(
    results: EvaluationResults,
    normalize: bool = True,
) -> Optional[Any]:
    """
    Create Plotly confusion matrix heatmap.

    Parameters
    ----------
    results : EvaluationResults
        Evaluation results
    normalize : bool
        Whether to normalize by row (GT class)

    Returns
    -------
    plotly.graph_objects.Figure or None
    """
    if go is None or px is None:
        return None

    if results.confusion_matrix is None:
        return None

    cm = results.confusion_matrix.astype(float)

    if normalize:
        row_sums = cm.sum(axis=1, keepdims=True)
        row_sums[row_sums == 0] = 1  # Avoid division by zero
        cm = cm / row_sums

    # Filter to classes with actual data
    active_classes = np.where(cm.sum(axis=0) + cm.sum(axis=1) > 0)[0]
    if len(active_classes) == 0:
        return None

    cm_filtered = cm[np.ix_(active_classes, active_classes)]
    class_names_filtered = [results.class_names[i] for i in active_classes]

    fig = go.Figure(data=go.Heatmap(
        z=cm_filtered,
        x=class_names_filtered,
        y=class_names_filtered,
        colorscale="Blues",
        text=np.round(cm_filtered, 2) if normalize else cm_filtered.astype(int),
        texttemplate="%{text}",
        textfont={"size": 10},
        hovertemplate="GT: %{y}<br>Pred: %{x}<br>Value: %{z:.2f}<extra></extra>",
    ))

    fig.update_layout(
        title="Confusion Matrix" + (" (Normalized)" if normalize else ""),
        xaxis_title="Predicted Class",
        yaxis_title="Ground Truth Class",
        height=500,
        xaxis=dict(tickangle=45),
    )

    return fig


def create_precision_recall_chart(
    results: EvaluationResults,
) -> Optional[Any]:
    """
    Create per-class precision/recall bar chart.

    Parameters
    ----------
    results : EvaluationResults
        Evaluation results

    Returns
    -------
    plotly.graph_objects.Figure or None
    """
    if go is None:
        return None

    # Filter to classes with data
    active_classes = [
        c for c in range(len(results.class_names))
        if results.per_class_precision.get(c, 0) > 0 or results.per_class_recall.get(c, 0) > 0
    ]

    if not active_classes:
        return None

    class_names = [results.class_names[c] for c in active_classes]
    precisions = [results.per_class_precision.get(c, 0) for c in active_classes]
    recalls = [results.per_class_recall.get(c, 0) for c in active_classes]
    f1_scores = [results.per_class_f1.get(c, 0) for c in active_classes]

    fig = go.Figure(data=[
        go.Bar(name="Precision", x=class_names, y=precisions, marker_color="#4ECDC4"),
        go.Bar(name="Recall", x=class_names, y=recalls, marker_color="#FF6B6B"),
        go.Bar(name="F1", x=class_names, y=f1_scores, marker_color="#45B7D1"),
    ])

    fig.update_layout(
        title="Per-Class Precision, Recall, and F1",
        xaxis_title="Class",
        yaxis_title="Score",
        barmode="group",
        height=400,
        xaxis=dict(tickangle=45),
        yaxis=dict(range=[0, 1]),
    )

    return fig
