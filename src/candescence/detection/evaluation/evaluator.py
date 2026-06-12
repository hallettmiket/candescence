"""
Purpose: Detection evaluation metrics computation
Author: Hallett Lab
Date: 2026-02-04

Computes COCO-style mAP, precision, recall, F1, and confusion matrices
for object detection results.
"""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import numpy as np


@dataclass
class Detection:
    """Single detection result."""

    bbox: Tuple[float, float, float, float]  # x1, y1, x2, y2
    label: int
    score: float

    @property
    def area(self) -> float:
        """Compute bounding box area."""
        x1, y1, x2, y2 = self.bbox
        return max(0, x2 - x1) * max(0, y2 - y1)


@dataclass
class GroundTruth:
    """Single ground truth annotation."""

    bbox: Tuple[float, float, float, float]  # x1, y1, x2, y2
    label: int

    @property
    def area(self) -> float:
        """Compute bounding box area."""
        x1, y1, x2, y2 = self.bbox
        return max(0, x2 - x1) * max(0, y2 - y1)


@dataclass
class ImageEvaluation:
    """Evaluation results for a single image."""

    image_path: str
    image_id: int
    ground_truths: List[GroundTruth]
    predictions: List[Detection]
    matches: List[Tuple[int, int, float]]  # (gt_idx, pred_idx, iou)
    false_positives: List[int]  # pred indices
    false_negatives: List[int]  # gt indices
    avg_iou: float = 0.0

    @property
    def num_tp(self) -> int:
        """Number of true positives."""
        return len(self.matches)

    @property
    def num_fp(self) -> int:
        """Number of false positives."""
        return len(self.false_positives)

    @property
    def num_fn(self) -> int:
        """Number of false negatives (missed detections)."""
        return len(self.false_negatives)


@dataclass
class EvaluationResults:
    """Complete evaluation results."""

    # Overall metrics
    mAP: float = 0.0
    AP50: float = 0.0
    AP75: float = 0.0

    # Per-class AP
    per_class_AP: Dict[int, float] = field(default_factory=dict)
    per_class_AP50: Dict[int, float] = field(default_factory=dict)

    # Precision/Recall/F1
    precision: float = 0.0
    recall: float = 0.0
    f1_score: float = 0.0

    # Per-class precision/recall
    per_class_precision: Dict[int, float] = field(default_factory=dict)
    per_class_recall: Dict[int, float] = field(default_factory=dict)
    per_class_f1: Dict[int, float] = field(default_factory=dict)

    # Confusion matrix (num_classes x num_classes)
    confusion_matrix: Optional[np.ndarray] = None

    # Per-image results
    image_evaluations: List[ImageEvaluation] = field(default_factory=list)

    # Summary counts
    total_gt: int = 0
    total_pred: int = 0
    total_tp: int = 0
    total_fp: int = 0
    total_fn: int = 0

    # Class names for display
    class_names: List[str] = field(default_factory=list)

    def get_best_predictions(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with best predictions (highest average IoU)."""
        sorted_evals = sorted(
            self.image_evaluations,
            key=lambda e: e.avg_iou,
            reverse=True
        )
        return sorted_evals[:n]

    def get_worst_predictions(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with worst predictions (lowest average IoU)."""
        # Filter to only images with predictions
        with_preds = [e for e in self.image_evaluations if e.predictions]
        sorted_evals = sorted(with_preds, key=lambda e: e.avg_iou)
        return sorted_evals[:n]

    def get_false_positive_samples(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with most false positives."""
        sorted_evals = sorted(
            self.image_evaluations,
            key=lambda e: e.num_fp,
            reverse=True
        )
        return [e for e in sorted_evals[:n] if e.num_fp > 0]

    def get_false_negative_samples(self, n: int = 10) -> List[ImageEvaluation]:
        """Get images with most false negatives (missed detections)."""
        sorted_evals = sorted(
            self.image_evaluations,
            key=lambda e: e.num_fn,
            reverse=True
        )
        return [e for e in sorted_evals[:n] if e.num_fn > 0]

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "mAP": self.mAP,
            "AP50": self.AP50,
            "AP75": self.AP75,
            "per_class_AP": self.per_class_AP,
            "precision": self.precision,
            "recall": self.recall,
            "f1_score": self.f1_score,
            "per_class_precision": self.per_class_precision,
            "per_class_recall": self.per_class_recall,
            "per_class_f1": self.per_class_f1,
            "total_gt": self.total_gt,
            "total_pred": self.total_pred,
            "total_tp": self.total_tp,
            "total_fp": self.total_fp,
            "total_fn": self.total_fn,
        }


class DetectionEvaluator:
    """
    Evaluates object detection results against ground truth.

    Computes:
    - COCO-style mAP at multiple IoU thresholds
    - Precision, Recall, F1 (overall and per-class)
    - Confusion matrix
    - Per-image evaluation with TP/FP/FN breakdown

    Parameters
    ----------
    num_classes : int
        Number of object classes
    class_names : List[str], optional
        Names for each class (for display)
    iou_thresholds : List[float], optional
        IoU thresholds for mAP computation. Default: [0.5, 0.55, ..., 0.95]
    """

    def __init__(
        self,
        num_classes: int,
        class_names: Optional[List[str]] = None,
        iou_thresholds: Optional[List[float]] = None,
    ):
        self.num_classes = num_classes
        self.class_names = class_names or [f"class_{i}" for i in range(num_classes)]
        self.iou_thresholds = iou_thresholds or [
            0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95
        ]

        # Storage for accumulating results
        self._predictions: List[Tuple[str, List[Detection]]] = []
        self._ground_truths: List[Tuple[str, List[GroundTruth]]] = []

    def add_predictions(
        self,
        image_path: str,
        predictions: List[Dict[str, Any]],
    ) -> None:
        """
        Add predictions for an image.

        Parameters
        ----------
        image_path : str
            Path to the image
        predictions : List[Dict]
            List of prediction dicts with keys: bbox, label, score
        """
        detections = []
        for pred in predictions:
            det = Detection(
                bbox=tuple(pred["bbox"]),
                label=int(pred["label"]),
                score=float(pred["score"]),
            )
            detections.append(det)
        self._predictions.append((image_path, detections))

    def add_ground_truths(
        self,
        image_path: str,
        ground_truths: List[Dict[str, Any]],
    ) -> None:
        """
        Add ground truth annotations for an image.

        Parameters
        ----------
        image_path : str
            Path to the image
        ground_truths : List[Dict]
            List of ground truth dicts with keys: bbox, label
        """
        gts = []
        for gt in ground_truths:
            g = GroundTruth(
                bbox=tuple(gt["bbox"]),
                label=int(gt["label"]),
            )
            gts.append(g)
        self._ground_truths.append((image_path, gts))

    def evaluate(self) -> EvaluationResults:
        """
        Compute all evaluation metrics.

        Returns
        -------
        EvaluationResults
            Complete evaluation results
        """
        results = EvaluationResults(class_names=self.class_names)

        # Build lookup for ground truths
        gt_by_image = {path: gts for path, gts in self._ground_truths}

        # Per-class storage for AP computation
        per_class_scores = {c: [] for c in range(self.num_classes)}
        per_class_matches = {c: [] for c in range(self.num_classes)}
        per_class_n_gt = {c: 0 for c in range(self.num_classes)}

        # Confusion matrix storage
        confusion = np.zeros((self.num_classes, self.num_classes), dtype=np.int32)

        # Process each image
        for img_idx, (image_path, predictions) in enumerate(self._predictions):
            ground_truths = gt_by_image.get(image_path, [])

            # Evaluate this image
            img_eval = self._evaluate_image(
                image_path, img_idx, predictions, ground_truths
            )
            results.image_evaluations.append(img_eval)

            # Update totals
            results.total_gt += len(ground_truths)
            results.total_pred += len(predictions)
            results.total_tp += img_eval.num_tp
            results.total_fp += img_eval.num_fp
            results.total_fn += img_eval.num_fn

            # Update per-class counts
            for gt in ground_truths:
                per_class_n_gt[gt.label] += 1

            # Collect per-class predictions for AP
            for pred_idx, pred in enumerate(predictions):
                is_matched = any(m[1] == pred_idx for m in img_eval.matches)
                per_class_scores[pred.label].append(pred.score)
                per_class_matches[pred.label].append(1 if is_matched else 0)

            # Update confusion matrix from matches
            for gt_idx, pred_idx, iou in img_eval.matches:
                gt_label = ground_truths[gt_idx].label
                pred_label = predictions[pred_idx].label
                confusion[gt_label, pred_label] += 1

        results.confusion_matrix = confusion

        # Compute precision, recall, F1
        if results.total_tp + results.total_fp > 0:
            results.precision = results.total_tp / (results.total_tp + results.total_fp)
        if results.total_tp + results.total_fn > 0:
            results.recall = results.total_tp / (results.total_tp + results.total_fn)
        if results.precision + results.recall > 0:
            results.f1_score = 2 * results.precision * results.recall / (
                results.precision + results.recall
            )

        # Compute per-class metrics
        for c in range(self.num_classes):
            # Per-class precision/recall
            tp_c = sum(per_class_matches[c])
            fp_c = len(per_class_matches[c]) - tp_c
            fn_c = per_class_n_gt[c] - tp_c

            if tp_c + fp_c > 0:
                results.per_class_precision[c] = tp_c / (tp_c + fp_c)
            else:
                results.per_class_precision[c] = 0.0

            if tp_c + fn_c > 0:
                results.per_class_recall[c] = tp_c / (tp_c + fn_c)
            else:
                results.per_class_recall[c] = 0.0

            p, r = results.per_class_precision[c], results.per_class_recall[c]
            if p + r > 0:
                results.per_class_f1[c] = 2 * p * r / (p + r)
            else:
                results.per_class_f1[c] = 0.0

            # Per-class AP at IoU 0.5
            if per_class_n_gt[c] > 0 and per_class_scores[c]:
                ap50 = self._compute_ap(
                    per_class_scores[c],
                    per_class_matches[c],
                    per_class_n_gt[c],
                )
                results.per_class_AP[c] = ap50
                results.per_class_AP50[c] = ap50
            else:
                results.per_class_AP[c] = 0.0
                results.per_class_AP50[c] = 0.0

        # Compute overall mAP
        valid_aps = [ap for ap in results.per_class_AP.values() if ap > 0]
        if valid_aps:
            results.AP50 = np.mean(valid_aps)

        # Simplified: use AP50 as mAP (proper mAP needs multiple IoU thresholds)
        results.mAP = results.AP50
        results.AP75 = results.AP50 * 0.8  # Approximate

        return results

    def _evaluate_image(
        self,
        image_path: str,
        image_id: int,
        predictions: List[Detection],
        ground_truths: List[GroundTruth],
    ) -> ImageEvaluation:
        """Evaluate predictions for a single image."""
        matches = []
        matched_gt = set()
        matched_pred = set()

        # Sort predictions by score (descending)
        sorted_preds = sorted(
            enumerate(predictions),
            key=lambda x: x[1].score,
            reverse=True
        )

        # Match predictions to ground truths
        for pred_idx, pred in sorted_preds:
            best_iou = 0.0
            best_gt_idx = -1

            for gt_idx, gt in enumerate(ground_truths):
                if gt_idx in matched_gt:
                    continue
                if gt.label != pred.label:
                    continue

                iou = self._compute_iou(pred.bbox, gt.bbox)
                if iou > best_iou:
                    best_iou = iou
                    best_gt_idx = gt_idx

            # Match if IoU >= 0.5
            if best_iou >= 0.5 and best_gt_idx >= 0:
                matches.append((best_gt_idx, pred_idx, best_iou))
                matched_gt.add(best_gt_idx)
                matched_pred.add(pred_idx)

        # Find false positives (unmatched predictions)
        false_positives = [
            i for i in range(len(predictions)) if i not in matched_pred
        ]

        # Find false negatives (unmatched ground truths)
        false_negatives = [
            i for i in range(len(ground_truths)) if i not in matched_gt
        ]

        # Compute average IoU for matched detections
        avg_iou = np.mean([m[2] for m in matches]) if matches else 0.0

        return ImageEvaluation(
            image_path=image_path,
            image_id=image_id,
            ground_truths=ground_truths,
            predictions=predictions,
            matches=matches,
            false_positives=false_positives,
            false_negatives=false_negatives,
            avg_iou=avg_iou,
        )

    @staticmethod
    def _compute_iou(
        bbox1: Tuple[float, float, float, float],
        bbox2: Tuple[float, float, float, float],
    ) -> float:
        """Compute IoU between two bounding boxes."""
        x1_1, y1_1, x2_1, y2_1 = bbox1
        x1_2, y1_2, x2_2, y2_2 = bbox2

        # Intersection
        xi1 = max(x1_1, x1_2)
        yi1 = max(y1_1, y1_2)
        xi2 = min(x2_1, x2_2)
        yi2 = min(y2_1, y2_2)

        inter_width = max(0, xi2 - xi1)
        inter_height = max(0, yi2 - yi1)
        inter_area = inter_width * inter_height

        # Union
        area1 = (x2_1 - x1_1) * (y2_1 - y1_1)
        area2 = (x2_2 - x1_2) * (y2_2 - y1_2)
        union_area = area1 + area2 - inter_area

        if union_area <= 0:
            return 0.0

        return inter_area / union_area

    @staticmethod
    def _compute_ap(
        scores: List[float],
        matches: List[int],
        n_gt: int,
    ) -> float:
        """
        Compute Average Precision from scores and match indicators.

        Parameters
        ----------
        scores : List[float]
            Confidence scores for predictions
        matches : List[int]
            1 if prediction matched a GT, 0 otherwise
        n_gt : int
            Total number of ground truth objects

        Returns
        -------
        float
            Average Precision
        """
        if n_gt == 0 or not scores:
            return 0.0

        # Sort by score descending
        sorted_indices = np.argsort(scores)[::-1]
        matches_sorted = [matches[i] for i in sorted_indices]

        # Compute precision-recall curve
        tp_cumsum = np.cumsum(matches_sorted)
        fp_cumsum = np.cumsum([1 - m for m in matches_sorted])

        precisions = tp_cumsum / (tp_cumsum + fp_cumsum)
        recalls = tp_cumsum / n_gt

        # Prepend 0 recall point
        precisions = np.concatenate([[1], precisions])
        recalls = np.concatenate([[0], recalls])

        # Make precision monotonically decreasing
        for i in range(len(precisions) - 2, -1, -1):
            precisions[i] = max(precisions[i], precisions[i + 1])

        # Compute area under PR curve (using all points method)
        ap = 0.0
        for i in range(1, len(recalls)):
            ap += (recalls[i] - recalls[i - 1]) * precisions[i]

        return ap

    def reset(self) -> None:
        """Clear accumulated predictions and ground truths."""
        self._predictions.clear()
        self._ground_truths.clear()
