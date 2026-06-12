"""
Purpose: Detection evaluation utilities for computing metrics and visualizations
Author: Hallett Lab
Date: 2026-02-04

Provides:
- DetectionEvaluator: Computes mAP, precision, recall, F1, confusion matrix
- PredictionVisualizer: Side-by-side GT vs prediction visualization
- SampleBrowser: Browse best/worst predictions, FP/FN
"""

from candescence.detection.evaluation.evaluator import (
    DetectionEvaluator,
    EvaluationResults,
)
from candescence.detection.evaluation.visualizer import (
    PredictionVisualizer,
    SampleBrowser,
)

__all__ = [
    "DetectionEvaluator",
    "EvaluationResults",
    "PredictionVisualizer",
    "SampleBrowser",
]
