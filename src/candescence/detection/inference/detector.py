"""
Purpose: FCOS-based Candida morphology detector
Author: Hallett Lab
Date: 2026-01-28
Input: Microscopy images
Output: Bounding boxes with morphology class labels

Provides inference wrapper for trained MMDETECTION FCOS models.
"""

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple, Union

import numpy as np
import torch

from candescence.core.settings import legacy_refined_root

from candescence.core.logging_config import get_logger
from candescence.core.morphology import CLASS_NAMES, INDEX_TO_CLASS

logger = get_logger("candescence.detection.detector")


@dataclass
class Detection:
    """
    Single detection result.

    Attributes:
        bbox: Bounding box as (x1, y1, x2, y2)
        label: Class index (0-14)
        label_name: Human-readable class name
        score: Confidence score (0-1)
    """
    bbox: Tuple[float, float, float, float]
    label: int
    label_name: str
    score: float

    @property
    def x1(self) -> float:
        return self.bbox[0]

    @property
    def y1(self) -> float:
        return self.bbox[1]

    @property
    def x2(self) -> float:
        return self.bbox[2]

    @property
    def y2(self) -> float:
        return self.bbox[3]

    @property
    def width(self) -> float:
        return self.x2 - self.x1

    @property
    def height(self) -> float:
        return self.y2 - self.y1

    @property
    def center(self) -> Tuple[float, float]:
        return ((self.x1 + self.x2) / 2, (self.y1 + self.y2) / 2)

    @property
    def area(self) -> float:
        return self.width * self.height


class CandidaDetector:
    """
    FCOS-based Candida morphology detector.

    Uses MMDETECTION framework for inference on microscopy images.

    Example:
        >>> detector = CandidaDetector(
        ...     config_path="/path/to/config.py",
        ...     checkpoint_path="/path/to/model.pth"
        ... )
        >>> detections = detector.detect("image.jpg", threshold=0.5)
        >>> for det in detections:
        ...     print(f"{det.label_name}: {det.score:.2f}")
    """

    # Default paths for the production Varasana detection model. The legacy
    # model currently lives in the candescence_master refined tree; the base is
    # resolved from the settings layer (CANDESCENCE_LEGACY_REFINED) so external
    # installs can point elsewhere (or pass explicit paths to the constructor).
    _LEGACY_REFINED = legacy_refined_root()
    DEFAULT_CONFIG = (
        _LEGACY_REFINED / "candescence_master/production/varasana/config.py"
    )
    DEFAULT_CHECKPOINT = (
        _LEGACY_REFINED / "candescence_master/production/varasana/model.pth"
    )

    def __init__(
        self,
        config_path: Optional[Union[str, Path]] = None,
        checkpoint_path: Optional[Union[str, Path]] = None,
        device: Optional[str] = None,
    ) -> None:
        """
        Initialize the detector.

        Args:
            config_path: Path to MMDETECTION config file.
                        Defaults to production model config.
            checkpoint_path: Path to model checkpoint (.pth).
                            Defaults to production model.
            device: Device to run inference on ('cuda:0', 'cpu', etc.).
                   Defaults to CUDA if available.
        """
        # Import MMDETECTION components (lazy import to avoid startup cost)
        try:
            from mmcv import Config
            from mmcv.parallel import collate
            from mmcv.runner import load_checkpoint
            from mmdet.datasets.pipelines import Compose
            from mmdet.models import build_detector
        except ImportError as e:
            raise ImportError(
                "MMDETECTION is required for detection. "
                "Install with: pip install mmdet mmcv-full"
            ) from e

        # Set paths
        config_path = Path(config_path) if config_path else self.DEFAULT_CONFIG
        checkpoint_path = Path(checkpoint_path) if checkpoint_path else self.DEFAULT_CHECKPOINT

        if not config_path.exists():
            raise FileNotFoundError(f"Config file not found: {config_path}")
        if not checkpoint_path.exists():
            raise FileNotFoundError(f"Checkpoint not found: {checkpoint_path}")

        # Determine device
        if device is None:
            device = "cuda:0" if torch.cuda.is_available() else "cpu"
        self.device = device

        logger.info(f"Loading detector from {checkpoint_path}")
        logger.info(f"Using device: {device}")

        # Load config
        self.cfg = Config.fromfile(str(config_path))

        # Build model
        self.model = build_detector(
            self.cfg.model,
            train_cfg=self.cfg.get('train_cfg'),
            test_cfg=self.cfg.get('test_cfg')
        )

        # Load checkpoint
        checkpoint = load_checkpoint(self.model, str(checkpoint_path))
        self.model.CLASSES = checkpoint.get('meta', {}).get('CLASSES', CLASS_NAMES)
        self.model.to(device)
        self.model.eval()

        # Setup test pipeline
        self.test_pipeline = Compose(self.cfg.data.val.pipeline)
        self._collate = collate

        logger.info(f"Detector loaded with {len(self.model.CLASSES)} classes")

    def detect(
        self,
        image: Union[str, Path, np.ndarray],
        threshold: float = 0.5,
        class_filter: Optional[List[str]] = None,
    ) -> List[Detection]:
        """
        Detect morphologies in an image.

        Args:
            image: Path to image file or numpy array (H, W, C)
            threshold: Confidence threshold (0-1)
            class_filter: Optional list of class names to return.
                         If None, returns all classes.

        Returns:
            List of Detection objects sorted by confidence (highest first)
        """
        # Prepare input
        if isinstance(image, (str, Path)):
            data = dict(img_info=dict(filename=str(image)), img_prefix=None)
        else:
            # For numpy array input, save temporarily
            import tempfile
            import cv2
            with tempfile.NamedTemporaryFile(suffix='.png', delete=False) as f:
                cv2.imwrite(f.name, image)
                data = dict(img_info=dict(filename=f.name), img_prefix=None)

        # Run pipeline
        data = self.test_pipeline(data)
        data = self._collate([data], samples_per_gpu=1)

        # Move to device
        if self.device != 'cpu':
            data['img'][0] = data['img'][0].to(self.device)

        # Inference
        with torch.no_grad():
            features = self.model.extract_feat(data['img'][0])
            outs = self.model.bbox_head(features)
            bbox_list = self.model.bbox_head.get_bboxes(
                *outs,
                data['img_metas'][0].data[0],
                rescale=False
            )

        # Parse results
        bboxes = bbox_list[0][0].cpu().numpy()
        labels = bbox_list[0][1].cpu().numpy()

        detections = []
        for i in range(len(bboxes)):
            score = bboxes[i, 4]
            if score < threshold:
                continue

            label_idx = int(labels[i])
            label_name = INDEX_TO_CLASS.get(label_idx, f"Unknown_{label_idx}")

            # Apply class filter
            if class_filter is not None and label_name not in class_filter:
                continue

            det = Detection(
                bbox=(
                    float(bboxes[i, 0]),
                    float(bboxes[i, 1]),
                    float(bboxes[i, 2]),
                    float(bboxes[i, 3]),
                ),
                label=label_idx,
                label_name=label_name,
                score=float(score),
            )
            detections.append(det)

        # Sort by confidence
        detections.sort(key=lambda x: x.score, reverse=True)

        logger.debug(f"Found {len(detections)} detections above threshold {threshold}")
        return detections

    def detect_batch(
        self,
        images: List[Union[str, Path]],
        threshold: float = 0.5,
    ) -> List[List[Detection]]:
        """
        Detect morphologies in multiple images.

        Args:
            images: List of image paths
            threshold: Confidence threshold

        Returns:
            List of detection lists, one per image
        """
        results = []
        for img_path in images:
            detections = self.detect(img_path, threshold=threshold)
            results.append(detections)
        return results

    def visualize(
        self,
        image: Union[str, Path],
        threshold: float = 0.5,
        show: bool = True,
        output_path: Optional[Union[str, Path]] = None,
    ) -> np.ndarray:
        """
        Visualize detections on an image.

        Args:
            image: Path to image file
            threshold: Confidence threshold
            show: Whether to display the image
            output_path: Optional path to save visualization

        Returns:
            Image array with drawn bounding boxes
        """
        import mmcv

        image_path = str(image)
        detections = self.detect(image_path, threshold=threshold)

        # Convert to mmcv format
        bboxes = np.array([[d.x1, d.y1, d.x2, d.y2, d.score] for d in detections])
        labels = np.array([d.label for d in detections])

        # Draw bounding boxes
        result_image = mmcv.imshow_det_bboxes(
            image_path,
            bboxes,
            labels,
            class_names=list(CLASS_NAMES),
            score_thr=threshold,
            wait_time=0,
            show=show,
            out_file=str(output_path) if output_path else None,
        )

        return result_image

    @property
    def classes(self) -> Tuple[str, ...]:
        """Get the class names supported by this detector."""
        return tuple(self.model.CLASSES)

    @property
    def num_classes(self) -> int:
        """Get the number of classes."""
        return len(self.model.CLASSES)
