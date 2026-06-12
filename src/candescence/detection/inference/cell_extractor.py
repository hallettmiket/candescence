"""
Purpose: Extract individual cell images from detections
Author: Hallett Lab
Date: 2026-01-28
Input: Detection results and source images
Output: Cropped and resized cell images for VAE analysis

Provides the bridge between detection and downstream VAE analysis.
"""

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple, Union

import numpy as np

from candescence.core.logging_config import get_logger
from candescence.detection.inference.detector import CandidaDetector, Detection

logger = get_logger("candescence.detection.cell_extractor")


@dataclass
class CellCrop:
    """
    Extracted cell image with metadata.

    Attributes:
        image: Cropped cell image as numpy array (H, W, C) or (H, W)
        label: Class index
        label_name: Human-readable class name
        score: Detection confidence
        source_image: Path to source image
        bbox: Original bounding box in source image
    """
    image: np.ndarray
    label: int
    label_name: str
    score: float
    source_image: Optional[Path] = None
    bbox: Optional[Tuple[float, float, float, float]] = None

    @property
    def shape(self) -> Tuple[int, ...]:
        return self.image.shape


class CellExtractor:
    """
    Extract individual cell images from detections.

    Crops detected cells from microscopy images and resizes them
    to a standard size for downstream VAE analysis.

    Example:
        >>> detector = CandidaDetector()
        >>> extractor = CellExtractor(detector, output_size=128)
        >>> crops = extractor.extract("plate_image.jpg", threshold=0.5)
        >>> for crop in crops:
        ...     print(f"{crop.label_name}: {crop.shape}")
    """

    def __init__(
        self,
        detector: Optional[CandidaDetector] = None,
        output_size: int = 128,
        grayscale: bool = False,
        padding: float = 0.0,
    ) -> None:
        """
        Initialize the cell extractor.

        Args:
            detector: CandidaDetector instance. If None, creates default.
            output_size: Size to resize crops to (square).
            grayscale: Whether to convert crops to grayscale.
            padding: Fraction of bbox size to add as padding (0.0-0.5).
        """
        self.detector = detector or CandidaDetector()
        self.output_size = output_size
        self.grayscale = grayscale
        self.padding = padding

        logger.info(
            f"CellExtractor initialized: size={output_size}, "
            f"grayscale={grayscale}, padding={padding}"
        )

    def extract(
        self,
        image: Union[str, Path, np.ndarray],
        threshold: float = 0.5,
        class_filter: Optional[List[str]] = None,
        min_size: int = 10,
    ) -> List[CellCrop]:
        """
        Extract cell crops from an image.

        Args:
            image: Path to image or numpy array
            threshold: Detection confidence threshold
            class_filter: Optional list of class names to include
            min_size: Minimum bbox dimension to include

        Returns:
            List of CellCrop objects
        """
        # Load image if path
        if isinstance(image, (str, Path)):
            image_path = Path(image)
            img_array = self._load_image(image_path)
        else:
            image_path = None
            img_array = image

        # Get detections
        detections = self.detector.detect(
            image if isinstance(image, (str, Path)) else img_array,
            threshold=threshold,
            class_filter=class_filter,
        )

        # Extract crops
        crops = []
        for det in detections:
            # Skip small detections
            if det.width < min_size or det.height < min_size:
                continue

            crop = self._extract_crop(img_array, det, image_path)
            if crop is not None:
                crops.append(crop)

        logger.info(f"Extracted {len(crops)} cell crops from image")
        return crops

    def extract_batch(
        self,
        images: List[Union[str, Path]],
        threshold: float = 0.5,
        class_filter: Optional[List[str]] = None,
    ) -> List[List[CellCrop]]:
        """
        Extract cell crops from multiple images.

        Args:
            images: List of image paths
            threshold: Detection confidence threshold
            class_filter: Optional list of class names to include

        Returns:
            List of crop lists, one per image
        """
        results = []
        for img_path in images:
            crops = self.extract(
                img_path,
                threshold=threshold,
                class_filter=class_filter,
            )
            results.append(crops)
        return results

    def extract_to_directory(
        self,
        images: List[Union[str, Path]],
        output_dir: Union[str, Path],
        threshold: float = 0.5,
        class_filter: Optional[List[str]] = None,
        filename_format: str = "{source}_{idx}_{label}.png",
    ) -> int:
        """
        Extract cells and save to directory.

        Args:
            images: List of image paths
            output_dir: Directory to save crops
            threshold: Detection confidence threshold
            class_filter: Optional list of class names
            filename_format: Format string for output filenames.
                           Supports: {source}, {idx}, {label}, {score}

        Returns:
            Total number of crops saved
        """
        import cv2

        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        total_saved = 0
        for img_path in images:
            img_path = Path(img_path)
            crops = self.extract(
                img_path,
                threshold=threshold,
                class_filter=class_filter,
            )

            for idx, crop in enumerate(crops):
                filename = filename_format.format(
                    source=img_path.stem,
                    idx=idx,
                    label=crop.label_name.replace(" ", "_"),
                    score=f"{crop.score:.2f}",
                )
                output_path = output_dir / filename
                cv2.imwrite(str(output_path), crop.image)
                total_saved += 1

        logger.info(f"Saved {total_saved} crops to {output_dir}")
        return total_saved

    def _load_image(self, path: Path) -> np.ndarray:
        """Load image from path."""
        import cv2
        img = cv2.imread(str(path))
        if img is None:
            raise ValueError(f"Failed to load image: {path}")
        return img

    def _extract_crop(
        self,
        image: np.ndarray,
        detection: Detection,
        source_path: Optional[Path] = None,
    ) -> Optional[CellCrop]:
        """
        Extract a single crop from an image.

        Args:
            image: Source image array
            detection: Detection object with bbox
            source_path: Optional source image path

        Returns:
            CellCrop object or None if extraction fails
        """
        import cv2

        h, w = image.shape[:2]

        # Get bbox with padding
        x1, y1, x2, y2 = detection.bbox
        if self.padding > 0:
            pad_w = (x2 - x1) * self.padding
            pad_h = (y2 - y1) * self.padding
            x1 = max(0, x1 - pad_w)
            y1 = max(0, y1 - pad_h)
            x2 = min(w, x2 + pad_w)
            y2 = min(h, y2 + pad_h)

        # Convert to int
        x1, y1, x2, y2 = int(x1), int(y1), int(x2), int(y2)

        # Validate bounds
        if x1 >= x2 or y1 >= y2:
            logger.warning(f"Invalid bbox: ({x1}, {y1}, {x2}, {y2})")
            return None

        # Crop
        crop = image[y1:y2, x1:x2]

        # Resize
        crop = cv2.resize(crop, (self.output_size, self.output_size))

        # Convert to grayscale if requested
        if self.grayscale and len(crop.shape) == 3:
            crop = cv2.cvtColor(crop, cv2.COLOR_BGR2GRAY)

        return CellCrop(
            image=crop,
            label=detection.label,
            label_name=detection.label_name,
            score=detection.score,
            source_image=source_path,
            bbox=detection.bbox,
        )

    def crops_to_tensor(
        self,
        crops: List[CellCrop],
        normalize: bool = True,
    ) -> "torch.Tensor":
        """
        Convert crops to PyTorch tensor for VAE input.

        Args:
            crops: List of CellCrop objects
            normalize: Whether to normalize to [0, 1]

        Returns:
            Tensor of shape (N, C, H, W)
        """
        import torch

        images = []
        for crop in crops:
            img = crop.image.astype(np.float32)
            if normalize:
                img = img / 255.0

            # Add channel dimension if grayscale
            if len(img.shape) == 2:
                img = img[np.newaxis, :, :]
            else:
                # Convert HWC to CHW
                img = np.transpose(img, (2, 0, 1))

            images.append(img)

        return torch.tensor(np.stack(images), dtype=torch.float32)
