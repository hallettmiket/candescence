"""
Purpose: Torch ``Dataset`` for FCOS detection training on the legacy pickle
         annotation format, for the modern (torchvision) detection track.
Author: Hallett Lab
Date: 2026-06-16
Input: A pickle annotation file (list of {filename, width, height,
       ann:{bboxes (N,4) xyxy float32, labels (N,) int}}) + an image directory.
Output: ``(image_tensor[C,H,W] in [0,1], target{boxes, labels})`` samples in the
        torchvision detection convention.

The pickle ``filename`` fields hold stale absolute paths, so images are resolved
by basename within ``img_dir``. Records whose image is missing or that have no
valid boxes are skipped.
"""

from __future__ import annotations

import pickle
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import torch
import torchvision.transforms.functional as TF
from PIL import Image
from torch.utils.data import Dataset

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.detection.modern.dataset")


def _valid_boxes_mask(boxes: np.ndarray) -> np.ndarray:
    """Boolean mask of boxes with positive width and height."""
    return (boxes[:, 2] > boxes[:, 0]) & (boxes[:, 3] > boxes[:, 1])


class PickleDetectionDataset(Dataset):
    """Detection dataset backed by a legacy ``.pkl`` annotation file."""

    def __init__(
        self,
        ann_pkl: str,
        img_dir: str,
        *,
        max_images: Optional[int] = None,
    ) -> None:
        self.img_dir = Path(img_dir)
        with open(ann_pkl, "rb") as handle:
            records = pickle.load(handle)

        self._samples: List[dict] = []
        max_label = -1
        for record in records:
            image_path = self.img_dir / Path(record["filename"]).name
            if not image_path.exists():
                continue
            boxes = np.asarray(record["ann"]["bboxes"], dtype=np.float32).reshape(-1, 4)
            labels = np.asarray(record["ann"]["labels"]).reshape(-1).astype(np.int64)
            keep = _valid_boxes_mask(boxes)
            boxes, labels = boxes[keep], labels[keep]
            if len(boxes) == 0:
                continue
            self._samples.append({"image": str(image_path),
                                  "boxes": boxes, "labels": labels})
            if labels.size:
                max_label = max(max_label, int(labels.max()))
            if max_images is not None and len(self._samples) >= max_images:
                break

        if not self._samples:
            raise ValueError(
                f"No usable samples in {ann_pkl} with images under {img_dir}."
            )
        self.num_classes = max_label + 1
        logger.info("Loaded %d images (%d classes) from %s",
                    len(self._samples), self.num_classes, ann_pkl)

    def __len__(self) -> int:
        return len(self._samples)

    def __getitem__(self, index: int) -> Tuple[torch.Tensor, Dict[str, torch.Tensor]]:
        sample = self._samples[index]
        image = TF.to_tensor(Image.open(sample["image"]).convert("RGB"))
        target = {
            "boxes": torch.as_tensor(sample["boxes"], dtype=torch.float32),
            "labels": torch.as_tensor(sample["labels"], dtype=torch.int64),
        }
        return image, target


def collate_detection(batch):
    """Collate detection samples into (list[images], list[targets])."""
    images, targets = zip(*batch)
    return list(images), list(targets)
