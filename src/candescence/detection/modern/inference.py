"""
Purpose: In-process inference for modern (torchvision) FCOS detectors.
Author: Hallett Lab
Date: 2026-06-16
Input: A checkpoint saved by candescence.detection.modern.trainer + an image path.
Output: ``Detection`` objects (same dataclass as the legacy detector).

Runs natively in the main environment (GPU when available) — no isolated worker.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Union

import torch
import torchvision.transforms.functional as TF
from PIL import Image

from candescence.core.logging_config import get_logger
from candescence.detection.inference.detector import Detection
from candescence.detection.modern.model import (
    MODERN_ARCHITECTURE,
    backbone_for_architecture,
    build_fcos,
)

logger = get_logger("candescence.detection.modern.inference")

PathLike = Union[str, Path]


@dataclass
class LoadedModernModel:
    """A loaded modern detector ready for inference."""

    model: torch.nn.Module
    classes: List[str]
    device: torch.device


def load_modern_model(checkpoint: PathLike,
                      device: Optional[str] = None) -> LoadedModernModel:
    """Load a modern FCOS checkpoint (with embedded meta) for inference."""
    dev = torch.device(device or ("cuda:0" if torch.cuda.is_available() else "cpu"))
    ckpt = torch.load(checkpoint, map_location=dev, weights_only=False)
    num_classes = int(ckpt["num_classes"])
    classes = list(ckpt.get("classes") or [str(i) for i in range(num_classes)])
    # Rebuild the same backbone the checkpoint was trained with (resnet50 for
    # checkpoints predating the resnet101 option, which lack/omit the field).
    backbone = backbone_for_architecture(ckpt.get("architecture", MODERN_ARCHITECTURE))

    model = build_fcos(num_classes, pretrained_backbone=False, backbone=backbone).to(dev)
    model.load_state_dict(ckpt["state_dict"])
    model.eval()
    return LoadedModernModel(model=model, classes=classes, device=dev)


@torch.no_grad()
def detect(loaded: LoadedModernModel, image_path: PathLike,
           score_thr: float = 0.3) -> List[Detection]:
    """Run the modern detector on one image; return detections above ``score_thr``."""
    image = TF.to_tensor(Image.open(image_path).convert("RGB")).to(loaded.device)
    prediction = loaded.model([image])[0]

    detections: List[Detection] = []
    boxes = prediction["boxes"].cpu()
    labels = prediction["labels"].cpu()
    scores = prediction["scores"].cpu()
    for box, label, score in zip(boxes, labels, scores):
        if float(score) < score_thr:
            continue
        label_idx = int(label)
        name = loaded.classes[label_idx] if label_idx < len(loaded.classes) else str(label_idx)
        detections.append(Detection(
            bbox=(float(box[0]), float(box[1]), float(box[2]), float(box[3])),
            label=label_idx, label_name=name, score=float(score),
        ))
    return detections
