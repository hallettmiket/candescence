"""
Purpose: Build torchvision FCOS detectors for the modern detection track.
Author: Hallett Lab
Date: 2026-06-16
Input: A class count (and optional pretrained-backbone flag).
Output: A ``torchvision`` FCOS model ready for training or inference.

Modern track (torch 2.x, GPU) — distinct from the legacy mmdet detectors, which
run inference-only in the isolated worker.
"""

from __future__ import annotations

import torch.nn as nn
from torchvision.models.detection import fcos_resnet50_fpn

MODERN_ARCHITECTURE = "fcos_resnet50_fpn_torchvision"


def build_fcos(num_classes: int, *, pretrained_backbone: bool = True) -> nn.Module:
    """Build an FCOS ResNet-50 FPN detector with ``num_classes`` output classes.

    Parameters
    ----------
    num_classes:
        Number of foreground classes (labels are 0-indexed in ``[0, num_classes-1]``,
        the torchvision FCOS convention).
    pretrained_backbone:
        Initialise the ResNet-50 backbone from ImageNet weights (recommended for
        training; ignored otherwise).
    """
    weights_backbone = "DEFAULT" if pretrained_backbone else None
    return fcos_resnet50_fpn(
        weights=None, weights_backbone=weights_backbone, num_classes=num_classes
    )
