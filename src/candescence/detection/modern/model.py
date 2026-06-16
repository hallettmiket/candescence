"""
Purpose: Build torchvision FCOS detectors for the modern detection track.
Author: Hallett Lab
Date: 2026-06-16
Input: A class count, a backbone name, and an optional pretrained-backbone flag.
Output: A ``torchvision`` FCOS model ready for training or inference.

Modern track (torch 2.x, GPU) — distinct from the legacy mmdet detectors, which
run inference-only in the isolated worker. Two backbones are supported:
``resnet50`` (default, the torchvision convenience model) and ``resnet101`` (to
match the ResNet-101 backbone used in the published Varasana/Grace papers).
"""

from __future__ import annotations

import torch.nn as nn
from torchvision.models import (
    ResNet50_Weights,
    ResNet101_Weights,
    resnet50,
    resnet101,
)
from torchvision.models.detection.backbone_utils import _resnet_fpn_extractor
from torchvision.models.detection.fcos import FCOS, fcos_resnet50_fpn
from torchvision.ops import misc as misc_nn_ops
from torchvision.ops.feature_pyramid_network import LastLevelP6P7

# Architecture identifiers stored in checkpoints + the model zoo. ``resnet50``
# keeps the original string (checkpoints predating the resnet101 option load
# unchanged); ``resnet101`` gets a distinct string so inference rebuilds the
# right backbone and the zoo can tell the two apart.
MODERN_ARCHITECTURE = "fcos_resnet50_fpn_torchvision"
MODERN_ARCHITECTURE_R101 = "fcos_resnet101_fpn_torchvision"

# Supported backbones, in display order. Maps backbone name -> architecture id.
ARCHITECTURE_BY_BACKBONE = {
    "resnet50": MODERN_ARCHITECTURE,
    "resnet101": MODERN_ARCHITECTURE_R101,
}
_BACKBONE_BY_ARCHITECTURE = {v: k for k, v in ARCHITECTURE_BY_BACKBONE.items()}

# Every architecture string that belongs to the modern track (used by zoo
# discovery in ``candescence.detection.specs``).
MODERN_ARCHITECTURES = frozenset(ARCHITECTURE_BY_BACKBONE.values())

# Valid backbone names for the trainer/UI.
SUPPORTED_BACKBONES = tuple(ARCHITECTURE_BY_BACKBONE)


def architecture_for_backbone(backbone: str) -> str:
    """Return the architecture id for a backbone name (e.g. ``"resnet101"``)."""
    try:
        return ARCHITECTURE_BY_BACKBONE[backbone]
    except KeyError:
        raise ValueError(
            f"Unknown backbone {backbone!r}; choose from {SUPPORTED_BACKBONES}."
        ) from None


def backbone_for_architecture(architecture: str) -> str:
    """Return the backbone name for an architecture id (defaults to resnet50)."""
    return _BACKBONE_BY_ARCHITECTURE.get(architecture, "resnet50")


def build_fcos(
    num_classes: int,
    *,
    pretrained_backbone: bool = True,
    backbone: str = "resnet50",
) -> nn.Module:
    """Build an FCOS-FPN detector with ``num_classes`` output classes.

    Parameters
    ----------
    num_classes:
        Number of foreground classes (labels are 0-indexed in ``[0, num_classes-1]``,
        the torchvision FCOS convention).
    pretrained_backbone:
        Initialise the backbone from ImageNet weights (recommended for training;
        ignored otherwise).
    backbone:
        ``"resnet50"`` (default) or ``"resnet101"``. ResNet-101 matches the
        backbone used in the published Varasana/Grace detectors.
    """
    if backbone == "resnet50":
        # The torchvision convenience model — keeps existing resnet50 checkpoints
        # byte-for-byte loadable.
        weights_backbone = ResNet50_Weights.IMAGENET1K_V1 if pretrained_backbone else None
        return fcos_resnet50_fpn(
            weights=None, weights_backbone=weights_backbone, num_classes=num_classes
        )
    if backbone == "resnet101":
        return _build_resnet101_fcos(num_classes, pretrained_backbone)
    raise ValueError(
        f"Unknown backbone {backbone!r}; choose from {SUPPORTED_BACKBONES}."
    )


def _build_resnet101_fcos(num_classes: int, pretrained_backbone: bool) -> nn.Module:
    """Assemble an FCOS detector on a ResNet-101 FPN backbone.

    Mirrors torchvision's ``fcos_resnet50_fpn`` recipe (P6/P7 extra levels, the
    last three residual stages returned, frozen batch-norm when pretrained).
    """
    weights_backbone = ResNet101_Weights.IMAGENET1K_V1 if pretrained_backbone else None
    norm_layer = misc_nn_ops.FrozenBatchNorm2d if weights_backbone is not None else None
    resnet = resnet101(weights=weights_backbone, norm_layer=norm_layer)
    # Train the last 3 backbone stages (torchvision's default for detection).
    fpn_backbone = _resnet_fpn_extractor(
        resnet, 3, returned_layers=[2, 3, 4], extra_blocks=LastLevelP6P7(256, 256)
    )
    return FCOS(fpn_backbone, num_classes)
