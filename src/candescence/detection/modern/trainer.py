"""
Purpose: Train a torchvision FCOS detector (modern detection track) on the legacy
         pickle annotation data, on GPU and in the main environment.
Author: Hallett Lab
Date: 2026-06-16
Input: An annotation pickle + image dir, an output dir, and hyperparameters.
Output: A self-contained checkpoint (.pth with embedded meta) + a loss history.

Distinct from the legacy mmdet detectors, which run inference-only in the
isolated worker. New models trained here are modern-native and run in-process.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Callable, Dict, List, Optional

import torch
from torch.utils.data import DataLoader

from candescence.core.logging_config import get_logger
from candescence.detection.modern.dataset import (
    PickleDetectionDataset,
    collate_detection,
)
from candescence.detection.modern.model import MODERN_ARCHITECTURE, build_fcos

logger = get_logger("candescence.detection.modern.trainer")

# Progress callback: receives a dict per step (epoch, step, total_steps, loss, ...).
ProgressCallback = Callable[[Dict], None]
# Stop callback: return True to request graceful cancellation after the step.
StopCallback = Callable[[], bool]


@dataclass
class TrainResult:
    """Outcome of a training run."""

    checkpoint_path: str
    num_classes: int
    num_images: int
    history: List[Dict] = field(default_factory=list)


def train_detector(
    ann_pkl: str,
    img_dir: str,
    out_dir: str,
    *,
    num_classes: Optional[int] = None,
    class_names: Optional[List[str]] = None,
    epochs: int = 2,
    lr: float = 5e-3,
    batch_size: int = 2,
    max_images: Optional[int] = None,
    device: Optional[str] = None,
    num_workers: int = 0,
    pretrained_backbone: bool = True,
    progress: Optional[ProgressCallback] = None,
    should_stop: Optional[StopCallback] = None,
) -> TrainResult:
    """Train an FCOS detector and save a checkpoint. Returns a :class:`TrainResult`.

    The checkpoint is a single ``.pth`` bundling ``state_dict`` and meta
    (``num_classes``, ``classes``, ``architecture``, ``history``), loadable by
    :func:`candescence.detection.modern.inference.load_modern_model`.
    """
    device = device or ("cuda:0" if torch.cuda.is_available() else "cpu")
    dataset = PickleDetectionDataset(ann_pkl, img_dir, max_images=max_images)
    # Prefer an explicit count, then the class-name list length (the full schema),
    # then the max label seen in the data (correct only on the full dataset).
    n_classes = num_classes or (len(class_names) if class_names else dataset.num_classes)
    # Guard: a head with too few classes would otherwise fail with an opaque CUDA
    # device-side assert when a label indexes past the classification head.
    if n_classes < dataset.num_classes:
        raise ValueError(
            f"num_classes={n_classes} but the data contains labels up to "
            f"{dataset.num_classes - 1} ({dataset.num_classes} classes). "
            f"Provide at least {dataset.num_classes} class names (or num_classes)."
        )

    loader = DataLoader(
        dataset, batch_size=batch_size, shuffle=True,
        collate_fn=collate_detection, num_workers=num_workers,
    )
    model = build_fcos(n_classes, pretrained_backbone=pretrained_backbone).to(device)
    optimizer = torch.optim.SGD(
        [p for p in model.parameters() if p.requires_grad],
        lr=lr, momentum=0.9, weight_decay=1e-4,
    )

    total_steps = len(loader)
    history: List[Dict] = []
    stopped = False
    for epoch in range(epochs):
        model.train()
        running = 0.0
        for step, (images, targets) in enumerate(loader):
            images = [img.to(device) for img in images]
            targets = [{k: v.to(device) for k, v in t.items()} for t in targets]

            loss_dict = model(images, targets)
            loss = sum(loss_dict.values())
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            running += float(loss)
            if progress is not None:
                progress({
                    "epoch": epoch, "epochs": epochs,
                    "step": step, "total_steps": total_steps,
                    "loss": float(loss),
                    "components": {k: float(v) for k, v in loss_dict.items()},
                })
            if should_stop is not None and should_stop():
                stopped = True
                break

        epoch_loss = running / max(1, (step + 1))
        history.append({"epoch": epoch, "epoch_loss": epoch_loss})
        logger.info("epoch %d/%d - mean loss %.4f", epoch + 1, epochs, epoch_loss)
        if stopped:
            break

    out_path = Path(out_dir)
    out_path.mkdir(parents=True, exist_ok=True)
    checkpoint_path = out_path / "model.pth"
    torch.save(
        {
            "state_dict": model.state_dict(),
            "num_classes": n_classes,
            "classes": class_names or [str(i) for i in range(n_classes)],
            "architecture": MODERN_ARCHITECTURE,
            "history": history,
            "created_at": datetime.now().isoformat(timespec="seconds"),
            "stopped_early": stopped,
        },
        checkpoint_path,
    )
    logger.info("saved checkpoint -> %s", checkpoint_path)
    return TrainResult(
        checkpoint_path=str(checkpoint_path),
        num_classes=n_classes,
        num_images=len(dataset),
        history=history,
    )
