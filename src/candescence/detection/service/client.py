"""
Purpose: Modern-side client for the isolated legacy MMDetection inference worker.
         Runs the worker (candescence.detection.service.worker) inside the pinned
         legacy venv as a subprocess and parses its JSON detections — so the main
         (torch 2.x) application never imports the incompatible mmcv/mmdet stack.
Author: Hallett Lab
Date: 2026-06-15
Input: A detector config + checkpoint (legacy mmdet) and one or more image paths.
Output: Parsed ``Detection`` objects per image.

See ``candescence/detection/legacy/README.md`` for how to build the legacy venv.
"""

from __future__ import annotations

import json
import os
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Union

from candescence.core.logging_config import get_logger
from candescence.detection.inference.detector import Detection

logger = get_logger("candescence.detection.service")

# Environment override for the legacy interpreter; otherwise the repo-local default.
ENV_LEGACY_PYTHON = "CANDESCENCE_LEGACY_PYTHON"
_REPO_ROOT = Path(__file__).resolve().parents[4]
_DEFAULT_LEGACY_PYTHON = _REPO_ROOT / ".venv-legacy" / "bin" / "python"
_WORKER = Path(__file__).with_name("worker.py")

PathLike = Union[str, Path]


class LegacyWorkerError(RuntimeError):
    """Raised when the legacy detection worker is missing or fails."""


@dataclass
class DetectionResult:
    """Detections for a batch of images from one detector.

    Attributes
    ----------
    classes:
        Ordered class names of the detector (index == label).
    per_image:
        One list of :class:`Detection` per input image, in input order.
    """

    classes: List[str]
    per_image: List[List[Detection]]


def legacy_python_path() -> Path:
    """Return the legacy interpreter path (``$CANDESCENCE_LEGACY_PYTHON`` or default)."""
    override = os.environ.get(ENV_LEGACY_PYTHON)
    return Path(override) if override else _DEFAULT_LEGACY_PYTHON


def is_available() -> bool:
    """Whether the legacy detection environment appears to be installed."""
    return legacy_python_path().exists()


def detect(
    config: PathLike,
    checkpoint: PathLike,
    images: Sequence[PathLike],
    *,
    score_thr: float = 0.3,
    device: str = "cpu",
    python: Optional[PathLike] = None,
) -> DetectionResult:
    """Run the legacy FCOS detector on ``images`` and return parsed detections.

    Parameters
    ----------
    config, checkpoint:
        Legacy mmdet config (.py) and checkpoint (.pth) paths.
    images:
        Image paths to run inference on.
    score_thr:
        Minimum confidence to keep a detection.
    device:
        ``"cpu"`` (default; the legacy stack predates this lab's A100s) or a CUDA
        device string if a compatible build is ever provided.
    python:
        Override the legacy interpreter (defaults to :func:`legacy_python_path`).

    Raises
    ------
    LegacyWorkerError
        If the legacy interpreter is missing or the worker exits non-zero.
    """
    interpreter = Path(python) if python else legacy_python_path()
    if not interpreter.exists():
        raise LegacyWorkerError(
            f"Legacy detection interpreter not found at {interpreter}. "
            f"Build it with detection/legacy/setup_legacy_env.sh or set "
            f"${ENV_LEGACY_PYTHON}."
        )

    request = {
        "config": str(config),
        "checkpoint": str(checkpoint),
        "images": [str(image) for image in images],
        "score_thr": float(score_thr),
        "device": device,
    }

    # File-based request/response keeps stdout (which mmdet/torch pollute with
    # CUDA + UserWarning chatter) out of the parsed payload.
    with tempfile.TemporaryDirectory() as tmp:
        req_path = Path(tmp) / "request.json"
        resp_path = Path(tmp) / "response.json"
        req_path.write_text(json.dumps(request))

        logger.info("Running legacy detector on %d image(s) via %s",
                    len(request["images"]), interpreter)
        proc = subprocess.run(
            [str(interpreter), str(_WORKER),
             "--request-file", str(req_path),
             "--output-file", str(resp_path)],
            capture_output=True,
            text=True,
        )
        if proc.returncode != 0 or not resp_path.exists():
            raise LegacyWorkerError(
                f"Legacy detection worker failed (exit {proc.returncode}).\n"
                f"stderr tail:\n{proc.stderr[-2000:]}"
            )
        payload: Dict = json.loads(resp_path.read_text())

    classes = payload.get("classes", [])
    per_image: List[List[Detection]] = []
    for result in payload.get("results", []):
        per_image.append([
            Detection(
                bbox=tuple(det["bbox"]),
                label=int(det["label"]),
                label_name=det["label_name"],
                score=float(det["score"]),
            )
            for det in result.get("detections", [])
        ])
    return DetectionResult(classes=classes, per_image=per_image)


def detect_image(
    config: PathLike,
    checkpoint: PathLike,
    image: PathLike,
    **kwargs,
) -> List[Detection]:
    """Convenience wrapper: run :func:`detect` on a single image."""
    result = detect(config, checkpoint, [image], **kwargs)
    return result.per_image[0] if result.per_image else []
