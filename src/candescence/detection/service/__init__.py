"""Isolated legacy-MMDetection inference service for the FCOS detectors.

The Varasana and Grace detectors run on a pinned legacy stack (Python 3.8 /
torch 1.7.1 / mmcv-full 1.2.5 / mmdet 2.10) that is incompatible with the main
application's modern torch stack. This package runs that stack in a separate
process:

- :mod:`worker` — self-contained inference script executed *inside* the legacy
  venv (no candescence imports).
- :mod:`client` — modern-side wrapper that invokes the worker as a subprocess
  and returns parsed ``Detection`` objects.
"""

from candescence.detection.service.client import (
    DetectionResult,
    LegacyWorkerError,
    detect,
    detect_image,
    is_available,
    legacy_python_path,
)

__all__ = [
    "DetectionResult",
    "LegacyWorkerError",
    "detect",
    "detect_image",
    "is_available",
    "legacy_python_path",
]
