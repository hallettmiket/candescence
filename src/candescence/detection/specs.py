"""
Purpose: Registry of runnable detectors across both tracks — the frozen legacy
         FCOS models (mmdet, inference via the isolated worker) and any modern
         torchvision detectors trained in-app and registered in the model zoo.
         The detection pages read available detectors from here and dispatch
         inference by ``engine``.
Author: Hallett Lab
Date: 2026-06-16
Input: None (legacy specs are static; modern specs are discovered from the zoo).
Output: ``DetectorSpec`` records + lookup helpers.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional

from candescence.core.logging_config import get_logger
from candescence.core.settings import legacy_refined_root

logger = get_logger("candescence.detection.specs")

ENGINE_LEGACY = "legacy_mmdet"
ENGINE_MODERN = "modern"


@dataclass(frozen=True)
class DetectorSpec:
    """A runnable detector.

    Attributes
    ----------
    key:
        Unique id (legacy: fixed key; modern: the zoo model id).
    project:
        Owning Candescence project (``"varasana"``, ``"grace"``, …).
    label:
        Human-readable label for the GUI.
    engine:
        ``ENGINE_LEGACY`` (mmdet, run via the isolated worker) or
        ``ENGINE_MODERN`` (torchvision, run in-process).
    checkpoint:
        Absolute path to the checkpoint.
    config:
        Absolute path to the mmdet config (legacy only; ``None`` for modern).
    """

    key: str
    project: str
    label: str
    engine: str
    checkpoint: Path
    config: Optional[Path] = None

    @property
    def is_modern(self) -> bool:
        return self.engine == ENGINE_MODERN

    def weights_available(self) -> bool:
        """Whether the checkpoint (and, for legacy, the config) exist."""
        if not self.checkpoint.exists():
            return False
        if self.engine == ENGINE_LEGACY:
            return self.config is not None and self.config.exists()
        return True


# Legacy production detectors: (key, project, label, config_subpath, ckpt_subpath)
# relative to the legacy refined root (resolved at call time).
_PRODUCTION = "candescence_master/production"
_LEGACY = [
    ("varasana", "varasana", "Varasana detector (15-class) · legacy",
     f"{_PRODUCTION}/varasana/config.py", f"{_PRODUCTION}/varasana/model.pth"),
    ("grace_macro", "grace", "Grace macro detector (9-class) · legacy",
     f"{_PRODUCTION}/grace_macro/macro_config.py",
     f"{_PRODUCTION}/grace_macro/macro_model.pth"),
    ("grace_tc", "grace", "Grace TC detector (7-class) · legacy",
     f"{_PRODUCTION}/grace_tc/tc_config.py",
     f"{_PRODUCTION}/grace_tc/tc_model.pth"),
]


def legacy_detectors() -> List[DetectorSpec]:
    """The fixed legacy (mmdet) detectors, with paths resolved now."""
    root = legacy_refined_root()
    return [
        DetectorSpec(key=k, project=p, label=lbl, engine=ENGINE_LEGACY,
                     checkpoint=root / ckpt, config=root / cfg)
        for (k, p, lbl, cfg, ckpt) in _LEGACY
    ]


def modern_detectors() -> List[DetectorSpec]:
    """Modern (torchvision) detectors discovered from the model zoo."""
    from candescence.core.model_zoo import ModelZoo
    from candescence.detection.modern.model import MODERN_ARCHITECTURES

    out: List[DetectorSpec] = []
    try:
        for entry in ModelZoo().list_models():
            if entry.architecture in MODERN_ARCHITECTURES and entry.exists():
                out.append(DetectorSpec(
                    key=entry.id, project=entry.project,
                    label=f"{entry.name} · modern", engine=ENGINE_MODERN,
                    checkpoint=Path(entry.get_checkpoint_path()), config=None,
                ))
    except Exception as exc:  # pragma: no cover - defensive
        logger.warning("modern detector discovery failed: %s", exc)
    return out


def all_detectors() -> List[DetectorSpec]:
    """Every runnable detector: legacy (fixed) + modern (from the zoo)."""
    return legacy_detectors() + modern_detectors()


def get_detector(key: str) -> Optional[DetectorSpec]:
    """Look up a detector by key across both tracks; ``None`` if unknown."""
    for detector in all_detectors():
        if detector.key == key:
            return detector
    return None


def detectors_for_project(project: str) -> List[DetectorSpec]:
    """All detectors (legacy + modern) belonging to a project."""
    return [d for d in all_detectors() if d.project == project]
