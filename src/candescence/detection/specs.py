"""
Purpose: Registry of the legacy FCOS detector models (Varasana, Grace macro/TC):
         where each one's config + checkpoint live, and which Candescence project
         it belongs to. The GUI detection page and the service client read model
         locations from here instead of hardcoding paths.
Author: Hallett Lab
Date: 2026-06-15
Input: None (static declarations; paths resolved at call time via settings).
Output: ``DetectorSpec`` records + lookup helpers.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional

from candescence.core.settings import legacy_refined_root


@dataclass(frozen=True)
class DetectorSpec:
    """A pretrained legacy FCOS detector.

    Attributes
    ----------
    key:
        Unique detector id (e.g. ``"varasana"``, ``"grace_macro"``, ``"grace_tc"``).
    project:
        The owning Candescence project id (``"varasana"`` or ``"grace"``); matches
        ``candescence.core.projects``.
    label:
        Human-readable label for the GUI.
    config_subpath / checkpoint_subpath:
        Paths to the mmdet config (.py) and checkpoint (.pth), relative to the
        legacy refined root (``candescence.core.settings.legacy_refined_root``).
    """

    key: str
    project: str
    label: str
    config_subpath: str
    checkpoint_subpath: str

    def config_path(self) -> Path:
        """Absolute path to the mmdet config file."""
        return legacy_refined_root() / self.config_subpath

    def checkpoint_path(self) -> Path:
        """Absolute path to the checkpoint file."""
        return legacy_refined_root() / self.checkpoint_subpath

    def weights_available(self) -> bool:
        """Whether both the config and checkpoint exist on disk."""
        return self.config_path().exists() and self.checkpoint_path().exists()


_PRODUCTION = "candescence_master/production"

_DETECTORS: Dict[str, DetectorSpec] = {
    "varasana": DetectorSpec(
        key="varasana",
        project="varasana",
        label="Varasana detector (15-class)",
        config_subpath=f"{_PRODUCTION}/varasana/config.py",
        checkpoint_subpath=f"{_PRODUCTION}/varasana/model.pth",
    ),
    "grace_macro": DetectorSpec(
        key="grace_macro",
        project="grace",
        label="Grace macro detector (9-class)",
        config_subpath=f"{_PRODUCTION}/grace_macro/macro_config.py",
        checkpoint_subpath=f"{_PRODUCTION}/grace_macro/macro_model.pth",
    ),
    "grace_tc": DetectorSpec(
        key="grace_tc",
        project="grace",
        label="Grace TC detector (7-class)",
        config_subpath=f"{_PRODUCTION}/grace_tc/tc_config.py",
        checkpoint_subpath=f"{_PRODUCTION}/grace_tc/tc_model.pth",
    ),
}


def all_detectors() -> List[DetectorSpec]:
    """Return every registered detector, in declaration order."""
    return list(_DETECTORS.values())


def get_detector(key: str) -> Optional[DetectorSpec]:
    """Look up a detector by key; ``None`` if unknown."""
    return _DETECTORS.get(key)


def detectors_for_project(project: str) -> List[DetectorSpec]:
    """Return all detectors belonging to a given project id."""
    return [d for d in _DETECTORS.values() if d.project == project]
