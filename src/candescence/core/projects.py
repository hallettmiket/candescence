"""
Purpose: Central registry of Candescence projects (the cross-cutting "layer above"
         the individual engines). Each project — TLV, Varasana, Grace — is declared
         once here, and the rest of the codebase (theme, home dashboard, model
         registry, dataset manager) reads project metadata from this single source
         instead of hardcoding the {tlv, varasana, grace} set in many places.
Author: Hallett Lab
Date: 2026-06-15
Input: None (static declarations).
Output: ``Project`` records + lookup helpers used across core and the interface.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional

# Fallback colour for an unknown project id (matches the legacy page default).
DEFAULT_PROJECT_COLOR = "#666666"


@dataclass(frozen=True)
class ProjectPage:
    """A Streamlit page that belongs to a project.

    Attributes
    ----------
    title:
        Human-readable page title shown on launcher cards.
    slug:
        Streamlit page identifier (the page file stem, e.g. ``"10_TLV_Training"``),
        used for navigation references (e.g. ``"10_TLV_Training"``).
    description:
        One-line description of what the page does.
    icon:
        Optional emoji icon.
    """

    title: str
    slug: str
    description: str
    icon: str = ""


@dataclass(frozen=True)
class Project:
    """A first-class Candescence project.

    Attributes
    ----------
    id:
        Canonical lowercase identifier; matches the ``project`` field used in the
        model zoo and dataset zoo (e.g. ``"tlv"``, ``"varasana"``, ``"grace"``).
    label:
        Short display label (e.g. ``"TLV"``).
    full_name:
        Expanded name (e.g. ``"Tendril Latent VAE"``).
    kind:
        Engine family: ``"vae"`` (latent-space models) or ``"detection"``
        (FCOS object detectors). Drives which shared engine a project uses.
    color:
        Hex accent colour for branding in the UI.
    description:
        One-line summary of the project.
    status:
        ``"active"`` if it has working pages, ``"planned"`` if integration is
        still in progress (no GUI pages yet).
    reference:
        Optional citation for the underlying method/paper.
    pages:
        Ordered list of the project's GUI pages (may be empty for planned projects).
    """

    id: str
    label: str
    full_name: str
    kind: str
    color: str
    description: str
    status: str = "active"
    reference: Optional[str] = None
    pages: List[ProjectPage] = field(default_factory=list)


# --------------------------------------------------------------------------- #
# The registry — declare every Candescence project exactly once.
# --------------------------------------------------------------------------- #
_PROJECTS: Dict[str, Project] = {
    "tlv": Project(
        id="tlv",
        label="TLV",
        full_name="Tendril Latent VAE",
        kind="vae",
        color="#9B59B6",  # purple
        description="Variational autoencoders for Candida albicans colony "
        "morphology and latent-space exploration.",
        status="active",
        pages=[
            ProjectPage(
                "TLV Training", "10_TLV_Training",
                "Train Tendril VAE models for latent space analysis", "🧬",
            ),
            ProjectPage(
                "TLV Explorer", "11_TLV_Explorer",
                "Explore trained VAE latent spaces interactively", "🔍",
            ),
            ProjectPage(
                "TLV Diffusion", "12_TLV_Diffusion",
                "Generate, reconstruct, and interpolate with the diffusion "
                "companion", "✨",
            ),
        ],
    ),
    "varasana": Project(
        id="varasana",
        label="Varasana",
        full_name="Varasana FCOS Detector",
        kind="detection",
        color="#E67E22",  # orange
        description="FCOS-based detection and morphology classification of "
        "Candida albicans cells in microscopy images.",
        status="active",
        reference="Bettauer et al. (2022) Microbiol. Spectrum, "
        "10.1128/spectrum.01472-22",
        pages=[
            ProjectPage(
                "Varasana Detection", "20_Varasana_Detection",
                "Detect & classify cells with the pretrained FCOS model", "🔬",
            ),
            ProjectPage(
                "Varasana Training", "21_Varasana_Training",
                "Train FCOS object detection models", "🎯",
            ),
            ProjectPage(
                "Varasana Generator", "22_Varasana_Generator",
                "Synthesise single-cell images with the FastGAN", "🧫",
            ),
        ],
    ),
    "grace": Project(
        id="grace",
        label="Grace",
        full_name="Grace Cell & Macro Classifiers",
        kind="detection",
        color="#1ABC9C",  # teal
        description="FCOS detectors for macrophage (9-class) and tissue-culture "
        "(7-class) Candida albicans morphology classification.",
        status="active",
        reference="Case, Westman et al. (2023) mBio, 10.1128/mbio.02745-23",
        pages=[
            ProjectPage(
                "Grace Detection", "30_Grace_Detection",
                "Detect & classify cells with the pretrained macro / TC models",
                "🔬",
            ),
        ],
    ),
}


def all_projects() -> List[Project]:
    """Return every registered project, in declaration order."""
    return list(_PROJECTS.values())


def active_projects() -> List[Project]:
    """Return only projects with working GUI pages (``status == "active"``)."""
    return [p for p in _PROJECTS.values() if p.status == "active"]


def get_project(project_id: str) -> Optional[Project]:
    """Look up a project by id (case-insensitive); ``None`` if unknown."""
    return _PROJECTS.get(project_id.lower())


def project_color(project_id: str, default: str = DEFAULT_PROJECT_COLOR) -> str:
    """Return the accent colour for a project id, or ``default`` if unknown.

    Parameters
    ----------
    project_id:
        Project identifier (case-insensitive).
    default:
        Colour to return when the id is not registered. Defaults to
        :data:`DEFAULT_PROJECT_COLOR` (the legacy page fallback).
    """
    project = _PROJECTS.get(project_id.lower())
    return project.color if project is not None else default


def project_label(project_id: str) -> str:
    """Return a project's display label, falling back to ``project_id.upper()``."""
    project = _PROJECTS.get(project_id.lower())
    return project.label if project is not None else project_id.upper()
