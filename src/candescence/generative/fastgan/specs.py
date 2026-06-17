"""
Purpose: Registry of runnable FastGAN generators (currently the one frozen,
         published Varasana GAN). Paths resolve through the settings layer so no
         absolute lab path is hard-coded.
Author: Hallett Lab
Date: 2026-06-16
Input: None (static spec; path resolved at call time).
Output: ``GeneratorSpec`` records + lookup helpers.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional

from candescence.core.settings import legacy_refined_root

# Architecture id recorded in the model zoo for FastGAN generators.
GAN_ARCHITECTURE = "fastgan"

# Location of the published Varasana GAN under the legacy candescence_master tree.
_VARASANA_GAN = "candescence_master/projects/varasana_GAN"


@dataclass(frozen=True)
class GeneratorSpec:
    """A runnable image generator.

    Attributes
    ----------
    key:
        Unique id (also the model-zoo id).
    project:
        Owning Candescence project (``"varasana"``).
    label:
        Human-readable label for the GUI.
    checkpoint:
        Absolute path to the generator checkpoint (``model.pth``: ``{g, d}``).
    nz:
        Latent (noise) dimension the generator expects.
    im_size:
        Output image size the checkpoint was trained at.
    """

    key: str
    project: str
    label: str
    checkpoint: Path
    nz: int = 256
    im_size: int = 256
    architecture: str = GAN_ARCHITECTURE

    def weights_available(self) -> bool:
        """Whether the checkpoint exists on disk."""
        return self.checkpoint.exists()


def varasana_generator() -> GeneratorSpec:
    """The published Varasana FastGAN, with its checkpoint path resolved now."""
    root = legacy_refined_root()
    return GeneratorSpec(
        key="varasana_fastgan",
        project="varasana",
        label="Varasana FastGAN · synthetic cells",
        checkpoint=root / _VARASANA_GAN / "model.pth",
        nz=256,
        im_size=256,
    )


def varasana_gan_training_images() -> Path:
    """The 886 single-cell crops the Varasana GAN was trained on (read-only)."""
    return legacy_refined_root() / _VARASANA_GAN / "images"


def all_generators() -> List[GeneratorSpec]:
    """Every runnable generator."""
    return [varasana_generator()]


def get_generator(key: str) -> Optional[GeneratorSpec]:
    """Look up a generator by key; ``None`` if unknown."""
    return next((g for g in all_generators() if g.key == key), None)
