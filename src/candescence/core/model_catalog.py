"""
Purpose: Curate which VAE strategies/architectures are exposed to end users.
Author: Hallett Lab
Date: 2026-06-12
Input: A strategy number or architecture name; the active "research mode" flag.
Output: Membership tests and filtered lists for the public model tier.

Candescence implements ~12 VAE architectures (strategies 0-16), but only a
curated subset is meant for end users of the production app. Everything else is
"research tier": still trainable and loadable, but hidden behind a research-mode
switch so newcomers are not overwhelmed (and so we can validate models before
promoting them).

Public tier (production-facing):
    Strategy 0  -> c_vae        (basic convolutional VAE)
    Strategy 1  -> uc_vae       (U-Net VAE)
    Strategy 14 -> tendril_vae  (Tendril VAE, recommended)

The Stable Diffusion model will join the public tier in a later phase.
"""

from __future__ import annotations

import os
from typing import Iterable

# Strategy numbers exposed to end users by default, in display order.
PUBLIC_STRATEGIES: tuple[int, ...] = (14, 1, 0)

# Architecture names exposed to end users by default. Kept in sync with the
# strategy->architecture mapping in ``candescence.tlv.factory._prepare_strategy``.
# ``diffusion_vae`` is the ported conditional-diffusion model (inference-only in
# the app for now; see candescence.tlv.diffusion). ``fcos_resnet101_fpn`` is the
# detector architecture for the (now active) Varasana and Grace projects.
PUBLIC_ARCHITECTURES: frozenset[str] = frozenset(
    {"c_vae", "uc_vae", "tendril_vae", "diffusion_vae", "fcos_resnet101_fpn"}
)

# Environment override: set truthy to default the app into research mode (all
# strategies/architectures visible) without clicking the in-app toggle.
ENV_RESEARCH_MODE = "CANDESCENCE_RESEARCH_MODE"

_TRUTHY = {"1", "true", "yes", "on"}


def research_mode_default() -> bool:
    """Return the default research-mode flag from ``$CANDESCENCE_RESEARCH_MODE``."""
    return os.environ.get(ENV_RESEARCH_MODE, "").strip().lower() in _TRUTHY


def is_public_strategy(strategy: int | float) -> bool:
    """Whether a strategy number belongs to the public tier."""
    return strategy in PUBLIC_STRATEGIES


def is_public_architecture(architecture: str | None) -> bool:
    """Whether an architecture name belongs to the public tier."""
    return architecture in PUBLIC_ARCHITECTURES


def filter_strategies(
    strategies: Iterable[int], *, research_mode: bool
) -> list[int]:
    """
    Filter strategy numbers to the public tier unless research mode is on.

    Order of the input iterable is preserved.
    """
    if research_mode:
        return list(strategies)
    return [s for s in strategies if is_public_strategy(s)]


def filter_architectures(
    architectures: Iterable[str], *, research_mode: bool
) -> list[str]:
    """Filter architecture names to the public tier unless research mode is on."""
    if research_mode:
        return list(architectures)
    return [a for a in architectures if is_public_architecture(a)]
