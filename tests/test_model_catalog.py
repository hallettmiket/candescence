"""
Purpose: Lock in the public/research model-tier curation.
Author: Hallett Lab
Date: 2026-06-12
Input: The model_catalog module.
Output: Pytest assertions.

End users of the production app must see only the curated public tier
(Strategy 0/1/14 -> c_vae/uc_vae/tendril_vae); research mode reveals everything.
"""

from __future__ import annotations

import pytest

from candescence.core import model_catalog as mc


def test_public_tier_membership() -> None:
    assert set(mc.PUBLIC_STRATEGIES) == {0, 1, 14}
    assert mc.PUBLIC_ARCHITECTURES == frozenset(
        {"c_vae", "uc_vae", "tendril_vae", "diffusion_vae", "fcos_resnet101_fpn"}
    )


@pytest.mark.parametrize("strategy", [0, 1, 14])
def test_public_strategies_are_public(strategy: int) -> None:
    assert mc.is_public_strategy(strategy)


@pytest.mark.parametrize("strategy", [7, 8, 11, 12, 13, 15, 16])
def test_research_strategies_are_not_public(strategy: int) -> None:
    assert not mc.is_public_strategy(strategy)


def test_public_mode_filters_to_curated_set_preserving_order() -> None:
    all_strats = [14, 15, 16, 7, 8, 13, 1, 11, 12, 0]
    assert mc.filter_strategies(all_strats, research_mode=False) == [14, 1, 0]


def test_research_mode_keeps_everything() -> None:
    all_strats = [14, 15, 16, 7, 8, 13, 1, 11, 12, 0]
    assert mc.filter_strategies(all_strats, research_mode=True) == all_strats


def test_architecture_filter() -> None:
    archs = ["tendril_vae", "cond_uc_vae", "c_vae", "crutch_vae", "uc_vae"]
    assert mc.filter_architectures(archs, research_mode=False) == [
        "tendril_vae",
        "c_vae",
        "uc_vae",
    ]
    assert mc.filter_architectures(archs, research_mode=True) == archs


def test_research_mode_default_env(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.delenv(mc.ENV_RESEARCH_MODE, raising=False)
    assert mc.research_mode_default() is False
    monkeypatch.setenv(mc.ENV_RESEARCH_MODE, "true")
    assert mc.research_mode_default() is True
    monkeypatch.setenv(mc.ENV_RESEARCH_MODE, "0")
    assert mc.research_mode_default() is False
