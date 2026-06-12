"""
Purpose: Render-path test for the research-mode toggle + strategy curation.
Author: Hallett Lab
Date: 2026-06-12
Input: The shared components helper, exercised under the Streamlit runtime.
Output: Pytest assertions (via streamlit.testing AppTest).

Unit tests cover the pure filter logic (test_model_catalog.py); this verifies
the toggle actually drives the curated strategy list when rendered.
"""

from __future__ import annotations

import json

import pytest

pytest.importorskip("streamlit")
from streamlit.testing.v1 import AppTest  # noqa: E402

_SCRIPT = """
import streamlit as st
from candescence.interface.core.components import (
    render_research_mode_toggle,
    research_mode_enabled,
)
from candescence.core.model_catalog import filter_strategies

render_research_mode_toggle()
strategies = filter_strategies(
    [14, 15, 16, 7, 8, 13, 1, 11, 12, 0], research_mode=research_mode_enabled()
)
st.json({"strategies": strategies})
"""


def _strategies(at) -> list[int]:
    value = at.json[0].value
    if isinstance(value, str):
        value = json.loads(value)
    return value["strategies"]


def test_public_mode_is_default_and_curated() -> None:
    at = AppTest.from_string(_SCRIPT).run()
    assert not at.exception
    assert at.toggle[0].value is False
    assert _strategies(at) == [14, 1, 0]


def test_toggling_research_mode_reveals_all() -> None:
    at = AppTest.from_string(_SCRIPT).run()
    at.toggle[0].set_value(True).run()
    assert not at.exception
    assert at.toggle[0].value is True
    assert _strategies(at) == [14, 15, 16, 7, 8, 13, 1, 11, 12, 0]
