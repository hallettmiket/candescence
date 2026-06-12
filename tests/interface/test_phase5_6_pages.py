"""
Purpose: Render-smoke tests for the Phase 5/6 pages (TLV Diffusion, Model Quality).
Author: Hallett Lab
Date: 2026-06-12
Input: The two page scripts, run under the Streamlit AppTest runtime.
Output: Pytest assertions — pages import and render their title without error.

These exercise the render path only (no heavy compute button is clicked), so they
are fast and run with or without the lab checkpoints present.
"""

from __future__ import annotations

import pytest

pytest.importorskip("streamlit")
pytest.importorskip("torch")
from streamlit.testing.v1 import AppTest  # noqa: E402

_DIFFUSION_PAGE = "src/candescence/interface/pages/6_TLV_Diffusion.py"
_QUALITY_PAGE = "src/candescence/interface/pages/7_Model_Quality.py"


def test_diffusion_page_renders() -> None:
    at = AppTest.from_file(_DIFFUSION_PAGE, default_timeout=120).run()
    assert not at.exception
    assert any("Diffusion" in t.value for t in at.title)


def test_model_quality_page_renders() -> None:
    at = AppTest.from_file(_QUALITY_PAGE, default_timeout=60).run()
    assert not at.exception
    assert any("Quality" in t.value for t in at.title)
