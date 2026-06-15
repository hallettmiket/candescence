"""
Purpose: Smoke test that the Varasana detection page renders without error under
         the Streamlit AppTest runtime (no actual inference is triggered).
Author: Hallett Lab
Date: 2026-06-15
Input: The page script.
Output: Pytest assertions.
"""

from pathlib import Path

from streamlit.testing.v1 import AppTest

_PAGES = Path(__file__).resolve().parents[2] / "src" / "candescence" / "interface" / "pages"
_DETECTION_PAGE = str(_PAGES / "8_Varasana_Detection.py")


def test_varasana_detection_page_renders():
    """The page loads and renders (availability/weights branches) without error."""
    at = AppTest.from_file(_DETECTION_PAGE, default_timeout=60).run()
    assert not at.exception
