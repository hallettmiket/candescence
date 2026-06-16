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
_VARASANA_PAGE = str(_PAGES / "8_Varasana_Detection.py")
_GRACE_PAGE = str(_PAGES / "9_Grace_Detection.py")
_TRAIN_PAGE = str(_PAGES / "10_Train_Detector.py")


def test_varasana_detection_page_renders():
    """The page loads and renders (availability/weights branches) without error."""
    at = AppTest.from_file(_VARASANA_PAGE, default_timeout=60).run()
    assert not at.exception


def test_grace_detection_page_renders():
    """The Grace page (Macro/TC/compare selector) renders without error."""
    at = AppTest.from_file(_GRACE_PAGE, default_timeout=60).run()
    assert not at.exception


def test_grace_detection_compare_mode_renders():
    """Switching the Grace page to compare mode renders without error."""
    at = AppTest.from_file(_GRACE_PAGE, default_timeout=120).run()
    at.radio(key="grace_mode").set_value("Compare both").run()
    assert not at.exception


def test_train_detector_page_renders():
    """The modern detector-training page renders its form without error."""
    at = AppTest.from_file(_TRAIN_PAGE, default_timeout=60).run()
    assert not at.exception
