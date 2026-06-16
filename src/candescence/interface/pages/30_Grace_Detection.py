"""
Purpose: Grace detection page — run Grace-project detectors on microscopy images.
         Single mode lets you pick any Grace detector (legacy macro / TC, or a
         modern one you trained); compare mode runs the two legacy models side by
         side.
Author: Hallett Lab
Date: 2026-06-15
Input: A microscopy image (via the in-app picker) and a detector / mode choice.
Output: Bounding-box overlay(s), per-class counts, and CSV of detections.

Launch via the unified interface:
    streamlit run src/candescence/interface/app.py
"""

import streamlit as st

from candescence.detection.specs import get_detector
from candescence.interface.components.detection_view import (
    render_compare,
    render_project_detection,
)
from candescence.interface.core.theme import apply_theme, page_header

st.set_page_config(
    page_title="Grace Detection | Candescence",
    page_icon="🔬",
    layout="wide",
    initial_sidebar_state="expanded",
)

_SINGLE = "Single detector"
_COMPARE = "Compare macro vs TC"


def main() -> None:
    apply_theme()
    page_header(
        "Grace detection",
        subproject="grace",
        icon="🔬",
        description=(
            "Detect and classify *Candida albicans* morphology with the pretrained "
            "Grace FCOS detectors (Case, Westman et al. 2023) — macrophage-assay "
            "(9-class) and tissue-culture (7-class) — or a detector you trained."
        ),
    )

    mode = st.radio("Mode", [_SINGLE, _COMPARE], horizontal=True, key="grace_mode")

    if mode == _SINGLE:
        render_project_detection("grace", with_header=False)
    else:
        macro = get_detector("grace_macro")
        tc = get_detector("grace_tc")
        render_compare([macro, tc], key_prefix="grace_compare")


main()
