"""
Purpose: Grace detection page — run the pretrained Grace FCOS detectors on
         microscopy images. One page with a Macro / TC selector plus a
         side-by-side compare mode.
Author: Hallett Lab
Date: 2026-06-15
Input: A microscopy image (via the in-app picker) and a detector choice.
Output: Bounding-box overlay(s), per-class counts, and CSV of detections.

The detectors run out-of-process in the isolated legacy MMDetection environment
(see detection/legacy/README.md).

Launch via the unified interface:
    streamlit run src/candescence/interface/app.py
"""

import streamlit as st

from candescence.detection.specs import get_detector
from candescence.interface.components.detection_view import (
    render_compare,
    render_detector_page,
)
from candescence.interface.core.theme import apply_theme, page_header

st.set_page_config(
    page_title="Grace Detection | Candescence",
    page_icon="🔬",
    layout="wide",
    initial_sidebar_state="expanded",
)

_MACRO = "Macro (9-class)"
_TC = "TC (7-class)"
_COMPARE = "Compare both"


def main() -> None:
    apply_theme()
    page_header(
        "Grace detection",
        subproject="grace",
        icon="🔬",
        description=(
            "Detect and classify *Candida albicans* morphology with the pretrained "
            "Grace FCOS detectors (Case, Westman et al. 2023) — a macrophage-assay "
            "(9-class) and a tissue-culture (7-class) model."
        ),
    )

    macro = get_detector("grace_macro")
    tc = get_detector("grace_tc")

    mode = st.radio(
        "Detector", [_MACRO, _TC, _COMPARE], horizontal=True, key="grace_mode",
    )

    if mode == _MACRO:
        render_detector_page(macro, with_header=False)
    elif mode == _TC:
        render_detector_page(tc, with_header=False)
    else:
        render_compare([macro, tc], key_prefix="grace_compare")


main()
