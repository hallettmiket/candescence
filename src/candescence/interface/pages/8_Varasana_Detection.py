"""
Purpose: Varasana FCOS detection page — run the pretrained Varasana detector on
         microscopy images and visualise Candida albicans morphology detections.
Author: Hallett Lab
Date: 2026-06-15
Input: A microscopy image (via the in-app picker).
Output: Bounding-box overlay, per-class counts, and a CSV of detections.

The detector runs out-of-process in the isolated legacy MMDetection environment
(see detection/legacy/README.md).

Launch via the unified interface:
    streamlit run src/candescence/interface/app.py
"""

import streamlit as st

from candescence.detection.specs import get_detector
from candescence.interface.components.detection_view import render_detector_page

st.set_page_config(
    page_title="Varasana Detection | Candescence",
    page_icon="🔬",
    layout="wide",
    initial_sidebar_state="expanded",
)


def main() -> None:
    spec = get_detector("varasana")
    render_detector_page(
        spec,
        icon="🔬",
        intro=(
            "Detect and classify *Candida albicans* cells with the pretrained "
            "Varasana FCOS model (Bettauer et al. 2022) — 15 morphology classes."
        ),
    )


main()
