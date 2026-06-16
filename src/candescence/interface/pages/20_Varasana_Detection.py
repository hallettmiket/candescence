"""
Purpose: Varasana detection page — run a Varasana-project detector on microscopy
         images. Offers the pretrained legacy FCOS model and any modern detectors
         trained in-app (dispatched to the right engine automatically).
Author: Hallett Lab
Date: 2026-06-15
Input: A microscopy image (via the in-app picker) and a detector choice.
Output: Bounding-box overlay, per-class counts, and a CSV of detections.

Launch via the unified interface:
    streamlit run src/candescence/interface/app.py
"""

import streamlit as st

from candescence.interface.components.detection_view import render_project_detection

st.set_page_config(
    page_title="Varasana Detection | Candescence",
    page_icon="🔬",
    layout="wide",
    initial_sidebar_state="expanded",
)


def main() -> None:
    render_project_detection(
        "varasana",
        icon="🔬",
        intro=(
            "Detect and classify *Candida albicans* cells. Choose the pretrained "
            "Varasana FCOS model (Bettauer et al. 2022) or a detector you trained "
            "on the modern track."
        ),
    )


main()
