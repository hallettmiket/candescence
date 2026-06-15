"""
Purpose: Reusable Streamlit UI for running a legacy FCOS detector (Varasana /
         Grace) on an image and visualising the detections. Shared by the
         per-project detection pages so each page is a thin wrapper.
Author: Hallett Lab
Date: 2026-06-15
Input: A ``DetectorSpec`` (config + checkpoint + owning project).
Output: Renders the detection UI; no return value.
"""

from __future__ import annotations

import colorsys
import glob
from pathlib import Path
from typing import List

import pandas as pd
import streamlit as st
from PIL import Image, ImageDraw

from candescence.core.logging_config import get_logger
from candescence.core.settings import get_settings
from candescence.detection import service
from candescence.detection.inference.detector import Detection
from candescence.detection.specs import DetectorSpec
from candescence.interface.core.components import render_image_source_picker
from candescence.interface.core.theme import apply_theme, page_header

logger = get_logger("candescence.interface.detection")

_IMAGE_GLOBS = ("*.png", "*.bmp", "*.jpg", "*.jpeg", "*.tif", "*.tiff")
_SETUP_CMD = "bash src/candescence/detection/legacy/setup_legacy_env.sh"


def _list_images(image_dir: str, limit: int = 500) -> List[str]:
    """Return sorted image paths in a directory (common extensions)."""
    files: List[str] = []
    for pattern in _IMAGE_GLOBS:
        files.extend(glob.glob(str(Path(image_dir) / pattern)))
    return sorted(files)[:limit]


def _class_color(label: int) -> str:
    """Deterministic, well-spread hex colour per class index (golden-ratio hue)."""
    hue = (label * 0.61803398875) % 1.0
    r, g, b = colorsys.hsv_to_rgb(hue, 0.65, 0.95)
    return f"#{int(r * 255):02x}{int(g * 255):02x}{int(b * 255):02x}"


def _overlay(image_path: str, detections: List[Detection]) -> Image.Image:
    """Draw bounding boxes + labels onto a copy of the image."""
    image = Image.open(image_path).convert("RGB")
    draw = ImageDraw.Draw(image)
    for det in detections:
        color = _class_color(det.label)
        draw.rectangle([det.x1, det.y1, det.x2, det.y2], outline=color, width=2)
        draw.text((det.x1, max(0, det.y1 - 10)),
                  f"{det.label_name} {det.score:.2f}", fill=color)
    return image


def _render_results(spec: DetectorSpec, image_path: str,
                    detections: List[Detection]) -> None:
    """Render the overlay, per-class counts, and a CSV download."""
    st.subheader(f"{len(detections)} detection(s)")
    col_img, col_stats = st.columns([3, 2])

    with col_img:
        st.image(_overlay(image_path, detections),
                 caption=Path(image_path).name, width="stretch")

    rows = [
        {"label": d.label_name, "score": round(d.score, 3),
         "x1": round(d.x1), "y1": round(d.y1),
         "x2": round(d.x2), "y2": round(d.y2)}
        for d in detections
    ]
    table = pd.DataFrame(rows)

    with col_stats:
        if detections:
            counts = (table.groupby("label").size()
                      .sort_values(ascending=False).rename("count"))
            st.markdown("**Per-class counts**")
            st.bar_chart(counts)
        else:
            st.info("No detections above the score threshold.")

    if detections:
        st.download_button(
            "Download detections (CSV)",
            data=table.to_csv(index=False).encode("utf-8"),
            file_name=f"{spec.key}_{Path(image_path).stem}_detections.csv",
            mime="text/csv",
            key=f"{spec.key}_csv",
        )


def render_detector_page(spec: DetectorSpec, *, icon: str = "🔬",
                         intro: str = "") -> None:
    """Render a complete detection page for a single :class:`DetectorSpec`."""
    apply_theme()
    page_header(spec.label, subproject=spec.project, icon=icon, description=intro)

    # 1. Legacy environment must be installed (the detectors run out-of-process).
    if not service.is_available():
        st.warning(
            "The isolated legacy detection environment is not installed. "
            "Varasana and Grace run on a pinned MMDetection stack in a separate "
            "venv (see detection/legacy/README.md)."
        )
        st.code(_SETUP_CMD, language="bash")
        st.caption(
            "Or set $CANDESCENCE_LEGACY_PYTHON to an existing legacy interpreter."
        )
        return

    # 2. Model weights must be present.
    if not spec.weights_available():
        st.error(
            f"Model weights for **{spec.label}** were not found. Expected:\n\n"
            f"- `{spec.config_path()}`\n- `{spec.checkpoint_path()}`"
        )
        return

    settings = get_settings()
    with st.sidebar:
        st.subheader("Detection settings")
        score_thr = st.slider(
            "Score threshold", 0.05, 0.95, 0.30, 0.05, key=f"{spec.key}_thr",
            help="Minimum confidence to keep a detection.",
        )
        st.caption("Runs on CPU (legacy stack) — a few seconds per image.")

    image_dir = render_image_source_picker(
        key_prefix=f"{spec.key}_img",
        default_dir=str(settings.image_dir),
        label="Image source",
    )
    files = _list_images(image_dir)
    if not files:
        st.warning("No images found in that directory.")
        return

    names = [Path(f).name for f in files]
    chosen = st.selectbox("Image", names, key=f"{spec.key}_sel")
    image_path = files[names.index(chosen)]

    if st.button("Detect", type="primary", key=f"{spec.key}_run"):
        try:
            with st.spinner("Running detection on CPU…"):
                detections = service.detect_image(
                    spec.config_path(), spec.checkpoint_path(),
                    image_path, score_thr=score_thr,
                )
        except service.LegacyWorkerError as exc:
            st.error(f"Detection failed: {exc}")
            return
        st.session_state[f"{spec.key}_result"] = {
            "path": image_path, "detections": detections,
        }

    result = st.session_state.get(f"{spec.key}_result")
    if result:
        _render_results(spec, result["path"], result["detections"])
