"""
Purpose: Reusable Streamlit UI for running the legacy FCOS detectors (Varasana /
         Grace) on an image and visualising detections. Composable helpers let a
         page render a single detector or compare several side by side.
Author: Hallett Lab
Date: 2026-06-15
Input: ``DetectorSpec`` records (config + checkpoint + owning project).
Output: Renders the detection UI; no return value.
"""

from __future__ import annotations

import colorsys
import glob
from pathlib import Path
from typing import List, Optional, Sequence

import pandas as pd
import streamlit as st
from PIL import Image, ImageDraw

from candescence.core.logging_config import get_logger
from candescence.core.settings import get_settings
from candescence.detection import service
from candescence.detection.inference.detector import Detection
from candescence.detection.specs import (
    ENGINE_LEGACY,
    DetectorSpec,
    detectors_for_project,
)
from candescence.interface.core.components import render_image_source_picker
from candescence.interface.core.theme import apply_theme, page_header

logger = get_logger("candescence.interface.detection")

_IMAGE_GLOBS = ("*.png", "*.bmp", "*.jpg", "*.jpeg", "*.tif", "*.tiff")
_SETUP_CMD = "bash src/candescence/detection/legacy/setup_legacy_env.sh"


# --------------------------------------------------------------------------- #
# Drawing / data helpers
# --------------------------------------------------------------------------- #
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


def _detections_table(detections: List[Detection]) -> pd.DataFrame:
    return pd.DataFrame([
        {"label": d.label_name, "score": round(d.score, 3),
         "x1": round(d.x1), "y1": round(d.y1),
         "x2": round(d.x2), "y2": round(d.y2)}
        for d in detections
    ])


def _render_counts(table: pd.DataFrame) -> None:
    if not table.empty:
        counts = (table.groupby("label").size()
                  .sort_values(ascending=False).rename("count"))
        st.markdown("**Per-class counts**")
        st.bar_chart(counts)
    else:
        st.info("No detections above the score threshold.")


# --------------------------------------------------------------------------- #
# Composable UI steps
# --------------------------------------------------------------------------- #
def legacy_env_ready() -> bool:
    """True if the legacy detection env is installed; otherwise render setup help."""
    if service.is_available():
        return True
    st.warning(
        "The isolated legacy detection environment is not installed. Varasana and "
        "Grace run on a pinned MMDetection stack in a separate venv "
        "(see detection/legacy/README.md)."
    )
    st.code(_SETUP_CMD, language="bash")
    st.caption("Or set $CANDESCENCE_LEGACY_PYTHON to an existing legacy interpreter.")
    return False


def weights_ready(spec: DetectorSpec) -> bool:
    """True if the detector's weights (and, for legacy, config) exist; else error."""
    if spec.weights_available():
        return True
    expected = f"- `{spec.checkpoint}`"
    if spec.engine == ENGINE_LEGACY and spec.config is not None:
        expected = f"- `{spec.config}`\n{expected}"
    st.error(
        f"Model weights for **{spec.label}** were not found. Expected:\n\n{expected}"
    )
    return False


def detector_ready(spec: DetectorSpec) -> bool:
    """Engine-aware readiness: legacy needs the isolated env; both need weights."""
    if spec.engine == ENGINE_LEGACY and not legacy_env_ready():
        return False
    return weights_ready(spec)


def threshold_slider(key: str) -> float:
    """Render the score-threshold slider in the sidebar and return its value."""
    with st.sidebar:
        st.subheader("Detection settings")
        value = st.slider("Score threshold", 0.05, 0.95, 0.30, 0.05, key=key,
                          help="Minimum confidence to keep a detection.")
    return value


@st.cache_resource(show_spinner=False)
def _load_modern_cached(checkpoint: str):
    """Load + cache a modern detector (keyed by checkpoint path)."""
    from candescence.detection.modern import load_modern_model
    return load_modern_model(checkpoint)


def pick_image(key_prefix: str, default_dir: str,
               project: Optional[str] = None) -> Optional[str]:
    """Render the image-source picker + selector; return the chosen image path."""
    image_dir = render_image_source_picker(
        key_prefix=f"{key_prefix}_img", default_dir=default_dir,
        label="Image source", project=project,
    )
    files = _list_images(image_dir)
    if not files:
        st.warning("No images found in that directory.")
        return None
    names = [Path(f).name for f in files]
    chosen = st.selectbox("Image", names, key=f"{key_prefix}_sel")
    return files[names.index(chosen)]


def run_detector(spec: DetectorSpec, image_path: str,
                 score_thr: float) -> Optional[List[Detection]]:
    """Run one detector and return detections; render an error and return None on failure.

    Dispatches by engine: modern detectors run in-process (GPU when available);
    legacy detectors run via the isolated mmdet worker.
    """
    try:
        if spec.is_modern:
            from candescence.detection.modern import inference as modern_inference
            with st.spinner(f"Running {spec.label} (in-process)…"):
                loaded = _load_modern_cached(str(spec.checkpoint))
                return modern_inference.detect(loaded, image_path, score_thr=score_thr)
        with st.spinner(f"Running {spec.label} on CPU…"):
            return service.detect_image(
                spec.config, spec.checkpoint, image_path, score_thr=score_thr,
            )
    except service.LegacyWorkerError as exc:
        st.error(f"Detection failed: {exc}")
        return None
    except Exception as exc:  # surface modern-inference errors too
        st.error(f"Detection failed: {exc}")
        return None


def render_results(spec: DetectorSpec, image_path: str,
                   detections: List[Detection], *, compact: bool = False) -> None:
    """Render overlay + per-class counts + CSV download for one detector's results.

    ``compact=True`` stacks image and counts (no inner columns) so the call is
    safe inside an outer column — used by the side-by-side compare view.
    """
    st.subheader(f"{len(detections)} detection(s)")
    table = _detections_table(detections)

    if compact:
        st.image(_overlay(image_path, detections),
                 caption=Path(image_path).name, width="stretch")
        _render_counts(table)
    else:
        col_img, col_stats = st.columns([3, 2])
        with col_img:
            st.image(_overlay(image_path, detections),
                     caption=Path(image_path).name, width="stretch")
        with col_stats:
            _render_counts(table)

    if detections:
        st.download_button(
            "Download detections (CSV)",
            data=table.to_csv(index=False).encode("utf-8"),
            file_name=f"{spec.key}_{Path(image_path).stem}_detections.csv",
            mime="text/csv",
            key=f"{spec.key}_csv",
        )


# --------------------------------------------------------------------------- #
# Page-level compositions
# --------------------------------------------------------------------------- #
def render_detector_page(spec: DetectorSpec, *, icon: str = "🔬",
                         intro: str = "", with_header: bool = True) -> None:
    """Render a complete detection page for a single detector.

    ``with_header=False`` skips the theme + page header (for embedding under a
    page that already drew its own header, e.g. the Grace selector page).
    """
    if with_header:
        apply_theme()
        page_header(spec.label, subproject=spec.project, icon=icon, description=intro)

    if not detector_ready(spec):
        return

    score_thr = threshold_slider(f"{spec.key}_thr")
    image_path = pick_image(spec.key, str(get_settings().image_dir), project=spec.project)
    if image_path is None:
        return

    if st.button("Detect", type="primary", key=f"{spec.key}_run"):
        detections = run_detector(spec, image_path, score_thr)
        if detections is not None:
            st.session_state[f"{spec.key}_result"] = {
                "path": image_path, "detections": detections,
            }

    result = st.session_state.get(f"{spec.key}_result")
    if result:
        render_results(spec, result["path"], result["detections"])


def render_compare(specs: Sequence[DetectorSpec], *, key_prefix: str) -> None:
    """Run several detectors on one image and show their results side by side."""
    if any(s.engine == ENGINE_LEGACY for s in specs) and not legacy_env_ready():
        return
    ready = [s for s in specs if weights_ready(s)]
    if len(ready) < 2:
        return

    score_thr = threshold_slider(f"{key_prefix}_thr")
    image_path = pick_image(key_prefix, str(get_settings().image_dir),
                            project=ready[0].project)
    if image_path is None:
        return

    if st.button("Detect (all)", type="primary", key=f"{key_prefix}_run"):
        st.session_state[f"{key_prefix}_result"] = {
            "path": image_path,
            "results": {s.key: (run_detector(s, image_path, score_thr) or [])
                        for s in ready},
        }

    result = st.session_state.get(f"{key_prefix}_result")
    if result:
        columns = st.columns(len(ready))
        for column, spec in zip(columns, ready):
            with column:
                st.markdown(f"**{spec.label}**")
                render_results(spec, result["path"],
                               result["results"].get(spec.key, []), compact=True)


def render_project_detection(project_id: str, *, icon: str = "🔬",
                             intro: str = "", with_header: bool = True) -> None:
    """Render a detection page for a project, letting the user pick among all of
    its detectors — the frozen legacy model(s) and any modern ones trained in-app.
    """
    if with_header:
        apply_theme()
        page_header(project_id.capitalize() + " detection",
                    subproject=project_id, icon=icon, description=intro)

    specs = detectors_for_project(project_id)
    if not specs:
        st.info(f"No detectors registered for the {project_id} project yet.")
        return

    if len(specs) == 1:
        spec = specs[0]
        st.caption(f"Detector: **{spec.label}**")
    else:
        labels = [s.label for s in specs]
        chosen = st.selectbox("Detector", labels, key=f"{project_id}_detector")
        spec = specs[labels.index(chosen)]

    render_detector_page(spec, with_header=False)
