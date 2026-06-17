"""
Purpose: Streamlit page for the Varasana FastGAN — synthesise single-cell
         Candida albicans images by sampling / interpolating in latent space.
Author: Hallett Lab
Date: 2026-06-16
Input: The published FastGAN generator (from the model zoo / spec).
Output: Grids of generated cells (downloadable as a zip for augmentation) and
        latent-interpolation strips.

Modern, in-process PyTorch (no legacy worker). Unconditional generator, so there
is no "reconstruct a given image" mode (cf. the TLV Diffusion page).

Run via the main app: ``streamlit run src/candescence/interface/app.py``.
"""

from __future__ import annotations

import io
import zipfile
from typing import List

import streamlit as st
import torch
from PIL import Image

from candescence.core.logging_config import get_logger
from candescence.generative.fastgan import (
    interpolate,
    load_generator,
    sample,
    varasana_generator,
)
from candescence.interface.core.theme import apply_theme, page_header

logger = get_logger("candescence.interface.varasana_generator")


@st.cache_resource(show_spinner=False)
def _load(checkpoint: str, nz: int, im_size: int, device: str):
    """Load + cache the FastGAN generator (keyed by checkpoint + device)."""
    return load_generator(checkpoint, nz=nz, im_size=im_size, device=device)


def _zip_images(images: List[Image.Image], prefix: str) -> bytes:
    """Pack PIL images into an in-memory zip of PNGs (for dataset augmentation)."""
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, "w", zipfile.ZIP_DEFLATED) as archive:
        for i, image in enumerate(images):
            png = io.BytesIO()
            image.save(png, format="PNG")
            archive.writestr(f"{prefix}_{i:04d}.png", png.getvalue())
    return buffer.getvalue()


def main() -> None:
    apply_theme()
    page_header(
        "Varasana Generator",
        subproject="varasana",
        icon="🧫",
        description=(
            "Synthesise single-cell *Candida albicans* images with the Varasana "
            "**FastGAN** (unconditional, 256×256). Sample novel cells or walk the "
            "latent space; export a batch as synthetic training data."
        ),
    )

    spec = varasana_generator()
    if not spec.weights_available():
        st.warning(
            f"Generator weights not found at `{spec.checkpoint}`. "
            "The published Varasana GAN checkpoint is required for this page."
        )
        return

    has_cuda = torch.cuda.is_available()
    with st.sidebar:
        st.subheader("Generator")
        st.caption(f"**{spec.label}** · nz={spec.nz} · {spec.im_size}×{spec.im_size}")
        device = st.radio(
            "Device", ["cuda:0", "cpu"] if has_cuda else ["cpu"], index=0,
            help="Sampling runs in-process; a GPU is faster but CPU works.",
        )

    try:
        with st.spinner("Loading generator…"):
            loaded = _load(str(spec.checkpoint), spec.nz, spec.im_size, device)
    except Exception as exc:  # surface load errors in the UI
        st.error(f"Failed to load generator: {exc}")
        logger.exception("generator load failed")
        return

    if loaded.device.type == "cpu":
        st.caption("Running on CPU — sampling works but is slower than GPU.")

    tab_sample, tab_interp = st.tabs(["🎨 Sample", "🔀 Interpolate"])

    # ---- Sample ------------------------------------------------------------
    with tab_sample:
        st.markdown("Generate novel cells from random latent vectors ~ N(0, I).")
        c1, c2 = st.columns(2)
        n = c1.slider("How many", 1, 64, 8, key="gan_n")
        seed = c2.number_input(
            "Seed (0 = random each run)", value=0, step=1, key="gan_seed",
            help="A fixed non-zero seed reproduces the same batch for this count.",
        )
        if st.button("Generate", type="primary", key="gan_sample_btn"):
            with st.spinner("Generating…"):
                images = sample(loaded, int(n),
                                seed=(int(seed) or None), batch_size=16)
            st.session_state["gan_images"] = images

        images = st.session_state.get("gan_images")
        if images:
            cols = st.columns(min(len(images), 4))
            for i, image in enumerate(images):
                cols[i % len(cols)].image(
                    image, use_container_width=True, caption=f"#{i + 1}")
            st.download_button(
                "Download as zip (synthetic dataset)",
                data=_zip_images(images, "varasana_synthetic"),
                file_name="varasana_synthetic.zip",
                mime="application/zip",
            )

    # ---- Interpolate -------------------------------------------------------
    with tab_interp:
        st.markdown("Walk the latent space between two random cells.")
        c1, c2, c3 = st.columns(3)
        seed_a = c1.number_input("Seed A", value=1, step=1, key="gan_seed_a")
        seed_b = c2.number_input("Seed B", value=2, step=1, key="gan_seed_b")
        steps = c3.slider("Steps", 3, 12, 8, key="gan_steps")
        if st.button("Interpolate", type="primary", key="gan_interp_btn"):
            with st.spinner("Interpolating…"):
                frames = interpolate(loaded, seed_a=int(seed_a),
                                     seed_b=int(seed_b), steps=int(steps))
            cols = st.columns(len(frames))
            for i, frame in enumerate(frames):
                cols[i].image(frame, use_container_width=True)


main()
