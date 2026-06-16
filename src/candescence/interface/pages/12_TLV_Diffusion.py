"""
Purpose: Streamlit page for the TLV conditional-diffusion model (Phase 5).
Author: Hallett Lab
Date: 2026-06-12
Input: A pretrained diffusion checkpoint (from the zoo or a custom path).
Output: Interactive generation / reconstruction / interpolation / latent-space
        exploration — adopting the good concepts from Jose's 38-diffusion GUI.

Run via the main app: ``streamlit run src/candescence/interface/app.py``.
"""

from __future__ import annotations

import glob
from pathlib import Path
from typing import List, Optional, Tuple

import numpy as np
import streamlit as st
import torch
import torchvision.transforms.functional as TF
from PIL import Image

from candescence.core.logging_config import get_logger
from candescence.core.model_catalog import is_public_architecture
from candescence.core.model_zoo import ModelZoo
from candescence.core.settings import get_settings
from candescence.interface.core.components import (
    render_image_source_picker,
    render_research_mode_toggle,
    research_mode_enabled,
)
from candescence.tlv.diffusion import load_diffusion_model
from candescence.tlv.diffusion import inference as diff_inf

logger = get_logger("candescence.interface.diffusion")
_settings = get_settings()

IMAGE_EXTS = ("*.png", "*.bmp", "*.jpg", "*.jpeg", "*.tif", "*.tiff")


# --------------------------------------------------------------------------- #
# Model + image loading (cached)
# --------------------------------------------------------------------------- #
@st.cache_resource(show_spinner=False)
def _load_model(path: str, device: str):
    """Load + cache a diffusion model (heavy: a few hundred MB)."""
    return load_diffusion_model(path, device=device)


def _discover_diffusion_models() -> List[Tuple[str, str]]:
    """Return [(label, checkpoint_path)] for diffusion models in the zoo."""
    out: List[Tuple[str, str]] = []
    try:
        zoo = ModelZoo()
        for m in zoo.list_models(project="tlv"):
            if m.architecture == "diffusion_vae" and m.exists():
                out.append((f"{m.name}  ({m.id})", str(m.get_checkpoint_path())))
    except Exception as exc:  # pragma: no cover - defensive
        logger.warning("Zoo discovery failed: %s", exc)
    return out


@st.cache_data(show_spinner=False)
def _list_images(image_dir: str, limit: int) -> List[str]:
    files: List[str] = []
    for ext in IMAGE_EXTS:
        files.extend(sorted(glob.glob(str(Path(image_dir) / ext))))
    return files[:limit]


def _load_image_tensor(path: str, channels: int, img_size: int) -> torch.Tensor:
    """Load one image as a (C, H, W) tensor normalised to [-1, 1]."""
    mode = "L" if channels == 1 else "RGB"
    im = Image.open(path).convert(mode).resize((img_size, img_size))
    return TF.to_tensor(im) * 2 - 1  # [-1, 1]


def _media_from_name(name: str) -> str:
    parts = Path(name).stem.split("_")
    return parts[1] if len(parts) > 1 else "unknown"


# --------------------------------------------------------------------------- #
# Page
# --------------------------------------------------------------------------- #
def main() -> None:
    st.title("TLV Diffusion")
    st.caption(
        "Conditional diffusion model for *Candida albicans* colonies "
        "(SemanticEncoder + FiLM-conditioned DiffusionUNet). "
        "Generate novel colonies, reconstruct real ones, interpolate, and "
        "explore the semantic latent space."
    )

    # ---- sidebar: model + device -------------------------------------------
    with st.sidebar:
        render_research_mode_toggle()
        st.subheader("Diffusion model")
        discovered = _discover_diffusion_models()
        # tier filter (diffusion_vae is public; honour research-mode anyway)
        if not research_mode_enabled():
            discovered = [d for d in discovered if is_public_architecture("diffusion_vae")]

        labels = [d[0] for d in discovered] + ["Custom path…"]
        choice = st.selectbox("Checkpoint", labels, index=0 if discovered else len(labels) - 1)
        if choice == "Custom path…":
            ckpt_path = st.text_input("Checkpoint path (.pt)", value="")
        else:
            ckpt_path = dict(discovered)[choice]

        has_cuda = torch.cuda.is_available()
        device = st.radio(
            "Device", ["cuda:0", "cpu"] if has_cuda else ["cpu"], index=0,
            help="Reconstruction runs the full reverse chain — use a GPU for it.",
        )
        ddim_steps = st.slider(
            "DDIM steps (generation)", 5, 100, 30,
            help="Fewer = faster, lower quality. Generation uses these steps; "
            "reconstruction always runs the full chain.",
        )

    if not ckpt_path:
        st.info("Select or enter a diffusion checkpoint in the sidebar to begin.")
        return

    try:
        with st.spinner("Loading diffusion model…"):
            loaded = _load_model(ckpt_path, device)
    except Exception as exc:
        st.error(f"Failed to load model: {exc}")
        return

    st.success(
        f"Loaded — epoch {loaded.epoch}, {loaded.input_channels}-channel, "
        f"latent_dim {loaded.latent_dim}, {loaded.img_size}×{loaded.img_size}, "
        f"T={loaded.cfg.diffusion.t_steps}, device {loaded.device}."
    )
    if loaded.device.type == "cpu":
        st.warning("Running on CPU — generation is OK, but reconstruction is slow (~30 s/image).")

    tab_gen, tab_rec, tab_interp, tab_latent = st.tabs(
        ["🎨 Generate", "🔁 Reconstruct", "🔀 Interpolate", "🧬 Latent space"]
    )

    # ---- Generate ----------------------------------------------------------
    with tab_gen:
        st.markdown("Sample novel colonies from random semantic codes ~ N(0, I).")
        c1, c2 = st.columns(2)
        n = c1.slider("How many", 1, 8, 4, key="gen_n")
        seed = c2.number_input("Seed", value=0, step=1, key="gen_seed")
        if st.button("Generate", type="primary", key="gen_btn"):
            with st.spinner("Generating…"):
                imgs = diff_inf.generate(loaded, n=n, ddim_steps=ddim_steps, seed=int(seed))
            cols = st.columns(min(n, 4))
            for i, img in enumerate(imgs):
                cols[i % len(cols)].image(img, clamp=True, use_container_width=True, caption=f"#{i+1}")

    # ---- Reconstruct -------------------------------------------------------
    with tab_rec:
        st.markdown("Encode a real colony to its semantic code, then regenerate it.")
        rec_dir = render_image_source_picker(
            key_prefix="diff_rec", default_dir=str(_settings.image_dir),
            label="Images", project="tlv",
        )
        files = _list_images(rec_dir, 200)
        if not files:
            st.warning("No images found in that directory.")
        else:
            names = [Path(f).name for f in files]
            sel = st.selectbox("Pick an image", names, key="rec_sel")
            path = files[names.index(sel)]
            if st.button("Reconstruct", type="primary", key="rec_btn"):
                x0 = _load_image_tensor(path, loaded.input_channels, loaded.img_size).unsqueeze(0)
                with st.spinner("Reconstructing (full reverse chain)…"):
                    recon = diff_inf.reconstruct(loaded, x0)[0]
                orig = ((x0[0] + 1) / 2).clamp(0, 1).cpu().numpy()
                orig = orig[0] if orig.shape[0] == 1 else np.transpose(orig, (1, 2, 0))
                cc1, cc2 = st.columns(2)
                cc1.image(orig, clamp=True, caption="Original", use_container_width=True)
                cc2.image(recon, clamp=True, caption="Reconstruction", use_container_width=True)

    # ---- Interpolate -------------------------------------------------------
    with tab_interp:
        st.markdown("Walk through semantic-code space between two colonies.")
        int_dir = render_image_source_picker(
            key_prefix="diff_int", default_dir=str(_settings.image_dir),
            label="Images", project="tlv",
        )
        files = _list_images(int_dir, 200)
        if len(files) >= 2:
            names = [Path(f).name for f in files]
            a = st.selectbox("From", names, index=0, key="int_a")
            b = st.selectbox("To", names, index=1, key="int_b")
            steps = st.slider("Steps", 3, 12, 8, key="int_steps")
            if st.button("Interpolate", type="primary", key="int_btn"):
                xa = _load_image_tensor(files[names.index(a)], loaded.input_channels, loaded.img_size)
                xb = _load_image_tensor(files[names.index(b)], loaded.input_channels, loaded.img_size)
                with st.spinner("Interpolating…"):
                    seq = diff_inf.interpolate(loaded, xa, xb, n_steps=steps, ddim_steps=ddim_steps)
                cols = st.columns(len(seq))
                for i, img in enumerate(seq):
                    cols[i].image(img, clamp=True, use_container_width=True)
        else:
            st.warning("Need at least two images in the directory.")

    # ---- Latent space ------------------------------------------------------
    with tab_latent:
        st.markdown("Encode a set of colonies and project their 32-d semantic codes to 2-D.")
        lat_dir = render_image_source_picker(
            key_prefix="diff_lat", default_dir=str(_settings.image_dir),
            label="Images", project="tlv",
        )
        max_n = st.slider("Max images", 20, 400, 120, key="lat_n")
        if st.button("Encode + project", type="primary", key="lat_btn"):
            files = _list_images(lat_dir, max_n)
            if len(files) < 5:
                st.warning("Need at least 5 images.")
            else:
                with st.spinner(f"Encoding {len(files)} images…"):
                    batch = torch.stack([
                        _load_image_tensor(f, loaded.input_channels, loaded.img_size) for f in files
                    ])
                    mus = diff_inf.encode(loaded, batch)  # (N, 32)
                from sklearn.decomposition import PCA
                coords = PCA(n_components=2).fit_transform(mus)
                import pandas as pd
                df = pd.DataFrame({
                    "PC1": coords[:, 0], "PC2": coords[:, 1],
                    "media": [_media_from_name(Path(f).name) for f in files],
                    "name": [Path(f).name for f in files],
                })
                import plotly.express as px
                fig = px.scatter(
                    df, x="PC1", y="PC2", color="media", hover_name="name",
                    title="Semantic codes (PCA)", height=560,
                )
                st.plotly_chart(fig, use_container_width=True)


main()
