"""
Purpose: Streamlit "Model Quality" panel (Phase 6) — a per-model scorecard that
         consolidates the evaluation / latent-analysis methods from the thesis.
Author: Hallett Lab
Date: 2026-06-12
Input: A trained VAE model (from the zoo) + a sample of colony images.
Output: Reconstruction metrics, posterior-collapse / active-dimension
        diagnostics, a latent PCA scree, latent silhouette by media, and the
        training-loss history.

Complements the interactive Latent Explorer (which is for deep, interactive
exploration); this page is the quick "how good is this model?" summary.
Diffusion models have their own page (12_TLV_Diffusion).
"""

from __future__ import annotations

import glob
import json
from pathlib import Path
from typing import List, Optional

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

logger = get_logger("candescence.interface.model_quality")
_settings = get_settings()
IMAGE_EXTS = ("*.bmp", "*.png", "*.jpg", "*.jpeg", "*.tif", "*.tiff")


# --------------------------------------------------------------------------- #
# Helpers
# --------------------------------------------------------------------------- #
def _discover_vae_models(research: bool):
    """Return [(label, model_dir)] for loadable VAE models in the zoo."""
    out = []
    try:
        zoo = ModelZoo()
        for m in zoo.list_models(project="tlv"):
            if m.architecture == "diffusion_vae":
                continue  # diffusion has its own page
            if not research and not is_public_architecture(m.architecture):
                continue
            if m.exists():
                ckpt = Path(m.get_checkpoint_path())
                out.append((f"{m.name}  [{m.architecture}]  ({m.id})", str(ckpt.parent)))
    except Exception as exc:  # pragma: no cover
        logger.warning("zoo discovery failed: %s", exc)
    return out


@st.cache_data(show_spinner=False)
def _list_images(image_dir: str, limit: int) -> List[str]:
    files: List[str] = []
    for ext in IMAGE_EXTS:
        files.extend(sorted(glob.glob(str(Path(image_dir) / ext))))
    return files[:limit]


def _load_batch(paths: List[str], img_size: int = 128) -> torch.Tensor:
    """Load images as an (N, 3, H, W) tensor in [0, 1] (the VAE input convention)."""
    tensors = []
    for p in paths:
        im = Image.open(p).convert("RGB").resize((img_size, img_size))
        tensors.append(TF.to_tensor(im))
    return torch.stack(tensors)


def _media_from_name(name: str) -> str:
    parts = Path(name).stem.split("_")
    return parts[1] if len(parts) > 1 else "unknown"


# Strategies whose encoder/decoder require a (tensor) conditioning vector.
_TENSOR_COND_STRATEGIES = {7, 8, 9, 9.5, 9.6, 9.7, 9.8, 9.9, 13, 14}


def _neutral_cond(model, batch: int, device: torch.device):
    """Build a neutral (0.5) conditioning tensor for conditional VAEs, else None.

    Returns None for unconditional models and for the dict-conditioned
    strategies 15/16 (handled by the caller's graceful fallback).
    """
    strategy = getattr(model, "_strategy", 0)
    if strategy not in _TENSOR_COND_STRATEGIES:
        return None
    cfg = getattr(model, "_config", None)
    cond_vars = getattr(cfg, "conditional_variables", None)
    if cond_vars is None and isinstance(cfg, dict):
        cond_vars = cfg.get("conditional_variables")
    cond_dim = len(cond_vars) if cond_vars else 1
    return torch.full((batch, cond_dim), 0.5, device=device)


def _per_dim_kl(mu: np.ndarray, logvar: np.ndarray) -> np.ndarray:
    """Mean per-dimension KL(q||N(0,1)) across samples: 0.5*(mu^2 + var - logvar - 1)."""
    kl = 0.5 * (mu ** 2 + np.exp(logvar) - logvar - 1.0)
    return kl.mean(axis=0)


def _find_loss_history(model_dir: Path) -> Optional[dict]:
    """Look for a saved loss history (json/csv) near the model dir."""
    run_dir = model_dir.parent
    for cand in [run_dir / "loss_statistics", run_dir, model_dir]:
        for jf in cand.glob("*.json"):
            try:
                data = json.loads(jf.read_text())
            except Exception:
                continue
            # accept {"train": [...], "val": [...]} or a list of epoch dicts
            if isinstance(data, dict) and any(
                k in data for k in ("train", "val", "train_loss", "loss")
            ):
                return data
            if isinstance(data, list) and data and isinstance(data[0], dict):
                return {"epochs": data}
    return None


# --------------------------------------------------------------------------- #
# Page
# --------------------------------------------------------------------------- #
def main() -> None:
    st.title("Model Quality")
    st.caption(
        "A per-model scorecard: reconstruction fidelity, posterior-collapse / "
        "active-dimension diagnostics, latent geometry, and training history — "
        "the evaluation story from Harry's thesis, in one view."
    )

    with st.sidebar:
        render_research_mode_toggle()
        st.subheader("Model")
        models = _discover_vae_models(research_mode_enabled())
        if not models:
            st.warning("No VAE models found in the zoo.")
            return
        labels = [m[0] for m in models]
        choice = st.selectbox("Trained model", labels)
        model_dir = dict(models)[choice]
        device = st.radio(
            "Device", ["cuda:0", "cpu"] if torch.cuda.is_available() else ["cpu"], index=0
        )
        kl_thresh = st.number_input(
            "Active-dim KL threshold", min_value=0.001, max_value=1.0, value=0.01, step=0.005,
            help="A latent dimension counts as 'active' if its mean KL exceeds this.",
        )

    img_dir = render_image_source_picker(
        key_prefix="mq_img", default_dir=str(_settings.image_dir), label="Evaluation images"
    )
    max_n = st.slider("Number of images to evaluate", 20, 500, 150)

    if not st.button("Run quality report", type="primary"):
        st.info("Pick a model + images, then run the report.")
        return

    files = _list_images(img_dir, max_n)
    if len(files) < 5:
        st.error("Need at least 5 images.")
        return

    # ---- load model + encode/decode ----------------------------------------
    from candescence.interface.model_loader import load_tlv_model

    try:
        with st.spinner("Loading model…"):
            model = load_tlv_model(str(Path(model_dir) / "model.pth"))
            model.to(torch.device(device)).eval()
    except Exception as exc:
        st.error(f"Failed to load model: {exc}")
        return

    x = _load_batch(files).to(torch.device(device))
    cond = _neutral_cond(model, len(files), torch.device(device))
    recon = mu = logvar = None
    try:
        with st.spinner(f"Encoding + reconstructing {len(files)} images…"):
            recon, _, mu, logvar = model.encode_and_decode(x, cond, cond)
            recon = recon.detach().cpu()
            mu = mu.detach().cpu().numpy()
            logvar = logvar.detach().cpu().numpy()
    except Exception as exc:
        st.warning(
            f"Full encode+decode failed (conditioning may be model-specific): {exc}. "
            "Trying latent-only metrics…"
        )
        try:
            _, mu, logvar = model.encode_full(x, cond)
            mu = mu.detach().cpu().numpy()
            logvar = logvar.detach().cpu().numpy()
        except Exception as exc2:
            st.error(f"Encoding failed: {exc2}")
            return

    media = np.array([_media_from_name(Path(f).name) for f in files])

    # ---- 1. Reconstruction fidelity ----------------------------------------
    st.subheader("1 · Reconstruction fidelity")
    if recon is not None:
        x_cpu = x.detach().cpu()
        mse = ((x_cpu - recon) ** 2).mean().item()
        psnr = 10 * np.log10(1.0 / mse) if mse > 0 else 100.0
        c1, c2 = st.columns(2)
        c1.metric("MSE", f"{mse:.4f}")
        c2.metric("PSNR (dB)", f"{psnr:.1f}")
        st.caption("Example originals (top) vs reconstructions (bottom):")
        n_show = min(6, len(files))
        cols = st.columns(n_show)
        for i in range(n_show):
            o = x_cpu[i].permute(1, 2, 0).numpy()
            r = recon[i].permute(1, 2, 0).clamp(0, 1).numpy()
            cols[i].image(o, clamp=True, use_container_width=True)
            cols[i].image(r, clamp=True, use_container_width=True)
    else:
        st.info("Reconstruction unavailable for this model (conditioning required).")

    # ---- 2. Posterior collapse / active dimensions -------------------------
    st.subheader("2 · Posterior collapse & active dimensions")
    kl_per_dim = _per_dim_kl(mu, logvar)
    active = int((kl_per_dim > kl_thresh).sum())
    total = kl_per_dim.shape[0]
    c1, c2, c3 = st.columns(3)
    c1.metric("Active dims", f"{active} / {total}")
    c2.metric("Active fraction", f"{active / total:.0%}")
    c3.metric("Mean KL/dim", f"{kl_per_dim.mean():.3f}")
    import plotly.express as px
    order = np.argsort(kl_per_dim)[::-1]
    st.plotly_chart(
        px.bar(x=np.arange(total), y=kl_per_dim[order],
               labels={"x": "latent dim (sorted)", "y": "mean KL"},
               title="Per-dimension KL (higher = more used)", height=300),
        use_container_width=True,
    )
    if active / total < 0.25:
        st.warning("Low active fraction — this model shows signs of posterior collapse.")

    # ---- 3. Latent geometry (PCA scree) ------------------------------------
    st.subheader("3 · Latent geometry")
    from sklearn.decomposition import PCA
    k = min(20, mu.shape[1], mu.shape[0])
    pca = PCA(n_components=k).fit(mu)
    ev = pca.explained_variance_ratio_
    st.plotly_chart(
        px.bar(x=np.arange(1, k + 1), y=ev,
               labels={"x": "principal component", "y": "explained variance"},
               title=f"PCA scree — top {k} components (cumulative "
                     f"{ev.cumsum()[-1]:.0%})", height=300),
        use_container_width=True,
    )

    # ---- 4. Latent structure vs media (silhouette) -------------------------
    st.subheader("4 · Latent structure (silhouette by media)")
    try:
        from candescence.tlv.analysis.latent_space_metrics import silhouette_metrics
        res = silhouette_metrics(mu, media, exclude_unlabelled=True)
        st.metric("Overall silhouette", f"{res.overall_score:.3f}")
        st.caption(
            "Higher = colonies of the same medium cluster together in latent "
            "space (−1 to 1)."
        )
        # Per-media mean from the per-sample scores (aligned to the rows used).
        used_media = media[res.mask] if res.mask.shape[0] == len(media) else None
        if used_media is not None and len(res.sample_scores) == len(used_media):
            import pandas as pd
            per = (
                pd.DataFrame({"media": used_media, "silhouette": res.sample_scores})
                .groupby("media")["silhouette"].mean().round(3).reset_index()
            )
            st.dataframe(per, use_container_width=True, hide_index=True)
    except Exception as exc:
        st.info(f"Silhouette unavailable: {exc}")

    # ---- 5. Training history -----------------------------------------------
    st.subheader("5 · Training history")
    hist = _find_loss_history(Path(model_dir))
    if hist is None:
        st.info("No saved loss history found alongside this model.")
    else:
        import pandas as pd
        try:
            if "epochs" in hist:
                df = pd.DataFrame(hist["epochs"])
            else:
                df = pd.DataFrame({k: v for k, v in hist.items() if isinstance(v, list)})
            numeric = df.select_dtypes("number")
            if not numeric.empty:
                st.line_chart(numeric)
            else:
                st.info("Loss history found but no numeric series to plot.")
        except Exception as exc:
            st.info(f"Could not parse loss history: {exc}")


main()
