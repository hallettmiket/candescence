"""
Purpose: TLV-specific latent space explorer page
Author: Hallett Lab
Date: 2026-01-28

Provides the main TLV exploration page with latent space visualization,
image display, and interpolation tools.
"""

from pathlib import Path
from typing import Any, Dict, Optional

import numpy as np
import pandas as pd

from candescence.core.logging_config import get_logger
from candescence.interface.base import CandescenceModel

logger = get_logger("candescence.interface.pages.tlv_explorer")

# Optional imports
try:
    import streamlit as st
    STREAMLIT_AVAILABLE = True
except ImportError:
    STREAMLIT_AVAILABLE = False


class TLVExplorerPage:
    """
    TLV Latent Space Explorer page.

    Provides:
    - 2D visualization of latent space (UMAP/t-SNE/PCA)
    - Click on points to view real images
    - Click on empty space to decode from that position
    - Interpolation between points
    - Conditioning controls

    Example:
        >>> page = TLVExplorerPage()
        >>> page.render(model, embeddings, metadata_df)
    """

    def __init__(self) -> None:
        """Initialize the explorer page."""
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit required for TLVExplorerPage")

    def render(
        self,
        model: CandescenceModel,
        embeddings: np.ndarray,
        metadata_df: pd.DataFrame,
        images_df: Optional[pd.DataFrame] = None,
        conditioning: Optional[np.ndarray] = None
    ) -> None:
        """
        Render the TLV explorer page.

        Args:
            model: Loaded TLV model wrapper
            embeddings: Latent vectors (N, latent_dim)
            metadata_df: Metadata with 'id' and feature columns
            images_df: Optional DataFrame with 'id' and 'rgb_image' columns
            conditioning: Optional conditioning values (N, cond_dim)
        """
        from candescence.interface.components import (
            LatentExplorer,
            ImagePanel,
            InterpolationTool
        )

        # Sidebar controls
        self._render_sidebar(model.model_info)

        # Get visualization settings from session state
        reduction_method = st.session_state.get('reduction_method', 'umap')
        color_by = st.session_state.get('color_by', None)

        # Create explorer
        explorer = LatentExplorer(
            embeddings=embeddings,
            metadata_df=metadata_df,
            model=model,
            reduction_method=reduction_method
        )

        # Main layout
        col_left, col_right = st.columns([2, 1])

        with col_left:
            self._render_latent_plot(explorer, color_by)

        with col_right:
            self._render_image_panel(
                explorer,
                images_df,
                conditioning
            )

        # Counterfactual conditioning swap (Strategy 15/16 diagnostic).
        st.divider()
        self._render_counterfactual_swap(
            model,
            metadata_df,
            images_df,
            conditioning,
        )

        # Interpolation section
        st.divider()
        self._render_interpolation(
            model,
            embeddings,
            conditioning
        )

    def _render_sidebar(self, model_info: Dict[str, Any]) -> None:
        """Render sidebar controls."""
        with st.sidebar:
            st.header("Model Info")
            st.text(f"Name: {model_info.get('name', 'Unknown')}")
            st.text(f"Architecture: {model_info.get('architecture', 'Unknown')}")
            st.text(f"Latent dim: {model_info.get('latent_dim', 'Unknown')}")
            st.text(f"Strategy: {model_info.get('strategy', 'Unknown')}")

            if model_info.get('is_production', False):
                st.success("✅ Production Model")
            else:
                st.warning("⚠️ Research Model")

            st.divider()

            st.header("Visualization")

            reduction_method = st.selectbox(
                "Projection Method",
                ["umap", "tsne", "pca"],
                key="reduction_method"
            )

            st.divider()

            st.header("Conditioning")
            st.info(
                "Conditioning controls allow you to modify "
                "the decoder's behavior when generating images."
            )

    def _render_latent_plot(
        self,
        explorer: "LatentExplorer",
        color_by: Optional[str]
    ) -> None:
        """Render the latent space plot."""
        st.subheader("Latent Space")

        # Color selection
        numeric_cols = explorer.metadata_df.select_dtypes(
            include=[np.number]
        ).columns.tolist()

        if numeric_cols:
            color_by = st.selectbox(
                "Color by",
                [None] + numeric_cols,
                key="color_by",
                format_func=lambda x: "None" if x is None else x
            )

        # Create and display figure
        fig = explorer.create_figure(color_by=color_by)

        # Use plotly_events if available for click handling
        try:
            from streamlit_plotly_events import plotly_events
            selected = plotly_events(
                fig,
                click_event=True,
                select_event=False,
                key="latent_plot"
            )

            if selected:
                point = selected[0]
                if 'pointIndex' in point:
                    st.session_state['selected_index'] = point['pointIndex']
                else:
                    # Clicked on empty space
                    st.session_state['clicked_x'] = point.get('x', 0)
                    st.session_state['clicked_y'] = point.get('y', 0)
                    st.session_state['selected_index'] = None

        except ImportError:
            # Fallback: just display the plot
            st.plotly_chart(fig, use_container_width=True)
            st.info(
                "Install streamlit-plotly-events for click interaction: "
                "`pip install streamlit-plotly-events`"
            )

            # Manual index input as fallback
            max_idx = explorer.n_samples - 1
            selected_idx = st.number_input(
                "Select sample index",
                min_value=0,
                max_value=max_idx,
                value=0,
                key="manual_index"
            )
            st.session_state['selected_index'] = selected_idx

    def _render_image_panel(
        self,
        explorer: "LatentExplorer",
        images_df: Optional[pd.DataFrame],
        conditioning: Optional[np.ndarray]
    ) -> None:
        """Render the image display panel."""
        from candescence.interface.components import ImagePanel

        panel = ImagePanel()
        selected_idx = st.session_state.get('selected_index')

        if selected_idx is not None:
            # Show real image
            metadata = explorer.get_metadata_at_index(selected_idx)

            # Try to get real image
            image = None
            if images_df is not None and 'rgb_image' in images_df.columns:
                sample_id = metadata.get('id')
                if sample_id is not None:
                    row = images_df[images_df['id'] == sample_id]
                    if len(row) > 0:
                        image = row.iloc[0]['rgb_image']

            if image is not None:
                panel.render_streamlit(
                    image,
                    metadata,
                    title=f"Sample {selected_idx}"
                )
            else:
                st.warning(f"Image not found for sample {selected_idx}")
                st.json(metadata)

        elif 'clicked_x' in st.session_state:
            # Decode from empty space
            x = st.session_state['clicked_x']
            y = st.session_state['clicked_y']

            st.subheader("Generated Image")
            st.text(f"Position: ({x:.2f}, {y:.2f})")

            with st.spinner("Decoding..."):
                # Get conditioning if available
                cond = None
                if conditioning is not None:
                    # Use average conditioning as default
                    cond_mean = conditioning.mean(axis=0)
                    import torch
                    cond = torch.tensor(cond_mean, dtype=torch.float32).unsqueeze(0)

                decoded = explorer.decode_from_2d(x, y, cond=cond)

                if decoded is not None:
                    panel.render_streamlit(
                        decoded,
                        {'x': x, 'y': y, 'type': 'generated'},
                        title="Generated from Latent"
                    )
                else:
                    st.error("Decoding failed")
        else:
            panel.render_streamlit(None)

    def _render_counterfactual_swap(
        self,
        model: CandescenceModel,
        metadata_df: pd.DataFrame,
        images_df: Optional[pd.DataFrame],
        conditioning: Optional[np.ndarray],
    ) -> None:
        """Counterfactual conditioning swap diagnostic (Strategy 15/16).

        For the currently-selected real sample, encode+decode twice — once
        with the true cond dict, once with a modified copy (e.g. hue zeroed,
        media replaced) — and render both reconstructions side-by-side with
        a pixel/μ delta. If the reconstructions are identical, the FiLM
        conditioning pathway is inert.
        """
        import torch
        from candescence.interface.training.cond_utils import (
            build_multi_cond_dict,
            resolve_categories,
            resolve_cond_keys,
        )

        st.subheader("Counterfactual Conditioning Swap")
        st.caption(
            "Diagnostic: feed the selected image through the encoder and "
            "decoder twice with different cond vectors. If the two "
            "reconstructions look identical, the FiLM pathway is inert."
        )

        active_keys = resolve_cond_keys(model)
        if not active_keys:
            st.info(
                "Counterfactual swap is only available for Strategy 15 / 16 "
                "models with per-variable FiLM conditioning."
            )
            return

        selected_idx = st.session_state.get('selected_index')
        if selected_idx is None:
            st.info(
                "Select a real sample on the latent plot above to enable "
                "the swap."
            )
            return

        if selected_idx < 0 or selected_idx >= len(metadata_df):
            st.warning(f"Selected index {selected_idx} is out of range.")
            return

        cats = resolve_categories(model)
        if 'media' in active_keys and not cats['media']:
            st.error(
                "Model is missing media_categories in args.json — cannot "
                "reconstruct the media one-hot vector."
            )
            return
        if 'day' in active_keys and not cats['day']:
            st.error(
                "Model is missing day_categories in args.json — cannot "
                "reconstruct the day one-hot vector."
            )
            return
        if 'hue' in active_keys and conditioning is None:
            st.error(
                "Hue conditioning array was not provided to the explorer — "
                "cannot run swap for the hue pathway."
            )
            return
        if 'plate_phys' in active_keys and not cats.get('plate_phys'):
            st.error(
                "Model is missing plate_phys_categories in args.json — "
                "cannot reconstruct the plate_phys one-hot vector."
            )
            return

        metadata_row = metadata_df.iloc[selected_idx]
        swap_spec = self._build_swap_spec_ui(
            active_keys=active_keys,
            media_categories=cats['media'],
            day_categories=cats['day'],
            hue_dim=conditioning.shape[1] if conditioning is not None and conditioning.ndim == 2 else 1,
            plate_phys_categories=cats.get('plate_phys', []),
        )
        if swap_spec is None:
            return  # form not submitted yet

        input_tensor = self._get_input_tensor_for_swap(
            images_df=images_df,
            metadata_row=metadata_row,
            device=model.device,
        )
        if input_tensor is None:
            st.warning(
                "Could not locate the input image tensor for the selected "
                "sample — swap cannot run."
            )
            return

        hue_vec = (
            conditioning[selected_idx]
            if conditioning is not None
            else np.zeros(0, dtype=np.float32)
        )
        cond_orig = build_multi_cond_dict(
            hue_vec=hue_vec,
            metadata_row=metadata_row,
            cond_keys=active_keys,
            media_categories=cats['media'],
            day_categories=cats['day'],
            device=model.device,
            plate_phys_categories=cats.get('plate_phys', []),
        )
        cond_swap = self._apply_swap_to_cond(cond_orig, swap_spec, active_keys, cats)

        with st.spinner("Running encode/decode for original and swapped cond..."):
            recon_orig, _, mu_o, _ = model.encode_and_decode(input_tensor, cond_orig)
            recon_swap, _, mu_s, _ = model.encode_and_decode(input_tensor, cond_swap)

        mean_abs_diff = float((recon_orig - recon_swap).abs().mean().item())
        mu_l2 = float(torch.norm(mu_o - mu_s).item())

        self._render_swap_results(
            input_tensor=input_tensor,
            recon_orig=recon_orig,
            recon_swap=recon_swap,
            swap_label=self._summarize_swap_spec(swap_spec, active_keys),
            mean_abs_diff=mean_abs_diff,
            mu_l2=mu_l2,
        )

    def _build_swap_spec_ui(
        self,
        active_keys: tuple,
        media_categories: list,
        day_categories: list,
        hue_dim: int,
        plate_phys_categories: Optional[list] = None,
    ) -> Optional[Dict[str, str]]:
        """Render swap-mode selectboxes inside a form. Returns the spec
        dict on submit, or ``None`` while the form is still being edited.
        """
        plate_phys_categories = list(plate_phys_categories or [])
        with st.form("cf_swap_form"):
            cols = st.columns(len(active_keys))
            spec: Dict[str, str] = {}

            for col, key in zip(cols, active_keys):
                with col:
                    if key == 'hue':
                        hue_options = ['original', 'zero', 'shuffled']
                        if hue_dim == 1:
                            hue_options.append('opposite (circular +0.5)')
                        else:
                            hue_options.append('phase-shift (+0.5 mod 1)')
                        spec['hue'] = st.selectbox(
                            "hue",
                            hue_options,
                            key="cf_swap_hue",
                        )
                    elif key == 'media':
                        options = ['original', 'zero vector', 'uniform'] + [
                            f"swap to {c}" for c in media_categories
                        ]
                        spec['media'] = st.selectbox(
                            "media",
                            options,
                            key="cf_swap_media",
                        )
                    elif key == 'day':
                        options = ['original', 'zero vector', 'uniform'] + [
                            f"swap to {c}" for c in day_categories
                        ]
                        spec['day'] = st.selectbox(
                            "day",
                            options,
                            key="cf_swap_day",
                        )
                    elif key == 'plate_phys':
                        options = ['original', 'zero vector', 'uniform'] + [
                            f"swap to {c}" for c in plate_phys_categories
                        ]
                        spec['plate_phys'] = st.selectbox(
                            "plate_phys",
                            options,
                            key="cf_swap_plate_phys",
                        )

            submitted = st.form_submit_button("Run swap")

        return spec if submitted else None

    @staticmethod
    def _apply_swap_to_cond(
        cond_orig: Dict[str, "torch.Tensor"],
        swap_spec: Dict[str, str],
        active_keys: tuple,
        cats: Dict[str, list],
    ) -> Dict[str, "torch.Tensor"]:
        """Produce a new cond dict with per-key swaps applied. Clones all
        tensors so ``cond_orig`` is never mutated.
        """
        import torch

        out: Dict[str, torch.Tensor] = {}
        for key in active_keys:
            tensor = cond_orig[key].clone()
            mode = swap_spec.get(key, 'original')

            if mode == 'original':
                out[key] = tensor
                continue

            if key == 'hue':
                if mode == 'zero':
                    out[key] = torch.zeros_like(tensor)
                elif mode == 'shuffled':
                    perm = torch.randperm(tensor.shape[1], device=tensor.device)
                    out[key] = tensor[:, perm]
                else:  # opposite / phase-shift
                    out[key] = torch.remainder(tensor + 0.5, 1.0)
            else:  # media / day — categorical one-hot
                if mode == 'zero vector':
                    out[key] = torch.zeros_like(tensor)
                elif mode == 'uniform':
                    k = tensor.shape[1]
                    out[key] = torch.full_like(tensor, 1.0 / max(k, 1))
                elif mode.startswith('swap to '):
                    target = mode[len('swap to '):]
                    categories = cats.get(key, [])
                    if target in categories:
                        swapped = torch.zeros_like(tensor)
                        swapped[0, categories.index(target)] = 1.0
                        out[key] = swapped
                    else:
                        out[key] = tensor  # unknown target — fall through
                else:
                    out[key] = tensor

        return out

    def _get_input_tensor_for_swap(
        self,
        images_df: Optional[pd.DataFrame],
        metadata_row: "pd.Series",
        device: "torch.device",
    ) -> Optional["torch.Tensor"]:
        """Fetch a (1, 3, H, W) float tensor for the selected sample."""
        import torch

        if images_df is None:
            return None

        sample_id = metadata_row.get('id') if hasattr(metadata_row, 'get') else None
        if sample_id is None and 'id' in metadata_row.index:
            sample_id = metadata_row['id']
        if sample_id is None:
            return None

        rows = images_df[images_df['id'] == sample_id]
        if len(rows) == 0:
            return None
        row = rows.iloc[0]

        if 'transformed_image' in images_df.columns:
            img = row['transformed_image']
            if isinstance(img, torch.Tensor):
                tensor = img.float()
            else:
                tensor = torch.as_tensor(np.asarray(img), dtype=torch.float32)
        elif 'rgb_image' in images_df.columns:
            from torchvision import transforms
            pil = row['rgb_image']
            tensor = transforms.ToTensor()(pil).float()
        else:
            return None

        if tensor.ndim == 3:
            tensor = tensor.unsqueeze(0)
        return tensor.to(device)

    def _render_swap_results(
        self,
        input_tensor: "torch.Tensor",
        recon_orig: "torch.Tensor",
        recon_swap: "torch.Tensor",
        swap_label: str,
        mean_abs_diff: float,
        mu_l2: float,
    ) -> None:
        """Render the three-column image comparison and delta metrics."""
        from candescence.interface.components import ImagePanel

        panel = ImagePanel()
        input_np = panel.prepare_image(input_tensor.detach().cpu().numpy())
        orig_np = panel.prepare_image(recon_orig.detach().cpu().numpy())
        swap_np = panel.prepare_image(recon_swap.detach().cpu().numpy())

        col_in, col_orig, col_swap = st.columns(3)
        with col_in:
            st.image(input_np, caption="Input")
        with col_orig:
            st.image(orig_np, caption="Recon @ original cond")
        with col_swap:
            st.image(swap_np, caption=f"Recon @ {swap_label}")

        m1, m2 = st.columns(2)
        with m1:
            st.metric("Mean |Δpixel|", f"{mean_abs_diff:.5f}")
        with m2:
            st.metric("‖Δμ‖₂", f"{mu_l2:.4f}")

        if mean_abs_diff < 1e-5 and swap_label != "original":
            st.warning(
                "Reconstructions are pixel-identical across the swap. "
                "FiLM pathway is likely inert for this variable."
            )

    @staticmethod
    def _summarize_swap_spec(swap_spec: Dict[str, str], active_keys: tuple) -> str:
        """One-line human-readable label for the swap spec."""
        non_trivial = [
            f"{k}={swap_spec.get(k, 'original')}"
            for k in active_keys
            if swap_spec.get(k, 'original') != 'original'
        ]
        return ", ".join(non_trivial) if non_trivial else "original"


def render_counterfactual_swap_section(
    model: Any,
    metadata_df: pd.DataFrame,
    images: Optional[np.ndarray],
    conditioning: Optional[np.ndarray],
    clicked_idx_key: str = "clicked_idx",
) -> None:
    """Module-level entry point for the live latent explorer app.

    Renders the counterfactual conditioning swap diagnostic as a section.
    Reads the selected sample index from
    ``st.session_state[clicked_idx_key]`` and the input image tensor from
    ``images[idx]`` (numpy array of shape (N, C, H, W), float in [0, 1]
    as produced by the explorer's data loading pipeline). See
    :class:`TLVExplorerPage._render_counterfactual_swap` for the full
    design spec — this is the thin wrapper the live app calls.
    """
    if not STREAMLIT_AVAILABLE:
        raise ImportError("Streamlit required")
    import torch
    from candescence.interface.training.cond_utils import (
        build_multi_cond_dict,
        resolve_categories,
        resolve_cond_keys,
    )
    from candescence.interface.components import ImagePanel

    st.subheader("Counterfactual Conditioning Swap")
    st.caption(
        "Diagnostic: feed the selected image through the encoder and "
        "decoder twice with different cond vectors. If the two "
        "reconstructions look identical, the FiLM pathway is inert."
    )

    active_keys = resolve_cond_keys(model)
    if not active_keys:
        st.info(
            "Counterfactual swap is only available for Strategy 15 / 16 "
            "models with per-variable FiLM conditioning."
        )
        return

    idx = st.session_state.get(clicked_idx_key)
    if idx is None:
        st.info(
            "Click a point on the scatter plot above to enable the swap."
        )
        return
    if idx < 0 or idx >= len(metadata_df):
        st.warning(f"Selected index {idx} is out of range.")
        return

    cats = resolve_categories(model)
    if 'media' in active_keys and not cats['media']:
        st.error(
            "Model is missing media_categories in args.json — cannot "
            "reconstruct the media one-hot vector."
        )
        return
    if 'day' in active_keys and not cats['day']:
        st.error(
            "Model is missing day_categories in args.json — cannot "
            "reconstruct the day one-hot vector."
        )
        return
    if 'hue' in active_keys and conditioning is None:
        st.error(
            "Hue conditioning array is None — cannot run swap for the "
            "hue pathway."
        )
        return
    if 'plate_phys' in active_keys and not cats.get('plate_phys'):
        st.error(
            "Model is missing plate_phys_categories in args.json — "
            "cannot reconstruct the plate_phys one-hot vector."
        )
        return
    if images is None:
        st.error("No images are loaded — swap cannot run.")
        return

    hue_dim = (
        conditioning.shape[1]
        if conditioning is not None and conditioning.ndim == 2
        else 1
    )

    with st.form("cf_swap_form"):
        cols = st.columns(len(active_keys))
        spec: Dict[str, str] = {}

        for col, key in zip(cols, active_keys):
            with col:
                if key == 'hue':
                    hue_options = ['original', 'zero', 'shuffled']
                    if hue_dim == 1:
                        hue_options.append('opposite (circular +0.5)')
                    else:
                        hue_options.append('phase-shift (+0.5 mod 1)')
                    spec['hue'] = st.selectbox(
                        "hue", hue_options, key="cf_swap_hue",
                    )
                elif key == 'media':
                    options = ['original', 'zero vector', 'uniform'] + [
                        f"swap to {c}" for c in cats['media']
                    ]
                    spec['media'] = st.selectbox(
                        "media", options, key="cf_swap_media",
                    )
                elif key == 'day':
                    options = ['original', 'zero vector', 'uniform'] + [
                        f"swap to {c}" for c in cats['day']
                    ]
                    spec['day'] = st.selectbox(
                        "day", options, key="cf_swap_day",
                    )
                elif key == 'plate_phys':
                    options = ['original', 'zero vector', 'uniform'] + [
                        f"swap to {c}" for c in cats.get('plate_phys', [])
                    ]
                    spec['plate_phys'] = st.selectbox(
                        "plate_phys", options, key="cf_swap_plate_phys",
                    )

        submitted = st.form_submit_button("Run swap")

    if not submitted:
        return

    img_np = np.asarray(images[idx])
    input_tensor = torch.from_numpy(img_np).float()
    if input_tensor.ndim == 3:
        input_tensor = input_tensor.unsqueeze(0)
    input_tensor = input_tensor.to(model.device)

    hue_vec = (
        conditioning[idx]
        if conditioning is not None
        else np.zeros(0, dtype=np.float32)
    )
    cond_orig = build_multi_cond_dict(
        hue_vec=hue_vec,
        metadata_row=metadata_df.iloc[idx],
        cond_keys=active_keys,
        media_categories=cats['media'],
        day_categories=cats['day'],
        device=model.device,
        plate_phys_categories=cats.get('plate_phys', []),
    )
    cond_swap = TLVExplorerPage._apply_swap_to_cond(
        cond_orig, spec, active_keys, cats,
    )

    with st.spinner("Running encode/decode for original and swapped cond..."):
        recon_orig, _, mu_o, _ = model.encode_and_decode(input_tensor, cond_orig)
        recon_swap, _, mu_s, _ = model.encode_and_decode(input_tensor, cond_swap)

    mean_abs_diff = float((recon_orig - recon_swap).abs().mean().item())
    mu_l2 = float(torch.norm(mu_o - mu_s).item())
    swap_label = TLVExplorerPage._summarize_swap_spec(spec, active_keys)

    panel = ImagePanel()
    col_in, col_orig, col_swap = st.columns(3)
    with col_in:
        st.image(
            panel.prepare_image(input_tensor.detach().cpu().numpy()),
            caption="Input",
        )
    with col_orig:
        st.image(
            panel.prepare_image(recon_orig.detach().cpu().numpy()),
            caption="Recon @ original cond",
        )
    with col_swap:
        st.image(
            panel.prepare_image(recon_swap.detach().cpu().numpy()),
            caption=f"Recon @ {swap_label}",
        )

    m1, m2 = st.columns(2)
    with m1:
        st.metric("Mean |Δpixel|", f"{mean_abs_diff:.5f}")
    with m2:
        st.metric("‖Δμ‖₂", f"{mu_l2:.4f}")

    if mean_abs_diff < 1e-5 and swap_label != "original":
        st.warning(
            "Reconstructions are pixel-identical across the swap. "
            "FiLM pathway is likely inert for this variable."
        )

    def _render_interpolation(
        self,
        model: CandescenceModel,
        embeddings: np.ndarray,
        conditioning: Optional[np.ndarray]
    ) -> None:
        """Render interpolation section."""
        from candescence.interface.components import InterpolationTool

        st.subheader("Interpolation")

        with st.expander("Interpolation Tool", expanded=False):
            tool = InterpolationTool(
                model=model,
                embeddings=embeddings,
                conditioning=conditioning
            )

            images = tool.render_streamlit()

            if images is not None:
                st.divider()
                display_mode = st.radio(
                    "Display mode",
                    ["filmstrip", "slider"],
                    horizontal=True,
                    key="interp_display_mode"
                )

                if display_mode == "filmstrip":
                    tool.display_filmstrip(images)
                else:
                    tool.display_slider(images)
