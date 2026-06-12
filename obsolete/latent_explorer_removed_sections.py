"""
Archived diagnostic sections removed from latent_explorer_app.py.

Date removed: 2026-04-10
Reason: These sections were unused and slowed down TLV Explorer page loading.
Source: src/candescence/interface/apps/latent_explorer_app.py

Sections:
    1. Posterior Collapse Diagnostics
    2. PC-Sorted Image Strips
    3. PC vs Image Features (+ _display_correlation_results helper)
    4. Skip-Space Latent Traversals
    5. Skip Saliency Maps (+ _gaussian_smooth_2d, _saliency_to_heatmap, _blend_overlay helpers)
    6. Skip Channel Statistics (+ _display_skip_channel_stats helper)
    7. Batch Effect Diagnostics

NOTE: This file is an archive only — it is not intended to be imported or run directly.
The analysis modules under candescence.tlv.analysis.* remain in place.
"""

import numpy as np
import pandas as pd
import streamlit as st


# ============================================================================
# 1. Posterior Collapse Diagnostics
# ============================================================================

def _render_posterior_collapse_diagnostics():
    """Render posterior collapse diagnostics section."""
    st.header("Posterior Collapse Diagnostics")

    all_mu = st.session_state.get('all_mu', {})
    all_logvar = st.session_state.get('all_logvar', {})

    # Check if we have any mu/logvar data
    mu = st.session_state.get('mu')
    logvar = st.session_state.get('logvar')

    if mu is None or logvar is None:
        st.info(
            "Posterior collapse diagnostics require loading from a trained model "
            "(mu and logvar are needed). Load data using 'From Model' to enable this section."
        )
        return

    # Shared KL threshold slider
    kl_threshold = st.slider(
        "KL threshold (dims below this are considered collapsed)",
        min_value=0.001,
        max_value=1.0,
        value=0.1,
        step=0.01,
        key="kl_collapse_threshold"
    )

    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        space_names = ['primary'] + list(tendril_keys)
        for space_name in space_names:
            space_mu = all_mu.get(space_name)
            space_logvar = all_logvar.get(space_name)
            if space_mu is None or space_logvar is None:
                continue
            label = "Primary (z)" if space_name == 'primary' else f"Tendril: {space_name}"
            with st.expander(label, expanded=(space_name == 'primary')):
                _render_collapse_for_space(
                    mu=space_mu,
                    logvar=space_logvar,
                    kl_threshold=kl_threshold,
                    key_suffix=f"_{space_name}",
                )
    else:
        _render_collapse_for_space(
            mu=mu,
            logvar=logvar,
            kl_threshold=kl_threshold,
            key_suffix="",
        )


def _render_collapse_for_space(
    mu: np.ndarray,
    logvar: np.ndarray,
    kl_threshold: float,
    key_suffix: str,
):
    """Render posterior collapse diagnostics for a single latent space."""
    import plotly.graph_objects as go

    # Compute per-dimension KL divergence
    kl_per_dim = 0.5 * np.mean(
        mu ** 2 + np.exp(logvar) - logvar - 1, axis=0
    )

    latent_dim = len(kl_per_dim)

    active_mask = kl_per_dim > kl_threshold
    n_active = int(np.sum(active_mask))
    n_collapsed = latent_dim - n_active

    # Severity assessment
    if n_collapsed == 0:
        severity = "No collapse"
    elif n_active >= latent_dim * 0.5:
        severity = f"Partial collapse ({n_active}/{latent_dim} dims active)"
    else:
        severity = f"Severe collapse ({n_active}/{latent_dim} dims active)"

    # Summary metrics
    c1, c2, c3, c4 = st.columns(4)
    c1.metric("Total Dimensions", latent_dim)
    c2.metric("Active Dimensions", n_active)
    c3.metric("Collapsed Dimensions", n_collapsed)
    c4.metric("Severity", severity)

    # Per-Dimension KL Divergence Bar Chart
    st.subheader("Per-Dimension KL Divergence")

    colors = ['green' if active else 'red' for active in active_mask]

    fig_kl = go.Figure()
    fig_kl.add_trace(go.Bar(
        x=list(range(latent_dim)),
        y=kl_per_dim,
        marker_color=colors,
        name="KL per dim"
    ))
    fig_kl.add_hline(
        y=kl_threshold,
        line_dash="dash",
        line_color="orange",
        annotation_text=f"threshold = {kl_threshold}",
        annotation_position="top right"
    )
    fig_kl.update_layout(
        xaxis_title="Latent Dimension",
        yaxis_title="KL Divergence",
        height=400,
        margin=dict(l=40, r=20, t=30, b=40)
    )
    st.plotly_chart(fig_kl, use_container_width=True, key=f"kl_bar_chart{key_suffix}")

    # Per-Dimension Latent Distribution Plot
    st.subheader("Per-Dimension Latent Distributions")
    st.caption("Histograms of mu values per dimension. Grey curve = N(0,1) prior. Red title = collapsed.")

    layout_mode = st.radio(
        "Layout",
        options=["vertical", "horizontal"],
        format_func=lambda x: "Vertical (tall)" if x == "vertical" else "Horizontal (wide, for landscape)",
        horizontal=True,
        key=f"latent_dist_layout{key_suffix}"
    )
    n_cols = 8 if layout_mode == "horizontal" else 4
    n_rows = (latent_dim + n_cols - 1) // n_cols

    from plotly.subplots import make_subplots
    import scipy.stats as stats

    fig_dist = make_subplots(
        rows=n_rows, cols=n_cols,
        subplot_titles=[f"z_{i}" for i in range(latent_dim)],
        vertical_spacing=max(0.02, 0.3 / n_rows),
        horizontal_spacing=0.05
    )

    # Reference N(0,1) curve
    x_ref = np.linspace(-4, 4, 200)
    y_ref = stats.norm.pdf(x_ref)

    for i in range(latent_dim):
        row = i // n_cols + 1
        col = i % n_cols + 1

        mu_vals = mu[:, i]

        fig_dist.add_trace(
            go.Histogram(
                x=mu_vals,
                nbinsx=40,
                histnorm='probability density',
                marker_color='red' if not active_mask[i] else 'steelblue',
                opacity=0.7,
                showlegend=False
            ),
            row=row, col=col
        )

        fig_dist.add_trace(
            go.Scatter(
                x=x_ref, y=y_ref,
                mode='lines',
                line=dict(color='grey', width=1, dash='dash'),
                showlegend=False
            ),
            row=row, col=col
        )

    fig_dist.update_layout(
        height=max(200, 120 * n_rows),
        margin=dict(l=20, r=20, t=40, b=20)
    )
    # Color collapsed dimension titles red
    for i in range(latent_dim):
        ann_idx = i  # subplot_titles become annotations
        if ann_idx < len(fig_dist.layout.annotations):
            if not active_mask[i]:
                fig_dist.layout.annotations[ann_idx].font = dict(color='red')

    st.plotly_chart(fig_dist, use_container_width=True, key=f"latent_dist_chart{key_suffix}")


# ============================================================================
# 2. PC-Sorted Image Strips
# ============================================================================

def _render_pc_sorted_strips():
    """Render horizontal strips of thumbnails sorted by PC score."""
    from sklearn.decomposition import PCA

    with st.expander("PC-Sorted Image Strips", expanded=False):
        images = st.session_state.images
        if images is None:
            st.info("Load images to view PC-sorted strips.")
            return

        all_embeddings = st.session_state.get('all_embeddings', {})
        if not all_embeddings:
            st.info("No embeddings loaded.")
            return

        # --- Controls ---
        tendril_keys = st.session_state.get('tendril_keys', [])
        space_options = ['primary'] + list(tendril_keys)

        ctrl1, ctrl2, ctrl3, ctrl4 = st.columns(4)
        with ctrl1:
            space_name = st.selectbox(
                "Latent space",
                space_options,
                format_func=lambda x: "Primary (z)" if x == "primary" else f"Tendril: {x}",
                key="pc_strip_space",
            )
        with ctrl2:
            # Determine max PCs available
            emb = all_embeddings.get(space_name)
            if emb is None:
                st.warning(f"No embeddings for '{space_name}'.")
                return
            max_k = min(32, emb.shape[1], emb.shape[0] - 1)
            pc_idx = st.number_input(
                "PC axis (1-based)",
                min_value=1, max_value=max_k, value=1,
                key="pc_strip_axis",
            )
        with ctrl3:
            n_tiles = st.slider(
                "Thumbnails",
                min_value=5, max_value=32, value=16,
                key="pc_strip_n_tiles",
            )
        with ctrl4:
            use_media_filter = st.checkbox(
                "Respect media filter",
                value=False,
                key="pc_strip_media_filter",
            )

        # --- Get or compute PCA scores ---
        scores = st.session_state.get('all_pca_scores', {}).get(space_name)
        if scores is None or scores.shape[1] < pc_idx:
            with st.spinner("Fitting PCA for this space..."):
                pca = PCA(n_components=max_k)
                scores = pca.fit_transform(emb)
                # Cache for reuse
                if 'all_pca_scores' not in st.session_state:
                    st.session_state.all_pca_scores = {}
                st.session_state.all_pca_scores[space_name] = scores

        pc_col = scores[:, pc_idx - 1]  # 0-based internally

        # --- Apply media filter ---
        metadata_df = st.session_state.metadata_df
        if use_media_filter and metadata_df is not None:
            mask = _media_subset_mask(metadata_df)
            valid_idx = np.flatnonzero(mask)
        else:
            valid_idx = np.arange(len(pc_col))

        if len(valid_idx) == 0:
            st.warning("No points match the current media filter.")
            return

        # Sort by PC score and subsample evenly
        order = valid_idx[np.argsort(pc_col[valid_idx])]
        if len(order) > n_tiles:
            pick = np.linspace(0, len(order) - 1, n_tiles, dtype=int)
            order = order[pick]

        # --- Render strip ---
        st.markdown(f"**PC{pc_idx}** — {space_name} — low → high ({len(order)} images)")
        cols = st.columns(len(order))
        for j, idx in enumerate(order):
            with cols[j]:
                st.image(
                    _image_array_for_display(idx, images),
                    caption=f"[{idx}] {pc_col[idx]:.2f}",
                    use_container_width=True,
                )


# ============================================================================
# 3. PC vs Image Features
# ============================================================================

def _render_pc_feature_correlation():
    """Render Pearson / partial correlation table of PC scores vs image features."""
    import plotly.express as px

    from candescence.tlv.analysis.image_feature_table import (
        compute_image_features_batch,
    )
    from candescence.tlv.analysis.pc_feature_correlation import (
        pc_feature_correlation,
    )

    with st.expander("PC vs Image Features", expanded=False):
        images = st.session_state.images
        if images is None:
            st.info("Load images first.")
            return

        all_embeddings = st.session_state.get('all_embeddings', {})
        if not all_embeddings:
            st.info("No embeddings loaded.")
            return

        # --- Controls ---
        tendril_keys = st.session_state.get('tendril_keys', [])
        space_options = ['primary'] + list(tendril_keys)

        ctrl1, ctrl2, ctrl3 = st.columns(3)
        with ctrl1:
            space_name = st.selectbox(
                "Latent space",
                space_options,
                format_func=lambda x: "Primary (z)" if x == "primary" else f"Tendril: {x}",
                key="pcfeat_space",
            )
        with ctrl2:
            n_pcs = st.slider("Number of PCs", 2, 16, 8, key="pcfeat_n_pcs")
        with ctrl3:
            metadata_df = st.session_state.metadata_df
            has_media = metadata_df is not None and "media" in metadata_df.columns
            partial_toggle = st.checkbox(
                "Partial (control for media)",
                value=has_media,
                disabled=not has_media,
                key="pcfeat_partial",
            )

        if not st.button("Run correlation", key="pcfeat_run_btn", type="primary"):
            # Show cached results if available
            cached = st.session_state.get('pcfeat_results')
            if cached is not None:
                _display_correlation_results(*cached)
            return

        # --- Compute features (cached) ---
        feat_cache_key = "image_feature_table_cache"
        feat_df = st.session_state.get(feat_cache_key)
        if feat_df is None or len(feat_df) != len(images):
            with st.spinner("Computing image features..."):
                feat_df = compute_image_features_batch(images)
                st.session_state[feat_cache_key] = feat_df

        # --- Get PCA scores ---
        from sklearn.decomposition import PCA

        scores = st.session_state.get('all_pca_scores', {}).get(space_name)
        emb = all_embeddings.get(space_name)
        if emb is None:
            st.warning(f"No embeddings for '{space_name}'.")
            return
        max_k = min(32, emb.shape[1], emb.shape[0] - 1)
        if scores is None or scores.shape[1] < n_pcs:
            with st.spinner("Fitting PCA..."):
                pca = PCA(n_components=max_k)
                scores = pca.fit_transform(emb)
                if 'all_pca_scores' not in st.session_state:
                    st.session_state.all_pca_scores = {}
                st.session_state.all_pca_scores[space_name] = scores

        # --- Run correlation ---
        covariate = None
        if partial_toggle and has_media:
            covariate = metadata_df["media"].fillna("__missing__").astype(str).values

        with st.spinner("Computing correlations..."):
            pearson_df, partial_df = pc_feature_correlation(
                scores, feat_df, n_pcs=n_pcs, covariate=covariate,
            )

        results = (pearson_df, partial_df, space_name)
        st.session_state['pcfeat_results'] = results
        _display_correlation_results(*results)


def _display_correlation_results(
    pearson_df: "pd.DataFrame",
    partial_df: "Optional[pd.DataFrame]",
    space_name: str,
):
    """Show correlation table and heatmap."""
    import plotly.express as px

    st.markdown(f"**Pearson *r* — {space_name}**")
    st.dataframe(
        pearson_df.style.background_gradient(cmap="RdBu_r", vmin=-1, vmax=1),
        use_container_width=True,
    )

    if partial_df is not None:
        st.markdown(f"**Partial *r* (controlling for media) — {space_name}**")
        st.dataframe(
            partial_df.style.background_gradient(cmap="RdBu_r", vmin=-1, vmax=1),
            use_container_width=True,
        )

    # Heatmap
    display_df = partial_df if partial_df is not None else pearson_df
    label = "Partial r" if partial_df is not None else "Pearson r"
    fig = px.imshow(
        display_df.values,
        x=list(display_df.columns),
        y=list(display_df.index),
        color_continuous_scale="RdBu_r",
        zmin=-1, zmax=1,
        labels=dict(color=label),
        title=f"{label} — {space_name}",
    )
    fig.update_layout(height=300, margin=dict(l=10, r=10, t=35, b=10))
    st.plotly_chart(fig, use_container_width=True)


# ============================================================================
# 4. Skip-Space Latent Traversals
# ============================================================================

def _render_skip_traversal():
    """Render tendril latent traversal image strips."""
    from candescence.tlv.analysis.skip_traversal import (
        skip_channels_to_rgb,
        tendril_latent_walk,
    )

    with st.expander("Skip-Space Latent Traversals", expanded=False):
        model = st.session_state.get('model')
        if model is None or not model.has_tendrils():
            st.info("Load a tendril model checkpoint to use skip-space traversals.")
            return

        images = st.session_state.images
        if images is None:
            st.info("Load images first.")
            return

        conditioning = st.session_state.conditioning
        tendril_keys = model.list_tendril_keys()

        # --- Controls ---
        c1, c2, c3, c4, c5 = st.columns(5)
        with c1:
            anchor = st.number_input(
                "Image index", min_value=0, max_value=len(images) - 1,
                value=st.session_state.get('clicked_idx') or 0,
                key="skip_trav_idx",
            )
        with c2:
            tkey = st.selectbox(
                "Tendril", tendril_keys, key="skip_trav_tendril",
            )
        with c3:
            t_dim = model.get_tendril_latent_dim(tkey)
            dim = st.number_input(
                "Dimension", min_value=0, max_value=t_dim - 1, value=0,
                key="skip_trav_dim",
            )
        with c4:
            n_steps = st.slider(
                "Steps/side", 1, 10, 5, key="skip_trav_nsteps",
            )
        with c5:
            step_size = st.number_input(
                "Step size", min_value=0.1, max_value=5.0, value=1.0,
                step=0.1, key="skip_trav_stepsize",
            )

        show_fmaps = st.checkbox(
            "Show skip-level feature maps", value=False,
            key="skip_trav_show_fmap",
        )

        if not st.button("Generate traversal", key="skip_trav_run", type="primary"):
            return

        with st.spinner("Computing traversal..."):
            decoded, offsets, fmaps = tendril_latent_walk(
                model, int(anchor), images, conditioning,
                tendril_key=tkey, dim=int(dim),
                n_steps=int(n_steps), step_size=float(step_size),
                return_feature_maps=show_fmaps,
            )

        # --- Render strip ---
        st.markdown(f"**Tendril {tkey}, dim {dim}** — {len(decoded)} steps")
        cols = st.columns(len(decoded))
        center = len(decoded) // 2
        for j, (img, off) in enumerate(zip(decoded, offsets)):
            with cols[j]:
                display = _prepare_image(img)
                caption = f"{off:+.2f}" if j != center else f"**0** (orig)"
                st.image(display, caption=caption, use_container_width=True)

        if show_fmaps and fmaps:
            st.markdown("**Feature maps (PCA → RGB)**")
            fmap_cols = st.columns(len(fmaps))
            for j, fm in enumerate(fmaps):
                with fmap_cols[j]:
                    rgb = skip_channels_to_rgb(fm, method="pca")
                    st.image(rgb, caption=f"{offsets[j]:+.2f}",
                             use_container_width=True)


# ============================================================================
# 5. Skip Saliency Maps
# ============================================================================

def _render_skip_saliency():
    """Render gradient-based saliency maps for skip connections."""
    from candescence.tlv.analysis.skip_saliency import (
        compute_skip_saliency,
        compute_tendril_mu_saliency,
    )

    with st.expander("Skip Saliency Maps", expanded=False):
        model = st.session_state.get('model')
        if model is None:
            st.info("Load a model checkpoint to compute saliency maps.")
            return

        images = st.session_state.images
        if images is None:
            st.info("Load images first.")
            return

        conditioning = st.session_state.conditioning

        # --- Controls ---
        c1, c2, c3 = st.columns(3)
        with c1:
            anchor = st.number_input(
                "Image index", min_value=0, max_value=len(images) - 1,
                value=st.session_state.get('clicked_idx') or 0,
                key="sal_idx",
            )
        with c2:
            has_tendrils = model.has_tendrils()
            target_options = ["Skip layer norm", "Skip layer mean"]
            if has_tendrils:
                tendril_keys = model.list_tendril_keys()
                for tk in tendril_keys:
                    target_options.append(f"Tendril {tk} μ (norm)")
            target_choice = st.selectbox(
                "Saliency target", target_options, key="sal_target",
            )
        with c3:
            if target_choice.startswith("Skip layer"):
                layer_idx = st.selectbox(
                    "Skip level", [0, 1, 2, 3],
                    format_func=lambda i: f"x{i+1} ({[64,128,256,512][i]} ch)",
                    key="sal_layer",
                )
            else:
                layer_idx = 0  # not used for tendril targets

        smooth = st.checkbox("Gaussian smoothing", value=True, key="sal_smooth")

        if not st.button("Compute saliency", key="sal_run", type="primary"):
            return

        with st.spinner("Computing saliency (gradient pass)..."):
            if target_choice.startswith("Tendril"):
                # Parse tendril key from "Tendril x1 μ (norm)"
                tk = target_choice.split()[1]
                saliency = compute_tendril_mu_saliency(
                    model, images, conditioning, int(anchor),
                    tendril_key=tk, target="norm",
                )
            else:
                target_type = "mean" if "mean" in target_choice else "norm"
                saliency = compute_skip_saliency(
                    model, images, conditioning, int(anchor),
                    layer_idx=int(layer_idx), target=target_type,
                )

        if smooth:
            saliency = _gaussian_smooth_2d(saliency, sigma=2.0)

        # Normalise to [0, 1]
        s_min, s_max = saliency.min(), saliency.max()
        if s_max - s_min > 1e-12:
            saliency = (saliency - s_min) / (s_max - s_min)

        # --- Display ---
        col_a, col_b, col_c = st.columns(3)
        with col_a:
            st.markdown("**Original**")
            st.image(
                _image_array_for_display(int(anchor), images),
                use_container_width=True,
            )
        with col_b:
            st.markdown("**Saliency**")
            heatmap = _saliency_to_heatmap(saliency)
            st.image(heatmap, use_container_width=True)
        with col_c:
            st.markdown("**Overlay**")
            orig = _image_array_for_display(int(anchor), images)
            overlay = _blend_overlay(orig, heatmap, alpha=0.5)
            st.image(overlay, use_container_width=True)


def _gaussian_smooth_2d(arr: np.ndarray, sigma: float = 2.0) -> np.ndarray:
    """Simple Gaussian blur via separable 1-D convolution (no scipy)."""
    size = int(4 * sigma + 0.5) | 1  # ensure odd
    x = np.arange(size) - size // 2
    kernel = np.exp(-0.5 * (x / sigma) ** 2)
    kernel /= kernel.sum()

    # Pad and convolve rows then columns
    padded = np.pad(arr, size // 2, mode='reflect')
    # Row-wise
    rows = np.apply_along_axis(lambda r: np.convolve(r, kernel, mode='valid'), 1, padded)
    # Re-pad for column pass
    padded2 = np.pad(rows, ((size // 2, size // 2), (0, 0)), mode='reflect')
    return np.apply_along_axis(lambda c: np.convolve(c, kernel, mode='valid'), 0, padded2)


def _saliency_to_heatmap(saliency: np.ndarray) -> np.ndarray:
    """Convert (H, W) saliency [0,1] to a (H, W, 3) uint8 red-yellow heatmap."""
    h, w = saliency.shape
    # Simple red-to-yellow colormap: R=1, G=saliency, B=0
    rgb = np.zeros((h, w, 3), dtype=np.uint8)
    rgb[:, :, 0] = (saliency * 255).astype(np.uint8)          # R
    rgb[:, :, 1] = (saliency * saliency * 255).astype(np.uint8)  # G (quadratic)
    return rgb


def _blend_overlay(
    original: np.ndarray,
    heatmap: np.ndarray,
    alpha: float = 0.5,
) -> np.ndarray:
    """Alpha-blend a heatmap onto an original image."""
    # Resize heatmap to match original if needed
    from PIL import Image as PILImage
    orig = np.asarray(original)
    heat = np.asarray(heatmap)
    if orig.shape[:2] != heat.shape[:2]:
        heat_pil = PILImage.fromarray(heat).resize(
            (orig.shape[1], orig.shape[0]), PILImage.BILINEAR,
        )
        heat = np.asarray(heat_pil)
    # Handle grayscale original
    if orig.ndim == 2:
        orig = np.stack([orig, orig, orig], axis=-1)
    blended = (alpha * heat.astype(np.float32) + (1 - alpha) * orig.astype(np.float32))
    return np.clip(blended, 0, 255).astype(np.uint8)


# ============================================================================
# 6. Skip Channel Statistics
# ============================================================================

def _render_skip_channel_stats():
    """Render per-channel skip connection statistics and PCA scree plots."""
    import plotly.express as px
    import plotly.graph_objects as go

    from candescence.tlv.analysis.skip_channel_stats import (
        compute_skip_channel_stats,
        skip_pca_summary,
    )

    with st.expander("Skip Channel Statistics", expanded=False):
        model = st.session_state.get('model')
        if model is None:
            st.info("Load a model checkpoint to analyse skip channels.")
            return

        images = st.session_state.images
        if images is None:
            st.info("Load images first.")
            return

        conditioning = st.session_state.conditioning

        # --- Controls ---
        c1, c2 = st.columns(2)
        with c1:
            batch_sz = st.slider(
                "Batch size", 8, 64, 32, key="skip_stats_batch",
            )
        with c2:
            max_samp = st.slider(
                "Max samples", 50, len(images), min(500, len(images)),
                key="skip_stats_max",
            )

        if not st.button("Compute channel stats", key="skip_stats_run", type="primary"):
            # Show cached results
            cached = st.session_state.get('skip_channel_stats_cache')
            if cached is not None:
                _display_skip_channel_stats(cached)
            return

        with st.spinner("Encoding images and computing statistics..."):
            stats = compute_skip_channel_stats(
                model, images, conditioning,
                batch_size=int(batch_sz), max_samples=int(max_samp),
            )

            # Also compute PCA scree for each layer
            scree = {}
            for layer_idx in sorted(stats.keys()):
                with st.spinner(f"PCA for skip layer {layer_idx}..."):
                    scree[layer_idx] = skip_pca_summary(
                        model, images, conditioning,
                        layer_idx=layer_idx,
                        batch_size=int(batch_sz),
                        max_samples=int(max_samp),
                    )

        result = (stats, scree)
        st.session_state['skip_channel_stats_cache'] = result
        _display_skip_channel_stats(result)


def _display_skip_channel_stats(result):
    """Display cached skip channel statistics."""
    import plotly.express as px
    import plotly.graph_objects as go

    stats, scree = result

    layer_names = {0: "x1 (64ch)", 1: "x2 (128ch)", 2: "x3 (256ch)", 3: "x4 (512ch)"}
    tabs = st.tabs([layer_names.get(i, f"Layer {i}") for i in sorted(stats.keys())])

    for tab, layer_idx in zip(tabs, sorted(stats.keys())):
        with tab:
            df = stats[layer_idx]
            n_dead = int(df["dead"].sum())
            n_ch = len(df)
            eff_rank = n_ch - n_dead

            # Summary metrics
            m1, m2, m3 = st.columns(3)
            m1.metric("Channels", n_ch)
            m2.metric("Dead channels", n_dead)
            m3.metric("Effective rank", eff_rank)

            # Variance bar chart (sorted, log scale)
            var_sorted = df["variance"].sort_values(ascending=False).reset_index(drop=True)
            fig_var = px.bar(
                x=list(range(len(var_sorted))),
                y=var_sorted.values,
                log_y=True,
                labels={"x": "Channel (sorted)", "y": "Variance"},
                title=f"Per-channel variance — {layer_names.get(layer_idx, '')}",
            )
            fig_var.update_layout(height=250, margin=dict(l=10, r=10, t=35, b=10))
            st.plotly_chart(fig_var, use_container_width=True)

            # PCA scree plot
            if layer_idx in scree:
                ev = scree[layer_idx]
                cumulative = np.cumsum(ev)
                fig_scree = go.Figure()
                fig_scree.add_trace(go.Bar(
                    x=list(range(1, len(ev) + 1)),
                    y=ev,
                    name="Explained var",
                ))
                fig_scree.add_trace(go.Scatter(
                    x=list(range(1, len(ev) + 1)),
                    y=cumulative,
                    name="Cumulative",
                    mode="lines+markers",
                ))
                fig_scree.update_layout(
                    title=f"PCA scree — {layer_names.get(layer_idx, '')}",
                    xaxis_title="Component",
                    yaxis_title="Explained variance ratio",
                    height=250,
                    margin=dict(l=10, r=10, t=35, b=10),
                )
                st.plotly_chart(fig_scree, use_container_width=True)

            # Data table
            with st.expander("Full channel table", expanded=False):
                st.dataframe(df, use_container_width=True)


# ============================================================================
# 7. Batch Effect Diagnostics
# ============================================================================

def _render_batch_effect_diagnostics():
    """Render batch-effect diagnostics (plate vs media vs hue)."""
    import plotly.express as px
    import plotly.graph_objects as go

    from candescence.tlv.analysis.batch_effect_diagnostics import (
        cross_plate_consistency,
        variance_partitioning,
        within_plate_separation,
    )

    with st.expander("Batch Effect Diagnostics (Plate vs Media vs Hue)", expanded=False):
        metadata_df = st.session_state.metadata_df
        if metadata_df is None:
            st.info("Load data first.")
            return

        # Ensure plate/media columns exist
        if not _ensure_plate_media_columns(metadata_df):
            st.warning(
                "Could not parse **plate** and **media** from image IDs. "
                "Batch-effect diagnostics require filenames following the "
                "`P{plate}_{media}_{condition}_{replicate}` convention."
            )
            return

        all_embeddings = st.session_state.get('all_embeddings', {})
        if not all_embeddings:
            st.info("No embeddings loaded.")
            return

        # Latent space selector
        tendril_keys = st.session_state.get('tendril_keys', [])
        space_options = ['primary'] + list(tendril_keys)
        space_name = st.selectbox(
            "Latent space",
            space_options,
            format_func=lambda x: "Primary (mu)" if x == "primary" else f"Tendril: {x}",
            key="batch_diag_space",
        )
        X = all_embeddings.get(space_name)
        if X is None:
            st.warning(f"No embeddings for space '{space_name}'.")
            return

        coords_2d = st.session_state.get('all_coords_2d', {}).get(space_name)

        media_arr = metadata_df['media'].values
        plate_arr = metadata_df['plate'].values
        hue_arr = (
            metadata_df['average_hue'].values
            if 'average_hue' in metadata_df.columns else None
        )

        # ── Three tabs ──────────────────────────────────────────────
        tab_var, tab_within, tab_cross = st.tabs([
            "Variance Partitioning",
            "Within-Plate Separation",
            "Cross-Plate Consistency",
        ])

        # ── Tab 1: Variance Partitioning ────────────────────────────
        with tab_var:
            st.markdown("#### Variance Partitioning")
            st.caption(
                "Decomposes latent variance (top PCA components) into contributions "
                "from **media** (morphology), **plate** (lighting), and **hue** (color)."
            )

            n_pcs = st.slider(
                "Number of PCs", min_value=2, max_value=min(50, X.shape[1]),
                value=min(10, X.shape[1]), key="batch_var_npcs",
            )

            if st.button("Run Variance Partitioning", key="batch_var_run"):
                # Use PCA scores if available, else raw embeddings
                pca_scores = st.session_state.get('all_pca_scores', {}).get(space_name)
                X_input = pca_scores if pca_scores is not None else X

                result = variance_partitioning(
                    X_input, media_arr, plate_arr, hue=hue_arr, n_pcs=n_pcs
                )
                st.session_state['batch_var_result'] = result
                st.session_state['batch_var_space'] = space_name

            vr = st.session_state.get('batch_var_result')
            if vr is not None and st.session_state.get('batch_var_space') == space_name:
                # Metrics row
                m1, m2, m3, m4 = st.columns(4)
                with m1:
                    st.metric("R² media", f"{vr.r2_media:.4f}")
                with m2:
                    st.metric("R² plate", f"{vr.r2_plate:.4f}")
                with m3:
                    if np.isfinite(vr.r2_hue):
                        st.metric("R² hue", f"{vr.r2_hue:.4f}")
                    else:
                        st.metric("R² hue", "N/A")
                with m4:
                    st.metric("R² full model", f"{vr.r2_full:.4f}")

                # Stacked bar chart of unique contributions
                bar_labels = ["media", "plate"]
                bar_values = [vr.unique_media, vr.unique_plate]
                bar_colors = ["#2ca02c", "#d62728"]
                if np.isfinite(vr.unique_hue):
                    bar_labels.append("hue")
                    bar_values.append(vr.unique_hue)
                    bar_colors.append("#ff7f0e")
                bar_labels.append("residual")
                bar_values.append(vr.residual)
                bar_colors.append("#cccccc")

                fig = go.Figure()
                cumulative = 0.0
                for lbl, val, col in zip(bar_labels, bar_values, bar_colors):
                    fig.add_trace(go.Bar(
                        y=["R² breakdown"], x=[val],
                        name=f"{lbl} ({val:.3f})",
                        orientation='h',
                        marker_color=col,
                    ))
                    cumulative += val
                fig.update_layout(
                    barmode='stack',
                    height=120,
                    margin=dict(l=10, r=10, t=10, b=10),
                    xaxis_title="Proportion of variance (R²)",
                    xaxis=dict(range=[0, 1]),
                    legend=dict(orientation="h", yanchor="bottom", y=1.02),
                )
                st.plotly_chart(fig, use_container_width=True)

                st.caption(
                    f"**Interpretation** (N={vr.n_samples}, {vr.n_pcs} PCs): "
                    "If **media** dominates unique R², the latent space encodes morphology. "
                    "If **plate** or **hue** dominate, the representation may be "
                    "confounded by lighting/background color."
                )

        # ── Tab 2: Within-Plate Separation ──────────────────────────
        with tab_within:
            st.markdown("#### Within-Plate Media Separation")
            st.caption(
                "Within a single plate (constant lighting), do media types separate? "
                "High silhouette + ARI → morphology learned."
            )

            unique_plates = sorted(metadata_df['plate'].dropna().unique())
            if not unique_plates:
                st.warning("No plates found.")
            else:
                sel_plate = st.selectbox(
                    "Select plate", unique_plates, key="batch_wp_plate"
                )
                if st.button("Run Within-Plate Test", key="batch_wp_run"):
                    result = within_plate_separation(
                        X, media_arr, plate_arr, sel_plate, coords_2d=coords_2d
                    )
                    st.session_state['batch_wp_result'] = result
                    st.session_state['batch_wp_space'] = space_name

                wpr = st.session_state.get('batch_wp_result')
                if (
                    wpr is not None
                    and st.session_state.get('batch_wp_space') == space_name
                ):
                    mc1, mc2, mc3 = st.columns(3)
                    with mc1:
                        st.metric("Silhouette (media)", f"{wpr.silhouette:.4f}")
                    with mc2:
                        st.metric("ARI (K-Means vs media)", f"{wpr.ari:.4f}")
                    with mc3:
                        st.metric("N samples", f"{wpr.n_samples}")

                    if wpr.coords_2d is not None and len(wpr.coords_2d) > 0:
                        plot_df = pd.DataFrame({
                            'x': wpr.coords_2d[:, 0],
                            'y': wpr.coords_2d[:, 1],
                            'media': wpr.media_labels,
                        })
                        fig = px.scatter(
                            plot_df, x='x', y='y', color='media',
                            title=f"Plate {wpr.plate} — colored by media",
                        )
                        fig.update_layout(height=400, margin=dict(l=10, r=10, t=35, b=10))
                        fig.update_traces(marker=dict(size=8, opacity=0.7))
                        st.plotly_chart(fig, use_container_width=True)

                    st.caption(
                        "High separation by **media** within constant lighting confirms "
                        "the model learned morphology, not just background hue."
                    )

        # ── Tab 3: Cross-Plate Consistency ──────────────────────────
        with tab_cross:
            st.markdown("#### Cross-Plate Consistency")
            st.caption(
                "Within a single media type, do plates (different lighting) mix together? "
                "Low plate silhouette → morphology-driven (good)."
            )

            unique_media = sorted(metadata_df['media'].dropna().unique())
            if not unique_media:
                st.warning("No media types found.")
            else:
                sel_media = st.selectbox(
                    "Select media", unique_media, key="batch_cp_media"
                )
                if st.button("Run Cross-Plate Test", key="batch_cp_run"):
                    result = cross_plate_consistency(
                        X, media_arr, plate_arr, sel_media, coords_2d=coords_2d
                    )
                    st.session_state['batch_cp_result'] = result
                    st.session_state['batch_cp_space'] = space_name

                cpr = st.session_state.get('batch_cp_result')
                if (
                    cpr is not None
                    and st.session_state.get('batch_cp_space') == space_name
                ):
                    mc1, mc2 = st.columns(2)
                    with mc1:
                        st.metric("Silhouette (plate)", f"{cpr.silhouette_plate:.4f}")
                    with mc2:
                        st.metric("N samples", f"{cpr.n_samples}")

                    if cpr.coords_2d is not None and len(cpr.coords_2d) > 0:
                        plot_df = pd.DataFrame({
                            'x': cpr.coords_2d[:, 0],
                            'y': cpr.coords_2d[:, 1],
                            'plate': cpr.plate_labels,
                        })
                        fig = px.scatter(
                            plot_df, x='x', y='y', color='plate',
                            title=f"Media: {cpr.media} — colored by plate",
                        )
                        fig.update_layout(height=400, margin=dict(l=10, r=10, t=35, b=10))
                        fig.update_traces(marker=dict(size=8, opacity=0.7))
                        st.plotly_chart(fig, use_container_width=True)

                    st.caption(
                        "**Low** plate separation means different-hue plates intermix — "
                        "the model encodes morphology, not lighting. "
                        "**High** plate separation suggests hue confounding."
                    )
