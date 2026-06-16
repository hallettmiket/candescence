"""
Purpose: Reusable UI components for Candescence Streamlit apps
Author: Hallett Lab
Date: 2026-02-04

Provides shared components used across TLV and Varasana training interfaces.
"""

import subprocess
import uuid
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

import pandas as pd
import plotly.graph_objects as go
import streamlit as st
import torch

from candescence.core.model_catalog import research_mode_default
from candescence.core.settings import get_settings
from candescence.interface.core.theme import THEME, get_subproject_color


# =============================================================================
# Model-tier (public vs research) Components
# =============================================================================

# Shared across all pages of the multipage app via st.session_state.
RESEARCH_MODE_KEY = "research_mode"


def research_mode_enabled() -> bool:
    """
    Return whether research mode (show all models) is active for this session.

    Defaults from ``$CANDESCENCE_RESEARCH_MODE`` on first access; thereafter it
    reflects whatever :func:`render_research_mode_toggle` last set.
    """
    return bool(st.session_state.get(RESEARCH_MODE_KEY, research_mode_default()))


def render_research_mode_toggle(*, location: str = "sidebar") -> bool:
    """
    Render the research-mode toggle and return its current value.

    End users see only the curated public model tier (Strategy 0/1/14). Enabling
    research mode reveals every architecture. The choice is shared across pages
    via ``st.session_state[RESEARCH_MODE_KEY]``.

    Parameters
    ----------
    location : str
        ``"sidebar"`` (default) renders in ``st.sidebar``; anything else renders
        inline in the main body.
    """
    # Initialise once from the env default, then let the widget own the key
    # (avoids the Streamlit "value set via both default and session_state"
    # warning and keeps the toggle consistent across pages).
    if RESEARCH_MODE_KEY not in st.session_state:
        st.session_state[RESEARCH_MODE_KEY] = research_mode_default()

    container = st.sidebar if location == "sidebar" else st
    value = container.toggle(
        "Research mode",
        key=RESEARCH_MODE_KEY,
        help=(
            "Off: show only the curated, production-ready models "
            "(Strategy 0/1/14). On: show every experimental architecture."
        ),
    )
    return bool(value)


# =============================================================================
# Image-source picker (directory or upload)
# =============================================================================

# Image file extensions the picker accepts for upload.
IMAGE_UPLOAD_TYPES = ["bmp", "png", "jpg", "jpeg", "tif", "tiff"]


def _upload_staging_dir(key_prefix: str, *, create: bool = False) -> Path:
    """
    Return a per-session staging directory under the refined tree for uploads.

    Uploaded images must never be written to the immutable raw tree; we stage
    them under ``<refined>/_uploads/<session-token>/<key_prefix>`` instead. The
    directory is only created on disk when ``create=True`` (i.e. when there are
    files to save), so merely rendering the picker has no filesystem side
    effects.
    """
    token_key = "_upload_session_token"
    if token_key not in st.session_state:
        st.session_state[token_key] = uuid.uuid4().hex[:12]
    token = st.session_state[token_key]
    staging = get_settings().refined_path / "_uploads" / token / key_prefix
    if create:
        staging.mkdir(parents=True, exist_ok=True)
    return staging


def _project_image_dirs(project: str) -> List[Dict[str, str]]:
    """Return the registered image datasets for a project as picker options.

    Each option is ``{"name", "dir", "description"}`` where ``dir`` is an existing
    image directory (``image_dir``-format datasets, or the ``image_dir`` /
    ``train_image_dir`` recorded in a dataset's metadata). Used to surface a
    project's own datasets in its image-source picker.
    """
    try:
        from candescence.core.dataset_zoo import DatasetZoo

        entries = DatasetZoo().list_datasets(project=project)
    except Exception:  # pragma: no cover - defensive
        return []

    options: List[Dict[str, str]] = []
    for entry in entries:
        image_dir = (
            entry.path if entry.format == "image_dir"
            else entry.metadata.get("image_dir")
            or entry.metadata.get("train_image_dir")
            or ""
        )
        if image_dir and Path(image_dir).is_dir():
            options.append(
                {"name": entry.name, "dir": str(image_dir),
                 "description": entry.description}
            )
    return options


def render_image_source_picker(
    *,
    key_prefix: str,
    default_dir: str,
    label: str = "Image source",
    project: Optional[str] = None,
) -> str:
    """
    Render a "directory path" or "upload images" picker; return the chosen dir.

    Lets a new user either point at a folder of images (default, with the
    settings-resolved path pre-filled) or drag-and-drop their own images, which
    are staged to a per-session directory under the refined tree. Either way the
    return value is a directory path the rest of the pipeline can consume.

    Parameters
    ----------
    key_prefix : str
        Unique prefix for this picker's Streamlit widget keys.
    default_dir : str
        Pre-filled directory path for the "directory" mode.
    label : str
        Heading shown above the picker.
    project : str, optional
        When given, prepend a "Dataset zoo" mode listing that project's own
        registered image datasets (e.g. the TLV corpus on the TLV pages), so the
        right data surfaces for the right analysis.

    Returns
    -------
    str
        The resolved directory path (the typed path, the chosen dataset's image
        directory, or the upload staging dir).
    """
    st.markdown(f"**{label}**")
    zoo_options = _project_image_dirs(project) if project else []
    modes = (["Dataset zoo"] if zoo_options else []) + [
        "Directory path", "Upload images"]
    mode = st.radio(
        "Provide images by",
        options=modes,
        horizontal=True,
        key=f"{key_prefix}_source_mode",
        help=(
            "Point at a folder of images, or upload your own. Uploaded files are "
            "staged under the refined data tree (never the read-only raw tree)."
            + (" 'Dataset zoo' lists this project's registered datasets."
               if zoo_options else "")
        ),
    )

    if mode == "Dataset zoo":
        names = [opt["name"] for opt in zoo_options]
        idx = st.selectbox(
            f"{(project or '').upper()} dataset",
            options=list(range(len(zoo_options))),
            format_func=lambda i: names[i],
            key=f"{key_prefix}_zoo",
        )
        chosen = zoo_options[idx]
        if chosen["description"]:
            st.caption(chosen["description"])
        return chosen["dir"]

    if mode == "Directory path":
        return st.text_input(
            "Image directory",
            value=default_dir,
            key=f"{key_prefix}_dir",
            help="Absolute path to a folder of images.",
        )

    uploads = st.file_uploader(
        "Upload images",
        type=IMAGE_UPLOAD_TYPES,
        accept_multiple_files=True,
        key=f"{key_prefix}_uploader",
    )
    if uploads:
        staging = _upload_staging_dir(key_prefix, create=True)
        saved = 0
        for uploaded in uploads:
            dest = staging / uploaded.name
            if not dest.exists():
                dest.write_bytes(uploaded.getbuffer())
                saved += 1
        st.success(
            f"{saved} new file(s) staged ({len(list(staging.iterdir()))} total) "
            f"in `{staging}`."
        )
    else:
        staging = _upload_staging_dir(key_prefix, create=False)
        st.caption(f"Uploads will be staged in `{staging}`.")
    return str(staging)


# =============================================================================
# GPU Selection Components
# =============================================================================


def get_gpu_info() -> List[Dict[str, Any]]:
    """
    Get information about available GPUs including memory usage.

    Returns:
        List of dicts with gpu_id, name, total_memory_gb, used_memory_gb, free_memory_gb
    """
    gpus = []
    if not torch.cuda.is_available():
        return gpus

    for i in range(torch.cuda.device_count()):
        props = torch.cuda.get_device_properties(i)
        total_mem = props.total_memory / 1e9

        # Get current memory usage
        try:
            torch.cuda.set_device(i)
            reserved = torch.cuda.memory_reserved(i) / 1e9
            # Use nvidia-smi for more accurate usage
            result = subprocess.run(
                ['nvidia-smi', '--query-gpu=memory.used', '--format=csv,noheader,nounits', f'--id={i}'],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0:
                used_mem = float(result.stdout.strip()) / 1024  # Convert MiB to GB
            else:
                used_mem = reserved
        except Exception:
            used_mem = 0.0

        gpus.append({
            'gpu_id': i,
            'name': props.name,
            'total_memory_gb': total_mem,
            'used_memory_gb': used_mem,
            'free_memory_gb': total_mem - used_mem,
        })

    return gpus


def get_recommended_gpu() -> int:
    """
    Get the recommended GPU (the one with most free memory).

    Returns:
        GPU index, defaults to 4 if multiple GPUs available, else 0
    """
    if not torch.cuda.is_available():
        return 0

    gpu_count = torch.cuda.device_count()
    if gpu_count <= 1:
        return 0

    gpus = get_gpu_info()
    if not gpus:
        return 4 if gpu_count > 4 else 0

    # Find GPU with most free memory
    best_gpu = max(gpus, key=lambda x: x['free_memory_gb'])

    # If GPU 4 exists and has reasonable free memory (>50% of best), prefer it
    if gpu_count > 4:
        gpu4 = next((g for g in gpus if g['gpu_id'] == 4), None)
        if gpu4 and gpu4['free_memory_gb'] > best_gpu['free_memory_gb'] * 0.5:
            return 4

    return best_gpu['gpu_id']


def render_gpu_selector(key_prefix: str = "gpu") -> int:
    """
    Render a GPU selection dropdown with memory info.

    Args:
        key_prefix: Prefix for the widget key

    Returns:
        Selected GPU index
    """
    if not torch.cuda.is_available():
        st.warning("No CUDA GPUs available. Training will use CPU (much slower).")
        return -1

    gpu_count = torch.cuda.device_count()
    gpus = get_gpu_info()
    recommended = get_recommended_gpu()

    if gpu_count == 1:
        gpu = gpus[0] if gpus else {'name': 'Unknown', 'free_memory_gb': 0, 'total_memory_gb': 0}
        st.info(f"Using GPU 0: {gpu['name']} ({gpu['free_memory_gb']:.1f}/{gpu['total_memory_gb']:.1f} GB free)")
        return 0
    else:
        # Build options with memory info
        options = []
        for gpu in gpus:
            label = f"GPU {gpu['gpu_id']}: {gpu['name']} ({gpu['free_memory_gb']:.1f}/{gpu['total_memory_gb']:.1f} GB free)"
            if gpu['gpu_id'] == recommended:
                label += " (Recommended)"
            options.append(label)

        selected_idx = st.selectbox(
            "Select GPU",
            options=range(len(options)),
            format_func=lambda i: options[i],
            index=recommended,
            key=f"{key_prefix}_selector",
            help="Select which GPU to use for training"
        )

        return selected_idx


# =============================================================================
# Loss Visualization Components
# =============================================================================


def create_loss_chart(
    df: pd.DataFrame,
    columns: List[str],
    burn_in: int = 0,
    use_log_scale: bool = False,
    title: str = "",
    height: int = 300,
    color_map: Optional[Dict[str, str]] = None,
) -> go.Figure:
    """
    Create a Plotly loss chart with burn-in and log scale options.

    Args:
        df: DataFrame with loss history
        columns: List of column names to plot
        burn_in: Number of initial epochs to skip
        use_log_scale: Whether to use log scale for y-axis
        title: Optional chart title
        height: Chart height in pixels
        color_map: Optional dict mapping column names to colors

    Returns:
        Plotly Figure object
    """
    # Apply burn-in
    plot_df = df.iloc[burn_in:].copy()

    if len(plot_df) == 0:
        fig = go.Figure()
        fig.add_annotation(
            text="Waiting for data...",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False
        )
        fig.update_layout(height=height)
        return fig

    # Adjust epoch numbers for x-axis
    plot_df = plot_df.reset_index(drop=True)
    plot_df['epoch'] = plot_df.index + burn_in + 1

    # Default color palette
    default_colors = {
        'train_loss': '#1f77b4',
        'val_loss': '#ff7f0e',
        'train_kl': '#2ca02c',
        'val_kl': '#d62728',
        'train_recon': '#9467bd',
        'val_recon': '#8c564b',
        'train_cond': '#e377c2',
        'val_cond': '#7f7f7f',
        # Detection losses
        'train_cls': '#1f77b4',
        'val_cls': '#ff7f0e',
        'train_bbox': '#2ca02c',
        'val_bbox': '#d62728',
        'train_centerness': '#9467bd',
        'val_centerness': '#8c564b',
    }
    colors = color_map or default_colors

    # Create figure
    fig = go.Figure()

    for col in columns:
        if col in plot_df.columns:
            is_val = col.startswith('val_')
            fig.add_trace(go.Scatter(
                x=plot_df['epoch'],
                y=plot_df[col],
                mode='lines',
                name=col.replace('_', ' ').title(),
                line=dict(
                    color=colors.get(col, None),
                    dash='dash' if is_val else 'solid'
                )
            ))

    fig.update_layout(
        title=title if title else None,
        xaxis_title="Epoch",
        yaxis_title="Loss",
        yaxis_type="log" if use_log_scale else "linear",
        height=height,
        margin=dict(l=50, r=20, t=30 if title else 10, b=40),
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="right",
            x=1
        ),
        hovermode="x unified"
    )

    return fig


def render_loss_chart_with_controls(
    df: pd.DataFrame,
    columns: List[str],
    key_prefix: str,
    title: str = "",
    show_controls: bool = True,
    height: int = 300,
    color_map: Optional[Dict[str, str]] = None,
) -> None:
    """
    Render a loss chart with burn-in slider and log scale toggle.

    Args:
        df: DataFrame with loss history
        columns: List of column names to plot
        key_prefix: Unique prefix for widget keys
        title: Optional chart title
        show_controls: Whether to show burn-in and log scale controls
        height: Chart height in pixels
        color_map: Optional dict mapping column names to colors
    """
    burn_in = 0
    use_log_scale = False

    if show_controls and len(df) > 5:
        ctrl_col1, ctrl_col2, ctrl_col3 = st.columns([2, 1, 1])

        with ctrl_col1:
            max_burn_in = max(0, len(df) - 3)
            burn_in = st.slider(
                "Skip early epochs (burn-in)",
                min_value=0,
                max_value=max_burn_in,
                value=min(3, max_burn_in) if len(df) > 10 else 0,
                key=f"{key_prefix}_burn_in",
                help="Skip initial epochs with high loss to better see later improvements"
            )

        with ctrl_col2:
            use_log_scale = st.checkbox(
                "Log scale",
                value=False,
                key=f"{key_prefix}_log_scale",
                help="Use logarithmic y-axis scale"
            )

        with ctrl_col3:
            if len(df) > 0:
                latest_epoch = len(df)
                st.metric("Showing epochs", f"{burn_in + 1}–{latest_epoch}")

    fig = create_loss_chart(
        df=df,
        columns=columns,
        burn_in=burn_in,
        use_log_scale=use_log_scale,
        title=title,
        height=height,
        color_map=color_map,
    )

    st.plotly_chart(fig, use_container_width=True, key=f"{key_prefix}_chart")


# =============================================================================
# Model Selection Components
# =============================================================================


def render_model_selector(
    subproject: str,
    model_type: Optional[str] = None,
    key_prefix: str = "model",
) -> Optional[str]:
    """
    Render a model selection dropdown for a specific subproject.

    Args:
        subproject: "tlv" or "varasana"
        model_type: Optional filter for "research" or "production"
        key_prefix: Prefix for the widget key

    Returns:
        Selected model ID or None
    """
    from candescence.core.model_zoo import ModelZoo

    zoo = ModelZoo()
    models = zoo.list_models(project=subproject, model_type=model_type)

    if not models:
        st.info(f"No {subproject.upper()} models available" +
                (f" ({model_type})" if model_type else ""))
        return None

    # Build options
    options = ["Select a model..."] + [
        f"{m.name} (v{m.version}) - {m.model_type}"
        for m in models
    ]

    selected_idx = st.selectbox(
        f"Select {subproject.upper()} Model",
        options=range(len(options)),
        format_func=lambda i: options[i],
        key=f"{key_prefix}_selector",
    )

    if selected_idx == 0:
        return None

    return models[selected_idx - 1].id


def render_dataset_selector(
    subproject: str,
    key_prefix: str = "dataset",
) -> Optional[str]:
    """
    Render a dataset selection dropdown for a specific subproject.

    Args:
        subproject: "tlv" or "varasana"
        key_prefix: Prefix for the widget key

    Returns:
        Selected dataset ID or None
    """
    from candescence.core.dataset_zoo import DatasetZoo

    zoo = DatasetZoo()
    datasets = zoo.list_datasets(project=subproject)

    if not datasets:
        st.info(f"No {subproject.upper()} datasets available")
        return None

    options = ["Select a dataset..."] + [
        f"{d.name} ({d.num_samples:,} samples)"
        for d in datasets
    ]

    selected_idx = st.selectbox(
        f"Select {subproject.upper()} Dataset",
        options=range(len(options)),
        format_func=lambda i: options[i],
        key=f"{key_prefix}_selector",
    )

    if selected_idx == 0:
        return None

    return datasets[selected_idx - 1].id


# =============================================================================
# Progress Components
# =============================================================================


def render_progress_metrics(
    current_epoch: int,
    total_epochs: int,
    best_epoch: int,
    best_loss: float,
    elapsed_seconds: float,
) -> None:
    """
    Render standard training progress metrics in a 4-column layout.

    Args:
        current_epoch: Current epoch number
        total_epochs: Total number of epochs
        best_epoch: Epoch with best validation loss
        best_loss: Best validation loss value
        elapsed_seconds: Total elapsed time in seconds
    """
    col1, col2, col3, col4 = st.columns(4)

    with col1:
        st.metric("Epoch", f"{current_epoch}/{total_epochs}")

    with col2:
        st.metric("Best Val Loss", f"{best_loss:.4f}" if best_loss < float('inf') else "-")

    with col3:
        st.metric("Best Epoch", best_epoch if best_epoch > 0 else "-")

    with col4:
        hours, remainder = divmod(int(elapsed_seconds), 3600)
        minutes, seconds = divmod(remainder, 60)
        if hours > 0:
            elapsed_str = f"{hours}h {minutes}m"
        elif minutes > 0:
            elapsed_str = f"{minutes}m {seconds}s"
        else:
            elapsed_str = f"{seconds}s"
        st.metric("Elapsed", elapsed_str)


def render_training_controls(
    is_paused: bool,
    on_pause: Callable[[], None],
    on_resume: Callable[[], None],
    on_stop: Callable[[], None],
    on_cancel: Callable[[], None],
    show_skip: bool = False,
    on_skip: Optional[Callable[[], None]] = None,
) -> Optional[str]:
    """
    Render training control buttons.

    Args:
        is_paused: Whether training is currently paused
        on_pause: Callback for pause button
        on_resume: Callback for resume button
        on_stop: Callback for stop button
        on_cancel: Callback for cancel button
        show_skip: Whether to show skip stage button
        on_skip: Callback for skip stage button

    Returns:
        Action taken ("pause", "resume", "stop", "cancel", "skip") or None
    """
    num_cols = 4 if show_skip else 3
    cols = st.columns(num_cols)

    action = None

    with cols[0]:
        if is_paused:
            if st.button("Resume", type="primary"):
                on_resume()
                action = "resume"
        else:
            if st.button("Pause"):
                on_pause()
                action = "pause"

    with cols[1]:
        if st.button("Stop & Save", type="secondary"):
            on_stop()
            action = "stop"

    with cols[2]:
        if st.button("Cancel", type="secondary"):
            on_cancel()
            action = "cancel"

    if show_skip and on_skip:
        with cols[3]:
            if st.button("Skip Stage"):
                on_skip()
                action = "skip"

    return action
