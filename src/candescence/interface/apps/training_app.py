"""
Purpose: Streamlit Training Application for Tendril VAE
Author: Hallett Lab
Date: 2026-02-01

A standalone Streamlit app for training Tendril VAE models with a guided,
step-by-step interface accessible to non-experts.

Features:
- Step 1: Configuration with sensible defaults and explanations
- Step 2: Real-time training progress with loss plots and reconstructions
- Step 3: Model saving with Research/Production classification

Launch with (recommended - uses nice 19 for process priority):
    nice -n 19 streamlit run src/candescence/interface/apps/training_app.py

Or from Python:
    from candescence.interface.apps.training_app import main
    main()
"""

import io
import re
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import streamlit as st
import torch

from candescence.core.logging_config import get_logger
from candescence.core.model_catalog import filter_strategies
from candescence.interface.core.components import (
    IMAGE_UPLOAD_TYPES,
    render_image_source_picker,
    render_research_mode_toggle,
    research_mode_enabled,
)
from candescence.interface.training.training_state import (
    TrainingState,
    TrainingStateManager,
)
from candescence.interface.components.dataset_report import DatasetReportPanel

logger = get_logger("candescence.interface.apps.training_app")


def _coerce_strategy_int(value):
    """Safely coerce a strategy value to a numeric.

    Handles int, float (15.0 → 15, 9.6 stays 9.6), numeric strings
    ("15" → 15, "9.6" → 9.6), and falls back to 14 on garbage.
    Fractional strategy IDs (9.5, 9.6, 9.7, 9.8, 9.9) are preserved
    as floats; integer-valued strategies are returned as int.
    """
    if isinstance(value, int):
        return value
    if isinstance(value, float):
        return int(value) if value == int(value) else value
    try:
        f = float(value)
        return int(f) if f == int(f) else f
    except (TypeError, ValueError):
        pass
    logger.warning(f"Unexpected strategy value {value!r} (type {type(value).__name__}); defaulting to 14")
    return 14


# Find logo path
LOGO_PATH = Path(__file__).parent.parent.parent.parent.parent / "assets" / "candescence-logo.png"

# Page configuration must be first - only when run standalone (not when imported by multipage app)
if __name__ == "__main__":
    st.set_page_config(
        page_title="Candescence VAE Training",
        page_icon=str(LOGO_PATH) if LOGO_PATH.exists() else "🧬",
        layout="wide",
        initial_sidebar_state="expanded"
    )

# =============================================================================
# Constants and Defaults
# =============================================================================

# Default paths — canonical metadata/label/image/zoo sources resolve from the
# settings layer (env var / config file / packaged default).
from candescence.core.config import (
    DEFAULT_CLEANED_METADATA_XLSX,
    DEFAULT_MANUALLY_LABELLED_IMAGES_CSV,
)
from candescence.core.settings import get_settings

_settings = get_settings()
DEFAULT_IMAGE_DIR = f"{_settings.image_dir}/"
DEFAULT_METADATA_PATH = str(DEFAULT_CLEANED_METADATA_XLSX)
DEFAULT_OUTPUT_BASE = f"{_settings.zoo_path}/"


def _parse_image_filename(filename: str) -> Dict[str, Any]:
    """
    Parse image filename to extract day and washed status.

    Filename patterns:
    - P11_control_day2_1-r1-c10.bmp -> day=2, washed=False, media='control'
    - P11_control_day5_1-r1-c10.bmp -> day=5, washed=False, media='control'
    - P11_control_wash_1-r1-c10.bmp -> day=None, washed=True, media='control'

    Returns:
        Dict with 'day' (int or None), 'washed' (bool), 'plate' (str),
        'media' (str), 'filename' (str)
    """
    parts = filename.split('_')
    plate = parts[0] if parts else ''

    # Media is the second token; normalise the known short aliases.
    _media_map = {'spdr': 'spider', 'ctrl': 'control'}
    media_raw = parts[1].lower() if len(parts) >= 2 else 'unknown'
    media = _media_map.get(media_raw, media_raw)

    day = None
    washed = False

    for part in parts:
        if part.startswith('day'):
            try:
                day = int(part.replace('day', ''))
            except ValueError:
                pass
        if part == 'wash' or part == 'washed':
            washed = True

    return {'day': day, 'washed': washed, 'plate': plate, 'media': media, 'filename': filename}


# Canonical media names available in the corpus.
MEDIA_OPTIONS = ['all', 'rpmi', 'serum', 'spider', 'ypd', 'control']


@st.cache_data(ttl=300)
def _get_filtered_images(
    image_dir: str,
    day_filter: Optional[str],
    washed_filter: Optional[str],
    media_filter: Optional[str] = 'all',
) -> List[Dict[str, Any]]:
    """
    Get list of images matching filters.

    Args:
        image_dir: Path to image directory
        day_filter: 'day2', 'day5', 'wash', or 'all'
        washed_filter: 'yes', 'no', or 'all'
        media_filter: 'all' or one of {'rpmi','serum','spider','ypd','control'}

    Returns:
        List of dicts with parsed image info
    """
    image_path = Path(image_dir)
    if not image_path.exists():
        return []

    images = []
    for fp in image_path.glob("*.bmp"):
        info = _parse_image_filename(fp.name)

        # Apply day filter
        if day_filter == 'day2' and info['day'] != 2:
            continue
        elif day_filter == 'day5' and info['day'] != 5:
            continue
        elif day_filter == 'wash' and info['day'] is not None:
            continue  # wash images have no day
        elif day_filter == 'all' and info['day'] is None:
            continue  # "All days" means day2+day5 only, excludes wash

        # Apply washed filter (skip when day_filter is 'wash' — wash is its own category)
        if day_filter != 'wash':
            if washed_filter == 'yes' and not info['washed']:
                continue
            elif washed_filter == 'no' and info['washed']:
                continue

        # Apply media filter
        if media_filter and media_filter != 'all' and info['media'] != media_filter:
            continue

        images.append(info)

    return images

# Parameter tooltips for non-experts
TOOLTIPS = {
    "latent_dim": "Dimensionality of the latent space. Higher values capture more detail but may overfit. Typical: 64-256.",
    "vae_lr": "Learning rate for VAE parameters. Controls how quickly the model learns. Start with 1e-4.",
    "film_lr": "Learning rate for FiLM conditioning layers. Usually 5x higher than VAE LR.",
    "batch_size": "Number of images per training step. Higher is faster but needs more GPU memory.",
    "number_epochs": "Number of complete passes through the training data. More epochs = better learning.",
    "kl_weight": "Peak beta for cyclical KL annealing (max_weight in get_kl_weight). The scheduled weight ramps 0 → this value → 0 over each cycle.",
    "cycle_length": "Number of epochs per KL annealing cycle. The weight ramps up over the first half and back down over the second half. Must be >= 2.",
    "conditional_loss_weight": "Weight for conditional loss. Controls how strongly conditioning affects learning.",
    "train_num": "Number of images for training.",
    "validation_num": "Number of images for validation (tracking performance during training).",
    "test_num": "Number of images for final testing (held out until the end).",
    "restrict_to_day": "Only use images from this day. Day 2 is typical for morphology analysis.",
    "report_periodicity": "Show reconstruction images every N epochs.",
}


def main():
    """Main application entry point."""
    # Display logo and title
    col_logo, col_title = st.columns([1, 4])
    with col_logo:
        if LOGO_PATH.exists():
            st.image(str(LOGO_PATH), width=200)
    with col_title:
        st.title("Tendril VAE Training")
        st.caption("Guided training for Candida morphology analysis")

    # Initialize session state
    TrainingStateManager.initialize_session_state()
    _init_app_session_state()

    # Get current status
    status = TrainingStateManager.get_state("status")

    # Render sidebar with step indicator and controls
    with st.sidebar:
        _render_sidebar(status)

    # Render main content based on status
    if status == "idle":
        _render_step_1_config()
    elif status in ("training", "paused"):
        _render_step_2_training()
    elif status in ("completed", "stopped"):
        _render_step_3_summary()
    elif status == "error":
        _render_error_state()
    elif status == "cancelled":
        # Reset state and go back to config
        TrainingStateManager.reset_session_state()
        st.rerun()
    else:
        _render_step_1_config()


def _init_app_session_state():
    """Initialize app-specific session state (beyond training state)."""
    app_defaults = {
        "config_validated": False,
        "show_advanced": False,
        "selected_gpu": _get_recommended_gpu(),
    }
    for key, default in app_defaults.items():
        if key not in st.session_state:
            st.session_state[key] = default


def _get_gpu_info() -> list:
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
            # This requires the GPU to be initialized
            torch.cuda.set_device(i)
            allocated = torch.cuda.memory_allocated(i) / 1e9
            reserved = torch.cuda.memory_reserved(i) / 1e9
            # Use nvidia-smi for more accurate usage
            import subprocess
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


def _get_recommended_gpu() -> int:
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

    # Default recommendation is GPU 4 (as per user preference)
    # but we'll check if it exists and has reasonable free memory
    gpus = _get_gpu_info()
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


def _render_sidebar(status: str):
    """Render sidebar with step indicator and controls."""
    st.header("Training Steps")

    # Step indicator
    steps = [
        ("1", "Configure", ["idle", "configuring"]),
        ("2", "Train", ["training", "paused"]),
        ("3", "Save", ["completed", "stopped"]),
    ]

    for step_num, step_name, active_statuses in steps:
        is_active = status in active_statuses
        is_completed = (
            (step_num == "1" and status not in ["idle", "configuring"]) or
            (step_num == "2" and status in ["completed", "stopped"])
        )

        if is_active:
            st.markdown(f"**→ Step {step_num}: {step_name}**")
        elif is_completed:
            st.markdown(f"✓ Step {step_num}: {step_name}")
        else:
            st.markdown(f"○ Step {step_num}: {step_name}")

    st.divider()

    # GPU selection
    st.subheader("GPU Selection")
    if torch.cuda.is_available():
        gpu_count = torch.cuda.device_count()
        gpus = _get_gpu_info()

        if gpu_count > 1:
            # Multiple GPUs - show selection dropdown
            gpu_options = []
            for gpu in gpus:
                usage_pct = (gpu['used_memory_gb'] / gpu['total_memory_gb']) * 100 if gpu['total_memory_gb'] > 0 else 0
                label = f"GPU {gpu['gpu_id']}: {gpu['name'][:20]} ({gpu['free_memory_gb']:.1f}GB free, {usage_pct:.0f}% used)"
                gpu_options.append((gpu['gpu_id'], label))

            # Find recommended GPU
            recommended = st.session_state.get("selected_gpu", _get_recommended_gpu())

            selected_idx = st.selectbox(
                "Select GPU",
                options=range(len(gpu_options)),
                index=recommended if recommended < len(gpu_options) else 0,
                format_func=lambda i: gpu_options[i][1],
                help="GPU 4 is recommended as it typically has lower usage. Choose one with more free memory.",
                key="cfg_gpu_select"
            )

            selected_gpu = gpu_options[selected_idx][0]
            st.session_state.selected_gpu = selected_gpu

            # Show selected GPU info
            gpu_info = gpus[selected_gpu]
            col1, col2 = st.columns(2)
            with col1:
                st.metric("Total Memory", f"{gpu_info['total_memory_gb']:.1f} GB")
            with col2:
                st.metric("Free Memory", f"{gpu_info['free_memory_gb']:.1f} GB")

            if selected_gpu == 4:
                st.success("GPU 4 selected (recommended)")
        else:
            # Single GPU
            gpu_info = gpus[0] if gpus else None
            if gpu_info:
                st.success(f"GPU: {gpu_info['name']}")
                st.metric("Memory", f"{gpu_info['total_memory_gb']:.1f} GB")
            else:
                st.success(f"GPU: {torch.cuda.get_device_name(0)}")
            st.session_state.selected_gpu = 0
    else:
        st.warning("No GPU detected - training will be slow")
        st.session_state.selected_gpu = None

    st.divider()

    # Model info (reflects selected strategy)
    st.subheader("Model")
    _strategy = _coerce_strategy_int(st.session_state.get("cfg_strategy", 14))
    _strategy_info = {
        15: "**Tendril VAE + per-variable FiLM (Strategy #15)**\n\nSame outer VAE as Strategy 14 but with separate FiLM modules for hue, day, and media at each block. Day and media one-hot from filename; targets batch-effect disentangling.",
        14: "**Tendril VAE (Strategy #14)**\n\nFiLM-conditioned VAE with skip connections for background-normalized morphology learning.",
        9.6: "**Cond U-Net VAE + Channel Attention (Strategy #9.6)**\n\nFiLM-conditioned VAE with squeeze-excitation gates on every skip connection. Used for the manuscript's adjustment-section nuisance-ablation experiments. Typically paired with HSV image adjustment.",
        7: "**Conditional U-Net VAE (Strategy #7)**\n\nFiLM-conditioned VAE with skip connections.",
        8: "**Cond U-Net VAE + Attention (Strategy #8)**\n\nFiLM-conditioned VAE with SE attention on skips.",
        13: "**CRUtch VAE (Strategy #13)**\n\nHierarchical latent VAE with reconstructed skip connections.",
        1: "**U-Net VAE (Strategy #1)**\n\nUnconditional VAE with skip connections.",
        11: "**ResNet VAE (Strategy #11)**\n\nResNet-152 backbone, unconditional.",
        12: "**Multi-stage U-Net VAE (Strategy #12)**\n\nProgressive skip removal, unconditional.",
        0: "**Convolutional VAE (Strategy #0)**\n\nSimplest unconditional VAE baseline.",
    }
    st.info(_strategy_info.get(_strategy, _strategy_info[14]))

    _loss_fn_label = {"mse": "MSE", "lpips": "LPIPS", "ssim": "SSIM"}
    _loss_fn = st.session_state.get("cfg_loss_fn", "mse")
    st.caption(f"Loss: **{_loss_fn_label.get(_loss_fn, _loss_fn)}**")

    if st.session_state.get("cfg_grayscale", False):
        st.caption("Input: **Grayscale**")

    st.divider()

    # Reset button
    if status != "idle":
        if st.button("Start New Training", type="secondary"):
            _cleanup_training_resources()
            TrainingStateManager.reset_session_state()
            st.session_state.config_validated = False
            st.rerun()


def _render_step_1_config():
    """Render Step 1: Configuration interface."""
    st.header("Step 1: Configure Training")

    # Data Configuration
    with st.expander("📁 Data Configuration", expanded=True):
        _render_data_config()

    # Dataset Split
    with st.expander("📊 Dataset Split", expanded=True):
        _render_split_config()

    # Model Architecture — auto-expand for strategies with decoder augmentation options
    _arch_expanded = _coerce_strategy_int(
        st.session_state.get("cfg_strategy", 14)
    ) in (9.6, 9.7, 13, 14, 15)
    with st.expander("🧬 Model Architecture", expanded=_arch_expanded):
        _render_model_config()

    # Training Hyperparameters
    with st.expander("⚙️ Training Parameters", expanded=True):
        _render_training_config()

    # Output Configuration
    with st.expander("💾 Output Configuration", expanded=True):
        _render_output_config()

    st.divider()

    # Validation and Start
    col1, col2, col3 = st.columns([1, 2, 1])
    with col2:
        if st.button("Validate & Start Training", type="primary", use_container_width=True):
            _validate_and_start()


def _render_data_config():
    """Render data configuration section."""
    col1, col2 = st.columns(2)

    with col1:
        # Directory-or-upload picker. New users can point at a folder or upload
        # their own images (staged under the refined tree). The chosen path is
        # mirrored into cfg_image_dir for the rest of the wizard.
        image_dir = render_image_source_picker(
            key_prefix="train_img",
            default_dir=DEFAULT_IMAGE_DIR,
            label="Training images",
            project="tlv",
        )
        st.session_state["cfg_image_dir"] = image_dir

        # Check image count (any supported extension, not just .bmp)
        image_path = Path(image_dir)
        if image_path.exists():
            total_count = sum(
                len(list(image_path.glob(f"*.{ext}"))) for ext in IMAGE_UPLOAD_TYPES
            )
            if total_count > 0:
                st.info(f"Directory contains {total_count:,} total images")
            else:
                st.warning("No supported images found in directory")
        else:
            st.error("Directory does not exist")

    with col2:
        image_dim = st.selectbox(
            "Image Dimension",
            options=[64, 128, 256],
            index=1,
            help="Size of input images (width = height).",
            key="cfg_image_dim"
        )

    st.divider()
    st.markdown("**Image Filters**")
    st.caption("Filter images by experimental conditions. The available images counter updates as you change filters.")

    col3, col4, col_media = st.columns(3)

    with col3:
        day_filter = st.selectbox(
            "Day Filter",
            options=['all', 'day2', 'day5', 'wash'],
            index=1,  # Default to day2
            format_func=lambda x: {
                'all': 'All days (day2 + day5)',
                'day2': 'Day 2 only',
                'day5': 'Day 5 only',
                'wash': 'Wash images only',
            }.get(x, x),
            help="Day 2 is typical for morphology analysis. Wash images are a separate category (post-wash, no day label).",
            key="cfg_day_filter"
        )

    with col4:
        if day_filter == 'wash':
            # Wash images are their own category — washed filter doesn't apply
            st.selectbox(
                "Washed Filter",
                options=['all'],
                index=0,
                format_func=lambda x: 'N/A (wash images)',
                disabled=True,
                help="Wash images are a separate category. The washed filter does not apply.",
                key="cfg_washed_filter_disabled"
            )
            # Keep a consistent value in session state
            washed_filter = 'all'
            st.session_state["cfg_washed_filter"] = 'all'
        else:
            washed_filter = st.selectbox(
                "Washed Filter",
                options=['all', 'no', 'yes'],
                index=1,  # Default to not washed
                format_func=lambda x: {
                    'all': 'All (washed + not washed)',
                    'no': 'Not washed only',
                    'yes': 'Washed only',
                }.get(x, x),
                help="Washed images have had media removed. Not washed is typical for morphology analysis.",
                key="cfg_washed_filter"
            )

    with col_media:
        media_filter = st.selectbox(
            "Media Filter",
            options=MEDIA_OPTIONS,
            index=0,
            format_func=lambda x: 'All media' if x == 'all' else x.upper() if x == 'ypd' else x.capitalize(),
            help=(
                "Restrict training to images grown in a single growth medium. "
                "Useful for medium-specific models (medium explains ~85% of edge-band hue variance)."
            ),
            key="cfg_media_filter",
        )

    # Show effective filter summary
    _day_labels = {'all': 'day2 + day5', 'day2': 'day 2', 'day5': 'day 5', 'wash': 'wash'}
    _wash_labels = {'all': 'washed + not washed', 'no': 'not washed', 'yes': 'washed'}
    _media_label = 'all media' if media_filter == 'all' else f"media = {media_filter}"
    if day_filter == 'wash':
        st.caption(f"Effective filter: **wash images only**, **{_media_label}**")
    else:
        st.caption(
            f"Effective filter: **{_day_labels[day_filter]}**, "
            f"**{_wash_labels[washed_filter]}**, **{_media_label}**"
        )

    st.divider()
    st.markdown("**Image Preprocessing**")
    grayscale = st.checkbox(
        "Convert to grayscale",
        value=False,
        help=(
            "Strip color from encoder input (R=G=B). FiLM HSV conditioning still uses "
            "**background statistics from the original color image** on disk. Conditional "
            "loss uses value-only terms for grayscale (hue/sat terms are skipped)."
        ),
        key="cfg_grayscale"
    )

    if grayscale:
        gs_bg_norm = st.checkbox(
            "Normalize background luminance",
            value=False,
            help=(
                "Shift greyscale images so the edge-band mean lands on a fixed target "
                "(default 0.5). Reduces plate-to-plate brightness shortcuts. "
                "HSV conditioning still uses the original color image."
            ),
            key="cfg_grayscale_bg_normalize",
        )
        if gs_bg_norm:
            gs_c1, gs_c2 = st.columns(2)
            with gs_c1:
                st.number_input(
                    "Background target",
                    min_value=0.0, max_value=1.0, value=0.5, step=0.05,
                    help="Target luminance for edge-band mean (0-1 scale).",
                    key="cfg_grayscale_bg_target",
                )
            with gs_c2:
                st.number_input(
                    "Border width (px)",
                    min_value=1, max_value=32, value=12, step=1,
                    help="Edge band width in pixels for background estimation.",
                    key="cfg_grayscale_bg_border",
                )

    merge_manual = st.checkbox(
        "Merge manual labels",
        value=True,
        help=(
            "Merge manually_labelled_images.csv into dataset metadata, adding a "
            "'manual_formation' column. The CSV must have 'file_name' and 'morphology' columns."
        ),
        key="cfg_merge_manual_labels"
    )

    # Get filtered image count with caching
    filtered_images = _get_filtered_images(
        st.session_state.get("cfg_image_dir", DEFAULT_IMAGE_DIR),
        st.session_state.get("cfg_day_filter", "day2"),
        st.session_state.get("cfg_washed_filter", "no"),
        st.session_state.get("cfg_media_filter", "all"),
    )

    # Store filtered count in session state for use in split config
    st.session_state["filtered_image_count"] = len(filtered_images)
    st.session_state["filtered_images"] = filtered_images

    # Display filtered count prominently
    if len(filtered_images) > 0:
        st.success(f"**{len(filtered_images):,} images available** after applying filters")
    else:
        st.error("No images match the current filter criteria")


def _render_split_config():
    """Render dataset split configuration."""
    # Get available image count from data config
    available_count = st.session_state.get("filtered_image_count", 0)

    st.markdown("Configure how images are split between training, validation, and test sets.")
    if available_count > 0:
        st.caption(f"Available images after filtering: **{available_count:,}**")

    col1, col2, col3 = st.columns(3)

    with col1:
        train_num = st.number_input(
            "Training Images",
            min_value=100,
            max_value=min(10000, max(100, available_count - 100)),
            value=min(1200, max(100, available_count - 800)),
            step=100,
            help=TOOLTIPS["train_num"],
            key="cfg_train_num"
        )

    with col2:
        val_num = st.number_input(
            "Validation Images",
            min_value=50,
            max_value=min(2000, max(50, available_count - train_num - 50)),
            value=min(400, max(50, (available_count - train_num) // 2)),
            step=50,
            help=TOOLTIPS["validation_num"],
            key="cfg_val_num"
        )

    with col3:
        test_num = st.number_input(
            "Test Images",
            min_value=50,
            max_value=min(2000, max(50, available_count - train_num - val_num)),
            value=min(400, max(50, available_count - train_num - val_num)),
            step=50,
            help=TOOLTIPS["test_num"],
            key="cfg_test_num"
        )

    total = train_num + val_num + test_num

    # Validation messages
    if available_count > 0:
        if total > available_count:
            st.error(f"⚠️ Requested {total:,} images but only {available_count:,} available. Reduce split sizes or change filters.")
        elif total == available_count:
            st.success(f"✓ Using all {total:,} available images")
        else:
            remaining = available_count - total
            st.info(f"Total: {total:,} images ({remaining:,} images will not be used)")
    else:
        st.warning("No images available. Check the Data Configuration filters above.")

    # Batch size warning
    batch_size = st.session_state.get("cfg_batch_size", 256)
    if val_num < batch_size:
        st.warning(
            f"⚠️ Validation set ({val_num}) is smaller than batch size ({batch_size}). "
            f"This is OK - PyTorch will use the incomplete batch. For cleaner metrics, "
            f"consider making validation size a multiple of batch size."
        )
    if train_num < batch_size:
        st.warning(
            f"⚠️ Training set ({train_num}) is smaller than batch size ({batch_size}). "
            f"Consider reducing batch size."
        )

    # Preview dataset button
    st.divider()
    if st.button("📊 Preview Dataset Report", help="Compute HSV distributions from filtered images"):
        # Clear any existing report first to avoid duplicate key errors
        st.session_state["dataset_report_loaded"] = False
        _render_dataset_report(train_num, val_num, test_num)
    elif st.session_state.get("dataset_report_loaded", False):
        # Only show cached report if we didn't just click the button
        _show_cached_dataset_report()


def _render_dataset_report(train_num: int, val_num: int, test_num: int):
    """Compute HSV distributions from filtered images."""
    image_dir = st.session_state.get("cfg_image_dir", DEFAULT_IMAGE_DIR)
    cond_vars = st.session_state.get(
        "cfg_cond_vars",
        ["average_hue", "average_saturation", "average_value"],
    )
    filtered_images = st.session_state.get("filtered_images", [])

    if not filtered_images:
        st.error("No images available. Check the Data Configuration filters.")
        return

    total_needed = train_num + val_num + test_num
    if len(filtered_images) < total_needed:
        st.error(f"Not enough images: found {len(filtered_images)}, need {total_needed}")
        return

    with st.spinner("Computing HSV values from images (sampling for preview)..."):
        try:
            from candescence.interface.components.dataset_report import compute_hsv_from_images

            # Create random split indices (using same seed as training for consistency)
            np.random.seed(42)
            indices = np.random.permutation(len(filtered_images))

            train_indices = indices[:train_num].tolist()
            val_indices = indices[train_num:train_num + val_num].tolist()
            test_indices = indices[train_num + val_num:train_num + val_num + test_num].tolist()

            # Sample images for HSV computation (computing all is slow)
            # Use up to 500 from each split for representative distributions
            sample_size_train = min(500, len(train_indices))
            sample_size_val = min(200, len(val_indices))
            sample_size_test = min(200, len(test_indices))

            train_sample_indices = train_indices[:sample_size_train]
            val_sample_indices = val_indices[:sample_size_val]
            test_sample_indices = test_indices[:sample_size_test]

            # Get filenames for samples
            train_files = [filtered_images[i]['filename'] for i in train_sample_indices]
            val_files = [filtered_images[i]['filename'] for i in val_sample_indices]
            test_files = [filtered_images[i]['filename'] for i in test_sample_indices]

            # Compute HSV for each split
            train_hsv_df = compute_hsv_from_images(Path(image_dir), train_files)
            val_hsv_df = compute_hsv_from_images(Path(image_dir), val_files)
            test_hsv_df = compute_hsv_from_images(Path(image_dir), test_files)

            st.info(
                f"Computed HSV for {len(train_hsv_df)} train, "
                f"{len(val_hsv_df)} val, {len(test_hsv_df)} test sample images"
            )

            # Build a combined metadata DataFrame with HSV values
            # Each row is an image with its HSV values
            all_hsv = []
            for df, split_name in [(train_hsv_df, 'train'), (val_hsv_df, 'val'), (test_hsv_df, 'test')]:
                df = df.copy()
                df['split'] = split_name
                all_hsv.append(df)

            if all_hsv:
                metadata_df = pd.concat(all_hsv, ignore_index=True)
            else:
                st.error("Failed to compute HSV values from images")
                return

            # Merge manual labels if enabled
            if st.session_state.get("cfg_merge_manual_labels", True):
                _labels_path = DEFAULT_MANUALLY_LABELLED_IMAGES_CSV
                if _labels_path.exists():
                    _labels_df = pd.read_csv(_labels_path)
                    if {'file_name', 'morphology'}.issubset(_labels_df.columns):
                        _labels_df = _labels_df.rename(columns={'file_name': 'filename'})
                        _labels_df['filename'] = _labels_df['filename'].str.strip()
                        metadata_df = metadata_df.merge(
                            _labels_df[['filename', 'morphology']],
                            on='filename', how='left'
                        )
                        metadata_df = metadata_df.rename(columns={'morphology': 'manual_formation'})
                        metadata_df['manual_formation'] = metadata_df['manual_formation'].fillna('unlabelled')
                        n_labelled = (metadata_df['manual_formation'] != 'unlabelled').sum()
                        st.info(
                            f"Manual labels: {n_labelled}/{len(metadata_df)} images labelled "
                            f"({n_labelled/len(metadata_df)*100:.1f}% coverage)"
                        )
                    else:
                        st.warning(
                            f"Manual labels CSV missing required columns "
                            f"(file_name, morphology): {list(_labels_df.columns)}"
                        )
                else:
                    st.warning(f"Manual labels file not found: {_labels_path}")

            # Create indices for each split within the combined DataFrame
            train_df_indices = list(range(len(train_hsv_df)))
            val_df_indices = list(range(len(train_hsv_df), len(train_hsv_df) + len(val_hsv_df)))
            test_df_indices = list(range(len(train_hsv_df) + len(val_hsv_df), len(metadata_df)))

            # Store in session state for re-display
            st.session_state["dataset_report_loaded"] = True
            st.session_state["dataset_report_metadata"] = metadata_df
            st.session_state["dataset_report_train_indices"] = train_df_indices
            st.session_state["dataset_report_val_indices"] = val_df_indices
            st.session_state["dataset_report_test_indices"] = test_df_indices
            st.session_state["dataset_report_cond_vars"] = cond_vars
            # Store actual split sizes for display
            st.session_state["dataset_report_split_sizes"] = {
                'train': train_num,
                'val': val_num,
                'test': test_num,
                'train_sampled': len(train_hsv_df),
                'val_sampled': len(val_hsv_df),
                'test_sampled': len(test_hsv_df),
            }

            # Render the report
            report_panel = DatasetReportPanel()
            report_panel.render(
                metadata_df=metadata_df,
                train_indices=train_df_indices,
                val_indices=val_df_indices,
                test_indices=test_df_indices,
                conditional_variables=cond_vars,
            )

        except Exception as e:
            logger.exception("Failed to compute dataset report")
            st.error(f"Error computing HSV values: {e}")


def _show_cached_dataset_report():
    """Show the cached dataset report if available."""
    if not st.session_state.get("dataset_report_loaded", False):
        return

    metadata_df = st.session_state.get("dataset_report_metadata")
    train_indices = st.session_state.get("dataset_report_train_indices")
    val_indices = st.session_state.get("dataset_report_val_indices")
    test_indices = st.session_state.get("dataset_report_test_indices")
    cond_vars = st.session_state.get("dataset_report_cond_vars")
    split_sizes = st.session_state.get("dataset_report_split_sizes", {})

    if metadata_df is not None and train_indices is not None:
        # Show note about sampling
        if split_sizes:
            st.caption(
                f"HSV distributions computed from sampled images: "
                f"{split_sizes.get('train_sampled', 0)} of {split_sizes.get('train', 0)} train, "
                f"{split_sizes.get('val_sampled', 0)} of {split_sizes.get('val', 0)} val, "
                f"{split_sizes.get('test_sampled', 0)} of {split_sizes.get('test', 0)} test"
            )

        report_panel = DatasetReportPanel()
        report_panel.render(
            metadata_df=metadata_df,
            train_indices=train_indices,
            val_indices=val_indices,
            test_indices=test_indices,
            conditional_variables=cond_vars,
        )


def _render_model_config():
    """Render model architecture configuration."""
    STRATEGY_OPTIONS = {
        16: "Strategy 16: Tendril VAE + per-variable FiLM (hue, media) — no day",
        15: "Strategy 15: Tendril VAE + per-variable FiLM (hue, day, media)",
        14: "Strategy 14: Tendril VAE (FiLM-conditioned, recommended)",
        9.6: "Strategy 9.6: Cond U-Net VAE + Channel Attention (adjustment-section model)",
        7: "Strategy 7: Conditional U-Net VAE (FiLM)",
        8: "Strategy 8: Conditional U-Net VAE + Attention",
        13: "Strategy 13: CRUtch VAE (hierarchical latent)",
        1: "Strategy 1: U-Net VAE (unconditional)",
        11: "Strategy 11: ResNet VAE (unconditional)",
        12: "Strategy 12: U-Net VAE Multi-stage (unconditional)",
        0: "Strategy 0: Convolutional VAE (unconditional, simplest)",
    }

    STRATEGY_DESCRIPTIONS = {
        16: "Same outer Tendril VAE as Strategy 15 but drops the day FiLM pathway. "
            "Motivated by variance analysis (scripts/hue_day_medium_plate_variance.py) "
            "showing day contributes <0.25% of hue variance; a dedicated day FiLM may "
            "add parameters without signal. Hue + media FiLM are still stacked per block.",
        15: "Same outer Tendril VAE as Strategy 14, but stacks separate FiLM modules per "
            "conditioning variable at each encoder/decoder block: one FiLM for hue (HSV "
            "sub-vector), one for day, one for media. Day and media are one-hot encoded "
            "from filename metadata and always enabled. Intended to disentangle batch "
            "effects (different days and growth media cause hue shifts that aren't "
            "morphology) from the morphology signal.",
        14: "Tendril VAE uses FiLM conditioning to learn morphology independent of background color. "
            "Requires conditioning variables (e.g. average_hue). Recommended for most use cases.",
        9.6: "Conditional U-Net VAE with channel-attention (squeeze-excitation) gates on every "
            "skip connection. Pairs FiLM conditioning with attention throttling on the encoder→decoder "
            "skips, which prevents posterior collapse and is the model used for the manuscript's "
            "adjustment-section nuisance-ablation experiments. Requires HSV conditioning variables "
            "and is typically run with image adjustment turned on (Methods §methods:adjustment).",
        7: "Conditional U-Net VAE with FiLM layers in both encoder and decoder. "
            "The original FiLM-conditioned architecture.",
        8: "Conditional U-Net VAE with Squeeze-and-Excitation attention on skip connections. "
            "Adds channel attention gating to selectively weight encoder features.",
        13: "Conditional Redirected Hierarchical U-Net VAE. Uses per-level latent variables "
            "and reconstructed skip connections. Experimental architecture.",
        1: "U-Net VAE with skip connections, no conditioning. Simpler architecture for "
            "datasets where background normalization is not needed.",
        11: "ResNet-152 based VAE. Uses pretrained backbone for encoding and mirrored "
            "transposed-convolution decoder. Unconditional, no skip connections.",
        12: "Multi-stage U-Net VAE where skip connections are progressively removed "
            "across training stages. Unconditional architecture.",
        0: "Basic convolutional VAE. Simplest architecture with no skip connections or conditioning. "
            "Good baseline for comparison.",
    }

    # Curate the strategy list: end users see only the public tier (0/1/14);
    # research mode reveals every architecture. Toggle is shared across pages.
    render_research_mode_toggle()
    all_strategies = [14, 15, 16, 9.6, 7, 8, 13, 1, 11, 12, 0]
    strategy_options = filter_strategies(
        all_strategies, research_mode=research_mode_enabled()
    )

    strategy = st.selectbox(
        "Training Strategy",
        options=strategy_options,
        index=0,
        format_func=lambda x: STRATEGY_OPTIONS[x],
        help=(
            "Strategy 14 (Tendril VAE) is recommended. Enable Research mode in "
            "the sidebar to access additional experimental architectures."
        ),
        key="cfg_strategy"
    )

    st.caption(STRATEGY_DESCRIPTIONS[strategy])

    is_conditional = (strategy in (7, 8, 9.6, 13, 14, 15, 16))
    if strategy in (0, 1, 11, 12):
        st.warning(
            "Unconditional strategies ignore **Conditioning variables** and FiLM. "
            "If you previously selected hue/sat/value for another strategy, those choices are not used."
        )

    col1, col2 = st.columns(2)

    with col1:
        st.selectbox(
            "Latent Dimension",
            options=[64, 128, 256, 512],
            index=1,  # Default 128
            help=TOOLTIPS["latent_dim"],
            key="cfg_latent_dim"
        )

    with col2:
        if is_conditional:
            st.multiselect(
                "Conditioning Variables",
                options=["average_hue", "average_saturation", "average_value"],
                default=["average_hue", "average_saturation", "average_value"],
                help=(
                    "HSV channels used for FiLM conditioning. All three are "
                    "selected by default — they're orthogonal axes of color "
                    "and a single FiLM with concatenated (h, s, v) input is "
                    "the right inductive bias. Deselect channels only if you "
                    "have a specific reason."
                ),
                key="cfg_cond_vars"
            )
            if strategy == 15:
                st.caption(
                    "Strategy 15 also conditions on **day** and **media** via "
                    "separate FiLM modules (one-hot encoded from filename). "
                    "These are always enabled and do not need to be selected here."
                )
            elif strategy == 16:
                st.caption(
                    "Strategy 16 also conditions on **media** and "
                    "**plate_phys** (plate:media:day) via separate FiLM "
                    "modules — these absorb the dominant medium effect "
                    "(~86% of hue variance), the plate batch effect (~9%), "
                    "and the small media×day interaction (~0.2%) on hue. "
                    "Always enabled — does not need to be selected here."
                )
        else:
            # No conditioning for strategies 0/1 — store None
            st.session_state["cfg_cond_vars"] = None
            st.info("No conditioning variables (unconditional model)")

    # Fixed decoder conditioning — only for strategy 14
    if is_conditional:
        cond_vars = st.session_state.get(
            "cfg_cond_vars",
            ["average_hue", "average_saturation", "average_value"],
        )

        st.divider()
        st.markdown("**Image Adjustment (deterministic HSV shift, pre-training)**")
        st.caption(
            "When enabled, every image's background HSV is shifted toward fixed targets "
            "before the model sees it (FullDataset._adjust_hsv). Used for the manuscript's "
            "adjustment-section experiments with Strategy 9.6. Targets default to the "
            "dataset means (H=107.15, S=49.07, V=93.38, all out of 255)."
        )
        _adjust_default = (strategy == 9.6)
        adjust_on = st.checkbox(
            "Apply HSV adjustment to all images",
            value=st.session_state.get("cfg_adjust_images", _adjust_default),
            help="Shifts each image's per-pixel HSV channels by (target − measured background mean), uniform across the image, with [0,1] clamping.",
            key="cfg_adjust_images",
        )
        if adjust_on:
            ac1, ac2, ac3 = st.columns(3)
            with ac1:
                st.number_input(
                    "Target Hue (0–1)",
                    min_value=0.0, max_value=1.0,
                    value=float(st.session_state.get("cfg_target_hue", 107.15 / 255.0)),
                    step=0.01,
                    help="Background hue target. 107.15/255 = real-cohort dataset mean.",
                    key="cfg_target_hue",
                )
            with ac2:
                st.number_input(
                    "Target Saturation (0–1)",
                    min_value=0.0, max_value=1.0,
                    value=float(st.session_state.get("cfg_target_saturation", 49.07 / 255.0)),
                    step=0.01,
                    help="Background saturation target. 49.07/255 = real-cohort dataset mean.",
                    key="cfg_target_saturation",
                )
            with ac3:
                st.number_input(
                    "Target Value (0–1)",
                    min_value=0.0, max_value=1.0,
                    value=float(st.session_state.get("cfg_target_value", 93.38 / 255.0)),
                    step=0.01,
                    help="Background value (brightness) target. 93.38/255 = real-cohort dataset mean.",
                    key="cfg_target_value",
                )

        if strategy in (9.6, 9.7):
            st.divider()
            st.markdown("**Attention gates (Strategy 9.6 / 9.7)**")
            st.number_input(
                "Reduction ratio",
                min_value=1, max_value=64,
                value=int(st.session_state.get("cfg_reduction_ratio", 16)),
                step=1,
                help="Bottleneck width inside each squeeze-excitation gate: feature_dim // reduction_ratio. Master used 16.",
                key="cfg_reduction_ratio",
            )

        st.divider()
        st.markdown("**Image Adjustment (deterministic HSV normalisation)**")
        st.caption(
            "Optionally recolour every image's background toward fixed "
            "Hue/Saturation/Value targets before training (adds 'adjusted' "
            "copies). Removes colour nuisance variation. Independent of FiLM "
            "conditioning and augmentation."
        )
        adjust_images = st.checkbox(
            "Adjust images to target HSV",
            value=bool(st.session_state.get("cfg_adjust_images", False)),
            help="Applies dataset._adjust_hsv: each image is shifted toward the "
            "targets below. Only effective for conditional strategies.",
            key="cfg_adjust_images",
        )
        if adjust_images:
            acol1, acol2, acol3 = st.columns(3)
            with acol1:
                st.slider(
                    "Target Hue",
                    min_value=0.0, max_value=1.0,
                    value=float(st.session_state.get("cfg_target_hue", 0.5)),
                    step=0.05,
                    help="0=red, 0.33=green, 0.66=blue. Typical agar: 0.1-0.3",
                    key="cfg_target_hue",
                )
            with acol2:
                st.slider(
                    "Target Saturation",
                    min_value=0.0, max_value=1.0,
                    value=float(st.session_state.get("cfg_target_saturation", 0.5)),
                    step=0.05,
                    help="0=gray, 1=fully saturated",
                    key="cfg_target_saturation",
                )
            with acol3:
                st.slider(
                    "Target Value",
                    min_value=0.0, max_value=1.0,
                    value=float(st.session_state.get("cfg_target_value", 0.5)),
                    step=0.05,
                    help="0=black, 1=bright",
                    key="cfg_target_value",
                )

        st.divider()
        st.markdown("**Decoder Background Augmentation (training)**")
        st.caption(
            "When enabled, the decoder is conditioned on augmented background HSV values. "
            "When disabled, the decoder uses the same conditioning as the encoder."
        )
        st.checkbox(
            "Augment decoder backgrounds (Strategies 9.6, 9.7, 13, 14, 15 & 16)",
            value=(strategy == 9.6),
            disabled=(strategy not in (9.6, 9.7, 13, 14, 15, 16)),
            help="Decoder uses perturbed HSV conditioning vs encoder when enabled; requires partner rows if validation is used. Strategy 9.6 needs this on to reproduce the master adjustment-section run.",
            key="cfg_augment_decoder_images",
        )

        _dec_strats = (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16)
        if strategy in _dec_strats:
            st.checkbox(
                "Create HSV-augmented partner rows (decoder pairing)",
                value=False,
                help=(
                    "Adds extra rows to the dataset (image_type='augmented', parent_id "
                    "→ real id) using HSV jitter on the same channels as 'Conditioning "
                    "Variables'. Required for asymmetric decoder training when decoder "
                    "conditioning is enabled. Train/val/test splits still use only **real** "
                    "colonies; partners are looked up from the full table."
                ),
                key="cfg_augment_images",
            )
            st.number_input(
                "HSV jitter std (augmentation spread)",
                min_value=0.01,
                max_value=0.5,
                value=float(st.session_state.get("cfg_augmentation_spread", 0.1)),
                step=0.01,
                help="Gaussian noise scale for perturbed HSV channels "
                "(see dataset._randomly_perturb_image).",
                key="cfg_augmentation_spread",
            )
        else:
            st.session_state["cfg_augment_images"] = False

        st.divider()
        st.markdown("**Fixed Decoder Conditioning (for inference)**")
        st.caption(
            "Set default fixed values for decoder conditioning during inference. "
            "This normalizes all reconstructions to the same background color."
        )

        use_fixed_decoder = st.checkbox(
            "Enable fixed decoder conditioning",
            value=True,
            help="When enabled, reconstructions during inference use fixed HSV values instead of each image's actual values.",
            key="cfg_use_fixed_decoder"
        )

        if use_fixed_decoder and cond_vars:
            fixed_decoder_values = {}
            cols = st.columns(len(cond_vars))

            for i, var in enumerate(cond_vars):
                with cols[i]:
                    var_name = var.replace('average_', '').title()
                    default_val = 0.5  # Neutral default

                    if var == "average_hue":
                        help_text = "0=red, 0.33=green, 0.66=blue. Typical agar: 0.1-0.3"
                    elif var == "average_saturation":
                        help_text = "0=gray, 1=fully saturated"
                    else:  # average_value
                        help_text = "0=black, 1=bright"

                    val = st.slider(
                        f"Fixed {var_name}",
                        min_value=0.0,
                        max_value=1.0,
                        value=default_val,
                        step=0.05,
                        help=help_text,
                        key=f"cfg_fixed_{var}"
                    )
                    fixed_decoder_values[var] = val

            st.session_state["cfg_fixed_decoder_values"] = fixed_decoder_values
        else:
            st.session_state["cfg_fixed_decoder_values"] = {}

        # Explain FiLM conditioning
        with st.expander("How does FiLM conditioning work?", expanded=False):
            st.markdown("""
            **FiLM (Feature-wise Linear Modulation)** injects HSV color information into the model:

            | Stage | What happens |
            |-------|--------------|
            | **Encoder** | Processes images with their actual HSV values from the background |
            | **Decoder (training)** | Learns to reconstruct using *augmented* HSV values (random perturbations) |
            | **Decoder (inference)** | Can be forced to reconstruct at a *fixed* HSV level |

            **Why is this useful?**
            - Different *C. albicans* strains have different background colors
            - By conditioning on background hue, the model learns morphology *independent* of color
            - During inference, you can reconstruct all images at the same hue for fair comparison

            **Fixed-hue reconstruction (inference only):**
            ```python
            config.conditional_decoder_fixed_values = {'average_hue': 0.5}  # 0-1 scale
            ```

            This normalizes all reconstructions to the same background color, enabling morphology comparison across strains.
            """)
    else:
        # Unconditional strategies — clear conditioning state
        st.session_state["cfg_use_fixed_decoder"] = False
        st.session_state["cfg_fixed_decoder_values"] = {}

    # Tendril VAE parameters (Strategies 14, 15 & 16)
    if strategy in (14, 15, 16):
        st.divider()
        st.subheader("Tendril VAE Parameters")
        st.caption(
            "Each tendril is a small VAE trained on skip connection feature maps "
            "after the main VAE finishes."
        )

        tcol1, tcol2 = st.columns(2)
        with tcol1:
            st.number_input(
                "Tendril Epochs",
                min_value=5,
                max_value=200,
                value=50,
                step=5,
                help="Number of training epochs for each tendril VAE.",
                key="cfg_tendril_num_epochs"
            )
            st.selectbox(
                "Tendril Batch Size",
                options=[32, 64, 128, 256, 512],
                index=3,  # Default 256
                help="Batch size for tendril training.",
                key="cfg_tendril_batch_size"
            )
            st.selectbox(
                "Tendril Loss Function",
                options=["MSE", "Log-Cosh"],
                index=0,
                help="Loss function for tendril reconstruction. MSE is standard, Log-Cosh is more robust to outliers.",
                key="cfg_tendril_loss_fn"
            )
        with tcol2:
            st.select_slider(
                "Tendril Learning Rate",
                options=[1e-6, 5e-6, 1e-5, 5e-5, 1e-4, 5e-4, 1e-3],
                value=1e-4,
                format_func=lambda x: f"{x:.0e}",
                help="Learning rate for tendril VAE training.",
                key="cfg_tendril_lr"
            )
            st.selectbox(
                "Tendril Latent Dimension",
                options=[32, 64, 128, 256],
                index=2,  # Default 128
                help="Latent space dimension for each tendril VAE.",
                key="cfg_tendril_latent_dim"
            )
            st.number_input(
                "Tendril Weight Decay",
                min_value=0.0,
                max_value=0.1,
                value=1.5e-3,
                step=1e-4,
                format="%.4f",
                help="Weight decay (L2 regularization) for tendril optimizer.",
                key="cfg_tendril_weight_decay"
            )

        with st.expander("Advanced Tendril Options"):
            atcol1, atcol2 = st.columns(2)
            with atcol1:
                st.number_input(
                    "Tendril KL Weight",
                    min_value=0.0,
                    max_value=100.0,
                    value=1.0,
                    step=0.1,
                    format="%.2f",
                    help="Weight for KL divergence loss in tendril training.",
                    key="cfg_tendril_kl_weight"
                )
            with atcol2:
                st.number_input(
                    "Tendril Reconstruction Weight",
                    min_value=0.0,
                    max_value=100.0,
                    value=1.0,
                    step=0.1,
                    format="%.2f",
                    help="Weight for reconstruction loss (MSE or Log-Cosh) in tendril training.",
                    key="cfg_tendril_recon_weight"
                )

    # Advanced options
    if st.checkbox("Show advanced architecture options", key="show_arch_advanced"):
        col3, col4 = st.columns(2)
        with col3:
            intermediate_dim = st.number_input(
                "Intermediate Dimension",
                min_value=64,
                max_value=512,
                value=256,
                step=64,
                help="Size of hidden layers in encoder/decoder.",
                key="cfg_intermediate_dim"
            )
        with col4:
            leaky_relu_slope = st.number_input(
                "Leaky ReLU Slope",
                min_value=0.01,
                max_value=0.2,
                value=0.02,
                step=0.01,
                help="Negative slope for LeakyReLU activations.",
                key="cfg_leaky_slope"
            )


def _render_training_config():
    """Render training hyperparameters configuration."""
    is_conditional = _coerce_strategy_int(st.session_state.get("cfg_strategy", 14)) in (
        7, 8, 9.6, 9.7, 13, 14, 15, 16
    )

    col1, col2 = st.columns(2)

    with col1:
        st.number_input(
            "Number of Epochs",
            min_value=10,
            max_value=500,
            value=100,
            step=10,
            help=TOOLTIPS["number_epochs"],
            key="cfg_epochs"
        )

        st.selectbox(
            "Batch Size",
            options=[32, 64, 128, 256, 512],
            index=3,  # Default 256
            help=TOOLTIPS["batch_size"],
            key="cfg_batch_size"
        )

    with col2:
        st.select_slider(
            "VAE Learning Rate",
            options=[1e-5, 5e-5, 1e-4, 5e-4, 1e-3],
            value=1e-4,
            format_func=lambda x: f"{x:.0e}",
            help=TOOLTIPS["vae_lr"],
            key="cfg_vae_lr"
        )

        if is_conditional:
            st.select_slider(
                "FiLM Learning Rate",
                options=[1e-4, 5e-4, 1e-3, 5e-3],
                value=5e-4,
                format_func=lambda x: f"{x:.0e}",
                help=TOOLTIPS["film_lr"],
                key="cfg_film_lr"
            )
        else:
            st.session_state["cfg_film_lr"] = None

    # Loss function selector
    st.selectbox(
        "Loss Function",
        options=["mse", "lpips", "ssim"],
        index=0,
        format_func=lambda x: {
            "mse": "MSE (Mean Squared Error)",
            "lpips": "LPIPS (Perceptual)",
            "ssim": "SSIM (Structural Similarity)",
        }.get(x, x),
        help="LPIPS uses VGG features for perceptual quality (sharper images). MSE is pixel-wise. SSIM measures structural similarity.",
        key="cfg_loss_fn"
    )

    # Advanced training options
    if st.checkbox("Show advanced training options", key="show_train_advanced"):
        col3, col4 = st.columns(2)
        with col3:
            st.number_input(
                "KL Weight (Peak Beta)",
                min_value=0.1,
                max_value=10.0,
                value=1.0,
                step=0.1,
                help=TOOLTIPS["kl_weight"],
                key="cfg_kl_weight"
            )

            st.number_input(
                "KL Cycle Length (epochs)",
                min_value=2,
                max_value=200,
                value=10,
                step=2,
                help=TOOLTIPS["cycle_length"],
                key="cfg_cycle_length"
            )

            # Show weight control matching the selected loss function
            _loss_fn = st.session_state.get("cfg_loss_fn", "mse")
            if _loss_fn == "mse":
                st.number_input(
                    "MSE Weight",
                    min_value=0.1,
                    max_value=200.0,
                    value=100.0,
                    step=10.0,
                    help="Weight for MSE reconstruction loss.",
                    key="cfg_mse_weight"
                )
            elif _loss_fn == "lpips":
                st.number_input(
                    "LPIPS Weight",
                    min_value=0.1,
                    max_value=200.0,
                    value=100.0,
                    step=10.0,
                    help="Weight for LPIPS perceptual reconstruction loss.",
                    key="cfg_lpips_weight"
                )
            elif _loss_fn == "ssim":
                st.number_input(
                    "SSIM Weight",
                    min_value=0.1,
                    max_value=200.0,
                    value=100.0,
                    step=10.0,
                    help="Weight for SSIM structural similarity loss.",
                    key="cfg_ssim_weight"
                )

        with col4:
            if is_conditional:
                st.number_input(
                    "Conditional Loss Weight",
                    min_value=1.0,
                    max_value=5000.0,
                    value=1000.0,
                    step=100.0,
                    help=TOOLTIPS["conditional_loss_weight"],
                    key="cfg_cond_weight"
                )
            else:
                st.session_state["cfg_cond_weight"] = 0.0

            st.number_input(
                "Reconstruction Preview Period",
                min_value=1,
                max_value=50,
                value=5,
                step=1,
                help=TOOLTIPS["report_periodicity"],
                key="cfg_report_period"
            )

    # Visualization options (always visible)
    st.markdown("**📊 Visualization Options**")
    col_viz1, col_viz2 = st.columns([3, 1])
    with col_viz1:
        skip_initial = st.checkbox(
            "Skip initial epochs in loss plot",
            value=False,
            help="Exclude the first few epochs from the loss plot during training to better see trends after the initial high-loss period.",
            key="skip_initial_epochs"
        )
    with col_viz2:
        if skip_initial:
            epochs_to_skip = st.number_input(
                "Epochs to skip",
                min_value=1,
                max_value=50,
                value=5,
                step=1,
                help="Number of initial epochs to exclude from the plot",
                key="epochs_to_skip"
            )


def _render_output_config():
    """Render output configuration section."""
    # If strategy changed, refresh run name to stay consistent (only for auto-style names)
    prev_strategy = st.session_state.get("_output_config_last_strategy")
    strategy = _coerce_strategy_int(st.session_state.get("cfg_strategy", 14))
    st.session_state["_output_config_last_strategy"] = strategy
    if prev_strategy is not None and prev_strategy != strategy:
        current_save = st.session_state.get("cfg_save_name", "")
        if re.match(r"run_\d{6}_s\d+$", current_save):
            st.session_state["cfg_save_name"] = re.sub(
                r"_s\d+$", f"_s{strategy}", current_save
            )

    col1, col2 = st.columns(2)

    with col1:
        # Auto-generate experiment name based on timestamp
        default_exp_name = f"training_{datetime.now().strftime('%Y%m%d')}"
        exp_name = st.text_input(
            "Experiment Name",
            value=default_exp_name,
            help="Name for this experiment. Will be used in output path.",
            key="cfg_exp_name"
        )

    with col2:
        default_save_name = f"run_{datetime.now().strftime('%H%M%S')}_s{strategy}"
        save_name = st.text_input(
            "Run Name",
            value=default_save_name,
            help="Name for this specific training run.",
            key="cfg_save_name"
        )

    # Show output path
    output_path = Path(DEFAULT_OUTPUT_BASE) / exp_name / save_name
    st.info(f"Output directory: `{output_path}`")


def _validate_and_start():
    """Validate configuration and start training."""
    # Get selected GPU
    selected_gpu = st.session_state.get("selected_gpu", 0)

    # Get filtering parameters and convert to restrict_to_day for compatibility
    day_filter = st.session_state.get("cfg_day_filter", "day2")
    washed_filter = st.session_state.get("cfg_washed_filter", "no")
    media_filter = st.session_state.get("cfg_media_filter", "all")

    # Convert day_filter to restrict_to_day for TLVConfig compatibility
    if day_filter == 'day2':
        restrict_to_day = 2
    elif day_filter == 'day5':
        restrict_to_day = 5
    else:
        restrict_to_day = None  # 'all' or 'wash'

    # Determine strategy and architecture
    strategy = _coerce_strategy_int(st.session_state.get("cfg_strategy", 14))
    STRATEGY_ARCH_MAP = {
        0: "c_vae",
        1: "uc_vae",
        7: "cond_uc_vae",
        8: "cond_uc_vae_attention",
        9.6: "cond_uc_vae_attention",
        9.7: "cond_uc_vae_spatial",
        11: "rc_vae",
        12: "uc_vae_multistage",
        13: "crutch_vae",
        14: "tendril_vae",
        15: "tendril_vae",
        16: "tendril_vae",
    }
    architecture = STRATEGY_ARCH_MAP.get(strategy, "tendril_vae")
    is_conditional = (strategy in (7, 8, 9.6, 9.7, 13, 14, 15, 16))
    _decoder_pair_strategies = (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16)
    _augment_images_on = (
        st.session_state.get("cfg_augment_images", False)
        if strategy in _decoder_pair_strategies
        else False
    )
    _cond_vars_list = (
        list(
            st.session_state.get("cfg_cond_vars")
            or ["average_hue", "average_saturation", "average_value"]
        )
        if is_conditional
        else None
    )

    # Collect configuration
    config_dict = {
        # Data
        "raw_images_path": st.session_state.cfg_image_dir,
        "image_dimension": st.session_state.cfg_image_dim,
        "restrict_to_day": restrict_to_day,
        # Filtering (new parameters)
        "day_filter": day_filter,
        "washed_filter": washed_filter,
        "media_filter": media_filter,
        "grayscale": st.session_state.get("cfg_grayscale", False),
        "grayscale_background_normalize": st.session_state.get("cfg_grayscale_bg_normalize", False),
        "grayscale_bg_target": st.session_state.get("cfg_grayscale_bg_target", 0.5),
        "grayscale_bg_border": int(st.session_state.get("cfg_grayscale_bg_border", 12)),
        "merge_manually_labelled": st.session_state.get("cfg_merge_manual_labels", True),
        # Split
        "train_num": st.session_state.cfg_train_num,
        "validation_num": st.session_state.cfg_val_num,
        "test_num": st.session_state.cfg_test_num,
        # Model
        "architecture": architecture,
        "strategy": strategy,
        "latent_dim": st.session_state.cfg_latent_dim,
        "conditional_variables": st.session_state.cfg_cond_vars if is_conditional else None,
        "intermediate_dim": st.session_state.get("cfg_intermediate_dim", 256),
        "leaky_relu_slope": st.session_state.get("cfg_leaky_slope", 0.02),
        # Fixed decoder conditioning for inference (only for conditional strategies)
        "use_fixed_decoder": st.session_state.get("cfg_use_fixed_decoder", True) if is_conditional else False,
        "conditional_decoder_fixed_values": st.session_state.get("cfg_fixed_decoder_values", {"average_hue": 0.5}) if is_conditional else {},
        # Decoder conditioning augmentation (Strategies 9.6, 9.7, 13, 14, 15, 16)
        "augment_decoder_images": (
            st.session_state.get("cfg_augment_decoder_images", False)
            if strategy in (9.6, 9.7, 13, 14, 15, 16)
            else False
        ),
        # HSV image adjustment (deterministic per-image shift). Only effective
        # for conditional strategies (incl. 9.6's adjustment ablations); the
        # factory forces it off for unconditional ones. Targets default to
        # cohort means.
        "adjust_images": (
            bool(st.session_state.get("cfg_adjust_images", False))
            if is_conditional
            else False
        ),
        "target_hue": float(st.session_state.get("cfg_target_hue", 107.15 / 255.0)),
        "target_saturation": float(
            st.session_state.get("cfg_target_saturation", 49.07 / 255.0)
        ),
        "target_value": float(st.session_state.get("cfg_target_value", 93.38 / 255.0)),
        # Strategy 9.6/9.7: SE/spatial attention reduction ratio
        "reduction_ratio": int(st.session_state.get("cfg_reduction_ratio", 16)),
        # Paired augmented rows in target_df (strategies 9.5–9.9, 13, 14, 15, 16)
        "augment_images": _augment_images_on,
        "augmentation_variables": (
            _cond_vars_list if _augment_images_on and _cond_vars_list else None
        ),
        "augmentation_spread": float(
            st.session_state.get("cfg_augmentation_spread", 0.1)
        ),
        # Training
        "number_epochs": st.session_state.cfg_epochs,
        "batch_size": st.session_state.cfg_batch_size,
        "vae_lr": st.session_state.cfg_vae_lr,
        "film_lr": st.session_state.get("cfg_film_lr") if is_conditional else None,
        "vae_loss_function": st.session_state.get("cfg_loss_fn", "mse"),
        "kl_weight": st.session_state.get("cfg_kl_weight", 1.0),
        "cycle_length": st.session_state.get("cfg_cycle_length", 10),
        "mse_weight": st.session_state.get("cfg_mse_weight", 100.0),
        "lpips_weight": st.session_state.get("cfg_lpips_weight", 100.0),
        "ssim_weight": st.session_state.get("cfg_ssim_weight", 100.0),
        "conditional_loss_weight": st.session_state.get("cfg_cond_weight", 1000.0) if is_conditional else 0.0,
        "report_periodicity": st.session_state.get("cfg_report_period", 5),
        # Tendril parameters (Strategy 14 only)
        "tendril_num_epochs": st.session_state.get("cfg_tendril_num_epochs", 50),
        "tendril_lr": st.session_state.get("cfg_tendril_lr", 1e-4),
        "tendril_batch_size": st.session_state.get("cfg_tendril_batch_size", 256),
        "tendril_latent_dim": st.session_state.get("cfg_tendril_latent_dim", 128),
        "tendril_weight_decay": st.session_state.get("cfg_tendril_weight_decay", 1.5e-3),
        "tendril_loss_fn": st.session_state.get("cfg_tendril_loss_fn", "MSE"),
        "tendril_kl_weight": st.session_state.get("cfg_tendril_kl_weight", 1.0),
        "tendril_recon_weight": st.session_state.get("cfg_tendril_recon_weight", 1.0),
        # Output
        "experiment_name": st.session_state.cfg_exp_name,
        "save_name": st.session_state.cfg_save_name,
        # GPU
        "device": f"cuda:{selected_gpu}" if selected_gpu is not None else "cpu",
    }

    # Validate
    errors = []

    # Check paths
    if not Path(config_dict["raw_images_path"]).exists():
        errors.append("Image directory does not exist")

    # Check GPU
    if not torch.cuda.is_available():
        st.warning("No GPU available. Training will proceed on CPU but will be very slow.")

    # Check filtered image count
    filtered_count = st.session_state.get("filtered_image_count", 0)
    total_needed = config_dict["train_num"] + config_dict["validation_num"] + config_dict["test_num"]

    if filtered_count == 0:
        errors.append("No images match the current filter criteria")

    if (
        strategy in (13, 14, 15, 16)
        and config_dict.get("augment_decoder_images")
        and not config_dict.get("augment_images")
    ):
        errors.append(
            "Decoder background augmentation requires HSV-augmented partner rows. "
            "Enable 'Create HSV-augmented partner rows (decoder pairing)' above."
        )
    elif filtered_count < total_needed:
        errors.append(f"Not enough images after filtering: found {filtered_count}, need {total_needed}")

    # Batch size warnings (not errors)
    batch_size = config_dict["batch_size"]
    if config_dict["validation_num"] < batch_size:
        st.warning(
            f"Validation set ({config_dict['validation_num']}) is smaller than batch size ({batch_size}). "
            "The incomplete batch will still be used."
        )

    if errors:
        for error in errors:
            st.error(error)
        return

    # Store filtered images for training
    config_dict["filtered_images"] = st.session_state.get("filtered_images", [])

    # Store config and start training
    TrainingStateManager.set_state("config_dict", config_dict)
    TrainingStateManager.set_state("status", "training")
    TrainingStateManager.set_state("total_epochs", config_dict["number_epochs"])
    TrainingStateManager.add_log_message(
        f"Configuration validated. Starting training with {filtered_count} filtered images "
        f"(day={day_filter}, washed={washed_filter}, media={media_filter}, "
        f"grayscale={config_dict['grayscale']})..."
    )

    st.success("Configuration validated! Starting training...")
    st.rerun()


def _cleanup_training_resources():
    """
    Clean up GPU resources from training to prevent memory leaks on restart.

    Called before reset_session_state() in cancel/reset paths AND at the
    start of _start_training() as a safety net.  Idempotent — safe to call
    multiple times.

    Strategy: delete *references* to heavy objects (model, optimizers,
    dataloaders) then let the garbage collector free the underlying CUDA
    tensors.  We intentionally do NOT call model.cpu() because that
    allocates a second (CPU) copy, which can itself OOM on a tight system
    and defeats the purpose of cleanup.
    """
    import gc

    try:
        trainer = TrainingStateManager.get_state("trainer")
        factory = TrainingStateManager.get_state("factory")

        for obj in (trainer, factory):
            if obj is None:
                continue

            # Clear optimizer state dicts first — they hold references to
            # parameter tensors that would otherwise prevent GC.
            for opt_name in ("optimizer_vae", "optimizer_film"):
                opt = getattr(obj, opt_name, None)
                if opt is not None:
                    try:
                        opt.state.clear()
                    except Exception:
                        pass

            # Delete all heavy attributes
            for attr_name in (
                "vae", "optimizer_vae", "optimizer_film", "scheduler",
                "loss_model", "dataset", "train_dataloader",
                "validation_dataloader",
                # Callback closures capture st.empty() placeholders — drop them
                "progress_callback", "reconstruction_callback",
                "stop_check", "pause_check",
            ):
                try:
                    if hasattr(obj, attr_name):
                        delattr(obj, attr_name)
                except Exception:
                    pass

        # Null out session state references so GC can collect the objects
        for key in ("trainer", "factory", "tlv_config", "reconstruction_data", "reconstruction_history"):
            try:
                TrainingStateManager.set_state(key, None)
            except Exception:
                pass

        # Two GC passes to break reference cycles, then free CUDA cache
        gc.collect()
        gc.collect()
        if torch.cuda.is_available():
            torch.cuda.synchronize()
            torch.cuda.empty_cache()

        logger.info("Training resources cleaned up, CUDA cache cleared")

    except Exception as e:
        logger.warning(f"Error during resource cleanup: {e}")


def _render_step_2_training():
    """Render Step 2: Training progress."""

    # Check if cancel was requested - clean up GPU resources, reset, go back to config
    if TrainingStateManager.get_state("should_cancel"):
        _cleanup_training_resources()
        TrainingStateManager.reset_session_state()
        st.rerun()
        return
    
    st.header("Step 2: Training in Progress")

    # Get current state
    state = TrainingStateManager.to_training_state()
    config_dict = TrainingStateManager.get_state("config_dict")

    if config_dict is None:
        st.error("Configuration not found. Please restart.")
        return

    # Check if we need to actually start training
    trainer = TrainingStateManager.get_state("trainer")
    if trainer is None:
        _start_training(config_dict)
        return

    # Render progress
    _render_training_progress(state)


def _start_training(config_dict: Dict[str, Any]):
    """Initialize and start training.

    This function blocks the Streamlit script for the duration of training.
    Button clicks during training cause Streamlit to raise RerunException
    inside the progress callbacks.  The ``try / except / finally`` structure
    below ensures GPU resources are freed regardless of how execution ends:

    * Normal completion / stop — model is saved, then ``st.rerun()``.
    * Cancel — cleanup, reset, ``st.rerun()``.
    * RerunException (button click interrupts script) — ``finally`` cleans
      up GPU memory so the next run doesn't OOM.
    * Unexpected errors — cleanup, set error state, ``st.rerun()``.
    """
    st.info("Initializing training...")

    # ── Safety-net: free leftover GPU resources from a previous run ────
    # This is the most important line for preventing OOM on the second run.
    # Even if a prior cleanup path was missed (e.g. RerunException raced
    # past the except block), this ensures we start with a clean GPU.
    _cleanup_training_resources()

    # Track whether we exited the training loop normally so the finally
    # block knows whether it still needs to clean up.
    training_settled = False

    try:
        import os
        from candescence.core.config import TLVConfig
        from candescence.tlv.factory import Factory
        from candescence.interface.training.streamlit_trainer import StreamlitTrainer

        # Set process priority to nice 19
        try:
            os.nice(19)
            logger.info("Process priority set to nice 19")
        except (OSError, AttributeError):
            logger.warning("Could not set process priority")

        # Create TLVConfig with the selected device (use_zoo=True so models save to zoo/)
        device_str = config_dict.get("device", "cuda:0")
        config = TLVConfig(
            experiment_name=config_dict["experiment_name"],
            save_name=config_dict["save_name"],
            device=device_str,
            use_zoo=True
        )

        # Set all config attributes
        for key, value in config_dict.items():
            if key == "device":
                # Device already set in constructor
                continue
            if hasattr(config, key):
                setattr(config, key, value)
            else:
                setattr(config, key, value)

        # Create directories
        config.create_directories()

        # Set custom paths if provided
        if config_dict.get("raw_images_path"):
            config.raw_images_path = Path(config_dict["raw_images_path"])
        if config_dict.get("metadata_path"):
            config.metadata_path = Path(config_dict["metadata_path"])

        TrainingStateManager.add_log_message(f"Using device: {config.device}")

        # Store config
        TrainingStateManager.set_state("tlv_config", config)

        # Create Factory and prepare
        with st.spinner("Loading dataset..."):
            factory = Factory(config)
            factory.load_dataset()
            TrainingStateManager.add_log_message(f"Dataset loaded: {len(factory.dataset)} samples")

        with st.spinner("Preparing model..."):
            factory.prepare_vae()
            factory.set_training_dataloader()
            TrainingStateManager.add_log_message("Model and dataloaders prepared")

        # Store factory
        TrainingStateManager.set_state("factory", factory)

        # Create progress placeholders
        is_tendril_strategy = (config_dict.get("strategy") in (14, 15, 16))
        phase_placeholder = st.empty()
        if is_tendril_strategy:
            with phase_placeholder:
                st.info("Phase 1/2: Training Outer VAE")

        progress_placeholder = st.empty()

        # Display skip epochs setting info (non-interactive during training)
        skip_initial = st.session_state.get("skip_initial_epochs", False)
        epochs_to_skip = st.session_state.get("epochs_to_skip", 5) if skip_initial else 0
        skip_info_placeholder = st.empty()
        if skip_initial:
            with skip_info_placeholder:
                st.caption(f"📊 Skip mode: First {epochs_to_skip} epochs will be hidden from plots")

        chart_placeholder = st.empty()
        loss_components_placeholder = st.empty()
        recon_container = st.empty()

        # Tendril-specific placeholders (Phase 2)
        tendril_phase_placeholder = st.empty()
        tendril_progress_placeholder = st.empty()
        tendril_chart_placeholder = st.empty()

        # Add training control buttons
        st.divider()
        st.subheader("Training Controls")

        # Prominent terminate button
        if st.button("🛑 TERMINATE TRAINING", type="primary", use_container_width=True, help="Stop training immediately and save the current model", key="terminate_btn_training"):
            TrainingStateManager.set_state("should_stop", True)
            TrainingStateManager.add_log_message("Training terminated by user")
            st.warning("Training will stop after the current epoch completes...")

        st.caption("Click to stop training and save the model at the current epoch.")

        # Additional controls
        col1, col2, col3 = st.columns(3)
        with col1:
            if st.button("⏸️ Pause", key="pause_btn_training"):
                TrainingStateManager.set_state("should_pause", True)
        with col2:
            if st.button("💾 Stop & Save", key="stop_save_btn_training"):
                TrainingStateManager.set_state("should_stop", True)
                TrainingStateManager.add_log_message("Stop requested by user")
        with col3:
            if st.button("❌ Cancel", key="cancel_btn_training"):
                TrainingStateManager.set_state("should_stop", True)
                TrainingStateManager.set_state("should_cancel", True)
                TrainingStateManager.add_log_message("Cancel requested")

        st.divider()
        log_placeholder = st.empty()

        # ── If a button click already set stop/cancel, skip the loop ──
        if TrainingStateManager.get_state("should_cancel"):
            _cleanup_training_resources()
            TrainingStateManager.reset_session_state()
            training_settled = True
            st.rerun()
            return

        # Define callbacks
        def on_progress(metrics: Dict[str, Any]):
            TrainingStateManager.update_epoch(
                epoch=metrics["epoch"],
                train_loss=metrics["train_loss"],
                val_loss=metrics["val_loss"],
                train_kl=metrics.get("train_kl", 0),
                val_kl=metrics.get("val_kl", 0),
                train_recon=metrics.get("train_recon", 0),
                val_recon=metrics.get("val_recon", 0),
                train_cond=metrics.get("train_cond", 0),
                val_cond=metrics.get("val_cond", 0),
            )
            TrainingStateManager.set_state("elapsed_seconds", metrics.get("elapsed_seconds", 0))

            # Update progress display
            with progress_placeholder:
                epoch = metrics["epoch"] + 1
                total = metrics["total_epochs"]
                st.progress(epoch / total, text=f"Epoch {epoch}/{total}")

            # Update chart (respecting skip initial epochs setting)
            # Use .container() so multiple elements (chart + caption) can coexist
            with chart_placeholder.container():
                loss_history = TrainingStateManager.get_state("loss_history")
                if loss_history:
                    df = pd.DataFrame(loss_history)
                    # Auto burn-in: skip first 3 epochs once we have 10+ epochs
                    auto_burn_in = 3 if len(df) > 10 else 0
                    st.caption(
                        "Live: **Total**, **KL**, **Reconstruction**, and **Conditional** "
                        "(when non-zero); train=solid, val=dashed."
                    )
                    fig = _create_vae_loss_dashboard(
                        df=df,
                        burn_in=auto_burn_in,
                        use_log_scale=False,
                        height=None,
                    )
                    st.plotly_chart(fig, use_container_width=True)

            # Update loss components (respecting skip setting)
            # Use epoch in key_prefix so each callback gets unique keys (avoids duplicate key error)
            with loss_components_placeholder:
                loss_history = TrainingStateManager.get_state("loss_history")
                if loss_history and len(loss_history) > 1:
                    df = pd.DataFrame(loss_history)
                    with st.expander("Loss Components", expanded=False):
                        _render_loss_components(df, key_prefix=f"realtime_comp_ep{metrics['epoch']}")

            TrainingStateManager.add_log_message(
                f"Epoch {metrics['epoch'] + 1}: train={metrics['train_loss']:.4f}, val={metrics['val_loss']:.4f}"
            )

        def on_reconstruction(recon_data: Dict[str, Any], epoch: int):
            # Accumulate reconstruction history
            recon_history = TrainingStateManager.get_state("reconstruction_history") or []
            recon_history.append({"data": recon_data, "epoch": epoch})
            TrainingStateManager.set_state("reconstruction_history", recon_history)
            # Keep backward compat
            TrainingStateManager.set_state("reconstruction_data", recon_data)
            TrainingStateManager.set_state("reconstruction_images", recon_data.get('images'))
            # Clear previous output and re-render the full history
            with recon_container.container():
                _render_reconstruction_history(recon_history)

        def should_stop():
            return TrainingStateManager.get_state("should_stop")

        def should_pause():
            return TrainingStateManager.get_state("should_pause")

        # Tendril progress callback (Phase 2)
        if not TrainingStateManager.get_state("tendril_loss_history"):
            TrainingStateManager.set_state("tendril_loss_history", {})

        def on_tendril_progress(metrics: Dict[str, Any]):
            status = metrics.get("status", "")

            if status == "computing_skips":
                with phase_placeholder:
                    st.info("Outer VAE training complete. Computing skip connections for tendril training...")
                return

            if status == "training":
                key = metrics["tendril_key"]
                current = metrics["current_tendril"]
                total = metrics["total_tendrils"]
                epoch = metrics["tendril_epoch"]
                total_epochs = metrics["tendril_total_epochs"]
                loss = metrics["loss"]
                kl = metrics["kl_loss"]
                recon = metrics["recon_loss"]

                # Update phase indicator
                with phase_placeholder:
                    st.info(f"Phase 2/2: Training Tendril VAEs (Layer {current + 1}/{total})")

                # Update progress bars: overall + per-tendril epoch
                with tendril_progress_placeholder.container():
                    overall = (current * total_epochs + epoch + 1) / (total * total_epochs)
                    st.progress(overall, text=f"Overall: Tendril {current + 1}/{total}")
                    tendril_frac = (epoch + 1) / total_epochs
                    st.progress(tendril_frac, text=f"Layer {key}: Epoch {epoch + 1}/{total_epochs}")

                # Accumulate loss history per tendril (persisted in session state)
                tendril_loss_history = TrainingStateManager.get_state("tendril_loss_history")
                if key not in tendril_loss_history:
                    tendril_loss_history[key] = []
                tendril_loss_history[key].append({
                    "epoch": epoch + 1,
                    "loss": loss,
                    "kl_loss": kl,
                    "recon_loss": recon,
                })
                TrainingStateManager.set_state("tendril_loss_history", tendril_loss_history)

                # Update tendril loss chart
                with tendril_chart_placeholder.container():
                    _render_tendril_loss_chart("live")

                TrainingStateManager.add_log_message(
                    f"Tendril Layer {key} - Epoch {epoch + 1}/{total_epochs}: "
                    f"loss={loss:.4f}, KL={kl:.4f}, RECON={recon:.4f}"
                )

        # Create trainer
        trainer = StreamlitTrainer(
            factory=factory,
            progress_callback=on_progress,
            reconstruction_callback=on_reconstruction,
            stop_check=should_stop,
            pause_check=should_pause,
            tendril_progress_callback=on_tendril_progress,
        )

        TrainingStateManager.set_state("trainer", trainer)
        TrainingStateManager.set_state("start_time", datetime.now())
        TrainingStateManager.add_log_message("Training started")

        # Run training
        summary = trainer.train_with_callbacks()

        # Check if cancel was requested (no save, full reset)
        if TrainingStateManager.get_state("should_cancel"):
            TrainingStateManager.add_log_message("Training cancelled - not saving model")
            _cleanup_training_resources()
            TrainingStateManager.reset_session_state()
            training_settled = True
            st.rerun()
            return

        # Handle completion
        TrainingStateManager.set_state("status", summary["status"])
        TrainingStateManager.set_state("final_model_path", summary.get("model_path"))

        # Store tendril summary if present
        tendril_summary = summary.get("tendril_summary")
        if tendril_summary:
            TrainingStateManager.set_state("tendril_summary", tendril_summary)

        if summary["status"] == "completed":
            # Save model to zoo
            trainer.save_model()
            _save_args_and_register_in_zoo(config_dict, factory)
            completion_msg = "Training completed and model saved to zoo"
            if tendril_summary:
                n = tendril_summary["num_tendrils"]
                p2_time = tendril_summary["phase2_seconds"]
                elapsed = summary.get("elapsed_seconds", 0)
                p1_time = elapsed - p2_time
                completion_msg += (
                    f" | Phase 1: {p1_time:.0f}s, Phase 2: {p2_time:.0f}s"
                    f" | {n} tendrils trained"
                )
                for k, v in (tendril_summary.get("tendril_losses") or {}).items():
                    if v is not None:
                        completion_msg += f" | Layer {k}: loss={v:.4f}"
            TrainingStateManager.add_log_message(completion_msg)
        elif summary["status"] == "stopped":
            trainer.save_model()
            _save_args_and_register_in_zoo(config_dict, factory)
            TrainingStateManager.add_log_message("Training stopped early and model saved to zoo")
        elif summary["status"] == "error":
            TrainingStateManager.set_error(summary.get("error_message", "Unknown error"))

        # Normal exit — model is saved or error recorded; don't clean up
        # because Step 3 (summary) needs the trainer in session state.
        training_settled = True
        st.rerun()

    except Exception as e:
        # Guard: on old Streamlit, RerunException inherits from Exception.
        # If we accidentally caught it, re-raise so Streamlit can rerun.
        if type(e).__name__ in ("RerunException", "StopException", "RerunData"):
            raise
        logger.exception("Training initialization failed")
        _cleanup_training_resources()
        TrainingStateManager.set_error(str(e))
        training_settled = True
        st.rerun()

    finally:
        # If we reach here without having "settled" the training state,
        # it means an unexpected exception (most likely RerunException)
        # blew us out of the training loop.  Free GPU memory now so the
        # next script run doesn't OOM when it allocates a new model.
        if not training_settled:
            logger.info("Training interrupted (likely RerunException) — cleaning up GPU resources")
            _cleanup_training_resources()


def _render_training_progress(state: TrainingState):
    """Render training progress display."""
    # Progress bar
    progress = state.get_progress_fraction()
    st.progress(progress, text=f"Epoch {state.current_epoch}/{state.total_epochs}")

    # Metrics row
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.metric("Current Epoch", f"{state.current_epoch}/{state.total_epochs}")
    with col2:
        st.metric("Best Val Loss", f"{state.best_val_loss:.4f}" if state.best_val_loss < float('inf') else "-")
    with col3:
        st.metric("Best Epoch", state.best_epoch if state.best_epoch > 0 else "-")
    with col4:
        st.metric("Elapsed", state.format_elapsed())

    # Loss chart with controls
    st.subheader("Loss Curves")
    if state.loss_history:
        df = pd.DataFrame(state.loss_history)
        _render_loss_chart_with_controls(
            df=df,
            columns=["train_loss", "val_loss"],
            key_prefix="training_main",
            show_controls=True,
            height=350
        )

        # Loss components in expandable section
        with st.expander("Loss Components", expanded=False):
            _render_loss_components(df, key_prefix="training_comp")
    else:
        st.info("Training starting...")

    # Reconstruction history (show all snapshots, not just the latest)
    recon_history = TrainingStateManager.get_state("reconstruction_history")
    if recon_history:
        _render_reconstruction_history(recon_history, key_prefix="progress_recon")
    elif TrainingStateManager.get_state("reconstruction_data") is not None:
        # Fallback: single reconstruction (backward compat)
        st.subheader("Reconstruction Quality")
        _render_reconstruction_grid_with_hsv(
            TrainingStateManager.get_state("reconstruction_data"),
            state.current_epoch,
            key_prefix="progress_recon_single",
        )
    elif state.reconstruction_images is not None:
        st.subheader("Reconstruction Quality")
        _render_reconstruction_grid(state.reconstruction_images, state.current_epoch)

    # Tendril loss chart (persists across reruns)
    _render_tendril_loss_chart("progress")

    # Training log
    with st.expander("Training Log", expanded=False):
        for msg in reversed(state.training_log[-20:]):
            st.text(msg)

    # Control buttons
    st.divider()
    st.subheader("Training Controls")
    
    # Prominent terminate button
    if st.button("🛑 TERMINATE TRAINING", type="primary", use_container_width=True, help="Stop training immediately and save the current model"):
        TrainingStateManager.set_state("should_stop", True)
        TrainingStateManager.add_log_message("Training terminated by user")
        st.warning("Training will stop after the current epoch completes...")
        st.rerun()
    
    st.caption("Use the button above to stop training and save the model at the current epoch.")
    
    # Additional controls in columns
    col1, col2, col3 = st.columns(3)

    with col1:
        is_paused = TrainingStateManager.get_state("should_pause")
        if is_paused:
            if st.button("▶️ Resume", type="secondary"):
                TrainingStateManager.set_state("should_pause", False)
                st.rerun()
        else:
            if st.button("⏸️ Pause"):
                TrainingStateManager.set_state("should_pause", True)
                st.rerun()

    with col2:
        if st.button("💾 Stop & Save", type="secondary", help="Stop training gracefully and save model"):
            TrainingStateManager.set_state("should_stop", True)
            TrainingStateManager.add_log_message("Stop requested by user")

    with col3:
        if st.button("❌ Cancel (no save)", type="secondary", help="Stop training without saving"):
            TrainingStateManager.set_state("should_stop", True)
            TrainingStateManager.set_state("should_cancel", True)
            st.rerun()


def _create_loss_chart(
    df: pd.DataFrame,
    columns: list,
    burn_in: int = 0,
    use_log_scale: bool = False,
    title: str = "",
    height: int = 300
) -> go.Figure:
    """
    Create a Plotly loss chart with burn-in and log scale options.

    Args:
        df: DataFrame with loss history
        columns: List of column names to plot
        burn_in: Number of initial epochs to skip (helps with y-axis scaling)
        use_log_scale: Whether to use log scale for y-axis
        title: Optional chart title
        height: Chart height in pixels

    Returns:
        Plotly Figure object
    """
    # Apply burn-in
    plot_df = df.iloc[burn_in:].copy()

    if len(plot_df) == 0:
        # Return empty figure if no data after burn-in
        fig = go.Figure()
        fig.add_annotation(
            text="Waiting for data...",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False
        )
        fig.update_layout(height=height)
        return fig

    # Adjust epoch numbers for x-axis (accounting for burn-in)
    plot_df = plot_df.reset_index(drop=True)
    plot_df['epoch'] = plot_df.index + burn_in + 1  # 1-indexed epochs

    # Create figure
    fig = go.Figure()

    # Color palette for train/val pairs
    colors = {
        'train_loss': '#1f77b4',  # blue
        'val_loss': '#ff7f0e',    # orange
        'train_kl': '#2ca02c',    # green
        'val_kl': '#d62728',      # red
        'train_recon': '#9467bd', # purple
        'val_recon': '#8c564b',   # brown
        'train_cond': '#e377c2',  # pink
        'val_cond': '#7f7f7f',    # gray
    }

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

    # Update layout
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


def _create_vae_loss_dashboard(
    df: pd.DataFrame,
    burn_in: int = 0,
    use_log_scale: bool = False,
    height: Optional[int] = None,
) -> go.Figure:
    """
    Multi-panel loss figure: total, KL, reconstruction, and conditional (if non-zero).

    Uses the same color scheme as _create_loss_chart. Train=solid, val=dashed.
    """
    plot_df = df.iloc[burn_in:].copy()
    if len(plot_df) == 0:
        fig = go.Figure()
        fig.add_annotation(
            text="Waiting for data...",
            xref="paper",
            yref="paper",
            x=0.5,
            y=0.5,
            showarrow=False,
        )
        fig.update_layout(height=height or 400)
        return fig

    plot_df = plot_df.reset_index(drop=True)
    plot_df["epoch"] = plot_df.index + burn_in + 1

    has_cond = False
    if "train_cond" in plot_df.columns and "val_cond" in plot_df.columns:
        has_cond = (plot_df["train_cond"].abs().max() > 1e-10) or (
            plot_df["val_cond"].abs().max() > 1e-10
        )

    colors = {
        "train_loss": "#1f77b4",
        "val_loss": "#ff7f0e",
        "train_kl": "#2ca02c",
        "val_kl": "#d62728",
        "train_recon": "#9467bd",
        "val_recon": "#8c564b",
        "train_cond": "#e377c2",
        "val_cond": "#7f7f7f",
    }

    titles = ("Total loss", "KL divergence", "Reconstruction")
    if has_cond:
        titles = titles + ("Conditional",)

    nrows = 4 if has_cond else 3
    fig = make_subplots(
        rows=nrows,
        cols=1,
        shared_xaxes=True,
        vertical_spacing=0.06,
        subplot_titles=titles,
    )

    def add_pair(row: int, train_col: str, val_col: str) -> None:
        if train_col in plot_df.columns:
            fig.add_trace(
                go.Scatter(
                    x=plot_df["epoch"],
                    y=plot_df[train_col],
                    mode="lines",
                    name=train_col.replace("_", " ").title(),
                    line=dict(color=colors.get(train_col), dash="solid"),
                    legendgroup=train_col,
                ),
                row=row,
                col=1,
            )
        if val_col in plot_df.columns:
            fig.add_trace(
                go.Scatter(
                    x=plot_df["epoch"],
                    y=plot_df[val_col],
                    mode="lines",
                    name=val_col.replace("_", " ").title(),
                    line=dict(color=colors.get(val_col), dash="dash"),
                    legendgroup=val_col,
                ),
                row=row,
                col=1,
            )

    add_pair(1, "train_loss", "val_loss")
    add_pair(2, "train_kl", "val_kl")
    add_pair(3, "train_recon", "val_recon")
    if has_cond:
        add_pair(4, "train_cond", "val_cond")

    yaxis_type = "log" if use_log_scale else "linear"
    fig.update_xaxes(title_text="Epoch", row=nrows, col=1)
    for r in range(1, nrows + 1):
        fig.update_yaxes(type=yaxis_type, row=r, col=1)

    default_h = 260 * nrows + 80
    fig.update_layout(
        height=height if height is not None else default_h,
        margin=dict(l=50, r=20, t=40, b=40),
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
        hovermode="x unified",
        showlegend=True,
    )
    return fig


def _render_tendril_loss_chart(key_prefix: str = "") -> None:
    """Render tendril loss chart from session state data.

    Args:
        key_prefix: Unique prefix for Streamlit widget keys to avoid duplicates.
    """
    tendril_loss_history = TrainingStateManager.get_state("tendril_loss_history")
    if not tendril_loss_history:
        return
    st.subheader("Tendril VAE Training Curves")
    chart_data = []
    for tk, entries in tendril_loss_history.items():
        for entry in entries:
            chart_data.append({
                "Layer": f"Layer {tk}",
                "Epoch": entry["epoch"],
                "Loss": entry["loss"],
            })
    if chart_data:
        tdf = pd.DataFrame(chart_data)
        fig = px.line(
            tdf, x="Epoch", y="Loss", color="Layer",
            title="Tendril Training Loss",
            height=300,
        )
        # Do not pass an explicit key here; this chart is always rendered
        # inside a dedicated placeholder or section, and supplying a fixed
        # key across multiple callback invocations in a single Streamlit
        # run can trigger StreamlitDuplicateElementKey errors.
        st.plotly_chart(fig, use_container_width=True)

    tendril_summary = TrainingStateManager.get_state("tendril_summary")
    if tendril_summary:
        st.caption(
            f"{tendril_summary['num_tendrils']} tendrils trained in "
            f"{tendril_summary['phase2_seconds']:.0f}s"
        )


def _render_loss_chart_with_controls(
    df: pd.DataFrame,
    columns: list,
    key_prefix: str,
    title: str = "",
    show_controls: bool = True,
    height: int = 300,
    show_component_breakdown: bool = True,
) -> None:
    """
    Render a loss chart with burn-in slider and log scale toggle.

    When show_component_breakdown is True and the dataframe has train_kl/val_kl
    columns, shows a stacked dashboard (total, KL, recon, conditional) instead
    of only the columns listed in ``columns``.

    Args:
        df: DataFrame with loss history
        columns: List of column names to plot (used when breakdown is off)
        key_prefix: Unique prefix for widget keys
        title: Optional chart title
        show_controls: Whether to show burn-in and log scale controls
        height: Chart height in pixels
        show_component_breakdown: If True, prefer multi-panel KL/recon/cond view
    """
    burn_in = 0
    use_log_scale = False
    use_breakdown = show_component_breakdown

    if show_controls and len(df) > 5:
        # Only show controls if we have enough data
        ctrl_col1, ctrl_col2, ctrl_col3, ctrl_col4 = st.columns([2, 1, 1, 1])

        with ctrl_col1:
            max_burn_in = max(0, len(df) - 3)  # Keep at least 3 points
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

        with ctrl_col4:
            has_kl_col = "train_kl" in df.columns
            if has_kl_col:
                use_breakdown = st.checkbox(
                    "Show KL / Recon / Cond",
                    value=True,
                    key=f"{key_prefix}_show_breakdown",
                    help="Multi-panel: total plus KL, reconstruction, and conditional loss",
                )

    can_dashboard = (
        show_component_breakdown
        and "train_kl" in df.columns
        and "val_kl" in df.columns
    )

    if use_breakdown and can_dashboard:
        fig = _create_vae_loss_dashboard(
            df=df,
            burn_in=burn_in,
            use_log_scale=use_log_scale,
            height=None,
        )
    else:
        fig = _create_loss_chart(
            df=df,
            columns=columns,
            burn_in=burn_in,
            use_log_scale=use_log_scale,
            title=title,
            height=height,
        )

    st.plotly_chart(fig, use_container_width=True, key=f"{key_prefix}_chart")


def _render_loss_components(df: pd.DataFrame, key_prefix: str = "components"):
    """
    Render individual loss component plots with burn-in controls.

    Args:
        df: DataFrame with loss history containing train/val for each component
        key_prefix: Unique prefix for widget keys to avoid conflicts
    """
    # Check which loss components have non-zero values
    has_kl = (df["train_kl"].abs() > 1e-10).any() or (df["val_kl"].abs() > 1e-10).any()
    has_recon = (df["train_recon"].abs() > 1e-10).any() or (df["val_recon"].abs() > 1e-10).any()
    has_cond = (df["train_cond"].abs() > 1e-10).any() or (df["val_cond"].abs() > 1e-10).any()

    if not (has_kl or has_recon or has_cond):
        st.info("No loss component data available yet.")
        return

    st.caption("Individual loss components (train=solid, validation=dashed)")

    # Shared burn-in control for all component charts
    burn_in = 0
    use_log_scale = False

    if len(df) > 5:
        ctrl_col1, ctrl_col2 = st.columns([2, 1])
        with ctrl_col1:
            max_burn_in = max(0, len(df) - 3)
            burn_in = st.slider(
                "Skip early epochs",
                min_value=0,
                max_value=max_burn_in,
                value=min(3, max_burn_in) if len(df) > 10 else 0,
                key=f"{key_prefix}_burn_in",
                help="Skip initial epochs with high loss for better visualization"
            )
        with ctrl_col2:
            use_log_scale = st.checkbox(
                "Log scale",
                value=False,
                key=f"{key_prefix}_log_scale"
            )

    # Create tabs for different views
    tab_combined, tab_individual = st.tabs(["Combined View", "Individual Components"])

    with tab_combined:
        # Combined chart with all components
        cols_to_plot = []
        if has_kl:
            cols_to_plot.extend(["train_kl", "val_kl"])
        if has_recon:
            cols_to_plot.extend(["train_recon", "val_recon"])
        if has_cond:
            cols_to_plot.extend(["train_cond", "val_cond"])

        if cols_to_plot:
            fig = _create_loss_chart(
                df=df,
                columns=cols_to_plot,
                burn_in=burn_in,
                use_log_scale=use_log_scale,
                height=300
            )
            st.plotly_chart(fig, width="stretch", key=f"{key_prefix}_combined")

    with tab_individual:
        # Individual charts in columns
        n_components = int(sum([has_kl, has_recon, has_cond]))
        if n_components > 0:
            cols = st.columns(n_components)
            col_idx = 0

            if has_kl:
                with cols[col_idx]:
                    st.markdown("**KL Divergence**")
                    fig_kl = _create_loss_chart(
                        df=df,
                        columns=["train_kl", "val_kl"],
                        burn_in=burn_in,
                        use_log_scale=use_log_scale,
                        height=250
                    )
                    st.plotly_chart(fig_kl, width="stretch", key=f"{key_prefix}_kl")
                col_idx += 1

            if has_recon:
                with cols[col_idx]:
                    st.markdown("**Reconstruction Loss**")
                    fig_recon = _create_loss_chart(
                        df=df,
                        columns=["train_recon", "val_recon"],
                        burn_in=burn_in,
                        use_log_scale=use_log_scale,
                        height=250
                    )
                    st.plotly_chart(fig_recon, width="stretch", key=f"{key_prefix}_recon")
                col_idx += 1

            if has_cond:
                with cols[col_idx]:
                    st.markdown("**Conditional Loss**")
                    fig_cond = _create_loss_chart(
                        df=df,
                        columns=["train_cond", "val_cond"],
                        burn_in=burn_in,
                        use_log_scale=use_log_scale,
                        height=250
                    )
                    st.plotly_chart(fig_cond, width="stretch", key=f"{key_prefix}_cond")

    # Show latest values
    if len(df) > 0:
        latest = df.iloc[-1]
        st.markdown("**Latest Values:**")
        metrics_cols = st.columns(3)

        with metrics_cols[0]:
            if has_kl:
                st.metric("KL (train/val)", f"{latest['train_kl']:.4f} / {latest['val_kl']:.4f}")

        with metrics_cols[1]:
            if has_recon:
                st.metric("Recon (train/val)", f"{latest['train_recon']:.4f} / {latest['val_recon']:.4f}")

        with metrics_cols[2]:
            if has_cond:
                st.metric("Cond (train/val)", f"{latest['train_cond']:.4f} / {latest['val_cond']:.4f}")


def _render_reconstruction_history(
    recon_history: List[Dict[str, Any]],
    key_prefix: str = "recon_hist",
):
    """
    Render full reconstruction history as stacked expanders.

    Only the latest entry is expanded by default.
    """
    if not recon_history:
        return

    st.subheader("Reconstruction Quality")
    for idx, entry in enumerate(recon_history):
        is_latest = (idx == len(recon_history) - 1)
        epoch = entry["epoch"]
        with st.expander(
            f"Epoch {epoch + 1}",
            expanded=is_latest,
        ):
            # Include the list index in the key so duplicate-epoch entries
            # (caused by callback replay) don't collide on widget keys.
            _render_reconstruction_grid_with_hsv(
                entry["data"], epoch, key_prefix=f"{key_prefix}_i{idx}_e{epoch}"
            )


def _render_reconstruction_grid(images: np.ndarray, epoch: int):
    """Render reconstruction comparison grid (legacy format - just images)."""
    if images is None or len(images) == 0:
        return

    # images shape: (2, n_images, H, W, C)
    originals = images[0]
    reconstructions = images[1]
    n_images = len(originals)

    st.caption(f"Reconstruction comparison (Epoch {epoch + 1})")

    # Create columns for images
    cols = st.columns(n_images)

    for i, col in enumerate(cols):
        with col:
            st.image(originals[i], caption="Original", use_container_width=True)
            st.image(reconstructions[i], caption="Reconstructed", use_container_width=True)


def _render_reconstruction_grid_with_hsv(
    recon_data: Dict[str, Any],
    epoch: int,
    key_prefix: str = "recon",
):
    """
    Render reconstruction comparison grid with HSV values.

    Shows three rows per column:
    1. Original image with HSV values
    2. Reconstruction at endogenous (original) hue
    3. Reconstruction at fixed target hue (if available)

    Args:
        recon_data: Dict with 'images', 'original_hsv', 'reconstructed_hsv',
                   'encoder_cond', 'decoder_cond', and optionally 'fixed_hue_data'
        epoch: Current epoch number
        key_prefix: Unique prefix for widget keys
    """
    images = recon_data.get('images')
    if images is None or len(images) == 0:
        return

    # images shape: (2, n_images, H, W, C)
    originals = images[0]
    reconstructions = images[1]
    n_images = len(originals)

    original_hsv = recon_data.get('original_hsv', [])
    reconstructed_hsv = recon_data.get('reconstructed_hsv', [])
    encoder_cond = recon_data.get('encoder_cond', [])
    decoder_cond = recon_data.get('decoder_cond', [])

    # Get fixed-hue data if available
    fixed_hue_data = recon_data.get('fixed_hue_data')
    has_fixed_hue = fixed_hue_data is not None

    # Get target hue value for caption
    target_hue_val = 0.0
    if has_fixed_hue:
        target_cond = fixed_hue_data.get('target_cond', [])
        if isinstance(target_cond, (list, np.ndarray)) and len(target_cond) > 0:
            target_hue_val = target_cond[0]
        elif isinstance(target_cond, (int, float, np.number)):
            target_hue_val = float(target_cond)

    # Check for color originals (present when grayscale mode is active)
    color_originals = recon_data.get('color_originals')
    has_color_originals = color_originals is not None and len(color_originals) == n_images

    st.caption(f"Reconstruction comparison (Epoch {epoch + 1})")
    if has_color_originals:
        row_desc = "Row 1: Original (color) | Row 2: Grayscale input | Row 3: Reconstruction @ endogenous hue"
        if has_fixed_hue:
            row_desc += f" | Row 4: Recon @ target hue ({target_hue_val:.3f})"
        st.caption(row_desc)
    elif has_fixed_hue:
        st.caption(f"Row 1: Original | Row 2: Recon @ endogenous hue | Row 3: Recon @ target hue ({target_hue_val:.3f})")
    else:
        st.caption("Row 1: Original | Row 2: Reconstruction @ endogenous hue")

    # Create columns for images
    cols = st.columns(n_images)

    for i, col in enumerate(cols):
        with col:
            # === ROW 1 (grayscale mode): Original COLOR image ===
            if has_color_originals:
                st.image(color_originals[i], caption="Original (color)", use_container_width=True)

            # === ROW 1 or 2: Original image (grayscale input when grayscale, else color original) ===
            orig_hsv = original_hsv[i] if i < len(original_hsv) else {}
            enc_cond = encoder_cond[i] if i < len(encoder_cond) else None

            orig_caption = "Grayscale input" if has_color_originals else "Original"
            if orig_hsv:
                hsv_str = f"H:{orig_hsv.get('hue', 0):.3f} S:{orig_hsv.get('saturation', 0):.3f} V:{orig_hsv.get('value', 0):.3f}"
                orig_caption = f"{'Gray' if has_color_originals else 'Orig'} {hsv_str}"
            if enc_cond is not None:
                if isinstance(enc_cond, (list, np.ndarray)):
                    cond_val = enc_cond[0] if len(enc_cond) > 0 else 0
                elif isinstance(enc_cond, (int, float, np.number)):
                    cond_val = float(enc_cond)
                else:
                    cond_val = 0
                orig_caption += f"\nEnc: {cond_val:.3f}"

            st.image(originals[i], caption=orig_caption, use_container_width=True)

            # === Next ROW: Reconstruction at endogenous hue ===
            rec_hsv = reconstructed_hsv[i] if i < len(reconstructed_hsv) else {}
            dec_cond = decoder_cond[i] if i < len(decoder_cond) else None

            rec_caption = "Recon (endogenous)"
            if rec_hsv:
                rec_caption = f"Rec H:{rec_hsv.get('hue', 0):.3f} S:{rec_hsv.get('saturation', 0):.3f} V:{rec_hsv.get('value', 0):.3f}"
            if dec_cond is not None:
                if isinstance(dec_cond, (list, np.ndarray)):
                    cond_val = dec_cond[0] if len(dec_cond) > 0 else 0
                elif isinstance(dec_cond, (int, float, np.number)):
                    cond_val = float(dec_cond)
                else:
                    cond_val = 0
                rec_caption += f"\nDec: {cond_val:.3f}"

            st.image(reconstructions[i], caption=rec_caption, use_container_width=True)

            # === ROW 3: Reconstruction at fixed target hue (if available) ===
            if has_fixed_hue:
                fixed_images = fixed_hue_data.get('images')
                fixed_reconstructed_hsv = fixed_hue_data.get('reconstructed_hsv', [])
                fixed_decoder_cond = fixed_hue_data.get('decoder_cond', [])

                if fixed_images is not None and len(fixed_images) > 1:
                    fixed_recons = fixed_images[1]

                    if i < len(fixed_recons):
                        fixed_rec_hsv = fixed_reconstructed_hsv[i] if i < len(fixed_reconstructed_hsv) else {}
                        fixed_dec_cond = fixed_decoder_cond[i] if i < len(fixed_decoder_cond) else None

                        fixed_caption = "Recon (target hue)"
                        if fixed_rec_hsv:
                            fixed_caption = f"Rec H:{fixed_rec_hsv.get('hue', 0):.3f} S:{fixed_rec_hsv.get('saturation', 0):.3f} V:{fixed_rec_hsv.get('value', 0):.3f}"
                        if fixed_dec_cond is not None:
                            if isinstance(fixed_dec_cond, (list, np.ndarray)):
                                cond_val = fixed_dec_cond[0] if len(fixed_dec_cond) > 0 else 0
                            elif isinstance(fixed_dec_cond, (int, float, np.number)):
                                cond_val = float(fixed_dec_cond)
                            else:
                                cond_val = 0
                            fixed_caption += f"\nDec: {cond_val:.3f}"

                        st.image(fixed_recons[i], caption=fixed_caption, use_container_width=True)

    # Download button for this reconstruction set
    _render_reconstruction_download(originals, reconstructions, epoch, key_prefix)

    # FiLM-spectrum diagnostic figure (conditional strategies only)
    spectrum = recon_data.get('spectrum')
    if spectrum and spectrum.get('png_bytes'):
        _render_spectrum_figure(spectrum, epoch, key_prefix)


def _render_spectrum_figure(
    spectrum: Dict[str, Any],
    epoch: int,
    key_prefix: str,
):
    """Display the FiLM-spectrum PNG and a download button.

    Wrapped in try/except because Streamlit re-renders the recon history
    multiple times per script run (the callback fires per epoch and replays
    on rerun), which would otherwise cause StreamlitDuplicateElementKey on
    the download button. A per-script-run counter is appended to the key as
    a second line of defence in case the suppression ever stops working.
    """
    try:
        png_bytes = spectrum['png_bytes']
        idx = spectrum.get('index', 0)

        # Per-script-run nonce so re-renders within one run get unique keys.
        nonce = st.session_state.get("_spectrum_render_nonce", 0) + 1
        st.session_state["_spectrum_render_nonce"] = nonce

        st.markdown("**FiLM Spectrum (decoder hue sweep)**")
        st.caption(
            f"Validation image #{idx}. Holds the latent fixed and varies one HSV "
            f"channel of the decoder condition at a time — checks whether FiLM "
            f"layers are actually responsive."
        )
        st.image(png_bytes, use_container_width=True)
        st.download_button(
            label="Download spectrum (PNG)",
            data=png_bytes,
            file_name=f"spectrum_epoch_{epoch + 1}_image_{idx}.png",
            mime="image/png",
            key=f"{key_prefix}_spectrum_dl_e{epoch}_n{nonce}",
        )
    except Exception:
        # Same defensive pattern as _render_reconstruction_download — a duplicate
        # render of the same epoch is harmless to the user; we just don't draw
        # the second copy.
        pass


def _render_reconstruction_download(
    originals: np.ndarray,
    reconstructions: np.ndarray,
    epoch: int,
    key_prefix: str,
):
    """Create a downloadable composite PNG of originals + reconstructions."""
    try:
        import matplotlib
        matplotlib.use('Agg')
        import matplotlib.pyplot as plt

        n_images = len(originals)
        fig, axes = plt.subplots(2, n_images, figsize=(3 * n_images, 6))
        if n_images == 1:
            axes = axes.reshape(2, 1)

        for i in range(n_images):
            axes[0, i].imshow(originals[i])
            axes[0, i].set_title("Original", fontsize=8)
            axes[0, i].axis('off')

            axes[1, i].imshow(reconstructions[i])
            axes[1, i].set_title("Reconstructed", fontsize=8)
            axes[1, i].axis('off')

        fig.suptitle(f"Epoch {epoch + 1}", fontsize=10)
        fig.tight_layout()

        buf = io.BytesIO()
        fig.savefig(buf, format='png', dpi=150, bbox_inches='tight')
        plt.close(fig)
        buf.seek(0)

        st.download_button(
            label="Download reconstruction (PNG)",
            data=buf.getvalue(),
            file_name=f"reconstruction_epoch_{epoch + 1}.png",
            mime="image/png",
            key=f"{key_prefix}_download_e{epoch}",
        )
    except Exception:
        pass  # matplotlib not critical — skip silently


def _render_fixed_hue_section(fixed_data: Dict[str, Any], epoch: int):
    """
    Render fixed-hue reconstruction section.

    Args:
        fixed_data: Dict with fixed-hue reconstruction data
        epoch: Current epoch number
    """
    images = fixed_data.get('images')
    if images is None or len(images) == 0:
        return

    # Get target conditioning values - handle both list and scalar
    target_cond = fixed_data.get('target_cond', [])
    if isinstance(target_cond, (list, np.ndarray)) and len(target_cond) > 0:
        target_hue = target_cond[0]
    elif isinstance(target_cond, (int, float, np.number)):
        target_hue = float(target_cond)
    else:
        target_hue = 0.0

    st.markdown("---")
    st.caption(f"**Fixed-Hue Reconstruction** (Target Hue: {target_hue:.3f})")

    # images shape: (2, n_images, H, W, C)
    originals = images[0]
    reconstructions = images[1]
    n_images = len(originals)

    original_hsv = fixed_data.get('original_hsv', [])
    reconstructed_hsv = fixed_data.get('reconstructed_hsv', [])
    encoder_cond = fixed_data.get('encoder_cond', [])
    decoder_cond = fixed_data.get('decoder_cond', [])

    # Create columns for images - show both original and reconstruction
    cols = st.columns(n_images)

    for i, col in enumerate(cols):
        with col:
            # Original HSV info
            orig_hsv = original_hsv[i] if i < len(original_hsv) else {}
            enc_cond = encoder_cond[i] if i < len(encoder_cond) else None

            orig_h = orig_hsv.get('hue', 0) if orig_hsv else 0
            orig_s = orig_hsv.get('saturation', 0) if orig_hsv else 0
            orig_v = orig_hsv.get('value', 0) if orig_hsv else 0

            # Handle enc_cond being a scalar or list
            if enc_cond is not None:
                if isinstance(enc_cond, (list, np.ndarray)) and len(enc_cond) > 0:
                    enc_val = enc_cond[0]
                elif isinstance(enc_cond, (int, float, np.number)):
                    enc_val = float(enc_cond)
                else:
                    enc_val = 0
            else:
                enc_val = 0

            # Original image caption
            orig_caption = f"Orig H:{orig_h:.3f} S:{orig_s:.3f} V:{orig_v:.3f}\nEnc: {enc_val:.3f}"
            st.image(originals[i], caption=orig_caption, use_container_width=True)

            # Reconstructed HSV info
            rec_hsv = reconstructed_hsv[i] if i < len(reconstructed_hsv) else {}
            dec_cond = decoder_cond[i] if i < len(decoder_cond) else None

            rec_h = rec_hsv.get('hue', 0) if rec_hsv else 0
            rec_s = rec_hsv.get('saturation', 0) if rec_hsv else 0
            rec_v = rec_hsv.get('value', 0) if rec_hsv else 0

            # Handle dec_cond being a scalar or list
            if dec_cond is not None:
                if isinstance(dec_cond, (list, np.ndarray)) and len(dec_cond) > 0:
                    dec_val = dec_cond[0]
                elif isinstance(dec_cond, (int, float, np.number)):
                    dec_val = float(dec_cond)
                else:
                    dec_val = 0
            else:
                dec_val = 0

            # Reconstruction caption: show target and result
            rec_caption = f"Target H:{dec_val:.3f} -> Result H:{rec_h:.3f}\nS:{rec_s:.3f} V:{rec_v:.3f}"
            st.image(reconstructions[i], caption=rec_caption, use_container_width=True)


def _generate_fixed_hue_reconstructions(fixed_hue: float, fixed_sat: float, fixed_val: float):
    """
    Generate reconstructions using fixed decoder conditioning values.

    Args:
        fixed_hue: Fixed hue value (0-1 scale)
        fixed_sat: Fixed saturation value (0-1 scale)
        fixed_val: Fixed value/brightness (0-1 scale)
    """
    trainer = TrainingStateManager.get_state("trainer")
    if trainer is None:
        st.error("Trainer not available. Training may not have completed.")
        return

    try:
        with st.spinner("Generating fixed-hue reconstructions..."):
            # Get the conditional variables from config
            config_dict = TrainingStateManager.get_state("config_dict")
            cond_vars = config_dict.get(
                "conditional_variables",
                ["average_hue", "average_saturation", "average_value"],
            )

            # Build fixed condition tensor based on selected variables
            fixed_cond_values = []
            for var in cond_vars:
                if var == "average_hue":
                    fixed_cond_values.append(fixed_hue)
                elif var == "average_saturation":
                    fixed_cond_values.append(fixed_sat)
                elif var == "average_value":
                    fixed_cond_values.append(fixed_val)

            fixed_cond = torch.tensor([fixed_cond_values], dtype=torch.float32)

            # Generate reconstructions with fixed decoder conditioning
            recon_data = _generate_reconstructions_with_fixed_cond(trainer, fixed_cond)

            # Store in session state
            st.session_state["fixed_hue_reconstructions"] = {
                'hue': fixed_hue,
                'sat': fixed_sat,
                'val': fixed_val,
                'data': recon_data,
            }

            st.success(f"Generated reconstructions at fixed Hue={fixed_hue:.2f}")
            st.rerun()

    except Exception as e:
        logger.exception("Failed to generate fixed-hue reconstructions")
        st.error(f"Error: {e}")


def _generate_reconstructions_with_fixed_cond(
    trainer: Any,
    fixed_decoder_cond: torch.Tensor,
) -> Dict[str, Any]:
    """
    Generate reconstructions using fixed decoder conditioning.

    Args:
        trainer: StreamlitTrainer instance
        fixed_decoder_cond: Fixed conditioning tensor for decoder (shape: [1, cond_dim])

    Returns:
        Dict with images and HSV data (same format as generate_reconstructions)
    """
    from candescence.tlv.utilities import convert_rgb_transformed_2_rgb

    trainer.vae.eval()
    device = trainer.device

    originals = []
    reconstructions = []
    original_hsv_list = []
    reconstructed_hsv_list = []
    encoder_cond_list = []
    decoder_cond_list = []

    fixed_decoder_cond = fixed_decoder_cond.to(device)

    with torch.no_grad():
        for idx in trainer._fixed_indices:
            item = trainer.validation_dataloader.dataset[idx]

            # Get image and encoder conditioning
            if len(item) == 4:
                img, encoder_cond, _, _ = item
            elif len(item) == 3:
                img, encoder_cond, _ = item
            else:
                img = item[0]
                encoder_cond = None

            # Store encoder conditioning (actual image's HSV). For Strategy
            # 15 encoder_cond is a dict; serialize per-variable.
            if encoder_cond is not None:
                if isinstance(encoder_cond, dict):
                    encoder_cond_list.append(
                        {k: v.detach().cpu().numpy().tolist()
                         for k, v in encoder_cond.items()}
                    )
                else:
                    encoder_cond_list.append(encoder_cond.cpu().numpy().tolist())
            else:
                encoder_cond_list.append(None)

            # Use FIXED decoder conditioning
            decoder_cond_list.append(fixed_decoder_cond.cpu().numpy().tolist()[0])

            img = img.unsqueeze(0).to(device).float()

            if encoder_cond is not None:
                if isinstance(encoder_cond, dict):
                    encoder_cond = {
                        k: (v.unsqueeze(0) if v.dim() == 1 else v).to(device).float()
                        for k, v in encoder_cond.items()
                    }
                    # Strategy 15: fixed_decoder_cond is an HSV tensor —
                    # overlay only the 'hue' entry; day/media keep their
                    # per-sample values so identity info is preserved.
                    decoder_cond = {**encoder_cond, 'hue': fixed_decoder_cond}
                    rec_img, *_ = trainer.vae(img, [encoder_cond, decoder_cond])
                else:
                    if encoder_cond.dim() == 1:
                        encoder_cond = encoder_cond.unsqueeze(0)
                    encoder_cond = encoder_cond.to(device).float()
                    rec_img, *_ = trainer.vae(img, [encoder_cond, fixed_decoder_cond])
            else:
                rec_img, *_ = trainer.vae(img)

            # Convert to displayable format
            orig = convert_rgb_transformed_2_rgb(img.cpu().squeeze())
            rec = convert_rgb_transformed_2_rgb(rec_img.cpu().squeeze())

            originals.append(orig)
            reconstructions.append(rec)

            # Compute HSV values
            orig_hsv = trainer._compute_image_hsv(orig)
            rec_hsv = trainer._compute_image_hsv(rec)
            original_hsv_list.append(orig_hsv)
            reconstructed_hsv_list.append(rec_hsv)

    # Stack into array
    originals_arr = np.stack(originals, axis=0)
    reconstructions_arr = np.stack(reconstructions, axis=0)
    images = np.stack([originals_arr, reconstructions_arr], axis=0)

    return {
        'images': images,
        'original_hsv': original_hsv_list,
        'reconstructed_hsv': reconstructed_hsv_list,
        'encoder_cond': encoder_cond_list,
        'decoder_cond': decoder_cond_list,
    }


def _render_step_3_summary():
    """Render Step 3: Post-training summary and save."""
    st.header("Step 3: Training Complete")

    state = TrainingStateManager.to_training_state()
    config_dict = TrainingStateManager.get_state("config_dict")

    # Summary metrics
    st.subheader("Training Summary")

    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.metric("Epochs Completed", state.current_epoch)
    with col2:
        st.metric("Best Epoch", state.best_epoch)
    with col3:
        st.metric("Best Val Loss", f"{state.best_val_loss:.4f}")
    with col4:
        st.metric("Total Time", state.format_elapsed())

    # Final loss values
    if state.loss_history:
        final_metrics = state.loss_history[-1]
        col1, col2 = st.columns(2)
        with col1:
            st.metric("Final Train Loss", f"{final_metrics['train_loss']:.4f}")
        with col2:
            st.metric("Final Val Loss", f"{final_metrics['val_loss']:.4f}")

        # Loss curves with controls
        st.subheader("Training Curves")
        df = pd.DataFrame(state.loss_history)
        _render_loss_chart_with_controls(
            df=df,
            columns=["train_loss", "val_loss"],
            key_prefix="summary_main",
            show_controls=True,
            height=350
        )

        # Loss components
        with st.expander("Loss Components", expanded=True):
            _render_loss_components(df, key_prefix="summary_comp")

    # Reconstruction history (all snapshots from training)
    recon_history = TrainingStateManager.get_state("reconstruction_history")
    if recon_history:
        _render_reconstruction_history(recon_history, key_prefix="summary_recon")
    elif TrainingStateManager.get_state("reconstruction_data") is not None:
        st.subheader("Final Reconstruction Quality")
        _render_reconstruction_grid_with_hsv(
            TrainingStateManager.get_state("reconstruction_data"),
            state.current_epoch - 1,
            key_prefix="summary_recon_single",
        )
    elif state.reconstruction_images is not None:
        st.subheader("Final Reconstruction Quality")
        _render_reconstruction_grid(state.reconstruction_images, state.current_epoch - 1)

    # Tendril loss chart in summary
    _render_tendril_loss_chart("summary")

    st.divider()

    # Fixed-hue inference test
    st.subheader("🎨 Fixed-Hue Inference Test")
    st.markdown("""
    Test reconstructions at a **fixed background hue**. This normalizes all images to the same
    background color, enabling fair morphology comparison across strains with different media colors.
    """)

    with st.expander("Generate Fixed-Hue Reconstructions", expanded=False):
        col1, col2 = st.columns(2)

        with col1:
            fixed_hue = st.slider(
                "Fixed Hue Value",
                min_value=0.0,
                max_value=1.0,
                value=0.5,
                step=0.05,
                help="Hue on 0-1 scale: 0=red, 0.33=green, 0.66=blue, 1=red. Typical agar: ~0.1-0.3",
                key="fixed_hue_slider"
            )
            st.caption("All values are on 0-1 scale")

        with col2:
            fixed_sat = st.slider(
                "Fixed Saturation",
                min_value=0.0,
                max_value=1.0,
                value=0.5,
                step=0.05,
                help="Saturation on 0-1 scale. 0=gray, 1=fully saturated. Leave at 0.5 for neutral.",
                key="fixed_sat_slider"
            )
            fixed_val = st.slider(
                "Fixed Value (Brightness)",
                min_value=0.0,
                max_value=1.0,
                value=0.5,
                step=0.05,
                help="Brightness on 0-1 scale. 0=black, 1=bright. Leave at 0.5 for neutral.",
                key="fixed_val_slider"
            )

        if st.button("Generate Fixed-Hue Reconstructions", key="gen_fixed_hue"):
            _generate_fixed_hue_reconstructions(fixed_hue, fixed_sat, fixed_val)

        # Show cached fixed-hue reconstructions
        fixed_recon = st.session_state.get("fixed_hue_reconstructions")
        if fixed_recon is not None:
            st.markdown("---")
            st.markdown(f"**Reconstructions at fixed Hue={fixed_recon['hue']:.2f}**")
            _render_reconstruction_grid_with_hsv(
                fixed_recon['data'], state.current_epoch - 1,
                key_prefix="summary_fixed_hue",
            )

    st.divider()

    # Model classification
    st.subheader("Model Classification")

    model_type = st.radio(
        "Save this model as:",
        options=["research", "production"],
        format_func=lambda x: "Research Model" if x == "research" else "Production Model",
        help="Research models are for experimental use. Production models are validated and visible in exploration apps.",
        key="save_model_type"
    )

    if model_type == "research":
        st.info("Research models are saved for experimental use and further development.")
    else:
        st.success("Production models are validated and will appear in the latent space explorer and other apps.")

    # Model metadata
    st.subheader("Model Metadata")

    col1, col2 = st.columns(2)
    with col1:
        _arch_name = config_dict.get("architecture", "tendril_vae")
        model_name = st.text_input(
            "Model Name",
            value=f"{_arch_name}_{config_dict.get('experiment_name', 'exp')}",
            help="Human-readable name for this model.",
            key="save_model_name"
        )

    with col2:
        model_version = st.text_input(
            "Version",
            value="1.0.0",
            help="Semantic version string.",
            key="save_model_version"
        )

    _strategy_num = config_dict.get("strategy", 14)
    _arch_display = config_dict.get("architecture", "tendril_vae").replace("_", " ").title()
    model_description = st.text_area(
        "Description",
        value=f"{_arch_display} (strategy {_strategy_num}) trained on day {config_dict.get('restrict_to_day', 2)} images with {config_dict.get('latent_dim', 128)}-dimensional latent space.",
        help="Description of what this model is for.",
        key="save_model_description"
    )

    st.divider()

    # Save button
    col1, col2, col3 = st.columns([1, 2, 1])
    with col2:
        if st.button("Save Model", type="primary", use_container_width=True):
            _save_model(model_type, model_name, model_version, model_description)


def _save_args_and_register_in_zoo(config_dict: Dict[str, Any], factory: Any) -> None:
    """Save args.json and auto-register model in Model Zoo as research."""
    config = TrainingStateManager.get_state("tlv_config")
    model_path = config.models_path / "model.pth"
    if not model_path.exists():
        return
    # Save args.json
    if factory:
        factory._save_arguments()
    # Register in zoo as research
    try:
        from candescence.core.model_zoo import ModelZoo

        zoo = ModelZoo()
        model_id = f"{config_dict['experiment_name']}_{config_dict['save_name']}"
        zoo_strategy = config_dict.get("strategy", 14)
        zoo_architecture = config_dict.get("architecture", "tendril_vae")
        zoo_arch_short = zoo_architecture.replace("_vae", "").replace("_", "-")
        name = f"{zoo_arch_short} (strategy {zoo_strategy})"
        description = (
            f"Auto-registered from TLV training. "
            f"Trained on day {config_dict.get('restrict_to_day', 2)} images, "
            f"{config_dict.get('latent_dim', 128)}-dim latent."
        )
        zoo.register(
            model_id=model_id,
            name=name,
            project="tlv",
            model_type="research",
            version="1.0",
            architecture=zoo_architecture,
            checkpoint="model.pth",
            config_file="args.json",
            path=model_path.parent,
            description=description,
            tags=[zoo_arch_short, "vae", f"strategy_{zoo_strategy}"],
            training_config={
                "strategy": zoo_strategy,
                "latent_dim": config_dict.get("latent_dim", 128),
                "conditional_variables": config_dict.get("conditional_variables"),
            },
        )
        logger.info(f"Model auto-registered in zoo: {model_id}")
    except Exception as e:
        logger.exception("Failed to auto-register model in zoo")


def _save_model(model_type: str, name: str, version: str, description: str):
    """Save the trained model."""
    config = TrainingStateManager.get_state("tlv_config")
    model_path = config.models_path / "model.pth"

    if not model_path.exists():
        st.error("Model file not found. Training may not have completed successfully.")
        return

    # Save arguments
    factory = TrainingStateManager.get_state("factory")
    if factory:
        factory._save_arguments()

    # Register in unified model zoo
    if model_type == "production":
        try:
            from candescence.core.model_zoo import ModelZoo

            zoo = ModelZoo()
            config_dict = TrainingStateManager.get_state("config_dict")

            model_id = f"{config_dict['experiment_name']}_{config_dict['save_name']}"

            zoo_strategy = config_dict.get("strategy", 14)
            zoo_architecture = config_dict.get("architecture", "tendril_vae")
            zoo_arch_short = zoo_architecture.replace("_vae", "").replace("_", "-")

            zoo.register(
                model_id=model_id,
                name=name,
                project="tlv",
                model_type="production",
                version=version,
                architecture=zoo_architecture,
                checkpoint="model.pth",
                config_file="args.json",
                path=model_path.parent,
                description=description,
                tags=[zoo_arch_short, "vae", f"strategy_{zoo_strategy}"],
                training_config={
                    "strategy": zoo_strategy,
                    "latent_dim": config_dict.get("latent_dim", 128),
                    "conditional_variables": config_dict.get("conditional_variables"),
                },
            )

            st.success(f"Model registered in zoo as production: `{model_id}`")
            TrainingStateManager.add_log_message(f"Model registered in model zoo: {model_id}")

        except Exception as e:
            logger.exception("Failed to register production model in zoo")
            st.warning(f"Model saved but zoo registration failed: {e}")
    else:
        st.success(f"Research model saved to: `{model_path}`")

    st.balloons()

    # Show next steps
    st.divider()
    st.subheader("Next Steps")

    if model_type == "production":
        st.markdown("""
        Your model is now available in:
        - **Latent Space Explorer**: Explore the learned latent space
        - **Other Candescence apps**: This model will appear in model selection dropdowns

        To use this model:
        ```python
        from candescence.core.model_zoo import ModelZoo
        zoo = ModelZoo()
        entry = zoo.get("your_model_id")
        print(entry.get_checkpoint_path())
        ```
        """)
    else:
        st.markdown(f"""
        Your research model is saved at:
        ```
        {model_path}
        ```

        To load and use:
        ```python
        from candescence.interface import load_tlv_model
        model = load_tlv_model("{model_path}")
        ```
        """)


def _render_error_state():
    """Render error state."""
    st.error("Training Error")

    error_msg = TrainingStateManager.get_state("error_message")
    if error_msg:
        st.exception(Exception(error_msg))

    if st.button("Start New Training"):
        TrainingStateManager.reset_session_state()
        st.rerun()


# =============================================================================
# Entry point
# =============================================================================

if __name__ == "__main__":
    main()
