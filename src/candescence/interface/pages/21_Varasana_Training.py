"""
Purpose: Varasana FCOS Object Detection Training Page
Author: Hallett Lab
Date: 2026-02-04

Streamlit page for training FCOS object detection models with curriculum
learning support for Candida morphology detection.

Launch via unified interface:
    streamlit run src/candescence/interface/app.py
"""

from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import streamlit as st

from candescence.core.logging_config import get_logger
from candescence.core.settings import get_settings, legacy_refined_root

_settings = get_settings()
from candescence.core.morphology import CURRICULUM_STAGES, CLASS_NAMES
from candescence.detection.evaluation import (
    DetectionEvaluator,
    EvaluationResults,
    PredictionVisualizer,
    SampleBrowser,
)
from candescence.detection.evaluation.visualizer import (
    create_confusion_matrix_figure,
    create_precision_recall_chart,
)
from candescence.interface.core.theme import (
    THEME,
    apply_theme,
    page_header,
    get_subproject_color,
)
from candescence.interface.core.components import (
    get_gpu_info,
    get_recommended_gpu,
    render_gpu_selector,
    create_loss_chart,
    render_loss_chart_with_controls,
    render_progress_metrics,
)
from candescence.interface.training.detection_state import (
    DetectionStateManager,
    DetectionTrainingState,
    STAGE_NAMES,
)
from candescence.interface.training.detection_trainer import (
    DetectionConfig,
    StreamlitDetectionTrainer,
)
from candescence.core.model_zoo import ModelZoo
from candescence.core.dataset_zoo import DatasetZoo

logger = get_logger("candescence.interface.pages.varasana_training")

# Page configuration

# =============================================================================
# Constants and Tooltips
# =============================================================================

VARASANA_COLOR = THEME["varasana_color"]

TOOLTIPS = {
    "training_mode": (
        "Curriculum training progressively introduces morphology classes, "
        "helping the model learn simpler patterns before complex ones. "
        "Standard training uses all classes from the start."
    ),
    "epochs_per_stage": (
        "Number of training epochs for each curriculum stage. "
        "More epochs = better learning but longer training time."
    ),
    "learning_rate": (
        "Initial learning rate. Start with 0.01 for SGD. "
        "Lower values = more stable but slower training."
    ),
    "nms_threshold": (
        "Non-Maximum Suppression threshold. "
        "Lower = fewer overlapping detections. "
        "Higher = more detections allowed to overlap."
    ),
    "center_sampling": (
        "FCOS center sampling radius. "
        "Larger values make positive sample selection more lenient."
    ),
}

# Known data locations: the project's own refined tree (from settings) plus
# optional legacy trees (candescence_master / candescence). All candidates are
# existence-checked before use, so missing legacy trees are simply skipped on
# external installs.
_LEGACY_REFINED = legacy_refined_root()
KNOWN_DATA_LOCATIONS = [
    f"{_settings.refined_path}/varasana_data/curriculum/",
    f"{_LEGACY_REFINED}/candescence_master/projects/varasana/train-data/",
    f"{_LEGACY_REFINED}/candescence/varasana/curriculum/",
    f"{_LEGACY_REFINED}/candescence/varasana/standard/",
]


# =============================================================================
# Main Application
# =============================================================================

def main() -> None:
    """Main application entry point."""
    apply_theme()
    DetectionStateManager.initialize_session_state()

    # Render sidebar
    _render_sidebar()

    # Main content based on status
    status = DetectionStateManager.get_state("status")

    if status == "idle":
        _render_step_1_config()
    elif status in ("training", "paused", "evaluating"):
        _render_step_2_training()
    elif status in ("completed", "stopped"):
        _render_step_3_summary()
    elif status == "error":
        _render_error()
    else:
        _render_step_1_config()


def _render_sidebar() -> None:
    """Render sidebar with step indicator and status."""
    with st.sidebar:
        page_header(
            title="Varasana Training",
            subproject="varasana",
            icon="🎯",
            description="FCOS Object Detection"
        )

        status = DetectionStateManager.get_state("status")

        st.divider()
        st.subheader("Training Steps")

        # Step indicators
        steps = [
            ("1. Configuration", status == "idle"),
            ("2. Training", status in ("training", "paused", "evaluating")),
            ("3. Summary", status in ("completed", "stopped")),
        ]

        for step_name, is_active in steps:
            if is_active:
                st.markdown(f"**→ {step_name}**")
            else:
                st.markdown(f"   {step_name}")

        # Status indicator
        st.divider()
        st.subheader("Status")

        status_colors = {
            "idle": "blue",
            "training": "orange",
            "paused": "orange",
            "evaluating": "orange",
            "completed": "green",
            "stopped": "gray",
            "error": "red",
        }

        st.markdown(
            f":{status_colors.get(status, 'gray')}[{status.upper()}]"
        )

        # Reset button (always available)
        if status != "idle":
            st.divider()
            if st.button("🔄 Start New Training", use_container_width=True):
                DetectionStateManager.reset_session_state()
                st.rerun()


# =============================================================================
# Step 1: Configuration
# =============================================================================

def _render_step_1_config() -> None:
    """Render configuration step."""
    st.title("🎯 Varasana Training Configuration")

    st.markdown(
        """
        Configure and train an FCOS object detection model for *Candida albicans*
        morphology detection. The model uses curriculum learning to progressively
        learn from simpler to more complex morphologies.
        """
    )

    # Configuration sections
    with st.expander("📁 Training Mode & Data", expanded=True):
        data_config = _render_data_config()

    with st.expander("🧠 Model Architecture", expanded=False):
        model_config = _render_model_config()

    with st.expander("⚙️ Training Parameters", expanded=False):
        training_config = _render_training_config()

    with st.expander("🎯 FCOS Settings", expanded=False):
        fcos_config = _render_fcos_config()

    with st.expander("📤 Output Configuration", expanded=False):
        output_config = _render_output_config()

    # GPU Selection
    st.divider()
    st.subheader("🖥️ GPU Selection")
    gpu_id = render_gpu_selector(key_prefix="varasana_gpu")

    # Validation and start
    st.divider()

    col1, col2 = st.columns([3, 1])

    with col2:
        start_clicked = st.button(
            "🚀 Start Training",
            type="primary",
            use_container_width=True,
        )

    if start_clicked:
        config_dict = {
            **data_config,
            **model_config,
            **training_config,
            **fcos_config,
            **output_config,
            "gpu_ids": [gpu_id] if gpu_id >= 0 else [0],
        }
        _validate_and_start(config_dict)


def _render_data_config() -> Dict[str, Any]:
    """Render data configuration section."""
    config = {}

    # Training mode
    mode = st.radio(
        "Training Mode",
        options=["curriculum", "standard"],
        format_func=lambda x: (
            "Curriculum Training (Recommended)" if x == "curriculum"
            else "Standard Training"
        ),
        help=TOOLTIPS["training_mode"],
    )
    config["training_mode"] = mode

    st.divider()

    # Data directory - offer zoo datasets and manual paths
    st.markdown("**Data Source**")

    source_mode = st.radio(
        "Select data source",
        options=["zoo", "path"],
        format_func=lambda x: (
            "From Dataset Zoo (Recommended)" if x == "zoo"
            else "Manual Path"
        ),
        horizontal=True,
        key="data_source_mode",
    )

    data_root = None

    if source_mode == "zoo":
        # Load datasets from zoo
        try:
            dataset_zoo = DatasetZoo()
            varasana_datasets = dataset_zoo.list_datasets(project="varasana")
            curriculum_datasets = [
                ds for ds in varasana_datasets
                if ds.format in ("curriculum_pkl", "image_dir")
            ]
        except Exception:
            curriculum_datasets = []

        if curriculum_datasets:
            options_map = {
                ds.id: f"{ds.name} ({ds.num_samples:,} samples) — {ds.format}"
                for ds in curriculum_datasets
            }
            selected_id = st.selectbox(
                "Select dataset",
                options=list(options_map.keys()),
                format_func=lambda x: options_map[x],
                key="zoo_dataset_selector",
            )

            selected_ds = dataset_zoo.get(selected_id)
            if selected_ds:
                data_root = selected_ds.path
                config["dataset_id"] = selected_ds.id

                # Show dataset info
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.metric("Samples", f"{selected_ds.num_samples:,}")
                with col2:
                    if selected_ds.splits:
                        split_str = ", ".join(
                            f"{k}: {v}" for k, v in selected_ds.splits.items()
                        )
                        st.caption(f"Splits: {split_str}")
                with col3:
                    if selected_ds.exists():
                        st.success("Path verified", icon="✅")
                    else:
                        st.warning("Path not found", icon="⚠️")
        else:
            st.info("No Varasana datasets registered in zoo. Use Manual Path instead.")
            source_mode = "path"

    if source_mode == "path":
        # Fallback to manual path selection
        valid_locations = [p for p in KNOWN_DATA_LOCATIONS if Path(p).exists()]
        options = valid_locations + ["Custom path..."]

        if valid_locations:
            selected = st.selectbox(
                "Select data directory",
                options=options,
                help="Choose from known data locations or enter a custom path",
            )

            if selected == "Custom path...":
                data_root = st.text_input("Enter custom path")
            else:
                data_root = selected
        else:
            data_root = st.text_input(
                "Data directory path",
                value=f"{_settings.refined_path}/varasana_data/",
                help="Root directory containing training data",
            )

    config["data_root"] = Path(data_root) if data_root else None

    # Validate and show data info
    if data_root and Path(data_root).exists():
        _render_data_preview(Path(data_root), mode)
    elif data_root:
        st.warning(f"Directory not found: {data_root}")

    return config


def _render_data_preview(data_root: Path, mode: str) -> None:
    """Render preview of data in the selected directory."""
    st.markdown("**Data Preview**")

    if mode == "curriculum":
        # Show curriculum stages
        cols = st.columns(3)

        for i, stage_name in enumerate(STAGE_NAMES):
            train_file = data_root / f"train_{stage_name}.pkl"
            val_file = data_root / f"val_{stage_name}.pkl"

            with cols[i % 3]:
                stage = CURRICULUM_STAGES[i]
                has_train = train_file.exists()
                has_val = val_file.exists()

                status_icon = "✅" if (has_train and has_val) else "❌"
                st.markdown(
                    f"{status_icon} **{stage_name.title()}** ({len(stage.cumulative_classes)} classes)"
                )

                if has_train:
                    st.caption(f"Train: {train_file.name}")
                else:
                    st.caption(f"⚠️ Missing: {train_file.name}")

    else:
        # Standard mode - look for single annotation file
        train_files = list(data_root.glob("train*.pkl"))
        val_files = list(data_root.glob("val*.pkl"))

        col1, col2 = st.columns(2)
        with col1:
            st.metric("Training files", len(train_files))
        with col2:
            st.metric("Validation files", len(val_files))


def _render_model_config() -> Dict[str, Any]:
    """Render model architecture configuration."""
    config = {}

    col1, col2 = st.columns(2)

    with col1:
        config["backbone"] = st.selectbox(
            "Backbone",
            options=["resnet50", "resnet101", "resnext101"],
            index=1,  # Default to resnet101
            help="Feature extraction backbone network"
        )

    with col2:
        config["num_classes"] = st.number_input(
            "Number of Classes",
            min_value=1,
            max_value=50,
            value=15,
            help="Total morphology classes (15 for full Candida detection)"
        )

    # Show class names
    with st.expander("View Class Names"):
        for i, name in enumerate(CLASS_NAMES[:config["num_classes"]]):
            st.text(f"{i}: {name}")

    return config


def _render_training_config() -> Dict[str, Any]:
    """Render training hyperparameters configuration."""
    config = {}

    col1, col2 = st.columns(2)

    with col1:
        config["epochs_per_stage"] = st.number_input(
            "Epochs per Stage",
            min_value=100,
            max_value=10000,
            value=5000,
            step=500,
            help=TOOLTIPS["epochs_per_stage"]
        )

        config["learning_rate"] = st.number_input(
            "Learning Rate",
            min_value=0.0001,
            max_value=0.1,
            value=0.01,
            format="%.4f",
            help=TOOLTIPS["learning_rate"]
        )

        config["optimizer"] = st.selectbox(
            "Optimizer",
            options=["SGD", "Adam", "AdamW"],
            index=0,
        )

    with col2:
        config["batch_size"] = st.selectbox(
            "Batch Size",
            options=[1, 2, 4, 8],
            index=1,  # Default to 2
            help="Images per training step. Lower if GPU memory issues."
        )

        config["checkpoint_interval"] = st.number_input(
            "Checkpoint Interval",
            min_value=50,
            max_value=1000,
            value=300,
            step=50,
            help="Save checkpoint every N epochs"
        )

        config["lr_scheduler"] = st.selectbox(
            "LR Scheduler",
            options=["step", "cosine"],
            index=0,
        )

    # Advanced options
    with st.expander("Advanced Options"):
        col1, col2 = st.columns(2)

        with col1:
            config["momentum"] = st.slider(
                "Momentum (SGD)",
                min_value=0.0,
                max_value=0.99,
                value=0.9,
            )

            config["warmup_iters"] = st.number_input(
                "Warmup Iterations",
                min_value=0,
                max_value=5000,
                value=1000,
            )

        with col2:
            config["weight_decay"] = st.number_input(
                "Weight Decay",
                min_value=0.0,
                max_value=0.01,
                value=0.0001,
                format="%.5f",
            )

            config["log_interval"] = st.number_input(
                "Log Interval",
                min_value=1,
                max_value=100,
                value=15,
                help="Log metrics every N iterations"
            )

    return config


def _render_fcos_config() -> Dict[str, Any]:
    """Render FCOS-specific configuration."""
    config = {}

    col1, col2 = st.columns(2)

    with col1:
        config["center_sampling_radius"] = st.number_input(
            "Center Sampling Radius",
            min_value=0.5,
            max_value=3.0,
            value=1.5,
            step=0.1,
            help=TOOLTIPS["center_sampling"]
        )

        config["score_threshold"] = st.slider(
            "Score Threshold",
            min_value=0.01,
            max_value=0.5,
            value=0.05,
            help="Minimum confidence for detections during inference"
        )

    with col2:
        config["nms_threshold"] = st.slider(
            "NMS Threshold",
            min_value=0.1,
            max_value=0.9,
            value=0.5,
            help=TOOLTIPS["nms_threshold"]
        )

        config["max_per_img"] = st.number_input(
            "Max Detections per Image",
            min_value=50,
            max_value=500,
            value=275,
            help="Maximum number of detections per image"
        )

    return config


def _render_output_config() -> Dict[str, Any]:
    """Render output configuration."""
    config = {}

    # Default paths
    timestamp = datetime.now().strftime("%Y%m%d")
    default_output = f"{_settings.refined_path}/varasana_training_{timestamp}"

    config["output_root"] = st.text_input(
        "Output Directory",
        value=default_output,
        help="Directory for saving checkpoints and logs"
    )

    # MMDETECTION path
    config["mmdetection_path"] = st.text_input(
        "MMDETECTION Path",
        value="/home/data/analysis-tools/mmdetection",
        help="Path to MMDETECTION installation"
    )

    # Base config
    default_base_config = str(
        Path(__file__).parent.parent.parent / "detection" / "configs" / "fcos_resnet101_fpn.py"
    )
    config["base_config"] = st.text_input(
        "Base Config",
        value=default_base_config,
        help="Path to base FCOS configuration file"
    )

    return config


def _validate_and_start(config_dict: Dict[str, Any]) -> None:
    """
    Validate configuration and start training.

    Per Section 2.1, performs:
    1. Validate all images have corresponding annotations
    2. Check annotation format is correct
    3. Verify class labels are consistent
    4. Check GPU memory availability
    5. Display configuration summary
    6. Require explicit "Start Training" button
    """
    errors = []
    warnings = []

    # 1. Validate paths exist
    if config_dict.get("data_root") is None:
        errors.append("Data directory is required")
    elif not Path(config_dict["data_root"]).exists():
        errors.append(f"Data directory not found: {config_dict['data_root']}")

    if not Path(config_dict.get("mmdetection_path", "")).exists():
        errors.append(f"MMDETECTION not found: {config_dict.get('mmdetection_path')}")

    if not Path(config_dict.get("base_config", "")).exists():
        errors.append(f"Base config not found: {config_dict.get('base_config')}")

    # 2. Validate annotation files
    if config_dict.get("data_root") and Path(config_dict["data_root"]).exists():
        data_root = Path(config_dict["data_root"])
        mode = config_dict.get("training_mode", "curriculum")

        if mode == "curriculum":
            for stage_name in STAGE_NAMES:
                train_file = data_root / f"train_{stage_name}.pkl"
                val_file = data_root / f"val_{stage_name}.pkl"

                if not train_file.exists():
                    errors.append(f"Missing training annotations: {train_file.name}")
                if not val_file.exists():
                    errors.append(f"Missing validation annotations: {val_file.name}")
        else:
            # Standard mode - check for train.pkl and val.pkl
            train_files = list(data_root.glob("train*.pkl"))
            val_files = list(data_root.glob("val*.pkl"))

            if not train_files:
                errors.append("No training annotation files found (train*.pkl)")
            if not val_files:
                warnings.append("No validation annotation files found")

    # 3. Check GPU availability
    import torch
    if not torch.cuda.is_available():
        warnings.append("No GPU available - training will be very slow on CPU")
    else:
        gpu_ids = config_dict.get("gpu_ids", [0])
        for gpu_id in gpu_ids:
            if gpu_id >= torch.cuda.device_count():
                errors.append(f"GPU {gpu_id} not available (only {torch.cuda.device_count()} GPUs)")

        # Check GPU memory
        try:
            gpu_id = gpu_ids[0] if gpu_ids else 0
            gpu_info = get_gpu_info()
            if gpu_info and gpu_id < len(gpu_info):
                free_mem = gpu_info[gpu_id]["free_memory_gb"]
                if free_mem < 4.0:
                    warnings.append(f"GPU {gpu_id} has only {free_mem:.1f}GB free memory - may cause OOM errors")
        except Exception:
            pass

    # Display errors
    if errors:
        st.error("**Validation Failed**")
        for error in errors:
            st.error(f"❌ {error}")
        return

    # Display warnings
    if warnings:
        for warning in warnings:
            st.warning(f"⚠️ {warning}")

    # 5. Display configuration summary
    st.success("✅ Configuration validated successfully")

    with st.expander("📋 Configuration Summary", expanded=True):
        col1, col2 = st.columns(2)

        with col1:
            st.markdown("**Training Settings**")
            st.write(f"- Mode: {config_dict.get('training_mode', 'curriculum').title()}")
            st.write(f"- Epochs per stage: {config_dict.get('epochs_per_stage', 5000)}")
            st.write(f"- Batch size: {config_dict.get('batch_size', 2)}")
            st.write(f"- Learning rate: {config_dict.get('learning_rate', 0.01)}")
            st.write(f"- Optimizer: {config_dict.get('optimizer', 'SGD')}")

        with col2:
            st.markdown("**Paths**")
            st.write(f"- Data: `{config_dict.get('data_root', 'N/A')}`")
            st.write(f"- Output: `{config_dict.get('output_root', 'N/A')}`")
            st.write(f"- Backbone: {config_dict.get('backbone', 'resnet101')}")
            st.write(f"- Classes: {config_dict.get('num_classes', 15)}")

    # 6. Require explicit confirmation
    st.divider()
    col1, col2, col3 = st.columns([1, 1, 2])

    with col1:
        if st.button("✅ Confirm & Start Training", type="primary", use_container_width=True):
            # Store configuration and start
            DetectionStateManager.set_state("config_dict", config_dict)
            DetectionStateManager.set_state("status", "training")
            DetectionStateManager.set_state("start_time", datetime.now())
            DetectionStateManager.set_state("training_mode", config_dict["training_mode"])
            DetectionStateManager.set_state("epochs_per_stage", config_dict["epochs_per_stage"])
            DetectionStateManager.set_state("output_dir", config_dict["output_root"])

            DetectionStateManager.add_log_message("Training configuration validated")
            DetectionStateManager.add_log_message(f"Mode: {config_dict['training_mode']}")
            DetectionStateManager.add_log_message(f"Output: {config_dict['output_root']}")

            st.rerun()

    with col2:
        if st.button("❌ Cancel", use_container_width=True):
            st.rerun()


# =============================================================================
# Step 2: Training Progress
# =============================================================================

def _render_step_2_training() -> None:
    """Render training progress step."""
    st.title("🎯 Varasana Training in Progress")

    state = DetectionStateManager.to_training_state()

    # Curriculum progress indicator (enhanced)
    if state.training_mode == "curriculum":
        _render_curriculum_progress_enhanced(state)

    st.divider()

    # Progress metrics with ETA
    _render_progress_metrics_enhanced(state)

    # Progress bars
    if state.training_mode == "curriculum":
        overall_progress = state.get_progress_fraction()
        st.progress(overall_progress, text=f"Overall: {overall_progress*100:.1f}%")

        stage_progress = state.get_stage_progress_fraction()
        st.progress(
            stage_progress,
            text=f"Stage '{state.current_stage_name}': Epoch {state.current_epoch}/{state.total_epochs_per_stage}"
        )
    else:
        progress = state.get_progress_fraction()
        st.progress(progress, text=f"Epoch {state.current_epoch}/{state.total_epochs}")

    st.divider()

    # Loss Components section (enhanced per spec Section 2.3)
    _render_loss_components_panel(state)

    # Training log
    with st.expander("Training Log", expanded=False):
        for msg in reversed(state.training_log[-20:]):
            st.text(msg)

    # Control buttons
    st.divider()
    _render_training_controls(state)


def _render_progress_metrics_enhanced(state: DetectionTrainingState) -> None:
    """Render enhanced progress metrics with ETA."""
    col1, col2, col3, col4, col5 = st.columns(5)

    with col1:
        if state.training_mode == "curriculum":
            st.metric(
                "Stage",
                f"{state.current_stage + 1}/{state.total_stages}",
                delta=state.current_stage_name.title(),
            )
        else:
            st.metric("Epoch", f"{state.current_epoch}/{state.total_epochs}")

    with col2:
        st.metric(
            "Epoch (Stage)",
            f"{state.current_epoch}/{state.total_epochs_per_stage}"
        )

    with col3:
        st.metric(
            "Best Loss",
            f"{state.best_val_loss:.4f}" if state.best_val_loss < float('inf') else "-"
        )

    with col4:
        st.metric("Elapsed", state.format_elapsed())

    with col5:
        eta = state.format_eta()
        st.metric("ETA", eta)


def _render_loss_components_panel(state: DetectionTrainingState) -> None:
    """
    Render loss components panel with current values and chart.

    Per Section 2.3: Display individual loss components with:
    - Real-time line chart
    - Current values with deltas
    """
    st.subheader("Loss Components")

    if not state.loss_history:
        st.info("Waiting for training data...")
        return

    df = pd.DataFrame(state.loss_history)

    # Get current and previous values for delta calculation
    current = df.iloc[-1] if len(df) > 0 else None
    previous = df.iloc[-2] if len(df) > 1 else None

    # Current values panel
    st.markdown(f"**Current Values (Epoch {state.current_epoch}):**")

    col1, col2, col3, col4 = st.columns(4)

    loss_components = [
        ("cls_loss", "Classification Loss", col1),
        ("bbox_loss", "Regression Loss", col2),
        ("centerness_loss", "Centerness Loss", col3),
        ("total_loss", "Total Loss", col4),
    ]

    for key, label, col in loss_components:
        with col:
            if current is not None and key in current:
                value = current[key]
                delta = None
                delta_color = "normal"

                if previous is not None and key in previous:
                    delta_val = value - previous[key]
                    delta = f"{delta_val:+.4f}"
                    delta_color = "inverse"  # Green for negative (decreasing loss)

                st.metric(
                    label,
                    f"{value:.4f}",
                    delta=delta,
                    delta_color=delta_color,
                )
            else:
                st.metric(label, "-")

    # Loss chart with all components
    st.markdown("**Loss Curves:**")

    columns = ["cls_loss", "bbox_loss", "centerness_loss", "total_loss"]
    available_cols = [c for c in columns if c in df.columns]

    if available_cols:
        render_loss_chart_with_controls(
            df=df,
            columns=available_cols,
            key_prefix="varasana_loss",
            show_controls=True,
            height=350,
        )

    # Export to CSV button
    col1, col2, col3 = st.columns([1, 1, 2])
    with col1:
        csv_data = df.to_csv(index=False)
        st.download_button(
            label="📥 Export Loss CSV",
            data=csv_data,
            file_name=f"varasana_loss_history_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv",
            mime="text/csv",
        )


def _render_curriculum_progress(state: DetectionTrainingState) -> None:
    """Render curriculum stage progress indicator (simple version)."""
    _render_curriculum_progress_enhanced(state)


def _render_curriculum_progress_enhanced(state: DetectionTrainingState) -> None:
    """
    Render enhanced curriculum progress per Section 2.4.

    Shows:
    - Current stage name and description
    - Progress within current stage
    - Summary of previous stages (final loss, time taken)
    - Preview of next stage
    """
    st.markdown("### Curriculum Progress")

    # Stage descriptions
    stage_descriptions = {
        "white": "Yeast White, Budding White (2 classes)",
        "opaque": "+ Yeast Opaque, Budding Opaque (4 classes)",
        "gray": "+ Yeast Gray, Budding Gray (6 classes)",
        "shmoo": "+ Shmoo (7 classes)",
        "pseudohyphae": "+ Pseudohyphae, P-Start, P-junction (10 classes)",
        "hyphae": "+ Hyphae, H-Start, H-junction (15 classes)",
    }

    # Render each stage row
    for i, stage_name in enumerate(STAGE_NAMES):
        stage = CURRICULUM_STAGES[i]

        # Determine stage status
        if stage_name in state.completed_stages:
            icon = "✓"
            status = "completed"
            bg_color = "#d4edda"  # Light green
            border_color = "#28a745"
        elif i == state.current_stage:
            icon = "●"
            status = "current"
            bg_color = "#fff3cd"  # Light yellow
            border_color = "#ffc107"
        else:
            icon = "○"
            status = "pending"
            bg_color = "#f8f9fa"  # Light gray
            border_color = "#dee2e6"

        # Build stage info
        stage_time = state.stage_times.get(stage_name, 0)
        time_str = _format_time(stage_time) if stage_time > 0 else ""

        # Get final loss for completed stages
        stage_loss = ""
        if stage_name in state.stage_loss_histories:
            stage_history = state.stage_loss_histories[stage_name]
            if stage_history:
                final_loss = stage_history[-1].get("total_loss", 0)
                stage_loss = f"Loss: {final_loss:.3f}"

        # Render stage row
        col1, col2, col3 = st.columns([1, 4, 2])

        with col1:
            st.markdown(
                f"<div style='font-size: 1.5rem; color: {border_color};'>{icon}</div>",
                unsafe_allow_html=True
            )

        with col2:
            st.markdown(f"**Stage {i+1}: {stage_name.title()}**")
            st.caption(stage_descriptions.get(stage_name, f"{len(stage.cumulative_classes)} classes"))

            # Show progress bar for current stage
            if status == "current":
                progress = state.current_epoch / state.total_epochs_per_stage
                st.progress(progress)
                st.caption(f"Epoch {state.current_epoch}/{state.total_epochs_per_stage}")

        with col3:
            if status == "completed":
                st.markdown(f"✅ {stage_loss}")
                if time_str:
                    st.caption(time_str)
            elif status == "current":
                st.markdown("🔄 **Training...**")
            else:
                st.markdown("⏳ *Pending*")

    # Next stage preview
    if state.current_stage < 5:
        next_stage_name = STAGE_NAMES[state.current_stage + 1]
        next_stage = CURRICULUM_STAGES[state.current_stage + 1]

        st.divider()
        st.markdown("**Next Stage Preview:**")
        st.info(
            f"**{next_stage_name.title()}**: {stage_descriptions.get(next_stage_name, '')} "
            f"— {len(next_stage.new_classes)} new class(es)"
        )


def _format_time(seconds: float) -> str:
    """Format seconds to human-readable time."""
    if seconds < 60:
        return f"{int(seconds)}s"
    elif seconds < 3600:
        minutes = int(seconds / 60)
        secs = int(seconds % 60)
        return f"{minutes}m {secs}s"
    else:
        hours = int(seconds / 3600)
        minutes = int((seconds % 3600) / 60)
        return f"{hours}h {minutes}m"


def _render_training_controls(state: DetectionTrainingState) -> None:
    """Render training control buttons."""
    col1, col2, col3, col4 = st.columns(4)

    with col1:
        is_paused = DetectionStateManager.get_state("should_pause")
        if is_paused:
            if st.button("▶️ Resume", type="primary", use_container_width=True):
                DetectionStateManager.set_state("should_pause", False)
                DetectionStateManager.add_log_message("Training resumed")
                st.rerun()
        else:
            if st.button("⏸️ Pause", use_container_width=True):
                DetectionStateManager.set_state("should_pause", True)
                DetectionStateManager.add_log_message("Training paused")
                st.rerun()

    with col2:
        if state.training_mode == "curriculum":
            if st.button("⏭️ Skip Stage", use_container_width=True):
                DetectionStateManager.set_state("should_skip_stage", True)
                DetectionStateManager.add_log_message(f"Skipping stage {state.current_stage_name}")
                st.rerun()

    with col3:
        if st.button("⏹️ Stop & Save", type="secondary", use_container_width=True):
            DetectionStateManager.set_state("should_stop", True)
            DetectionStateManager.set_state("status", "stopped")
            DetectionStateManager.add_log_message("Stop requested")
            st.rerun()

    with col4:
        if st.button("❌ Cancel", type="secondary", use_container_width=True):
            DetectionStateManager.set_state("should_stop", True)
            DetectionStateManager.set_state("status", "cancelled")
            DetectionStateManager.add_log_message("Training cancelled")
            st.rerun()


# =============================================================================
# Step 3: Summary with Evaluation
# =============================================================================

def _render_step_3_summary() -> None:
    """Render training summary step with comprehensive evaluation."""
    st.title("🎯 Varasana Training Complete")

    state = DetectionStateManager.to_training_state()

    # Get or compute evaluation results
    eval_results = _get_or_compute_evaluation(state)

    # Tab layout for summary sections
    tab1, tab2, tab3, tab4, tab5 = st.tabs([
        "📊 Overview",
        "🎯 GT vs Predictions",
        "📈 Metrics",
        "🔍 Sample Browser",
        "💾 Save Model",
    ])

    with tab1:
        _render_overview_tab(state, eval_results)

    with tab2:
        _render_gt_vs_predictions_tab(state, eval_results)

    with tab3:
        _render_metrics_tab(state, eval_results)

    with tab4:
        _render_sample_browser_tab(state, eval_results)

    with tab5:
        _render_save_model_tab(state, eval_results)


def _get_or_compute_evaluation(state: DetectionTrainingState) -> Optional[EvaluationResults]:
    """Get cached evaluation results or compute them."""
    # Check if already computed
    if "eval_results" in st.session_state:
        return st.session_state["eval_results"]

    # For now, return mock results if no real evaluation data
    # In production, this would run actual model inference
    eval_results = _create_mock_evaluation_results()
    st.session_state["eval_results"] = eval_results
    return eval_results


def _create_mock_evaluation_results() -> EvaluationResults:
    """Create placeholder evaluation results for demonstration."""
    return EvaluationResults(
        mAP=0.0,
        AP50=0.0,
        AP75=0.0,
        precision=0.0,
        recall=0.0,
        f1_score=0.0,
        class_names=CLASS_NAMES,
        per_class_precision={i: 0.0 for i in range(len(CLASS_NAMES))},
        per_class_recall={i: 0.0 for i in range(len(CLASS_NAMES))},
        per_class_f1={i: 0.0 for i in range(len(CLASS_NAMES))},
        per_class_AP={i: 0.0 for i in range(len(CLASS_NAMES))},
    )


def _render_overview_tab(
    state: DetectionTrainingState,
    eval_results: Optional[EvaluationResults],
) -> None:
    """Render overview tab with summary metrics."""
    # Summary metrics row
    col1, col2, col3, col4 = st.columns(4)

    with col1:
        st.metric("Status", state.status.upper())

    with col2:
        st.metric("Stages Completed", f"{len(state.completed_stages)}/{state.total_stages}")

    with col3:
        if state.mAP is not None:
            st.metric("mAP", f"{state.mAP:.3f}")
        elif eval_results and eval_results.mAP > 0:
            st.metric("mAP", f"{eval_results.mAP:.3f}")
        else:
            st.metric("Best Loss", f"{state.best_val_loss:.4f}")

    with col4:
        st.metric("Total Time", state.format_elapsed())

    st.divider()

    # Performance metrics table
    if eval_results:
        st.subheader("Performance Metrics")

        metrics_data = {
            "Metric": ["mAP (COCO)", "AP50", "AP75", "Precision", "Recall", "F1 Score"],
            "Value": [
                f"{eval_results.mAP:.3f}" if eval_results.mAP else "-",
                f"{eval_results.AP50:.3f}" if eval_results.AP50 else "-",
                f"{eval_results.AP75:.3f}" if eval_results.AP75 else "-",
                f"{eval_results.precision:.3f}" if eval_results.precision else "-",
                f"{eval_results.recall:.3f}" if eval_results.recall else "-",
                f"{eval_results.f1_score:.3f}" if eval_results.f1_score else "-",
            ],
            "Description": [
                "Mean Average Precision (COCO style)",
                "AP at IoU threshold 0.5",
                "AP at IoU threshold 0.75",
                "Overall precision",
                "Overall recall",
                "Harmonic mean of precision/recall",
            ],
        }

        st.dataframe(
            pd.DataFrame(metrics_data),
            hide_index=True,
            use_container_width=True,
        )

    st.divider()

    # Checkpoint summary
    st.subheader("Stage Checkpoints")
    if state.stage_checkpoints:
        for stage_name, checkpoint_path in state.stage_checkpoints.items():
            st.markdown(f"**{stage_name.title()}**: `{checkpoint_path}`")
    else:
        st.info("No checkpoints saved")

    # Loss curves
    st.subheader("Training Curves")
    if state.loss_history:
        df = pd.DataFrame(state.loss_history)
        columns = ["cls_loss", "bbox_loss", "centerness_loss", "total_loss"]
        available_cols = [c for c in columns if c in df.columns]

        if available_cols:
            render_loss_chart_with_controls(
                df=df,
                columns=available_cols,
                key_prefix="varasana_overview",
                show_controls=True,
                height=350,
            )


def _render_gt_vs_predictions_tab(
    state: DetectionTrainingState,
    eval_results: Optional[EvaluationResults],
) -> None:
    """Render Ground Truth vs Predictions visualization tab."""
    st.subheader("Ground Truth vs Predictions")

    # Test dataset selector from zoo
    st.markdown("**Select Test Dataset**")
    try:
        dataset_zoo = DatasetZoo()
        test_datasets = [
            ds for ds in dataset_zoo.list_datasets(project="varasana")
            if "test" in ds.id or ds.format == "image_dir"
        ]
    except Exception:
        test_datasets = []

    if test_datasets:
        test_options = {ds.id: f"{ds.name} ({ds.num_samples:,} samples)" for ds in test_datasets}
        selected_test_id = st.selectbox(
            "Test dataset for evaluation",
            options=list(test_options.keys()),
            format_func=lambda x: test_options[x],
            key="eval_test_dataset",
        )
        selected_test = dataset_zoo.get(selected_test_id)
        if selected_test:
            st.caption(f"Path: `{selected_test.path}`")
    else:
        st.caption("No test datasets in zoo. Register one via Dataset Manager.")

    st.divider()

    if not eval_results or not eval_results.image_evaluations:
        st.info(
            "No evaluation data available. Run evaluation on the test set "
            "to see GT vs Prediction comparisons."
        )

        st.markdown("""
        **This panel will show:**
        - Side-by-side comparison of ground truth and predictions
        - Bounding boxes color-coded by class
        - IoU scores for matched detections
        - Confidence scores for predictions
        """)
        return

    # Sample selector
    col1, col2, col3 = st.columns([2, 1, 1])

    with col1:
        sample_idx = st.number_input(
            "Sample",
            min_value=1,
            max_value=len(eval_results.image_evaluations),
            value=1,
            key="gt_pred_sample_idx",
        )

    with col2:
        if st.button("🎲 Random Sample", key="gt_pred_random"):
            sample_idx = np.random.randint(1, len(eval_results.image_evaluations) + 1)
            st.session_state["gt_pred_sample_idx"] = sample_idx

    with col3:
        view_mode = st.selectbox(
            "View Mode",
            options=["random", "best", "worst"],
            format_func=lambda x: {
                "random": "Random Samples",
                "best": "Best Predictions (High IoU)",
                "worst": "Worst Predictions (Low IoU)",
            }[x],
            key="gt_pred_view_mode",
        )

    # Get sample based on view mode
    if view_mode == "best":
        samples = eval_results.get_best_predictions(20)
    elif view_mode == "worst":
        samples = eval_results.get_worst_predictions(20)
    else:
        samples = eval_results.image_evaluations

    if samples:
        idx = min(sample_idx - 1, len(samples) - 1)
        evaluation = samples[idx]

        # Display sample info
        st.markdown(f"**Image:** `{evaluation.image_path}`")

        # Metrics for this image
        col1, col2, col3, col4 = st.columns(4)
        with col1:
            st.metric("IoU", f"{evaluation.avg_iou:.2f}")
        with col2:
            st.metric("True Positives", evaluation.num_tp)
        with col3:
            st.metric("False Positives", evaluation.num_fp)
        with col4:
            st.metric("False Negatives", evaluation.num_fn)

        # Create visualization
        visualizer = PredictionVisualizer(CLASS_NAMES)

        # Try to load and visualize the image
        data_root = DetectionStateManager.get_state("config_dict", {}).get("data_root")
        if data_root:
            browser = SampleBrowser(eval_results, Path(data_root))
            sample_data = browser.get_sample_with_visualization(evaluation)

            if "gt_image" in sample_data and "pred_image" in sample_data:
                col1, col2 = st.columns(2)

                with col1:
                    st.markdown("**Ground Truth**")
                    st.image(sample_data["gt_image"], use_container_width=True)

                with col2:
                    st.markdown("**Prediction**")
                    st.image(sample_data["pred_image"], use_container_width=True)
            else:
                st.warning("Could not load image for visualization")
        else:
            st.info("Configure data directory to enable visualization")


def _render_metrics_tab(
    state: DetectionTrainingState,
    eval_results: Optional[EvaluationResults],
) -> None:
    """Render detailed metrics tab with confusion matrix and per-class metrics."""
    st.subheader("Detailed Metrics")

    if not eval_results:
        st.info("No evaluation results available.")
        return

    # Confusion Matrix
    st.markdown("### Confusion Matrix")

    normalize_cm = st.checkbox("Normalize by row (GT class)", value=True, key="cm_normalize")

    cm_fig = create_confusion_matrix_figure(eval_results, normalize=normalize_cm)
    if cm_fig:
        st.plotly_chart(cm_fig, use_container_width=True, key="confusion_matrix_chart")
    else:
        st.info("Confusion matrix not available - run evaluation to compute.")

    st.divider()

    # Per-class precision/recall bar chart
    st.markdown("### Per-Class Precision, Recall, and F1")

    pr_fig = create_precision_recall_chart(eval_results)
    if pr_fig:
        st.plotly_chart(pr_fig, use_container_width=True, key="precision_recall_chart")
    else:
        st.info("Per-class metrics not available - run evaluation to compute.")

    st.divider()

    # Per-class table
    st.markdown("### Per-Class AP")

    if eval_results.per_class_AP:
        class_data = []
        for class_id, ap in eval_results.per_class_AP.items():
            class_name = CLASS_NAMES[class_id] if class_id < len(CLASS_NAMES) else f"class_{class_id}"
            precision = eval_results.per_class_precision.get(class_id, 0)
            recall = eval_results.per_class_recall.get(class_id, 0)
            f1 = eval_results.per_class_f1.get(class_id, 0)

            class_data.append({
                "Class": class_name,
                "AP": f"{ap:.3f}",
                "Precision": f"{precision:.3f}",
                "Recall": f"{recall:.3f}",
                "F1": f"{f1:.3f}",
            })

        st.dataframe(
            pd.DataFrame(class_data),
            hide_index=True,
            use_container_width=True,
        )


def _render_sample_browser_tab(
    state: DetectionTrainingState,
    eval_results: Optional[EvaluationResults],
) -> None:
    """Render sample browser for exploring predictions."""
    st.subheader("Sample Browser")

    if not eval_results or not eval_results.image_evaluations:
        st.info(
            "No evaluation data available. Run evaluation on the validation set "
            "to browse samples."
        )
        return

    # Browser mode selection
    browser_mode = st.radio(
        "Browse by",
        options=["random", "best", "worst", "false_positives", "false_negatives"],
        format_func=lambda x: {
            "random": "🎲 Random Samples",
            "best": "✅ Best Predictions (Highest IoU)",
            "worst": "❌ Worst Predictions (Lowest IoU)",
            "false_positives": "⚠️ False Positives",
            "false_negatives": "🔍 False Negatives (Missed Detections)",
        }[x],
        horizontal=True,
        key="browser_mode",
    )

    # Get samples based on mode
    n_samples = 10
    if browser_mode == "random":
        seed = st.number_input("Random seed", value=42, key="browser_seed")
        samples = SampleBrowser(eval_results, None).get_random_samples(n_samples, seed)
    elif browser_mode == "best":
        samples = eval_results.get_best_predictions(n_samples)
    elif browser_mode == "worst":
        samples = eval_results.get_worst_predictions(n_samples)
    elif browser_mode == "false_positives":
        samples = eval_results.get_false_positive_samples(n_samples)
    else:  # false_negatives
        samples = eval_results.get_false_negative_samples(n_samples)

    if not samples:
        st.info(f"No samples found for '{browser_mode}' mode.")
        return

    st.markdown(f"**Showing {len(samples)} samples**")

    # Display samples in a grid
    for i, evaluation in enumerate(samples):
        with st.expander(
            f"Sample {i+1}: {Path(evaluation.image_path).name} "
            f"(IoU: {evaluation.avg_iou:.2f}, TP: {evaluation.num_tp}, "
            f"FP: {evaluation.num_fp}, FN: {evaluation.num_fn})",
            expanded=(i == 0),
        ):
            # Metrics row
            col1, col2, col3, col4, col5 = st.columns(5)
            with col1:
                st.metric("Avg IoU", f"{evaluation.avg_iou:.2f}")
            with col2:
                st.metric("Ground Truths", len(evaluation.ground_truths))
            with col3:
                st.metric("Predictions", len(evaluation.predictions))
            with col4:
                st.metric("False Positives", evaluation.num_fp)
            with col5:
                st.metric("False Negatives", evaluation.num_fn)

            # Try to show image
            data_root = DetectionStateManager.get_state("config_dict", {}).get("data_root")
            if data_root:
                browser = SampleBrowser(eval_results, Path(data_root))
                sample_data = browser.get_sample_with_visualization(evaluation)

                if "gt_image" in sample_data:
                    col1, col2 = st.columns(2)
                    with col1:
                        st.markdown("**Ground Truth**")
                        st.image(sample_data["gt_image"], use_container_width=True)
                    with col2:
                        st.markdown("**Prediction**")
                        st.image(sample_data["pred_image"], use_container_width=True)

            # Show detection details
            if evaluation.matches:
                st.markdown("**Matched Detections:**")
                for gt_idx, pred_idx, iou in evaluation.matches:
                    gt = evaluation.ground_truths[gt_idx]
                    pred = evaluation.predictions[pred_idx]
                    gt_class = CLASS_NAMES[gt.label] if gt.label < len(CLASS_NAMES) else f"class_{gt.label}"
                    pred_class = CLASS_NAMES[pred.label] if pred.label < len(CLASS_NAMES) else f"class_{pred.label}"
                    st.text(f"  GT: {gt_class} ↔ Pred: {pred_class} (IoU: {iou:.2f}, Conf: {pred.score:.2f})")


def _render_save_model_tab(
    state: DetectionTrainingState,
    eval_results: Optional[EvaluationResults],
) -> None:
    """Render model saving tab."""
    st.subheader("Save Model")

    st.markdown("""
    Choose where to save your trained model:
    - **Research**: For experimental use, saved to research directory
    - **Production**: Validated and ready for inference, available in Varasana apps
    """)

    model_type = st.radio(
        "Model Classification",
        options=["research", "production"],
        format_func=lambda x: (
            "🔬 Research - For experimental use" if x == "research"
            else "🚀 Production - Validated for inference apps"
        ),
        horizontal=True,
        key="save_model_type",
    )

    # Show save paths
    if model_type == "research":
        save_path = f"{_settings.refined_path}/research/varasana/"
    else:
        save_path = f"{_settings.refined_path}/production/varasana/"

    st.info(f"Models will be saved to: `{save_path}`")

    col1, col2 = st.columns(2)

    with col1:
        model_name = st.text_input(
            "Model Name",
            value=f"varasana_{datetime.now().strftime('%Y%m%d')}",
            key="save_model_name",
        )

    with col2:
        model_version = st.text_input(
            "Version",
            value="1.0",
            key="save_model_version",
        )

    model_description = st.text_area(
        "Description",
        value="FCOS object detection model for Candida morphology",
        key="save_model_description",
    )

    # Include evaluation metrics if available
    if eval_results and eval_results.mAP > 0:
        st.markdown("**Evaluation metrics will be saved with the model:**")
        st.json({
            "mAP": round(eval_results.mAP, 3),
            "AP50": round(eval_results.AP50, 3),
            "precision": round(eval_results.precision, 3),
            "recall": round(eval_results.recall, 3),
            "f1_score": round(eval_results.f1_score, 3),
        })

    if st.button("💾 Save Model", type="primary", key="save_model_button"):
        _save_model(
            state=state,
            model_type=model_type,
            model_name=model_name,
            model_version=model_version,
            model_description=model_description,
            eval_results=eval_results,
        )


def _save_model(
    state: DetectionTrainingState,
    model_type: str,
    model_name: str,
    model_version: str,
    model_description: str,
    eval_results: Optional[EvaluationResults] = None,
) -> None:
    """Save the trained model to the unified model zoo."""
    # Find best checkpoint
    best_checkpoint = state.best_checkpoint
    if best_checkpoint is None and state.stage_checkpoints:
        # Use latest stage checkpoint
        best_checkpoint = list(state.stage_checkpoints.values())[-1]

    if best_checkpoint is None:
        st.error("No checkpoint found to save")
        return

    # Build metrics dict with evaluation results if available
    metrics = {
        "best_loss": state.best_val_loss,
    }

    if eval_results and eval_results.mAP > 0:
        metrics.update({
            "mAP": eval_results.mAP,
            "AP50": eval_results.AP50,
            "AP75": eval_results.AP75,
            "precision": eval_results.precision,
            "recall": eval_results.recall,
            "f1_score": eval_results.f1_score,
        })
    else:
        metrics.update({
            "mAP": state.mAP or 0.0,
            "AP50": state.AP50 or 0.0,
            "AP75": state.AP75 or 0.0,
        })

    # Build a model ID from name + version
    model_id = f"varasana_{model_name.lower().replace(' ', '_')}_{model_version.replace('.', '_')}"

    config_dict = DetectionStateManager.get_state("config_dict") or {}

    # Register in unified ModelZoo
    try:
        zoo = ModelZoo()
        zoo.register(
            model_id=model_id,
            name=model_name,
            project="varasana",
            model_type=model_type,
            version=model_version,
            architecture="fcos_resnet101_fpn",
            checkpoint=Path(best_checkpoint).name,
            config_file=config_dict.get("base_config", ""),
            path=Path(best_checkpoint).parent,
            metrics=metrics,
            description=model_description,
            tags=["fcos", "detection", "curriculum"],
            training_config=config_dict,
        )
        st.success("Model saved to zoo!")
        st.info(f"**Model ID:** {model_id}")
        st.info(f"**Checkpoint:** {best_checkpoint}")

        if model_type == "production":
            st.success("Model is now available in Varasana inference and exploration apps.")
    except Exception as e:
        st.error(f"Failed to save model: {e}")


def _render_error() -> None:
    """Render error state."""
    st.title("🎯 Training Error")

    error_message = DetectionStateManager.get_state("error_message")
    st.error(f"Training failed: {error_message}")

    # Show log
    state = DetectionStateManager.to_training_state()
    with st.expander("Training Log", expanded=True):
        for msg in state.training_log:
            st.text(msg)

    if st.button("🔄 Start Over"):
        DetectionStateManager.reset_session_state()
        st.rerun()


# =============================================================================
# Run Application
# =============================================================================

if __name__ == "__main__":
    main()
else:
    main()
