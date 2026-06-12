"""
Purpose: Configuration panel component for VAE training
Author: Hallett Lab
Date: 2026-02-01

Provides TrainingConfigPanel for rendering training configuration UI
with sensible defaults and parameter explanations.
"""

from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import torch

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.interface.components.training_config")

# Check Streamlit availability
try:
    import streamlit as st
    STREAMLIT_AVAILABLE = True
except ImportError:
    STREAMLIT_AVAILABLE = False

# Default paths — resolved from the settings layer (env / config / default).
from candescence.core.config import DEFAULT_CLEANED_METADATA_XLSX
from candescence.core.settings import get_settings

_settings = get_settings()
DEFAULT_IMAGE_DIR = f"{_settings.image_dir}/"
DEFAULT_METADATA_PATH = str(DEFAULT_CLEANED_METADATA_XLSX)
DEFAULT_OUTPUT_BASE = f"{_settings.refined_path}/"

# Parameter tooltips for non-experts
TOOLTIPS = {
    "latent_dim": "Dimensionality of the latent space. Higher values capture more detail but may overfit. Typical: 64-256.",
    "vae_lr": "Learning rate for VAE parameters. Controls how quickly the model learns. Start with 1e-4.",
    "film_lr": "Learning rate for FiLM conditioning layers. Usually 5x higher than VAE LR.",
    "batch_size": "Number of images per training step. Higher is faster but needs more GPU memory.",
    "number_epochs": "Number of complete passes through the training data. More epochs = better learning.",
    "kl_weight": "Beta parameter for KL divergence loss. Higher values encourage smoother latent space.",
    "conditional_loss_weight": "Weight for conditional loss. Controls how strongly conditioning affects learning.",
    "train_num": "Number of images for training.",
    "validation_num": "Number of images for validation (tracking performance during training).",
    "test_num": "Number of images for final testing (held out until the end).",
    "restrict_to_day": "Only use images from this day. Day 2 is typical for morphology analysis.",
    "report_periodicity": "Show reconstruction images every N epochs.",
    "image_dir": "Directory containing training images (.bmp format).",
    "metadata_path": "Path to metadata Excel/CSV file with image information.",
    "image_dimension": "Size of input images (width = height).",
    "intermediate_dim": "Size of hidden layers in encoder/decoder.",
    "leaky_relu_slope": "Negative slope for LeakyReLU activations.",
    "mse_weight": "Weight for MSE reconstruction loss.",
    "experiment_name": "Name for this experiment. Will be used in output path.",
    "save_name": "Name for this specific training run.",
}


class TrainingConfigPanel:
    """
    Configuration panel for VAE training.

    Renders configuration UI with four sections:
    1. Data configuration (paths, dimensions)
    2. Dataset split (train/val/test proportions)
    3. Model architecture (latent dim, conditioning)
    4. Training hyperparameters (learning rates, epochs, etc.)
    5. Output configuration (experiment name, save path)

    Provides validation and returns TLVConfig-compatible dictionary.
    """

    def __init__(self) -> None:
        """Initialize TrainingConfigPanel."""
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit is required for TrainingConfigPanel")

    def render(self) -> Optional[Dict[str, Any]]:
        """
        Render configuration form.

        Returns:
            Validated configuration dictionary, or None if not yet submitted.
        """
        # Data Configuration
        with st.expander("📁 Data Configuration", expanded=True):
            data_config = self._render_data_config()

        # Dataset Split
        with st.expander("📊 Dataset Split", expanded=True):
            split_config = self._render_split_config()

        # Model Architecture (mostly locked)
        with st.expander("🧬 Model Architecture", expanded=False):
            model_config = self._render_model_config()

        # Training Hyperparameters
        with st.expander("⚙️ Training Parameters", expanded=True):
            training_config = self._render_training_config()

        # Output Configuration
        with st.expander("💾 Output Configuration", expanded=True):
            output_config = self._render_output_config()

        st.divider()

        # Validation and Start
        col1, col2, col3 = st.columns([1, 2, 1])
        with col2:
            if st.button("Validate & Start Training", type="primary", use_container_width=True):
                # Combine all config
                config = {
                    **data_config,
                    **split_config,
                    **model_config,
                    **training_config,
                    **output_config,
                }

                # Validate
                is_valid, errors = self._validate_config(config)

                if is_valid:
                    return config
                else:
                    for error in errors:
                        st.error(error)

        return None

    def _render_data_config(self) -> Dict[str, Any]:
        """Render data configuration section."""
        col1, col2 = st.columns(2)

        with col1:
            image_dir = st.text_input(
                "Image Directory",
                value=DEFAULT_IMAGE_DIR,
                help=TOOLTIPS["image_dir"],
                key="cfg_image_dir"
            )

            # Check image count
            image_path = Path(image_dir)
            if image_path.exists():
                image_count = len(list(image_path.glob("*.bmp")))
                if image_count > 0:
                    st.success(f"Found {image_count:,} images")
                else:
                    st.warning("No .bmp images found in directory")
            else:
                st.error("Directory does not exist")

        with col2:
            metadata_path = st.text_input(
                "Metadata File",
                value=DEFAULT_METADATA_PATH,
                help=TOOLTIPS["metadata_path"],
                key="cfg_metadata_path"
            )

            if Path(metadata_path).exists():
                st.success("Metadata file found")
            else:
                st.warning("Metadata file not found")

        col3, col4 = st.columns(2)

        with col3:
            image_dim = st.selectbox(
                "Image Dimension",
                options=[64, 128, 256],
                index=1,
                help=TOOLTIPS["image_dimension"],
                key="cfg_image_dim"
            )

        with col4:
            restrict_day = st.selectbox(
                "Restrict to Day",
                options=[None, 1, 2, 3, 4, 5],
                index=2,  # Default to day 2
                format_func=lambda x: "All days" if x is None else f"Day {x}",
                help=TOOLTIPS["restrict_to_day"],
                key="cfg_restrict_day"
            )

        return {
            "raw_images_path": image_dir,
            "metadata_path": metadata_path,
            "image_dimension": image_dim,
            "restrict_to_day": restrict_day,
        }

    def _render_split_config(self) -> Dict[str, Any]:
        """Render dataset split configuration."""
        st.markdown("Configure how images are split between training, validation, and test sets.")

        col1, col2, col3 = st.columns(3)

        with col1:
            train_num = st.number_input(
                "Training Images",
                min_value=100,
                max_value=10000,
                value=1200,
                step=100,
                help=TOOLTIPS["train_num"],
                key="cfg_train_num"
            )

        with col2:
            val_num = st.number_input(
                "Validation Images",
                min_value=50,
                max_value=2000,
                value=400,
                step=50,
                help=TOOLTIPS["validation_num"],
                key="cfg_val_num"
            )

        with col3:
            test_num = st.number_input(
                "Test Images",
                min_value=50,
                max_value=2000,
                value=400,
                step=50,
                help=TOOLTIPS["test_num"],
                key="cfg_test_num"
            )

        total = train_num + val_num + test_num
        st.info(f"Total images needed: {total:,}")

        return {
            "train_num": train_num,
            "validation_num": val_num,
            "test_num": test_num,
        }

    def _render_model_config(self) -> Dict[str, Any]:
        """Render model architecture configuration."""
        st.markdown("**Architecture: Tendril VAE (Strategy #14)** - This is locked for this training app.")

        col1, col2 = st.columns(2)

        with col1:
            latent_dim = st.selectbox(
                "Latent Dimension",
                options=[64, 128, 256, 512],
                index=1,  # Default 128
                help=TOOLTIPS["latent_dim"],
                key="cfg_latent_dim"
            )

        with col2:
            cond_vars = st.multiselect(
                "Conditioning Variables",
                options=["average_hue", "average_saturation", "average_value"],
                default=["average_hue", "average_saturation", "average_value"],
                help=(
                    "HSV channels used for FiLM conditioning. All three are "
                    "selected by default — they're orthogonal axes of color "
                    "and a single FiLM with concatenated (h, s, v) input is "
                    "the right inductive bias."
                ),
                key="cfg_cond_vars"
            )

        # Advanced options
        intermediate_dim = 256
        leaky_relu_slope = 0.02

        if st.checkbox("Show advanced architecture options", key="show_arch_advanced"):
            col3, col4 = st.columns(2)
            with col3:
                intermediate_dim = st.number_input(
                    "Intermediate Dimension",
                    min_value=64,
                    max_value=512,
                    value=256,
                    step=64,
                    help=TOOLTIPS["intermediate_dim"],
                    key="cfg_intermediate_dim"
                )
            with col4:
                leaky_relu_slope = st.number_input(
                    "Leaky ReLU Slope",
                    min_value=0.01,
                    max_value=0.2,
                    value=0.02,
                    step=0.01,
                    help=TOOLTIPS["leaky_relu_slope"],
                    key="cfg_leaky_slope"
                )

        return {
            "architecture": "tendril_vae",
            "strategy": 14,
            "latent_dim": latent_dim,
            "conditional_variables": cond_vars,
            "intermediate_dim": intermediate_dim,
            "leaky_relu_slope": leaky_relu_slope,
        }

    def _render_training_config(self) -> Dict[str, Any]:
        """Render training hyperparameters configuration."""
        col1, col2 = st.columns(2)

        with col1:
            epochs = st.number_input(
                "Number of Epochs",
                min_value=10,
                max_value=500,
                value=100,
                step=10,
                help=TOOLTIPS["number_epochs"],
                key="cfg_epochs"
            )

            batch_size = st.selectbox(
                "Batch Size",
                options=[32, 64, 128, 256, 512],
                index=3,  # Default 256
                help=TOOLTIPS["batch_size"],
                key="cfg_batch_size"
            )

        with col2:
            vae_lr = st.select_slider(
                "VAE Learning Rate",
                options=[1e-5, 5e-5, 1e-4, 5e-4, 1e-3],
                value=1e-4,
                format_func=lambda x: f"{x:.0e}",
                help=TOOLTIPS["vae_lr"],
                key="cfg_vae_lr"
            )

            film_lr = st.select_slider(
                "FiLM Learning Rate",
                options=[1e-4, 5e-4, 1e-3, 5e-3],
                value=5e-4,
                format_func=lambda x: f"{x:.0e}",
                help=TOOLTIPS["film_lr"],
                key="cfg_film_lr"
            )

        # Advanced training options
        kl_weight = 1.0
        mse_weight = 100.0
        cond_weight = 1000.0
        report_period = 5

        if st.checkbox("Show advanced training options", key="show_train_advanced"):
            col3, col4 = st.columns(2)
            with col3:
                kl_weight = st.number_input(
                    "KL Weight (Beta)",
                    min_value=0.1,
                    max_value=10.0,
                    value=1.0,
                    step=0.1,
                    help=TOOLTIPS["kl_weight"],
                    key="cfg_kl_weight"
                )

                mse_weight = st.number_input(
                    "MSE Weight",
                    min_value=0.1,
                    max_value=200.0,
                    value=100.0,
                    step=10.0,
                    help=TOOLTIPS["mse_weight"],
                    key="cfg_mse_weight"
                )

            with col4:
                cond_weight = st.number_input(
                    "Conditional Loss Weight",
                    min_value=1.0,
                    max_value=5000.0,
                    value=1000.0,
                    step=100.0,
                    help=TOOLTIPS["conditional_loss_weight"],
                    key="cfg_cond_weight"
                )

                report_period = st.number_input(
                    "Reconstruction Preview Period",
                    min_value=1,
                    max_value=50,
                    value=5,
                    step=1,
                    help=TOOLTIPS["report_periodicity"],
                    key="cfg_report_period"
                )

        return {
            "number_epochs": epochs,
            "batch_size": batch_size,
            "vae_lr": vae_lr,
            "film_lr": film_lr,
            "kl_weight": kl_weight,
            "mse_weight": mse_weight,
            "conditional_loss_weight": cond_weight,
            "report_periodicity": report_period,
        }

    def _render_output_config(self) -> Dict[str, Any]:
        """Render output configuration section."""
        col1, col2 = st.columns(2)

        with col1:
            # Auto-generate experiment name based on timestamp
            default_exp_name = f"training_{datetime.now().strftime('%Y%m%d')}"
            exp_name = st.text_input(
                "Experiment Name",
                value=default_exp_name,
                help=TOOLTIPS["experiment_name"],
                key="cfg_exp_name"
            )

        with col2:
            default_save_name = f"run_{datetime.now().strftime('%H%M%S')}"
            save_name = st.text_input(
                "Run Name",
                value=default_save_name,
                help=TOOLTIPS["save_name"],
                key="cfg_save_name"
            )

        # Show output path
        output_path = Path(DEFAULT_OUTPUT_BASE) / exp_name / save_name
        st.info(f"Output directory: `{output_path}`")

        return {
            "experiment_name": exp_name,
            "save_name": save_name,
        }

    def _validate_config(self, config: Dict[str, Any]) -> Tuple[bool, List[str]]:
        """
        Validate configuration.

        Args:
            config: Configuration dictionary to validate

        Returns:
            Tuple of (is_valid, list of error messages)
        """
        errors = []

        # Check paths
        if not Path(config["raw_images_path"]).exists():
            errors.append("Image directory does not exist")

        if not Path(config["metadata_path"]).exists():
            errors.append("Metadata file does not exist")

        # Check GPU
        if not torch.cuda.is_available():
            # This is a warning, not an error
            st.warning("No GPU available. Training will proceed on CPU but will be very slow.")

        # Check image count
        image_path = Path(config["raw_images_path"])
        if image_path.exists():
            image_count = len(list(image_path.glob("*.bmp")))
            total_needed = config["train_num"] + config["validation_num"] + config["test_num"]
            if image_count < total_needed:
                errors.append(f"Not enough images: found {image_count}, need {total_needed}")

        # Check conditioning variables
        if not config.get("conditional_variables"):
            errors.append("At least one conditioning variable must be selected")

        return len(errors) == 0, errors

    @staticmethod
    def get_default_config() -> Dict[str, Any]:
        """
        Get default configuration values.

        Returns:
            Dictionary with default configuration values
        """
        return {
            # Data
            "raw_images_path": DEFAULT_IMAGE_DIR,
            "metadata_path": DEFAULT_METADATA_PATH,
            "image_dimension": 128,
            "restrict_to_day": 2,
            # Split
            "train_num": 1200,
            "validation_num": 400,
            "test_num": 400,
            # Model
            "architecture": "tendril_vae",
            "strategy": 14,
            "latent_dim": 128,
            "conditional_variables": [
                "average_hue", "average_saturation", "average_value"
            ],
            "intermediate_dim": 256,
            "leaky_relu_slope": 0.02,
            # Training
            "number_epochs": 100,
            "batch_size": 256,
            "vae_lr": 1e-4,
            "film_lr": 5e-4,
            "kl_weight": 1.0,
            "mse_weight": 100.0,
            "conditional_loss_weight": 1000.0,
            "report_periodicity": 5,
            # Output
            "experiment_name": f"training_{datetime.now().strftime('%Y%m%d')}",
            "save_name": f"run_{datetime.now().strftime('%H%M%S')}",
        }
