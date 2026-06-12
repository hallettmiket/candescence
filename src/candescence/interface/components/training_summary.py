"""
Purpose: Post-training summary component for VAE training
Author: Hallett Lab
Date: 2026-02-01

Provides TrainingSummaryPanel for displaying training results and
handling model saving with Research/Production classification.
"""

from pathlib import Path
from typing import Any, Dict, Optional

import numpy as np
import pandas as pd

from candescence.core.logging_config import get_logger
from candescence.interface.training.training_state import TrainingState

logger = get_logger("candescence.interface.components.training_summary")

# Check Streamlit availability
try:
    import streamlit as st
    STREAMLIT_AVAILABLE = True
except ImportError:
    STREAMLIT_AVAILABLE = False


class TrainingSummaryPanel:
    """
    Post-training summary panel.

    Displays:
    - Final training metrics
    - Training curves
    - Final reconstruction visualization
    - Research vs Production model classification
    - Model metadata form
    - Save confirmation
    """

    def __init__(self) -> None:
        """Initialize TrainingSummaryPanel."""
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit is required for TrainingSummaryPanel")

    def render(
        self,
        state: TrainingState,
        config_dict: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Render training summary and save options.

        Args:
            state: Final TrainingState
            config_dict: Configuration dictionary used for training

        Returns:
            Save metadata dictionary if user clicks save, None otherwise
        """
        # Summary metrics
        self._render_metrics_summary(state)

        # Loss curves
        self._render_final_curves(state)

        # Final reconstruction
        if state.reconstruction_images is not None:
            self._render_final_reconstruction(state)

        st.divider()

        # Model classification
        model_type = self._render_classification_choice()

        # Model metadata form
        metadata = self._render_metadata_form(config_dict)

        st.divider()

        # Save button
        col1, col2, col3 = st.columns([1, 2, 1])
        with col2:
            if st.button("Save Model", type="primary", use_container_width=True):
                return {
                    "model_type": model_type,
                    **metadata,
                }

        return None

    def _render_metrics_summary(self, state: TrainingState) -> None:
        """Render summary metrics."""
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

    def _render_final_curves(self, state: TrainingState) -> None:
        """Render final training curves."""
        st.subheader("Training Curves")

        if state.loss_history:
            df = pd.DataFrame(state.loss_history)
            st.line_chart(df[["train_loss", "val_loss"]])

            # Show detailed breakdown in expander
            with st.expander("Detailed Loss Components", expanded=False):
                col1, col2 = st.columns(2)

                with col1:
                    if "train_recon" in df.columns:
                        st.markdown("**Reconstruction Loss**")
                        st.line_chart(df[["train_recon", "val_recon"]])

                with col2:
                    if "train_kl" in df.columns:
                        st.markdown("**KL Divergence**")
                        st.line_chart(df[["train_kl", "val_kl"]])

    def _render_final_reconstruction(self, state: TrainingState) -> None:
        """Render final reconstruction quality."""
        st.subheader("Final Reconstruction Quality")

        images = state.reconstruction_images
        if images is None or len(images) == 0:
            return

        # images shape: (2, n_images, H, W, C)
        originals = images[0]
        reconstructions = images[1]
        n_images = len(originals)

        # Create columns for images
        cols = st.columns(min(n_images, 8))

        for i, col in enumerate(cols):
            if i < n_images:
                with col:
                    st.image(originals[i], caption="Original", use_container_width=True)
                    st.image(reconstructions[i], caption="Reconstructed", use_container_width=True)

    def _render_classification_choice(self) -> str:
        """
        Render Research vs Production model choice.

        Returns:
            'research' or 'production'
        """
        st.subheader("Model Classification")

        model_type = st.radio(
            "Save this model as:",
            options=["research", "production"],
            format_func=lambda x: "Research Model" if x == "research" else "Production Model",
            help="Research models are for experimental use. Production models are validated and visible in exploration apps.",
            key="save_model_type"
        )

        if model_type == "research":
            st.info("""
            **Research Model**
            - For experimental use and further development
            - Saved to the experiment output directory
            - Not visible in latent space explorer or other apps
            """)
        else:
            st.success("""
            **Production Model**
            - Validated and ready for end-user exploration
            - Registered in the production model registry
            - Will appear in latent space explorer and other apps
            """)

        return model_type

    def _render_metadata_form(self, config_dict: Dict[str, Any]) -> Dict[str, Any]:
        """
        Render model metadata form.

        Returns:
            Dictionary with model metadata
        """
        st.subheader("Model Metadata")

        col1, col2 = st.columns(2)

        with col1:
            default_name = f"tendril_vae_{config_dict.get('experiment_name', 'exp')}"
            model_name = st.text_input(
                "Model Name",
                value=default_name,
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

        default_desc = (
            f"Tendril VAE trained on day {config_dict.get('restrict_to_day', 2)} images "
            f"with {config_dict.get('latent_dim', 128)}-dimensional latent space."
        )
        model_description = st.text_area(
            "Description",
            value=default_desc,
            help="Description of what this model is for.",
            key="save_model_description"
        )

        return {
            "name": model_name,
            "version": model_version,
            "description": model_description,
        }

    @staticmethod
    def save_model(
        model_path: Path,
        model_type: str,
        metadata: Dict[str, Any],
        config_dict: Dict[str, Any],
        factory: Optional[Any] = None
    ) -> bool:
        """
        Save model and optionally register as production.

        Args:
            model_path: Path to saved model.pth file
            model_type: 'research' or 'production'
            metadata: Model metadata dictionary
            config_dict: Training configuration dictionary
            factory: Factory instance (for saving args.json)

        Returns:
            True if save successful
        """
        if not model_path.exists():
            logger.error(f"Model file not found: {model_path}")
            return False

        # Save arguments if factory available
        if factory is not None:
            try:
                factory._save_arguments()
            except Exception as e:
                logger.warning(f"Failed to save arguments: {e}")

        # Register in unified model zoo
        if model_type == "production":
            try:
                from candescence.core.model_zoo import ModelZoo

                zoo = ModelZoo()
                model_id = f"{config_dict['experiment_name']}_{config_dict['save_name']}"

                zoo.register(
                    model_id=model_id,
                    name=metadata["name"],
                    project="tlv",
                    model_type="production",
                    version=metadata["version"],
                    architecture="tendril_vae",
                    checkpoint="model.pth",
                    config_file="args.json",
                    path=model_path.parent,
                    description=metadata["description"],
                    tags=["tendril", "vae", "strategy_14"],
                    training_config={
                        "strategy": 14,
                        "latent_dim": config_dict.get("latent_dim", 128),
                        "conditional_variables": config_dict.get(
                            "conditional_variables",
                            ["average_hue", "average_saturation", "average_value"],
                        ),
                    },
                )

                logger.info(f"Model registered in zoo as production: {model_id}")
                return True

            except Exception as e:
                logger.exception("Failed to register production model in zoo")
                return False

        logger.info(f"Research model saved to: {model_path}")
        return True

    @staticmethod
    def render_next_steps(model_type: str, model_path: Path) -> None:
        """
        Render next steps after model save.

        Args:
            model_type: 'research' or 'production'
            model_path: Path to saved model
        """
        st.divider()
        st.subheader("Next Steps")

        if model_type == "production":
            st.markdown("""
            Your model is now available in:
            - **Latent Space Explorer**: Explore the learned latent space
            - **Other Candescence apps**: This model will appear in model selection dropdowns

            To use this model programmatically:
            ```python
            from candescence.core.model_zoo import ModelZoo

            zoo = ModelZoo()
            models = zoo.list_models(project="tlv")
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

            To load and use this model:
            ```python
            from candescence.interface import load_tlv_model

            model = load_tlv_model("{model_path}")

            # Encode an image
            z = model.encode(image_tensor)

            # Decode from latent space
            reconstructed = model.decode(z)
            ```

            To promote to production later:
            ```bash
            python scripts/production/register_model.py \\
                --path "{model_path}" \\
                --name "Your Model Name" \\
                --version "1.0.0"
            ```
            """)

        # Option to start new training
        st.divider()
        if st.button("Start New Training", use_container_width=True):
            # This would reset session state in the main app
            st.session_state["training_status"] = "idle"
            st.rerun()
