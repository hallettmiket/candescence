"""
Purpose: Training progress visualization component
Author: Hallett Lab
Date: 2026-02-01

Provides TrainingProgressPanel for displaying real-time training progress
with loss plots, reconstruction images, and control buttons.
"""

from typing import Any, Dict, List, Optional

import numpy as np
import pandas as pd

from candescence.core.logging_config import get_logger
from candescence.interface.training.training_state import TrainingState

logger = get_logger("candescence.interface.components.training_progress")

# Check Streamlit availability
try:
    import streamlit as st
    STREAMLIT_AVAILABLE = True
except ImportError:
    STREAMLIT_AVAILABLE = False


class TrainingProgressPanel:
    """
    Training progress visualization panel.

    Displays:
    - Progress bar with epoch count and ETA
    - Live loss curves (training and validation)
    - Reconstruction comparison grid (fixed images)
    - Training log
    - Control buttons (pause/resume, stop, cancel)
    """

    def __init__(self) -> None:
        """Initialize TrainingProgressPanel."""
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit is required for TrainingProgressPanel")

    def render(self, state: TrainingState) -> Optional[str]:
        """
        Render training progress display.

        Args:
            state: Current TrainingState

        Returns:
            Action string if control button clicked: 'pause', 'resume', 'stop', 'cancel'
            Returns None if no action taken.
        """
        # Progress bar
        self._render_progress_bar(state)

        # Metrics row
        self._render_metrics(state)

        # Loss chart
        self._render_loss_chart(state)

        # Reconstruction images
        if state.reconstruction_images is not None:
            self._render_reconstruction_grid(state.reconstruction_images, state.current_epoch)

        # Training log
        self._render_log(state)

        # Control buttons
        return self._render_controls(state)

    def _render_progress_bar(self, state: TrainingState) -> None:
        """Render progress bar with epoch count and ETA."""
        progress = state.get_progress_fraction()
        eta = state.format_eta()

        progress_text = f"Epoch {state.current_epoch}/{state.total_epochs} | ETA: {eta}"
        st.progress(progress, text=progress_text)

    def _render_metrics(self, state: TrainingState) -> None:
        """Render metrics row."""
        col1, col2, col3, col4 = st.columns(4)

        with col1:
            st.metric("Current Epoch", f"{state.current_epoch}/{state.total_epochs}")

        with col2:
            best_loss_str = f"{state.best_val_loss:.4f}" if state.best_val_loss < float('inf') else "-"
            st.metric("Best Val Loss", best_loss_str)

        with col3:
            best_epoch_str = str(state.best_epoch) if state.best_epoch > 0 else "-"
            st.metric("Best Epoch", best_epoch_str)

        with col4:
            st.metric("Elapsed", state.format_elapsed())

    def _render_loss_chart(self, state: TrainingState) -> None:
        """Render loss curves chart."""
        st.subheader("Loss Curves")

        if state.loss_history:
            df = pd.DataFrame(state.loss_history)

            # Main losses
            if "train_loss" in df.columns and "val_loss" in df.columns:
                st.line_chart(df[["train_loss", "val_loss"]])

            # Detailed loss breakdown in expander
            with st.expander("Detailed Loss Components", expanded=False):
                col1, col2 = st.columns(2)

                with col1:
                    if "train_recon" in df.columns and "val_recon" in df.columns:
                        st.markdown("**Reconstruction Loss**")
                        st.line_chart(df[["train_recon", "val_recon"]])

                with col2:
                    if "train_kl" in df.columns and "val_kl" in df.columns:
                        st.markdown("**KL Divergence**")
                        st.line_chart(df[["train_kl", "val_kl"]])

                if "train_cond" in df.columns and df["train_cond"].sum() > 0:
                    st.markdown("**Conditional Loss**")
                    st.line_chart(df[["train_cond", "val_cond"]])
        else:
            st.info("Training starting...")

    def _render_reconstruction_grid(
        self,
        images: np.ndarray,
        epoch: int
    ) -> None:
        """
        Render reconstruction comparison grid.

        Args:
            images: Array of shape (2, n_images, H, W, C)
                    images[0] = originals, images[1] = reconstructions
            epoch: Current epoch number
        """
        st.subheader("Reconstruction Quality")

        if images is None or len(images) == 0:
            return

        # images shape: (2, n_images, H, W, C)
        originals = images[0]
        reconstructions = images[1]
        n_images = len(originals)

        st.caption(f"Reconstruction comparison (Epoch {epoch})")

        # Create columns for images
        cols = st.columns(min(n_images, 8))

        for i, col in enumerate(cols):
            if i < n_images:
                with col:
                    st.image(originals[i], caption="Original", use_container_width=True)
                    st.image(reconstructions[i], caption="Reconstructed", use_container_width=True)

    def _render_log(self, state: TrainingState) -> None:
        """Render training log."""
        with st.expander("Training Log", expanded=False):
            # Show last 20 messages, most recent first
            for msg in reversed(state.training_log[-20:]):
                st.text(msg)

    def _render_controls(self, state: TrainingState) -> Optional[str]:
        """
        Render control buttons.

        Returns:
            Action string if button clicked, None otherwise
        """
        st.divider()

        col1, col2, col3 = st.columns(3)
        action = None

        with col1:
            if state.status == "paused":
                if st.button("Resume", type="primary", key="btn_resume"):
                    action = "resume"
            else:
                if st.button("Pause", key="btn_pause"):
                    action = "pause"

        with col2:
            if st.button("Stop & Save", type="secondary", key="btn_stop"):
                action = "stop"

        with col3:
            if st.button("Cancel", type="secondary", key="btn_cancel"):
                action = "cancel"

        return action

    @staticmethod
    def create_loss_dataframe(loss_history: List[Dict[str, float]]) -> pd.DataFrame:
        """
        Create DataFrame from loss history for plotting.

        Args:
            loss_history: List of loss dictionaries per epoch

        Returns:
            DataFrame with loss values
        """
        if not loss_history:
            return pd.DataFrame()

        return pd.DataFrame(loss_history)

    @staticmethod
    def format_reconstruction_grid_as_figure(
        images: np.ndarray,
        epoch: int
    ) -> "matplotlib.figure.Figure":
        """
        Create matplotlib figure of reconstruction comparison.

        Useful for saving to disk.

        Args:
            images: Array of shape (2, n_images, H, W, C)
            epoch: Current epoch

        Returns:
            Matplotlib figure
        """
        import matplotlib.pyplot as plt

        originals = images[0]
        reconstructions = images[1]
        n_images = len(originals)

        fig, axes = plt.subplots(2, n_images, figsize=(2 * n_images, 4))

        for i in range(n_images):
            axes[0, i].imshow(originals[i])
            axes[0, i].axis('off')
            if i == n_images // 2:
                axes[0, i].set_title('Original', fontsize=10)

            axes[1, i].imshow(reconstructions[i])
            axes[1, i].axis('off')
            if i == n_images // 2:
                axes[1, i].set_title('Reconstructed', fontsize=10)

        fig.suptitle(f'Epoch {epoch}', fontsize=12)
        plt.tight_layout()

        return fig
