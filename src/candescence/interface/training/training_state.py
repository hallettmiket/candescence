"""
Purpose: Training state management for Streamlit training app
Author: Hallett Lab
Date: 2026-02-01

Provides TrainingState dataclass and TrainingStateManager for handling
training lifecycle and session state persistence.
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Literal, Optional

import numpy as np

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.interface.training.training_state")

# Session state key prefix to avoid conflicts
STATE_PREFIX = "training_"

# Valid training status values
TrainingStatus = Literal[
    "idle",
    "configuring",
    "validating",
    "training",
    "paused",
    "completed",
    "cancelled",
    "error",
]


@dataclass
class TrainingState:
    """
    Immutable training state container.

    Captures all information about the current training run for display
    and persistence in Streamlit session state.

    Attributes:
        status: Current training status
        current_epoch: Current epoch number (0-indexed)
        total_epochs: Total number of epochs to train
        loss_history: List of loss dictionaries per epoch
        best_epoch: Epoch with lowest validation loss
        best_val_loss: Lowest validation loss achieved
        reconstruction_images: Latest reconstruction comparison images
        start_time: When training started
        elapsed_seconds: Total elapsed training time
        error_message: Error message if status is 'error'
        training_log: List of log messages
    """

    status: TrainingStatus = "idle"
    current_epoch: int = 0
    total_epochs: int = 100

    # Progress tracking
    loss_history: List[Dict[str, float]] = field(default_factory=list)
    best_epoch: int = 0
    best_val_loss: float = float("inf")

    # Reconstruction visualization
    fixed_recon_indices: Optional[List[int]] = None
    reconstruction_images: Optional[np.ndarray] = None

    # Timing
    start_time: Optional[datetime] = None
    elapsed_seconds: float = 0.0

    # Error handling
    error_message: Optional[str] = None

    # Log
    training_log: List[str] = field(default_factory=list)

    def get_progress_fraction(self) -> float:
        """Get training progress as a fraction between 0 and 1."""
        if self.total_epochs == 0:
            return 0.0
        return min(1.0, self.current_epoch / self.total_epochs)

    def get_eta_seconds(self) -> Optional[float]:
        """Estimate remaining time based on average epoch duration."""
        if self.current_epoch == 0 or self.elapsed_seconds == 0:
            return None

        avg_epoch_time = self.elapsed_seconds / self.current_epoch
        remaining_epochs = self.total_epochs - self.current_epoch
        return avg_epoch_time * remaining_epochs

    def format_eta(self) -> str:
        """Format estimated time remaining as human-readable string."""
        eta = self.get_eta_seconds()
        if eta is None:
            return "Calculating..."

        if eta < 60:
            return f"{int(eta)} seconds"
        elif eta < 3600:
            minutes = int(eta / 60)
            return f"{minutes} minute{'s' if minutes != 1 else ''}"
        else:
            hours = int(eta / 3600)
            minutes = int((eta % 3600) / 60)
            return f"{hours}h {minutes}m"

    def format_elapsed(self) -> str:
        """Format elapsed time as human-readable string."""
        elapsed = self.elapsed_seconds
        if elapsed < 60:
            return f"{int(elapsed)} seconds"
        elif elapsed < 3600:
            minutes = int(elapsed / 60)
            seconds = int(elapsed % 60)
            return f"{minutes}m {seconds}s"
        else:
            hours = int(elapsed / 3600)
            minutes = int((elapsed % 3600) / 60)
            seconds = int(elapsed % 60)
            return f"{hours}h {minutes}m {seconds}s"

    def get_latest_metrics(self) -> Optional[Dict[str, float]]:
        """Get metrics from the most recent epoch."""
        if not self.loss_history:
            return None
        return self.loss_history[-1]

    def get_summary(self) -> Dict[str, Any]:
        """Get training summary for display."""
        latest = self.get_latest_metrics()
        return {
            "status": self.status,
            "epochs_completed": self.current_epoch,
            "total_epochs": self.total_epochs,
            "best_epoch": self.best_epoch,
            "best_val_loss": self.best_val_loss,
            "final_train_loss": latest.get("train_loss") if latest else None,
            "final_val_loss": latest.get("val_loss") if latest else None,
            "elapsed_time": self.format_elapsed(),
        }


class TrainingStateManager:
    """
    Manages training state transitions and Streamlit session state persistence.

    Provides static methods for common state operations to keep state
    management logic centralized.
    """

    # Default values for session state initialization
    DEFAULTS: Dict[str, Any] = {
        # Status
        f"{STATE_PREFIX}status": "idle",
        f"{STATE_PREFIX}current_epoch": 0,
        f"{STATE_PREFIX}total_epochs": 100,
        # Configuration
        f"{STATE_PREFIX}config_dict": {},
        f"{STATE_PREFIX}tlv_config": None,
        # Progress
        f"{STATE_PREFIX}loss_history": [],
        f"{STATE_PREFIX}best_epoch": 0,
        f"{STATE_PREFIX}best_val_loss": float("inf"),
        f"{STATE_PREFIX}start_time": None,
        f"{STATE_PREFIX}elapsed_seconds": 0.0,
        # Reconstruction
        f"{STATE_PREFIX}fixed_recon_indices": None,
        f"{STATE_PREFIX}reconstruction_images": None,
        f"{STATE_PREFIX}reconstruction_history": [],
        # Control flags
        f"{STATE_PREFIX}should_stop": False,
        f"{STATE_PREFIX}should_pause": False,
        f"{STATE_PREFIX}should_cancel": False,
        # Training objects
        f"{STATE_PREFIX}factory": None,
        f"{STATE_PREFIX}trainer": None,
        # Log
        f"{STATE_PREFIX}training_log": [],
        # Tendril (Phase 2)
        f"{STATE_PREFIX}tendril_loss_history": {},
        f"{STATE_PREFIX}tendril_summary": None,
        # Final
        f"{STATE_PREFIX}final_model_path": None,
        f"{STATE_PREFIX}save_metadata": None,
        # Error
        f"{STATE_PREFIX}error_message": None,
    }

    @staticmethod
    def initialize_session_state() -> None:
        """
        Initialize Streamlit session state with default values.

        Safe to call multiple times - only sets keys that don't exist.
        """
        try:
            import streamlit as st

            for key, default in TrainingStateManager.DEFAULTS.items():
                if key not in st.session_state:
                    st.session_state[key] = default
        except ImportError:
            logger.warning("Streamlit not available, session state not initialized")

    @staticmethod
    def reset_session_state() -> None:
        """Reset all training-related session state to defaults."""
        try:
            import streamlit as st

            for key, default in TrainingStateManager.DEFAULTS.items():
                st.session_state[key] = default
            logger.info("Training session state reset")
        except ImportError:
            logger.warning("Streamlit not available")

    @staticmethod
    def get_state(key: str) -> Any:
        """
        Get a value from session state.

        Args:
            key: Key name without prefix

        Returns:
            Value from session state or default
        """
        try:
            import streamlit as st

            full_key = f"{STATE_PREFIX}{key}"
            return st.session_state.get(
                full_key, TrainingStateManager.DEFAULTS.get(full_key)
            )
        except ImportError:
            return TrainingStateManager.DEFAULTS.get(f"{STATE_PREFIX}{key}")

    @staticmethod
    def set_state(key: str, value: Any) -> None:
        """
        Set a value in session state.

        Args:
            key: Key name without prefix
            value: Value to set
        """
        try:
            import streamlit as st

            st.session_state[f"{STATE_PREFIX}{key}"] = value
        except ImportError:
            logger.warning(f"Streamlit not available, cannot set {key}")

    @staticmethod
    def to_training_state() -> TrainingState:
        """
        Create TrainingState from current session state.

        Returns:
            TrainingState populated from session state
        """
        return TrainingState(
            status=TrainingStateManager.get_state("status"),
            current_epoch=TrainingStateManager.get_state("current_epoch"),
            total_epochs=TrainingStateManager.get_state("total_epochs"),
            loss_history=TrainingStateManager.get_state("loss_history") or [],
            best_epoch=TrainingStateManager.get_state("best_epoch"),
            best_val_loss=TrainingStateManager.get_state("best_val_loss"),
            fixed_recon_indices=TrainingStateManager.get_state("fixed_recon_indices"),
            reconstruction_images=TrainingStateManager.get_state("reconstruction_images"),
            start_time=TrainingStateManager.get_state("start_time"),
            elapsed_seconds=TrainingStateManager.get_state("elapsed_seconds"),
            error_message=TrainingStateManager.get_state("error_message"),
            training_log=TrainingStateManager.get_state("training_log") or [],
        )

    @staticmethod
    def from_training_state(state: TrainingState) -> None:
        """
        Update session state from TrainingState.

        Args:
            state: TrainingState to persist
        """
        TrainingStateManager.set_state("status", state.status)
        TrainingStateManager.set_state("current_epoch", state.current_epoch)
        TrainingStateManager.set_state("total_epochs", state.total_epochs)
        TrainingStateManager.set_state("loss_history", state.loss_history)
        TrainingStateManager.set_state("best_epoch", state.best_epoch)
        TrainingStateManager.set_state("best_val_loss", state.best_val_loss)
        TrainingStateManager.set_state("fixed_recon_indices", state.fixed_recon_indices)
        TrainingStateManager.set_state("reconstruction_images", state.reconstruction_images)
        TrainingStateManager.set_state("start_time", state.start_time)
        TrainingStateManager.set_state("elapsed_seconds", state.elapsed_seconds)
        TrainingStateManager.set_state("error_message", state.error_message)
        TrainingStateManager.set_state("training_log", state.training_log)

    @staticmethod
    def update_epoch(
        epoch: int,
        train_loss: float,
        val_loss: float,
        train_kl: float = 0.0,
        val_kl: float = 0.0,
        train_recon: float = 0.0,
        val_recon: float = 0.0,
        train_cond: float = 0.0,
        val_cond: float = 0.0,
    ) -> None:
        """
        Update session state after epoch completion.

        Args:
            epoch: Completed epoch number (0-indexed)
            train_loss: Training loss for this epoch
            val_loss: Validation loss for this epoch
            train_kl: Training KL divergence loss
            val_kl: Validation KL divergence loss
            train_recon: Training reconstruction loss
            val_recon: Validation reconstruction loss
            train_cond: Training conditional loss
            val_cond: Validation conditional loss
        """
        loss_history = TrainingStateManager.get_state("loss_history") or []
        loss_history.append({
            "epoch": epoch,
            "train_loss": train_loss,
            "val_loss": val_loss,
            "train_kl": train_kl,
            "val_kl": val_kl,
            "train_recon": train_recon,
            "val_recon": val_recon,
            "train_cond": train_cond,
            "val_cond": val_cond,
        })

        TrainingStateManager.set_state("loss_history", loss_history)
        TrainingStateManager.set_state("current_epoch", epoch + 1)

        # Update best epoch tracking
        best_val_loss = TrainingStateManager.get_state("best_val_loss")
        if val_loss < best_val_loss:
            TrainingStateManager.set_state("best_val_loss", val_loss)
            TrainingStateManager.set_state("best_epoch", epoch)

    @staticmethod
    def add_log_message(message: str) -> None:
        """
        Add a message to the training log.

        Args:
            message: Log message to add
        """
        log = TrainingStateManager.get_state("training_log") or []
        timestamp = datetime.now().strftime("%H:%M:%S")
        log.append(f"[{timestamp}] {message}")
        TrainingStateManager.set_state("training_log", log)
        logger.info(message)

    @staticmethod
    def set_error(message: str) -> None:
        """
        Set error state with message.

        Args:
            message: Error message
        """
        TrainingStateManager.set_state("status", "error")
        TrainingStateManager.set_state("error_message", message)
        TrainingStateManager.add_log_message(f"ERROR: {message}")
        logger.error(message)
