"""
Purpose: Detection training state management for Varasana Streamlit app
Author: Hallett Lab
Date: 2026-02-04

Provides DetectionTrainingState dataclass and DetectionStateManager for handling
FCOS object detection training lifecycle with curriculum learning support.
"""

from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Literal, Optional

from candescence.core.logging_config import get_logger
from candescence.core.morphology import CURRICULUM_STAGES

logger = get_logger("candescence.interface.training.detection_state")

# Session state key prefix to avoid conflicts with TLV training
STATE_PREFIX = "varasana_"

# Curriculum stage names in order
STAGE_NAMES = ["white", "opaque", "gray", "shmoo", "pseudohyphae", "hyphae"]

# Valid training status values
DetectionTrainingStatus = Literal[
    "idle",
    "configuring",
    "validating",
    "training",
    "paused",
    "evaluating",
    "completed",
    "cancelled",
    "error",
]


@dataclass
class DetectionTrainingState:
    """
    Training state container for FCOS object detection.

    Supports curriculum learning with 6 progressive stages.

    Attributes:
        status: Current training status
        training_mode: "curriculum" or "standard"

        # Curriculum tracking
        current_stage: Current curriculum stage index (0-5)
        current_stage_name: Name of current stage
        total_stages: Total number of stages (6 for curriculum)
        completed_stages: List of completed stage names

        # Epoch tracking (within current stage)
        current_epoch: Current epoch within stage
        total_epochs_per_stage: Epochs to train per stage
        total_epochs: Total epochs across all stages

        # Loss tracking
        loss_history: List of loss dicts per epoch
        stage_loss_histories: Dict mapping stage name to loss history

        # Detection-specific metrics
        mAP: Mean Average Precision (COCO style)
        AP50: AP at IoU 0.5
        AP75: AP at IoU 0.75
        per_class_AP: Dict mapping class name to AP

        # Checkpoints
        stage_checkpoints: Dict mapping stage name to checkpoint path
        best_checkpoint: Path to best checkpoint so far

        # Timing
        start_time: When training started
        elapsed_seconds: Total elapsed time
        stage_times: Dict mapping stage name to elapsed time

        # Error handling
        error_message: Error message if status is 'error'
        training_log: List of log messages
    """

    status: DetectionTrainingStatus = "idle"
    training_mode: str = "curriculum"

    # Curriculum tracking
    current_stage: int = 0
    current_stage_name: str = "white"
    total_stages: int = 6
    completed_stages: List[str] = field(default_factory=list)

    # Epoch tracking
    current_epoch: int = 0
    total_epochs_per_stage: int = 5000
    total_epochs: int = 0  # Computed: stages * epochs_per_stage

    # Loss tracking
    loss_history: List[Dict[str, float]] = field(default_factory=list)
    stage_loss_histories: Dict[str, List[Dict[str, float]]] = field(default_factory=dict)

    # Best tracking
    best_epoch: int = 0
    best_val_loss: float = float("inf")
    best_mAP: float = 0.0

    # Detection metrics (after evaluation)
    mAP: Optional[float] = None
    AP50: Optional[float] = None
    AP75: Optional[float] = None
    per_class_AP: Optional[Dict[str, float]] = None

    # Checkpoints
    stage_checkpoints: Dict[str, str] = field(default_factory=dict)
    best_checkpoint: Optional[str] = None

    # Timing
    start_time: Optional[datetime] = None
    elapsed_seconds: float = 0.0
    stage_times: Dict[str, float] = field(default_factory=dict)

    # Error handling
    error_message: Optional[str] = None

    # Log
    training_log: List[str] = field(default_factory=list)

    def get_progress_fraction(self) -> float:
        """Get overall training progress as a fraction between 0 and 1."""
        if self.training_mode == "curriculum":
            # Progress across all stages
            completed_epochs = (
                self.current_stage * self.total_epochs_per_stage + self.current_epoch
            )
            total = self.total_stages * self.total_epochs_per_stage
            return min(1.0, completed_epochs / total) if total > 0 else 0.0
        else:
            # Standard mode - just epochs
            if self.total_epochs == 0:
                return 0.0
            return min(1.0, self.current_epoch / self.total_epochs)

    def get_stage_progress_fraction(self) -> float:
        """Get progress within current stage as a fraction."""
        if self.total_epochs_per_stage == 0:
            return 0.0
        return min(1.0, self.current_epoch / self.total_epochs_per_stage)

    def get_eta_seconds(self) -> Optional[float]:
        """Estimate remaining time based on average epoch duration."""
        if self.current_epoch == 0 or self.elapsed_seconds == 0:
            return None

        avg_epoch_time = self.elapsed_seconds / (
            self.current_stage * self.total_epochs_per_stage + self.current_epoch
        )

        if self.training_mode == "curriculum":
            remaining_epochs = (
                (self.total_stages - self.current_stage) * self.total_epochs_per_stage
                - self.current_epoch
            )
        else:
            remaining_epochs = self.total_epochs - self.current_epoch

        return avg_epoch_time * remaining_epochs

    def format_eta(self) -> str:
        """Format estimated time remaining as human-readable string."""
        eta = self.get_eta_seconds()
        if eta is None:
            return "Calculating..."

        if eta < 60:
            return f"{int(eta)}s"
        elif eta < 3600:
            minutes = int(eta / 60)
            return f"{minutes}m"
        else:
            hours = int(eta / 3600)
            minutes = int((eta % 3600) / 60)
            return f"{hours}h {minutes}m"

    def format_elapsed(self) -> str:
        """Format elapsed time as human-readable string."""
        elapsed = self.elapsed_seconds
        if elapsed < 60:
            return f"{int(elapsed)}s"
        elif elapsed < 3600:
            minutes = int(elapsed / 60)
            seconds = int(elapsed % 60)
            return f"{minutes}m {seconds}s"
        else:
            hours = int(elapsed / 3600)
            minutes = int((elapsed % 3600) / 60)
            return f"{hours}h {minutes}m"

    def get_latest_metrics(self) -> Optional[Dict[str, float]]:
        """Get metrics from the most recent epoch."""
        if not self.loss_history:
            return None
        return self.loss_history[-1]

    def get_stage_summary(self) -> Dict[str, Any]:
        """Get summary for current stage."""
        return {
            "stage_index": self.current_stage,
            "stage_name": self.current_stage_name,
            "epoch": self.current_epoch,
            "total_epochs": self.total_epochs_per_stage,
            "progress": self.get_stage_progress_fraction(),
            "completed_stages": self.completed_stages,
        }

    def get_training_summary(self) -> Dict[str, Any]:
        """Get overall training summary."""
        latest = self.get_latest_metrics()
        return {
            "status": self.status,
            "mode": self.training_mode,
            "current_stage": self.current_stage_name,
            "stages_completed": len(self.completed_stages),
            "total_stages": self.total_stages,
            "epochs_in_stage": self.current_epoch,
            "best_mAP": self.mAP,
            "best_checkpoint": self.best_checkpoint,
            "final_loss": latest.get("total_loss") if latest else None,
            "elapsed_time": self.format_elapsed(),
        }


class DetectionStateManager:
    """
    Manages detection training state and Streamlit session state persistence.

    Similar to TrainingStateManager but with detection-specific fields.
    """

    # Default values for session state initialization
    DEFAULTS: Dict[str, Any] = {
        # Status
        f"{STATE_PREFIX}status": "idle",
        f"{STATE_PREFIX}training_mode": "curriculum",
        # Curriculum tracking
        f"{STATE_PREFIX}current_stage": 0,
        f"{STATE_PREFIX}current_stage_name": "white",
        f"{STATE_PREFIX}total_stages": 6,
        f"{STATE_PREFIX}completed_stages": [],
        # Epoch tracking
        f"{STATE_PREFIX}current_epoch": 0,
        f"{STATE_PREFIX}total_epochs_per_stage": 5000,
        f"{STATE_PREFIX}total_epochs": 0,
        # Loss tracking
        f"{STATE_PREFIX}loss_history": [],
        f"{STATE_PREFIX}stage_loss_histories": {},
        f"{STATE_PREFIX}best_epoch": 0,
        f"{STATE_PREFIX}best_val_loss": float("inf"),
        f"{STATE_PREFIX}best_mAP": 0.0,
        # Detection metrics
        f"{STATE_PREFIX}mAP": None,
        f"{STATE_PREFIX}AP50": None,
        f"{STATE_PREFIX}AP75": None,
        f"{STATE_PREFIX}per_class_AP": None,
        # Checkpoints
        f"{STATE_PREFIX}stage_checkpoints": {},
        f"{STATE_PREFIX}best_checkpoint": None,
        # Configuration
        f"{STATE_PREFIX}config_dict": {},
        # Timing
        f"{STATE_PREFIX}start_time": None,
        f"{STATE_PREFIX}elapsed_seconds": 0.0,
        f"{STATE_PREFIX}stage_times": {},
        # Control flags
        f"{STATE_PREFIX}should_stop": False,
        f"{STATE_PREFIX}should_pause": False,
        f"{STATE_PREFIX}should_skip_stage": False,
        # Training objects
        f"{STATE_PREFIX}trainer": None,
        # Log
        f"{STATE_PREFIX}training_log": [],
        # Error
        f"{STATE_PREFIX}error_message": None,
        # Output
        f"{STATE_PREFIX}output_dir": None,
    }

    @staticmethod
    def initialize_session_state() -> None:
        """Initialize Streamlit session state with default values."""
        try:
            import streamlit as st

            for key, default in DetectionStateManager.DEFAULTS.items():
                if key not in st.session_state:
                    st.session_state[key] = default
        except ImportError:
            logger.warning("Streamlit not available, session state not initialized")

    @staticmethod
    def reset_session_state() -> None:
        """Reset all detection training session state to defaults."""
        try:
            import streamlit as st

            for key, default in DetectionStateManager.DEFAULTS.items():
                st.session_state[key] = default
            logger.info("Detection training session state reset")
        except ImportError:
            logger.warning("Streamlit not available")

    @staticmethod
    def get_state(key: str) -> Any:
        """Get a value from session state."""
        try:
            import streamlit as st

            full_key = f"{STATE_PREFIX}{key}"
            return st.session_state.get(
                full_key, DetectionStateManager.DEFAULTS.get(full_key)
            )
        except ImportError:
            return DetectionStateManager.DEFAULTS.get(f"{STATE_PREFIX}{key}")

    @staticmethod
    def set_state(key: str, value: Any) -> None:
        """Set a value in session state."""
        try:
            import streamlit as st

            st.session_state[f"{STATE_PREFIX}{key}"] = value
        except ImportError:
            logger.warning(f"Streamlit not available, cannot set {key}")

    @staticmethod
    def to_training_state() -> DetectionTrainingState:
        """Create DetectionTrainingState from current session state."""
        return DetectionTrainingState(
            status=DetectionStateManager.get_state("status"),
            training_mode=DetectionStateManager.get_state("training_mode"),
            current_stage=DetectionStateManager.get_state("current_stage"),
            current_stage_name=DetectionStateManager.get_state("current_stage_name"),
            total_stages=DetectionStateManager.get_state("total_stages"),
            completed_stages=DetectionStateManager.get_state("completed_stages") or [],
            current_epoch=DetectionStateManager.get_state("current_epoch"),
            total_epochs_per_stage=DetectionStateManager.get_state("total_epochs_per_stage"),
            total_epochs=DetectionStateManager.get_state("total_epochs"),
            loss_history=DetectionStateManager.get_state("loss_history") or [],
            stage_loss_histories=DetectionStateManager.get_state("stage_loss_histories") or {},
            best_epoch=DetectionStateManager.get_state("best_epoch"),
            best_val_loss=DetectionStateManager.get_state("best_val_loss"),
            best_mAP=DetectionStateManager.get_state("best_mAP"),
            mAP=DetectionStateManager.get_state("mAP"),
            AP50=DetectionStateManager.get_state("AP50"),
            AP75=DetectionStateManager.get_state("AP75"),
            per_class_AP=DetectionStateManager.get_state("per_class_AP"),
            stage_checkpoints=DetectionStateManager.get_state("stage_checkpoints") or {},
            best_checkpoint=DetectionStateManager.get_state("best_checkpoint"),
            start_time=DetectionStateManager.get_state("start_time"),
            elapsed_seconds=DetectionStateManager.get_state("elapsed_seconds"),
            stage_times=DetectionStateManager.get_state("stage_times") or {},
            error_message=DetectionStateManager.get_state("error_message"),
            training_log=DetectionStateManager.get_state("training_log") or [],
        )

    @staticmethod
    def update_epoch(
        epoch: int,
        cls_loss: float,
        bbox_loss: float,
        centerness_loss: float,
        total_loss: float,
        lr: Optional[float] = None,
    ) -> None:
        """
        Update session state after epoch completion.

        Args:
            epoch: Completed epoch number (0-indexed within stage)
            cls_loss: Classification loss (FocalLoss)
            bbox_loss: Bounding box regression loss (IoULoss)
            centerness_loss: Centerness loss (CrossEntropyLoss)
            total_loss: Combined total loss
            lr: Current learning rate
        """
        loss_history = DetectionStateManager.get_state("loss_history") or []
        loss_entry = {
            "epoch": epoch,
            "stage": DetectionStateManager.get_state("current_stage_name"),
            "cls_loss": cls_loss,
            "bbox_loss": bbox_loss,
            "centerness_loss": centerness_loss,
            "total_loss": total_loss,
            "lr": lr,
        }
        loss_history.append(loss_entry)

        DetectionStateManager.set_state("loss_history", loss_history)
        DetectionStateManager.set_state("current_epoch", epoch + 1)

        # Update best tracking
        best_val_loss = DetectionStateManager.get_state("best_val_loss")
        if total_loss < best_val_loss:
            DetectionStateManager.set_state("best_val_loss", total_loss)
            DetectionStateManager.set_state("best_epoch", epoch)

    @staticmethod
    def complete_stage(stage_name: str, checkpoint_path: str, elapsed_time: float) -> None:
        """
        Mark a curriculum stage as complete.

        Args:
            stage_name: Name of completed stage
            checkpoint_path: Path to stage checkpoint
            elapsed_time: Time spent on this stage
        """
        # Update completed stages
        completed = DetectionStateManager.get_state("completed_stages") or []
        if stage_name not in completed:
            completed.append(stage_name)
        DetectionStateManager.set_state("completed_stages", completed)

        # Save checkpoint path
        checkpoints = DetectionStateManager.get_state("stage_checkpoints") or {}
        checkpoints[stage_name] = checkpoint_path
        DetectionStateManager.set_state("stage_checkpoints", checkpoints)

        # Save stage time
        times = DetectionStateManager.get_state("stage_times") or {}
        times[stage_name] = elapsed_time
        DetectionStateManager.set_state("stage_times", times)

        # Save loss history for this stage
        loss_history = DetectionStateManager.get_state("loss_history") or []
        stage_histories = DetectionStateManager.get_state("stage_loss_histories") or {}
        stage_histories[stage_name] = [
            entry for entry in loss_history
            if entry.get("stage") == stage_name
        ]
        DetectionStateManager.set_state("stage_loss_histories", stage_histories)

        # Move to next stage
        current_stage = DetectionStateManager.get_state("current_stage")
        if current_stage < 5:  # 0-5 for 6 stages
            next_stage = current_stage + 1
            DetectionStateManager.set_state("current_stage", next_stage)
            DetectionStateManager.set_state("current_stage_name", STAGE_NAMES[next_stage])
            DetectionStateManager.set_state("current_epoch", 0)

        DetectionStateManager.add_log_message(
            f"Completed stage '{stage_name}' - checkpoint: {checkpoint_path}"
        )

    @staticmethod
    def update_metrics(
        mAP: float,
        AP50: float,
        AP75: float,
        per_class_AP: Optional[Dict[str, float]] = None,
    ) -> None:
        """
        Update detection evaluation metrics.

        Args:
            mAP: Mean Average Precision
            AP50: AP at IoU 0.5
            AP75: AP at IoU 0.75
            per_class_AP: Per-class AP values
        """
        DetectionStateManager.set_state("mAP", mAP)
        DetectionStateManager.set_state("AP50", AP50)
        DetectionStateManager.set_state("AP75", AP75)
        DetectionStateManager.set_state("per_class_AP", per_class_AP)

        # Update best mAP
        best_mAP = DetectionStateManager.get_state("best_mAP") or 0.0
        if mAP > best_mAP:
            DetectionStateManager.set_state("best_mAP", mAP)

    @staticmethod
    def add_log_message(message: str) -> None:
        """Add a message to the training log."""
        log = DetectionStateManager.get_state("training_log") or []
        timestamp = datetime.now().strftime("%H:%M:%S")
        log.append(f"[{timestamp}] {message}")
        DetectionStateManager.set_state("training_log", log)
        logger.info(message)

    @staticmethod
    def set_error(message: str) -> None:
        """Set error state with message."""
        DetectionStateManager.set_state("status", "error")
        DetectionStateManager.set_state("error_message", message)
        DetectionStateManager.add_log_message(f"ERROR: {message}")
        logger.error(message)
