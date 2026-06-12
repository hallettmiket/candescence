"""
Purpose: Streamlit-compatible wrapper for FCOS detection training
Author: Hallett Lab
Date: 2026-02-04

Wraps CurriculumTrainer with callback support for real-time progress
monitoring in Streamlit.
"""

import subprocess
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

from candescence.core.logging_config import get_logger
from candescence.core.morphology import CURRICULUM_STAGES, CurriculumStage
from candescence.detection.training.curriculum_trainer import CurriculumConfig
from candescence.detection.training.log_parser import (
    MMDetectionLogParser,
    find_latest_checkpoint,
    find_latest_log_file,
)

logger = get_logger("candescence.interface.training.detection_trainer")


@dataclass
class DetectionConfig:
    """
    Configuration for Streamlit detection training.

    Extends CurriculumConfig with additional UI-specific settings.
    """

    # Data paths
    data_root: Path
    output_root: Path
    base_config: Path
    mmdetection_path: Path = Path("/home/data/analysis-tools/mmdetection")

    # Training settings
    training_mode: str = "curriculum"  # "curriculum" or "standard"
    gpu_ids: List[int] = field(default_factory=lambda: [0])
    epochs_per_stage: int = 5000
    checkpoint_interval: int = 300
    resume_from: Optional[Path] = None

    # Standard mode settings (if training_mode == "standard")
    total_epochs: int = 5000
    train_split: float = 0.8
    val_split: float = 0.1
    test_split: float = 0.1

    # Model settings
    backbone: str = "resnet101"
    pretrained: str = "open-mmlab://detectron/resnet101_caffe"
    num_classes: int = 15

    # Hyperparameters
    learning_rate: float = 0.01
    optimizer: str = "SGD"
    momentum: float = 0.9
    weight_decay: float = 0.0001
    lr_scheduler: str = "step"
    warmup_iters: int = 1000

    # FCOS specific
    center_sampling_radius: float = 1.5
    nms_threshold: float = 0.5
    score_threshold: float = 0.05
    max_per_img: int = 275

    # Logging
    log_interval: int = 15
    eval_interval: int = 1

    def to_curriculum_config(self) -> CurriculumConfig:
        """Convert to CurriculumConfig for CurriculumTrainer."""
        return CurriculumConfig(
            data_root=self.data_root,
            output_root=self.output_root,
            base_config=self.base_config,
            mmdetection_path=self.mmdetection_path,
            gpu_ids=self.gpu_ids,
            epochs_per_stage=self.epochs_per_stage,
            checkpoint_interval=self.checkpoint_interval,
            resume_from=self.resume_from,
        )


class StreamlitDetectionTrainer:
    """
    Detection trainer with Streamlit callback support.

    Wraps CurriculumTrainer to provide:
    - Progress callbacks via log file monitoring
    - Stage completion callbacks
    - Stop/pause/skip stage controls
    - Validation metric computation
    """

    def __init__(
        self,
        config: DetectionConfig,
        progress_callback: Optional[Callable[[Dict[str, Any]], None]] = None,
        stage_callback: Optional[Callable[[str, Path], None]] = None,
        stop_check: Optional[Callable[[], bool]] = None,
        pause_check: Optional[Callable[[], bool]] = None,
        skip_stage_check: Optional[Callable[[], bool]] = None,
    ) -> None:
        """
        Initialize the Streamlit detection trainer.

        Args:
            config: Detection training configuration
            progress_callback: Called with metrics dict after each logged iteration
            stage_callback: Called when a stage completes with (stage_name, checkpoint_path)
            stop_check: Returns True if training should stop
            pause_check: Returns True if training should pause
            skip_stage_check: Returns True if current stage should be skipped
        """
        self.config = config
        self.progress_callback = progress_callback or (lambda x: None)
        self.stage_callback = stage_callback or (lambda s, p: None)
        self.stop_check = stop_check or (lambda: False)
        self.pause_check = pause_check or (lambda: False)
        self.skip_stage_check = skip_stage_check or (lambda: False)

        self.stages = CURRICULUM_STAGES
        self._current_process: Optional[subprocess.Popen] = None
        self._monitor_thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._log_parser = MMDetectionLogParser()

        # Track training state
        self._current_stage: Optional[CurriculumStage] = None
        self._stage_start_time: Optional[datetime] = None
        self._training_start_time: Optional[datetime] = None

        # Validate paths
        if not config.data_root.exists():
            raise FileNotFoundError(f"Data root not found: {config.data_root}")
        if not config.base_config.exists():
            raise FileNotFoundError(f"Base config not found: {config.base_config}")

        # Create output directory
        config.output_root.mkdir(parents=True, exist_ok=True)

        logger.info("StreamlitDetectionTrainer initialized")

    def train_stage(
        self,
        stage: CurriculumStage,
        resume_from: Optional[Path] = None,
    ) -> Optional[Path]:
        """
        Train a single curriculum stage with progress monitoring.

        Args:
            stage: CurriculumStage to train
            resume_from: Optional checkpoint to resume from

        Returns:
            Path to the trained checkpoint, or None if stopped/skipped
        """
        self._current_stage = stage
        self._stage_start_time = datetime.now()

        stage_output = self.config.output_root / stage.name
        stage_output.mkdir(parents=True, exist_ok=True)

        # Generate stage config
        stage_config = self._generate_stage_config(stage, stage_output)

        logger.info(f"Training stage: {stage.name}")
        logger.info(f"Classes: {stage.cumulative_classes}")
        logger.info(f"Output: {stage_output}")

        # Build training command
        train_script = self.config.mmdetection_path / "tools" / "train.py"
        cmd = [
            "nice", "-n", "19",  # Low priority as per lab guidelines
            "python",
            str(train_script),
            str(stage_config),
            f"--work-dir={stage_output}",
            f"--gpu-ids={','.join(map(str, self.config.gpu_ids))}",
        ]

        if resume_from is not None:
            cmd.append(f"--resume-from={resume_from}")

        logger.info(f"Running: {' '.join(cmd)}")

        # Start training process
        try:
            self._current_process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )

            # Start log monitoring thread
            self._stop_event.clear()
            self._monitor_thread = threading.Thread(
                target=self._monitor_training_progress,
                args=(stage_output, stage.name),
                daemon=True,
            )
            self._monitor_thread.start()

            # Wait for process with stop/pause/skip checking
            while self._current_process.poll() is None:
                if self.stop_check():
                    logger.info("Stop requested - terminating training")
                    self._current_process.terminate()
                    self._stop_event.set()
                    return None

                if self.skip_stage_check():
                    logger.info(f"Skip requested - terminating stage {stage.name}")
                    self._current_process.terminate()
                    self._stop_event.set()
                    break

                if self.pause_check():
                    # Pause - just wait
                    time.sleep(1.0)
                    continue

                time.sleep(0.5)

            # Stop monitoring
            self._stop_event.set()
            if self._monitor_thread is not None:
                self._monitor_thread.join(timeout=5.0)

            # Check for errors
            if self._current_process.returncode not in (None, 0, -15):  # -15 is SIGTERM
                stderr = self._current_process.stderr.read() if self._current_process.stderr else ""
                logger.error(f"Training failed: {stderr}")
                raise RuntimeError(f"Training failed with code {self._current_process.returncode}: {stderr}")

            logger.info(f"Stage {stage.name} completed")

        except Exception as e:
            logger.exception(f"Error during training: {e}")
            raise
        finally:
            self._current_process = None

        # Find checkpoint
        checkpoint = find_latest_checkpoint(stage_output)
        if checkpoint is None:
            logger.warning(f"No checkpoint found in {stage_output}")
            return None

        # Call stage callback
        elapsed = (datetime.now() - self._stage_start_time).total_seconds()
        self.stage_callback(stage.name, checkpoint)

        return checkpoint

    def train_all_stages(
        self,
        start_stage: Optional[str] = None,
    ) -> Dict[str, Path]:
        """
        Train all curriculum stages sequentially with progress monitoring.

        Args:
            start_stage: Optional stage name to start from

        Returns:
            Dictionary mapping stage names to checkpoint paths
        """
        self._training_start_time = datetime.now()
        checkpoints: Dict[str, Path] = {}
        resume_from = self.config.resume_from

        # Find start index
        start_idx = 0
        if start_stage is not None:
            for i, stage in enumerate(self.stages):
                if stage.name == start_stage:
                    start_idx = i
                    break

        # Train each stage
        for i, stage in enumerate(self.stages):
            if self.stop_check():
                logger.info("Stop requested - ending training")
                break

            if i < start_idx:
                # Look for existing checkpoint
                stage_output = self.config.output_root / stage.name
                checkpoint = stage_output / "latest.pth"
                if checkpoint.exists():
                    resume_from = checkpoint
                    checkpoints[stage.name] = checkpoint
                continue

            try:
                checkpoint = self.train_stage(stage, resume_from=resume_from)
                if checkpoint is not None:
                    checkpoints[stage.name] = checkpoint
                    resume_from = checkpoint
            except Exception as e:
                logger.error(f"Stage {stage.name} failed: {e}")
                raise

        logger.info("Training completed")
        return checkpoints

    def _monitor_training_progress(
        self,
        work_dir: Path,
        stage_name: str,
    ) -> None:
        """
        Background thread to monitor log files and call progress_callback.

        Args:
            work_dir: MMDETECTION work directory
            stage_name: Current stage name
        """
        logger.info(f"Starting log monitor for {work_dir}")

        # Wait for log file to appear
        log_file = None
        for _ in range(30):  # Wait up to 30 seconds
            if self._stop_event.is_set():
                return
            log_file = find_latest_log_file(work_dir)
            if log_file is not None:
                break
            time.sleep(1.0)

        if log_file is None:
            logger.warning("No log file found, monitoring disabled")
            return

        self._log_parser.set_log_file(log_file)
        logger.info(f"Monitoring log file: {log_file}")

        last_epoch = -1

        while not self._stop_event.is_set():
            try:
                # Parse new log entries
                new_metrics = self._log_parser.parse_new()

                for metrics in new_metrics:
                    # Add stage info
                    metrics["stage"] = stage_name

                    # Calculate elapsed time
                    if self._stage_start_time:
                        metrics["stage_elapsed"] = (
                            datetime.now() - self._stage_start_time
                        ).total_seconds()
                    if self._training_start_time:
                        metrics["total_elapsed"] = (
                            datetime.now() - self._training_start_time
                        ).total_seconds()

                    # Call progress callback
                    self.progress_callback(metrics)

                    # Log epoch changes
                    epoch = metrics.get("epoch", 0)
                    if epoch != last_epoch:
                        last_epoch = epoch
                        logger.debug(f"Epoch {epoch}: loss={metrics.get('loss', 'N/A')}")

            except Exception as e:
                logger.warning(f"Error monitoring log: {e}")

            time.sleep(1.0)  # Poll every second

        logger.info("Log monitor stopped")

    def _generate_stage_config(
        self,
        stage: CurriculumStage,
        output_dir: Path,
    ) -> Path:
        """Generate MMDETECTION config for a curriculum stage."""
        # Annotation files
        ann_file = self.config.data_root / f"train_{stage.name}.pkl"
        val_ann_file = self.config.data_root / f"val_{stage.name}.pkl"

        # Generate config
        stage_config_content = f'''
# Auto-generated config for curriculum stage: {stage.name}
# Classes: {stage.cumulative_classes}
# Generated: {datetime.now().isoformat()}

_base_ = ['{self.config.base_config}']

# Override data paths
data = dict(
    train=dict(
        ann_file='{ann_file}',
        img_prefix='{self.config.data_root / "train"}/',
    ),
    val=dict(
        ann_file='{val_ann_file}',
        img_prefix='{self.config.data_root / "val"}/',
    ),
    test=dict(
        ann_file='{val_ann_file}',
        img_prefix='{self.config.data_root / "val"}/',
    ),
)

# Training settings
total_epochs = {self.config.epochs_per_stage}
checkpoint_config = dict(interval={self.config.checkpoint_interval})
work_dir = '{output_dir}'
gpu_ids = {self.config.gpu_ids}

# Logging
log_config = dict(
    interval={self.config.log_interval},
    hooks=[
        dict(type='TextLoggerHook'),
    ]
)

# Learning rate
optimizer = dict(
    type='{self.config.optimizer}',
    lr={self.config.learning_rate},
    momentum={self.config.momentum},
    weight_decay={self.config.weight_decay},
)

lr_config = dict(
    policy='{self.config.lr_scheduler}',
    warmup='linear',
    warmup_iters={self.config.warmup_iters},
    warmup_ratio=0.001,
)

# Evaluation
evaluation = dict(interval={self.config.eval_interval})
'''

        config_path = output_dir / "config.py"
        with open(config_path, "w") as f:
            f.write(stage_config_content)

        logger.debug(f"Generated config: {config_path}")
        return config_path

    def evaluate_checkpoint(
        self,
        checkpoint_path: Path,
        config_path: Optional[Path] = None,
    ) -> Dict[str, float]:
        """
        Evaluate a checkpoint and return metrics.

        Args:
            checkpoint_path: Path to model checkpoint
            config_path: Path to config file (auto-detected if None)

        Returns:
            Dict with mAP, AP50, AP75, etc.
        """
        if config_path is None:
            # Look for config in same directory
            config_path = checkpoint_path.parent / "config.py"
            if not config_path.exists():
                raise FileNotFoundError(f"Config not found: {config_path}")

        # Run evaluation
        test_script = self.config.mmdetection_path / "tools" / "test.py"
        cmd = [
            "nice", "-n", "19",
            "python",
            str(test_script),
            str(config_path),
            str(checkpoint_path),
            "--eval", "bbox",
        ]

        logger.info(f"Evaluating: {checkpoint_path}")

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
        )

        # Parse results from output
        metrics = MMDetectionLogParser.parse_eval_results(
            checkpoint_path.parent / "test_results.log"
        )

        # Also try parsing stdout
        if not metrics:
            import re
            for line in result.stdout.split("\n"):
                if "mAP" in line or "AP50" in line:
                    match = re.search(r"(mAP|AP50|AP75):\s*([\d.]+)", line)
                    if match:
                        metrics[match.group(1)] = float(match.group(2))

        return metrics

    def stop_training(self) -> None:
        """Stop the current training process."""
        if self._current_process is not None:
            self._current_process.terminate()
        self._stop_event.set()

    def get_current_stage(self) -> Optional[str]:
        """Get the name of the current training stage."""
        return self._current_stage.name if self._current_stage else None

    def list_stages(self) -> List[str]:
        """Get list of all stage names in order."""
        return [stage.name for stage in self.stages]
