"""
Purpose: Curriculum learning trainer for FCOS detector
Author: Hallett Lab
Date: 2026-01-28
Input: Training configuration and data paths
Output: Trained FCOS model checkpoints

Implements curriculum learning strategy for training the FCOS detector
progressively from simpler to more complex morphology classes.
"""

import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

from candescence.core.logging_config import get_logger
from candescence.core.morphology import CURRICULUM_STAGES, CurriculumStage

logger = get_logger("candescence.detection.curriculum_trainer")


@dataclass
class CurriculumConfig:
    """
    Configuration for curriculum training.

    Attributes:
        data_root: Root directory containing training data
        output_root: Root directory for training outputs
        base_config: Path to base MMDETECTION config
        mmdetection_path: Path to MMDETECTION installation
        gpu_ids: List of GPU IDs to use
        epochs_per_stage: Number of epochs per curriculum stage
        checkpoint_interval: Save checkpoint every N epochs
    """
    data_root: Path
    output_root: Path
    base_config: Path
    mmdetection_path: Path = Path("/home/data/analysis-tools/mmdetection")
    gpu_ids: List[int] = field(default_factory=lambda: [0])
    epochs_per_stage: int = 5000
    checkpoint_interval: int = 300
    resume_from: Optional[Path] = None


class CurriculumTrainer:
    """
    Curriculum learning trainer for FCOS detector.

    Trains the detector progressively through morphology stages:
    1. white (Yeast White, Budding White)
    2. opaque (+ Yeast Opaque, Budding Opaque)
    3. gray (+ Yeast Gray, Budding Gray)
    4. shmoo (+ Shmoo)
    5. pseudohyphae (+ Pseudohyphae, P-Start, P-junction)
    6. hyphae (+ Hyphae, H-Start, H-junction)

    Each stage uses the previous stage's checkpoint as initialization.

    Example:
        >>> config = CurriculumConfig(
        ...     data_root=Path("/data/train"),
        ...     output_root=Path("/data/output"),
        ...     base_config=Path("configs/fcos.py"),
        ... )
        >>> trainer = CurriculumTrainer(config)
        >>> trainer.train_all_stages()
    """

    def __init__(self, config: CurriculumConfig) -> None:
        """
        Initialize the curriculum trainer.

        Args:
            config: Training configuration
        """
        self.config = config
        self.stages = CURRICULUM_STAGES

        # Validate paths
        if not config.data_root.exists():
            raise FileNotFoundError(f"Data root not found: {config.data_root}")
        if not config.base_config.exists():
            raise FileNotFoundError(f"Base config not found: {config.base_config}")

        # Create output directory
        config.output_root.mkdir(parents=True, exist_ok=True)

        logger.info(f"CurriculumTrainer initialized with {len(self.stages)} stages")

    def train_stage(
        self,
        stage: CurriculumStage,
        resume_from: Optional[Path] = None,
    ) -> Path:
        """
        Train a single curriculum stage.

        Args:
            stage: CurriculumStage to train
            resume_from: Optional checkpoint to resume from

        Returns:
            Path to the trained checkpoint
        """
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
            "python",
            str(train_script),
            str(stage_config),
            f"--work-dir={stage_output}",
            f"--gpu-ids={','.join(map(str, self.config.gpu_ids))}",
        ]

        if resume_from is not None:
            cmd.append(f"--resume-from={resume_from}")

        logger.info(f"Running: {' '.join(cmd)}")

        # Run training
        try:
            result = subprocess.run(
                cmd,
                check=True,
                capture_output=True,
                text=True,
            )
            logger.info(f"Stage {stage.name} completed successfully")
        except subprocess.CalledProcessError as e:
            logger.error(f"Training failed: {e.stderr}")
            raise

        # Return path to latest checkpoint
        checkpoint = stage_output / "latest.pth"
        if not checkpoint.exists():
            # Find the last epoch checkpoint
            checkpoints = list(stage_output.glob("epoch_*.pth"))
            if checkpoints:
                checkpoint = max(checkpoints, key=lambda p: int(p.stem.split("_")[1]))
            else:
                raise FileNotFoundError(f"No checkpoint found in {stage_output}")

        return checkpoint

    def train_all_stages(
        self,
        start_stage: Optional[str] = None,
    ) -> Dict[str, Path]:
        """
        Train all curriculum stages sequentially.

        Args:
            start_stage: Optional stage name to start from.
                        Previous stages are skipped.

        Returns:
            Dictionary mapping stage names to checkpoint paths
        """
        checkpoints = {}
        resume_from = self.config.resume_from

        # Find start index
        start_idx = 0
        if start_stage is not None:
            for i, stage in enumerate(self.stages):
                if stage.name == start_stage:
                    start_idx = i
                    break
            else:
                raise ValueError(f"Unknown stage: {start_stage}")

        # Train each stage
        for i, stage in enumerate(self.stages):
            if i < start_idx:
                # Look for existing checkpoint
                stage_output = self.config.output_root / stage.name
                checkpoint = stage_output / "latest.pth"
                if checkpoint.exists():
                    resume_from = checkpoint
                    checkpoints[stage.name] = checkpoint
                continue

            checkpoint = self.train_stage(stage, resume_from=resume_from)
            checkpoints[stage.name] = checkpoint
            resume_from = checkpoint

        logger.info("All curriculum stages completed")
        return checkpoints

    def _generate_stage_config(
        self,
        stage: CurriculumStage,
        output_dir: Path,
    ) -> Path:
        """
        Generate MMDETECTION config for a curriculum stage.

        Args:
            stage: CurriculumStage to configure
            output_dir: Output directory for this stage

        Returns:
            Path to generated config file
        """
        # Read base config
        with open(self.config.base_config, "r") as f:
            base_config = f.read()

        # Annotation file for this stage
        ann_file = self.config.data_root / f"train_{stage.name}.pkl"
        val_ann_file = self.config.data_root / f"val_{stage.name}.pkl"

        # Generate stage-specific config
        stage_config_content = f'''
# Auto-generated config for curriculum stage: {stage.name}
# Classes: {stage.cumulative_classes}

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
'''

        # Write config
        config_path = output_dir / "config.py"
        with open(config_path, "w") as f:
            f.write(stage_config_content)

        logger.debug(f"Generated config: {config_path}")
        return config_path

    def get_stage_info(self, stage_name: str) -> CurriculumStage:
        """
        Get information about a curriculum stage.

        Args:
            stage_name: Name of the stage

        Returns:
            CurriculumStage object
        """
        for stage in self.stages:
            if stage.name == stage_name:
                return stage
        raise ValueError(f"Unknown stage: {stage_name}")

    def list_stages(self) -> List[str]:
        """Get list of all stage names in order."""
        return [stage.name for stage in self.stages]
