"""
Purpose: Tests for Candescence detection module
Author: Hallett Lab
Date: 2026-01-28
"""

import pytest
import numpy as np
from pathlib import Path
from unittest.mock import MagicMock, patch

from candescence.core.morphology import (
    CLASS_NAMES,
    CLASS_TO_INDEX,
    INDEX_TO_CLASS,
    NUM_CLASSES,
    CURRICULUM_STAGES,
    MorphologyClass,
    get_curriculum_stage,
    get_class_indices,
)
from candescence.detection.inference.detector import Detection


class TestMorphologyClasses:
    """Tests for morphology class definitions."""

    def test_class_count(self):
        """Test that we have 15 classes."""
        assert NUM_CLASSES == 15
        assert len(CLASS_NAMES) == 15

    def test_class_mappings_consistent(self):
        """Test that class mappings are bijective."""
        for name, idx in CLASS_TO_INDEX.items():
            assert INDEX_TO_CLASS[idx] == name

        for idx, name in INDEX_TO_CLASS.items():
            assert CLASS_TO_INDEX[name] == idx

    def test_morphology_enum(self):
        """Test MorphologyClass enum values."""
        assert MorphologyClass.YEAST_WHITE == 0
        assert MorphologyClass.HYPHAE == 10
        assert MorphologyClass.H_START == 14

    def test_curriculum_stages(self):
        """Test curriculum learning stages."""
        assert len(CURRICULUM_STAGES) == 6

        # Check stage names
        stage_names = [s.name for s in CURRICULUM_STAGES]
        assert stage_names == [
            "white", "opaque", "gray", "shmoo", "pseudohyphae", "hyphae"
        ]

        # Check cumulative nature
        for i in range(1, len(CURRICULUM_STAGES)):
            prev_classes = set(CURRICULUM_STAGES[i - 1].cumulative_classes)
            curr_classes = set(CURRICULUM_STAGES[i].cumulative_classes)
            assert prev_classes.issubset(curr_classes)

    def test_get_curriculum_stage(self):
        """Test curriculum stage lookup."""
        stage = get_curriculum_stage("white")
        assert stage.name == "white"
        assert "Yeast White" in stage.classes

        with pytest.raises(ValueError):
            get_curriculum_stage("invalid_stage")

    def test_get_class_indices(self):
        """Test class name to index conversion."""
        indices = get_class_indices(["Yeast White", "Hyphae"])
        assert indices == [0, 10]


class TestDetection:
    """Tests for Detection dataclass."""

    def test_detection_properties(self):
        """Test Detection property calculations."""
        det = Detection(
            bbox=(10.0, 20.0, 50.0, 80.0),
            label=0,
            label_name="Yeast White",
            score=0.95,
        )

        assert det.x1 == 10.0
        assert det.y1 == 20.0
        assert det.x2 == 50.0
        assert det.y2 == 80.0
        assert det.width == 40.0
        assert det.height == 60.0
        assert det.area == 2400.0
        assert det.center == (30.0, 50.0)

    def test_detection_score_range(self):
        """Test that detection scores are in valid range."""
        det = Detection(
            bbox=(0, 0, 10, 10),
            label=0,
            label_name="Yeast White",
            score=0.5,
        )
        assert 0.0 <= det.score <= 1.0


class TestCandidaDetector:
    """Tests for CandidaDetector (mocked MMDETECTION)."""

    def test_detector_default_paths(self):
        """Test that default paths are set correctly."""
        from candescence.detection.inference.detector import CandidaDetector

        assert CandidaDetector.DEFAULT_CONFIG.name == "config.py"
        assert CandidaDetector.DEFAULT_CHECKPOINT.name == "model.pth"

    @patch('candescence.detection.inference.detector.CandidaDetector.__init__')
    def test_detector_classes_property(self, mock_init):
        """Test detector classes property."""
        mock_init.return_value = None
        from candescence.detection.inference.detector import CandidaDetector

        detector = CandidaDetector.__new__(CandidaDetector)
        detector.model = MagicMock()
        detector.model.CLASSES = CLASS_NAMES

        assert detector.classes == CLASS_NAMES
        assert detector.num_classes == 15


class TestCellExtractor:
    """Tests for CellExtractor."""

    def test_cell_crop_shape(self):
        """Test CellCrop shape property."""
        from candescence.detection.inference.cell_extractor import CellCrop

        image = np.zeros((128, 128, 3), dtype=np.uint8)
        crop = CellCrop(
            image=image,
            label=0,
            label_name="Yeast White",
            score=0.9,
        )
        assert crop.shape == (128, 128, 3)

    def test_cell_crop_grayscale(self):
        """Test grayscale CellCrop."""
        from candescence.detection.inference.cell_extractor import CellCrop

        image = np.zeros((128, 128), dtype=np.uint8)
        crop = CellCrop(
            image=image,
            label=0,
            label_name="Yeast White",
            score=0.9,
        )
        assert crop.shape == (128, 128)


class TestMorphologyStats:
    """Tests for MorphologyStats analysis."""

    def test_empty_stats(self):
        """Test stats with no detections."""
        from candescence.analysis.morphology_stats import MorphologyStats

        stats = MorphologyStats()
        summary = stats.summary()
        assert "error" in summary

    def test_add_detections(self):
        """Test adding detections to stats."""
        from candescence.analysis.morphology_stats import MorphologyStats

        stats = MorphologyStats()
        detections = [
            Detection(bbox=(0, 0, 10, 10), label=0, label_name="Yeast White", score=0.9),
            Detection(bbox=(20, 20, 30, 30), label=10, label_name="Hyphae", score=0.8),
        ]

        image_stats = stats.add_detections(Path("test.jpg"), detections)

        assert image_stats.total_detections == 2
        assert image_stats.class_counts["Yeast White"] == 1
        assert image_stats.class_counts["Hyphae"] == 1
        assert image_stats.filamentous_ratio == 0.5

    def test_summary(self):
        """Test summary statistics."""
        from candescence.analysis.morphology_stats import MorphologyStats

        stats = MorphologyStats()
        detections = [
            Detection(bbox=(0, 0, 10, 10), label=0, label_name="Yeast White", score=0.9),
            Detection(bbox=(20, 20, 30, 30), label=0, label_name="Yeast White", score=0.8),
        ]
        stats.add_detections(Path("test.jpg"), detections)

        summary = stats.summary()
        assert summary["total_images"] == 1
        assert summary["total_detections"] == 2
        assert abs(summary["confidence_mean"] - 0.85) < 1e-9

    def test_to_dataframe(self):
        """Test conversion to DataFrame."""
        from candescence.analysis.morphology_stats import MorphologyStats

        stats = MorphologyStats()
        detections = [
            Detection(bbox=(0, 0, 10, 10), label=0, label_name="Yeast White", score=0.9),
        ]
        stats.add_detections(Path("test.jpg"), detections)

        df = stats.to_dataframe()
        assert len(df) == 1
        assert "class" in df.columns
        assert df.iloc[0]["class"] == "Yeast White"


class TestCurriculumTrainer:
    """Tests for CurriculumTrainer."""

    def test_curriculum_config(self):
        """Test CurriculumConfig defaults."""
        from candescence.detection.training.curriculum_trainer import CurriculumConfig

        config = CurriculumConfig(
            data_root=Path("/tmp/data"),
            output_root=Path("/tmp/output"),
            base_config=Path("/tmp/config.py"),
        )

        assert config.epochs_per_stage == 5000
        assert config.checkpoint_interval == 300
        assert config.gpu_ids == [0]

    def test_trainer_list_stages(self):
        """Test listing curriculum stages."""
        from candescence.detection.training.curriculum_trainer import (
            CurriculumTrainer,
            CurriculumConfig,
        )

        # Mock the path checks
        with patch.object(Path, 'exists', return_value=True):
            with patch.object(Path, 'mkdir'):
                config = CurriculumConfig(
                    data_root=Path("/tmp/data"),
                    output_root=Path("/tmp/output"),
                    base_config=Path("/tmp/config.py"),
                )
                trainer = CurriculumTrainer(config)

                stages = trainer.list_stages()
                assert stages == [
                    "white", "opaque", "gray", "shmoo", "pseudohyphae", "hyphae"
                ]
