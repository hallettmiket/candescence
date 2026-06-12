"""
Purpose: Populate the Candescence model zoo with all known models and datasets
Author: Hallett Lab
Date: 2026-02-05

Run once to register existing production models and datasets into the
unified model/dataset zoo (resolved from the settings layer: get_settings().zoo_path).

Usage:
    python scripts/populate_zoo.py
"""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from candescence.core.model_zoo import ModelZoo
from candescence.core.dataset_zoo import DatasetZoo
from candescence.core.settings import get_settings

# Refined-data root, resolved from the settings layer (env / config / default).
REFINED = get_settings().refined_path


def populate_models() -> None:
    """Register all known production models."""
    zoo = ModelZoo()

    # 1. Varasana FCOS v1 (production)
    zoo.register(
        model_id="varasana_fcos_v1",
        name="Varasana FCOS v1",
        project="varasana",
        model_type="production",
        version="1.0",
        architecture="fcos_resnet101_fpn",
        checkpoint="model.pth",
        config_file="config.py",
        description=(
            "Production FCOS object detection model for Candida morphology. "
            "ResNet-101 backbone with FPN neck, trained with curriculum learning "
            "across 6 stages (white → hyphae). 15 output classes."
        ),
        tags=["fcos", "detection", "curriculum", "resnet101", "fpn"],
        training_config={
            "backbone": "resnet101",
            "num_classes": 15,
            "epochs_per_stage": 4800,
            "checkpoint_interval": 300,
            "learning_rate": 0.01,
            "batch_size": 1,
            "input_size": [800, 800],
            "nms_threshold": 0.5,
            "max_per_img": 275,
        },
    )

    # 2. Tendril VAE v1 (production) - already exists in candescence_new
    tendril_path = REFINED / "training_20260201/run_162807/models"
    if tendril_path.exists():
        zoo.register(
            model_id="tendril_vae_v1",
            name="Tendril VAE v1",
            project="tlv",
            model_type="production",
            version="1.0",
            architecture="tendril_vae",
            checkpoint="model.pth",
            config_file="args.json",
            path=tendril_path,
            description=(
                "Production Tendril VAE trained on day 2 images. "
                "Strategy 14, latent_dim=128, conditioned on average_hue."
            ),
            tags=["vae", "tendril", "strategy_14", "day2"],
            training_config={
                "strategy": 14,
                "latent_dim": 128,
                "conditional_variables": ["average_hue"],
                "number_epochs": 100,
                "batch_size": 256,
            },
        )

    # 3. Grace TC v1 (production)
    zoo.register(
        model_id="grace_tc_v1",
        name="Grace Cell Classifier v1",
        project="grace",
        model_type="production",
        version="1.0",
        architecture="fcos_resnet101_fpn",
        checkpoint="tc_model.pth",
        config_file="tc_config.py",
        description=(
            "Grace cell-level classification model from the Cowen Lab "
            "collaboration. FCOS-based detection for transfer learning."
        ),
        tags=["fcos", "detection", "grace", "cowen_lab", "cell_classification"],
    )

    # 4. Grace Macro v1 (production)
    zoo.register(
        model_id="grace_macro_v1",
        name="Grace Macro Classifier v1",
        project="grace",
        model_type="production",
        version="1.0",
        architecture="fcos_resnet101_fpn",
        checkpoint="macro_model.pth",
        config_file="macro_config.py",
        description=(
            "Grace macroscopic classification model from the Cowen Lab "
            "collaboration. FCOS-based detection for colony morphology."
        ),
        tags=["fcos", "detection", "grace", "cowen_lab", "macro_classification"],
    )

    print(f"Registered {len(zoo)} models in zoo")
    for entry in zoo.list_models():
        status = "✅" if entry.exists() else "⚠️ (file not found)"
        print(f"  {entry.id}: {entry.name} [{entry.project}/{entry.model_type}] {status}")


def populate_datasets() -> None:
    """Register all known datasets."""
    zoo = DatasetZoo()

    morphology_classes = [
        "yeast_white", "budding_white", "yeast_opaque", "budding_opaque",
        "yeast_gray", "budding_gray", "shmoo", "artifact", "unknown",
        "pseudohyphae", "hyphae", "h_junction", "p_junction", "p_start", "h_start",
    ]

    # 1. Varasana curriculum dataset
    zoo.register(
        dataset_id="varasana_curriculum",
        name="Varasana Curriculum Training Data",
        project="varasana",
        path=REFINED / "varasana_data/curriculum",
        num_samples=1096,
        splits={"train": 775, "val": 321},
        format="curriculum_pkl",
        classes=morphology_classes,
        description=(
            "Curriculum learning dataset for FCOS training. Contains per-stage "
            "PKL annotation files (train/val for white, opaque, gray, shmoo, "
            "pseudohyphae, hyphae). Images are BMP format, ~4MB each."
        ),
        metadata={
            "image_format": "bmp",
            "annotation_format": "pkl",
            "curriculum_stages": ["white", "opaque", "gray", "shmoo", "pseudohyphae", "hyphae"],
            "image_dir": str(REFINED / "varasana_data/images"),
        },
    )

    # 2. Varasana test dataset
    zoo.register(
        dataset_id="varasana_test",
        name="Varasana Test Images",
        project="varasana",
        path=REFINED / "varasana_data/test_images",
        num_samples=900,
        splits={"test": 900},
        format="image_dir",
        classes=morphology_classes,
        description=(
            "Independent test set for Varasana model evaluation. "
            "900 BMP images from various Candida strain/condition combinations."
        ),
        metadata={
            "image_format": "bmp",
            "annotation_file": str(REFINED / "varasana_data/annotations/varasana.json"),
        },
    )

    # 3. Varasana full annotations
    zoo.register(
        dataset_id="varasana_annotations",
        name="Varasana Labelbox Annotations",
        project="varasana",
        path=REFINED / "varasana_data/annotations",
        num_samples=0,  # Annotation file, not directly countable
        format="labelbox_json",
        classes=morphology_classes,
        description=(
            "Original Labelbox annotations (varasana.json) containing bounding box "
            "coordinates and morphology labels for detection training."
        ),
        metadata={
            "annotation_format": "labelbox_json",
            "file": "varasana.json",
        },
    )

    print(f"Registered {len(zoo)} datasets in zoo")
    for entry in zoo.list_datasets():
        status = "✅" if entry.exists() else "⚠️ (path not found)"
        print(f"  {entry.id}: {entry.name} [{entry.project}] ({entry.num_samples} samples) {status}")


if __name__ == "__main__":
    print("=== Populating Model Zoo ===")
    populate_models()
    print()
    print("=== Populating Dataset Zoo ===")
    populate_datasets()
    print()
    print(f"Done! Registry files saved to {get_settings().zoo_path}/")
