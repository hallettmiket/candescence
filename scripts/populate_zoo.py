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
from candescence.core.settings import get_settings, legacy_refined_root

# Resolved from the settings layer (env / config / default).
SETTINGS = get_settings()
REFINED = SETTINGS.refined_path
# Root holding the legacy candescence_master tree (Varasana/Grace source data).
LEGACY_GRACE = legacy_refined_root() / "candescence_master/projects/grace/train-data"

# Authoritative class names, read from the production checkpoints' ``meta.CLASSES``.
# TLV is a VAE project (no detection labels) — its images map to the 6 morphology
# grades the platform classifies.
TLV_GRADES = ["white", "opaque", "gray", "shmoo", "pseudohyphae", "hyphae"]
GRACE_MACRO_CLASSES = ["0", "1", "2", "time", "unknown", "artifact", "3",
                       "macrophage", "UFO"]
GRACE_TC_CLASSES = ["c0", "c1", "c2", "time", "unknown", "artifact", "c3"]


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
            # Detection-training hints (read by the modern Train Detector page to
            # pre-fill its inputs). ``train_hyphae.pkl`` is the full 15-class set.
            "engine": "detection",
            "train_pkl": str(REFINED / "varasana_data/curriculum/train_hyphae.pkl"),
            "val_pkl": str(REFINED / "varasana_data/curriculum/val_hyphae.pkl"),
            "train_image_dir": str(REFINED / "varasana_data/images"),
            "num_classes": 15,
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

    # 4. TLV colony image corpus (the VAE training images)
    zoo.register(
        dataset_id="tlv_images",
        name="TLV Colony Image Corpus",
        project="tlv",
        path=SETTINGS.image_dir,
        num_samples=50880,
        splits={"all": 50880},
        format="image_dir",
        classes=TLV_GRADES,
        description=(
            "The TLV image corpus was originally developed for the TLV "
            "(Tendril Latent VAE) project — Candida albicans colony microscopy "
            "images (BMP) used to train the variational autoencoders and the "
            "diffusion companion. Unlabelled for detection; conditioning "
            "variables (e.g. average_hue) are derived from the images."
        ),
        metadata={
            "image_format": "bmp",
            "engine": "vae",
            "metadata_xlsx": str(SETTINGS.metadata_xlsx),
        },
    )

    # 5. TLV manually-labelled morphologies (the supervised subset)
    zoo.register(
        dataset_id="tlv_manual_labels",
        name="TLV Manually-Labelled Morphologies",
        project="tlv",
        path=SETTINGS.manual_labels_csv,
        num_samples=34176,
        splits={"labelled": 34176},
        format="csv_labels",
        classes=TLV_GRADES,
        description=(
            "Hand-labelled morphology grades for TLV colony images "
            "(file_name, morphology). Originally produced for the TLV project to "
            "validate VAE latent structure and to condition/evaluate the models "
            "against the 6 morphology grades."
        ),
        metadata={"engine": "vae", "label_column": "morphology"},
    )

    # 6. Grace macro detection data (9-class)
    zoo.register(
        dataset_id="grace_macro",
        name="Grace Macro Detection Data (9-class)",
        project="grace",
        path=LEGACY_GRACE / "grace_macro",
        num_samples=82,
        splits={"train": 57, "val": 25},
        format="curriculum_pkl",
        classes=GRACE_MACRO_CLASSES,
        description=(
            "The Grace macro dataset was originally developed for the Grace macro "
            "FCOS classifier (Case, Westman et al. 2023, mBio; Cowen Lab) — "
            "9-class macrophage-interaction morphology detection. Per-image "
            "bounding boxes in PKL format (train/val), 800x800 BMP frames."
        ),
        metadata={
            "engine": "detection",
            "annotation_format": "pkl",
            "train_pkl": str(LEGACY_GRACE / "grace_macro/train_grace_macro.pkl"),
            "val_pkl": str(LEGACY_GRACE / "grace_macro/val_grace_macro.pkl"),
            "train_image_dir": str(LEGACY_GRACE / "grace_macro/train"),
            "num_classes": 9,
        },
    )

    # 7. Grace TC (tissue-culture) detection data (7-class)
    zoo.register(
        dataset_id="grace_tc",
        name="Grace TC Detection Data (7-class)",
        project="grace",
        path=LEGACY_GRACE / "grace_tc",
        num_samples=98,
        splits={"train": 71, "val": 27},
        format="curriculum_pkl",
        classes=GRACE_TC_CLASSES,
        description=(
            "The Grace TC dataset was originally developed for the Grace "
            "tissue-culture FCOS classifier (Case, Westman et al. 2023, mBio; "
            "Cowen Lab) — 7-class cell-level morphology detection. Per-image "
            "bounding boxes in PKL format (train/val), 800x800 BMP frames."
        ),
        metadata={
            "engine": "detection",
            "annotation_format": "pkl",
            "train_pkl": str(LEGACY_GRACE / "grace_tc/train_grace_tc.pkl"),
            "val_pkl": str(LEGACY_GRACE / "grace_tc/val_grace_tc.pkl"),
            "train_image_dir": str(LEGACY_GRACE / "grace_tc/train"),
            "num_classes": 7,
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
