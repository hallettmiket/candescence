"""
Purpose: CLI entry point for TLV VAE training — no Streamlit required.
Author: Hallett Lab
Date: 2026-04-15

Runs a full TLV training job (outer VAE + optional tendril Phase 2) from
a JSON config file. Designed to be invoked outside the Streamlit UI so
long-running jobs survive closing the browser / disconnecting ssh.

Usage
-----

Foreground (logs stream to your terminal):

    python scripts/train_tlv.py --config my_run.json

Detached background run (survives ssh disconnect):

    nohup python scripts/train_tlv.py --config my_run.json \
        > logs/my_run.log 2>&1 &
    disown                  # removes job from this shell's job table

Check progress:

    tail -f logs/my_run.log

The config JSON uses exactly the same field names the Streamlit UI
collects (see ``_validate_and_start`` in training_app.py). A minimal
example lives at ``scripts/example_config_s15.json``. Unspecified
fields fall back to ``TLVConfig`` defaults.
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sys
from pathlib import Path
from typing import Any, Dict

# Add src to path (mirrors other scripts in this folder)
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import torch  # noqa: E402

from candescence.core.config import TLVConfig  # noqa: E402
from candescence.core.logging_config import get_logger  # noqa: E402
from candescence.tlv.architectures.tendril_sub import Tendrils  # noqa: E402
from candescence.tlv.factory import Factory  # noqa: E402
from candescence.tlv.losses import (  # noqa: E402
    get_kl_log_cosh_loss_tendril,
    get_kl_mse_loss_tendril,
)
from candescence.tlv.training.skip_logger import SkipLogger  # noqa: E402

logger = get_logger("scripts.train_tlv")


def _load_config_from_json(path: Path) -> TLVConfig:
    """Build a TLVConfig from a JSON dict (same schema as the Streamlit UI)."""
    with open(path) as f:
        config_dict: Dict[str, Any] = json.load(f)

    experiment_name = config_dict.get("experiment_name")
    save_name = config_dict.get("save_name")
    if not experiment_name or not save_name:
        raise ValueError("config JSON must include 'experiment_name' and 'save_name'")

    device_str = config_dict.get("device", "cuda:0" if torch.cuda.is_available() else "cpu")

    config = TLVConfig(
        experiment_name=experiment_name,
        save_name=save_name,
        device=device_str,
        use_zoo=True,
    )

    # Apply every other field verbatim onto config. Unknown fields are set
    # via setattr so downstream getattr() calls pick them up.
    for key, value in config_dict.items():
        if key in ("experiment_name", "save_name", "device"):
            continue
        setattr(config, key, value)

    # Overrides for path-typed fields that came in as strings.
    if config_dict.get("raw_images_path"):
        config.raw_images_path = Path(config_dict["raw_images_path"])
    if config_dict.get("metadata_path"):
        config.metadata_path = Path(config_dict["metadata_path"])

    config.create_directories()
    return config


def _build_tendril_arguments(config: TLVConfig) -> Dict[str, Any]:
    """Mirror StreamlitTrainer._get_tendril_arguments_dict."""
    loss_fn_name = getattr(config, "tendril_loss_fn", "MSE")
    tendril_loss_fn = (
        get_kl_log_cosh_loss_tendril
        if loss_fn_name == "Log-Cosh"
        else get_kl_mse_loss_tendril
    )
    recon_weight = getattr(config, "tendril_recon_weight", 1.0)
    return {
        "DEVICE": str(config.device),
        "MODELS": str(config.models_path),
        "tendril_batch_size": getattr(config, "tendril_batch_size", 256),
        "tendril_lr": getattr(config, "tendril_lr", 1e-4),
        "tendril_weight_decay": getattr(config, "tendril_weight_decay", 1.5e-3),
        "tendril_latent_dim": getattr(config, "tendril_latent_dim", 128),
        "tendril_num_epochs": getattr(config, "tendril_num_epochs", 50),
        "tendril_loss_fn": tendril_loss_fn,
        "tendril_MSE_weight": recon_weight,
        "tendril_log_cosh_weight": recon_weight,
        "tendril_KL_weight": getattr(config, "tendril_kl_weight", 1.0),
    }


def _run_tendril_phase(factory: Factory) -> None:
    """Compute skip connections and train one Tendril sub-VAE per level."""
    logger.info("=== Phase 2: training tendrils ===")
    skip_logger = SkipLogger(factory.vae, factory.config.device)
    skip_logger.compute_skip_connections(factory.train_dataloader, "train")
    skip_logger.compute_skip_connections(factory.validation_dataloader, "validation")

    tendril_args = _build_tendril_arguments(factory.config)
    tendrils = Tendrils(arguments=tendril_args)

    train_skips = skip_logger.skips["train"]
    val_skips = skip_logger.skips["validation"]
    layer_keys = sorted(train_skips.keys())
    logger.info(f"Training {len(layer_keys)} tendrils for layers {layer_keys}")

    for key in layer_keys:
        tendrils.add_tendril(str(key), train_skips[key], val_skips[key])

    for idx, key in enumerate(layer_keys, 1):
        logger.info(f"Training tendril {idx}/{len(layer_keys)} (layer {key})")
        tendrils.train_tendril(str(key))

    factory.save_tendrils(tendrils)
    logger.info("Tendril phase complete")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--config",
        required=True,
        type=Path,
        help="Path to JSON config (schema matches the Streamlit training UI).",
    )
    parser.add_argument(
        "--nice",
        type=int,
        default=19,
        help="POSIX niceness for the process (default: 19 = lowest priority).",
    )
    args = parser.parse_args()

    if not args.config.exists():
        parser.error(f"Config not found: {args.config}")

    try:
        os.nice(args.nice)
    except (OSError, AttributeError):
        logger.warning(f"Could not set process nice level to {args.nice}")

    config = _load_config_from_json(args.config)

    # Funnel logs into the experiment's loss_path so a run's output lives
    # next to its checkpoint. This is in addition to anything the user
    # captures via stdout redirection.
    log_file = config.loss_path / "train_tlv.log"
    log_file.parent.mkdir(parents=True, exist_ok=True)
    file_handler = logging.FileHandler(log_file)
    file_handler.setLevel(logging.INFO)
    file_handler.setFormatter(
        logging.Formatter("%(asctime)s  %(name)s  %(levelname)s  %(message)s")
    )
    logging.getLogger("candescence").addHandler(file_handler)
    logger.addHandler(file_handler)

    strategy = getattr(config, "strategy", None)
    logger.info(f"Starting experiment '{config.experiment_name}' (save_name='{config.save_name}')")
    logger.info(f"Strategy: {strategy}, architecture: {getattr(config, 'architecture', '?')}")
    logger.info(f"Device: {config.device}")
    logger.info(f"Checkpoints will be saved under: {config.models_path}")
    logger.info(f"Training log: {log_file}")

    factory = Factory(config)
    factory.load_dataset()
    logger.info(f"Dataset loaded: {len(factory.dataset)} samples")

    factory.prepare_vae()
    factory.set_training_dataloader()
    logger.info("Model and dataloaders ready; starting outer VAE training")

    factory.train_model()
    logger.info("Outer VAE training complete")

    if strategy in (14, 15, 16):
        _run_tendril_phase(factory)

    logger.info("Run finished cleanly")
    logger.info(f"Model:    {config.models_path / 'model.pth'}")
    logger.info(f"Args:     {config.models_path / 'args.json'}")
    if strategy in (14, 15, 16):
        logger.info(f"Tendrils: {config.models_path / 'tendrils'}")


if __name__ == "__main__":
    main()
