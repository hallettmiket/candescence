"""
Purpose: Reproduce 37-tendrils manuscript experiment
Author: Hallett Lab
Date: 2026-01-27

This script reproduces the 37-tendrils VAE experiment for manuscript figures.
Uses the new candescence package structure with TLVConfig.

Usage:
    python reproduce_37_tendrils.py [--train | --inference]

Environment:
    uv run python reproduce_37_tendrils.py ...   # or activate .venv first
"""

import argparse
import sys
from pathlib import Path

# Add src to path for development
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from candescence.core.config import TLVConfig
from candescence.core.logging_config import get_logger, setup_logging
from candescence.tlv.factory import Factory

logger = get_logger("candescence.scripts.reproduce_37_tendrils")


def create_config() -> TLVConfig:
    """
    Create TLVConfig for 37-tendrils reproduction.

    Returns:
        Configured TLVConfig object.
    """
    config = TLVConfig(
        experiment_name="0_37_tendrils_reproduction",
        save_name="manuscript_v1"
    )

    # ---------------------- Model Architecture ---------------------- #
    config.strategy = 14
    config.architecture = 'tendril_vae'
    config.latent_dim = 128
    config.intermediate_dim = 256
    config.kernel_size = 3
    config.leaky_relu_slope = 0.02

    # ---------------------- Conditioning ---------------------- #
    config.conditional_variables = ['average_hue']
    config.conditional_decoder_fixed_values = {'average_hue': 0.5}
    config.adjustment_variables = None
    config.augmentation_variables = None
    config.decoder_augmentation = {
        'variables': ['average_hue'],
        'variance': 0.0
    }
    config.augment_images = False
    config.augment_decoder_images = True
    config.adjust_images = False

    # ---------------------- Dataset ---------------------- #
    config.image_dimension = 128
    config.image_type = 'non-wash'
    config.restrict_to_day = 2
    config.merge_manually_labelled_images = True
    config.dataset_seed = 9954
    config.training_seed = 9954
    config.train_num = 1200
    config.validation_num = 400
    config.test_num = 400

    # ---------------------- Training ---------------------- #
    config.batch_size = 256
    config.number_epochs = 100
    config.vae_lr = 1e-4
    config.film_lr = 5e-4
    config.weight_decay = 1.5e-3

    # ---------------------- Loss Weights ---------------------- #
    config.kl_weight = 1
    config.LPIPS_weight = 10
    config.MSE_weight = 100
    config.SSIM_weight = 100
    config.skip_weight = 1
    config.conditional_loss_weight = 1000
    config.TC_weight = 10
    config.cycle_length = 10

    # Strategy 14 specific KL weights (per phase)
    config.kl_strategy_14_weights = [1.0, 1.0, 1.0, 1.0]

    # ---------------------- Tendril-specific ---------------------- #
    config.tendril_batch_size = 256
    config.tendril_lr = 1e-6
    config.tendril_weight_decay = 1.5e-3
    config.tendril_latent_dim = 128
    config.tendril_num_epochs = 50
    config.tendril_MSE_weight = 1
    config.tendril_log_cosh_weight = 1
    config.tendril_KL_weight = 1

    # ---------------------- Skip Connections ---------------------- #
    config.reduction_ratio = 16
    config.spatial_reduction = 2

    # ---------------------- System ---------------------- #
    config.process_priority = 19
    config.num_threads = 10
    config.number_processors = 10
    config.loss_plot_burn_in = 5

    return config


def run_training(config: TLVConfig) -> None:
    """
    Run the full training pipeline.

    Args:
        config: TLVConfig object.
    """
    logger.info("Starting 37-tendrils training pipeline")
    logger.info(f"Experiment: {config.experiment_name}/{config.save_name}")
    logger.info(f"Output path: {config.exp_path}")

    factory = Factory(config)
    factory.run_experiment()

    logger.info("Training complete")


def run_inference(config: TLVConfig) -> None:
    """
    Run inference on trained model.

    Args:
        config: TLVConfig object.
    """
    logger.info("Starting 37-tendrils inference pipeline")

    factory = Factory(config)
    factory.load_dataset()
    factory.prepare_vae()
    factory.load_model()

    # Run inference on the learning set
    infer = factory.inference_with_learning_set(output_filename='manuscript')

    # Generate reports
    infer.generate_reports(key='manuscript')

    logger.info("Inference complete")
    logger.info(f"Results saved to {config.analyses_path}")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Reproduce 37-tendrils manuscript experiment"
    )
    parser.add_argument(
        '--train',
        action='store_true',
        help='Run full training pipeline'
    )
    parser.add_argument(
        '--inference',
        action='store_true',
        help='Run inference on trained model'
    )
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose logging'
    )

    args = parser.parse_args()

    # Setup logging
    log_level = 'DEBUG' if args.verbose else 'INFO'
    setup_logging(level=log_level)

    # Create configuration
    config = create_config()

    if args.train:
        run_training(config)
    elif args.inference:
        run_inference(config)
    else:
        # Default: show configuration summary
        print("37-Tendrils Reproduction Script")
        print("=" * 50)
        print(f"Experiment: {config.experiment_name}")
        print(f"Save name: {config.save_name}")
        print(f"Strategy: {config.strategy}")
        print(f"Architecture: {config.architecture}")
        print(f"Latent dim: {config.latent_dim}")
        print(f"Conditional variables: {config.conditional_variables}")
        print(f"Number epochs: {config.number_epochs}")
        print(f"Output path: {config.exp_path}")
        print()
        print("Usage:")
        print("  python reproduce_37_tendrils.py --train      # Full training")
        print("  python reproduce_37_tendrils.py --inference  # Run inference")


if __name__ == "__main__":
    main()
