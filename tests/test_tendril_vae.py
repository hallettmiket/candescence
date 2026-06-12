"""
Purpose: Test script to verify Tendril VAE training and inference pipeline
Author: Hallett Lab
Date: 2026-01-27

This script runs through the complete Tendril VAE workflow to verify
the installation and migration are working correctly.

Usage:
    python tests/test_tendril_vae.py [--quick | --full]

Options:
    --quick    Run minimal tests (default, ~2 minutes)
    --full     Run comprehensive tests with mini-training (~10 minutes)
"""

import argparse
import sys
from pathlib import Path

import numpy as np
import torch

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


def test_imports():
    """Test that all required modules can be imported."""
    print("\n" + "="*60)
    print("TEST 1: Import Verification")
    print("="*60)

    try:
        from candescence.core.config import TLVConfig
        from candescence.core.logging_config import get_logger, configure_logging
        from candescence.tlv.factory import Factory
        from candescence.tlv.data import FullDataset, LearningDataset
        from candescence.tlv.architectures import (
            c_VAE, uc_VAE, cond_uc_VAE, tendril_VAE
        )
        from candescence.tlv.training import VAETrainer, LossLogger, SkipLogger
        from candescence.tlv.inference import Inference, LatentEmbedding
        from candescence.tlv.losses import (
            compute_kl_divergence,
            get_kl_recon_loss,
            SSIMLoss
        )
        from candescence.tlv.utilities import convert_rgb_transformed_2_rgb

        print("[PASS] All core modules imported successfully")
        return True
    except ImportError as e:
        print(f"[FAIL] Import error: {e}")
        return False


def test_config():
    """Test TLVConfig creation and path management."""
    print("\n" + "="*60)
    print("TEST 2: Configuration")
    print("="*60)

    from candescence.core.config import TLVConfig

    try:
        config = TLVConfig(
            experiment_name="test_migration",
            save_name="verification_run"
        )

        # Create directories (not done automatically in __init__)
        config.create_directories()

        # Check paths exist
        assert config.exp_path.exists(), "Experiment path not created"
        assert config.models_path.exists(), "Models path not created"
        assert config.analyses_path.exists(), "Analyses path not created"

        print(f"  Experiment path: {config.exp_path}")
        print(f"  Raw images path: {config.raw_images_path}")
        print(f"  Metadata path: {config.metadata_path}")

        print("[PASS] TLVConfig working correctly")
        return config
    except Exception as e:
        print(f"[FAIL] Config error: {e}")
        return None


def test_architecture_instantiation(config):
    """Test that all architectures can be instantiated."""
    print("\n" + "="*60)
    print("TEST 3: Architecture Instantiation")
    print("="*60)

    from candescence.tlv.architectures import tendril_VAE

    try:
        # Configure for tendril VAE
        config.strategy = 14
        config.architecture = 'tendril_vae'
        config.latent_dim = 64  # Smaller for testing
        config.intermediate_dim = 128
        config.kernel_size = 3
        config.leaky_relu_slope = 0.02
        config.image_dimension = 128
        config.conditional_variables = ['average_hue']
        config.cond_dim = 1

        # Get arguments dictionary for architecture (backwards compatibility)
        # NOTE: Architectures still expect dict, need migration
        args = config.get_arguments_dict()
        args['_cond_dim'] = config.cond_dim
        args['latent_dim'] = config.latent_dim
        args['leaky_relu_slope'] = config.leaky_relu_slope
        args['image_dimension'] = config.image_dimension
        args['intermediate_dim'] = config.intermediate_dim

        # Instantiate model with arguments dict
        model = tendril_VAE(args)
        print(f"  Model type: {type(model).__name__}")

        # Count parameters
        total_params = sum(p.numel() for p in model.parameters())
        print(f"  Total parameters: {total_params:,}")

        # Test on CPU
        model = model.cpu()
        print("[PASS] Tendril VAE instantiated successfully")
        return model
    except Exception as e:
        print(f"[FAIL] Architecture error: {e}")
        import traceback
        traceback.print_exc()
        return None


def test_forward_pass(model, config):
    """Test forward pass through encoder and decoder."""
    print("\n" + "="*60)
    print("TEST 4: Forward Pass")
    print("="*60)

    try:
        model.eval()
        batch_size = 2

        # Create dummy input
        x = torch.randn(batch_size, 3, 128, 128)
        cond_enc = torch.rand(batch_size, 1)  # Hue values
        cond_dec = torch.rand(batch_size, 1)

        with torch.no_grad():
            # Encoder forward
            z, mu, logvar, skip = model.encoder(x, cond_enc)
            print(f"  Encoder output z shape: {z.shape}")
            print(f"  Skip connections: {len(skip)} tensors")

            # Decoder forward
            reconstruction = model.decoder(z, skip, cond_dec)
            print(f"  Reconstruction shape: {reconstruction.shape}")

        # Verify shapes
        assert z.shape == (batch_size, config.latent_dim), f"Z shape mismatch: {z.shape}"
        assert reconstruction.shape == x.shape, f"Reconstruction shape mismatch"

        print("[PASS] Forward pass successful")
        return True
    except Exception as e:
        print(f"[FAIL] Forward pass error: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_loss_functions():
    """Test loss function computations."""
    print("\n" + "="*60)
    print("TEST 5: Loss Functions")
    print("="*60)

    from candescence.tlv.losses import (
        compute_kl_divergence,
        SSIMLoss
    )

    try:
        batch_size = 4

        # Test KL divergence (requires arguments dict for strategy checking)
        mu = torch.randn(batch_size, 64)
        logvar = torch.randn(batch_size, 64)
        args = {'strategy': 14}  # Minimal args for KL computation
        kl = compute_kl_divergence(mu, logvar, args)
        print(f"  KL divergence: {kl.item():.4f}")
        assert kl.item() >= 0, "KL should be non-negative"

        # Test SSIM loss
        ssim_loss = SSIMLoss()
        img1 = torch.rand(batch_size, 3, 128, 128)
        img2 = torch.rand(batch_size, 3, 128, 128)
        ssim = ssim_loss(img1, img2)
        print(f"  SSIM loss (random): {ssim.item():.4f}")

        # Same image should have low loss
        ssim_same = ssim_loss(img1, img1)
        print(f"  SSIM loss (same): {ssim_same.item():.6f}")
        assert ssim_same.item() < 0.01, "SSIM of same image should be ~0"

        print("[PASS] Loss functions working correctly")
        return True
    except Exception as e:
        print(f"[FAIL] Loss function error: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_dataset_loading(config):
    """Test dataset loading (requires data files)."""
    print("\n" + "="*60)
    print("TEST 6: Dataset Loading")
    print("="*60)

    from candescence.tlv.data import FullDataset

    try:
        # Configure dataset parameters
        config.train_num = 10
        config.validation_num = 5
        config.test_num = 5
        config.image_type = 'non-wash'
        config.restrict_to_day = 2
        config.dataset_seed = 9954
        config.merge_manually_labelled_images = False

        # Check if data paths exist
        if not config.raw_images_path.exists():
            print(f"  [SKIP] Raw images not found at {config.raw_images_path}")
            print("  Dataset loading requires symlinked data")
            return None

        if not config.metadata_path.exists():
            print(f"  [SKIP] Metadata not found at {config.metadata_path}")
            return None

        # Load dataset
        dataset = FullDataset(config)
        print(f"  Total samples: {len(dataset)}")
        print(f"  Training samples: {len(dataset.train_dataset)}")
        print(f"  Columns: {len(dataset.target_df.columns)}")

        print("[PASS] Dataset loaded successfully")
        return dataset
    except Exception as e:
        print(f"[WARN] Dataset loading error: {e}")
        print("  This may be expected if data symlinks are not set up")
        return None


def test_factory_workflow(config):
    """Test the Factory workflow."""
    print("\n" + "="*60)
    print("TEST 7: Factory Workflow")
    print("="*60)

    from candescence.tlv.factory import Factory

    try:
        # Use minimal settings
        config.train_num = 10
        config.validation_num = 5
        config.test_num = 5
        config.batch_size = 4
        config.number_epochs = 1

        factory = Factory(config)
        print(f"  Factory created for: {config.experiment_name}")

        # Check if we can load dataset
        if config.raw_images_path.exists():
            factory.load_dataset()
            print(f"  Dataset loaded: {len(factory.dataset)} samples")

            factory.prepare_vae()
            print(f"  VAE prepared: {type(factory.vae).__name__}")

            factory.set_training_dataloader()
            print(f"  DataLoaders created")

            print("[PASS] Factory workflow successful")
            return factory
        else:
            print("  [SKIP] Data not available, skipping dataset load")
            return None

    except Exception as e:
        print(f"[WARN] Factory error: {e}")
        import traceback
        traceback.print_exc()
        return None


def test_pretrained_model_loading(config):
    """Test loading a pre-trained model."""
    print("\n" + "="*60)
    print("TEST 8: Pre-trained Model Loading")
    print("="*60)

    from candescence.tlv.architectures import tendril_VAE

    try:
        from candescence.core.settings import get_settings
        pretrained_path = (
            get_settings().refined_path
            / "0_37_tendrils_reproduction/manuscript_v1/models/model.pth"
        )

        if not pretrained_path.exists():
            print(f"  [SKIP] Pre-trained model not found at {pretrained_path}")
            return None

        # Configure model to match pre-trained (using arguments dict)
        args = config.get_arguments_dict()
        args['_cond_dim'] = 1
        args['latent_dim'] = 128
        args['intermediate_dim'] = 256
        args['leaky_relu_slope'] = 0.02
        args['image_dimension'] = 128

        # Instantiate model with arguments dict
        model = tendril_VAE(args)

        # Load weights (map to CPU for testing)
        state = torch.load(pretrained_path, map_location='cpu', weights_only=False)
        model.load_state_dict(state)
        model = model.cpu()  # Ensure model is on CPU
        print(f"  Loaded model from: {pretrained_path}")

        # Test forward pass on CPU
        model.eval()
        x = torch.randn(1, 3, 128, 128)  # CPU tensor
        cond = torch.rand(1, 1)  # CPU tensor

        with torch.no_grad():
            z, mu, logvar, skip = model.encoder(x, cond)
            rec = model.decoder(z, skip, cond)

        print(f"  Forward pass successful, output shape: {rec.shape}")
        print("[PASS] Pre-trained model loaded and verified")
        return model
    except Exception as e:
        print(f"[FAIL] Model loading error: {e}")
        import traceback
        traceback.print_exc()
        return None


def test_latent_embedding():
    """Test LatentEmbedding class."""
    print("\n" + "="*60)
    print("TEST 9: LatentEmbedding Analysis")
    print("="*60)

    import pandas as pd
    from candescence.tlv.inference import LatentEmbedding

    try:
        # Create dummy embedding data
        n_samples = 50
        latent_dim = 64

        data = {f'latent_{i}': np.random.randn(n_samples) for i in range(latent_dim)}
        data['id'] = list(range(n_samples))
        df = pd.DataFrame(data)

        # Create LatentEmbedding
        device = torch.device('cpu')
        le = LatentEmbedding(df, device)

        print(f"  Samples: {len(le._latent_embedding)}")
        print(f"  Matrix shape: {le._latent_matrix.shape}")

        # Test distance computation
        dist_df, dist_array, dist_tensor = le.compute_distances()
        print(f"  Distance matrix shape: {dist_array.shape}")

        # Test density computation
        density = le.compute_latent_density(bandwidth=1.0)
        print(f"  Average density: {density['avg_density']:.4f}")

        # Test PCA
        pca_coords = le.apply_pca(n_components=2)
        print(f"  PCA output shape: {pca_coords.shape}")

        print("[PASS] LatentEmbedding working correctly")
        return True
    except Exception as e:
        print(f"[FAIL] LatentEmbedding error: {e}")
        import traceback
        traceback.print_exc()
        return False


def run_mini_training(config, factory):
    """Run a mini training loop to verify training works."""
    print("\n" + "="*60)
    print("TEST 10: Mini Training Loop")
    print("="*60)

    try:
        if factory is None:
            print("  [SKIP] Factory not available")
            return None

        print("  Running 1 epoch of training...")
        config.number_epochs = 1

        # This would run actual training
        # factory.train_model()

        # Instead, manually test one batch
        factory.vae.train()
        batch = next(iter(factory.train_dataloader))

        img, cond_enc, cond_dec, target = batch
        img = img.to(config.device)
        cond_enc = cond_enc.to(config.device).float()
        cond_dec = cond_dec.to(config.device).float()

        # Forward pass
        z, mu, logvar, skip = factory.vae.encoder(img, cond_enc)
        rec = factory.vae.decoder(z, skip, cond_dec)

        # Compute loss (simplified)
        from candescence.tlv.losses import compute_kl_divergence
        recon_loss = torch.nn.functional.mse_loss(rec, img)
        args = config.get_arguments_dict()
        kl_loss = compute_kl_divergence(mu, logvar, args)
        total_loss = recon_loss + 0.001 * kl_loss

        print(f"  Batch shape: {img.shape}")
        print(f"  Reconstruction loss: {recon_loss.item():.4f}")
        print(f"  KL loss: {kl_loss.item():.4f}")
        print(f"  Total loss: {total_loss.item():.4f}")

        # Backward pass
        factory.optimizer_vae.zero_grad()
        total_loss.backward()
        factory.optimizer_vae.step()
        print("  Backward pass successful")

        print("[PASS] Mini training loop successful")
        return True
    except Exception as e:
        print(f"[FAIL] Training error: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run all tests."""
    parser = argparse.ArgumentParser(description="Test Tendril VAE pipeline")
    parser.add_argument('--quick', action='store_true', help='Run quick tests only')
    parser.add_argument('--full', action='store_true', help='Run full tests with training')
    args = parser.parse_args()

    print("\n" + "#"*60)
    print("#  TENDRIL VAE VERIFICATION TESTS")
    print("#"*60)

    results = {}

    # Test 1: Imports
    results['imports'] = test_imports()

    # Test 2: Config
    config = test_config()
    results['config'] = config is not None

    if config is None:
        print("\n[ABORT] Config test failed, cannot continue")
        return 1

    # Test 3: Architecture
    model = test_architecture_instantiation(config)
    results['architecture'] = model is not None

    # Test 4: Forward pass
    if model is not None:
        results['forward_pass'] = test_forward_pass(model, config)
    else:
        results['forward_pass'] = False

    # Test 5: Loss functions
    results['loss_functions'] = test_loss_functions()

    # Test 6: Dataset loading
    dataset = test_dataset_loading(config)
    results['dataset'] = dataset is not None

    # Test 7: Factory workflow
    factory = test_factory_workflow(config)
    results['factory'] = factory is not None

    # Test 8: Pre-trained model
    results['pretrained'] = test_pretrained_model_loading(config) is not None

    # Test 9: LatentEmbedding
    results['latent_embedding'] = test_latent_embedding()

    # Test 10: Mini training (only if --full)
    if args.full and factory is not None:
        results['training'] = run_mini_training(config, factory)
    else:
        results['training'] = None

    # Summary
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)

    passed = 0
    failed = 0
    skipped = 0

    for test_name, result in results.items():
        if result is True:
            status = "[PASS]"
            passed += 1
        elif result is False:
            status = "[FAIL]"
            failed += 1
        else:
            status = "[SKIP]"
            skipped += 1
        print(f"  {status} {test_name}")

    print(f"\nTotal: {passed} passed, {failed} failed, {skipped} skipped")

    if failed > 0:
        print("\n[WARNING] Some tests failed. Check output above for details.")
        return 1
    else:
        print("\n[SUCCESS] All critical tests passed!")
        return 0


if __name__ == "__main__":
    sys.exit(main())
