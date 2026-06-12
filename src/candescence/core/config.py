"""
Purpose: Configuration management for TLV experiments
Author: Hallett Lab
Date: 2026-01-27
Input: Experiment name and save path
Output: TLVConfig object with all paths configured
"""

from pathlib import Path
from typing import Optional, Dict, Any, List, Union
import logging
import json

from candescence.core.settings import get_settings

try:
    import torch
    TORCH_AVAILABLE = True
except ImportError:
    TORCH_AVAILABLE = False

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Canonical paths for TLV data assets, resolved from the settings layer
# (env var / config file / packaged default) rather than hard-coded. See
# ``candescence.core.settings`` for the override mechanism. Module-level
# aliases are kept for backward compatibility with existing imports.
# ---------------------------------------------------------------------------
_settings = get_settings()

# Manual morphology labels (file_name + morphology); used by merge in
# latent_explorer_app, training_app dataset report, and dataset.py.
DEFAULT_MANUALLY_LABELLED_IMAGES_CSV = _settings.manual_labels_csv

# "Big" cleaned metadata (strain, plate, niche, …).
DEFAULT_CLEANED_METADATA_XLSX = _settings.metadata_xlsx

# Backward-compatible aliases (some scripts import these directory constants).
TLV_MASTER_DATA_FILES = _settings.manual_labels_csv.parent
TLV_MASTER_METADATA_CLEANING = _settings.metadata_xlsx.parent


class TLVConfig:
    """
    Configuration class for TLV (Trained Latent VAE) experiments.

    Replaces all hardcoded paths in the original factory.py with
    configurable paths that follow lab conventions.

    Parameters
    ----------
    experiment_name : str
        Name of the experiment directory (e.g., "0_37_tendrils_reproduction")
    save_name : str
        Name of the specific run within the experiment (e.g., "manuscript_v1")
    device : str, optional
        PyTorch device string. If None, auto-detects GPU availability.

    Attributes
    ----------
    exp_path : Path
        Full path to experiment output directory
    models_path : Path
        Path where trained models are saved
    analyses_path : Path
        Path where analysis outputs are saved

    Examples
    --------
    >>> config = TLVConfig("0_37_tendrils_reproduction", "manuscript_v1")
    >>> config.create_directories()                      # doctest: +SKIP
    >>> print(config.models_path)                        # doctest: +SKIP
    <CANDESCENCE_REFINED>/0_37_tendrils_reproduction/manuscript_v1/models
    """

    # Class-level constants
    PROJECT_NAME = "candescence_new"

    # Morphology grades from original candescence
    MORPHOLOGY_GRADES = [
        "white",
        "opaque",
        "gray",
        "shmoo",
        "pseudohyphae",
        "hyphae"
    ]

    def __init__(
        self,
        experiment_name: str,
        save_name: str,
        device: Optional[str] = None,
        use_zoo: bool = False
    ) -> None:
        self.experiment_name = experiment_name
        self.save_name = save_name
        self.use_zoo = use_zoo

        # Base paths resolved from the settings layer (env/config/default).
        settings = get_settings()
        self.repo_path = settings.repo_path
        self.raw_path = settings.raw_path
        self.refined_path = settings.refined_path

        # Experiment output paths (under zoo/ when use_zoo=True)
        if use_zoo:
            self.exp_base_path = settings.zoo_path / experiment_name
        else:
            self.exp_base_path = self.refined_path / experiment_name
        self.exp_path = self.exp_base_path / save_name

        # Sub-paths for experiment outputs
        self.analyses_path = self.exp_path / "analyses"
        self.meta_info_path = self.exp_path / "meta_info"
        self.loss_path = self.exp_path / "loss_statistics"
        self.models_path = self.exp_path / "models"
        self.grid_search_path = self.exp_path / "grid_searches"
        self.images_path = self.exp_path / "images"
        self.augmented_images_path = self.images_path / "augmented_images"

        # Input data paths (image_dir is independently overridable)
        self.raw_images_path = settings.image_dir
        self.metadata_path = settings.metadata_xlsx
        self.manual_labels_path = settings.manual_labels_csv
        self.manually_labelled_path = self.manual_labels_path  # alias used by dataset.py

        # Device configuration
        if TORCH_AVAILABLE:
            if device is None:
                self.device = torch.device(
                    "cuda:0" if torch.cuda.is_available() else "cpu"
                )
            else:
                self.device = torch.device(device)
        else:
            self.device = None
            logger.warning("PyTorch not available, device set to None")

        # Training defaults - these can be overridden after initialization
        self._set_training_defaults()

        self._validate_input_paths()

    def _set_training_defaults(self) -> None:
        """Set default values for training-related attributes."""
        # Random seeds
        self.dataset_seed = 42
        self.training_seed = 9954

        # Dataset split sizes
        self.train_num = 1200
        self.validation_num = 400
        self.test_num = 400

        # Model architecture (default to Tendril VAE)
        self.architecture = "tendril_vae"
        self.strategy = 14
        self.latent_dim = 128
        self.intermediate_dim = 256
        self.leaky_relu_slope = 0.02
        self.kernel_size = 3

        # Conditioning
        self.conditional_variables = [
            "average_hue", "average_saturation", "average_value"
        ]
        self.conditional_decoder_fixed_values = {
            "average_hue": 0.5,
            "average_saturation": 0.5,
            "average_value": 0.5,
        }
        self.augment_decoder_images = False
        self.augment_images = False
        self.augmentation_variables: Optional[List[str]] = None
        self.augmentation_spread = 0.1
        self.augmentation_arity = 1
        self.cond_dim = 1

        # Strategy 9.6/9.7: SE / spatial attention gate reduction ratio.
        # Channel feature_dim // reduction_ratio sets the bottleneck width.
        self.reduction_ratio: Union[int, List[int]] = 16

        # HSV image adjustment (deterministic per-image shift, pre-training).
        # Targets default to the real-cohort dataset means computed across the
        # full compendium (see zoo/figures/adjustment_histograms/build_target_df.py).
        self.adjust_images = False
        self.target_hue = 107.15 / 255.0
        self.target_saturation = 49.07 / 255.0
        self.target_value = 93.38 / 255.0

        # Training hyperparameters
        self.number_epochs = 100
        self.batch_size = 256
        self.vae_lr = 1e-4
        self.film_lr = 5e-4
        self.weight_decay = 1.5e-3

        # Loss weights
        self.kl_weight = 1.0
        self.mse_weight = 100.0
        self.lpips_weight = 10.0
        self.ssim_weight = 1.0
        self.conditional_loss_weight = 1000.0
        self.skip_weight = 1.0
        self.cycle_length = 10
        self.vae_loss_function = "mse"

        # Manual labels
        self.merge_manually_labelled = False

        # Image settings
        self.image_dimension = 128
        self.restrict_to_day = 2

        # Reporting
        self.report_periodicity = 5
        self.training_stage = "NA"

        # Tendril VAE parameters (Stage 2 of strategy 14)
        self.tendril_batch_size = 256
        self.tendril_lr = 1e-6
        self.tendril_weight_decay = 1.5e-3
        self.tendril_latent_dim = 128
        self.tendril_num_epochs = 50
        self.tendril_loss_function = "log_cosh"  # 'mse' or 'log_cosh'
        self.tendril_mse_weight = 1.0
        self.tendril_kl_weight = 1.0
        self.tendril_log_cosh_weight = 1.0

        # System settings
        self.process_priority = 19
        self.num_threads = 10

    def _validate_input_paths(self) -> None:
        """Validate that required input paths exist."""
        if not self.raw_images_path.exists():
            logger.warning(f"Raw images path does not exist: {self.raw_images_path}")
        if not self.metadata_path.exists():
            logger.warning(f"Metadata path does not exist: {self.metadata_path}")

    def create_directories(self) -> None:
        """Create all required directories for an experiment."""
        directories = [
            self.exp_path,
            self.analyses_path,
            self.meta_info_path,
            self.loss_path,
            self.models_path,
            self.grid_search_path,
            self.images_path,
            self.augmented_images_path,
        ]

        for directory in directories:
            directory.mkdir(parents=True, exist_ok=True)
            logger.debug(f"Created directory: {directory}")

        logger.info(f"Created experiment directories at: {self.exp_path}")

    def get_arguments_dict(self) -> Dict[str, Any]:
        """
        Return dictionary compatible with legacy Factory.arguments format.

        This provides backward compatibility with the original Factory class
        while using the new path management system.

        Returns
        -------
        dict
            Dictionary of path arguments matching Factory expectations
        """
        return {
            # Device
            'DEVICE': self.device,

            # Base paths
            'CANDESCENCE_ROOT': str(self.refined_path),
            'TLV': str(self.refined_path),

            # Input data paths
            'RAW_COLONY_IMAGES': str(self.raw_images_path),
            'METADATA': str(self.metadata_path),
            'MANUALLY_LABELLED_IMAGES': str(self.manual_labels_path),

            # Experiment paths
            'EXP': str(self.exp_base_path),
            'SAVE_PATH': str(self.exp_path),

            # Output paths
            'ANALYSES': str(self.analyses_path),
            'META_INFO': str(self.meta_info_path),
            'LOSS': str(self.loss_path),
            'MODELS': str(self.models_path),
            'GRID_SEARCH': str(self.grid_search_path),
            'IMAGES': str(self.images_path),
            'AUGMENTED_IMAGES': str(self.augmented_images_path),
        }

    def save_config(self, filepath: Optional[Path] = None) -> Path:
        """
        Save configuration to JSON file.

        Parameters
        ----------
        filepath : Path, optional
            Where to save. Defaults to meta_info/config.json

        Returns
        -------
        Path
            Path to saved config file
        """
        if filepath is None:
            filepath = self.meta_info_path / "config.json"

        config_dict = {
            'experiment_name': self.experiment_name,
            'save_name': self.save_name,
            'device': str(self.device) if self.device else None,
            'paths': {k: str(v) for k, v in self.get_arguments_dict().items()
                     if k != 'DEVICE'},
        }

        filepath.parent.mkdir(parents=True, exist_ok=True)
        with open(filepath, 'w') as f:
            json.dump(config_dict, f, indent=2)

        logger.info(f"Saved config to: {filepath}")
        return filepath

    @classmethod
    def from_json(cls, filepath: Path) -> "TLVConfig":
        """
        Load configuration from JSON file.

        Parameters
        ----------
        filepath : Path
            Path to config JSON file

        Returns
        -------
        TLVConfig
            Loaded configuration object
        """
        with open(filepath) as f:
            config_dict = json.load(f)

        return cls(
            experiment_name=config_dict['experiment_name'],
            save_name=config_dict['save_name'],
            device=config_dict.get('device'),
        )

    def __repr__(self) -> str:
        return (
            f"TLVConfig(experiment='{self.experiment_name}', "
            f"save='{self.save_name}', device={self.device})"
        )
