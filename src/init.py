"""
Purpose: Initialize paths and global constants for candescence_new experiments
Author: Hallett Lab
Date: 2026-01-27
Input: None
Output: Init class with project configuration
"""

from pathlib import Path
from typing import Optional
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def _get_settings():
    """Return resolved Candescence settings (env var / config / default).

    Imported lazily so that this module loads even when the ``candescence``
    package is not importable in a given context.
    """
    from candescence.core.settings import get_settings

    return get_settings()


class Init:
    """
    Configuration class for candescence_new project.

    Provides standardized paths and constants for use across experiments.

    Attributes
    ----------
    project_name : str
        Name of the project ('candescence_new')
    repo_path : Path
        Path to the repository root
    raw_path : Path
        Path to raw data directory
    refined_path : Path
        Path to refined data directory
    src_path : Path
        Path to shared source code
    exp_path : Path
        Path to experiments directory

    Examples
    --------
    >>> from src.init import Init
    >>> config = Init()
    >>> print(config.raw_path)        # doctest: +SKIP
    <CANDESCENCE_RAW>
    >>>
    >>> # Get experiment-specific refined path
    >>> exp_refined = config.get_experiment_refined_path("0_baseline")
    >>> print(exp_refined)            # doctest: +SKIP
    <CANDESCENCE_REFINED>/0_baseline
    """

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

    def __init__(self, experiment_name: Optional[str] = None) -> None:
        """
        Initialize configuration.

        Parameters
        ----------
        experiment_name : str, optional
            Name of the current experiment (e.g., '0_baseline')
        """
        self.project_name = self.PROJECT_NAME
        self.experiment_name = experiment_name

        # Paths resolved from the settings layer (env var / config / default)
        # so external users can relocate the data trees without editing code.
        settings = _get_settings()

        # Repository paths
        self.repo_path = settings.repo_path
        self.src_path = self.repo_path / "src"
        self.exp_path = self.repo_path / "exp"
        self.data_path = self.repo_path / "data"
        self.obsolete_path = self.repo_path / "obsolete"

        # Data storage paths
        self.raw_path = settings.raw_path
        self.refined_path = settings.refined_path

        self._validate_paths()

    def _validate_paths(self) -> None:
        """Validate that essential paths exist."""
        essential_paths = [
            self.repo_path,
            self.raw_path,
            self.refined_path,
        ]

        for path in essential_paths:
            if not path.exists():
                logger.warning(f"Path does not exist: {path}")

    def get_experiment_refined_path(
        self,
        experiment_name: Optional[str] = None
    ) -> Path:
        """
        Get the refined data path for a specific experiment.

        Parameters
        ----------
        experiment_name : str, optional
            Name of the experiment. Uses self.experiment_name if not provided.

        Returns
        -------
        Path
            Path to experiment's refined data directory

        Raises
        ------
        ValueError
            If no experiment name is provided or set
        """
        exp_name = experiment_name or self.experiment_name
        if exp_name is None:
            raise ValueError("No experiment name provided")
        return self.refined_path / exp_name

    def create_experiment_directories(
        self,
        experiment_name: str
    ) -> dict[str, Path]:
        """
        Create directory structure for a new experiment.

        Parameters
        ----------
        experiment_name : str
            Name of the experiment (e.g., '0_baseline')

        Returns
        -------
        dict[str, Path]
            Dictionary with 'exp' and 'refined' paths
        """
        exp_dir = self.exp_path / experiment_name
        refined_dir = self.refined_path / experiment_name

        exp_dir.mkdir(parents=True, exist_ok=True)
        refined_dir.mkdir(parents=True, exist_ok=True)

        logger.info(f"Created experiment directories: {exp_dir}, {refined_dir}")

        return {
            "exp": exp_dir,
            "refined": refined_dir
        }

    def __repr__(self) -> str:
        return (
            f"Init(project='{self.project_name}', "
            f"experiment='{self.experiment_name}')"
        )


# For convenience, create a default instance
default_config = Init()
