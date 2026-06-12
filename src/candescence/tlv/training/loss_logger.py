"""
Purpose: Loss logging for VAE training
Author: Hallett Lab
Date: 2026-01-27

Provides LossLogger class for tracking and visualizing training losses.
"""

import csv
from pathlib import Path
from typing import Any, Dict, List, Optional

import matplotlib.pyplot as plt

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.tlv.training.loss_logger")


class LossLogger:
    """
    Logger for tracking and visualizing VAE training losses.

    Tracks training and validation losses for different components:
    - Total loss
    - KL divergence loss
    - Reconstruction loss
    - Regressor loss
    - Conditional loss
    - Skip connection loss
    """

    def __init__(self, config: Any) -> None:
        """
        Initialize the LossLogger with configuration.

        Args:
            config: Configuration object with attributes:
                - loss_path: Path for saving loss plots
                - architecture: Model architecture name
                - training_stage: Current training stage
                - loss_plot_burn_in: Epochs to skip in plots
        """
        self.config = config
        self.burn_in = int(getattr(config, 'loss_plot_burn_in', 0))
        self.loss_path = getattr(config, 'loss_path', Path('.'))
        self.architecture = getattr(config, 'architecture', 'unknown')
        self.training_stage = getattr(config, 'training_stage', 'NA')

        # VAE losses
        self.train_losses: List[float] = []
        self.val_losses: List[float] = []
        self.train_kl_losses: List[float] = []
        self.train_recon_losses: List[float] = []
        self.val_kl_losses: List[float] = []
        self.val_recon_losses: List[float] = []

        # Regressor losses
        self.train_reg_losses: List[float] = []
        self.val_reg_losses: List[float] = []

        # Conditional losses
        self.train_cond_losses: List[float] = []
        self.val_cond_losses: List[float] = []

        # Skip connection losses
        self.train_skip_losses: List[float] = []
        self.val_skip_losses: List[float] = []

        # Legacy regressor losses
        self.regressor_train_losses: List[float] = []
        self.regressor_val_losses: List[float] = []

    def log_epoch(self, **kwargs: float) -> None:
        """
        Log loss values from an epoch.

        Args:
            **kwargs: Loss values with keys:
                - train_loss, train_kl_loss, train_recon_loss
                - val_loss, val_kl_loss, val_recon_loss
                - train_reg_loss, val_reg_loss
                - train_cond_loss, val_cond_loss
                - train_skip_loss, val_skip_loss
        """
        log_mapping = {
            'train_loss': self.train_losses,
            'train_kl_loss': self.train_kl_losses,
            'train_recon_loss': self.train_recon_losses,
            'train_reg_loss': self.train_reg_losses,
            'train_cond_loss': self.train_cond_losses,
            'train_skip_loss': self.train_skip_losses,
            'val_loss': self.val_losses,
            'val_kl_loss': self.val_kl_losses,
            'val_recon_loss': self.val_recon_losses,
            'val_reg_loss': self.val_reg_losses,
            'val_cond_loss': self.val_cond_losses,
            'val_skip_loss': self.val_skip_losses,
            # Legacy support
            'train_KL_loss': self.train_kl_losses,
            'train_RECON_loss': self.train_recon_losses,
            'val_KL_loss': self.val_kl_losses,
            'val_RECON_loss': self.val_recon_losses,
        }

        for key, value in kwargs.items():
            if key in log_mapping:
                log_mapping[key].append(value)

    def _to_float_list(self, data: Any) -> List[float]:
        """
        Convert data to list of floats.

        Args:
            data: Tensor, list of tensors, or list of floats.

        Returns:
            List of floats.
        """
        if hasattr(data, 'cpu'):
            return data.cpu().numpy().tolist() if data.ndim > 0 else [data.cpu().item()]
        elif isinstance(data, list) and data and hasattr(data[0], 'cpu'):
            return [x.cpu().item() for x in data]
        else:
            return list(data)

    def _plot_loss(
        self,
        train_data: List[float],
        val_data: List[float],
        title: str,
        ylabel: str,
        file_suffix: str,
        label_train: str,
        label_val: str,
        ls_train: str = '-',
        ls_val: str = '--'
    ) -> None:
        """
        Generic method to plot losses.

        Args:
            train_data: Training loss values.
            val_data: Validation loss values.
            title: Plot title.
            ylabel: Y-axis label.
            file_suffix: Suffix for filename.
            label_train: Label for training curve.
            label_val: Label for validation curve.
            ls_train: Line style for training curve.
            ls_val: Line style for validation curve.
        """
        train_data = self._to_float_list(train_data)
        val_data = self._to_float_list(val_data)

        plt.figure(figsize=(10, 5))
        plt.plot(train_data, label=label_train, linestyle=ls_train)
        plt.plot(val_data, label=label_val, linestyle=ls_val)
        plt.xlabel('Epoch')
        plt.ylabel(ylabel)
        plt.title(title)
        plt.legend()
        plt.tight_layout()

        file_name = f"stage{self.training_stage}_{file_suffix}"
        save_path = self.loss_path / file_name

        self.loss_path.mkdir(parents=True, exist_ok=True)
        plt.savefig(save_path)
        plt.show()
        plt.close()

        logger.info(f"Loss plot saved to {save_path}")

    def vae_plot_losses(self) -> None:
        """
        Generate VAE loss graphs.

        Creates plots for:
        - Total loss (training vs validation)
        - KL divergence loss
        - Reconstruction loss

        Applies burn-in period to skip early epochs.
        """
        train_losses = self.train_losses[self.burn_in:]
        val_losses = self.val_losses[self.burn_in:]
        train_kl_losses = self.train_kl_losses[self.burn_in:]
        val_kl_losses = self.val_kl_losses[self.burn_in:]
        train_recon_losses = self.train_recon_losses[self.burn_in:]
        val_recon_losses = self.val_recon_losses[self.burn_in:]

        # General loss plot
        self._plot_loss(
            train_losses,
            val_losses,
            'VAE Loss over Epochs General',
            'Loss',
            "_TrainValLoss-graph.jpg",
            "Training Loss",
            "Validation Loss",
            ls_train='-',
            ls_val='--'
        )

        # KL divergence loss plot
        self._plot_loss(
            train_kl_losses,
            val_kl_losses,
            'VAE Loss over Epochs KL Divergence',
            'Loss',
            "_loss-graph-KL.jpg",
            "Training KL Loss",
            "Validation KL Loss",
            ls_train='--',
            ls_val='-.'
        )

        # Reconstruction loss plot
        self._plot_loss(
            train_recon_losses,
            val_recon_losses,
            'VAE Loss over Epochs RECON Error',
            'Loss',
            "_loss-graph-RECON.jpg",
            "Training RECON Loss",
            "Validation RECON Loss",
            ls_train='--',
            ls_val='-.'
        )

    def cond_plot_losses(self) -> None:
        """
        Generate conditional loss graphs.

        Only generates plots for conditional architectures.
        """
        if self.architecture not in ('cond_uc_vae', 'tendril_vae'):
            return

        train_cond_losses = self.train_cond_losses[self.burn_in:]
        val_cond_losses = self.val_cond_losses[self.burn_in:]

        self._plot_loss(
            train_cond_losses,
            val_cond_losses,
            'Conditional Loss over Epochs',
            'Loss',
            "_loss-graph-conditional.jpg",
            "Training Conditional Loss",
            "Validation Conditional Loss",
            ls_train='--',
            ls_val='-.'
        )

    def regressor_plot_losses(self) -> None:
        """
        Generate regressor loss graphs.

        Creates plots for regressor training if regressor losses were logged.
        """
        train_reg_losses = self.train_reg_losses[self.burn_in:]
        val_reg_losses = self.val_reg_losses[self.burn_in:]

        if not train_reg_losses or not val_reg_losses:
            logger.warning("No regressor losses to plot")
            return

        self._plot_loss(
            train_reg_losses,
            val_reg_losses,
            'Regressor Loss over Epochs',
            'Loss',
            "_loss-graph-regressor.jpg",
            "Training Regressor Loss",
            "Validation Regressor Loss",
            ls_train='--',
            ls_val='-.'
        )

    def skip_plot_losses(self) -> None:
        """
        Generate skip connection loss graphs.
        """
        train_skip_losses = self.train_skip_losses[self.burn_in:]
        val_skip_losses = self.val_skip_losses[self.burn_in:]

        if not train_skip_losses or not val_skip_losses:
            logger.warning("No skip connection losses to plot")
            return

        self._plot_loss(
            train_skip_losses,
            val_skip_losses,
            'Skip Connection Loss over Epochs',
            'Loss',
            "_loss-graph-skip.jpg",
            "Training Skip Loss",
            "Validation Skip Loss",
            ls_train='--',
            ls_val='-.'
        )

    def save_loss_history_csv(self, file_name: Optional[str] = None) -> Optional[Path]:
        """
        Persist the per-epoch loss history to a CSV (one row per epoch).

        Only columns with at least one logged value are written. Existing
        checkpoints never produced this file (the logger previously kept
        history in memory and saved only PNGs); writing it here lets the
        diagnostic harness compute an "epochs-to-inflection" statistic for
        freshly trained models.

        Args:
            file_name: Optional filename. Defaults to
                ``stage{training_stage}_loss_history.csv``.

        Returns:
            The path written, or ``None`` if no losses were logged.
        """
        columns = [
            ('train_loss', self.train_losses),
            ('val_loss', self.val_losses),
            ('train_kl_loss', self.train_kl_losses),
            ('val_kl_loss', self.val_kl_losses),
            ('train_recon_loss', self.train_recon_losses),
            ('val_recon_loss', self.val_recon_losses),
            ('train_cond_loss', self.train_cond_losses),
            ('val_cond_loss', self.val_cond_losses),
            ('train_skip_loss', self.train_skip_losses),
            ('val_skip_loss', self.val_skip_losses),
            ('train_reg_loss', self.train_reg_losses),
            ('val_reg_loss', self.val_reg_losses),
        ]
        populated = [
            (name, self._to_float_list(values))
            for name, values in columns
            if values
        ]
        if not populated:
            logger.warning("No losses logged; skipping loss_history.csv")
            return None

        n_epochs = max(len(values) for _, values in populated)
        file_name = file_name or f"stage{self.training_stage}_loss_history.csv"
        save_path = self.loss_path / file_name
        self.loss_path.mkdir(parents=True, exist_ok=True)

        header = ['epoch'] + [name for name, _ in populated]
        with open(save_path, 'w', newline='') as fh:
            writer = csv.writer(fh)
            writer.writerow(header)
            for epoch in range(n_epochs):
                row: List[Any] = [epoch]
                for _, values in populated:
                    row.append(values[epoch] if epoch < len(values) else '')
                writer.writerow(row)

        logger.info(f"Loss history saved to {save_path} ({n_epochs} epochs)")
        return save_path

    def plot_all_losses(self) -> None:
        """
        Generate all available loss plots.
        """
        self.vae_plot_losses()
        self.cond_plot_losses()
        self.regressor_plot_losses()
        self.skip_plot_losses()
        self.save_loss_history_csv()

    def get_summary(self) -> Dict[str, Dict[str, float]]:
        """
        Get summary statistics for all logged losses.

        Returns:
            Dictionary with min, max, and final values for each loss type.
        """
        summary = {}

        loss_types = [
            ('train_loss', self.train_losses),
            ('val_loss', self.val_losses),
            ('train_kl_loss', self.train_kl_losses),
            ('val_kl_loss', self.val_kl_losses),
            ('train_recon_loss', self.train_recon_losses),
            ('val_recon_loss', self.val_recon_losses),
            ('train_cond_loss', self.train_cond_losses),
            ('val_cond_loss', self.val_cond_losses),
        ]

        for name, losses in loss_types:
            if losses:
                summary[name] = {
                    'min': min(losses),
                    'max': max(losses),
                    'final': losses[-1],
                    'count': len(losses)
                }

        return summary
