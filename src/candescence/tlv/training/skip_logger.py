"""
Purpose: Skip connection logging for VAE training
Author: Hallett Lab
Date: 2026-01-27

Provides SkipLogger class for tracking skip connection statistics during training.
"""

from typing import Any, Callable, Dict, List, Optional, Tuple, Union

import torch
import torch.nn as nn
from torch.utils.data import DataLoader

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.tlv.training.skip_logger")


def _cond_to_device(cond, device):
    """Move a tensor or dict-of-tensors cond onto *device* as float.

    Returns ``None`` unchanged so callers can pass through the no-cond path.
    """
    if cond is None:
        return None
    if isinstance(cond, dict):
        return {k: v.to(device).float() for k, v in cond.items()}
    return cond.to(device).float()


class SkipLogger:
    """
    Logger for tracking skip connection statistics during VAE training.

    Monitors skip connections from the encoder to track information flow
    through the network.
    """

    def __init__(
        self,
        vae: nn.Module,
        device: torch.device,
        collect_nuisance: bool = False,
    ) -> None:
        """
        Initialize SkipLogger with VAE model and device.

        Args:
            vae: The VAE model with encoder.
            device: Device (CPU or GPU) for computation.
            collect_nuisance: When True, also collect per-image nuisance tensors
                (plate one-hot, HSV, morphology label) row-aligned with the skip
                features, for Strategy-17 invariance training.
        """
        self.vae = vae.to(device)
        self.device = device
        self.collect_nuisance = collect_nuisance
        # Per-phase nuisance: {'plate': (N,P), 'hsv': (N,3), 'morph': (N,)}.
        self.nuisance: Dict[str, Optional[Dict[str, torch.Tensor]]] = {
            'train': None,
            'validation': None,
        }
        self.skips: Dict[str, Dict[int, torch.Tensor]] = {
            'train': {},
            'validation': {}
        }
        self.conds: Dict[str, Optional[Union[torch.Tensor, Dict[str, torch.Tensor]]]] = {
            'train': None,
            'validation': None
        }
        self.conds_dec: Dict[str, Optional[Union[torch.Tensor, Dict[str, torch.Tensor]]]] = {
            'train': None,
            'validation': None
        }

    def reset(self) -> None:
        """Reset skip connections for both training and validation."""
        self.skips = {'train': {}, 'validation': {}}
        self.conds = {'train': None, 'validation': None}
        self.conds_dec = {'train': None, 'validation': None}

    def log_epoch(
        self,
        phase: str,
        skip_connections_data: Dict[int, torch.Tensor]
    ) -> None:
        """
        Log skip connections for the current epoch.

        Args:
            phase: Either 'train' or 'validation'.
            skip_connections_data: Skip connections for each layer.
        """
        self.skips[phase] = skip_connections_data
        self.generate_report(skip_connections_data, phase)

    def compute_skip_connections(
        self,
        dataloader: DataLoader,
        phase: str,
    ) -> None:
        """Compute skip connection statistics from a dataloader.

        Parameters
        ----------
        dataloader : DataLoader
            DataLoader to process.
        phase : str
            Either ``'train'`` or ``'validation'``.
        """
        self.vae.eval()
        skip_connections_data: Dict[int, List[torch.Tensor]] = {}
        # Collect per-batch cond dicts so tendril training can build
        # FiLM conditioning aligned with the (batch-order) skip tensors.
        # Only populated when encoder_cond is a dict (Strategies 15/16).
        cond_data: Dict[str, List[torch.Tensor]] = {}
        # Per-image nuisance tensors (plate one-hot, HSV, morphology), row-aligned
        # with the skip features, for Strategy-17 invariance training.
        nuisance_data: Dict[str, List[torch.Tensor]] = {}
        dataset = getattr(dataloader, 'dataset', None)

        with torch.no_grad():
            for batch_idx, batch in enumerate(dataloader):
                x, encoder_cond, decoder_cond, cond, indices = self._prepare_batch_inputs(batch)
                if self.collect_nuisance and dataset is not None:
                    for key, tensor in self._batch_nuisance(dataset, indices).items():
                        nuisance_data.setdefault(key, []).append(tensor)

                x = x.to(self.device).float()
                encoder_cond = _cond_to_device(encoder_cond, self.device)

                # Forward pass through encoder (encoder_cond may be a tensor
                # for Strategies 13/14 or a dict for Strategy 15).
                if encoder_cond is None:
                    _, _, _, skip_connections = self.vae.encoder(x)
                else:
                    _, _, _, skip_connections = self.vae.encoder(x, encoder_cond)

                # Store skip connections for each layer
                for layer, skip in enumerate(skip_connections):
                    if layer not in skip_connections_data:
                        skip_connections_data[layer] = []
                    # IMPORTANT: move to CPU before storing to avoid OOM.
                    # Skip tensors are large (especially early layers) and we collect
                    # them for the full dataset; keeping them on GPU can easily exceed
                    # VRAM during Phase 2 tendril training.
                    skip_connections_data[layer].append(skip.detach().to("cpu"))

                # Collect per-key cond tensors for Strategy 15/16 dict conds.
                if isinstance(encoder_cond, dict):
                    for key, tensor in encoder_cond.items():
                        if key not in cond_data:
                            cond_data[key] = []
                        cond_data[key].append(tensor.detach().to("cpu"))

                # Reduce peak GPU memory during long skip collection loops.
                if torch.cuda.is_available():
                    torch.cuda.empty_cache()

        # Concatenate all batches for each layer
        concatenated_skips: Dict[int, torch.Tensor] = {}
        for layer in skip_connections_data.keys():
            concatenated_skips[layer] = torch.cat(skip_connections_data[layer], dim=0)

        # Concatenate per-key cond tensors so they align row-wise with
        # the concatenated skip tensors.
        if cond_data:
            self.conds[phase] = {
                key: torch.cat(parts, dim=0) for key, parts in cond_data.items()
            }

        # Concatenate nuisance tensors (same batch order as the skips).
        if nuisance_data:
            self.nuisance[phase] = {
                key: torch.cat(parts, dim=0) for key, parts in nuisance_data.items()
            }

        # === DIAGNOSTIC: Log skip tensor stats after concatenation ===
        logger.info(f"=== SKIP CONNECTION STATS after compute_skip_connections ({phase}) ===")
        for layer in sorted(concatenated_skips.keys()):
            t = concatenated_skips[layer]
            logger.info(
                f"  Layer {layer}: shape={list(t.shape)}, "
                f"min={t.min().item():.6f}, max={t.max().item():.6f}, "
                f"mean={t.mean().item():.6f}, std={t.std().item():.6f}"
            )
            if t.std().item() < 1e-6:
                logger.warning(f"  Layer {layer} ({phase}): NEAR-ZERO VARIANCE — encoder may not be producing varied features!")
        logger.info("============================================================")

        self.log_epoch(phase, concatenated_skips)

    def _batch_nuisance(
        self, dataset: Any, indices: torch.Tensor,
    ) -> Dict[str, torch.Tensor]:
        """Build per-image nuisance tensors for one batch (plate one-hot, HSV,
        morphology label), looked up from the dataset metadata by row index."""
        rows = dataset.meta_df_subset.iloc[[int(i) for i in indices]]
        out: Dict[str, torch.Tensor] = {}

        plate_cats = list(getattr(dataset, 'plate_categories', []) or [])
        if plate_cats:
            cat_index = {str(c): i for i, c in enumerate(plate_cats)}
            oh = torch.zeros((len(rows), len(plate_cats)), dtype=torch.float32)
            for r, val in enumerate(rows['plate'].astype(str).tolist()):
                j = cat_index.get(val)
                if j is not None:
                    oh[r, j] = 1.0
            out['plate'] = oh

        hsv = torch.tensor(
            rows[['average_hue', 'average_saturation', 'average_value']]
            .to_numpy(dtype='float32') / 255.0,
            dtype=torch.float32,
        )
        out['hsv'] = hsv

        morph_cats = list(getattr(dataset, 'morphology_categories', []) or [])
        if morph_cats and 'manual_formation' in rows.columns:
            cat_index = {str(c): i for i, c in enumerate(morph_cats)}
            labels = torch.tensor(
                [cat_index.get(str(v), -1) for v in rows['manual_formation'].tolist()],
                dtype=torch.long,
            )
            out['morph'] = labels

        return out

    def generate_report(
        self,
        skip_connections_data: Dict[int, torch.Tensor],
        phase: str
    ) -> Dict[int, Dict[str, float]]:
        """
        Generate report of skip connection statistics.

        Args:
            skip_connections_data: Skip connections for each layer.
            phase: Either 'train' or 'validation'.

        Returns:
            Dictionary with statistics for each layer.
        """
        report: Dict[int, Dict[str, float]] = {}

        for layer, skips in skip_connections_data.items():
            # Compute statistics for each image in the layer
            min_values = skips.min(dim=1)[0]
            max_values = skips.max(dim=1)[0]
            mean_values = skips.mean(dim=1)

            # Compute average statistics across all images
            avg_min = min_values.mean().item()
            avg_max = max_values.mean().item()
            avg_mean = mean_values.mean().item()

            report[layer] = {
                'avg_min': avg_min,
                'avg_max': avg_max,
                'avg_mean': avg_mean
            }

            logger.debug(
                f"Layer {layer}: Avg Min: {avg_min:.4f}, "
                f"Avg Max: {avg_max:.4f}, Avg Mean: {avg_mean:.4f}"
            )

        return report

    def _prepare_batch_inputs(
        self,
        batch: Union[torch.Tensor, Tuple, List]
    ) -> Tuple[
        torch.Tensor,
        Optional[torch.Tensor],
        Optional[torch.Tensor],
        Optional[torch.Tensor],
        Optional[torch.Tensor]
    ]:
        """
        Prepare batch inputs for the VAE.

        Args:
            batch: A batch of data from the dataloader.

        Returns:
            Tuple of (x, encoder_cond, decoder_cond, cond, indices).
        """
        encoder_cond = None
        decoder_cond = None
        cond = None
        indices = None

        if isinstance(batch, (tuple, list)):
            if len(batch) == 4:
                # Conditional VAE strategy where encoder and decoder conditions differ
                x, encoder_cond, decoder_cond, indices = batch
            elif len(batch) == 3:
                # Conditional VAE: batch is (img, cond, indices)
                x, cond, indices = batch
            elif len(batch) == 2:
                # Non-conditional VAE: batch is (img, indices)
                x, indices = batch
            else:
                x = batch[0] if len(batch) == 1 else batch
        else:
            x = batch

        return x, encoder_cond, decoder_cond, cond, indices

    def get_layer_statistics(
        self,
        phase: str = 'validation'
    ) -> Dict[int, Dict[str, float]]:
        """
        Get statistics for all layers from the most recent epoch.

        Args:
            phase: Either 'train' or 'validation'.

        Returns:
            Dictionary with statistics for each layer.
        """
        if not self.skips[phase]:
            logger.warning(f"No skip connection data for {phase}")
            return {}

        return self.generate_report(self.skips[phase], phase)

    def compare_phases(self) -> Dict[int, Dict[str, float]]:
        """
        Compare skip connection statistics between training and validation.

        Returns:
            Dictionary with difference statistics for each layer.
        """
        if not self.skips['train'] or not self.skips['validation']:
            logger.warning("Need both train and validation data for comparison")
            return {}

        comparison: Dict[int, Dict[str, float]] = {}

        train_report = self.generate_report(self.skips['train'], 'train')
        val_report = self.generate_report(self.skips['validation'], 'validation')

        for layer in train_report.keys():
            if layer in val_report:
                comparison[layer] = {
                    'mean_diff': abs(
                        train_report[layer]['avg_mean'] - val_report[layer]['avg_mean']
                    ),
                    'range_diff': abs(
                        (train_report[layer]['avg_max'] - train_report[layer]['avg_min']) -
                        (val_report[layer]['avg_max'] - val_report[layer]['avg_min'])
                    )
                }

        return comparison
