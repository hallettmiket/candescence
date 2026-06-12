"""
Purpose: Inference class for VAE analysis
Author: Hallett Lab
Date: 2026-01-27

Provides Inference class for running inference on trained VAE models
and analyzing latent space properties.
"""

import pickle
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
from sklearn.manifold import TSNE

from candescence.core.logging_config import get_logger
from candescence.tlv.utilities import convert_rgb_transformed_2_rgb
from candescence.tlv.inference.latent_embedding import LatentEmbedding

# Optional import
try:
    import umap
except ImportError:
    umap = None

logger = get_logger("candescence.tlv.inference.inference")


class Inference:
    """
    Inference class for analyzing trained VAE models.

    Manages latent space analysis, reconstruction evaluation,
    and report generation for VAE experiments.
    """

    def __init__(
        self,
        df: pd.DataFrame,
        trainer: Any,
        name: str
    ) -> None:
        """
        Initialize Inference object.

        Args:
            df: DataFrame of images to include in analyses.
            trainer: VAETrainer instance.
            name: Name identifier for this inference object.
        """
        from candescence.tlv.data import FullDataset

        self.config = trainer.config
        self.name = name
        self._latent_embeddings: Dict[str, LatentEmbedding] = {}
        self.df = df.copy()
        self.df.loc[:, ['hat_rgb_image', 'hat_hsv_image', 'hat_transformed_image']] = None
        self.trainer = trainer

        self.device = torch.device(
            self.config.device if torch.cuda.is_available() else "cpu"
        )
        trainer.vae.to(self.device)

        # Apply VAE to all images and create default latent embedding
        self.apply_vae(trainer, name)

        # Estimate backgrounds for reconstructed images
        self.df = FullDataset.estimate_backgrounds(
            df=self.df,
            config=self.config,
            prefix="hat_"
        )

        logger.info(f"Inference object '{name}' initialized with {len(df)} samples")

    @property
    def empty(self) -> bool:
        """Check if inference object is empty."""
        return len(self._latent_embeddings) == 0

    def get_dataframe(self) -> pd.DataFrame:
        """Get the inference DataFrame."""
        return self.df

    def get_ordered_ids_from_df(self) -> pd.Series:
        """Get ordered IDs from DataFrame."""
        return self.df['id']

    def get_latent_embedding(self, key: str) -> LatentEmbedding:
        """Get a specific latent embedding by key."""
        return self._latent_embeddings[key]

    def get_latent_embedding_keys(self) -> List[str]:
        """Get all latent embedding keys."""
        return list(self._latent_embeddings.keys())

    def save(self) -> Path:
        """
        Save inference object to file.

        Returns:
            Path to saved file.
        """
        # Move GPU tensors to CPU before saving
        self.trainer.vae = self.trainer.vae.cpu()
        file_path = self.config.analyses_path / f"inference_obj_{self.name}.pkl"

        with open(file_path, "wb") as f:
            pickle.dump(self, f)
        logger.info(f"Saved inference object to {file_path}")

        self.trainer.vae = self.trainer.vae.to(self.device)
        return file_path

    def _save_plot(self, fig: plt.Figure, filename: str) -> None:
        """Save a plot to the analyses directory."""
        path = self.config.analyses_path / f"{filename}.png"
        fig.savefig(path, dpi=150, bbox_inches="tight")

    @staticmethod
    def load(file_path: Union[str, Path]) -> 'Inference':
        """
        Load inference object from file.

        Args:
            file_path: Path to saved inference object.

        Returns:
            Loaded Inference object.
        """
        with open(file_path, "rb") as f:
            inference_object = pickle.load(f)

        # Move model back to device
        inference_object.trainer.vae = inference_object.trainer.vae.to(
            inference_object.device
        )
        logger.info(f"Loaded inference object from {file_path}")

        return inference_object

    def _get_decoder_conditions(
        self,
        cond_encoder: torch.Tensor
    ) -> torch.Tensor:
        """
        Get decoder conditions for strategy 9.5+.

        Args:
            cond_encoder: Encoder conditioning tensor.

        Returns:
            Decoder conditioning tensor.
        """
        fixed_values = list(
            self.config.conditional_decoder_fixed_values.values()
        )

        if cond_encoder.dim() == 1:
            cond_encoder = cond_encoder.unsqueeze(0)

        if len(fixed_values) != cond_encoder.shape[-1]:
            logger.error(f"fixed values: {fixed_values}")
            logger.error(f"cond_encoder: {cond_encoder}")
            raise ValueError(
                "The number of fixed values does not match cond_encoder columns."
            )

        cond = torch.tensor(
            fixed_values,
            device=cond_encoder.device,
            dtype=cond_encoder.dtype
        )
        return cond

    def apply_vae(self, trainer: Any, name: str) -> None:
        """
        Apply VAE to all images in DataFrame.

        Args:
            trainer: VAETrainer instance.
            name: Name for this latent embedding.
        """
        strategy = getattr(self.config, 'strategy', 0)
        latent_dim = getattr(self.config, 'latent_dim', 128)

        def pass_image_through_vae(id_value: Any) -> np.ndarray:
            row_idx = self.df[self.df['id'] == id_value].index
            if row_idx.empty:
                raise ValueError(f"ID {id_value} not found")
            row_idx = row_idx[0]

            trainer.vae.encoder.eval()
            trainer.vae.decoder.eval()

            with torch.no_grad():
                item = self.trainer.validation_dataloader.dataset.__getitem__(
                    index=row_idx, override=True, alt_df=self.df
                )

                if strategy in (14, 15, 16):
                    # Tendril-VAE: encoder(cond_enc), decoder(cond_dec)
                    img, cond_enc, cond_dec, _ = item
                    img = img.unsqueeze(0).to(self.device)
                    cond_enc = cond_enc.unsqueeze(0).to(self.device).float()
                    cond_dec = cond_dec.unsqueeze(0).to(self.device).float()

                    z, mu, logvar, skip = trainer.vae.encoder(img, cond_enc)
                    rec_img = trainer.vae.decoder(z, skip, cond_dec)

                elif strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13):
                    # List-based conditional
                    img, cond_enc, cond_dec, _ = item
                    cond_dec = self._get_decoder_conditions(cond_enc)
                    img = img.unsqueeze(0).to(self.device)
                    cond_enc = cond_enc.unsqueeze(0).to(self.device).float()
                    cond_dec = cond_dec.unsqueeze(0).to(self.device).float()

                    z, _, _, skip, _ = trainer.vae.encoder(img, [cond_enc, cond_dec])
                    rec_img = trainer.vae.decoder(
                        z, skip, trainer.vae.feature_map_shape, cond_dec
                    )

                elif strategy == 9:
                    img, cond_enc, cond_dec, _ = item
                    img = img.unsqueeze(0).to(self.device)
                    cond_enc = cond_enc.unsqueeze(0).to(self.device).float()
                    cond_dec = cond_dec.unsqueeze(0).to(self.device).float()

                    z, _, _, skip, _ = trainer.vae.encoder(img, [cond_enc, cond_dec])
                    rec_img = trainer.vae.decoder(
                        z, skip, trainer.vae.feature_map_shape, cond_dec
                    )

                elif strategy in (7, 8):
                    # Single-vector conditional
                    img, cond, _ = item
                    img = img.unsqueeze(0).to(self.device)
                    cond = cond.unsqueeze(0).to(self.device).float()

                    z, _, _, skip, _ = trainer.vae.encoder(img, cond)
                    rec_img = trainer.vae.decoder(
                        z, skip, trainer.vae.feature_map_shape, cond
                    )

                elif strategy in (1, 12):
                    # U-Net VAE (skip only)
                    img = item[0].unsqueeze(0).to(self.device)
                    enc_out = trainer.vae.encoder(img)
                    z, mu, logvar, skip, fmap_shape = enc_out
                    rec_img = trainer.vae.decoder(z, skip, fmap_shape)

                else:
                    # Non-conditional fallback
                    img = item[0].unsqueeze(0).to(self.device)
                    dummy_cond = torch.zeros(
                        1, trainer.vae.encoder.cond_dim, device=self.device
                    )
                    enc_out = trainer.vae.encoder(img, dummy_cond)
                    z = enc_out[0] if isinstance(enc_out, tuple) else enc_out
                    rec_img = trainer.vae.decoder(z)

                # Post-process and store
                rec_img_rgb = convert_rgb_transformed_2_rgb(rec_img.cpu().squeeze())
                self.df.at[row_idx, 'hat_rgb_image'] = rec_img_rgb
                self.df.at[row_idx, 'hat_hsv_image'] = rec_img_rgb.convert('HSV')
                self.df.at[row_idx, 'hat_transformed_image'] = rec_img

                z_np = z.cpu().detach().numpy().squeeze()
                return z_np

        self.df = self.df.reset_index(drop=True)
        embeddings = self.df['id'].apply(pass_image_through_vae).tolist()

        latent_cols = [f'latent_{i}' for i in range(latent_dim)]
        tmp = pd.DataFrame(embeddings, columns=latent_cols)
        tmp['id'] = self.df['id'].values
        tmp = LatentEmbedding(pd.DataFrame(tmp), self.device)
        self._latent_embeddings[name] = tmp

        logger.debug(f"VAE applied to {len(self.df)} images")

    def compute_mantel(self, key: str, property: np.ndarray) -> Dict[str, float]:
        """Compute Mantel test for latent embedding."""
        return self.get_latent_embedding(key).compute_mantel(property)

    def compute_silhouette(
        self,
        key: str = 'default',
        class_col: str = "manual_formation",
        metric: str = "euclidean"
    ) -> Dict[str, Any]:
        """
        Compute silhouette scores for clustering analysis.

        Args:
            key: Latent embedding key.
            class_col: Column name for class labels.
            metric: Distance metric.

        Returns:
            Dictionary with 'overall_score' and 'sample_scores'.
        """
        if class_col not in self.df.columns:
            raise ValueError(
                f"Label column '{class_col}' not found in the dataframe."
            )

        labels = self.df[class_col].values
        unique_labels = np.unique(labels)
        if len(unique_labels) < 2:
            raise ValueError(
                "Silhouette analysis requires at least 2 clusters/classes."
            )

        overall_score, sample_scores = self.get_latent_embedding(key).compute_silhouette(
            labels, metric
        )

        return {'overall_score': overall_score, 'sample_scores': sample_scores}

    def plot_silhouette_analysis(
        self,
        key: str = 'default',
        class_col: str = "manual_formation",
        save_path: Optional[Union[str, Path]] = None
    ) -> Tuple[plt.Figure, plt.Axes]:
        """
        Perform silhouette analysis and plot results.

        Args:
            key: Latent embedding key.
            class_col: Column name for class labels.
            save_path: Optional path to save plot.

        Returns:
            Tuple of (figure, axes).
        """
        result = self.compute_silhouette(key, class_col=class_col)
        overall_score = result['overall_score']
        sample_scores = result['sample_scores']

        class_labels = np.unique(self.df[class_col])
        n_clusters = len(class_labels)

        fig, ax = plt.subplots(1, 1, figsize=(8, 6))
        ax.set_title(f"Silhouette Analysis (Score: {overall_score:.2f})")
        ax.set_xlabel("Silhouette Coefficient")
        ax.set_ylabel("Cluster Label")

        y_lower = 10
        for i, label in enumerate(class_labels):
            cluster_silhouette_vals = sample_scores[self.df[class_col] == label]
            cluster_silhouette_vals.sort()

            y_upper = y_lower + cluster_silhouette_vals.shape[0]
            color = plt.cm.nipy_spectral(float(i) / n_clusters)
            ax.fill_betweenx(
                np.arange(y_lower, y_upper),
                0, cluster_silhouette_vals,
                facecolor=color, edgecolor=color, alpha=0.7
            )
            ax.text(-0.05, y_lower + 0.5 * cluster_silhouette_vals.shape[0], str(label))
            y_lower = y_upper + 10

        ax.axvline(x=overall_score, color="red", linestyle="--")

        if save_path:
            save_path = Path(save_path)
            if save_path.is_dir():
                save_path = save_path / f"silhouette_{class_col}.png"
            plt.savefig(save_path, bbox_inches='tight')
            plt.close()
        else:
            plt.show()

        return fig, ax

    def compute_class_distances(
        self,
        class_col: str,
        key: str = 'default',
        metric: str = 'euclidean'
    ) -> Dict[Any, float]:
        """
        Compute pairwise distances within each class.

        Args:
            class_col: Column name for class labels.
            key: Latent embedding key.
            metric: Distance metric.

        Returns:
            Dictionary mapping class values to average distances.
        """
        le = self.get_latent_embedding(key)

        class_values = self.df[class_col].unique()
        class_distances = {}
        for class_val in class_values:
            class_indices = self.df[self.df[class_col] == class_val].index
            tmp_df = LatentEmbedding(self.df.loc[class_indices].copy(), self.device)
            class_distances[class_val] = tmp_df.compute_distances(
                global_average=True
            )['global_average']
        return class_distances

    def compute_latent_density(
        self,
        key: str,
        bandwidth: float = 1.0,
        kernel: str = 'gaussian'
    ) -> Dict[str, Any]:
        """Compute latent space density."""
        le = self.get_latent_embedding(key)
        return le.compute_latent_density(bandwidth=bandwidth, kernel=kernel)

    def save_analyses_to_file(self, filename: str) -> None:
        """Save analysis reports to files."""
        with open(
            self.config.analyses_path / f"report_replicates_{filename}.pkl", "wb"
        ) as f:
            pickle.dump(self._report_replicates, f)
        with open(
            self.config.analyses_path / f"change_in_hsv_{filename}.pkl", "wb"
        ) as f:
            pickle.dump(self._change_in_hsv, f)

    # ---------------------- VISUALIZATION METHODS ---------------------- #

    def display_cluster_images(self, k: int = 5) -> None:
        """
        Display sample images from each cluster.

        Args:
            k: Number of images per cluster.
        """
        unique_clusters = self.df['hierarchical_clusters'].unique()

        for cluster in unique_clusters:
            cluster_rows = self.df[self.df['hierarchical_clusters'] == cluster]
            selected_rows = cluster_rows.sample(
                n=min(k, len(cluster_rows)), random_state=42
            )

            logger.info(
                f"Displaying {len(selected_rows)} images for cluster {cluster}"
            )
            fig, axes = plt.subplots(1, len(selected_rows), figsize=(15, 5))
            if len(selected_rows) == 1:
                axes = [axes]

            for ax, (_, row) in zip(axes, selected_rows.iterrows()):
                img = row['rgb_image']
                ax.imshow(img)
                ax.axis('off')
                ax.set_title(f"Cluster {cluster}")

            plt.show()

    # ---------------------- REPORT GENERATION ---------------------- #

    def generate_reports(self, key: str) -> None:
        """Generate analysis reports."""
        le = self.get_latent_embedding(key)._latent_embedding
        self._report_replicates = self.report_replicates(le)
        self._change_in_hsv = self.change_in_hsv()
        self.generate_text_report()

    def report_replicates(
        self,
        le: pd.DataFrame,
        verbose: bool = True
    ) -> Dict[str, Any]:
        """
        Generate report comparing replicate distances.

        Args:
            le: Latent embedding DataFrame.
            verbose: Whether to print progress.

        Returns:
            Dictionary of replicate analysis results.
        """
        replicate_distances = {}
        hue_differences = []
        sat_differences = []
        val_differences = []

        null_trial_means = []
        null_trial_std = []
        null_trial_hue_means = []
        null_trial_sat_means = []
        null_trial_val_means = []

        K = getattr(self.config, 'number_null_hypothesis_permutations', 100)
        latent_dim = getattr(self.config, 'latent_dim', 128)

        if verbose:
            logger.info('Deriving estimate for replicate distances')

        for rep_value, group in self.df.groupby('my_rep'):
            if len(group) != 2:
                continue

            id1, id2 = group['id'].tolist()

            try:
                emb1 = le.loc[le['id'] == id1].iloc[0, :latent_dim].values
                emb2 = le.loc[le['id'] == id2].iloc[0, :latent_dim].values
            except (KeyError, IndexError) as e:
                logger.warning(
                    f"Unable to find embedding for group {rep_value}: {e}"
                )
                continue

            replicate_distances[rep_value] = np.linalg.norm(emb1 - emb2)

            row1 = group.iloc[0]
            row2 = group.iloc[1]
            hue_differences.append(abs(row1['average_hue'] - row2['average_hue']))
            sat_differences.append(
                abs(row1['average_saturation'] - row2['average_saturation'])
            )
            val_differences.append(abs(row1['average_value'] - row2['average_value']))

        replicate_values = list(replicate_distances.values())
        replicate_mean = np.mean(replicate_values) if replicate_values else np.nan
        replicate_std = np.std(replicate_values) if replicate_values else np.nan

        avg_hue_diff = np.mean(hue_differences) if hue_differences else np.nan
        avg_sat_diff = np.mean(sat_differences) if sat_differences else np.nan
        avg_val_diff = np.mean(val_differences) if val_differences else np.nan

        if verbose:
            logger.info(f'Deriving null estimate: {K} permutations')

        for k in range(K):
            df_perm = self.df.copy()
            df_perm['my_rep_rand'] = np.random.permutation(df_perm['my_rep'].values)
            trial_distances = []
            trial_hue = []
            trial_sat = []
            trial_val = []

            for perm_value, group in df_perm.groupby('my_rep_rand'):
                if len(group) != 2:
                    continue
                id1, id2 = group['id'].tolist()
                try:
                    emb1 = le.loc[id1].values
                    emb2 = le.loc[id2].values
                except KeyError:
                    continue
                dist = np.linalg.norm(emb1 - emb2)
                trial_distances.append(dist)

                row1 = group.iloc[0]
                row2 = group.iloc[1]
                trial_hue.append(abs(row1['average_hue'] - row2['average_hue']))
                trial_sat.append(
                    abs(row1['average_saturation'] - row2['average_saturation'])
                )
                trial_val.append(abs(row1['average_value'] - row2['average_value']))

            if trial_distances:
                null_trial_means.append(np.mean(trial_distances))
                null_trial_std.append(np.std(trial_distances))
            if trial_hue:
                null_trial_hue_means.append(np.mean(trial_hue))
                null_trial_sat_means.append(np.mean(trial_sat))
                null_trial_val_means.append(np.mean(trial_val))

        null_mean = np.mean(null_trial_means) if null_trial_means else np.nan
        null_std = np.std(null_trial_std) if null_trial_std else np.nan
        null_avg_hue_diff = (
            np.mean(null_trial_hue_means) if null_trial_hue_means else np.nan
        )
        null_avg_sat_diff = (
            np.mean(null_trial_sat_means) if null_trial_sat_means else np.nan
        )
        null_avg_val_diff = (
            np.mean(null_trial_val_means) if null_trial_val_means else np.nan
        )

        def kl_divergence_normal(rep_mean, rep_std, n_mean, n_std):
            if rep_std == 0 or n_std == 0:
                return np.nan
            term1 = np.log(n_std / rep_std)
            term2 = (rep_std**2 + (rep_mean - n_mean)**2) / (2 * (n_std**2))
            return term1 + term2 - 0.5

        kl = kl_divergence_normal(replicate_mean, replicate_std, null_mean, null_std)

        return {
            'replicate_mean': replicate_mean,
            'replicate_std': replicate_std,
            'avg_hue_diff': avg_hue_diff,
            'avg_sat_diff': avg_sat_diff,
            'avg_val_diff': avg_val_diff,
            'null_mean': null_mean,
            'null_std': null_std,
            'null_avg_hue_diff': null_avg_hue_diff,
            'null_avg_sat_diff': null_avg_sat_diff,
            'null_avg_val_diff': null_avg_val_diff,
            'kl(Obs||Null)': kl
        }

    def change_in_hsv(self) -> Dict[str, float]:
        """
        Compute changes in HSV between original and reconstructed images.

        All HSV values are in storage scale (0-255) from PIL HSV mode.
        To convert to normalized (0-1) scale, divide by 255.
        """
        stats = {
            # Method 1 - global changes
            'orig_ave_hue': self.df['average_hue'].mean(),
            'orig_ave_saturation': self.df['average_saturation'].mean(),
            'orig_ave_value': self.df['average_value'].mean(),
            'orig_std_hue': self.df['average_hue'].std(),
            'orig_std_saturation': self.df['average_saturation'].std(),
            'orig_std_value': self.df['average_value'].std(),

            'hat_ave_hue': self.df['hat_average_hue'].mean(),
            'hat_ave_saturation': self.df['hat_average_saturation'].mean(),
            'hat_ave_value': self.df['hat_average_value'].mean(),
            'hat_std_hue': self.df['hat_average_hue'].std(),
            'hat_std_saturation': self.df['hat_average_saturation'].std(),
            'hat_std_value': self.df['hat_average_value'].std(),

            # Method 2 - delta change
            'average_difference_hue': (
                self.df['average_hue'] - self.df['hat_average_hue']
            ).mean(),
            'average_difference_saturation': (
                self.df['average_saturation'] - self.df['hat_average_saturation']
            ).mean(),
            'average_difference_value': (
                self.df['average_value'] - self.df['hat_average_value']
            ).mean(),

            'variance_difference_hue': (
                self.df['average_hue'] - self.df['hat_average_hue']
            ).std(),
            'variance_difference_saturation': (
                self.df['average_saturation'] - self.df['hat_average_saturation']
            ).std(),
            'variance_difference_value': (
                self.df['average_value'] - self.df['hat_average_value']
            ).std(),

            # Method 3 - delta delta
            'average_matched_difference_hue': self.df.groupby("my_rep").apply(
                lambda group: (
                    (group.iloc[0]["average_hue"] - group.iloc[1]["average_hue"]) -
                    (group.iloc[0]["hat_average_hue"] - group.iloc[1]["hat_average_hue"])
                ) if len(group) == 2 else np.nan
            ).mean(),

            'average_matched_difference_saturation': self.df.groupby("my_rep").apply(
                lambda group: (
                    (group.iloc[0]["average_saturation"] - group.iloc[1]["average_saturation"]) -
                    (group.iloc[0]["hat_average_saturation"] - group.iloc[1]["hat_average_saturation"])
                ) if len(group) == 2 else np.nan
            ).mean(),

            'average_matched_difference_value': self.df.groupby("my_rep").apply(
                lambda group: (
                    (group.iloc[0]["average_value"] - group.iloc[1]["average_value"]) -
                    (group.iloc[0]["hat_average_value"] - group.iloc[1]["hat_average_value"])
                ) if len(group) == 2 else np.nan
            ).mean()
        }

        return stats

    def generate_text_report(self) -> None:
        """Print formatted text report."""
        stats = self._change_in_hsv
        other = self._report_replicates

        logger.info("\n------\n\tHue")
        logger.info(
            f"\t original images: {stats['orig_ave_hue']}, "
            f"+/- {stats['orig_std_hue']}"
        )
        logger.info(
            f"\t estimated images: {stats['hat_ave_hue']}, "
            f"+/- {stats['hat_std_hue']}"
        )
        logger.info(
            f"\t Average difference between replicates: {other['avg_hue_diff']} "
            f"vs random {other['null_avg_hue_diff']}"
        )
        logger.info(
            f"\t Average difference (orig vs estimated): "
            f"{stats['average_difference_hue']}, +/- {stats['variance_difference_hue']}"
        )

        logger.info("------\n\tSaturation")
        logger.info(
            f"\t original images: {stats['orig_ave_saturation']}, "
            f"+/- {stats['orig_std_saturation']}"
        )
        logger.info(
            f"\t estimated images: {stats['hat_ave_saturation']}, "
            f"+/- {stats['hat_std_saturation']}"
        )
        logger.info(
            f"\t Average difference between replicates: {other['avg_sat_diff']} "
            f"vs {other['null_avg_sat_diff']}"
        )

        logger.info("------\n\tValue")
        logger.info(
            f"\t original images: {stats['orig_ave_value']}, "
            f"+/- {stats['orig_std_value']}"
        )
        logger.info(
            f"\t estimated images: {stats['hat_ave_value']}, "
            f"+/- {stats['hat_std_value']}"
        )

        logger.info("------\n\t\tDistance between replicates")
        logger.info(
            f"Distance between replicates (latent): {other['replicate_mean']}, "
            f"+/- {other['replicate_std']}"
        )
        logger.info(f"\t\t random pairs: {other['null_mean']}, +/- {other['null_std']}")
        logger.info(f"KL(Obs || Null): {other['kl(Obs||Null)']}")

    def compute_spans(self, factor: float = 10.0) -> Tuple[List[int], List[Tuple[float, float]]]:
        """
        Compute per-dimension spans for latent walk.

        Args:
            factor: Multiplier for standard deviation.

        Returns:
            Tuple of (dims, spans) where spans are (min, max) tuples.
        """
        self.trainer.vae.encoder.eval()

        all_mu_list = []
        with torch.no_grad():
            for batch in self.trainer.validation_dataloader:
                x = batch[0].to(self.device).float()
                strategy = getattr(self.config, 'strategy', 0)
                if strategy in (7, 8, 9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16):
                    cond = batch[1].to(self.device).float()
                    out = self.trainer.vae.encoder(x, cond)
                else:
                    out = self.trainer.vae.encoder(x)
                mu = out[0] if isinstance(out, (tuple, list)) else out
                all_mu_list.append(mu.cpu().numpy())

        all_mu = np.concatenate(all_mu_list, axis=0)
        stds = all_mu.std(axis=0)

        dims = list(range(stds.shape[0]))
        spans = [(-factor * stds[d], factor * stds[d]) for d in dims]

        return dims, spans

    def latent_walk(
        self,
        id_value: Optional[Any] = None,
        idx: Optional[int] = None,
        dims: Optional[List[int]] = None,
        n_steps: int = 7,
        span: Tuple[float, float] = (-2.0, 2.0),
        spans: Optional[List[Tuple[float, float]]] = None,
        figsize: Tuple[int, int] = (10, 10)
    ) -> None:
        """
        Perform latent walk visualization.

        Args:
            id_value: Image ID to use as starting point.
            idx: Alternative index to use.
            dims: Dimensions to walk.
            n_steps: Number of steps in each direction.
            span: Default span for all dimensions.
            spans: Per-dimension spans.
            figsize: Figure size.
        """
        if idx is None:
            if id_value is None:
                raise ValueError("Must provide either idx or id_value")
            matching = self.df.index[self.df['id'] == id_value].tolist()
            if not matching:
                raise KeyError(f"id_value {id_value} not found in inference df")
            idx = matching[0]

        item = self.trainer.validation_dataloader.dataset.__getitem__(idx)
        strategy = getattr(self.config, 'strategy', 0)

        # Unpack based on strategy
        if strategy in (0, 1, 12):
            img, _ = item
            cond = None
            cond_enc = None
        elif strategy in (7, 8):
            img, cond, _ = item
            cond = cond.unsqueeze(0).to(self.device).float()
            cond_enc = cond
        elif strategy in (9, 9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16):
            img, cond_enc, cond_dec, _ = item
            cond_enc = cond_enc.unsqueeze(0).to(self.device).float()
            cond = self._get_decoder_conditions(cond_enc).unsqueeze(0).to(self.device).float()
        else:
            raise ValueError(f"Unrecognized strategy {strategy}")

        img = img.unsqueeze(0).to(self.device).float()

        # Run encoder
        self.trainer.vae.encoder.eval()
        with torch.no_grad():
            if strategy in (7, 8):
                z, mu, logvar, skip, fmap_shape = self.trainer.vae.encoder(img, cond)
            elif strategy in (9, 9.5, 9.6, 9.7, 9.8, 9.9, 13):
                z, mu, logvar, skip, fmap_shape = self.trainer.vae.encoder(img, cond_enc)
            elif strategy in (14, 15, 16):
                z, mu, logvar, skip = self.trainer.vae.encoder(img, cond_enc)
                fmap_shape = None
            else:
                enc_out = self.trainer.vae.encoder(img)
                if isinstance(enc_out, tuple) and len(enc_out) == 5:
                    z, mu, logvar, skip, fmap_shape = enc_out
                else:
                    z, mu, logvar = enc_out
                    skip = None
                    fmap_shape = None

        # Build decode function
        if strategy == 0:
            decode_fn = lambda z_mod: self.trainer.vae.decoder(z_mod)
        elif strategy in (1, 12):
            decode_fn = lambda z_mod: self.trainer.vae.decoder(z_mod, skip, fmap_shape)
        elif strategy in (14, 15, 16):
            decode_fn = lambda z_mod: self.trainer.vae.decoder(z_mod, skip, cond)
        else:
            decode_fn = lambda z_mod: self.trainer.vae.decoder(
                z_mod, skip, fmap_shape, cond
            )

        # Build walk dimensions and spans
        latent_size = z.shape[1]
        walk_dims = dims if dims is not None else list(range(latent_size))

        if spans is not None:
            if len(spans) != len(walk_dims):
                raise ValueError("`spans` must be same length as `dims`")
            spans_list = spans
        else:
            spans_list = [span] * len(walk_dims)

        # Generate grid
        recon_grid = []
        for i, dim in enumerate(walk_dims):
            min_d, max_d = spans_list[i]
            alphas = np.linspace(min_d, max_d, n_steps)
            row_recons = []
            for alpha in alphas:
                z_mod = z.clone()
                z_mod[:, dim] = z[:, dim] + alpha
                with torch.no_grad():
                    rec = decode_fn(z_mod)
                row_recons.append(rec.cpu().squeeze())
            recon_grid.append(row_recons)

        # Plot
        orig_np = convert_rgb_transformed_2_rgb(img.cpu().squeeze())
        n_rows = len(walk_dims)
        n_cols = n_steps

        fig = plt.figure(figsize=(n_cols * 1.5, (n_rows + 1) * 1.5))
        gs = fig.add_gridspec(n_rows + 1, n_cols, wspace=0.05, hspace=0.05)

        ax0 = fig.add_subplot(gs[0, :])
        ax0.imshow(orig_np)
        ax0.set_title("Original Image", fontsize=8)
        ax0.axis("off")

        for i in range(n_rows):
            min_d, max_d = spans_list[i]
            alphas = np.linspace(min_d, max_d, n_steps)
            for j in range(n_cols):
                ax = fig.add_subplot(gs[i + 1, j])
                ax.imshow(convert_rgb_transformed_2_rgb(recon_grid[i][j]))
                ax.axis("off")
                if i == 0:
                    ax.set_title(f"{alphas[j]:.2f}", fontsize=6)
                if j == 0:
                    ax.set_ylabel(
                        f"dim {walk_dims[i]}", fontsize=6,
                        rotation=0, labelpad=15
                    )

        plt.tight_layout()
        fname = "latent_walk.png"
        plt.savefig(self.config.analyses_path / fname, dpi=200)
        plt.show()

    def show_nearest_neighbor(
        self,
        key: str = 'default',
        query_id: Optional[Any] = None,
        top_k: int = 1
    ) -> None:
        """
        Show nearest neighbors in latent space.

        Args:
            key: Latent embedding key.
            query_id: Query image ID.
            top_k: Number of neighbors to show.
        """
        le_df = self.get_latent_embedding(key)._latent_embedding

        if query_id is None:
            query_id = le_df['id'].iloc[0]

        query_vec = le_df.loc[le_df['id'] == query_id].drop(columns='id').values.squeeze()
        all_vecs = le_df.drop(columns='id').values
        dists = np.linalg.norm(all_vecs - query_vec, axis=1)

        dist_df = le_df[['id']].copy()
        dist_df['distance'] = dists
        dist_df = dist_df[dist_df['id'] != query_id].sort_values('distance')

        neighbors = dist_df['id'].iloc[:top_k].tolist()

        ncols = 1 + len(neighbors)
        fig, axes = plt.subplots(2, ncols, figsize=(4 * ncols, 8))

        ids_to_show = [query_id] + neighbors
        for col, img_id in enumerate(ids_to_show):
            row = self.df[self.df['id'] == img_id].iloc[0]

            axes[0, col].imshow(row['rgb_image'])
            axes[0, col].set_title(f"Orig ID {img_id}")
            axes[0, col].axis('off')

            axes[1, col].imshow(row['hat_rgb_image'])
            axes[1, col].set_title(f"Recon ID {img_id}")
            axes[1, col].axis('off')

        plt.tight_layout()

        fname = f"nearest_neighbour_{query_id}.png"
        fig.savefig(self.config.analyses_path / fname, dpi=200)
        plt.show()

    def plot_reconstructions(
        self,
        subset_df: pd.DataFrame,
        k: int = 5,
        seed: int = 0
    ) -> None:
        """
        Plot original and reconstructed images.

        Args:
            subset_df: DataFrame subset to sample from.
            k: Number of images to show.
            seed: Random seed for sampling.
        """
        subset_df = subset_df.reset_index(drop=True)
        sampled = subset_df.sample(n=k, random_state=seed)

        originals = []
        reconstructions = []

        for pos in sampled.index:
            img_id = subset_df.loc[pos, 'id']
            orig_pil = self.df.loc[self.df['id'] == img_id, 'rgb_image'].iloc[0]
            originals.append(orig_pil)

            recon_pil = self.df.loc[self.df['id'] == img_id, 'hat_rgb_image'].iloc[0]
            reconstructions.append(recon_pil)

        fig, axes = plt.subplots(2, k, figsize=(2 * k, 4))
        for i in range(k):
            axes[0, i].imshow(originals[i])
            axes[0, i].set_title("Original")
            axes[0, i].axis('off')
            axes[1, i].imshow(reconstructions[i])
            axes[1, i].set_title("Reconstruction")
            axes[1, i].axis('off')

        plt.tight_layout()

        fname = f"reconstructions_{self.name}.png"
        fig.savefig(self.config.analyses_path / fname, dpi=200)
        plt.show()

    def plot_latent_2d(
        self,
        key: str = 'default',
        method: str = 'umap',
        color_by: str = 'average_hue',
        random_state: int = 0
    ) -> None:
        """
        Plot 2D visualization of latent space.

        Args:
            key: Latent embedding key.
            method: Dimensionality reduction method ('umap' or 'tsne').
            color_by: Column to color points by.
            random_state: Random seed.
        """
        le_df = self.get_latent_embedding(key)._latent_embedding.copy()
        meta = self.df[['id', 'average_hue', 'colony_size']].copy()
        if 'hierarchical_clusters' in self.df.columns:
            meta['hierarchical_clusters'] = self.df['hierarchical_clusters']

        df = le_df.merge(meta, on='id')

        latent_cols = [c for c in le_df.columns if c.startswith('latent_')]
        X = df[latent_cols].values

        if method.lower() == 'umap':
            if umap is None:
                raise RuntimeError("umap-learn is not installed")
            reducer = umap.UMAP(n_components=2, random_state=random_state)
        else:
            reducer = TSNE(n_components=2, random_state=random_state)
        Z = reducer.fit_transform(X)

        df['emb1'], df['emb2'] = Z[:, 0], Z[:, 1]
        fig, ax = plt.subplots(figsize=(8, 6))
        sc = ax.scatter(df['emb1'], df['emb2'], c=df[color_by], s=12, alpha=0.8)
        cb = plt.colorbar(sc, ax=ax)
        cb.set_label(color_by)
        ax.set_title(f"{method.upper()} of latent space colored by {color_by}")
        ax.set_xlabel('Component 1')
        ax.set_ylabel('Component 2')
        ax.grid(False)

        plt.tight_layout()
        self._save_plot(fig, f"{method}_{key}_{color_by}")
        plt.show()

    # ──────────────────────────────────────────────────────────────
    #  Tendril / replicate diagnostics (delegated to module)
    # ──────────────────────────────────────────────────────────────

    def compute_intermediate_distances_for_pairs(
        self,
        pairs: List[Tuple[int, int]],
        *,
        layers: Tuple[str, ...] = ("x0", "x1", "x2", "x3"),
        pool_hw: int = 8,
    ) -> "pd.DataFrame":
        """Delegate to :func:`tendril_replicate_diagnostics.compute_intermediate_distances_for_pairs`."""
        from candescence.tlv.inference.tendril_replicate_diagnostics import (
            compute_intermediate_distances_for_pairs as _compute,
        )
        strategy = getattr(self.config, "strategy", 14)
        return _compute(
            self.df, self.trainer.vae, pairs,
            device=self.device, layers=layers, pool_hw=pool_hw,
            strategy=strategy,
        )

    def inspect_distance_peaks_for_layer(
        self,
        layer: str = "x1",
        bins: int = 60,
        n_examples_per_peak: int = 6,
        max_peaks: int = 3,
        pool_hw: int = 8,
    ) -> Dict[str, Any]:
        """Delegate to :func:`tendril_replicate_diagnostics.inspect_distance_peaks_for_layer`."""
        from candescence.tlv.inference.tendril_replicate_diagnostics import (
            build_replicate_pairs,
            compute_intermediate_distances_for_pairs as _compute,
            inspect_distance_peaks_for_layer as _inspect,
        )
        strategy = getattr(self.config, "strategy", 14)
        pairs = build_replicate_pairs(self.df)
        dist_df = _compute(
            self.df, self.trainer.vae, pairs,
            device=self.device, layers=(layer,), pool_hw=pool_hw,
            strategy=strategy,
        )
        return _inspect(dist_df, self.df, layer=layer, bins=bins,
                        n_examples_per_peak=n_examples_per_peak,
                        max_peaks=max_peaks)
