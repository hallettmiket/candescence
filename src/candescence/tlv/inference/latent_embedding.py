"""
Purpose: Latent embedding analysis for VAE latent spaces
Author: Hallett Lab
Date: 2026-01-27

Provides LatentEmbedding class for analyzing and clustering VAE latent spaces.
"""

import pickle
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
from scipy.cluster.hierarchy import dendrogram, fcluster, linkage
from scipy.signal import find_peaks
from scipy.spatial.distance import pdist, squareform
from sklearn.cluster import (
    DBSCAN,
    AgglomerativeClustering,
    KMeans,
    MeanShift,
    SpectralClustering,
)
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
from sklearn.metrics import silhouette_samples, silhouette_score
from sklearn.mixture import BayesianGaussianMixture, GaussianMixture
from sklearn.neighbors import KernelDensity, kneighbors_graph

from candescence.core.logging_config import get_logger

# Optional imports with graceful fallback
try:
    import umap
except ImportError:
    umap = None

try:
    import igraph as ig
    import leidenalg
except ImportError:
    ig = None
    leidenalg = None

try:
    import hdbscan
except ImportError:
    hdbscan = None

try:
    import community as community_louvain
except ImportError:
    community_louvain = None

try:
    from skbio.stats.distance import mantel
except ImportError:
    mantel = None

logger = get_logger("candescence.tlv.inference.latent_embedding")


class LatentEmbedding:
    """
    Class for analyzing VAE latent space embeddings.

    Provides methods for distance computation, clustering, dimensionality
    reduction, and continuity analysis of latent representations.
    """

    def __init__(
        self,
        embedding_df: pd.DataFrame,
        device: torch.device,
        metric: str = 'euclidean'
    ) -> None:
        """
        Initialize a LatentEmbedding instance.

        Args:
            embedding_df: DataFrame containing latent embeddings with 'id' column
                and numerical feature columns.
            device: PyTorch device for tensor computations.
            metric: Distance metric to use.
        """
        if not isinstance(embedding_df, pd.DataFrame):
            raise ValueError("embedding_df must be a pandas DataFrame.")

        self._latent_embedding = embedding_df.copy()
        self._ids = self._latent_embedding['id']
        self.device = device

        self._latent_matrix = self.to_matrix()
        self._latent_tensor = self.to_tensor()
        (
            self._latent_distances_df,
            self._latent_distances_matrix,
            self._latent_distances_tensor
        ) = self.compute_distances(
            metric='euclidean',
            return_average=False,
            global_average=False
        )

        logger.debug(f"LatentEmbedding initialized with {len(embedding_df)} samples")

    def __repr__(self) -> str:
        return (
            f"LatentEmbedding with {len(self._latent_embedding)} rows and "
            f"{len(self._latent_embedding.columns)} columns:\n"
            f"{self._latent_embedding.head()}"
        )

    def to_matrix(self, drop_non_numeric: bool = True) -> np.ndarray:
        """
        Convert the latent_embedding DataFrame to a NumPy matrix.

        Args:
            drop_non_numeric: If True, drops non-numeric columns (e.g., 'id').

        Returns:
            NumPy array representation of the latent embeddings.
        """
        if drop_non_numeric:
            numeric_df = self._latent_embedding.select_dtypes(include=[np.number])
            return numeric_df.to_numpy()
        else:
            return self._latent_embedding.to_numpy()

    def to_tensor(
        self,
        indices: Union[str, List[Any]] = 'all'
    ) -> torch.Tensor:
        """
        Convert latent embeddings to PyTorch tensor.

        Args:
            indices: 'all' for all samples, or list of ids to select.

        Returns:
            Tensor of latent embeddings.
        """
        if isinstance(indices, str) and indices.lower() == 'all':
            selected_df = self._latent_embedding
        else:
            selected_df = self._latent_embedding[
                self._latent_embedding['id'].isin(indices)
            ]
        selected_embeddings = selected_df.drop('id', axis=1).values
        return torch.tensor(selected_embeddings, device=self.device)

    def compute_distances(
        self,
        metric: str = 'euclidean',
        return_average: bool = False,
        global_average: bool = False
    ) -> Union[Tuple[pd.DataFrame, np.ndarray, torch.Tensor], Dict[str, Any]]:
        """
        Compute pairwise distances between latent embeddings.

        Args:
            metric: Distance metric (currently only 'euclidean' supported).
            return_average: If True, include average distance per sample.
            global_average: If True, return dict with global average.

        Returns:
            If global_average: dict with 'distances' and 'global_average'.
            Otherwise: tuple of (DataFrame, array, tensor) of distances.
        """
        selected_tensor = self._latent_tensor

        # Compute pairwise distances (p=2 for Euclidean)
        distances_tensor = torch.cdist(selected_tensor, selected_tensor, p=2)
        # Set diagonal to zero
        distances_tensor.diagonal(dim1=-2, dim2=-1).fill_(0)

        distances_array = distances_tensor.cpu().detach().numpy()

        distances_df = pd.DataFrame(
            distances_array,
            index=self._ids,
            columns=self._ids
        )

        if return_average:
            n = distances_array.shape[0]
            avg_distance = distances_array.sum(axis=1) / (n - 1)
            distances_df['avg_distance'] = avg_distance

        if global_average:
            n = distances_array.shape[0]
            global_avg_distance = distances_array.sum() / (n * (n - 1))
            return {
                'distances': distances_df,
                'global_average': global_avg_distance
            }

        return distances_df, distances_array, distances_tensor

    def compute_silhouette(
        self,
        labels: Union[np.ndarray, List],
        metric: str = "euclidean"
    ) -> Tuple[float, np.ndarray]:
        """
        Compute silhouette scores for clustering.

        Args:
            labels: Cluster labels for each sample.
            metric: Distance metric to use.

        Returns:
            Tuple of (overall_score, sample_scores).
        """
        m = self._latent_matrix
        overall_score = silhouette_score(m, labels, metric=metric)
        sample_scores = silhouette_samples(m, labels, metric=metric)
        return overall_score, sample_scores

    def compute_mantel(self, property: Optional[np.ndarray] = None) -> Dict[str, float]:
        """
        Compute Mantel test between latent distances and property distances.

        Args:
            property: 1D array of property values.

        Returns:
            Dictionary with 'mantel_statistic' and 'p_value'.
        """
        if mantel is None:
            raise RuntimeError("skbio is required for Mantel test")

        if property is None:
            raise ValueError("The 'property' input cannot be None.")
        if not isinstance(property, np.ndarray):
            property = np.array(property)
        if property.ndim != 1:
            raise ValueError("The 'property' input must be a 1D array or list.")
        if self._latent_distances_matrix.shape[0] != self._latent_distances_matrix.shape[1]:
            raise ValueError("The latent distance matrix must be square.")

        # Compute property differences
        P_differences = squareform(
            pdist(property[:, np.newaxis], metric='euclidean')
        )
        np.fill_diagonal(P_differences, 0)
        np.fill_diagonal(self._latent_distances_matrix, 0)

        self._latent_distances_matrix = np.round(
            self._latent_distances_matrix, decimals=8
        )
        self._latent_distances_matrix = (
            self._latent_distances_matrix + self._latent_distances_matrix.T
        ) / 2

        # Validate matrices
        for i in range(self._latent_distances_matrix.shape[0]):
            if self._latent_distances_matrix[i, i] != 0:
                raise ValueError(
                    f"Diagonal element at index {i} of latent matrix is not zero."
                )
            if P_differences[i, i] != 0:
                raise ValueError(
                    f"Diagonal element at index {i} of property matrix is not zero."
                )

        if not np.allclose(
            self._latent_distances_matrix, self._latent_distances_matrix.T
        ):
            raise ValueError("The latent distance matrix must be symmetric.")
        if not np.allclose(P_differences, P_differences.T):
            raise ValueError("The property distance matrix must be symmetric.")
        if np.isnan(self._latent_distances_matrix).any():
            raise ValueError("NaN values found in the latent distance matrix.")
        if np.isnan(P_differences).any():
            raise ValueError("NaN values found in the property distance matrix.")

        self._latent_distances_matrix = self._latent_distances_matrix.astype(np.float32)
        P_differences = P_differences.astype(np.float32)

        # Perform Mantel test
        self.mantel_stat, self.p_value, _ = mantel(
            self._latent_distances_matrix, P_differences,
            method='pearson', permutations=999
        )

        return {'mantel_statistic': self.mantel_stat, 'p_value': self.p_value}

    def compute_latent_density(
        self,
        bandwidth: float = 1.0,
        kernel: str = 'gaussian'
    ) -> Dict[str, Any]:
        """
        Compute kernel density estimate of latent space.

        Args:
            bandwidth: KDE bandwidth parameter.
            kernel: Kernel type ('gaussian', etc.).

        Returns:
            Dictionary with 'densities' and 'avg_density'.
        """
        X = self._latent_matrix
        kde = KernelDensity(bandwidth=bandwidth, kernel=kernel)
        kde.fit(X)

        log_p = kde.score_samples(X)
        p = np.exp(log_p)

        return {
            'densities': p,
            'avg_density': float(p.mean())
        }

    def compute_density_per_dim(
        self,
        bandwidth: float = 0.5,
        kernel: str = 'gaussian',
        grid_size: int = 200
    ) -> Tuple[List[np.ndarray], List[np.ndarray]]:
        """
        Compute per-dimension density estimates.

        Args:
            bandwidth: KDE bandwidth.
            kernel: Kernel type.
            grid_size: Number of grid points per dimension.

        Returns:
            Tuple of (grids, densities) lists.
        """
        Z = self.to_matrix()
        D = Z.shape[1]
        grids, dens = [], []

        for d in range(D):
            col = Z[:, d][:, None]
            kde = KernelDensity(bandwidth=bandwidth, kernel=kernel)
            kde.fit(col)
            vmin, vmax = col.min(), col.max()
            grid = np.linspace(vmin, vmax, grid_size)[:, None]
            p = np.exp(kde.score_samples(grid))
            grids.append(grid.ravel())
            dens.append(p)

        self.density_grids = grids
        self.density_values = dens
        return grids, dens

    def assess_continuity(
        self,
        density_threshold_frac: float = 0.01,
        prominence_frac: float = 0.1,
        min_gap_width: int = 3,
        tail_ignore_frac: float = 0.1
    ) -> Tuple[np.ndarray, Dict[str, Dict]]:
        """
        Assess continuity of latent dimensions.

        Args:
            density_threshold_frac: Fraction of max density for gap detection.
            prominence_frac: Minimum peak prominence fraction.
            min_gap_width: Minimum gap width to count.
            tail_ignore_frac: Fraction of tails to ignore.

        Returns:
            Tuple of (continuous_mask, details_dict).
        """
        if not hasattr(self, 'density_values'):
            self.compute_density_per_dim()

        D = len(self.density_values)
        gaps, modes = {}, {}

        for d, dens in enumerate(self.density_values):
            grid = self.density_grids[d]
            N = len(grid)

            # Restrict to central region
            lo = int(N * tail_ignore_frac)
            hi = int(N * (1 - tail_ignore_frac))
            central_dens = dens[lo:hi]
            central_grid = grid[lo:hi]

            # Find interior gaps
            thresh = central_dens.max() * density_threshold_frac
            low_mask = central_dens < thresh
            runs, in_run, run_start = [], False, None

            for i, is_low in enumerate(low_mask):
                if is_low and not in_run:
                    in_run, run_start = True, i
                elif not is_low and in_run:
                    length = i - run_start
                    if length >= min_gap_width:
                        runs.append((central_grid[run_start], central_grid[i-1]))
                    in_run = False

            if in_run and len(low_mask) - run_start >= min_gap_width:
                runs.append((central_grid[run_start], central_grid[-1]))

            gaps[d] = runs

            # Count modes
            prom = dens.max() * prominence_frac
            peaks, _ = find_peaks(dens, prominence=prom)
            modes[d] = len(peaks)

        continuous_mask = np.array([
            (len(gaps[d]) == 0 and modes[d] == 1)
            for d in range(D)
        ])

        return continuous_mask, {'gaps': gaps, 'modes': modes}

    def detect_multimodal_dims(
        self,
        prominence_frac: float = 0.1
    ) -> Dict[int, int]:
        """
        Detect multimodal dimensions in latent space.

        Args:
            prominence_frac: Minimum peak prominence fraction.

        Returns:
            Dictionary mapping dimension index to mode count.
        """
        if not hasattr(self, 'density_values'):
            self.compute_density_per_dim()

        mode_counts = {}
        for d, dens in enumerate(self.density_values):
            prom = dens.max() * prominence_frac
            peaks, _ = find_peaks(dens, prominence=prom)
            mode_counts[d] = len(peaks)
        return mode_counts

    def continuity_print(
        self,
        density_threshold_frac: float = 0.01,
        prominence_frac: float = 0.1,
        min_gap_width: int = 5,
        tail_ignore_frac: float = 0.1
    ) -> str:
        """
        Generate formatted continuity report.

        Returns:
            Formatted string with continuity analysis.
        """
        self.compute_density_per_dim()

        cont_mask, details = self.assess_continuity(
            density_threshold_frac=density_threshold_frac,
            prominence_frac=prominence_frac,
            min_gap_width=min_gap_width,
            tail_ignore_frac=tail_ignore_frac
        )
        gaps = details['gaps']
        modes = details['modes']

        lines = [
            "Latent-Space Continuity Summary",
            f"  thresholds: gaps <{density_threshold_frac*100:.1f}% of peak, "
            f"prominence >{prominence_frac*100:.1f}%, ignore tails {tail_ignore_frac*100:.0f}%",
            ""
        ]
        for d in range(len(cont_mask)):
            status = "continuous" if cont_mask[d] else "discontinuous"
            desc = []
            if gaps[d]:
                desc.append(f"{len(gaps[d])} gap(s)")
            if modes[d] != 1:
                desc.append(f"{modes[d]} mode(s)")
            detail = ", ".join(desc) if desc else "no interior gaps, 1 mode"
            lines.append(f"  dim {d:2d}: {status} ({detail})")

        return "\n".join(lines)

    @staticmethod
    def save_latent_embedding(
        latent_embedding: 'LatentEmbedding',
        file_path: Union[str, Path]
    ) -> None:
        """Save LatentEmbedding object to file."""
        with open(file_path, 'wb') as f:
            pickle.dump(latent_embedding, f)
        logger.info(f"LatentEmbedding object saved to {file_path}")

    @staticmethod
    def load_latent_embedding(file_path: Union[str, Path]) -> 'LatentEmbedding':
        """Load LatentEmbedding object from file."""
        with open(file_path, 'rb') as f:
            latent_embedding = pickle.load(f)
        logger.info(f"LatentEmbedding object loaded from {file_path}")
        return latent_embedding

    # ---------------------- DIMENSIONALITY REDUCTION ---------------------- #

    def apply_pca(self, n_components: int = 2) -> np.ndarray:
        """Apply PCA dimensionality reduction."""
        pca = PCA(n_components=n_components)
        return pca.fit_transform(self._latent_matrix)

    def apply_tsne(
        self,
        n_components: int = 2,
        perplexity: int = 30,
        n_iter: int = 1000
    ) -> np.ndarray:
        """Apply t-SNE dimensionality reduction."""
        tsne = TSNE(n_components=n_components, perplexity=perplexity, n_iter=n_iter)
        return tsne.fit_transform(self._latent_matrix)

    def apply_umap(
        self,
        n_components: int = 2,
        n_neighbors: int = 15,
        min_dist: float = 0.1,
        spread: float = 1.0,
        metric: str = 'euclidean',
        init: str = 'spectral',
        random_state: int = 42,
        densmap: bool = False
    ) -> np.ndarray:
        """Apply UMAP dimensionality reduction."""
        if umap is None:
            raise RuntimeError("umap-learn is not installed")

        umap_model = umap.UMAP(
            n_components=n_components,
            n_neighbors=n_neighbors,
            min_dist=min_dist,
            spread=spread,
            metric=metric,
            init=init,
            random_state=random_state,
            densmap=densmap
        )
        return umap_model.fit_transform(self._latent_matrix)

    # ---------------------- CLUSTERING TECHNIQUES ---------------------- #

    def apply_kmeans(
        self,
        n_clusters: int = 3,
        random_state: int = 42
    ) -> np.ndarray:
        """Apply K-means clustering."""
        kmeans = KMeans(n_clusters=n_clusters, random_state=random_state)
        return kmeans.fit_predict(self._latent_matrix)

    def apply_gaussian_mixture(
        self,
        n_components: int = 3,
        covariance_type: str = 'full'
    ) -> np.ndarray:
        """Apply Gaussian Mixture Model clustering."""
        gmm = GaussianMixture(
            n_components=n_components,
            covariance_type=covariance_type
        )
        return gmm.fit_predict(self._latent_matrix)

    def apply_dbscan(
        self,
        eps: float = 0.5,
        min_samples: int = 5
    ) -> np.ndarray:
        """Apply DBSCAN clustering."""
        dbscan = DBSCAN(eps=eps, min_samples=min_samples)
        return dbscan.fit_predict(self._latent_matrix)

    def apply_hdbscan(
        self,
        min_cluster_size: int = 5,
        min_samples: Optional[int] = None,
        cluster_selection_method: str = 'eom'
    ) -> np.ndarray:
        """Apply HDBSCAN clustering."""
        if hdbscan is None:
            raise RuntimeError("hdbscan is not installed")

        hdbscan_model = hdbscan.HDBSCAN(
            min_cluster_size=min_cluster_size,
            min_samples=min_samples,
            cluster_selection_method=cluster_selection_method
        )
        return hdbscan_model.fit_predict(self._latent_matrix)

    def apply_hierarchical_clustering(
        self,
        n_clusters: int = 3,
        method: str = 'ward'
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Apply hierarchical clustering."""
        Z = linkage(self._latent_matrix, method=method)
        clusters = fcluster(Z, t=n_clusters, criterion='maxclust')
        return Z, clusters

    def apply_spectral_clustering(
        self,
        n_clusters: int = 3,
        affinity: str = 'nearest_neighbors'
    ) -> np.ndarray:
        """Apply spectral clustering."""
        spectral = SpectralClustering(n_clusters=n_clusters, affinity=affinity)
        return spectral.fit_predict(self._latent_matrix)

    def apply_agglomerative_clustering(
        self,
        n_clusters: int = 3,
        metric: str = 'euclidean',
        linkage_method: str = 'ward'
    ) -> np.ndarray:
        """Apply agglomerative clustering."""
        agglomerative = AgglomerativeClustering(
            n_clusters=n_clusters,
            metric=metric,
            linkage=linkage_method
        )
        return agglomerative.fit_predict(self._latent_matrix)

    def apply_leiden(
        self,
        resolution: float = 1.0,
        n_neighbors: int = 10
    ) -> np.ndarray:
        """Apply Leiden clustering algorithm."""
        if ig is None or leidenalg is None:
            raise RuntimeError("igraph and leidenalg are required for Leiden")

        knn_graph = kneighbors_graph(
            self._latent_matrix,
            n_neighbors=n_neighbors,
            mode='connectivity',
            include_self=True
        )
        sources, targets = knn_graph.nonzero()
        edges = list(zip(sources, targets))
        g = ig.Graph(edges=edges, directed=False)

        partition = leidenalg.find_partition(
            g, leidenalg.ModularityVertexPartition
        )
        return np.array(partition.membership)

    def apply_louvain(self, resolution: float = 1.0) -> np.ndarray:
        """Apply Louvain clustering algorithm."""
        if ig is None or community_louvain is None:
            raise RuntimeError(
                "igraph and python-louvain are required for Louvain"
            )

        knn_graph = kneighbors_graph(
            self._latent_matrix,
            n_neighbors=10,
            mode='connectivity',
            include_self=True
        )
        sources, targets = knn_graph.nonzero()
        edges = list(zip(sources, targets))
        g = ig.Graph(edges=edges, directed=False)

        partition = community_louvain.best_partition(g, resolution=resolution)
        return np.array(list(partition.values()))

    def apply_bayesian_gaussian_mixture(
        self,
        n_components: int = 3,
        covariance_type: str = 'full'
    ) -> np.ndarray:
        """Apply Bayesian Gaussian Mixture Model clustering."""
        bgm = BayesianGaussianMixture(
            n_components=n_components,
            covariance_type=covariance_type
        )
        return bgm.fit_predict(self._latent_matrix)

    def apply_mean_shift(self, bandwidth: Optional[float] = None) -> np.ndarray:
        """Apply Mean Shift clustering."""
        mean_shift = MeanShift(bandwidth=bandwidth)
        return mean_shift.fit_predict(self._latent_matrix)

    # ---------------------- VISUALIZATION TECHNIQUES ---------------------- #

    def visualize_2d(
        self,
        data: np.ndarray,
        labels: Optional[np.ndarray] = None,
        title: str = "2D Visualization",
        method: str = "PCA",
        s: float = 0.5,
        label_mapping: Optional[Dict[Any, int]] = None
    ) -> None:
        """Visualize 2D data with optional labels."""
        plt.figure(figsize=(8, 6))

        if labels is not None:
            if label_mapping is not None:
                if not np.issubdtype(labels.dtype, np.number):
                    labels = np.array([label_mapping[label] for label in labels])

            scatter = plt.scatter(
                data[:, 0], data[:, 1],
                c=labels, cmap='viridis', s=s, alpha=0.8
            )
            plt.colorbar(scatter, label="Labels")

            handles = []
            for label, idx in label_mapping.items():
                handles.append(plt.Line2D(
                    [0], [0], marker='o', color='w', label=label,
                    markerfacecolor=scatter.cmap(scatter.norm(idx)), markersize=8
                ))
            plt.legend(handles=handles, title="", loc='best')
        else:
            plt.scatter(data[:, 0], data[:, 1], s=s, alpha=0.8)

        plt.title(f"{method} Visualization (2D)" if title == "2D Visualization" else title)
        plt.xlabel(f"{method} Dimension 1")
        plt.ylabel(f"{method} Dimension 2")
        plt.grid(True)
        plt.show()

    def plot_dendrogram(
        self,
        Z: np.ndarray,
        labels: Optional[List] = None,
        title: str = "Dendrogram",
        color_threshold: Optional[float] = None
    ) -> None:
        """Plot hierarchical clustering dendrogram."""
        plt.figure(figsize=(10, 7))
        plt.title(title)
        dendrogram(Z, labels=labels, color_threshold=color_threshold)
        plt.xlabel("Sample Index")
        plt.ylabel("Distance")
        plt.grid(True)
        plt.show()

    def plot_density_grid(
        self,
        pdf: Any,
        n_cols: int = 4,
        figsize_per: Tuple[int, int] = (4, 3)
    ) -> None:
        """Plot per-dimension density estimates."""
        import math

        grids = self.density_grids
        dens = self.density_values
        D = len(grids)
        n_rows = math.ceil(D / n_cols)

        fig, axes = plt.subplots(
            n_rows, n_cols,
            figsize=(n_cols * figsize_per[0], n_rows * figsize_per[1])
        )
        axes = axes.flatten()
        for d in range(D):
            ax = axes[d]
            ax.plot(grids[d], dens[d])
            ax.set_title(f"Dim {d}", fontsize=8)
            ax.set_xlabel("Value", fontsize=6)
            ax.set_ylabel("Density", fontsize=6)
        for ax in axes[D:]:
            ax.axis('off')
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close(fig)

    def plot_silhouette_bars(
        self,
        labels: Optional[np.ndarray] = None,
        method: str = "tsne",
        figsize: Tuple[int, int] = (8, 6),
        gap: int = 10
    ) -> plt.Figure:
        """Plot silhouette analysis bars."""
        if labels is None:
            if hasattr(self, "_last_cluster_labels"):
                labels = self._last_cluster_labels
            else:
                raise ValueError("Please pass labels or run clustering first")

        coords = self.to_matrix()
        scores = silhouette_samples(coords, labels)

        fig, ax = plt.subplots(figsize=figsize)
        y_lower = gap

        for i in np.unique(labels):
            ith_scores = np.sort(scores[labels == i])
            size = ith_scores.shape[0]
            y_upper = y_lower + size
            ax.fill_betweenx(
                np.arange(y_lower, y_upper),
                0,
                ith_scores,
                alpha=0.7
            )
            ax.text(-0.02, y_lower + size / 2, str(i))
            y_lower = y_upper + gap

        ax.axvline(x=np.mean(scores), color="red", linestyle="--")
        ax.set_title(f"Silhouette Plot ({method})")
        ax.set_xlabel("Silhouette Coefficient")
        ax.set_ylabel("Cluster")
        plt.tight_layout()

        return fig

    def plot_cluster_scatter(
        self,
        labels: np.ndarray,
        coords: Optional[np.ndarray] = None,
        method: str = "tsne",
        label_mapping: Optional[Dict[Any, int]] = None,
        figsize: Tuple[int, int] = (8, 8)
    ) -> plt.Figure:
        """Plot cluster scatter visualization."""
        if coords is None:
            coords = getattr(self, f"_coords_{method}", None)
            if coords is None:
                raise ValueError(f"No stored coords for method={method}")

        if label_mapping is None:
            unique = np.unique(labels)
            label_mapping = {c: i for i, c in enumerate(unique)}
        colors = np.array([label_mapping[c] for c in labels])

        fig, ax = plt.subplots(figsize=figsize)
        scatter = ax.scatter(coords[:, 0], coords[:, 1], c=colors, s=5, cmap="tab10")
        ax.set_title(f"{method.upper()} projection (cluster-colored)")
        ax.set_xlabel(f"{method} 1")
        ax.set_ylabel(f"{method} 2")
        plt.tight_layout()

        return fig
