"""
Purpose: Interactive latent space visualization component
Author: Hallett Lab
Date: 2026-01-28

Provides LatentExplorer for interactive 2D visualization of VAE latent spaces
using Plotly for click handling and Streamlit for rendering.
"""

from typing import Any, Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import torch

from candescence.core.logging_config import get_logger
from candescence.interface.base import CandescenceModel

logger = get_logger("candescence.interface.components.latent_explorer")

# Optional imports
try:
    import plotly.graph_objects as go
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False
    logger.warning("Plotly not available")

try:
    from sklearn.decomposition import PCA
    from sklearn.manifold import TSNE
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False
    logger.warning("sklearn not available")

try:
    import umap
    UMAP_AVAILABLE = True
except ImportError:
    UMAP_AVAILABLE = False
    logger.debug("umap-learn not available")


class LatentExplorer:
    """
    Interactive latent space visualization component.

    Uses Plotly for rendering and supports click events for:
    - Clicking on data points to show real images
    - Clicking on empty space to decode from estimated latent position

    The component performs dimensionality reduction (UMAP, t-SNE, or PCA)
    to project high-dimensional latent vectors to 2D for visualization.

    Example:
        >>> explorer = LatentExplorer(
        ...     embeddings=latent_vectors,
        ...     metadata_df=sample_metadata,
        ...     model=tlv_wrapper
        ... )
        >>> fig = explorer.create_figure(color_by='average_hue')
    """

    SUPPORTED_METHODS = ['pca', 'tsne', 'umap']

    def __init__(
        self,
        embeddings: np.ndarray,
        metadata_df: pd.DataFrame,
        model: Optional[CandescenceModel] = None,
        reduction_method: str = 'umap',
        random_state: int = 42
    ) -> None:
        """
        Initialize the latent explorer.

        Args:
            embeddings: Latent vectors of shape (N, latent_dim)
            metadata_df: DataFrame with 'id' column and feature columns
            model: Optional CandescenceModel for decoding
            reduction_method: Dimensionality reduction method
            random_state: Random seed for reproducibility
        """
        if not PLOTLY_AVAILABLE:
            raise ImportError("Plotly is required for LatentExplorer")
        if not SKLEARN_AVAILABLE:
            raise ImportError("sklearn is required for LatentExplorer")

        self.embeddings = embeddings
        self.metadata_df = metadata_df.copy()
        self.model = model
        self.reduction_method = reduction_method.lower()
        self.random_state = random_state

        if self.reduction_method not in self.SUPPORTED_METHODS:
            raise ValueError(
                f"Unknown method '{reduction_method}'. "
                f"Supported: {self.SUPPORTED_METHODS}"
            )

        if self.reduction_method == 'umap' and not UMAP_AVAILABLE:
            logger.warning("UMAP not available, falling back to t-SNE")
            self.reduction_method = 'tsne'

        # Compute 2D projection
        self._coords_2d: Optional[np.ndarray] = None
        self._projector: Optional[Any] = None

        logger.debug(
            f"LatentExplorer initialized with {len(embeddings)} samples, "
            f"method={self.reduction_method}"
        )

    def compute_projection(self, force: bool = False) -> np.ndarray:
        """
        Compute 2D projection of embeddings.

        Args:
            force: Recompute even if already cached

        Returns:
            2D coordinates of shape (N, 2)
        """
        if self._coords_2d is not None and not force:
            return self._coords_2d

        logger.info(f"Computing {self.reduction_method.upper()} projection...")

        if self.reduction_method == 'umap':
            self._projector = umap.UMAP(
                n_components=2,
                random_state=self.random_state,
                n_neighbors=15,
                min_dist=0.1
            )
        elif self.reduction_method == 'tsne':
            self._projector = TSNE(
                n_components=2,
                random_state=self.random_state,
                perplexity=min(30, len(self.embeddings) - 1)
            )
        else:  # pca
            self._projector = PCA(n_components=2)

        self._coords_2d = self._projector.fit_transform(self.embeddings)

        # Store in metadata for convenience
        self.metadata_df['x_2d'] = self._coords_2d[:, 0]
        self.metadata_df['y_2d'] = self._coords_2d[:, 1]

        logger.info("Projection complete")
        return self._coords_2d

    def create_figure(
        self,
        color_by: Optional[str] = None,
        title: Optional[str] = None,
        point_size: int = 8,
        opacity: float = 0.8,
        show_colorbar: bool = True
    ) -> "go.Figure":
        """
        Create an interactive Plotly figure.

        Args:
            color_by: Column name to color points by
            title: Plot title (auto-generated if None)
            point_size: Marker size
            opacity: Marker opacity
            show_colorbar: Whether to show colorbar for continuous colors

        Returns:
            Plotly Figure object
        """
        # Ensure projection is computed
        coords = self.compute_projection()

        if title is None:
            title = f"Latent Space ({self.reduction_method.upper()})"

        fig = go.Figure()

        # Prepare hover text
        hover_template = "ID: %{customdata[0]}<br>x: %{x:.2f}<br>y: %{y:.2f}"
        if color_by and color_by in self.metadata_df.columns:
            hover_template += f"<br>{color_by}: %{{customdata[1]:.3f}}"

        # Prepare custom data for hover
        if color_by and color_by in self.metadata_df.columns:
            customdata = np.column_stack([
                self.metadata_df['id'].values,
                self.metadata_df[color_by].values
            ])
        else:
            customdata = self.metadata_df['id'].values.reshape(-1, 1)

        # Color configuration
        if color_by and color_by in self.metadata_df.columns:
            color_values = self.metadata_df[color_by].values
            marker_config = dict(
                size=point_size,
                color=color_values,
                colorscale='Viridis',
                showscale=show_colorbar,
                colorbar=dict(title=color_by) if show_colorbar else None,
                opacity=opacity
            )
        else:
            marker_config = dict(
                size=point_size,
                color='steelblue',
                opacity=opacity
            )

        fig.add_trace(go.Scatter(
            x=coords[:, 0],
            y=coords[:, 1],
            mode='markers',
            marker=marker_config,
            customdata=customdata,
            hovertemplate=hover_template + "<extra></extra>",
        ))

        fig.update_layout(
            title=title,
            xaxis_title='Component 1',
            yaxis_title='Component 2',
            hovermode='closest',
            clickmode='event+select',
            dragmode='zoom',
            template='plotly_white'
        )

        return fig

    def get_nearest_index(self, x: float, y: float, k: int = 1) -> List[int]:
        """
        Find indices of k nearest points to given 2D coordinates.

        Args:
            x: X coordinate in 2D space
            y: Y coordinate in 2D space
            k: Number of neighbors to return

        Returns:
            List of indices of nearest points
        """
        coords = self.compute_projection()
        point = np.array([[x, y]])

        # Compute distances
        distances = np.linalg.norm(coords - point, axis=1)

        # Return k smallest
        return np.argsort(distances)[:k].tolist()

    def decode_from_2d(
        self,
        x: float,
        y: float,
        cond: Optional[torch.Tensor] = None,
        k_neighbors: int = 5
    ) -> Optional[np.ndarray]:
        """
        Decode an image from 2D coordinates.

        Uses weighted average of k nearest neighbors in latent space
        to estimate the latent vector, then decodes.

        Args:
            x: X coordinate in 2D projection space
            y: Y coordinate in 2D projection space
            cond: Optional conditioning tensor
            k_neighbors: Number of neighbors for interpolation

        Returns:
            Decoded image as numpy array, or None if no model available
        """
        if self.model is None:
            logger.warning("No model available for decoding")
            return None

        coords = self.compute_projection()

        # Find k nearest neighbors
        indices = self.get_nearest_index(x, y, k=k_neighbors)

        # Compute weights (inverse distance)
        point = np.array([x, y])
        distances = np.linalg.norm(coords[indices] - point, axis=1)
        weights = 1.0 / (distances + 1e-6)
        weights = weights / weights.sum()

        # Weighted average of latent vectors
        z_estimated = np.sum(
            self.embeddings[indices] * weights[:, np.newaxis],
            axis=0
        )

        # Decode
        z_tensor = torch.tensor(
            z_estimated,
            dtype=torch.float32
        ).unsqueeze(0).to(self.model.device)

        if cond is not None:
            cond = cond.to(self.model.device)

        with torch.no_grad():
            # Need to encode first to set skip connections
            # Use nearest neighbor's embedding as starting point
            nearest_idx = indices[0]
            # For now, just decode without skip (may not work for all architectures)
            decoded = self.model.decode(z_tensor, cond)

        return decoded.cpu().numpy().squeeze()

    def get_embedding_at_index(self, index: int) -> np.ndarray:
        """Get latent vector at given index."""
        return self.embeddings[index]

    def get_metadata_at_index(self, index: int) -> Dict[str, Any]:
        """Get metadata at given index."""
        return self.metadata_df.iloc[index].to_dict()

    def get_2d_coords_at_index(self, index: int) -> Tuple[float, float]:
        """Get 2D projection coordinates at given index."""
        coords = self.compute_projection()
        return float(coords[index, 0]), float(coords[index, 1])

    @property
    def n_samples(self) -> int:
        """Number of samples."""
        return len(self.embeddings)

    @property
    def latent_dim(self) -> int:
        """Dimensionality of latent space."""
        return self.embeddings.shape[1]
