"""
Purpose: Latent space interpolation tool for the interface
Author: Hallett Lab
Date: 2026-01-28

Provides InterpolationTool for interpolating between points in latent space
and visualizing the results.
"""

from typing import List, Optional, Tuple

import numpy as np
import torch

from candescence.core.logging_config import get_logger
from candescence.interface.base import CandescenceModel
from candescence.interface.components.image_panel import ImagePanel

logger = get_logger("candescence.interface.components.interpolation")

# Optional imports
try:
    import streamlit as st
    STREAMLIT_AVAILABLE = True
except ImportError:
    STREAMLIT_AVAILABLE = False


class InterpolationTool:
    """
    Tool for interpolating between points in latent space.

    Supports linear interpolation between two latent vectors
    and displays the results as a filmstrip or slider.

    Example:
        >>> tool = InterpolationTool(model, embeddings)
        >>> images = tool.interpolate(0, 100, n_steps=7)
        >>> tool.display_filmstrip(images)
    """

    def __init__(
        self,
        model: CandescenceModel,
        embeddings: np.ndarray,
        conditioning: Optional[np.ndarray] = None,
        images: Optional[np.ndarray] = None
    ) -> None:
        """
        Initialize the interpolation tool.

        Args:
            model: CandescenceModel for decoding
            embeddings: Array of latent vectors (N, latent_dim)
            conditioning: Optional array of conditioning values (N, cond_dim)
            images: Optional array of original images (N, C, H, W) for skip connections
        """
        self.model = model
        self.embeddings = embeddings
        self.conditioning = conditioning
        self.images = images

        self._point_a: Optional[int] = None
        self._point_b: Optional[int] = None
        self._image_panel = ImagePanel()

    def set_points(self, index_a: int, index_b: int) -> None:
        """
        Set the two points for interpolation.

        Args:
            index_a: Index of first point
            index_b: Index of second point
        """
        if index_a < 0 or index_a >= len(self.embeddings):
            raise ValueError(f"index_a out of range: {index_a}")
        if index_b < 0 or index_b >= len(self.embeddings):
            raise ValueError(f"index_b out of range: {index_b}")

        self._point_a = index_a
        self._point_b = index_b

    def interpolate(
        self,
        index_a: Optional[int] = None,
        index_b: Optional[int] = None,
        n_steps: int = 7,
        conditioning_strategy: str = 'interpolate'
    ) -> List[np.ndarray]:
        """
        Generate interpolation between two points.

        Args:
            index_a: Index of first point (uses stored if None)
            index_b: Index of second point (uses stored if None)
            n_steps: Number of interpolation steps
            conditioning_strategy: How to handle conditioning:
                - 'interpolate': Linear interpolation of conditioning
                - 'first': Use conditioning from first point
                - 'second': Use conditioning from second point
                - 'none': No conditioning

        Returns:
            List of decoded images as numpy arrays
        """
        if index_a is None:
            index_a = self._point_a
        if index_b is None:
            index_b = self._point_b

        if index_a is None or index_b is None:
            raise ValueError("Must set or provide both point indices")

        z_a = self.embeddings[index_a]
        z_b = self.embeddings[index_b]

        # Handle conditioning
        cond_a = None
        cond_b = None
        if self.conditioning is not None:
            cond_a = self.conditioning[index_a]
            cond_b = self.conditioning[index_b]

        alphas = np.linspace(0, 1, n_steps)

        # Build all interpolated latents and conditions at once
        z_interp_list = []
        cond_list = []

        for alpha in alphas:
            z_interp = (1 - alpha) * z_a + alpha * z_b
            z_interp_list.append(z_interp)

            if self.conditioning is not None:
                if conditioning_strategy == 'interpolate':
                    cond = (1 - alpha) * cond_a + alpha * cond_b
                elif conditioning_strategy == 'first':
                    cond = cond_a.copy()
                elif conditioning_strategy == 'second':
                    cond = cond_b.copy()
                else:
                    cond = None
                cond_list.append(cond)

        # Stack into batched tensors
        z_batch = torch.tensor(
            np.stack(z_interp_list), dtype=torch.float32
        ).to(self.model.device)

        if self.conditioning is not None and cond_list[0] is not None:
            cond_batch = torch.tensor(
                np.stack(cond_list), dtype=torch.float32
            ).to(self.model.device)
        else:
            cond_batch = None

        # Re-encode reference image to set skip connections with correct batch size
        # Use image from point_a, replicated to match batch size
        if self.images is not None:
            ref_img = torch.tensor(
                self.images[index_a], dtype=torch.float32
            ).unsqueeze(0).to(self.model.device)
            # Replicate to match batch size
            ref_img_batch = ref_img.repeat(n_steps, 1, 1, 1)

            if self.conditioning is not None:
                ref_cond = torch.tensor(
                    self.conditioning[index_a], dtype=torch.float32
                ).unsqueeze(0).to(self.model.device)
                ref_cond_batch = ref_cond.repeat(n_steps, 1)
            else:
                ref_cond_batch = None

            # Encode to set skip connections
            with torch.no_grad():
                _ = self.model.encode(ref_img_batch, ref_cond_batch)

        # Decode all at once
        with torch.no_grad():
            decoded_batch = self.model.decode(z_batch, cond_batch)

        # Split back into list
        interpolated_images = [
            decoded_batch[i].cpu().numpy()
            for i in range(n_steps)
        ]

        return interpolated_images

    def render_streamlit(self) -> Optional[List[np.ndarray]]:
        """
        Render interpolation controls in Streamlit.

        Returns:
            List of interpolated images if generated, None otherwise
        """
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit required")

        st.subheader("Interpolation")

        col1, col2 = st.columns(2)

        with col1:
            point_a = st.number_input(
                "Point A (index)",
                min_value=0,
                max_value=len(self.embeddings) - 1,
                value=0,
                key="interp_point_a"
            )

        with col2:
            point_b = st.number_input(
                "Point B (index)",
                min_value=0,
                max_value=len(self.embeddings) - 1,
                value=min(1, len(self.embeddings) - 1),
                key="interp_point_b"
            )

        n_steps = st.slider(
            "Number of steps",
            min_value=3,
            max_value=15,
            value=7,
            key="interp_n_steps"
        )

        cond_strategy = 'none'
        if self.conditioning is not None:
            cond_strategy = st.selectbox(
                "Conditioning strategy",
                ['interpolate', 'first', 'second'],
                key="interp_cond_strategy"
            )

        if st.button("Generate Interpolation", key="interp_generate"):
            with st.spinner("Generating..."):
                self.set_points(int(point_a), int(point_b))
                images = self.interpolate(
                    n_steps=n_steps,
                    conditioning_strategy=cond_strategy
                )
                return images

        return None

    def display_filmstrip(
        self,
        images: List[np.ndarray],
        show_alpha: bool = True
    ) -> None:
        """
        Display interpolation as a filmstrip.

        Args:
            images: List of images to display
            show_alpha: Whether to show interpolation alpha values
        """
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit required")

        n_images = len(images)
        cols = st.columns(n_images)

        for i, (col, img) in enumerate(zip(cols, images)):
            with col:
                display_img = self._image_panel.prepare_image(img)
                alpha = i / (n_images - 1) if n_images > 1 else 0

                if show_alpha:
                    st.image(display_img, caption=f"α={alpha:.2f}")
                else:
                    st.image(display_img)

    def display_slider(
        self,
        images: List[np.ndarray]
    ) -> None:
        """
        Display interpolation with a slider control.

        Args:
            images: List of images to display
        """
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit required")

        n_images = len(images)

        step_idx = st.slider(
            "Interpolation step",
            min_value=0,
            max_value=n_images - 1,
            value=0,
            key="interp_slider"
        )

        alpha = step_idx / (n_images - 1) if n_images > 1 else 0
        st.markdown(f"**α = {alpha:.3f}**")

        display_img = self._image_panel.prepare_image(images[step_idx])
        st.image(display_img, use_container_width=True)

    def get_endpoint_info(self) -> Tuple[Optional[int], Optional[int]]:
        """Get current interpolation endpoints."""
        return self._point_a, self._point_b
