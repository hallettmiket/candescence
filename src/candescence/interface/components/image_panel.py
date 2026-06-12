"""
Purpose: Image display panel component for the interface
Author: Hallett Lab
Date: 2026-01-28

Provides ImagePanel for displaying images with metadata in the interface.
"""

from pathlib import Path
from typing import Any, Dict, Optional, Union

import numpy as np

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.interface.components.image_panel")

# Optional imports
try:
    from PIL import Image
    PIL_AVAILABLE = True
except ImportError:
    PIL_AVAILABLE = False

try:
    import streamlit as st
    STREAMLIT_AVAILABLE = True
except ImportError:
    STREAMLIT_AVAILABLE = False


class ImagePanel:
    """
    Component for displaying images with metadata.

    Handles various image formats (numpy arrays, PIL Images, file paths)
    and provides metadata display and export functionality.

    Example:
        >>> panel = ImagePanel()
        >>> panel.render_streamlit(image, metadata, title="Selected")
    """

    def __init__(self, default_size: tuple = (256, 256)) -> None:
        """
        Initialize the image panel.

        Args:
            default_size: Default display size (width, height)
        """
        self.default_size = default_size

    def prepare_image(
        self,
        image: Union[np.ndarray, "Image.Image", str, Path]
    ) -> np.ndarray:
        """
        Convert image to displayable numpy array.

        Args:
            image: Input image in various formats

        Returns:
            RGB numpy array suitable for display
        """
        # Load from path if needed
        if isinstance(image, (str, Path)):
            if PIL_AVAILABLE:
                image = Image.open(image)
                image = np.array(image)
            else:
                raise ImportError("PIL required for loading images from paths")

        # Convert PIL Image to numpy
        if PIL_AVAILABLE and isinstance(image, Image.Image):
            image = np.array(image)

        # Handle tensor output format (B, C, H, W) or (C, H, W)
        if len(image.shape) == 4:
            image = image[0]  # Remove batch dimension

        if len(image.shape) == 3 and image.shape[0] in [1, 3]:
            # Channel-first format, convert to channel-last
            image = np.transpose(image, (1, 2, 0))

        # Handle single-channel
        if len(image.shape) == 2:
            image = np.stack([image] * 3, axis=-1)
        elif image.shape[-1] == 1:
            image = np.repeat(image, 3, axis=-1)

        # Normalize to 0-255 if needed
        if image.dtype == np.float32 or image.dtype == np.float64:
            if image.max() <= 1.0:
                image = (image * 255).astype(np.uint8)
            else:
                image = image.astype(np.uint8)
        elif image.dtype != np.uint8:
            image = image.astype(np.uint8)

        return image

    def render_streamlit(
        self,
        image: Optional[Union[np.ndarray, "Image.Image", str, Path]] = None,
        metadata: Optional[Dict[str, Any]] = None,
        title: str = "Selected Image",
        show_metadata: bool = True,
        show_export: bool = True,
        use_container_width: bool = True
    ) -> None:
        """
        Render image panel in Streamlit.

        Args:
            image: Image to display
            metadata: Optional metadata dictionary
            title: Panel title
            show_metadata: Whether to show metadata
            show_export: Whether to show export button
            use_container_width: Use full container width
        """
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit required for render_streamlit")

        st.subheader(title)

        if image is None:
            st.info("Click on a point in the latent space to view an image")
            return

        # Prepare image
        display_image = self.prepare_image(image)

        if show_metadata and metadata:
            col1, col2 = st.columns([2, 1])

            with col1:
                st.image(display_image, use_container_width=use_container_width)

            with col2:
                st.markdown("**Metadata**")
                for key, value in metadata.items():
                    if isinstance(value, float):
                        st.text(f"{key}: {value:.4f}")
                    elif isinstance(value, (np.ndarray, list)):
                        st.text(f"{key}: [array]")
                    else:
                        st.text(f"{key}: {value}")

                if show_export:
                    self._render_export_button(display_image)
        else:
            st.image(display_image, use_container_width=use_container_width)

            if show_export:
                self._render_export_button(display_image)

    def _render_export_button(self, image: np.ndarray) -> None:
        """Render export/download button."""
        import io

        if not PIL_AVAILABLE:
            st.warning("PIL required for export")
            return

        buf = io.BytesIO()
        pil_image = Image.fromarray(image)
        pil_image.save(buf, format='PNG')

        st.download_button(
            label="Download PNG",
            data=buf.getvalue(),
            file_name="exported_image.png",
            mime="image/png"
        )

    def compare_images(
        self,
        original: Union[np.ndarray, "Image.Image"],
        reconstructed: Union[np.ndarray, "Image.Image"],
        title: str = "Original vs Reconstructed"
    ) -> None:
        """
        Display side-by-side comparison of original and reconstructed images.

        Args:
            original: Original image
            reconstructed: Reconstructed image
            title: Section title
        """
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit required")

        st.subheader(title)

        orig_display = self.prepare_image(original)
        recon_display = self.prepare_image(reconstructed)

        col1, col2 = st.columns(2)

        with col1:
            st.image(orig_display, caption="Original")

        with col2:
            st.image(recon_display, caption="Reconstructed")
