"""
Purpose: Dataset report component showing HSV distributions
Author: Hallett Lab
Date: 2026-02-01

Provides DatasetReportPanel for visualizing HSV distributions of the
training, validation, and test datasets after configuration.
"""

from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import numpy as np
import pandas as pd

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.interface.components.dataset_report")

# Check dependencies
try:
    import streamlit as st
    STREAMLIT_AVAILABLE = True
except ImportError:
    STREAMLIT_AVAILABLE = False

try:
    import plotly.express as px
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False


class DatasetReportPanel:
    """
    Panel for displaying dataset statistics and HSV distributions.

    Shows:
    - Sample counts per split (train/val/test)
    - HSV distribution histograms
    - Summary statistics table
    - Conditioning variable distributions
    """

    def __init__(self) -> None:
        """Initialize DatasetReportPanel."""
        if not STREAMLIT_AVAILABLE:
            raise ImportError("Streamlit is required for DatasetReportPanel")
        if not PLOTLY_AVAILABLE:
            raise ImportError("Plotly is required for DatasetReportPanel")

    def render(
        self,
        metadata_df: pd.DataFrame,
        train_indices: List[int],
        val_indices: List[int],
        test_indices: List[int],
        conditional_variables: List[str],
    ) -> None:
        """
        Render the dataset report.

        Parameters
        ----------
        metadata_df : pd.DataFrame
            Full metadata DataFrame with HSV columns (stored in 0-255 scale)
        train_indices : List[int]
            Indices of training samples
        val_indices : List[int]
            Indices of validation samples
        test_indices : List[int]
            Indices of test samples
        conditional_variables : List[str]
            List of conditioning variable names (e.g., ['average_hue'])
        """
        st.subheader("📊 Dataset Report")

        # Create split DataFrames and normalize HSV columns to 0-1 scale
        hsv_cols = ['average_hue', 'average_saturation', 'average_value']

        train_df = metadata_df.iloc[train_indices].copy()
        val_df = metadata_df.iloc[val_indices].copy()
        test_df = metadata_df.iloc[test_indices].copy()

        # Normalize HSV values from 0-255 to 0-1 scale for display
        for df in [train_df, val_df, test_df]:
            for col in hsv_cols:
                if col in df.columns:
                    # Check if values are in 0-255 range and normalize
                    if df[col].max() > 1.0:
                        df[col] = df[col] / 255.0

        # Sample counts
        self._render_sample_counts(train_df, val_df, test_df)

        st.divider()

        # HSV distributions
        self._render_hsv_distributions(train_df, val_df, test_df)

        st.divider()

        # Summary statistics
        self._render_summary_stats(train_df, val_df, test_df)

        # Conditioning variable info
        if conditional_variables:
            st.divider()
            self._render_conditioning_info(train_df, conditional_variables)

    def _render_sample_counts(
        self,
        train_df: pd.DataFrame,
        val_df: pd.DataFrame,
        test_df: pd.DataFrame,
    ) -> None:
        """Render sample count metrics."""
        col1, col2, col3, col4 = st.columns(4)

        total = len(train_df) + len(val_df) + len(test_df)

        with col1:
            st.metric("Training", f"{len(train_df):,}")
        with col2:
            st.metric("Validation", f"{len(val_df):,}")
        with col3:
            st.metric("Test", f"{len(test_df):,}")
        with col4:
            st.metric("Total", f"{total:,}")

    def _render_hsv_distributions(
        self,
        train_df: pd.DataFrame,
        val_df: pd.DataFrame,
        test_df: pd.DataFrame,
    ) -> None:
        """Render HSV distribution histograms."""
        st.markdown("### HSV Distributions (Training Set)")
        st.caption("HSV values extracted from 12-pixel image borders. All values in 0-1 scale.")

        # Check which HSV columns exist
        hsv_columns = []
        for col in ['average_hue', 'average_saturation', 'average_value']:
            if col in train_df.columns:
                hsv_columns.append(col)

        if not hsv_columns:
            st.warning("No HSV columns found in metadata. HSV values will be computed during training.")
            return

        # Create subplot figure
        fig = make_subplots(
            rows=1, cols=len(hsv_columns),
            subplot_titles=[col.replace('average_', '').title() for col in hsv_columns],
            horizontal_spacing=0.1,
        )

        colors = {
            'average_hue': '#FF6B6B',      # Red-ish for Hue
            'average_saturation': '#4ECDC4',  # Teal for Saturation
            'average_value': '#45B7D1',     # Blue for Value
        }

        for i, col in enumerate(hsv_columns, 1):
            if col in train_df.columns:
                values = train_df[col].dropna()
                if len(values) > 0:
                    fig.add_trace(
                        go.Histogram(
                            x=values,
                            name=col.replace('average_', '').title(),
                            marker_color=colors.get(col, '#888888'),
                            opacity=0.7,
                            nbinsx=50,
                        ),
                        row=1, col=i
                    )
                    # Add mean line
                    mean_val = values.mean()
                    fig.add_vline(
                        x=mean_val,
                        line_dash="dash",
                        line_color="red",
                        annotation_text=f"μ={mean_val:.3f}",
                        row=1, col=i
                    )

        fig.update_layout(
            height=300,
            showlegend=False,
            margin=dict(l=40, r=40, t=40, b=40),
        )

        # Set x-axis range to 0-1 for all
        for i in range(1, len(hsv_columns) + 1):
            fig.update_xaxes(range=[0, 1], row=1, col=i)

        st.plotly_chart(fig, use_container_width=True, key="hsv_dist_main")

        # Show comparison across splits
        if st.checkbox("Show split comparison", key="show_split_comparison"):
            self._render_split_comparison(train_df, val_df, test_df, hsv_columns)

    def _render_split_comparison(
        self,
        train_df: pd.DataFrame,
        val_df: pd.DataFrame,
        test_df: pd.DataFrame,
        hsv_columns: List[str],
    ) -> None:
        """Render comparison of HSV distributions across splits."""
        st.markdown("#### HSV Distribution Comparison (All Splits)")

        for col in hsv_columns:
            if col not in train_df.columns:
                continue

            fig = go.Figure()

            for df, name, color in [
                (train_df, 'Training', '#FF6B6B'),
                (val_df, 'Validation', '#4ECDC4'),
                (test_df, 'Test', '#45B7D1'),
            ]:
                values = df[col].dropna()
                if len(values) > 0:
                    fig.add_trace(go.Histogram(
                        x=values,
                        name=name,
                        marker_color=color,
                        opacity=0.5,
                        nbinsx=30,
                    ))

            fig.update_layout(
                title=col.replace('average_', '').title(),
                barmode='overlay',
                height=250,
                xaxis_range=[0, 1],
                margin=dict(l=40, r=40, t=40, b=40),
            )

            st.plotly_chart(fig, use_container_width=True, key=f"split_comparison_{col}")

    def _render_summary_stats(
        self,
        train_df: pd.DataFrame,
        val_df: pd.DataFrame,
        test_df: pd.DataFrame,
    ) -> None:
        """Render summary statistics table."""
        st.markdown("### Summary Statistics")

        hsv_columns = ['average_hue', 'average_saturation', 'average_value']
        available_cols = [c for c in hsv_columns if c in train_df.columns]

        if not available_cols:
            return

        # Build stats table (values already normalized to 0-1)
        stats_data = []
        for col in available_cols:
            col_name = col.replace('average_', '').title()

            for df, split_name in [(train_df, 'Train'), (val_df, 'Val'), (test_df, 'Test')]:
                values = df[col].dropna()
                if len(values) > 0:
                    stats_data.append({
                        'Variable': col_name,
                        'Split': split_name,
                        'Mean': f"{values.mean():.3f}",
                        'Std': f"{values.std():.3f}",
                        'Min': f"{values.min():.3f}",
                        'Max': f"{values.max():.3f}",
                        'Median': f"{values.median():.3f}",
                    })

        if stats_data:
            stats_df = pd.DataFrame(stats_data)
            st.dataframe(stats_df, use_container_width=True, hide_index=True)

    def _render_conditioning_info(
        self,
        train_df: pd.DataFrame,
        conditional_variables: List[str],
    ) -> None:
        """Render information about conditioning variables."""
        st.markdown("### Conditioning Variables")

        for var in conditional_variables:
            if var in train_df.columns:
                values = train_df[var].dropna()
                if len(values) > 0:
                    col1, col2 = st.columns([2, 1])

                    with col1:
                        # Small histogram
                        fig = px.histogram(
                            values,
                            nbins=30,
                            title=f"{var.replace('average_', '').title()} Distribution",
                        )
                        fig.update_layout(
                            height=200,
                            showlegend=False,
                            margin=dict(l=40, r=40, t=40, b=40),
                            xaxis_range=[0, 1],
                        )
                        st.plotly_chart(fig, use_container_width=True, key=f"cond_var_{var}")

                    with col2:
                        st.markdown("**Conditioning Info**")
                        st.markdown(f"- **Scale**: 0-1 (normalized)")
                        st.markdown(f"- **Mean**: {values.mean():.3f}")
                        st.markdown(f"- **Std**: {values.std():.3f}")

                        # Show what fixed value means
                        st.markdown("---")
                        st.markdown("**Fixed Value Decoder**")
                        st.caption(
                            "During inference, you can set "
                            f"`conditional_decoder_fixed_values={{'{var}': 0.5}}` "
                            "to reconstruct all images at a normalized hue/saturation/value."
                        )


def compute_hsv_from_images(
    image_dir: Path,
    image_files: List[str],
    border_width: int = 12,
) -> pd.DataFrame:
    """
    Compute HSV values from image borders.

    Parameters
    ----------
    image_dir : Path
        Directory containing images
    image_files : List[str]
        List of image filenames
    border_width : int
        Width of border to sample (default 12 pixels)

    Returns
    -------
    pd.DataFrame
        DataFrame with columns: filename, average_hue, average_saturation, average_value
        All values are in 0-1 normalized scale.
    """
    try:
        from PIL import Image
    except ImportError:
        logger.error("PIL is required for HSV computation")
        return pd.DataFrame()

    results = []

    for filename in image_files:
        filepath = image_dir / filename
        if not filepath.exists():
            continue

        try:
            img = Image.open(filepath).convert('RGB')
            width, height = img.size

            # Extract border pixels
            border_pixels = []

            # Top and bottom borders
            for x in range(width):
                for y in range(min(border_width, height)):
                    border_pixels.append(img.getpixel((x, y)))
                for y in range(max(0, height - border_width), height):
                    border_pixels.append(img.getpixel((x, y)))

            # Left and right borders (excluding corners already counted)
            for y in range(border_width, height - border_width):
                for x in range(min(border_width, width)):
                    border_pixels.append(img.getpixel((x, y)))
                for x in range(max(0, width - border_width), width):
                    border_pixels.append(img.getpixel((x, y)))

            if not border_pixels:
                continue

            # Convert to HSV using PIL
            hsv_img = img.convert('HSV')

            # Get HSV values for border region
            hsv_values = []
            for x in range(width):
                for y in range(min(border_width, height)):
                    hsv_values.append(hsv_img.getpixel((x, y)))
                for y in range(max(0, height - border_width), height):
                    hsv_values.append(hsv_img.getpixel((x, y)))

            for y in range(border_width, height - border_width):
                for x in range(min(border_width, width)):
                    hsv_values.append(hsv_img.getpixel((x, y)))
                for x in range(max(0, width - border_width), width):
                    hsv_values.append(hsv_img.getpixel((x, y)))

            if hsv_values:
                hsv_array = np.array(hsv_values)
                # Circular mean for hue (compute in radians, normalize to 0-1)
                hue_rad = hsv_array[:, 0] * 2 * np.pi / 255
                avg_hue = np.arctan2(np.sin(hue_rad).mean(), np.cos(hue_rad).mean())
                avg_hue = (avg_hue / (2 * np.pi)) % 1.0  # Normalize to 0-1

                results.append({
                    'filename': filename,
                    'average_hue': avg_hue,
                    'average_saturation': hsv_array[:, 1].mean() / 255.0,  # Normalize to 0-1
                    'average_value': hsv_array[:, 2].mean() / 255.0,  # Normalize to 0-1
                })

        except Exception as e:
            logger.warning(f"Error processing {filename}: {e}")
            continue

    return pd.DataFrame(results)
