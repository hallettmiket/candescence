"""
Purpose: Unified theme configuration for Candescence Streamlit apps
Author: Hallett Lab
Date: 2026-02-04

Provides consistent styling across all Candescence interface pages.
"""

from pathlib import Path
from typing import Dict, Optional

import streamlit as st

# Logo paths (resolved relative to this file -> repo root / assets/)
_LOGO_PATH = Path(__file__).parent.parent.parent.parent.parent / "assets" / "candescence-logo.png"
_SIDEBAR_LOGO_PATH = Path(__file__).parent.parent.parent.parent.parent / "assets" / "candescence-logo-sidebar.png"


# Candescence UI Theme
THEME: Dict[str, str] = {
    # Primary colors
    "primary_color": "#4A90D9",
    "secondary_color": "#2ECC71",
    "background_color": "#FFFFFF",
    "text_color": "#333333",
    "font_family": "Inter, sans-serif",

    # Subproject colors
    "tlv_color": "#9B59B6",        # Purple for TLV (Tendril Latent VAE)
    "varasana_color": "#E67E22",   # Orange for Varasana (Object Detection)
    "grace_color": "#1ABC9C",      # Teal for Grace (future)

    # Status colors
    "success_color": "#27AE60",
    "warning_color": "#F39C12",
    "error_color": "#E74C3C",
    "info_color": "#3498DB",
}


# Standard component styles
CARD_STYLE = """
    background-color: #f8f9fa;
    border-radius: 8px;
    padding: 1rem;
    margin: 0.5rem 0;
    border-left: 4px solid {color};
"""

HEADER_STYLE = """
    font-family: {font_family};
    color: {text_color};
"""


def get_subproject_color(subproject: str) -> str:
    """
    Get the theme color for a specific subproject.

    Args:
        subproject: One of "tlv", "varasana", "grace"

    Returns:
        Hex color string for the subproject
    """
    color_map = {
        "tlv": THEME["tlv_color"],
        "varasana": THEME["varasana_color"],
        "grace": THEME["grace_color"],
    }
    return color_map.get(subproject.lower(), THEME["primary_color"])


def apply_theme() -> None:
    """
    Apply consistent theming to the current Streamlit page.

    Call this at the start of each page to ensure consistent styling.
    """
    # Sidebar logo at the top — use the cropped version (no rotated text)
    # with a proper horizontal title underneath
    with st.sidebar:
        logo_path = _SIDEBAR_LOGO_PATH if _SIDEBAR_LOGO_PATH.exists() else _LOGO_PATH
        if logo_path.exists():
            st.image(str(logo_path), width="stretch")
            st.markdown(
                '<p style="text-align: center; font-size: 1.3rem; font-weight: 700; '
                'letter-spacing: 0.15em; margin: -0.5rem 0 0.5rem 0; '
                'color: #FFFFFF !important;">CANDESCENCE</p>',
                unsafe_allow_html=True,
            )

    st.markdown(
        f"""
        <style>
        /* Hide the default "app" entry at the top of sidebar nav */
        section[data-testid="stSidebar"] [data-testid="stSidebarNav"] > ul > li:first-child {{
            display: none;
        }}

        /* Bump base font size for main content area */
        .main .block-container {{
            font-size: 1.1rem;
        }}

        .main .block-container p,
        .main .block-container li,
        .main .block-container span {{
            font-size: 1.1rem;
        }}

        /* Primary button styling */
        .stButton > button[kind="primary"] {{
            background-color: {THEME["primary_color"]};
            border-color: {THEME["primary_color"]};
        }}

        /* Card styling for metrics */
        div[data-testid="metric-container"] {{
            background-color: #f8f9fa;
            border-radius: 8px;
            padding: 0.5rem;
        }}

        /* Sidebar styling */
        section[data-testid="stSidebar"] {{
            background-color: #1E2A3A;
        }}

        /* Sidebar text color */
        section[data-testid="stSidebar"] * {{
            color: #D0D8E0;
        }}

        /* Sidebar navigation links */
        section[data-testid="stSidebar"] a {{
            color: #B0BEC5 !important;
        }}

        section[data-testid="stSidebar"] a:hover {{
            color: #FFFFFF !important;
        }}

        /* Active/current page link */
        section[data-testid="stSidebar"] a[aria-selected="true"],
        section[data-testid="stSidebar"] .st-emotion-cache-ue6h4q {{
            color: #FFFFFF !important;
            font-weight: 600;
        }}

        /* Sidebar header text */
        section[data-testid="stSidebar"] h1,
        section[data-testid="stSidebar"] h2,
        section[data-testid="stSidebar"] h3,
        section[data-testid="stSidebar"] .stMarkdown h1,
        section[data-testid="stSidebar"] .stMarkdown h2,
        section[data-testid="stSidebar"] .stMarkdown h3 {{
            color: #FFFFFF !important;
        }}

        /* Sidebar selectbox and widget labels */
        section[data-testid="stSidebar"] label {{
            color: #D0D8E0 !important;
        }}

        /* Sidebar dividers */
        section[data-testid="stSidebar"] hr {{
            border-color: #3A4A5C;
        }}

        /* Sidebar captions */
        section[data-testid="stSidebar"] .stCaption {{
            color: #8899AA !important;
        }}

        /* Tab styling */
        .stTabs [data-baseweb="tab-list"] {{
            gap: 8px;
        }}

        .stTabs [data-baseweb="tab"] {{
            padding: 8px 16px;
            border-radius: 4px;
        }}
        </style>
        """,
        unsafe_allow_html=True,
    )


def page_header(
    title: str,
    subproject: Optional[str] = None,
    icon: str = "",
    description: Optional[str] = None,
) -> None:
    """
    Render a standardized page header with optional subproject branding.

    Args:
        title: Page title
        subproject: Subproject name for color coding (None for project-wide pages)
        icon: Optional emoji icon
        description: Optional page description
    """
    color = get_subproject_color(subproject) if subproject else THEME["primary_color"]

    # Subproject label only when a specific subproject is given
    label_html = ""
    if subproject:
        label_html = (
            f'<span style="color: {color}; font-weight: 500; '
            f'text-transform: uppercase; font-size: 0.8rem;">'
            f'{subproject.upper()}</span>'
        )

    # Header with colored accent
    st.markdown(
        f"""
        <div style="border-left: 4px solid {color}; padding-left: 1rem; margin-bottom: 1rem;">
            <h1 style="margin: 0; color: {THEME['text_color']};">{icon} {title}</h1>
            {label_html}
        </div>
        """,
        unsafe_allow_html=True,
    )

    if description:
        st.caption(description)


def status_badge(status: str) -> str:
    """
    Get HTML for a status badge.

    Args:
        status: One of "idle", "training", "paused", "completed", "error"

    Returns:
        HTML string for the badge
    """
    colors = {
        "idle": THEME["info_color"],
        "training": THEME["warning_color"],
        "paused": THEME["warning_color"],
        "completed": THEME["success_color"],
        "error": THEME["error_color"],
        "research": THEME["info_color"],
        "production": THEME["success_color"],
    }

    color = colors.get(status.lower(), THEME["text_color"])

    return f"""
        <span style="
            background-color: {color}20;
            color: {color};
            padding: 2px 8px;
            border-radius: 4px;
            font-size: 0.8rem;
            font-weight: 500;
        ">{status.upper()}</span>
    """
