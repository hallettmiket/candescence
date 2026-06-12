"""
Purpose: Main Streamlit application for Candescence unified interface
Author: Hallett Lab
Date: 2026-01-28

Entry point for the Candescence multi-page interface.
The home dashboard is rendered here as the landing page.

Launch with: streamlit run src/candescence/interface/app.py
"""

from pathlib import Path

import streamlit as st

# Page configuration must be first Streamlit command
LOGO_PATH = Path(__file__).parent.parent.parent.parent / "assets" / "candescence-logo.png"

st.set_page_config(
    page_title="Candescence",
    page_icon=str(LOGO_PATH) if LOGO_PATH.exists() else "🧬",
    layout="wide",
    initial_sidebar_state="expanded",
)

# Import and render the home dashboard
from candescence.interface.home import render_home_page  # noqa: E402

render_home_page()
