"""
Purpose: Home dashboard page for Candescence unified interface
Author: Hallett Lab
Date: 2026-02-04

Sidebar navigation entry for the home dashboard.
Content is shared with app.py via candescence.interface.home module.
"""

from pathlib import Path

import streamlit as st

# Page configuration
LOGO_PATH = Path(__file__).parent.parent.parent.parent.parent / "assets" / "candescence-logo.png"

st.set_page_config(
    page_title="Candescence",
    page_icon=str(LOGO_PATH) if LOGO_PATH.exists() else "🧬",
    layout="wide",
    initial_sidebar_state="expanded",
)

from candescence.interface.home import render_home_page  # noqa: E402

render_home_page()
