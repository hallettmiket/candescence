"""
Purpose: TLV (Tendril Latent VAE) Explorer Page
Author: Hallett Lab
Date: 2026-02-04

Streamlit page for exploring trained TLV latent spaces interactively.
This page wraps the existing latent_explorer_app functionality.

Launch via unified interface:
    streamlit run src/candescence/interface/app.py
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="TLV Explorer | Candescence",
    page_icon="🔍",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Import and run the explorer app main function
from candescence.interface.apps.latent_explorer_app import main as explorer_main

if __name__ == "__main__":
    explorer_main()
else:
    # When loaded as a page, run directly
    explorer_main()
