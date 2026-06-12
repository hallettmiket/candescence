"""
Purpose: TLV (Tendril Latent VAE) Training Page
Author: Hallett Lab
Date: 2026-02-04

Streamlit page for training Tendril VAE models for latent space analysis.
This page wraps the existing training_app functionality.

Launch via unified interface:
    streamlit run src/candescence/interface/app.py
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="TLV Training | Candescence",
    page_icon="🧬",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Import and run the training app main function
# We import here to avoid circular imports and ensure page config is set first
from candescence.interface.apps.training_app import main as training_main

if __name__ == "__main__":
    training_main()
else:
    # When loaded as a page, run directly
    training_main()
