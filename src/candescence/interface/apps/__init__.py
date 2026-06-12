"""
Purpose: Standalone Streamlit applications for Candescence
Author: Hallett Lab
Date: 2026-01-29

This package contains standalone Streamlit applications for interactive
exploration of Candescence models and data.

Applications:
- latent_explorer_app: Interactive latent space exploration
- training_app: Guided VAE training interface
"""

from candescence.interface.apps.latent_explorer_app import main as latent_explorer_main
from candescence.interface.apps.training_app import main as training_main

__all__ = ['latent_explorer_main', 'training_main']
