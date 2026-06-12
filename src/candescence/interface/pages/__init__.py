"""
Purpose: Streamlit multi-page navigation for Candescence interface
Author: Hallett Lab
Date: 2026-02-04

This module provides page components for the unified Candescence interface.

Pages follow Streamlit's multi-page app convention:
- Files prefixed with numbers (0_, 1_, etc.) for ordering
- Automatically discovered by Streamlit when using pages/ folder

Page Structure:
- app.py (main entry): Home dashboard (via candescence.interface.home)
- 1_TLV_Training.py: TLV VAE training interface
- 2_TLV_Explorer.py: TLV latent space explorer
- 3_Varasana_Training.py: FCOS object detection training
- 4_Model_Registry.py: View all registered models
- 5_Dataset_Manager.py: View all registered datasets
"""

from candescence.interface.home import render_home_page
from candescence.interface.tlv_explorer import TLVExplorerPage

__all__ = [
    "render_home_page",
    "TLVExplorerPage",
]
