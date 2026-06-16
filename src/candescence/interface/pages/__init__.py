"""
Purpose: Streamlit multi-page navigation for Candescence interface
Author: Hallett Lab
Date: 2026-02-04

This module provides page components for the unified Candescence interface.

Pages follow Streamlit's multi-page app convention:
- Files prefixed with numbers for ordering, grouped by project: the tens digit is
  the group (1x TLV, 2x Varasana, 3x Grace, 4x shared/cross-cutting), the units
  digit orders within the group.
- Automatically discovered by Streamlit when using pages/ folder

Page Structure:
- app.py (main entry) / 00_Home.py: Home dashboard (via candescence.interface.home)
- 10_TLV_Training.py / 11_TLV_Explorer.py / 12_TLV_Diffusion.py: TLV (VAE) pages
- 20_Varasana_Detection.py / 21_Varasana_Training.py: Varasana (detection) pages
- 30_Grace_Detection.py: Grace (detection) page
- 40_Train_Detector.py: train a modern (torchvision) detector
- 41_Model_Registry.py / 42_Dataset_Manager.py / 43_Model_Quality.py: shared tools
"""

from candescence.interface.home import render_home_page
from candescence.interface.tlv_explorer import TLVExplorerPage

__all__ = [
    "render_home_page",
    "TLVExplorerPage",
]
