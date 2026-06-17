"""
Purpose: Main Streamlit application for the Candescence unified interface.
Author: Hallett Lab
Date: 2026-01-28

Entry point + navigation router. Pages are grouped into sidebar sections by
project (TLV / Varasana / Grace) with shared cross-cutting tools set apart, via
``st.navigation``. The individual page scripts live in ``pages/`` and no longer
call ``st.set_page_config`` themselves — it is set once here.

Launch with: streamlit run src/candescence/interface/app.py
"""

from pathlib import Path

import streamlit as st

# Page configuration must be the first Streamlit command.
LOGO_PATH = Path(__file__).parent.parent.parent.parent / "assets" / "candescence-logo.png"

st.set_page_config(
    page_title="Candescence",
    page_icon=str(LOGO_PATH) if LOGO_PATH.exists() else "🧬",
    layout="wide",
    initial_sidebar_state="expanded",
)

# Sidebar navigation, grouped into sections. Section titles (the dict keys)
# render as headers in the sidebar, visually grouping each project's pages and
# setting the shared tools apart. Per-page titles are short because the section
# header already supplies the project context.
navigation = st.navigation(
    {
        "Home": [
            st.Page("pages/00_Home.py", title="Home", icon="🏠", default=True),
        ],
        "TLV — morphology VAEs": [
            st.Page("pages/10_TLV_Training.py", title="Training", icon="🧬"),
            st.Page("pages/11_TLV_Explorer.py", title="Explorer", icon="🔍"),
            st.Page("pages/12_TLV_Diffusion.py", title="Diffusion", icon="✨"),
        ],
        "Varasana — cell detection": [
            st.Page("pages/20_Varasana_Detection.py", title="Detection", icon="🔬"),
            st.Page("pages/21_Varasana_Training.py", title="Training", icon="🎯"),
        ],
        "Morphology Map (GAN)": [
            st.Page("pages/22_Varasana_Generator.py", title="Generator", icon="🧫"),
        ],
        "Grace — cell detection": [
            st.Page("pages/30_Grace_Detection.py", title="Detection", icon="🔬"),
        ],
        "Shared tools": [
            st.Page("pages/40_Train_Detector.py", title="Train New Detector", icon="🧪"),
            st.Page("pages/41_Model_Registry.py", title="Model Registry", icon="📦"),
            st.Page("pages/42_Dataset_Manager.py", title="Dataset Manager", icon="📁"),
            st.Page("pages/43_Model_Quality.py", title="Model Quality", icon="📊"),
        ],
    }
)
navigation.run()
