"""
VAE Architecture variants for TLV.

This module provides 14 VAE architecture variants for morphology analysis.
All architectures share the FiLM conditioning layer from film.py.
"""

from .film import FiLM

# Architecture imports - explicit instead of wildcard
from .c_vae import c_VAE
from .uc_vae import uc_VAE
from .cond_uc_vae import cond_uc_VAE, Encoder as CondEncoder, Decoder as CondDecoder
from .cond_uc_vae_attention import cond_uc_VAE_attention
from .cond_uc_vae_spatial import cond_uc_VAE_spatial
from .rc_vae import rc_vae
from .uc_vae_multistage import uc_VAE_multistage
from .cruc_vae import cruc_VAE
from .cruc_vae_conv import cruc_VAE_conv
from .crutch_vae import crutch_VAE
from .tendril_vae import tendril_VAE
from .tendril_vae_multi_cond import multi_cond_tendril_VAE
from .tendril_sub import Tendrils, TendrilLayerVAE, TendrilTrainer
from .regressor import PropertyRegressor

__all__ = [
    # Shared components
    "FiLM",

    # VAE architectures
    "c_VAE",
    "uc_VAE",
    "cond_uc_VAE",
    "CondEncoder",
    "CondDecoder",
    "cond_uc_VAE_attention",
    "cond_uc_VAE_spatial",
    "rc_vae",
    "uc_VAE_multistage",
    "cruc_VAE",
    "cruc_VAE_conv",
    "crutch_VAE",
    "tendril_VAE",
    "multi_cond_tendril_VAE",

    # Tendril-specific
    "Tendrils",
    "TendrilLayerVAE",
    "TendrilTrainer",

    # Regressor
    "PropertyRegressor",
]

# Architecture name to class mapping (used by Factory)
ARCHITECTURE_MAP = {
    'c_vae': c_VAE,
    'uc_vae': uc_VAE,
    'cond_uc_vae': cond_uc_VAE,
    'cond_uc_vae_attention': cond_uc_VAE_attention,
    'cond_uc_vae_spatial': cond_uc_VAE_spatial,
    'rc_vae': rc_vae,
    'uc_vae_multistage': uc_VAE_multistage,
    'cruc_vae': cruc_VAE,
    'cruc_vae_conv': cruc_VAE_conv,
    'crutch_vae': crutch_VAE,
    'tendril_vae': tendril_VAE,
    'multi_cond_tendril_vae': multi_cond_tendril_VAE,
}
