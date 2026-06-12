"""
TLV conditional diffusion model (ported from Jose's 38-diffusion / BW-1.0.0).

A pure-PyTorch conditional DDPM/DDIM for *Candida albicans* colony images: a
``SemanticEncoder`` maps an image to a low-dim semantic code, and a
``DiffusionUNet`` denoises 128x128 images conditioned on that code via FiLM-style
``AdaGroupNorm``. See ``docs/phase5_diffusion_blueprint.md``.

Public API:
    load_diffusion_model(path, device) -> LoadedDiffusionModel
    encode / reconstruct / generate / interpolate  (in .inference)
"""

from .loader import LoadedDiffusionModel, load_diffusion_model
from .model import TLVCondDiffVAE

__all__ = [
    "LoadedDiffusionModel",
    "load_diffusion_model",
    "TLVCondDiffVAE",
]
