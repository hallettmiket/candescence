"""Varasana FastGAN: synthesise single-cell *Candida albicans* morphology images.

Modern, in-process PyTorch (no legacy worker). ``models`` holds the vendored
FastGAN architecture; ``inference`` loads a trained generator and samples /
interpolates; ``specs`` registers the published Varasana generator and resolves
its checkpoint through the settings layer.
"""

from candescence.generative.fastgan.inference import (
    LoadedGenerator,
    interpolate,
    load_generator,
    sample,
)
from candescence.generative.fastgan.specs import (
    GAN_ARCHITECTURE,
    GeneratorSpec,
    all_generators,
    get_generator,
    varasana_gan_training_images,
    varasana_generator,
)

__all__ = [
    "GAN_ARCHITECTURE",
    "GeneratorSpec",
    "LoadedGenerator",
    "all_generators",
    "get_generator",
    "interpolate",
    "load_generator",
    "sample",
    "varasana_gan_training_images",
    "varasana_generator",
]
