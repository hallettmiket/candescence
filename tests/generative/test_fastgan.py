"""
Purpose: Tests for the Varasana FastGAN engine — architecture build, spec path
         resolution, and a weights-guarded load + sample/interpolate round trip.
Author: Hallett Lab
Date: 2026-06-16
Input: The vendored model + (when present) the published checkpoint.
Output: Pytest assertions.
"""

import numpy as np
import pytest
import torch

from candescence.generative.fastgan import (
    GAN_ARCHITECTURE,
    interpolate,
    load_generator,
    sample,
    varasana_generator,
)
from candescence.generative.fastgan.models import Generator


def test_generator_builds_and_forwards_256():
    """The 256px generator returns [256x256, 128x128] outputs from a latent."""
    gen = Generator(ngf=64, nz=256, nc=3, im_size=256).eval()
    with torch.no_grad():
        out = gen(torch.randn(2, 256))
    assert isinstance(out, list) and len(out) == 2
    assert out[0].shape == (2, 3, 256, 256)
    assert out[1].shape == (2, 3, 128, 128)


def test_spec_resolves_paths():
    spec = varasana_generator()
    assert spec.key == "varasana_fastgan"
    assert spec.project == "varasana"
    assert spec.architecture == GAN_ARCHITECTURE
    assert spec.im_size == 256 and spec.nz == 256
    assert spec.checkpoint.name == "model.pth"


@pytest.mark.skipif(not varasana_generator().weights_available(),
                    reason="published Varasana GAN checkpoint not present")
def test_load_sample_interpolate_round_trip():
    spec = varasana_generator()
    loaded = load_generator(spec.checkpoint, nz=spec.nz, im_size=spec.im_size,
                            device="cpu")
    imgs = sample(loaded, 3, seed=123, batch_size=2)
    assert len(imgs) == 3
    assert imgs[0].size == (256, 256) and imgs[0].mode == "RGB"

    # Same seed + same (n, batch_size) reproduces the batch (latent + injected
    # noise are both seeded). Different seed gives a different image.
    again = sample(loaded, 3, seed=123, batch_size=2)
    assert (np.asarray(imgs[0]) == np.asarray(again[0])).all()
    other = sample(loaded, 3, seed=124, batch_size=2)
    assert (np.asarray(imgs[0]) != np.asarray(other[0])).any()

    walk = interpolate(loaded, seed_a=1, seed_b=2, steps=4)
    assert len(walk) == 4 and all(im.size == (256, 256) for im in walk)
