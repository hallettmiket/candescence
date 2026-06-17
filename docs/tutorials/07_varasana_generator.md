# Tutorial 07 — Varasana Generator (synthetic cells)

**What you'll do:** Use the **Varasana FastGAN** to synthesise single-cell
*Candida albicans* images — sample novel cells, walk the latent space, and export
a batch as synthetic training data — from the **Varasana → Generator** page in
the app.

The generator is an **unconditional FastGAN** (Liu et al. 2021) trained on 886
single-cell crops spanning 9 morphology classes. It maps a random latent vector
(256-d) to a **256×256** cell image. It was built for **data augmentation** of
the Varasana detector.

> **Reference:** Liu et al. (2021) *ICLR*, "Towards Faster and Stabilized GAN
> Training for High-fidelity Few-shot Image Synthesis"
> ([FastGAN](https://github.com/odegeasslbc/FastGAN-pytorch)).

---

## How it runs

Unlike the Varasana/Grace **detectors** (which use the isolated legacy
MMDetection worker), the FastGAN is **modern, self-contained PyTorch** and runs
**in-process** — on GPU when available, otherwise CPU. Nothing extra to install
beyond the main environment.

The trained generator is registered in the model zoo as `varasana_fastgan`
(architecture `fastgan`); its 886 training crops are the `varasana_gan_images`
dataset.

---

## Steps

1. Launch the app: `uv run streamlit run src/candescence/interface/app.py`.
2. In the sidebar, open **Varasana → Generator**.
3. **Sample** tab — pick how many cells and a seed (a fixed non-zero seed
   reproduces the same batch for that count), then **Generate**. Use **Download
   as zip** to save the batch as PNGs for augmentation.
4. **Interpolate** tab — pick two seeds and a number of steps to see a smooth
   latent walk between two cells.

> **Note:** the GAN is *unconditional* — you cannot ask it for a specific
> morphology, and (unlike the diffusion companion) there is no "reconstruct a
> real image" mode, because the model has no encoder.

---

## Programmatic use

```python
from candescence.generative.fastgan import varasana_generator, load_generator, sample

spec = varasana_generator()                       # checkpoint path via settings
gen = load_generator(spec.checkpoint, nz=spec.nz, im_size=spec.im_size)
images = sample(gen, n=16, seed=42)               # list of PIL images (256×256)
```
