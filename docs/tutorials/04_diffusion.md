# Tutorial 04 — The Diffusion Companion

**What you'll do:** Use the conditional diffusion model — the generative companion to
the TLV VAEs — to **generate** novel *Candida albicans* colonies, **reconstruct** real
ones, **interpolate** between two colonies, and read off their **semantic codes**, both
interactively in the app and programmatically.

This tutorial is the diffusion counterpart to
[Tutorial 03 — The Models](03_the_models.md). Read that first if you want the VAE
context the diffusion model builds on.

---

## What the model is

The diffusion model (ported from Jose's *38-diffusion* / BW-1.0.0) is a pure-PyTorch
**conditional DDPM/DDIM**, the thesis's "diffusion-based generative companion to the
VAE." It has two parts:

- a **`SemanticEncoder`** that maps a 128×128 colony image to a low-dimensional
  *semantic code* (its posterior mean), and
- a **`DiffusionUNet`** that denoises images conditioned on that code via FiLM-style
  `AdaGroupNorm`.

Generation is fast (DDIM with a handful of steps); reconstruction runs the full reverse
diffusion chain and really wants a GPU.

> **Status:** the diffusion model is currently **inference-only** in the app and lives
> behind the sidebar **Research mode** toggle. It joins the public model tier in a later
> phase (see [`docs/phase5_diffusion_blueprint.md`](../phase5_diffusion_blueprint.md)).

---

## Prerequisites

- The project environment (`uv sync`).
- A **diffusion checkpoint** — a model registered in the zoo with architecture
  `diffusion_vae`, or a checkpoint file on disk. Confirm what's available with:

  ```bash
  uv run python -c "from candescence.core.model_zoo import ModelZoo; \
  print([m.id for m in ModelZoo().list_models(project='tlv') if m.architecture=='diffusion_vae'])"
  ```

  If that list is empty, point `CANDESCENCE_ZOO` at a populated zoo (see
  [Tutorial 02](02_using_your_own_images.md)) or load a checkpoint directly by path.
- A GPU is strongly recommended for reconstruction and interpolation.

---

## Path A — in the app (TLV Diffusion page)

1. Launch the app:

   ```bash
   nice -n 19 uv run streamlit run src/candescence/interface/app.py
   ```

2. Open the **TLV Diffusion** page from the sidebar. (If you don't see it, enable
   **Research mode** in the sidebar.)
3. Pick a checkpoint from the **Diffusion model** dropdown, or choose **Custom path…**
   and paste a `.pt` path. Select `cuda:0` as the device if you have a GPU.
4. Use the tabs to **generate** novel colonies from random semantic codes,
   **reconstruct** real ones, **interpolate** between two images, and explore the
   semantic latent space. The **DDIM steps** slider trades speed for quality.

---

## Path B — programmatically

The hands-on notebook
[`tutorials/tutorial_diffusion.ipynb`](../../tutorials/tutorial_diffusion.ipynb) walks
through the full API. The essentials:

```python
from candescence.core.model_zoo import ModelZoo
from candescence.tlv.diffusion import load_diffusion_model
from candescence.tlv.diffusion import inference as diff_inf

# 1. Find and load a diffusion checkpoint from the zoo.
model = next(m for m in ModelZoo().list_models(project="tlv")
             if m.architecture == "diffusion_vae" and m.exists())
loaded = load_diffusion_model(str(model.get_checkpoint_path()), device="cuda:0")

# 2. Generate 8 novel colonies (fast; reproducible with a seed).
samples = diff_inf.generate(loaded, n=8, ddim_steps=30, seed=0)

# 3. Reconstruct / interpolate real images (load them as (C, H, W) tensors in [-1, 1]).
#    diff_inf.reconstruct(loaded, x, ddim_steps=30)
#    diff_inf.interpolate(loaded, x_a, x_b, n_steps=8, ddim_steps=30)

# 4. Read off the semantic codes (B, latent_dim).
codes = diff_inf.encode(loaded, x)
```

Every inference function returns a list of `[0, 1]` numpy images — grayscale `(H, W)`
or RGB `(H, W, 3)` — ready for `matplotlib`.

---

## Parameters worth knowing

| Parameter | Where | Effect |
|---|---|---|
| `ddim_steps` | all generation calls | Fewer = faster, lower quality. Generation tolerates ~20–30; raise it for sharper reconstructions. |
| `seed` | `generate` | Reproducible random semantic codes. |
| `n` | `generate` | Number of novel colonies to sample. |
| `n_steps` | `interpolate` | Number of frames along the semantic-code path. |
| `device` | `load_diffusion_model` | `cuda:0` for reconstruction/interpolation; `cpu` is fine for quick generation. |

---

## Further reading

- Programmatic walkthrough: [`tutorials/tutorial_diffusion.ipynb`](../../tutorials/tutorial_diffusion.ipynb)
- Architecture blueprint: [`docs/phase5_diffusion_blueprint.md`](../phase5_diffusion_blueprint.md)
- The VAEs it complements: [Tutorial 03 — The Models](03_the_models.md)
- Thesis: *Filamentation in Candida albicans*, Hallett Lab, Western University, 2026
