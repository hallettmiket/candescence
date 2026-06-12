# Tutorial 03 — The Models

**What you'll do:** Understand the three public-tier VAE architectures — Strategy 0
(`c_vae`), Strategy 1 (`uc_vae`), and Strategy 14 (`tendril_vae`) — learn when to
choose each one, and understand how adjustment, augmentation, and conditioning differ
as tools for handling nuisance variation.

The narrative follows the research arc in Harry's thesis
(*Filamentation in Candida albicans*, Hallett Lab, Western University, 2026):
the simplest model is always introduced first, and each successor is motivated by a
concrete limitation of its predecessor.

---

## The problem

*C. albicans* colonies photographed on 96-well plates carry two kinds of signal:

1. **Biological signal** — colony morphology reflecting filamentation state
   (smooth, light filament, heavy filament, wrinkle, ring, blank, …).
2. **Technical nuisance** — background hue, saturation, and value driven almost entirely
   by growth medium (~85% of background hue variance is medium-specific) and, to a much
   lesser degree, by imaging day and plate.

A useful latent space must separate these two signals. The three public models represent
successive, thesis-documented strategies for achieving that separation.

---

## Strategy 0 — `c_vae` (basic convolutional VAE)

### What it is

A four-block convolutional encoder followed by transposed-convolution decoder with no
skip connections. Each block is: `Conv2d → BatchNorm → LeakyReLU × 2 → MaxPool`.
The bottleneck projects to (μ, log σ²) in a latent space of configurable dimension
(default 128). A standard β-VAE loss is used: reconstruction + β × KL divergence.

The thesis calls this the **"Newborn"** (Strategy 0, Supplemental Table of models).

### Latent-space behavior

Because there are no skip connections the decoder has no shortcut around the bottleneck.
The encoder is therefore *forced* to compress all information — including morphology —
into the latent vector. This produces a structured, informative latent space in which
nearby points genuinely represent similar colonies, and latent interpolation produces
smooth phenotypic transitions between colonies.

The downside is reconstruction quality: without skip connections, fine spatial detail
(filamentation texture, colony edges) is lost. Roughly 50 epochs of training are needed
to converge.

### When to choose it

- You want the clearest possible latent geometry for downstream clustering or
  metadata association, and reconstruction fidelity matters less.
- You are benchmarking a new dataset and want the simplest possible baseline.
- You have limited compute; c_vae converges reliably even on CPU for small datasets.

### Not recommended when

- You need to visually inspect reconstructions (e.g., to sanity-check what the model
  has learned about texture or filamentation structure).

---

## Strategy 1 — `uc_vae` (U-Net VAE)

### What it is

The same encoder as Strategy 0, but the decoder receives **skip connections** from
each encoder block (U-Net style). The skip connections carry high-resolution spatial
feature maps directly to the corresponding decoder stage, dramatically improving
reconstruction quality.

The thesis calls this the **"Toddler"** (Strategy 1).

### The posterior collapse problem

The skip connections are precisely what breaks the latent space.

Because the decoder can reconstruct fine spatial detail directly from the skip
features — without ever consulting the latent vector — the encoder learns that
investing capacity in the bottleneck is unnecessary. The KL divergence
collapses toward zero early in training: the variational posterior aligns with the
prior, `q(z|x) ≈ p(z)`, and most latent dimensions carry zero information.

The thesis demonstrates this concretely: in Strategy 0, latent interpolation between
two colonies produces smooth phenotypic transitions; in Strategy 1, the same
interpolation produces only black backgrounds — the latent space has become meaningless
(thesis Figure on posterior collapse, Chapter 4).

### When to choose it

- You need sharp reconstructions and do not need a structured latent space at all
  (e.g., a pure image-generation use case).
- You are studying whether a specific preprocessing step affects reconstruction quality
  independently of latent structure.

### Not recommended when

- You intend to do any latent-space analysis (clustering, UMAP, interpolation,
  dimension sliders). Strategy 14 is almost always the better choice.

### Research-mode note

Strategies 7 and 8 (conditional U-Net VAE, and the same with squeeze-excitation
attention) are visible in Research mode. They add FiLM conditioning to the U-Net
base but still carry posterior-collapse risk.

---

## Strategy 14 — `tendril_vae` (Tendril VAE, **recommended**)

### What it is

The Tendril VAE is the novel architecture introduced in the thesis (Chapter 4,
"The Tendril VAE: a balanced approach that addresses both reconstruction and nuisance
variable issues"). It starts from the insight that if skip connections are going to
carry spatial information anyway, we can use that capacity constructively rather than
fighting it.

**Outer VAE** — a standard U-Net VAE (same as Strategy 1) is trained first, producing
high-quality reconstructions. The bottleneck of this outer VAE may still suffer
from posterior collapse.

**Tendrils** — at each encoder depth level ℓ the outer VAE produces a feature map
x_ℓ of increasing abstraction (x_1 is fine-grained spatial detail; x_4 is global
semantic structure). A small auxiliary VAE — a "tendril" — is trained to reconstruct
each feature map x_ℓ, learning a compact latent representation z_ℓ of dimension 128.
The outer encoder is frozen during tendril training.

**Multiple latent spaces** — the result is four (or more) latent spaces z_1 through
z_4 that encode the same images at different levels of abstraction. The primary outer
bottleneck z_0 may still be collapsed, but the tendril latent spaces are healthy and
informative. This allows you to:

- Select the depth whose latent representation best balances reconstruction quality
  and biological interpretability for your specific question.
- Down-weight or exclude tendrils dominated by a known nuisance covariate (e.g., a
  tendril that encodes mainly background hue).

FiLM conditioning (see below) is integrated into every encoder block of the outer VAE,
allowing color-related nuisance variables to be partially disentangled during training.

### When to choose it

- You want a structured latent space **and** good reconstructions.
- You are doing morphology classification, clustering, metadata association, or any
  downstream biological analysis.
- This is the default and recommended choice for all new analyses.

### In the Explorer

When a Tendril model is loaded, the TLV Explorer exposes a **space selector** that
lets you switch between tendril levels. Each level produces a different PCA/UMAP/t-SNE
projection. Cross-space neighbor comparison panels let you see how the same colony is
represented at different depths of abstraction.

---

## Adjustment, augmentation, and conditioning — what are these?

All three are strategies for handling nuisance variation (primarily background hue
driven by growth medium). They are exposed as options in the Training wizard.

### Adjustment (deterministic HSV normalisation)

Every input image is shifted to a target average hue/saturation/value **before** it
enters the encoder. This removes nuisance variation at the cost of discarding real
color information. The VAE then trains on a color-normalised dataset.

In the Training wizard: enable **"HSV adjustment"** and set target H, S, V values
(0–1 scale).

### Augmentation (capacity-pressure approach)

Each training image is paired with several randomly perturbed copies that differ in
background hue/saturation/value. The encoder's limited bottleneck capacity is then
"wasted" on arbitrary hue perturbations, discouraging it from using latent dimensions
for hue information.

In the Training wizard: enable **"Background augmentation"** and set the perturbation
strength and number of augmented copies.

### Conditioning (FiLM layers)

The VAE learns explicitly to accept conditioning variables — average hue, saturation,
or value — as inputs via **Feature-wise Linear Modulation (FiLM)** layers (Perez et al.
2017). A FiLM layer maps the conditioning signal y to per-channel scale γ(y) and
shift β(y) and applies the affine transform `γ(y) · h + β(y)` to each feature map h,
allowing the network to modulate its representations based on known nuisance covariates
without discarding the original image content.

In the Training wizard (Strategy 14 / conditional strategies): expand the **"FiLM
conditioning"** section and select which variables to condition on
(`average_hue`, `average_saturation`, `average_value`).

In the Explorer, per-variable H/S/V **conditioning sliders** let you fix the latent
vector z and re-decode with different conditioning values, showing how the same latent
code maps to different visual appearances under different color conditions.

> **Note on colony size:** Colony size is recorded as metadata and is visible in the
> Explorer color-by menu, but it is **not** a conditioning variable — do not expect
> it to appear in the FiLM conditioning controls.

---

## Quick-reference comparison table

| | Strategy 0 | Strategy 1 | Strategy 14 |
|---|---|---|---|
| Architecture | Conv VAE | U-Net VAE | Tendril VAE |
| Skip connections | No | Yes | Yes (outer) |
| Latent structure | Healthy | Collapsed | Healthy (tendrils) |
| Reconstruction quality | Moderate | High | High |
| FiLM conditioning | No | No | Yes |
| Multiple latent spaces | No | No | Yes (one per depth) |
| Training epochs | ~50 | ~10 | ~10 (outer) + tendril |
| Recommended for | Baseline / latent analysis | Reconstruction only | General use |

---

## Coming soon — Stable Diffusion companion

A Stable Diffusion generative model for *C. albicans* colony image synthesis is
described in the thesis discussion (Chapter 5, "A diffusion-based generative companion
to the VAE") and is planned for a future release. It is not yet available in the app.

---

## Further reading

- Architecture details: [`docs/tendril_vae_architecture.txt`](../tendril_vae_architecture.txt)
- Programmatic model training: [`tutorials/tutorial_tendril_vae.ipynb`](../../tutorials/tutorial_tendril_vae.ipynb)
- Full exploration workflow: [`tutorials/tutorial_latent_explorer.ipynb`](../../tutorials/tutorial_latent_explorer.ipynb)
- Thesis: *Filamentation in Candida albicans*, Hallett Lab, Western University, 2026
  (the canonical source for all architectural decisions documented here).
