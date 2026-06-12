# Changelog

## v2.0.0 — Production release (2026-06-12)

A ground-up production version of Candescence for *Candida albicans* colony
morphology analysis. Clone it, point it at your own images (or use the bundled
sample), and explore VAE latent spaces through an interactive app.

> The original 2022 codebase is preserved under the **`v1-legacy`** git tag.

### Highlights

- **Runs on a fresh clone.** A bundled sample dataset (36 real colony images
  across morphologies and growth media) plus `candescence.sample.toml` let you
  launch the app with no lab infrastructure:
  ```bash
  conda env create -f environment.yml && conda activate candescence_new
  cp candescence.sample.toml candescence.toml
  streamlit run src/candescence/interface/app.py
  ```

- **Configurable data paths.** Every path resolves through a settings layer
  (`src/candescence/core/settings.py`): explicit argument → environment variable
  (`CANDESCENCE_RAW`, `CANDESCENCE_REFINED`, `CANDESCENCE_IMAGES`, …) →
  `candescence.toml` → packaged default. Nothing is hard-coded; bring your own
  data with a single env var or config file.

- **Curated model catalog.** End users see three production-ready architectures —
  **Strategy 0** (convolutional VAE), **Strategy 1** (U-Net VAE), and
  **Strategy 14** (Tendril VAE, recommended) — with a sidebar *Research mode*
  toggle that reveals the full experimental set.

- **Adjustment, augmentation & conditioning on H/S/V.** The training wizard
  exposes deterministic HSV adjustment, decoder-background augmentation, and FiLM
  conditioning on hue/saturation/value; the latent explorer adds conditioning
  sliders that re-decode at fixed latent *z*.

- **Bring-your-own-images.** A directory-or-upload picker (in both the training
  wizard and the explorer) accepts common image formats; uploads are staged
  safely under the refined tree.

- **Interactive Streamlit app.** TLV Training wizard, a rich Latent Explorer
  (PCA/UMAP/t-SNE, sprite maps, interpolation, latent/conditioning sliders,
  cluster enrichment), a Model Registry, a Dataset Manager, and Varasana colony
  detection.

- **Tutorials & docs.** Step-by-step guides in `docs/tutorials/`
  (Getting Started, Using Your Own Images, The Models) plus deeper programmatic
  notebooks in `tutorials/`.

### Under the hood

- Robustness for external data: multi-format image loading, optional strain
  metadata (skipped gracefully when absent), and small-dataset training fixes.
- A guard test (`tests/test_settings.py`) enforces that no machine-specific path
  leaks into any tracked file.
- The science follows Harry's thesis (*Filamentation in Candida albicans*):
  convolutional VAE → U-Net VAE and posterior collapse → FiLM conditioning →
  the Tendril VAE as the balanced solution.

### Notes

- This release replaces the public `candescence` repository contents; the prior
  2022 code remains available via the `v1-legacy` tag.
- A Stable Diffusion generative companion is planned for a future release.
