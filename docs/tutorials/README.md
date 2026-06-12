# Candescence Tutorials

Welcome to the Candescence documentation. The tutorials below will guide you from
a fresh clone to trained models and interactive latent-space exploration of
*Candida albicans* colony morphology.

---

## Step-by-step tutorials (start here)

| # | Tutorial | What you will learn |
|---|---|---|
| 01 | [Getting Started](01_getting_started.md) | Clone, install, launch the app against bundled sample data |
| 02 | [Using Your Own Images](02_using_your_own_images.md) | Point the app at a custom image directory via config, env vars, or the in-app picker |
| 03 | [The Models](03_the_models.md) | Strategy 0 / 1 / 14 explained; adjustment vs. augmentation vs. conditioning |

---

## Jupyter notebooks (deeper dives)

These notebooks live in [`tutorials/`](../../tutorials/) at the repository root and
provide hands-on, code-level walkthroughs. They complement the step-by-step tutorials
above but require a running `candescence_new` conda environment.

| Notebook | What it covers |
|---|---|
| [`tutorial_interface.ipynb`](../../tutorials/tutorial_interface.ipynb) | Programmatic model loading, encoding images, and using the `LatentExplorer` and `InterpolationTool` APIs |
| [`tutorial_latent_explorer.ipynb`](../../tutorials/tutorial_latent_explorer.ipynb) | In-depth latent-space analysis: projections, neighbor galleries, interpolation, silhouette and Mantel metrics |
| [`tutorial_tendril_vae.ipynb`](../../tutorials/tutorial_tendril_vae.ipynb) | Full Tendril VAE training and verification: `TLVConfig`, dataset loading, training loop, inference, latent analysis |
| [`tutorial_production_workflow.ipynb`](../../tutorials/tutorial_production_workflow.ipynb) | End-to-end production run: configure paths, train, register model, run inference on new images |
| [`tutorial_varasana_detection.ipynb`](../../tutorials/tutorial_varasana_detection.ipynb) | Colony detection with the Varasana object-detection pipeline |

---

## Quick-start cheat sheet

```bash
# 1. Install
conda env create -f environment.yml && conda activate candescence_new

# 2. Sample data quickstart
cp candescence.sample.toml candescence.toml

# 3. Launch
nice -n 19 streamlit run src/candescence/interface/app.py

# 4. Point at your own images
export CANDESCENCE_IMAGES=/path/to/your/images
export CANDESCENCE_REFINED=/path/to/outputs
nice -n 19 streamlit run src/candescence/interface/app.py
```

---

## Screenshots needed

The following images are referenced in the tutorials but have not yet been captured.
A human should launch the app, navigate to each page, and save a screenshot to
`docs/tutorials/images/`:

| Filename | Where to capture |
|---|---|
| `home_page.png` | App home page after launch |
| `explorer_pca.png` | TLV Explorer PCA plot with sample data loaded |
| `image_source_picker.png` | The directory/upload radio buttons in Training or Explorer |
| `training_wizard.png` | TLV Training wizard step 1 (configuration) |
| `training_progress.png` | TLV Training wizard step 2 (live loss curves) |
| `explorer_conditioning_sliders.png` | HSV conditioning sliders in the Explorer |
| `tendril_space_selector.png` | The tendril depth selector in the Explorer with a Tendril model loaded |

---

## Where to go next

- Architecture notes: [`docs/tendril_vae_architecture.txt`](../tendril_vae_architecture.txt)
- Source audit: [`docs/audit_report.md`](../audit_report.md)
- Thesis (canonical reference): *Filamentation in Candida albicans*, Hallett Lab,
  Western University, 2026
