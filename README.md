# Candescence

Deep-learning tools for exploring *Candida albicans* colony morphology through
variational autoencoders (VAEs).

> **v2 — production release.** A ground-up production version: configurable data
> paths, a curated model catalog (Strategy 0 / 1 / 14), an interactive Streamlit
> app, a bundled sample dataset that runs on a fresh clone, and step-by-step
> tutorials. The original 2022 code is preserved under the **`v1-legacy`** tag.
> See [CHANGELOG.md](CHANGELOG.md).

Candescence encodes top-view photographs of *C. albicans* colonies into structured
latent spaces, enabling interactive visualization, phenotype clustering, and metadata
association across thousands of colonies. It accompanies Harry's thesis
(*Filamentation in Candida albicans*, Hallett Lab, Western University, 2026).

---

## Quickstart (5 minutes)

```bash
# 1. Clone and install (uv creates a project-local .venv from uv.lock)
git clone https://github.com/hallettmiket/candescence.git
cd candescence
uv sync                 # add --all-extras for the dev + analysis tooling

# 2. Use the bundled sample data (36 real colony images, no lab VM needed)
cp candescence.sample.toml candescence.toml

# 3. Launch (uv run executes inside the project environment)
nice -n 19 uv run streamlit run src/candescence/interface/app.py
```

The app opens in your browser. The **TLV Explorer** projects colony images into
PCA/UMAP/t-SNE latent space **through a trained model** — so you load a *model + images*,
not raw images alone. If you have access to a populated model zoo (e.g. the lab VM), skip
step 2, pick a pre-trained Tendril (Strategy 14) model in the Explorer, and explore
immediately. On a fresh clone, first train a small model on the sample data with the
**TLV Training** wizard (with only 36 images it is a toy for learning the workflow). See
[Getting Started](docs/tutorials/01_getting_started.md) for both paths.

---

## Tutorials

The [docs/tutorials/](docs/tutorials/) directory has step-by-step guides:

| Tutorial | What you learn |
|---|---|
| [01 Getting Started](docs/tutorials/01_getting_started.md) | Clone, install, first launch |
| [02 Using Your Own Images](docs/tutorials/02_using_your_own_images.md) | Config, env vars, in-app picker |
| [03 The Models](docs/tutorials/03_the_models.md) | Strategy 0 / 1 / 14; adjustment vs conditioning |
| [04 The Diffusion Companion](docs/tutorials/04_diffusion.md) | Generate / reconstruct / interpolate with the conditional diffusion model |

Deeper programmatic walkthroughs live in [`tutorials/`](tutorials/) as Jupyter
notebooks.

---

## The models

Three architectures are available to all users (a sidebar Research mode toggle
reveals the full experimental set):

| Strategy | Architecture | When to use |
|---|---|---|
| **14** (recommended) | Tendril VAE | Structured latent space + sharp reconstructions; FiLM conditioning on HSV |
| **1** | U-Net VAE | Sharp reconstructions; beware posterior collapse in latent space |
| **0** | Conv VAE | Cleanest latent geometry; moderate reconstruction quality |

See [Tutorial 03](docs/tutorials/03_the_models.md) for the full story.

---

## Pointing Candescence at your own data

No path is hard-coded — every location resolves through
`candescence.core.settings`. To run on your own machine, override the defaults
in either of two ways (you can override just one path; the rest derive from it):

**Environment variables**

```bash
export CANDESCENCE_IMAGES=/path/to/your/images   # input colony image directory
export CANDESCENCE_REFINED=/path/to/outputs       # models, zoo, analysis outputs
# Optional finer-grained overrides:
#   CANDESCENCE_RAW, CANDESCENCE_ZOO,
#   CANDESCENCE_METADATA_XLSX, CANDESCENCE_MANUAL_LABELS_CSV
# Point CANDESCENCE_CONFIG at a non-default config file location.
```

**Config file** — copy `candescence.toml.example` to `candescence.toml` in the
repo root (or `~/.config/candescence/candescence.toml`) and edit the `[paths]`
table. Precedence: explicit argument → environment variable → config file →
packaged lab default.

See [Tutorial 02](docs/tutorials/02_using_your_own_images.md) for a full walkthrough.

---

## Repository layout

```
candescence/
├── src/candescence/          # Python package
│   ├── interface/            # Streamlit app and pages
│   ├── tlv/                  # VAE architectures and training
│   └── core/                 # Settings, model catalog, logging
├── tutorials/                # Jupyter notebooks (hands-on deep dives)
├── docs/
│   └── tutorials/            # Step-by-step Markdown tutorials (start here)
├── data/sample/              # 36 bundled colony images for first-run demo
├── scripts/                  # Analysis and utility scripts
├── exp/                      # Numbered experiments
├── candescence.sample.toml   # Config for the bundled sample data
└── candescence.toml.example  # Config template for your own data
```

---

## Resource guidelines

Per lab convention, all Candescence processes should run under low CPU priority:

```bash
nice -n 19 uv run streamlit run src/candescence/interface/app.py
```

The training wizard enforces `nice 19` internally and uses at most 20 CPU cores
and 1 GPU by default. Do not override these limits on shared machines without
PI sign-off.

---

## Authors

Hallett Lab — [mikehallett.science](https://mikehallett.science)
Contact: mike.hallett@uwo.ca

---

## Related projects

- **candescence_master** — original implementation (read-only reference):
  `<legacy-repo>/candescence_master/`

---

## License

See [LICENSE](LICENSE).
