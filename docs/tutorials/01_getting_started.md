# Tutorial 01 — Getting Started with Candescence

**What you'll do:** Clone the repository, set up the conda environment, launch the app,
and take your first look at a *Candida albicans* latent space by loading a **trained
model** and projecting colony images through it.

> **Important:** the TLV Explorer visualises images *through a trained VAE* — it does
> not display raw images on their own. So you need a model. If you have access to a
> populated model zoo (e.g. the Hallett-lab VM), you pick one there. On a fresh clone
> with no models, you first train one in the **TLV Training** wizard — see Step 6.

---

## Prerequisites

- [Miniconda](https://docs.conda.io/en/latest/miniconda.html) or Anaconda
- Git
- A browser (the app opens one automatically)
- Optional: an NVIDIA GPU (CPU-only works fine for the sample data)

---

## Step 1 — Clone the repository

```bash
git clone https://github.com/hallettmiket/candescence_new.git
cd candescence_new
```

All commands in this tutorial are run from the **repository root** (`candescence_new/`)
unless otherwise stated.

---

## Step 2 — Create the conda environment

```bash
conda env create -f environment.yml
conda activate candescence_new
```

This installs PyTorch, Streamlit, and all scientific dependencies listed in
`environment.yml`. The environment name is `candescence_new`.

Alternatively, if you already have a compatible Python 3.10 environment, you can
install the package in editable mode instead:

```bash
pip install -e .
```

---

## Step 3 — Choose your data + model setup

Candescence resolves all paths through a settings layer (env vars or a
`candescence.toml`). Pick the setup that matches your situation:

**A. You have access to a populated model zoo (e.g. the Hallett-lab VM).**
Use the default paths — do **not** create a `candescence.toml`. The app then reads the
lab image tree and the lab zoo of pre-trained models, which is the quickest way to see a
real latent space (Step 6, path A).

**B. Fresh clone, no lab access.** Copy the sample config so the app uses the 36 real
colony images bundled in `data/sample/images/` and writes outputs locally:

```bash
cp candescence.sample.toml candescence.toml
```

This needs no `<data>/` access. Note the bundled zoo starts **empty**, so you will
train a small model yourself before exploring (Step 6, path B).

You can verify the resolved paths before launching:

```python
# run from the repo root with: python -c "..."
from candescence.core.settings import load_settings
s = load_settings()
print("Images :", s.image_dir)
print("Outputs:", s.refined_path)
```

Expected output:

```
Images : /path/to/candescence_new/data/sample/images
Outputs: /path/to/candescence_new/_candescence_runs
```

---

## Step 4 — Launch the app

```bash
nice -n 19 streamlit run src/candescence/interface/app.py
```

> **Why `nice -n 19`?** Lab convention limits background processes to low CPU priority
> so they do not compete with other work on shared machines. The training wizard also
> sets `nice 19` internally, but wrapping the whole launch is good practice.

Streamlit prints a local URL (default `http://localhost:8501`). Your browser will
open it automatically; if not, open the URL manually.

![Home page](images/home_page.png)

---

## Step 5 — Tour the sidebar

The sidebar lists all pages:

| Page | Purpose |
|---|---|
| **Home** | Status dashboard and quick-start links |
| **TLV Training** | Step-by-step wizard to train a VAE on your images |
| **TLV Explorer** | Interactive latent-space exploration of a trained model |
| **Varasana Training** | Object-detection training (colony segmentation; brief mention below) |
| **Model Registry** | Browse and manage saved models in the zoo |
| **Dataset Manager** | Inspect and curate image datasets and labels |

By default the app shows the **public tier**: three curated models (Strategies 0, 1, 14).
A **Research mode** toggle in the sidebar unlocks the full set of experimental
architectures (Strategies 0–16). New users should leave Research mode off.

---

## Step 6 — Open the TLV Explorer

The Explorer encodes images **through a trained model**, so it offers a *model + images*
loader. Pick the path that matches your Step 3 choice.

### Path A — Use a pre-trained model from the zoo (recommended)

Use this when you did **not** create a `candescence.toml` (so the app sees a populated
zoo, e.g. on the lab VM).

1. Navigate to **TLV Explorer** and set **Load method** to **"From Model + Images"**.
2. In **Select model**, pick a Tendril (Strategy 14) model — e.g. one named `…S14_RPMI`.
   (Only public-tier models show by default; the sidebar **Research mode** toggle reveals
   the rest.) Use **Refresh** if the list looks stale.
3. Under **Image source → Directory path**, keep the default image directory and set
   **Filter images by media** to match the model (e.g. *RPMI*), with **Max samples ≈ 800**.
   For a quick tiny look instead, point the directory at `data/sample/images`.
4. Click **"Load Model + Compute Embeddings"** (≈20–40 s).
5. Choose **PCA** (or UMAP) and color by **morphology** or **media**; click points to see
   the colonies.

![TLV Explorer PCA plot](images/explorer_pca.png)

### Path B — Fresh clone with the sample data (train a small model first)

With the sample config the zoo is empty, so the Explorer has nothing to load yet. Train a
quick model first:

1. Go to **TLV Training**, choose **Strategy 0 (Conv VAE)**, and confirm the image
   directory is `data/sample/images`.
2. Set small split sizes (e.g. **train 24 / val 6 / test 6**), a handful of epochs, and
   start training. It saves into your local zoo.
3. Return to **TLV Explorer → "From Model + Images"**, pick the model you just trained,
   point images at `data/sample/images`, and **Load**.

> With only 36 images this is a **toy** model for learning the workflow — the latent
> structure will be weak. For a meaningful model, train on a larger image set
> (Tutorial 02) or use a pre-trained zoo model (Path A).

---

## Step 7 — Stopping and restarting

Press `Ctrl-C` in the terminal to stop Streamlit. Your trained models are saved under
`_candescence_runs/zoo/` (or wherever `refined_path` resolves). Delete or rename
`candescence.toml` to return to the lab-VM default paths.

---

## Resource limits

Per lab convention:

- Always launch with `nice -n 19`.
- Training uses at most **20 CPU cores** and **1 GPU**. The wizard enforces these
  defaults; do not override them on shared machines without PI sign-off.

---

## Next steps

- **Tutorial 02** — [Using your own images](02_using_your_own_images.md)
- **Tutorial 03** — [The models: Strategy 0, 1 and 14 explained](03_the_models.md)
- Deeper dive (programmatic): [`tutorials/tutorial_production_workflow.ipynb`](../../tutorials/tutorial_production_workflow.ipynb)
