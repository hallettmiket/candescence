# Tutorial 02 — Using Your Own Images

**What you'll do:** Point Candescence at a directory of your own colony images, verify
that the app resolves paths correctly, and confirm images load in both the Training
wizard and the Latent Explorer.

---

## Before you start

Complete [Tutorial 01](01_getting_started.md) so the environment is working and you
have seen the app run against the sample data.

---

## Supported image formats

The app accepts: **bmp, png, jpg, jpeg, tif, tiff**.

Images should contain one cropped colony each. The recommended resolution is
128 × 128 pixels (the model default), but the loaders resize automatically.
If your images come from 96-well plate scans, use the colony-cropping pipeline
in `scripts/` or the Varasana object-detection page to produce per-colony crops first.

### Filename convention

Candescence parses growth medium, day, and plate number directly from filenames.
The expected pattern is:

```
P{plate}_{media}_{day}_{replicate}-r{row}-c{col}.{ext}
```

Examples:

```
P11_RPMI_day2_1-r1-c3.png        # plate 11, RPMI medium, day 2, rep 1, row 1, col 3
P7_spider_day5_2-r4-c8.bmp       # plate 7, spider medium, day 5
P2_YPD_wash_1-r2-c1.tif          # wash timepoint (day is None)
```

If your filenames do not follow this convention the images will still load and encode,
but medium/day metadata will not appear in the Explorer color-by menus.

---

## Option A — Config file (recommended for a fixed dataset)

1. Copy the example template:

   ```bash
   cp candescence.toml.example candescence.toml
   ```

2. Edit the `[paths]` section in your editor:

   ```toml
   [paths]
   images          = "/path/to/your/colony/images"
   manual_labels_csv = "/path/to/your/labels.csv"   # optional
   refined         = "/path/to/output/directory"
   ```

   Every key is optional — omit lines you do not want to override.

3. Launch as usual:

   ```bash
   nice -n 19 streamlit run src/candescence/interface/app.py
   ```

The config file is read from the repo root by default. To keep it elsewhere:

```bash
export CANDESCENCE_CONFIG=/path/to/my_config.toml
```

---

## Option B — Environment variables (good for scripting / CI)

```bash
export CANDESCENCE_IMAGES=/path/to/your/images
export CANDESCENCE_REFINED=/path/to/outputs
# Optional fine-grained overrides:
# export CANDESCENCE_ZOO=/path/to/model/zoo
# export CANDESCENCE_MANUAL_LABELS_CSV=/path/to/labels.csv
# export CANDESCENCE_METADATA_XLSX=/path/to/metadata.xlsx
```

Precedence order (highest wins):

```
explicit argument in code
        ↓
environment variable  (CANDESCENCE_IMAGES, etc.)
        ↓
candescence.toml  [paths] table
        ↓
packaged lab default  (<data>/…)
```

---

## Option C — In-app directory picker (no config needed)

Both the **TLV Training** wizard and the **TLV Explorer** have a built-in
image-source picker at the top of the page:

```
( ) Directory path   (●) Upload images
```

- **Directory path** — paste an absolute path into the text box. The app resolves
  relative paths from the repository root.
- **Upload images** — drag-and-drop or file-dialog upload. Uploaded files are staged
  under your `refined` output directory (never the raw tree).

![Image source picker](images/image_source_picker.png)

---

## Providing manual labels

The Explorer can color points by manually-assigned morphology labels. Labels live in
a CSV with two columns:

```csv
file_name,morphology
P11_RPMI_day2_1-r1-c3.png,smooth
P11_RPMI_day5_1-r2-c3.png,light filament
```

Point `manual_labels_csv` (in the config or via `CANDESCENCE_MANUAL_LABELS_CSV`) at
your CSV. The sample file at `data/sample/manually_labelled_images.csv` is a concrete
reference.

---

## Verifying path resolution before launching

```python
# Quick sanity check — run from the repo root
from candescence.core.settings import load_settings
s = load_settings()
print("image_dir :", s.image_dir)
print("refined   :", s.refined_path)
print("zoo       :", s.zoo_path)
print("labels    :", s.manual_labels_csv)
```

If `image_dir` does not point where you expect, check which config file Streamlit is
seeing and which environment variables are set.

---

## Worked example — a local folder of PNGs

```bash
# Suppose your images are in ~/Desktop/my_candida_images/
export CANDESCENCE_IMAGES=~/Desktop/my_candida_images
export CANDESCENCE_REFINED=~/Desktop/candescence_outputs
nice -n 19 streamlit run src/candescence/interface/app.py
```

Then in the **TLV Explorer** select **"Directory path"**, leave it blank (it reads from
the env var), and click **Load**. The app will enumerate all supported images in that
directory.

---

## Raw vs. refined — the lab rule

The lab convention is:

- **Raw data** (`<raw>/…`) is immutable — never overwrite or rename.
- **Refined** (`<refined>/…` or your own `refined` path) is where outputs,
  trained models, and uploaded files go.

Candescence enforces this: uploaded images are staged under `refined`, and training
outputs always go to the `zoo` sub-tree of `refined`. Do not point `refined` at a raw
data directory.

---

## Next steps

- **Tutorial 03** — [The models: choosing between Strategy 0, 1, and 14](03_the_models.md)
- Programmatic bulk encoding: [`tutorials/tutorial_production_workflow.ipynb`](../../tutorials/tutorial_production_workflow.ipynb)
- Latent space exploration in depth: [`tutorials/tutorial_latent_explorer.ipynb`](../../tutorials/tutorial_latent_explorer.ipynb)
