# Sample TLV dataset

A small, in-repo slice of the real *Candida albicans* colony imaging data so
Candescence runs out-of-the-box on a fresh clone — no lab-VM access required.

- `images/` — 36 real colony images (PNG, 136×135 RGB), stratified across
  morphology classes (smooth, light/heavy filament, wrinkle, ring, blank, …)
  and growth media (RPMI, YPD, spider, serum, control).
- `manually_labelled_images.csv` — `file_name,morphology` for those 36 images.

Filenames keep the original convention `P{plate}_{media}_{day}_{rep}-r{row}-c{col}`,
so the app derives media/day/plate automatically.

## Use it

From the repository root:

```bash
cp candescence.sample.toml candescence.toml
streamlit run src/candescence/interface/app.py
```

The sample config points `images` and `manual_labels_csv` here and writes any
trained models to a local `_candescence_runs/` directory. For richer latent-space
exploration, point the app at a larger image folder (your own, or the full lab
tree) via the in-app picker or the `CANDESCENCE_IMAGES` env var.

## Regenerate

The sample is reproducible from the full dataset (deterministic selection):

```bash
python scripts/build_sample_dataset.py
```
