# Tutorial 05 — Varasana Cell Detection

**What you'll do:** Run the pretrained **Varasana** detector on a microscopy
image, view the bounding-box overlay and per-class counts, and export the
detections as CSV — all from the **Varasana Detection** page in the app.

Varasana is the original Candescence detector: an **FCOS** (ResNet-101 + FPN)
object detector that finds *Candida albicans* cells and classifies each into one
of **15 morphologies** (yeast/budding × white/opaque/gray, shmoo, pseudohyphae,
hyphae, junction/start classes, plus artifact/unknown).

> **Reference:** Bettauer et al. (2022) *Microbiology Spectrum*,
> [10.1128/spectrum.01472-22](https://doi.org/10.1128/spectrum.01472-22).

---

## How it runs (and why there's a setup step)

Varasana was built on a legacy **MMDetection** stack (torch 1.7 / mmcv-full 1.2.5)
that is incompatible with Candescence's modern environment. Rather than
reimplement or retrain, the detector runs **unchanged** in an isolated venv that
the app calls out-of-process. It runs on **CPU** (the legacy CUDA build predates
this lab's GPUs) — a few seconds per image, fine for interactive use.

See [detection/legacy/README.md](../../src/candescence/detection/legacy/README.md)
for the full rationale.

---

## Prerequisites

1. The main app environment (`uv sync`) and a launched app.
2. The **isolated legacy detection environment**, built once:

   ```bash
   bash src/candescence/detection/legacy/setup_legacy_env.sh
   ```

   This creates `<repo>/.venv-legacy/` (git-ignored). The path is overridable via
   `$CANDESCENCE_LEGACY_PYTHON`. If it's missing, the page shows this command.
3. The Varasana weights (config + checkpoint). With lab-VM access these resolve
   automatically under the legacy refined tree; otherwise the page tells you the
   expected paths.

---

## Walkthrough

1. Launch the app and open **Varasana Detection** (also reachable from the
   **Home** launcher under the Varasana project):

   ```bash
   uv run streamlit run src/candescence/interface/app.py
   ```

2. In the sidebar, set the **Score threshold** (default 0.30) — the minimum
   confidence to keep a detection.
3. Choose an image: either point the **Image source** at a directory or upload
   your own, then pick one from the dropdown.
4. Click **Detect**. The detector runs on CPU (a few seconds) and the page shows:
   - the image with **colour-coded bounding boxes** + class labels,
   - a **per-class count** bar chart, and
   - a **Download detections (CSV)** button (label, score, and box coordinates).

---

## Notes

- **Input domain:** Varasana was trained on DIC microscopy of *C. albicans*.
  Feeding it out-of-domain images (e.g. colony top-views) will run fine but
  produce meaningless detections.
- **Threshold:** raise it to keep only confident detections; lower it to surface
  more (and more false positives).
- **Speed:** CPU inference reloads the model per click (~seconds). This is
  expected for the isolated worker.

## Further reading

- [Tutorial 06 — Grace Detection](06_grace.md) — the sibling FCOS detectors.
- [detection/legacy/README.md](../../src/candescence/detection/legacy/README.md) —
  the legacy environment and worker design.
