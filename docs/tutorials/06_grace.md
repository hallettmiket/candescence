# Tutorial 06 — Grace Detection

**What you'll do:** Run the pretrained **Grace** detectors on a microscopy image,
switch between the two models, and compare them side by side — from the
**Grace Detection** page in the app.

Grace contributes **two** FCOS (ResNet-101 + FPN) detectors, built with the same
machinery as [Varasana](05_varasana.md) but for different assays:

| Model | Classes | Assay |
|---|---|---|
| **Macro** | 9 | macrophage-challenged *C. albicans* |
| **TC** | 7 | tissue-culture-grown *C. albicans* |

> **Reference:** Case, Westman et al. (2023) *mBio*,
> [10.1128/mbio.02745-23](https://doi.org/10.1128/mbio.02745-23).

---

## How it runs

Like Varasana, the Grace detectors run **unchanged** on the isolated legacy
MMDetection stack, called out-of-process on CPU. If you've already done
[Tutorial 05](05_varasana.md), the environment is the same — nothing extra to
install.

---

## Prerequisites

1. The main app environment (`uv sync`) and a launched app.
2. The isolated legacy detection environment (shared with Varasana):

   ```bash
   bash src/candescence/detection/legacy/setup_legacy_env.sh
   ```

3. The Grace weights (macro + TC config/checkpoint), resolved under the legacy
   refined tree (the page reports the expected paths if missing).

---

## Walkthrough

1. Open **Grace Detection** (or reach it from the **Home** launcher under the
   Grace project).
2. Choose a **Detector** at the top of the page:
   - **Macro (9-class)** — single-model detection, like the Varasana page.
   - **TC (7-class)** — same, with the tissue-culture model.
   - **Compare both** — pick one image and run *both* detectors, shown
     **side by side** (overlay + per-class counts for each). Useful for seeing
     how the macro and TC models read the same field.
3. Set the **Score threshold** in the sidebar, pick an image, and click
   **Detect** (or **Detect (all)** in compare mode).
4. Each result panel offers a **CSV download** of its detections.

---

## Notes

- **Pick the model that matches your assay.** Macro and TC have different class
  sets and were trained on different image types; running the wrong one on a
  given image will under-detect.
- **Compare mode** runs two CPU inferences, so it takes roughly twice as long as
  a single detector.

## Further reading

- [Tutorial 05 — Varasana Cell Detection](05_varasana.md) — single-detector walkthrough and the shared detection workflow.
- [detection/legacy/README.md](../../src/candescence/detection/legacy/README.md) —
  the legacy environment and worker design.
