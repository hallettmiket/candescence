# Phase 2 Interface Audit — `src/candescence/interface/`

**Author:** Blacksmith (engineering audit)
**Date:** 2026-06-12
**Env:** `conda activate candescence_new` (Python 3.10.19)
**Launch:** `streamlit run src/candescence/interface/app.py`
**Scope:** runnable-state audit + reconnaissance for three Phase 2 features
(model curation tier, conditioning/adjust/augment UI, image-input picker).

> All paths absolute. All claims verified by reading the code and by import/launch
> tests in the `candescence_new` env. Path layer is Phase-1 `get_settings()` — no
> recommendation re-introduces hardcoded paths.

---

## 1. Per-page runnable state

Pages are **thin wrappers**: each `pages/N_*.py` sets `st.set_page_config(...)` then
imports and calls a backing `main()`. The two heavy backends are
`apps/training_app.py` (3436 lines) and `apps/latent_explorer_app.py` (7137 lines).

**Import test** (importlib by file path, since filenames start with digits) + a
headless `streamlit run` smoke launch (port 8753). Backing modules imported directly.

| Target | Result | Notes |
|---|---|---|
| `candescence.interface.home` | OK | landing dashboard |
| `apps/training_app.py` | OK (import) | |
| `apps/latent_explorer_app.py` | OK (import) | |
| `core/settings.py`, `core/model_zoo.py`, `core/config.py`, `tlv/factory.py` | OK | `model_zoo` logs "Loaded 4 models from zoo" |
| `pages/0_Home.py` | OK | |
| `pages/1_TLV_Training.py` | OK | executes `training_main()` on import (no `__name__` guard) |
| `pages/2_TLV_Explorer.py` | OK | executes `explorer_main()` on import |
| `pages/3_Varasana_Training.py` (1675 lines) | OK | |
| `pages/4_Model_Registry.py` | OK | executes `main()` on import |
| `pages/5_Dataset_Manager.py` | OK | |
| **Full app headless launch** | OK | "You can now view your Streamlit app… Local URL: http://localhost:8753"; no tracebacks |

**No import failures, no missing deps, no module-level crashes** in the live env.

Module-level side effects worth noting (not bugs, but load-bearing):
- `apps/training_app.py:88` `_settings = get_settings()`, `:89` `DEFAULT_IMAGE_DIR = f"{_settings.image_dir}/"`, `:91` `DEFAULT_OUTPUT_BASE` — resolved at import, correctly via settings.
- `apps/latent_explorer_app.py:36` `_settings = get_settings()`, `:1015` `ZOO_BASE = _settings.zoo_path` — same.
- `core/model_zoo.py:31` `DEFAULT_ZOO_PATH = get_settings().zoo_path` — resolved at import.
- `pages/1`, `2`, `4` call `main()` on **both** `__name__=="__main__"` and the `else`
  branch, so importing the file *runs the page*. Fine under Streamlit's page runner;
  means any module-level exception in a backend surfaces immediately on navigation.

---

## 2. Stubs / dead code / TODOs

| File:line | Kind | What's incomplete |
|---|---|---|
| `pages/3_Varasana_Training.py:1107-1128` | **Mock data (P1)** | `_get_or_compute_evaluation` returns `_create_mock_evaluation_results()` — all metrics hardcoded to `0.0` (mAP/AP50/AP75/precision/recall/f1, per-class dicts). Comment: *"In production, this would run actual model inference."* The eval/overview/sample-browser tabs render zeros, not real detector metrics. |
| `base.py:115`, `base.py:124` | Legitimate ABC | `raise NotImplementedError("Subclass must implement to()/eval()")` — abstract base `CandescenceModel`; not a bug. |
| `apps/training_app.py:2859` | Intentional | `pass  # matplotlib not critical — skip silently` — benign. |
| `apps/training_app.py:2959` | Guard | `st.error("Trainer not available. Training may not have completed.")` — error path, not a stub. |

The many `*_placeholder = st.empty()` hits in `training_app.py` (~1800-2400) and the
`placeholder=` kwargs in `5_Dataset_Manager.py` are **Streamlit UI placeholders**, not
unimplemented code.

**Dead / unreferenced legacy modules** (live pages do not use them; relevant to Phase 2
because they duplicate logic that could drift):
- `interface/tlv_explorer.py` — `TLVExplorerPage`, imported only by `pages/__init__.py:22`.
  The live Explorer page (`pages/2`) uses `apps/latent_explorer_app.py` instead. This file
  contains the only use of `streamlit_plotly_events` (lazy import at `tlv_explorer.py:168`)
  — see dependency flag in §3.
- `components/training_config.py` — `TrainingConfigPanel`/`TrainingConfigForm`, exported by
  `components/__init__.py:10` but **not used by the live training wizard**. It hardcodes
  Strategy 14 (`:247` *"This is locked for this training app"*, `:297-298`
  `"architecture": "tendril_vae", "strategy": 14`). The live wizard is
  `apps/training_app.py::_render_model_config` (`:905`). Treat `components/training_config.py`
  as legacy; do **not** wire Phase 2 into it.

---

## 3. Dependency / launch matrix

**Launch:** `streamlit run src/candescence/interface/app.py`
(lab rule: `nice -n 19 streamlit run …`; `apps/training_app.py:15` documents the nice form).
Standalone backends also runnable directly:
`streamlit run src/candescence/interface/apps/latent_explorer_app.py` and `…/apps/training_app.py`.

**Third-party libs by area** (import sites):
- `streamlit` — every page + app.
- `plotly` (express + graph_objects + subplots) — `training_app.py:30-32`,
  `latent_explorer_app.py:96` + lazy imports, `components/dataset_report.py:28-30`,
  `core/components.py:13`.
- `torch` / `torchvision` — training + explorer decode paths.
- `umap` — lazy: `latent_explorer_app.py:2548`, `components/latent_explorer.py:38`.
- `lpips` — `training/streamlit_trainer.py:16` (module-level).
- `pandas`, `numpy`, `PIL`, `cv2`(opencv), `scipy`, `sklearn`, `matplotlib`, `seaborn`.
- `streamlit_plotly_events` — **only** `tlv_explorer.py:168` (lazy, inside a fn).

**Installed in live env (all present):** torch 2.5.1, torchvision 0.20.1, lpips, umap 0.5.11,
plotly 6.5.2, hdbscan, sklearn 1.7.2, pandas 2.3.3, numpy 2.2.6, PIL 12.1.0, streamlit 1.53.1,
matplotlib, seaborn, cv2 4.13.0, scipy, h5py, pyarrow, **streamlit_plotly_events**, igraph,
leidenalg, piqa, torchmetrics, skimage.

**Declaration gaps — would break a fresh install:**

| Lib | `environment.yml` | `pyproject.toml` core `dependencies` | Verdict |
|---|---|---|---|
| streamlit | yes (`:35`) | **MISSING** | pyproject `pip install candescence` yields no GUI |
| plotly | yes (`:32`) | **MISSING** | same |
| umap-learn | yes (pip `:68`) | only `[analysis]` extra (`:61`) | explorer UMAP breaks on default install |
| hdbscan | yes (pip `:69`) | only `[analysis]` extra (`:62`) | clustering breaks on default install |
| opencv (cv2) | **MISSING** | **MISSING** | imported by code but undeclared everywhere |
| streamlit_plotly_events | **MISSING** | **MISSING** | undeclared in both; only used by dead `tlv_explorer.py` |

`environment.yml` (`conda env create -f environment.yml`) **does** install streamlit/plotly/umap/
hdbscan, so the documented path works. The fresh-`pip` path via `pyproject.toml` does **not**
install the GUI stack. **P1:** add `streamlit>=1.30`, `plotly>=5.18` to core `dependencies`;
add `opencv-python` to both manifests; either delete `tlv_explorer.py` or declare
`streamlit-plotly-events`.

---

## 4. Recon — model curation ("public vs research" tier)

Goal: public users see only Strategy 0 (`c_vae`), 1 (`uc_vae`), 14 (`tendril_vae`).

### (a) Where the wizard chooses a strategy/architecture
- **Canonical selector:** `apps/training_app.py:949-956`
  ```python
  strategy = st.selectbox(
      "Training Strategy",
      options=[14, 15, 16, 7, 8, 13, 1, 11, 12, 0],
      index=0, format_func=lambda x: STRATEGY_OPTIONS[x],
      key="cfg_strategy")
  ```
  Option label dict `STRATEGY_OPTIONS` at `:907-918`; description dict
  `STRATEGY_DESCRIPTIONS` at `:920-947`. This is **the** filter point.
- A second, read-only "Model" info panel at `:413-427` (`_strategy_info` dict) just echoes
  the chosen strategy — no widget, update its dict only for cosmetics.
- **Legacy/unused:** `components/training_config.py:245-298` hardcodes Strategy 14
  ("locked"). Not the live path — do not modify for tier work.

### (b) `factory.py` architecture map + strategy↔name
- `architecture_map` (name → class): `tlv/factory.py:78-91`
  (`c_vae, uc_vae, cond_uc_vae, cond_uc_vae_attention, cond_uc_vae_spatial, rc_vae,
  uc_vae_multistage, cruc_vae, cruc_vae_conv, crutch_vae, tendril_vae, multi_cond_tendril_vae`).
- Strategy number → architecture name in `_prepare_strategy`: `tlv/factory.py:371-452`.
  Key rows: `:384` strat 0→`c_vae`, `:387` strat 1→`uc_vae`, `:406-407` strat 14→`tendril_vae`,
  `:421` strat 15/16→`multi_cond_tendril_vae`, `:398` strat 7/8→`cond_uc_vae`, `:429` strat 13→`crutch_vae`.
- Selection dispatch: `prepare_vae` at `:244-254` (`self.architecture_map[architecture]`,
  raises `ValueError` for unknown arch at `:248-249`).

### (c) How the Registry + zoo filter/display models
- `core/model_zoo.py`: `ZooEntry` dataclass `:34-96` — fields include
  `model_type` (`"research"`/`"production"`, `:74`), `architecture` (`:76`),
  `tags` (`:83`), `training_config` (`:84`, holds the `strategy`).
  **No `tier` field yet.**
- `ModelZoo.list_models(project, model_type, tags)`: `:248-281` — filters by `project`
  (`:273-274`), `model_type` (`:275-276`), `tags` (`:277-278`).
- Registry page filters: `pages/4_Model_Registry.py:87-91`
  (`project`, `model_type` → `zoo.list_models(...)`), sidebar selectors at `:58` (Project)
  and `:64-68` (Model Type: All/Research/Production). Cards render at `:210-340`.
- Registration writes `model_type` + `training_config` from the wizard at
  `apps/training_app.py:3316` and `:3367` (`training_config={...}` passed to `zoo.register`).
- **Third filter site (do not miss):** the Explorer discovers models by **filesystem scan**,
  not the zoo: `latent_explorer_app.py::_discover_models` (`:1018`), surfaced in the model
  selectbox `_render_model_loader` (`:1047-1072`). A public tier must filter here too, or
  public users will still see research checkpoints in the Explorer.

### (d) Recommended mechanism
**Add `core/model_catalog.py` constant** (single source of truth), *not* a `tier` field on
`ZooEntry`. Rationale: tier is a property of the *strategy/architecture*, not of an individual
trained model; a constant avoids backfilling `tier` onto existing `registry.json` entries and
keeps the wizard, registry, and explorer consistent. Suggested:
```python
# core/model_catalog.py
PUBLIC_STRATEGIES = {0, 1, 14}
PUBLIC_ARCHITECTURES = {"c_vae", "uc_vae", "tendril_vae"}
def is_public(strategy: int | None = None, architecture: str | None = None) -> bool: ...
```
**Filter at these call sites** (gate on a `public_mode` session flag / settings):
1. `apps/training_app.py:951` — restrict `options=[…]` to `PUBLIC_STRATEGIES` when public.
2. `pages/4_Model_Registry.py:91` — post-filter `models` so that
   `entry.training_config.get("strategy") in PUBLIC_STRATEGIES` (or `entry.architecture in
   PUBLIC_ARCHITECTURES`) when public.
3. `latent_explorer_app.py::_discover_models` (`:1018`) — drop non-public checkpoints
   (read each model's `args.json`/`training_config` strategy).

If per-model overrides are ever needed (e.g. a research-only S0 model), add an **optional**
`tier: str = "research"` to `ZooEntry` *in addition to* the catalog — `ZooEntry(**entry_data)`
at `model_zoo.py:127` already tolerates new defaulted fields, so old registries stay loadable.

---

## 5. Recon — conditioning / adjust / augment UI (FiLM)

> **Reality check vs the brief:** the brief asks to expose conditioning on
> `hue/saturation/value/colony_size`. In code, FiLM conditioning variables are
> **`average_hue / average_saturation / average_value`** (HSV); **`colony_size` is NOT a
> conditioning input** — it exists only as a metadata/feature column
> (`tlv/inference/inference.py:1021`, `tlv/analysis/image_feature_table.py`). The multi-cond
> architecture's `COND_KEYS` are **`("hue", "day", "media")`** (batch-effect FiLM), *not*
> `hue/sat/value/colony_size`. Adding `colony_size` conditioning is **new model work**, not a
> UI toggle. Plan the UI around HSV; flag colony_size as an architecture change.

### (a) Config keys + how the wizard builds the config dict
- `core/config.py` defaults: `conditional_variables` (`:166` `["average_hue"]`),
  `conditional_decoder_fixed_values` (`:167` `{"average_hue": 0.5}`),
  `augment_decoder_images` (`:168`), `augment_images` (`:169`),
  `augmentation_variables` (`:170`), `augmentation_spread` (`:171`), `cond_dim` (`:173`).
  `adjust_images`/`adjustment_variables` are **not** defaulted in `config.py` — they are set
  in `factory._prepare_strategy` (`:379`, `:381`) and read by the dataset
  (`tlv/data/dataset.py:346, 755-762, 826-837`).
- **Live config-dict builder:** `apps/training_app.py:1484-1559` (`config_dict = {…}`). Key rows:
  `:1506` `conditional_variables`, `:1510-1511` `use_fixed_decoder` /
  `conditional_decoder_fixed_values`, `:1513-1517` `augment_decoder_images`,
  `:1519` `augment_images`, `:1520-1522` `augmentation_variables`,
  `:1523-1525` `augmentation_spread`.
- Conditioning/augment **widgets**: conditioning multiselect at
  `apps/training_app.py:980-986` (`"Conditioning Variables"`,
  options `["average_hue","average_saturation","average_value"]`, key `cfg_cond_vars`);
  augment/decoder controls at `:1019` (`cfg_augment_decoder_images`),
  `:1034` (`cfg_augment_images`), `:1040-1044` (`cfg_augmentation_spread`);
  fixed-decoder values built at `:1112` (`conditional_decoder_fixed_values = {'average_hue': 0.5}`).
- TLVConfig instantiation from the dict: `apps/training_app.py:1742-1777`
  (`from candescence.core.config import TLVConfig`, `config = TLVConfig(...)`).
- **Legacy builder (skip):** `components/training_config.py:296-300` and `:488-500`.

### (b) Multi-cond architecture + FiLM
- `tlv/architectures/tendril_vae_multi_cond.py`:
  `COND_KEYS = ("hue", "day", "media")` at **`:35`**;
  `_resolve_cond_keys(arguments)` at `:38` (reads `_cond_keys` from args, validates subset);
  per-variable dims from `_cond_dim_hue/_cond_dim_day/_cond_dim_media` (`:70` etc.);
  `_MultiFiLM` stack at `:76-105` (`film_hue`/`film_day`/`film_media`, named `film_*` so the
  factory optimizer picks them up via the substring `'film'` check, see `factory.py:260-261`).
- FiLM module: `tlv/architectures/film.py` — `class FiLM` `:20` (`__init__` `:46`,
  `forward` `:61`); `class FiLMGenerator` `:93`.
- `cond_keys` is persisted by `factory._prepare_strategy` (`:424-427`: S16→`['hue','media']`,
  S15→`['hue','day','media']`) and written to `args.json` (`factory._save_arguments`,
  `:337-357`) so inference rebuilds the same FiLM pathways. The explorer reads it back at
  `latent_explorer_app.py:1442-1451`.

### (c) Latent traversal / interpolation / decode-from-position (where to add cond sliders)
All take a `cond_tensor` and pass it into the model's decode — these are the exact hooks:
- `_decode_from_2d(x, y, k)` — `latent_explorer_app.py:3351-3404`. Builds `cond_tensor` from
  `cond_est` (`:3369-3371`) and calls `model.decode(z, cond_tensor)` (`:3402`) or
  `model.decode_with_tendril_modification(..., cond_tensor)` (`:3395-3397`). **Override
  `cond_tensor` from sliders here.**
- `_decode_from_sliders(base_index, z_values)` — `:4901-4940`. Derives `cond_tensor` from
  `st.session_state.conditioning[base_index]` (`:4918-4922`); calls `model.decode_with_skip
  (z, skip, cond_tensor)` (`:4938`). Slider UI: `_render_latent_sliders` (`:4943`).
- Interpolation: `_render_interpolation` (`:3696`), `_render_interpolation_for_space`
  (`:3723`), `_render_interpolation_plot` (`:3898`), `_generate_interpolation_with_refs`
  (`:4082`).
- Conditioning state plumbing: stored at `st.session_state.conditioning` (set
  `:1352`, read in both decode fns). Multi-cond load path: `:1397-1451`.

**Wire plan (5):** add HSV (+ later colony_size, post-model-change) sliders to the Explorer
sidebar; in `_decode_from_2d` (`:3369`) and `_decode_from_sliders` (`:4918`) replace the
neighbor-derived `cond_tensor` with the slider vector, re-decode. Add the same
augment/adjust controls to the wizard's config-dict at `apps/training_app.py:1484` if you
want to expose `adjustment_variables`/`adjust_images` (currently only auto-set in
`factory._prepare_strategy`).

---

## 6. Recon — image-input picker

- **Settings source:** `get_settings().image_dir` (Phase 1). Used at:
  - Training: `apps/training_app.py:89` `DEFAULT_IMAGE_DIR`, text_input at `:488-492`
    (key `cfg_image_dir`), read back at `:643`, `:745`, and into the config-dict at
    `:1486` (`"raw_images_path": st.session_state.cfg_image_dir`).
  - Explorer: **already has a source picker** at `latent_explorer_app.py:1090-1107`
    (radio `image_source` → "Raw images directory" uses `_settings.image_dir` `:1100`,
    or custom `st.text_input` `:1103-1106`, key `custom_images_path`); metadata text_input at
    `:1110-1113` (`_settings.metadata_xlsx`).
- **Image-load + dataset-build + metadata merge functions:**
  - Disk scan / replicate pairing: `_scan_replicate_pairs_from_disk(images_dir)`
    → `(rep_pairs, stem_to_file)`, `latent_explorer_app.py:6422-6445` (glob `*.bmp`,
    `:6432` `stem_to_file = {f.stem: f for f in all_files}`).
  - Load + HSV + metadata merge: `_load_images_for_stems(...)`,
    `latent_explorer_app.py:6448-…` (returns `(pair_df, conditioning)` with
    `id/transformed_image/rgb_image/my_rep/average_hue/average_saturation/average_value`);
    called at `:6552` and `:6650`.
  - Canonical training dataset builder: `tlv/data/dataset.py::FullDataset` (`:298`);
    `raw_images_path.glob('*.bmp')` at `:321`, filename→metadata at
    `_filenames_to_dataframe` (`:323`).
- **Existing QC component to reuse:** `components/dataset_report.py` —
  `class DatasetReportPanel` (`:36`, `render()` `:54`); helper
  `compute_hsv_from_images(image_dir, ...)` (`:329-361`). Training app already calls
  `compute_hsv_from_images` at `apps/training_app.py:786-788`.

**Wire plan (6):** add a sidebar **directory text + `st.file_uploader`** that writes the chosen
path into `st.session_state.cfg_image_dir` (training, `:488`) and `custom_images_path`
(explorer, `:1103`); both already feed `_load_images_for_stems` / `FullDataset` and
`compute_hsv_from_images`. No new load path needed — just a new input that sets the same keys.
Uploaded files must be staged to a temp dir under `get_settings().refined_path` (never `raw`),
then point the existing path at it.

---

## Prioritized punch-list

**P0 (blocking Phase 2):**
- Build `core/model_catalog.py` (`PUBLIC_STRATEGIES={0,1,14}`,
  `PUBLIC_ARCHITECTURES={c_vae,uc_vae,tendril_vae}`) and gate the strategy selector
  `apps/training_app.py:951`, the registry filter `pages/4_Model_Registry.py:91`, **and**
  the Explorer's `_discover_models` (`latent_explorer_app.py:1018`). All three must filter or
  the tier leaks.

**P1 (important):**
- Add `streamlit`, `plotly` to `pyproject.toml` core `dependencies` (`:35`); add
  `opencv-python` to `pyproject.toml` + `environment.yml`; resolve `streamlit_plotly_events`
  (declare it, or delete the only consumer `interface/tlv_explorer.py`). Fresh `pip install`
  currently ships no GUI.
- Replace Varasana mock metrics: `pages/3_Varasana_Training.py:1107-1128`
  (`_get_or_compute_evaluation` / `_create_mock_evaluation_results`) — wire real detector eval.
- Decide fate of legacy `components/training_config.py` (Strategy-14-locked) and
  `interface/tlv_explorer.py` (move to `obsolete/`, log in `src/ready_to_delete.md`) so Phase 2
  doesn't get wired into dead duplicates.

**P2 (nice-to-have):**
- `colony_size` conditioning is an **architecture change** (add a 4th FiLM key to
  `COND_KEYS`/`_MultiFiLM` in `tendril_vae_multi_cond.py:35,76`), not a UI toggle — scope
  separately from the HSV slider UI.
- Add `__name__`-guarding to pages so importing a page file doesn't auto-run `main()` (cleaner
  for tests).
- Promote a session-level `public_mode` flag into `core/settings.py` for a single switch.

---

## Where-to-wire cheat-sheet (file:line anchors)

| Phase 2 feature | Anchor(s) |
|---|---|
| **Tier — strategy selector** | `apps/training_app.py:949-956` (`cfg_strategy` selectbox) |
| **Tier — registry filter** | `pages/4_Model_Registry.py:87-91` (`zoo.list_models`); sidebar `:58,:64` |
| **Tier — explorer model list** | `apps/latent_explorer_app.py:1018` (`_discover_models`), `:1047-1072` |
| **Tier — arch/strategy maps** | `tlv/factory.py:78-91` (`architecture_map`), `:371-452` (`_prepare_strategy`) |
| **Tier — zoo schema** | `core/model_zoo.py:34-96` (`ZooEntry`), `:248-281` (`list_models`) |
| **Cond — config keys** | `core/config.py:166-173`; builder `apps/training_app.py:1484-1559` |
| **Cond — widgets** | `apps/training_app.py:980-986` (cond vars), `:1019/:1034/:1040` (augment) |
| **Cond — FiLM/COND_KEYS** | `tlv/architectures/tendril_vae_multi_cond.py:35,38,76-105`; `film.py:20,93` |
| **Cond — explorer decode** | `apps/latent_explorer_app.py:3351` (`_decode_from_2d`, cond@`:3369`), `:4901` (`_decode_from_sliders`, cond@`:4918`), `:4943` (`_render_latent_sliders`) |
| **Cond — interpolation** | `apps/latent_explorer_app.py:3696,3723,3898,4082` |
| **Image picker — training** | `apps/training_app.py:89,488-492,1486` (`cfg_image_dir`) |
| **Image picker — explorer** | `apps/latent_explorer_app.py:1090-1113` (source radio + text_input) |
| **Image picker — load/build** | `apps/latent_explorer_app.py:6422,6448,6552,6650`; `tlv/data/dataset.py:298,321,323` |
| **Image picker — QC reuse** | `components/dataset_report.py:36,54,329` |
