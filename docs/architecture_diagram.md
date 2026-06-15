# Candescence Architecture Overview

This document describes how the Candescence software and data are organized across the repository, refined, and raw directories.

## Directory Structure Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              FILE SYSTEM                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  <repo>/     [REPOSITORY - Code]           │
│  ├── src/candescence/                          Python package               │
│  ├── scripts/                                  CLI tools                    │
│  ├── tutorials/                                Jupyter notebooks            │
│  ├── tests/                                    Unit tests                   │
│  └── docs/                                     Documentation                │
│                                                                              │
│  <raw>/candescence_new/            [RAW - Read-only Input]       │
│  ├── tlv_images/                               Original microscopy images   │
│  └── tlv_metadata/                             Original metadata files      │
│                                                                              │
│  <refined>/candescence_new/        [REFINED - Output & Models]   │
│  ├── {experiment}/{run}/                       Training outputs             │
│  ├── production_models.json                    Model registry               │
│  └── production_datasets.json                  Dataset registry             │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Data Flow Diagram

```
┌──────────────────┐
│   RAW DATA       │
│   (Read-only)    │
├──────────────────┤
│ tlv_images/      │──────────────────────────────────────┐
│   all-final/     │                                      │
│                  │                                      │
│ tlv_metadata/    │──────────────────────────────────────┤
│   *.xlsx, *.csv  │                                      │
└──────────────────┘                                      │
                                                          │
                                                          ▼
┌──────────────────────────────────────────────────────────────────────────┐
│                           REPOSITORY (Code)                               │
├──────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐       │
│  │  src/candescence│    │    scripts/     │    │   tutorials/    │       │
│  │  ───────────────│    │  ──────────────│    │  ──────────────│       │
│  │  core/          │    │  production/   │    │  tutorial_*.ipynb│      │
│  │   config.py     │    │   register_    │    │                  │       │
│  │   production_   │    │   model.py     │    │                  │       │
│  │   registry.py   │    │                │    │                  │       │
│  │                 │    │  research/     │    │                  │       │
│  │  tlv/           │    │   reproduce_   │    │                  │       │
│  │   factory.py    │    │   37_tendrils  │    │                  │       │
│  │   training/     │    │   .py          │    │                  │       │
│  │   inference/    │    │                │    │                  │       │
│  │   architectures/│    └────────────────┘    └──────────────────┘       │
│  │                 │                                                      │
│  │  interface/     │                                                      │
│  │   apps/         │◄─────── Streamlit Apps                               │
│  │    latent_      │                                                      │
│  │    explorer_    │                                                      │
│  │    app.py       │                                                      │
│  │                 │                                                      │
│  │  detection/     │                                                      │
│  │   detector.py   │                                                      │
│  │                 │                                                      │
│  │  analysis/      │                                                      │
│  │   morphology_   │                                                      │
│  │   stats.py      │                                                      │
│  └─────────────────┘                                                      │
│                                                                           │
└───────────────────────────────────┬───────────────────────────────────────┘
                                    │
                                    │ Training & Inference Output
                                    ▼
┌──────────────────────────────────────────────────────────────────────────┐
│                         REFINED DATA (Output)                             │
├──────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  {experiment_name}/{save_name}/          ◄── TLVConfig creates this      │
│  ├── models/                                                              │
│  │   ├── model.pth                       Trained weights                  │
│  │   └── args.json                       Training configuration           │
│  ├── analyses/                           Inference outputs                │
│  ├── loss_statistics/                    Training loss plots              │
│  └── meta_info/                          Experiment metadata              │
│                                                                           │
│  production_models.json                  ◄── ProductionRegistry           │
│  production_datasets.json                                                 │
│                                                                           │
└──────────────────────────────────────────────────────────────────────────┘
```

## Component Interactions

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              WORKFLOWS                                       │
└─────────────────────────────────────────────────────────────────────────────┘

                         ┌──────────────────┐
                         │   User/Notebook  │
                         └────────┬─────────┘
                                  │
       ┌──────────────────────────┼──────────────────────────┐
       │                          │                          │
       ▼                          ▼                          ▼
┌──────────────┐   ┌───────────────────────┐   ┌─────────────────┐
│  TRAINING    │   │   INFERENCE           │   │   EXPLORATION   │
│  WORKFLOW    │   │   WORKFLOW            │   │   WORKFLOW      │
│  (2 options) │   │                       │   │                 │
└──────┬───────┘   └───────────┬───────────┘   └────────┬────────┘
       │                       │                        │
   ┌───┴───┐                   ▼                        ▼
   │       │          ┌─────────────────┐      ┌─────────────────┐
   ▼       ▼          │   Inference     │      │  Streamlit App  │
┌─────┐ ┌─────────┐   │   class         │      │  (latent_       │
│Script│ │Streamlit│   │                 │      │   explorer)     │
│/Note-│ │Training │   └────────┬────────┘      └────────┬────────┘
│book  │ │App      │            │                        │
└──┬───┘ └────┬────┘            │                        │
   │          │                 │                        │
   ▼          ▼                 │                        │
┌─────────────────┐             │                        │
│   TLVConfig     │             │                        │
│   Factory       │             │                        │
│   Trainer       │             │                        │
└────────┬────────┘             │                        │
            │                     │                     │
            │ Reads               │ Reads               │ Reads
            ▼                     ▼                     ▼
   ┌─────────────────────────────────────────────────────────────┐
   │                         RAW DATA                             │
   │   <raw>/candescence_new/                         │
   │   - tlv_images/all-final/*.bmp                              │
   │   - tlv_metadata/*.xlsx, *.csv                              │
   └─────────────────────────────────────────────────────────────┘
            │                     │                     │
            │ Writes              │ Writes              │ Reads
            ▼                     ▼                     ▼
   ┌─────────────────────────────────────────────────────────────┐
   │                       REFINED DATA                           │
   │   <refined>/candescence_new/                     │
   │   - {experiment}/{run}/models/model.pth                     │
   │   - {experiment}/{run}/analyses/                            │
   │   - production_models.json                                  │
   └─────────────────────────────────────────────────────────────┘
            │                     │                     │
            │                     │                     │
            ▼                     ▼                     ▼
   ┌─────────────────┐   ┌─────────────────┐   ┌─────────────────┐
   │  Register to    │   │  Quality        │   │  Interactive    │
   │  Production     │   │  Metrics        │   │  Visualization  │
   │  Registry       │   │  Reports        │   │                 │
   └─────────────────┘   └─────────────────┘   └─────────────────┘
```

## Production Registry Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        PRODUCTION REGISTRY FLOW                              │
└─────────────────────────────────────────────────────────────────────────────┘

  ┌──────────────┐
  │ Train Model  │
  │ (Factory)    │
  └──────┬───────┘
         │
         │ Saves model.pth + args.json
         ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  <refined>/candescence_new/{exp}/{run}/models/   │
  │  ├── model.pth                                               │
  │  └── args.json                                               │
  └──────────────────────────────────────────────────────────────┘
         │
         │ Validate quality
         ▼
  ┌──────────────┐
  │  Inference   │──────► Quality OK?
  │  Validation  │              │
  └──────────────┘              │
                                │ Yes
                                ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  ProductionRegistry.register_model()                         │
  │  - or -                                                      │
  │  python scripts/production/register_model.py                 │
  └──────────────────────────────────────────────────────────────┘
         │
         │ Writes to registry
         ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  <refined>/candescence_new/production_models.json │
  │                                                              │
  │  {                                                           │
  │    "models": {                                               │
  │      "my_model_v1": {                                        │
  │        "name": "My Production Model",                        │
  │        "model_path": "/data/.../models/model.pth",           │
  │        "version": "1.0.0",                                   │
  │        ...                                                   │
  │      }                                                       │
  │    }                                                         │
  │  }                                                           │
  └──────────────────────────────────────────────────────────────┘
         │
         │ Used by applications
         ▼
  ┌──────────────┐     ┌──────────────┐     ┌──────────────┐
  │  Streamlit   │     │   Scripts    │     │  Notebooks   │
  │  Apps        │     │              │     │              │
  │              │     │              │     │              │
  │ registry.    │     │ registry.    │     │ registry.    │
  │ get_model_   │     │ get_model_   │     │ get_model_   │
  │ path(id)     │     │ path(id)     │     │ path(id)     │
  └──────────────┘     └──────────────┘     └──────────────┘
```

## Module Dependencies

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         src/candescence/ MODULES                             │
└─────────────────────────────────────────────────────────────────────────────┘

                              ┌─────────────┐
                              │    core/    │
                              │  ─────────  │
                              │  config.py  │
                              │  morphology │
                              │  logging    │
                              │  production │
                              │  _registry  │
                              └──────┬──────┘
                                     │
           ┌─────────────────────────┼─────────────────────────┐
           │                         │                         │
           ▼                         ▼                         ▼
    ┌─────────────┐          ┌─────────────┐          ┌─────────────┐
    │    tlv/     │          │ detection/  │          │  analysis/  │
    │  ─────────  │          │  ─────────  │          │  ─────────  │
    │  factory    │          │  detector   │          │  morphology │
    │  training/  │          │  cell_      │          │  _stats     │
    │  inference/ │          │  extractor  │          │  visualiza- │
    │  architec-  │          │  curriculum │          │  tion       │
    │  tures/     │          │  _trainer   │          │             │
    │  data/      │          │             │          │             │
    │  losses     │          │             │          │             │
    └──────┬──────┘          └──────┬──────┘          └─────────────┘
           │                        │
           │                        │
           └────────────┬───────────┘
                        │
                        ▼
                 ┌─────────────┐
                 │ interface/  │
                 │  ─────────  │
                 │  app.py     │
                 │  apps/      │◄──── Streamlit Applications
                 │   latent_   │
                 │   explorer  │
                 │  model_     │
                 │  loader     │
                 │  components/│
                 └─────────────┘
```

## Streamlit Training App Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        STREAMLIT TRAINING APP FLOW                           │
└─────────────────────────────────────────────────────────────────────────────┘

  ┌──────────────────────────────────────────────────────────────────────────┐
  │  STEP 1: CONFIGURE                                                        │
  │                                                                           │
  │  TrainingConfigPanel.render()                                             │
  │  ├── Data Configuration (paths, image dimension)                          │
  │  ├── Dataset Split (train/val/test counts)                               │
  │  ├── Model Architecture (latent dim, conditioning) [locked to #14]       │
  │  ├── Training Parameters (epochs, batch size, learning rates)            │
  │  └── Output Configuration (experiment name, save name)                   │
  │                                                                           │
  │  GPU Selection: nvidia-smi → recommend GPU #4 (lowest usage)             │
  └──────────────────────────────────────────────────────────────────────────┘
                                     │
                                     │ Validate & Start
                                     ▼
  ┌──────────────────────────────────────────────────────────────────────────┐
  │  STEP 2: TRAIN                                                            │
  │                                                                           │
  │  StreamlitTrainer.train_with_callbacks()                                  │
  │  ├── os.nice(19)  ◄── Process priority (low)                             │
  │  │                                                                        │
  │  │  for epoch in range(total_epochs):                                    │
  │  │  ├── check pause_flag → wait                                          │
  │  │  ├── check stop_flag → break (save)                                   │
  │  │  ├── training_epoch() → train_loss                                    │
  │  │  ├── validation_epoch() → val_loss                                    │
  │  │  ├── progress_callback(metrics) → update UI                           │
  │  │  └── reconstruction_callback(images) → update grid                    │
  │  │                                                                        │
  │  TrainingProgressPanel.render()                                           │
  │  ├── Progress bar + ETA                                                   │
  │  ├── Loss curves (Plotly line chart)                                     │
  │  ├── Reconstruction grid (8 images: original + reconstructed)            │
  │  ├── Training log (scrollable)                                           │
  │  └── Controls: [Pause] [Stop] [Cancel]                                   │
  └──────────────────────────────────────────────────────────────────────────┘
                                     │
                                     │ Training Complete
                                     ▼
  ┌──────────────────────────────────────────────────────────────────────────┐
  │  STEP 3: SAVE                                                             │
  │                                                                           │
  │  TrainingSummaryPanel.render()                                            │
  │  ├── Final metrics (best epoch, train/val loss)                          │
  │  ├── Final reconstruction visualization                                   │
  │  ├── Research vs Production choice (radio buttons)                       │
  │  │   └── Production → ProductionRegistry.register_model()                │
  │  ├── Model metadata form (name, description, version)                    │
  │  └── [Save Model] [Start New Training]                                   │
  └──────────────────────────────────────────────────────────────────────────┘
                                     │
                                     │ Save Model
                                     ▼
  ┌──────────────────────────────────────────────────────────────────────────┐
  │  OUTPUT                                                                   │
  │                                                                           │
  │  <refined>/candescence_new/{exp}/{run}/                       │
  │  ├── models/                                                              │
  │  │   ├── model.pth          ◄── Trained weights                          │
  │  │   └── args.json          ◄── Training configuration                   │
  │  ├── loss_statistics/       ◄── Loss plots                               │
  │  └── meta_info/             ◄── Experiment metadata                      │
  │                                                                           │
  │  production_models.json     ◄── Registry entry (if Production selected)  │
  └──────────────────────────────────────────────────────────────────────────┘
```

## Key Path Summary

| Category | Path | Purpose |
|----------|------|---------|
| **Repository** | `<repo>/` | Source code |
| **Raw Images** | `<raw>/candescence_new/tlv_images/all-final/` | Original microscopy images (read-only) |
| **Raw Metadata** | `<raw>/candescence_new/tlv_metadata/` | Original metadata files (read-only) |
| **Experiment Output** | `<refined>/candescence_new/{exp}/{run}/` | Training outputs |
| **Trained Models** | `.../{exp}/{run}/models/model.pth` | Model weights |
| **Model Config** | `.../{exp}/{run}/models/args.json` | Training configuration |
| **Model Registry** | `<refined>/candescence_new/production_models.json` | Production model catalog |
| **Dataset Registry** | `<refined>/candescence_new/production_datasets.json` | Production dataset catalog |

## Launch Commands

| Application | Command |
|-------------|---------|
| **Latent Explorer** | `uv run streamlit run src/candescence/interface/apps/latent_explorer_app.py` |
| **Training App** | `uv run streamlit run src/candescence/interface/apps/training_app.py` |
