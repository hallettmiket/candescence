# Candescence Master Audit Report

**Date:** 2026-01-27 (Updated: 2026-01-28)
**Purpose:** Pre-migration audit for candescence_new refactoring project
**Priority Focus:** TLV subproject (manuscript in progress)

---

## Executive Summary

Candescence is a mature research codebase (~10,700 lines in TLV alone) with **639 commits** over 2+ years. The codebase has:
- ✅ Feature-rich VAE framework with 14 architecture variants
- ✅ Comprehensive analysis capabilities
- ❌ No formal versioning (no tags, no CHANGELOG)
- ❌ 10+ hardcoded absolute paths
- ❌ Heavy use of wildcard imports
- ❌ Only 2 of 37 experiments have README documentation
- ❌ 11 fragmented conda environments with version conflicts

---

## Answers to Your Questions

### 1. What is the current structure of `candescence_master`?

```
candescence_master/
├── README.md
├── INSTALLATION_INSTRUCTIONS
├── LICENSE
├── pytorch_env.yml                 # Root-level conda environment
├── assets/
└── projects/
    ├── tlv/                        # Tel Aviv VAE (PRIORITY) - 780MB, 111 files
    │   ├── vae/                    # Core implementation (10,702 lines)
    │   ├── yml/                    # 3 conda environments
    │   ├── figures/                # Visualization outputs
    │   └── [37 experiment dirs]    # Numbered experiments (0-37)
    ├── varasana/                   # Original FCOS classifier - 1.7MB, 378 files
    ├── grace/                      # Transfer learning (Cowen Lab) - 169MB, 148 files
    ├── guelph/                     # Guelph classifier - 8.7MB, 161 files
    ├── varasana_gan/               # FastGAN - 80KB, 8 files
    ├── varasana_vae/               # R/Keras VAE - 188KB, 5 files
    └── bhavnit_thesis/             # Thesis work - 47MB, 23 files
```

**Total:** 834 code files, ~2GB repository size

### 2. What are the main scripts/entry points in `tlv`?

| Entry Point | Location | Purpose |
|-------------|----------|---------|
| `run-all.ipynb` | Each experiment dir | Primary experiment execution |
| `candescence_vae.py` | vae/ | Simple wrapper (3 lines) |
| `factory.py` | vae/ | Master orchestration (1,373 lines) |
| `train_vae.py` | vae/ | VAETrainer class (963 lines) |
| `inference.py` | vae/ | Post-training analysis (926 lines) |

**Typical usage:**
```python
from factory import Factory
factory = Factory(strategy=14, ...)  # Configure
factory.create_dataset()
factory.create_vae()
factory.create_trainer()
factory.train()
```

### 3. What dependencies does each subproject require?

| Environment | Python | PyTorch | CUDA | Status |
|-------------|--------|---------|------|--------|
| **pytorch_tut** (root) | 3.9.16 | 2.0.1 | 11.8 | ✅ Modern, GPU |
| **tlv_env** | 3.10.13 | 2.2.0 | CPU | ⚠️ CPU-only! |
| **candescence** (grace) | 3.7.9 | 1.7.1 | 10.2 | ❌ EOL Python |
| **candescence** (varasana) | 3.7.9 | 1.7.1 | 10.2 | ❌ EOL Python |
| **deepmicroscopy** (guelph) | 3.7.9 | 1.7.1 | 10.2 | ❌ EOL Python |
| **guelph2clone** | 3.7.9 | 1.10.0 | Mismatch | ❌ Broken |

**Core dependencies (TLV):**
- PyTorch (torch, torchvision, torchaudio)
- Scientific: numpy, scipy, pandas, scikit-learn
- Visualization: matplotlib, seaborn, PIL
- Analysis: umap-learn, hdbscan, igraph, leidenalg
- Metrics: lpips, piqa (SSIM), ignite

### 4. Are there obvious conflicts between subproject dependencies?

**Yes, significant conflicts:**

| Conflict Type | Details |
|---------------|---------|
| Python versions | 3.7.9 (EOL) vs 3.9.16 vs 3.10.13 |
| PyTorch versions | 1.7.1, 2.0.1, 2.2.0 |
| CUDA versions | 10.2 vs 11.1 vs 11.8 |
| GPU vs CPU | tlv_env is CPU-only despite GPU libs |

**Recommendation:** Use `pytorch_tut` as base environment (Python 3.9.16, PyTorch 2.0.1, CUDA 11.8) and add project-specific overlays.

### 5. What is the current state of documentation?

| Documentation Type | Status |
|--------------------|--------|
| Main README | ✅ Exists |
| INSTALLATION_INSTRUCTIONS | ✅ Exists |
| Per-experiment README | ❌ Only 2 of 37 experiments |
| Function docstrings | ❌ Most missing |
| API documentation | ❌ None |
| CHANGELOG | ❌ None |
| Version file | ❌ None |

**Documented experiments:**
- `22-adjusted_attitude`: Single line about Table 1
- `9-augmentation-experiments`: Brief augmentation details

### 6. What trained models exist in `<legacy-refined>/candescence_master`?

**Total Model Inventory:**
- **1,406 model files** (.pth, .pt)
- **470.5 GB** total model storage
- **7.4 TB** total refined data (TLV project)

**TLV Model Organization:**
```
<legacy-refined>/candescence_master/projects/tlv/
├── 37-tendrils/           # 3.3 TB - Main production (62 subdirectories)
│   ├── tendrils_figure_runs/
│   │   └── models/
│   │       ├── model.pth       # Main checkpoint
│   │       ├── args.json       # Hyperparameters
│   │       └── tendril_*.pth   # Layer-specific
│   └── [61 more runs]
├── figures/               # 457 GB - Publication outputs
│   └── tendril_ucvae_figures_complete/  # Latest (Jan 14, 2025)
├── 31-spatial_attention/  # 1.5 TB
├── 34-cruvae/             # 411 GB
└── [40+ other experiments]
```

**Recent production models (Jan 2025):**
- `tendril_ucvae_figures_complete/` - Latest complete analysis
- `tendrils_figure_runs_afterbreak/` - Post-break production
- `38-tendril_cvae/firstrun_debug/` - Conditional VAE

**Configuration tracking:**
- 30 `args.json` files with hyperparameters
- No centralized model registry
- No version documentation beyond directory names

### 7. Are there any existing version indicators or release tags?

**No formal versioning exists:**
- ❌ No git tags (releases)
- ❌ No CHANGELOG.md
- ❌ No VERSION file
- ❌ No `__version__` in code
- ❌ No pyproject.toml/setup.py

**Branches:**
- main (active)
- tendril (feature)
- adversarial, jonathanli-alpha, resnet_vae (remote)

---

## Critical Issues for Manuscript Preparation

### 1. Hardcoded Paths (10+ locations)

**In factory.py:**
```python
CANDESCENCE_ROOT = "<legacy-refined>/candescence_master/"  # Line 54
```

**In ALL architecture files (9 files):**
```python
logging.basicConfig(filename='<legacy-refined>/candescence_master/projects/tlv/training.log', ...)
```

### 2. Code Quality Issues

| Issue | Severity | Files Affected |
|-------|----------|----------------|
| Wildcard imports | High | 5 files (factory.py, train_vae.py, inference.py, candescence_vae.py, __init__.py) |
| Print statements | Medium | 15+ in dataset.py |
| Missing docstrings | High | Most functions |
| Commented-out code | Low | 4+ files |
| Magic numbers | Medium | factory.py (100+ parameters) |
| Duplicate logging config | Medium | 10+ files |

### 3. Reproducibility Concerns

| Concern | Current State |
|---------|--------------|
| Random seed management | ✅ Present in Factory |
| Hyperparameter tracking | ⚠️ args.json (inconsistent) |
| Environment specification | ❌ CPU-only in latest tlv_env |
| Model provenance | ❌ No registry |
| Result versioning | ❌ Directory names only |

---

## TLV Architecture Overview

### VAE Variants (14 architectures)

| Architecture | File | Purpose |
|--------------|------|---------|
| c_vae | architecture_c_vae.py | Simple conditional VAE |
| uc_vae | architecture_uc_vae.py | Unconditional U-Net VAE |
| cond_uc_vae | architecture_cond_uc_vae.py | Conditional U-Net VAE |
| cond_uc_vae_attention | architecture_cond_uc_vae_attention.py | With attention blocks |
| cond_uc_vae_spatial | architecture_cond_uc_vae_spatial.py | With spatial filtering |
| rc_vae | architecture_rc_vae.py | ResNet-based VAE |
| uc_vae_multistage | architecture_uc_vae_multistage.py | Multi-stage U-Net |
| cruc_vae | architecture_cruc_vae.py | Conditional Redirected U-Net |
| cruc_vae_conv | architecture_cruc_vae_conv.py | CRUC with conv optimization |
| crutch_vae | architecture_crutch_vae.py | Skip-connection based |
| **tendril_vae** | architecture_tendril_vae.py | **Latest: hierarchical latent** |

### 37-tendrils (Latest Experiment)

```python
# Key configuration from run-all.ipynb
strategy = 14                      # tendril_vae strategy
conditional_variables = ['average_hue']
train_num = 1200
validation_num = 400
test_num = 400
kl_weight = 1
LPIPS_weight = 10
latent_dim = 128
tendril_latent_dim = 128
tendril_num_epochs = 50
```

**Tendril Innovation:**
- Creates secondary VAEs per image for hierarchical latent learning
- Per-image normalization of latent representations
- Separate training for each tendril layer

---

## Recommended Migration Strategy

### Phase 1: Immediate (TLV Manuscript) ✅ COMPLETE

1. ✅ **Fix hardcoded paths** → Use configurable paths via Init class
2. ✅ **Replace wildcard imports** → Explicit imports
3. ✅ **Add GPU support** → Fix environment.yml for CUDA
4. ✅ **Document 37-tendrils** → Tutorial notebook with reproduction steps
5. ✅ **Create model manifest** → TLVConfig with experiment tracking

### Phase 2: Varasana Detection Integration ✅ COMPLETE (2026-01-28)

1. ✅ **Integrate FCOS detection** → CandidaDetector class
2. ✅ **Add cell extraction** → CellExtractor for VAE input
3. ✅ **Create analysis module** → MorphologyStats, DetectionVisualizer
4. ✅ **Shared morphology definitions** → 15 classes, curriculum stages
5. ✅ **Tutorial notebook** → tutorial_varasana_detection.ipynb
6. ✅ **Test coverage** → 19 tests in test_detection.py

### Phase 3: Code Quality (In Progress)

1. Add docstrings to all public functions
2. Replace print() with logging
3. Remove commented-out code
4. Add type hints
5. Create unified configuration system

### Phase 4: Structure

1. Reorganize into src/candescence/ package
2. Create pyproject.toml
3. Implement semantic versioning
4. Separate research/production code

### Phase 5: Environment

1. Create unified environment based on pytorch_tut
2. Add project-specific requirements files
3. Test on clean Linux installation

---

## File Summary Statistics

| Metric | Value |
|--------|-------|
| Total Python files (TLV vae/) | 25 |
| Total lines of code | 10,702 |
| Classes defined | 55+ |
| Functions defined | 47+ |
| Experiment directories | 37 |
| Architecture variants | 14 |
| Hardcoded paths | 10+ locations |
| Files with wildcard imports | 5 |
| Experiments with documentation | 2 of 37 |
| Largest file | factory.py (1,373 lines) |
| Total trained models | 1,406 files |
| Total model storage | 470.5 GB |
| Total refined data | 7.4 TB (TLV) |

---

## Phase 2: Varasana Detection Integration (2026-01-28)

### Overview

Integrated the Varasana FCOS detection system from `candescence_master` into `candescence_new` as a clean, modular package. This phase adds object detection capabilities while maintaining the existing TLV (Tendril VAE) functionality.

### Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Detection framework | MMDETECTION (FCOS) | Keep existing production model |
| R/Keras VAE | **Dropped entirely** | Superseded by Tendril VAE |
| Curriculum learning | Retained | Proven effective for 15-class training |
| Architecture | 3-stage pipeline | Detection → TLV → Analysis |

### New Module Structure

```
src/candescence/
├── core/
│   ├── morphology.py          # NEW: Shared 15-class definitions
│   └── ...
├── detection/                  # NEW: FCOS-based detection module
│   ├── __init__.py
│   ├── configs/
│   │   └── fcos_resnet101_fpn.py
│   ├── datasets/
│   │   └── candida_dataset.py
│   ├── inference/
│   │   ├── detector.py        # CandidaDetector class
│   │   └── cell_extractor.py  # CellExtractor for VAE input
│   └── training/
│       └── curriculum_trainer.py
├── analysis/                   # NEW: Statistical analysis module
│   ├── __init__.py
│   ├── morphology_stats.py    # MorphologyStats class
│   └── visualization.py       # DetectionVisualizer class
└── tlv/                        # Existing Tendril VAE (unchanged)
```

### Core Classes Created

#### `candescence.core.morphology`

Shared morphology class definitions used across all modules:

```python
class MorphologyClass(IntEnum):
    YEAST_WHITE = 0
    BUDDING_WHITE = 1
    YEAST_OPAQUE = 2
    BUDDING_OPAQUE = 3
    YEAST_GRAY = 4
    BUDDING_GRAY = 5
    SHMOO = 6
    ARTIFACT = 7
    UNKNOWN = 8
    PSEUDOHYPHAE = 9
    HYPHAE = 10
    H_JUNCTION = 11
    P_JUNCTION = 12
    P_START = 13
    H_START = 14

CURRICULUM_STAGES = [
    "white",        # Yeast White, Budding White
    "opaque",       # + Opaque variants
    "gray",         # + Gray variants
    "shmoo",        # + Shmoo
    "pseudohyphae", # + Pseudohyphae, junctions
    "hyphae",       # + Hyphae (all 15 classes)
]
```

#### `candescence.detection.CandidaDetector`

FCOS inference wrapper:

```python
from candescence.detection import CandidaDetector

detector = CandidaDetector()  # Uses production model
detections = detector.detect("image.jpg", threshold=0.5)

# Each detection has:
# - bbox: (x1, y1, x2, y2)
# - label: int (0-14)
# - label_name: str ("Yeast White", etc.)
# - score: float (0-1)
```

#### `candescence.detection.CellExtractor`

Crops detected cells for VAE input:

```python
from candescence.detection import CellExtractor

extractor = CellExtractor(detector, output_size=128)
crops = extractor.extract("image.jpg", threshold=0.5)
tensor = extractor.crops_to_tensor(crops)  # Ready for VAE
```

#### `candescence.analysis.MorphologyStats`

Statistical analysis:

```python
from candescence.analysis import MorphologyStats

stats = MorphologyStats()
stats.add_detections(image_path, detections)
summary = stats.summary()  # Class distributions, confidence metrics
df = stats.to_dataframe()  # Per-detection DataFrame
```

#### `candescence.analysis.DetectionVisualizer`

Plotting utilities:

```python
from candescence.analysis import DetectionVisualizer

viz = DetectionVisualizer()
fig = viz.plot_class_distribution(detections)
fig = viz.plot_confidence_histogram(detections)
fig = viz.plot_filamentous_pie(detections)
annotated = viz.annotate_image(image, detections)
```

### Production Model Location

```
<legacy-refined>/candescence_master/production/varasana/
├── config.py     # MMDETECTION configuration
└── model.pth     # Trained FCOS weights (ResNet-101 + FPN)
```

### Tests Added

New test file: `tests/test_detection.py`

| Test Class | Tests | Coverage |
|------------|-------|----------|
| `TestMorphologyClasses` | 6 | Class definitions, mappings, curriculum |
| `TestDetection` | 2 | Detection dataclass properties |
| `TestCandidaDetector` | 2 | Detector paths, classes property |
| `TestCellExtractor` | 2 | CellCrop shape properties |
| `TestMorphologyStats` | 5 | Statistics, DataFrame conversion |
| `TestCurriculumTrainer` | 2 | Config defaults, stage listing |

**Total: 19 tests**

### Tutorial Notebook

Created: `notebooks/tutorial_varasana_detection.ipynb`

Covers:
1. Environment setup and morphology class overview
2. Loading the pre-trained FCOS detector
3. Running detection on microscopy images
4. Extracting cell crops for VAE analysis
5. Statistical analysis with MorphologyStats
6. Visualization with DetectionVisualizer
7. Integration with Tendril VAE
8. Batch processing documentation

### Pipeline Integration

Complete workflow from plate image to latent space:

```python
# Step 1: Detection
from candescence.detection import CandidaDetector, CellExtractor
detector = CandidaDetector()
extractor = CellExtractor(detector, output_size=128)
crops = extractor.extract("plate.jpg", threshold=0.5)

# Step 2: Convert to tensor
tensor = extractor.crops_to_tensor(crops, normalize=True)

# Step 3: VAE encoding
from candescence.tlv.factory import Factory
factory = Factory(strategy=14, ...)
# ... load trained model ...
with torch.no_grad():
    z, mu, logvar, skip = vae.encoder(tensor, cond)

# Step 4: Analysis
from candescence.analysis import MorphologyStats
stats = MorphologyStats()
stats.add_detections("plate.jpg", detections)
```

### What Was Dropped

| Component | Reason |
|-----------|--------|
| `varasana_vae/` (R/Keras VAE) | Superseded by Tendril VAE |
| R analysis scripts | Replaced by Python analysis module |
| Legacy training scripts | Replaced by CurriculumTrainer |

### Files Modified Today

| File | Changes |
|------|---------|
| `src/candescence/__init__.py` | Added morphology exports |
| `src/candescence/core/__init__.py` | Added morphology exports |
| `tests/test_detection.py` | Fixed floating-point assertion |

### Next Steps

1. Verify MMDETECTION installation in `candescence_new` environment
2. Test detection on actual microscopy images
3. Validate pipeline integration with Tendril VAE
4. Consider adding batch processing optimizations

---

## Phase 2 Extended: Interface Development (2026-01-28)

### Overview

Created an interactive web-based interface for exploring trained Candescence models. The interface is inference-only (no training) and designed to be accessible for non-expert users while remaining extensible for future models.

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Web framework | Streamlit | Rapid prototyping, good interactive widgets |
| Visualization | Plotly | Interactive plots with click events |
| Click handling | streamlit-plotly-events | Direct integration with Streamlit |
| Architecture | ABC pattern | CandescenceModel interface for extensibility |
| Model discovery | ModelRegistry | Centralized model catalog |
| Caching | @st.cache_resource | Efficient model/embedding loading |

### Architecture

```
Interface Layer (Inference Only)
┌─────────────────────────────────────────────────────────┐
│  app.py (Streamlit Entry Point)                         │
│    ├── Model Selection Sidebar                          │
│    ├── Welcome Page                                     │
│    └── Explorer Page                                    │
├─────────────────────────────────────────────────────────┤
│  Components                                             │
│    ├── LatentExplorer (Plotly scatter + click events)   │
│    ├── ImagePanel (Display + metadata)                  │
│    └── InterpolationTool (Latent interpolation)         │
├─────────────────────────────────────────────────────────┤
│  Model Layer                                            │
│    ├── CandescenceModel (ABC)                           │
│    ├── TLVModelWrapper (Adapts TLV VAEs)                │
│    └── ModelRegistry (Discovery + Loading)              │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  TLV Module (Existing)                                  │
│    ├── Factory                                          │
│    ├── Architectures (0-14)                             │
│    └── Trained Models                                   │
└─────────────────────────────────────────────────────────┘
```

### New Module Structure

```
src/candescence/
├── interface/                    # NEW: Interactive exploration module
│   ├── __init__.py              # Package exports
│   ├── app.py                   # Main Streamlit application
│   ├── base.py                  # CandescenceModel ABC
│   ├── model_loader.py          # TLVModelWrapper
│   ├── data_utils.py            # Data preprocessing utilities
│   ├── components/
│   │   ├── __init__.py
│   │   ├── latent_explorer.py   # LatentExplorer with Plotly
│   │   ├── image_panel.py       # ImagePanel component
│   │   └── interpolation.py     # InterpolationTool
│   └── pages/
│       ├── __init__.py
│       └── tlv_explorer.py      # TLVExplorerPage
└── core/
    └── model_registry.py        # NEW: ModelRegistry class
```

### Core Classes Created

#### `candescence.interface.CandescenceModel` (ABC)

Abstract base class defining the interface for all models:

```python
from abc import ABC, abstractmethod

class CandescenceModel(ABC):
    @abstractmethod
    def encode(self, x: torch.Tensor, cond: Optional[torch.Tensor] = None) -> torch.Tensor:
        """Encode input to latent space."""

    @abstractmethod
    def decode(self, z: torch.Tensor, cond: Optional[torch.Tensor] = None) -> torch.Tensor:
        """Decode latent vector to output."""

    @abstractmethod
    def get_latent_dim(self) -> int:
        """Return latent space dimensionality."""

    @property
    @abstractmethod
    def model_info(self) -> Dict[str, Any]:
        """Return model metadata."""

    @property
    @abstractmethod
    def device(self) -> torch.device:
        """Return device model is on."""
```

#### `candescence.interface.TLVModelWrapper`

Adapts TLV VAE models to the CandescenceModel interface:

```python
from candescence.interface import TLVModelWrapper, load_tlv_model

# Load from checkpoint
model = load_tlv_model(
    checkpoint_path="/path/to/model.pth",
    args_path="/path/to/args.json"
)

# Encode images
latent = model.encode(images, conditioning)

# Decode latent vectors
reconstructed = model.decode(latent, conditioning)
```

Supports all TLV strategies:
- Strategy 0: c_vae (conditional VAE)
- Strategy 1: uc_vae (unconditional U-Net VAE)
- Strategy 7: cond_uc_vae (conditional U-Net VAE)
- Strategy 8: rc_vae (ResNet-based VAE)
- Strategy 9: cond_uc_vae_attention (with attention)
- Strategy 9.5-9.9: Spatial filtering variants
- Strategy 13: crutch_vae (skip-connection based)
- Strategy 14: tendril_vae (hierarchical latent)

#### `candescence.core.ModelRegistry`

Centralized model discovery and loading:

```python
from candescence.core.model_registry import ModelRegistry

registry = ModelRegistry()
models = registry.list_models(model_type='tlv')

info = registry.get_model_info('tlv_14_tendrils')
# {'name': 'tendrils_figure_runs', 'type': 'tlv',
#  'strategy': 14, 'is_production': True, ...}

model = registry.load_model('tlv_14_tendrils')
```

Searches configured paths:
- `<refined>/candescence_new/`
- `<legacy-refined>/candescence_master/projects/tlv/`

#### `candescence.interface.components.LatentExplorer`

Interactive latent space visualization:

```python
from candescence.interface.components import LatentExplorer

explorer = LatentExplorer(model, embeddings, metadata_df)

# Set projection method
explorer.set_projection('umap', n_neighbors=15)

# Render in Streamlit (returns clicked index)
clicked_idx = explorer.render_streamlit(color_by='average_hue')

# Decode from 2D coordinates (weighted k-NN interpolation)
decoded = explorer.decode_from_2d(x=0.5, y=-0.3, k=5)
```

Features:
- UMAP, t-SNE, PCA projections
- Color by any metadata column
- Click events return sample indices
- Decode from arbitrary 2D coordinates using weighted k-NN

#### `candescence.interface.components.ImagePanel`

Image display with metadata:

```python
from candescence.interface.components import ImagePanel

panel = ImagePanel()
panel.display_image(image, metadata={'hue': 120, 'class': 'pseudohyphae'})
panel.display_comparison(real_image, generated_image)
bytes_data = panel.get_download_bytes(image, 'png')
```

#### `candescence.interface.components.InterpolationTool`

Latent space interpolation:

```python
from candescence.interface.components import InterpolationTool

tool = InterpolationTool(model, embeddings, conditioning)
tool.set_points(index_a=0, index_b=100)

# Generate interpolation
images = tool.interpolate(
    n_steps=7,
    conditioning_strategy='interpolate'  # or 'first', 'second', 'none'
)

# Display as filmstrip or slider
tool.display_filmstrip(images)
tool.display_slider(images)
```

### Data Quality Assessment

Analyzed metadata files for data cleaning requirements:

**File:** `<legacy-refined>/candescence_master/projects/tlv/data_files/Calb_Master_10062022_Labeled.tsv`

| Issue | Details |
|-------|---------|
| Missing value encoding | Inconsistent: NA, N/A, unknown, empty strings |
| Non-ASCII characters | Found in text columns |
| Duplicate keys | 36 duplicate Plate.Position values |
| Missing isolate types | ~48% unlabeled |

**File:** `<legacy-refined>/candescence_master/projects/tlv/data_files/manually_labelled_images.csv`

| Issue | Details |
|-------|---------|
| Unlabeled samples | 56% (19,188 of 34,176) have morphology="None" |
| Class imbalance | "smooth" dominant class |

### Data Preprocessing Utilities

Created `candescence.interface.data_utils` for cleaning:

```python
from candescence.interface import (
    standardize_missing_values,
    clean_non_ascii,
    filter_labeled_samples,
    load_and_clean_metadata,
    DataQualityReport
)

# Load and clean metadata
df = load_and_clean_metadata(
    "Calb_Master.tsv",
    label_column='morphology',
    filter_labeled=True
)

# Or run quality checks
report = DataQualityReport(df)
report.run_all_checks(
    key_columns=['Plate.Position'],
    label_column='morphology'
)
print(report.summary())
```

Features:
- Standardize missing values (NA, N/A, unknown → np.nan)
- Remove non-ASCII characters
- Filter labeled/unlabeled samples
- Compute class weights for imbalanced data
- Generate quality reports

### Main Application

Entry point: `src/candescence/interface/app.py`

Launch with:
```bash
streamlit run src/candescence/interface/app.py
```

Features:
- Model selection sidebar with registry integration
- Welcome page with system status
- Explorer page with LatentExplorer, ImagePanel, InterpolationTool
- Efficient caching with @st.cache_resource
- Automatic loading from inference pickle files

### Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `interface/__init__.py` | 32 | Package exports |
| `interface/base.py` | 100 | CandescenceModel ABC |
| `interface/model_loader.py` | 270 | TLVModelWrapper |
| `interface/data_utils.py` | 340 | Data preprocessing |
| `interface/app.py` | 290 | Streamlit application |
| `interface/components/__init__.py` | 16 | Component exports |
| `interface/components/latent_explorer.py` | 300 | LatentExplorer |
| `interface/components/image_panel.py` | 220 | ImagePanel |
| `interface/components/interpolation.py` | 277 | InterpolationTool |
| `interface/pages/__init__.py` | 10 | Page exports |
| `interface/pages/tlv_explorer.py` | 200 | TLVExplorerPage |
| `core/model_registry.py` | 200 | ModelRegistry |

**Total: ~2,255 lines of new code**

### Dependencies Added

```yaml
# Interface dependencies
streamlit>=1.28.0
streamlit-plotly-events>=0.0.6
plotly>=5.18.0
umap-learn>=0.5.4
scikit-learn>=1.3.0
```

### Constraints Respected

Per the original specification:
- ✅ Inference-only (no training code)
- ✅ No gradient computation (torch.no_grad context)
- ✅ No model saving functionality
- ✅ Separate module from training code
- ✅ Abstract interface for extensibility
- ✅ Accessible for non-expert users

### Next Steps

1. Test interface with actual TLV models
2. Add CLI entry point (`candescence interface launch`)
3. Add conditioning controls to explorer page
4. Integrate with Varasana detection for full pipeline
5. Add export functionality for latent embeddings
6. Performance optimization for large inference pickle files

---

## Phase 2 Continued: Standalone Latent Explorer App (2026-01-29)

### Overview

Created a standalone Streamlit application for interactive latent space exploration that can load models directly from candescence_new without depending on legacy pickle files from candescence_master.

### Key Improvements

| Improvement | Before | After |
|-------------|--------|-------|
| Model loading | Pickle files with dependencies (skbio, inference) | Direct loading via `load_tlv_model()` |
| Data source | candescence_master only | candescence_new models and raw images |
| Click events | streamlit-plotly-events (broken in v0.0.6) | Native Streamlit selection API |
| Skip connections | Manual reference image selection | Automatic k-NN weighted interpolation |

### New Loading Method: "From Model + Images"

The recommended data loading method that:
1. Loads trained models directly from candescence_new
2. Loads raw images from `<raw>/candescence_new/tlv_images/`
3. Computes embeddings on-the-fly
4. Supports conditioning from metadata files

Available models:
- `tutorial_tendril_vae/test_run_v1` - Tutorial training run
- `0_37_tendrils_reproduction/manuscript_v1` - 37 tendrils reproduction

### Application Architecture

```
latent_explorer_app.py
├── Data Loading
│   ├── From Model + Images (NEW - recommended)
│   ├── From Inference Object (legacy)
│   ├── From NPZ + Images
│   └── Demo Data
├── Visualization
│   ├── PCA / t-SNE / UMAP projections
│   ├── Color by metadata attributes
│   └── Interactive scatter plot with click events
├── Selection Panel
│   ├── Clicked Point (shows image immediately)
│   ├── Selection list (for interpolation)
│   └── Manual index/coordinate entry
└── Interpolation
    ├── Coordinate-based start/end points
    ├── k-NN weighted latent estimation
    └── Filmstrip display
```

### Technical Notes

**k-NN Decoding Explanation:**

When clicking on empty space in the latent projection, the app cannot directly decode because:
1. The 2D projection loses information (128D → 2D)
2. The Tendril VAE uses skip connections that require encoding a reference image

The k-NN approach:
1. Find k=5 nearest neighbors in 2D space
2. Compute inverse-distance weighted average of their latent vectors
3. Encode nearest neighbor to get valid skip connections
4. Decode using the interpolated latent vector

This is an approximation - the decoded image is influenced by the reference image's skip connections.

**Click Event Handling:**

Streamlit's native selection API (`on_select="rerun"`) is used for click handling. The event returns selected point indices which are processed to show the corresponding image in the right panel.

### Files Created/Modified

| File | Status | Purpose |
|------|--------|---------|
| `interface/apps/__init__.py` | Created | Apps package init |
| `interface/apps/latent_explorer_app.py` | Created | Standalone Streamlit app (~1100 lines) |
| `interface/apps/README.md` | Created | App documentation |
| `assets/candescence-logo.png` | Used | Logo display (3x size) |

### Tutorial Created

**File:** `tutorials/tutorial_latent_explorer.ipynb`

Covers:
1. Launching the standalone app
2. Loading data (Model + Images method)
3. Navigating the interface
4. Click-to-explore workflow
5. Interpolation between points
6. Understanding k-NN decoding

### Resolved Issues

| Issue | Resolution |
|-------|------------|
| "No module named 'skbio'" | Use "From Model + Images" instead of pickle files |
| "No module named 'inference'" | Direct model loading avoids pickle dependencies |
| Empty scatter plot | Removed streamlit-plotly-events, use native API |
| Click events not working | Using Streamlit >= 1.29 native selection |

### Launch Command

```bash
cd <repo>
streamlit run src/candescence/interface/apps/latent_explorer_app.py
```

### Dependencies

```yaml
streamlit>=1.29.0  # Native click events
plotly>=5.18.0
scikit-learn>=1.3.0
umap-learn>=0.5.4  # Optional
torch>=2.0.0
```

---

## Phase 7: Interactive Point Selection and Production Registry (2026-01-30 / 2026-01-31)

### Overview

Enhanced the latent explorer with interactive point selection for interpolation and added a production model/dataset registry system for streamlined model management.

### Key Improvements (2026-01-30)

**Contributor:** HarryZhang67

| Feature | Before | After |
|---------|--------|-------|
| Interpolation endpoints | Manual coordinate entry only | Click-to-select directly from plot |
| Point selection | Empty space clicking (k-NN approximation) | Must select existing points (accurate latent vectors) |
| Endpoint display | Decoded images only | Original + reconstructed side-by-side |

### Interactive Point Selection for Interpolation

Users can now select start and end points for interpolation directly by clicking on existing data points in the latent space scatter plot. This ensures accurate latent vectors are used rather than k-NN approximations from empty space.

**Workflow:**
1. Click a point in the scatter plot → added to selection list
2. Select "Start Point" and "End Point" from selection list
3. Generate interpolation with accurate skip connections

### Skip Connection Handling Improvements

Added explicit skip connection methods to `TLVModelWrapper`:

```python
class TLVModelWrapper(CandescenceModel):
    def get_last_skip(self) -> Optional[List[torch.Tensor]]:
        """Return cached skip connections from last encode call."""

    def decode_with_skip(
        self,
        z: torch.Tensor,
        skip: List[torch.Tensor],
        cond: Optional[torch.Tensor] = None
    ) -> torch.Tensor:
        """Decode with explicit skip connections (for interpolation)."""
```

This allows proper interpolation by:
1. Encoding both start and end images to get their skip connections
2. Interpolating latent vectors in z-space
3. Interpolating skip connections (weighted average)
4. Decoding with interpolated latents and skips

### Enhanced Interpolation Display

The interpolation section now shows:
- **Original images** from dataset (ground truth)
- **Reconstructed images** (encode→decode roundtrip)
- **Interpolation filmstrip** between the two endpoints

This helps users understand reconstruction fidelity vs latent interpolation quality.

### HSV Scale Standardization (2026-01-31)

Standardized all HSV values to use **storage scale (0-255)** consistent with PIL's HSV mode:

| Component | Storage Scale | Normalized Scale |
|-----------|---------------|------------------|
| Hue | 0-255 | 0-1 |
| Saturation | 0-255 | 0-1 |
| Value | 0-255 | 0-1 |

**Files updated:**
- `interface/apps/latent_explorer_app.py` - Demo data generation
- `interface/app.py` - Demo data generation
- `tlv/inference/inference.py` - Documentation clarification

### Production Registry (2026-01-31)

Created `candescence.core.ProductionRegistry` for centralized model and dataset management:

```python
from candescence.core.production_registry import ProductionRegistry

registry = ProductionRegistry()

# Register a production model
registry.register_model(
    model_id="tendril_vae_v1",
    model_path="<refined>/.../models/model.pth",
    name="Tendril VAE v1",
    description="Production model for day 2 images",
    version="1.0.0"
)

# List available models/datasets
models = registry.list_models()
datasets = registry.list_datasets()

# Get Streamlit-friendly choices
choices = registry.get_model_choices()
# [{'id': 'tendril_vae_v1', 'label': 'Tendril VAE v1 (v1.0.0)', 'description': '...'}]
```

**Features:**
- JSON-based manifests stored in `<refined>/candescence_new/`
- Model registration with automatic args.json parsing
- Dataset registration with image counting
- Export/import for registry backup
- Streamlit-friendly choice formatting

**Registry Files:**
- `production_models.json` - Registered production models
- `production_datasets.json` - Registered production datasets

### Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `core/production_registry.py` | 574 | ProductionRegistry class |

### Files Modified

| File | Changes |
|------|---------|
| `interface/apps/latent_explorer_app.py` | Point selection, interpolation display, HSV scale |
| `interface/model_loader.py` | `get_last_skip()`, `decode_with_skip()` methods |
| `interface/app.py` | HSV scale standardization |
| `tlv/inference/inference.py` | HSV documentation |

### Technical Notes

**Why require existing points for interpolation?**

The Tendril VAE uses skip connections from the encoder that capture image-specific structural information. When clicking on empty space:
1. The 2D projection loses 126 dimensions (128D → 2D)
2. k-NN interpolation approximates the latent vector but cannot recover accurate skip connections
3. The decoded image is heavily influenced by the reference image's structure

By requiring selection of existing points, we ensure:
1. Exact latent vectors from the dataset
2. Accurate skip connections from actual encoded images
3. Proper interpolation of both z-space and skip connections

### Next Steps

1. Integrate ProductionRegistry with Streamlit apps
2. Add model validation (verify files exist before loading)
3. ~~Create CLI commands for registry management~~ (Done - see below)
4. Add versioned model promotion workflow

---

## Phase 7 Continued: Production Workflow Documentation (2026-01-31)

### Overview

Added comprehensive documentation and tooling for the train-to-production workflow.

### Tutorial Created

**File:** `tutorials/tutorial_production_workflow.ipynb`

Step-by-step guide covering:
1. Configure experiment with `TLVConfig`
2. Train model with `Factory`
3. Validate quality with `Inference`
4. Register to `ProductionRegistry`
5. Load registered models from anywhere
6. Registry management (update, unregister, export/import)
7. Using registered models in Streamlit apps

### CLI Script Created

**File:** `scripts/production/register_model.py`

Command-line interface for model registration:

```bash
# Register by experiment/run name:
python scripts/production/register_model.py \
    --experiment tutorial_tendril_vae \
    --run test_run_v1 \
    --name "My Production Model" \
    --version 1.0.0

# List all registered models:
python scripts/production/register_model.py --list

# Show model details:
python scripts/production/register_model.py --info model_id

# Unregister (doesn't delete files):
python scripts/production/register_model.py --unregister model_id
```

### Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `tutorials/tutorial_production_workflow.ipynb` | ~400 | End-to-end production workflow tutorial |
| `scripts/production/register_model.py` | 185 | CLI for model registration |

### Production Workflow Summary

| Step | Action | Tool |
|------|--------|------|
| 1 | Configure | `TLVConfig(experiment_name, save_name)` |
| 2 | Train | `Factory.train_model()` |
| 3 | Validate | `Inference.run_inference()` |
| 4 | Register | `ProductionRegistry.register_model()` or CLI |
| 5 | Use | `registry.get_model_path(model_id)` |

### Key Paths

| Item | Location |
|------|----------|
| Model weights | `<refined>/candescence_new/{exp}/{run}/models/model.pth` |
| Model config | `<refined>/candescence_new/{exp}/{run}/models/args.json` |
| Model registry | `<refined>/candescence_new/production_models.json` |
| Dataset registry | `<refined>/candescence_new/production_datasets.json` |

### Architecture Diagram

**File:** `docs/architecture_diagram.md`

ASCII diagram showing:
- Directory structure (repository, raw, refined)
- Data flow between components
- Component interactions (training, inference, exploration workflows)
- Production registry flow
- Module dependencies within `src/candescence/`

---

## Phase 8: Streamlit Training App (2026-02-01)

### Overview

Created a guided Streamlit application for training Tendril VAE models that is accessible to non-expert users. The app provides a 3-step wizard (Configure → Train → Save) with real-time progress visualization and integration with the ProductionRegistry.

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Training interface | Streamlit wizard | Guided workflow for non-experts |
| Real-time updates | `st.empty()` placeholders | Update UI during training loop |
| GPU selection | nvidia-smi subprocess | Accurate real-time memory usage |
| Process priority | `os.nice(19)` | Low priority per lab conventions |
| Architecture | Locked to Tendril VAE (#14) | Most mature architecture |

### New Module Structure

```
src/candescence/interface/
├── training/                        # NEW: Training utilities module
│   ├── __init__.py                  # Module exports
│   ├── training_state.py            # TrainingState dataclass and manager
│   └── streamlit_trainer.py         # StreamlitTrainer with callbacks
├── components/
│   ├── training_config.py           # NEW: Configuration panel component
│   ├── training_progress.py         # NEW: Progress visualization component
│   └── training_summary.py          # NEW: Post-training summary component
└── apps/
    └── training_app.py              # NEW: Standalone training app (~800 lines)
```

### Core Classes Created

#### `candescence.interface.training.TrainingState`

Dataclass for training state management:

```python
@dataclass
class TrainingState:
    status: str  # idle | configuring | training | paused | completed | cancelled | error
    current_epoch: int
    total_epochs: int
    loss_history: List[Dict[str, Any]]
    best_epoch: int
    best_val_loss: float
    start_time: Optional[datetime]
    reconstruction_images: Optional[np.ndarray]
    training_log: List[str]
    final_model_path: Optional[Path]
```

#### `candescence.interface.training.TrainingStateManager`

Session state persistence with Streamlit:

```python
class TrainingStateManager:
    @staticmethod
    def initialize_session_state() -> None:
        """Initialize all training-related session state."""

    @staticmethod
    def update_epoch(epoch: int, metrics: Dict[str, Any]) -> None:
        """Update state after each epoch."""

    @staticmethod
    def to_training_state() -> TrainingState:
        """Convert session state to TrainingState dataclass."""
```

#### `candescence.interface.training.StreamlitTrainer`

Training wrapper with callback support:

```python
class StreamlitTrainer:
    def __init__(
        self,
        factory: Factory,
        progress_callback: Callable[[Dict[str, Any]], None],
        reconstruction_callback: Callable[[np.ndarray, int], None],
        stop_check: Callable[[], bool],
        pause_check: Callable[[], bool],
    )

    def train_with_callbacks(self) -> Dict[str, Any]:
        """Training loop with per-epoch callbacks."""

    def generate_reconstructions(self, indices: List[int]) -> np.ndarray:
        """Generate reconstruction grid for visualization."""

    def save_model(self, model_path: Path) -> Path:
        """Save trained model weights."""
```

#### `candescence.interface.components.TrainingConfigPanel`

Configuration UI with parameter validation:

```python
class TrainingConfigPanel:
    def render(self) -> Optional[Dict[str, Any]]:
        """Render configuration form with 5 sections."""

    @staticmethod
    def get_default_config() -> Dict[str, Any]:
        """Return default configuration values."""
```

**Configuration Sections:**
1. **Data Configuration**: Image directory, metadata path, image dimension, restrict to day
2. **Dataset Split**: Train/validation/test counts with validation
3. **Model Architecture**: Latent dimension, conditioning variables (locked to #14)
4. **Training Parameters**: Epochs, batch size, learning rates, loss weights
5. **Output Configuration**: Experiment name, save name with auto-generated defaults

#### `candescence.interface.components.TrainingProgressPanel`

Real-time training visualization:

```python
class TrainingProgressPanel:
    def render(
        self,
        loss_history: List[Dict],
        current_epoch: int,
        total_epochs: int,
        start_time: datetime,
        reconstruction_images: Optional[np.ndarray],
        training_log: List[str],
    ) -> Tuple[bool, bool, bool]:
        """Render progress display, return (should_stop, should_pause, should_cancel)."""
```

**Features:**
- Progress bar with ETA
- Loss curves (train/val) with Plotly
- Reconstruction grid (8 images: original + reconstructed)
- Scrollable training log
- Pause/Resume/Stop/Cancel controls

#### `candescence.interface.components.TrainingSummaryPanel`

Post-training summary and registration:

```python
class TrainingSummaryPanel:
    def render(
        self,
        final_metrics: Dict[str, Any],
        config: Dict[str, Any],
        reconstruction_images: Optional[np.ndarray],
        model_path: Path,
    ) -> Optional[Dict[str, Any]]:
        """Render summary with Research/Production registration choice."""
```

### TLVConfig Enhancements

Added `_set_training_defaults()` method with all training-related attributes:

```python
class TLVConfig:
    def _set_training_defaults(self) -> None:
        # Random seeds
        self.dataset_seed = 42
        self.training_seed = 9954

        # Dataset split sizes
        self.train_num = 1200
        self.validation_num = 400
        self.test_num = 400

        # Model architecture
        self.architecture = "tendril_vae"
        self.strategy = 14
        self.latent_dim = 128
        self.intermediate_dim = 256
        self.leaky_relu_slope = 0.02

        # Conditioning
        self.conditional_variables = ["average_hue"]
        self.conditional_decoder_fixed_values = {"average_hue": 0.5}

        # Training hyperparameters
        self.number_epochs = 100
        self.batch_size = 256
        self.vae_lr = 1e-4
        self.film_lr = 5e-4
        self.weight_decay = 1.5e-3

        # Loss weights
        self.kl_weight = 1.0
        self.mse_weight = 100.0
        self.lpips_weight = 10.0
        self.ssim_weight = 1.0
        self.conditional_loss_weight = 1000.0

        # System settings
        self.process_priority = 19
        self.num_threads = 10
```

### GPU Selection Feature

The training app includes intelligent GPU selection:

```python
def _get_gpu_info() -> List[Dict[str, Any]]:
    """Get GPU information via nvidia-smi subprocess."""
    # Returns: [{'id': 0, 'name': 'A100', 'used_mb': 1000, 'total_mb': 40000}, ...]

def _get_recommended_gpu() -> int:
    """Recommend GPU #4 if available with >50% of best GPU's free memory."""
```

**Features:**
- Real-time memory usage via `nvidia-smi`
- Dropdown with GPU details: "GPU 0: NVIDIA A100 (5,000 / 40,960 MB used)"
- Automatic recommendation of GPU #4 (lowest typical usage)
- Warning if no GPU available

### Process Priority

All training processes run with low priority per lab conventions:

```python
# At training start
os.nice(19)  # Lowest priority
```

### Launch Command

```bash
nice -n 19 streamlit run src/candescence/interface/apps/training_app.py
```

### UI Flow

```
┌─────────────────────────────────────────────────────────────┐
│  STEP 1: CONFIGURATION                                     │
│                                                             │
│  Sidebar:                    Main:                          │
│  - Step indicator            - Data Configuration panel     │
│  - GPU status + selector     - Dataset Split sliders        │
│  - Model info (locked #14)   - Training Parameters          │
│                              - Output Configuration          │
│                              - [Validate & Start] button     │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  STEP 2: TRAINING                                          │
│                                                             │
│  Sidebar:                    Main:                          │
│  - Current epoch             - Progress bar + ETA           │
│  - Best val loss             - Loss curves (live updating)  │
│  - Elapsed time              - Reconstruction grid          │
│  - [Pause] [Stop] [Cancel]   - Training log (scrollable)    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  STEP 3: SAVE                                              │
│                                                             │
│  Sidebar:                    Main:                          │
│  - Final metrics             - Training summary             │
│  - Save location             - Final reconstruction         │
│                              - Research/Production choice    │
│                              - Model metadata form           │
│                              - [Save Model] [Start New]      │
└─────────────────────────────────────────────────────────────┘
```

### Parameter Tooltips

Comprehensive tooltips for non-expert users:

| Parameter | Tooltip |
|-----------|---------|
| latent_dim | "Dimensionality of the latent space. Higher values capture more detail but may overfit. Typical: 64-256." |
| vae_lr | "Learning rate for VAE parameters. Controls how quickly the model learns. Start with 1e-4." |
| film_lr | "Learning rate for FiLM conditioning layers. Usually 5x higher than VAE LR." |
| kl_weight | "Beta parameter for KL divergence loss. Higher values encourage smoother latent space." |
| batch_size | "Number of images per training step. Higher is faster but needs more GPU memory." |

### Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `interface/training/__init__.py` | 22 | Module exports |
| `interface/training/training_state.py` | 180 | TrainingState and TrainingStateManager |
| `interface/training/streamlit_trainer.py` | 350 | StreamlitTrainer with callbacks |
| `interface/components/training_config.py` | 512 | TrainingConfigPanel |
| `interface/components/training_progress.py` | 300 | TrainingProgressPanel |
| `interface/components/training_summary.py` | 280 | TrainingSummaryPanel |
| `interface/apps/training_app.py` | 800 | Main training application |

**Total: ~2,444 lines of new code**

### Files Modified

| File | Changes |
|------|---------|
| `core/config.py` | Added `_set_training_defaults()` method |
| `interface/apps/__init__.py` | Added `training_main` export |
| `interface/components/__init__.py` | Added training component exports |

### Integration with Existing Infrastructure

- **Factory**: Uses existing `Factory.load_dataset()`, `Factory.prepare_vae()` for model setup
- **VAETrainer**: Adapts `training_epoch()` and `validation_epoch()` patterns
- **TLVConfig**: All paths and defaults from configuration class
- **ProductionRegistry**: Model registration for research/production classification

### Constraints Respected

Per lab conventions:
- ✅ Process priority 19 (nice mode)
- ✅ Max 1 GPU core usage
- ✅ GPU selection with recommendation
- ✅ snake_case naming throughout
- ✅ Type hints on all functions
- ✅ Proper docstrings (numpy style)
- ✅ No hardcoded paths (uses TLVConfig)

### Next Steps

1. End-to-end testing with actual training runs
2. GPU memory monitoring during training
3. Add model versioning in summary panel
4. Consider checkpoint saving during training
5. Add training resume capability

---

## Phase 8 Enhancements: HSV Standardization and Dataset Report (2026-02-01)

### Overview

Enhanced the Streamlit Training App with HSV standardization, image filtering, dataset preview reports, and fixed decoder conditioning UI.

### Key Changes

#### 1. HSV Scale Standardization (0-1)

All HSV values throughout the codebase now use a consistent 0-1 normalized scale:

| Component | Before | After |
|-----------|--------|-------|
| `compute_hsv_from_images()` | Mixed 0-255 | Returns 0-1 scale |
| `StreamlitTrainer._compute_image_hsv()` | 0-255 internal | Returns 0-1 scale |
| Dataset report histograms | x-axis 0-255 | x-axis 0-1 |
| Stats format | `.1f` | `.3f` (3 decimal places) |
| Conditioning sliders | N/A | 0-1 scale with tooltips |

**Circular mean for hue** preserved to handle hue's circular nature:
```python
hue_rad = hsv_array[:, 0] * 2 * np.pi / 255
avg_hue = np.arctan2(np.sin(hue_rad).mean(), np.cos(hue_rad).mean())
avg_hue = (avg_hue / (2 * np.pi)) % 1.0  # Normalize to 0-1
```

#### 2. Image Filtering by Day and Washed Status

Added filename parsing to extract experimental conditions:

```python
def _parse_image_filename(filename: str) -> Dict[str, Any]:
    """
    Filename patterns:
    - P11_control_day2_1-r1-c10.bmp -> day=2, washed=False
    - P11_control_day5_1-r1-c10.bmp -> day=5, washed=False
    - P11_control_wash_1-r1-c10.bmp -> day=None, washed=True
    """
```

**Filter options:**
- **Day filter**: All, Day 2 only, Day 5 only, Wash images only
- **Washed filter**: All, Not washed only, Washed only

**Live counter** updates as filters change, showing available images.

**Image counts by filter:**
| Filter | Count |
|--------|-------|
| Day 2 | 17,472 |
| Day 5 | 16,704 |
| Wash (no day) | 16,704 |
| Washed | 16,704 |
| Not washed | 34,176 |

#### 3. Dataset Report Component

New `DatasetReportPanel` component (`interface/components/dataset_report.py`):

```python
class DatasetReportPanel:
    def render(
        self,
        metadata_df: pd.DataFrame,
        train_indices: List[int],
        val_indices: List[int],
        test_indices: List[int],
        conditional_variables: List[str],
    ) -> None
```

**Features:**
- Sample counts per split (train/val/test)
- HSV distribution histograms with mean lines
- Summary statistics table (Mean, Std, Min, Max, Median)
- Split comparison checkbox for overlay histograms
- Conditioning variable info with decoder hints

**HSV computed directly from images** using `compute_hsv_from_images()` with sampling:
- Up to 500 train, 200 val, 200 test images sampled for preview
- Circular mean for hue values
- 12-pixel border extraction

#### 4. Fixed Decoder Conditioning UI

Added UI controls for `conditional_decoder_fixed_values`:

```python
use_fixed_decoder = st.checkbox("Enable fixed decoder conditioning", ...)
if use_fixed_decoder and cond_vars:
    fixed_decoder_values = {}
    for var in cond_vars:
        val = st.slider(f"Fixed {var_name}", min_value=0.0, max_value=1.0, ...)
        fixed_decoder_values[var] = val
    st.session_state["cfg_fixed_decoder_values"] = fixed_decoder_values
```

**Tooltips:**
| Variable | Help Text |
|----------|-----------|
| Hue | "0=red, 0.33=green, 0.66=blue. Typical agar: 0.1-0.3" |
| Saturation | "0=gray, 1=fully saturated" |
| Value | "0=black, 1=bright" |

#### 5. Batch Size Validation

Added warnings (not errors) for batch size vs. split size:

```python
if val_num < batch_size:
    st.warning(
        f"Validation set ({val_num}) is smaller than batch size ({batch_size}). "
        f"This is OK - PyTorch will use the incomplete batch."
    )
```

#### 6. Duplicate Key Error Fixes

Fixed `StreamlitDuplicateElementKey` errors:

- Added unique `key` parameters to all `st.plotly_chart()` calls
- Fixed rendering logic to prevent double-render of cached reports:

```python
if st.button("📊 Preview Dataset Report"):
    st.session_state["dataset_report_loaded"] = False  # Clear first
    _render_dataset_report(train_num, val_num, test_num)
elif st.session_state.get("dataset_report_loaded", False):
    _show_cached_dataset_report()  # Only if not just clicked
```

### Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `interface/components/dataset_report.py` | 425 | DatasetReportPanel and compute_hsv_from_images() |

### Files Modified

| File | Changes |
|------|---------|
| `interface/apps/training_app.py` | Image filtering, dataset report, fixed decoder UI, batch validation |
| `interface/components/__init__.py` | Added DatasetReportPanel export |
| `interface/training/streamlit_trainer.py` | HSV computation returns 0-1 scale |

### UI Changes

**Data Configuration section now includes:**
```
┌──────────────────────────────────────────────────────────────┐
│ Image Directory: [path input]                                │
│ Directory contains 50,880 total images                       │
├──────────────────────────────────────────────────────────────┤
│ Image Filters                                                │
│ ┌─────────────────┐  ┌─────────────────┐                    │
│ │ Day Filter      │  │ Washed Filter   │                    │
│ │ [Day 2 only ▼]  │  │ [Not washed ▼]  │                    │
│ └─────────────────┘  └─────────────────┘                    │
│                                                              │
│ ✓ 17,472 images available after applying filters            │
└──────────────────────────────────────────────────────────────┘
```

**Model Configuration section now includes:**
```
┌──────────────────────────────────────────────────────────────┐
│ Fixed Decoder Conditioning (for inference)                   │
│                                                              │
│ [✓] Enable fixed decoder conditioning                        │
│                                                              │
│ Fixed Hue: [────●────] 0.50                                  │
│ "0=red, 0.33=green, 0.66=blue. Typical agar: 0.1-0.3"       │
└──────────────────────────────────────────────────────────────┘
```

---

## Phase 9: Varasana Post-Training Evaluation (2026-02-04)

### Overview

Added comprehensive post-training evaluation features to the Varasana Training app, including performance metrics, ground truth vs predictions visualization, confusion matrix, and sample browser.

### Key Features Implemented

#### 1. Detection Evaluation Module

Created `src/candescence/detection/evaluation/` package with:

| File | Lines | Purpose |
|------|-------|---------|
| `__init__.py` | 25 | Package exports |
| `evaluator.py` | 400+ | DetectionEvaluator, EvaluationResults classes |
| `visualizer.py` | 350+ | PredictionVisualizer, SampleBrowser classes |

**Total: ~775 lines of new evaluation code**

#### 2. Performance Metrics Display

Added comprehensive metrics table to summary view:

| Metric | Description |
|--------|-------------|
| mAP | Mean Average Precision (COCO style) |
| AP50 | AP at IoU threshold 0.5 |
| AP75 | AP at IoU threshold 0.75 |
| Precision | Overall precision (TP / (TP + FP)) |
| Recall | Overall recall (TP / (TP + FN)) |
| F1 Score | Harmonic mean of precision/recall |

Plus per-class breakdown for all 15 morphology classes.

#### 3. Ground Truth vs Predictions Visualization

```
┌─────────────────────────────────────────────────────────────────┐
│  Ground Truth vs Predictions                                    │
│                                                                 │
│  [Sample: ◄ 1 of 20 ►]  [🎲 Random]  View: [Best/Worst/Random] │
│                                                                 │
│  ┌─────────────────────┐  ┌─────────────────────┐              │
│  │                     │  │                     │              │
│  │   Ground Truth      │  │   Prediction        │              │
│  │   [Solid boxes]     │  │   [Dashed boxes]    │              │
│  │                     │  │                     │              │
│  └─────────────────────┘  └─────────────────────┘              │
│                                                                 │
│  Metrics: IoU: 0.82  TP: 5  FP: 1  FN: 0                       │
└─────────────────────────────────────────────────────────────────┘
```

Features:
- Side-by-side image comparison
- Color-coded bounding boxes by class
- Sample navigation (random, best IoU, worst IoU)
- Per-image metrics display

#### 4. Sample Browser

Browse samples by category:
- 🎲 **Random Samples**: Random selection from evaluation set
- ✅ **Best Predictions**: Highest average IoU
- ❌ **Worst Predictions**: Lowest average IoU
- ⚠️ **False Positives**: Images with most FP
- 🔍 **False Negatives**: Images with most missed detections

Each sample shows:
- Ground truth vs prediction visualization
- Detailed metrics (IoU, TP, FP, FN counts)
- Matched detection pairs with confidence scores

#### 5. Confusion Matrix Visualization

```
┌─────────────────────────────────────────────────────────────────┐
│  Confusion Matrix (Normalized)                                  │
│                                                                 │
│  [✓] Normalize by row                                          │
│                                                                 │
│     Predicted Class →                                           │
│  GT ┌────────────────────────────────────────────┐              │
│  ↓  │   W    O    G    S    P    H              │              │
│     │ W 0.92 0.05 0.02 0.01 0.00 0.00          │              │
│     │ O 0.03 0.89 0.05 0.02 0.01 0.00          │              │
│     │ ...                                       │              │
│     └────────────────────────────────────────────┘              │
└─────────────────────────────────────────────────────────────────┘
```

Interactive Plotly heatmap with:
- Optional normalization by ground truth class
- Filtering to active classes
- Hover tooltips with values

#### 6. Per-Class Precision/Recall Chart

Grouped bar chart showing:
- Precision per class
- Recall per class
- F1 score per class

#### 7. Enhanced Model Saving

Model saving now includes evaluation metrics:
- Saves mAP, AP50, AP75, precision, recall, F1 with model
- Metrics available in Model Registry for comparison
- Research vs Production path selection

### Core Classes Created

#### `DetectionEvaluator`

```python
from candescence.detection.evaluation import DetectionEvaluator

evaluator = DetectionEvaluator(num_classes=15, class_names=CLASS_NAMES)

# Add predictions and ground truths
evaluator.add_predictions(image_path, predictions)
evaluator.add_ground_truths(image_path, ground_truths)

# Compute metrics
results = evaluator.evaluate()
print(f"mAP: {results.mAP:.3f}")
print(f"Precision: {results.precision:.3f}")
print(f"Recall: {results.recall:.3f}")
```

#### `EvaluationResults`

Dataclass containing:
- Overall metrics (mAP, precision, recall, F1)
- Per-class metrics
- Confusion matrix (numpy array)
- Per-image evaluations (for browsing)
- Helper methods for sample selection

#### `PredictionVisualizer`

```python
from candescence.detection.evaluation import PredictionVisualizer

viz = PredictionVisualizer(class_names=CLASS_NAMES)

# Draw annotations
gt_image = viz.draw_ground_truth(image, ground_truths)
pred_image = viz.draw_predictions(image, predictions)

# Side-by-side comparison
gt_img, pred_img = viz.draw_comparison(image, ground_truths, predictions, matches)
```

#### `SampleBrowser`

```python
from candescence.detection.evaluation import SampleBrowser

browser = SampleBrowser(results, image_dir=Path("/data/..."))

# Get samples by category
best = browser.get_best_predictions(n=10)
worst = browser.get_worst_predictions(n=10)
fp_samples = browser.get_false_positive_samples(n=10)
fn_samples = browser.get_false_negative_samples(n=10)

# Get visualization data
sample_data = browser.get_sample_with_visualization(evaluation)
```

### UI Changes

The summary step now uses a tabbed interface:

```
┌─────────────────────────────────────────────────────────────────┐
│  [📊 Overview] [🎯 GT vs Predictions] [📈 Metrics]              │
│  [🔍 Sample Browser] [💾 Save Model]                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Tab content here...                                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `detection/evaluation/__init__.py` | 25 | Package exports |
| `detection/evaluation/evaluator.py` | 400+ | Evaluation metrics computation |
| `detection/evaluation/visualizer.py` | 350+ | Visualization utilities |

### Files Modified

| File | Changes |
|------|---------|
| `interface/pages/3_Varasana_Training.py` | Added evaluation imports, tabbed summary UI, GT vs predictions, sample browser, confusion matrix |

### Requirements Met

Per Section 3 of the specification:

| Requirement | Status |
|-------------|--------|
| mAP (Mean Average Precision) | ✅ Implemented |
| AP50 (IoU @ 0.5) | ✅ Implemented |
| AP75 (IoU @ 0.75) | ✅ Implemented |
| AP per class | ✅ Implemented |
| Precision | ✅ Implemented |
| Recall | ✅ Implemented |
| F1 Score | ✅ Implemented |
| GT vs Predictions side-by-side | ✅ Implemented |
| Random test samples view | ✅ Implemented |
| Best predictions (highest IoU) | ✅ Implemented |
| Worst predictions (lowest IoU) | ✅ Implemented |
| False positives view | ✅ Implemented |
| False negatives view | ✅ Implemented |
| Confusion matrix | ✅ Implemented |
| Per-class precision/recall chart | ✅ Implemented |
| Research/Production model saving | ✅ Implemented |

### Next Steps

1. Integrate with actual MMDETECTION inference for live evaluation
2. Add export functionality for evaluation reports (PDF/CSV)
3. Add model comparison dashboard
4. Implement evaluation caching for large datasets

---

## Phase 10: Unified Model Zoo and Data Migration (2026-02-05)

### Overview

Created a unified "model zoo" to manage all production models and datasets across Candescence subprojects (Varasana, TLV, Grace). Migrated essential Varasana training data from `candescence_master` to `candescence_new` using a copy-essential + symlink strategy.

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Zoo layout | Flat with metadata registry | Simple to navigate, JSON registry for programmatic access |
| Image migration | Symlinks to candescence_master | Avoids duplicating ~93GB of image data |
| Essential data | Copy PKLs, config, model.pth | Small files (~400MB each) that should live alongside the project |
| Registry consolidation | Replace ProductionRegistry + ModelRegistry | Two competing systems → one unified zoo |

### Data Migration

#### Zoo Directory Structure

```
<refined>/candescence_new/zoo/
├── registry.json                    # Unified model registry (4 models)
├── datasets.json                    # Unified dataset registry (3 datasets)
├── varasana_fcos_v1/                # Production FCOS model
│   ├── model.pth                    # Trained weights (~390 MB)
│   └── config.py                    # MMDETECTION configuration
├── grace_tc_v1/                     # Grace cell classifier
│   ├── tc_model.pth                 # Trained weights (~390 MB)
│   └── tc_config.py                 # Configuration
└── grace_macro_v1/                  # Grace macro classifier
    ├── macro_model.pth              # Trained weights (~391 MB)
    └── macro_config.py              # Configuration
```

Note: `tendril_vae_v1` is registered but stored at its existing location
(`<refined>/candescence_new/training_20260201/run_162807/models/`).

#### Varasana Dataset Structure

```
<refined>/candescence_new/varasana_data/
├── curriculum/                      # COPIED: 12 PKL files (~1.1 MB total)
│   ├── train_white.pkl ... train_hyphae.pkl
│   └── val_white.pkl   ... val_hyphae.pkl
├── annotations/                     # COPIED: Labelbox annotations
│   └── varasana.json                # (~5.8 MB)
├── images/                          # SYMLINK → .../candescence_master/projects/varasana/train-data/train/
├── test_images/                     # SYMLINK → .../candescence_master/projects/varasana/test/
└── all_original_images/             # SYMLINK → .../candescence_master/projects/varasana/all-original-images/
```

### Unified ModelZoo Class

**File:** `src/candescence/core/model_zoo.py`

Replaces both `ProductionRegistry` (core/) and `ModelRegistry` (interface/core/):

```python
from candescence.core.model_zoo import ModelZoo

zoo = ModelZoo()
models = zoo.list_models(project="varasana")
entry = zoo.get("varasana_fcos_v1")
print(entry.get_checkpoint_path())
```

**Key features:**
- `ZooEntry` dataclass: id, name, project, model_type, version, architecture, checkpoint, config_file, path, metrics, tags, training_config
- `ModelZoo` class: register(), get(), list_models(), remove(), promote_to_production(), get_summary(), get_projects()
- Filtering by project (varasana, tlv, grace) and model_type (research, production)
- Registry stored as `zoo/registry.json`

### Unified DatasetZoo Class

**File:** `src/candescence/core/dataset_zoo.py`

```python
from candescence.core.dataset_zoo import DatasetZoo

zoo = DatasetZoo()
datasets = zoo.list_datasets(project="varasana")
entry = zoo.get("varasana_curriculum")
print(entry.path)
```

**Key features:**
- `DatasetEntry` dataclass: id, name, project, path, num_samples, splits, format, classes, description, metadata
- `DatasetZoo` class: register(), get(), list_datasets(), remove(), get_summary(), get_projects()
- Registry stored as `zoo/datasets.json`

### Registered Models

| Model ID | Project | Architecture | Description |
|----------|---------|-------------|-------------|
| varasana_fcos_v1 | varasana | fcos_resnet101_fpn | Production FCOS detector (15 classes, curriculum trained) |
| tendril_vae_v1 | tlv | tendril_vae | Production Tendril VAE (strategy 14, latent_dim=128) |
| grace_tc_v1 | grace | fcos_resnet101_fpn | Grace cell-level classifier (Cowen Lab) |
| grace_macro_v1 | grace | fcos_resnet101_fpn | Grace macroscopic classifier (Cowen Lab) |

### Registered Datasets

| Dataset ID | Project | Samples | Format | Description |
|------------|---------|---------|--------|-------------|
| varasana_curriculum | varasana | 1,096 | curriculum_pkl | Curriculum learning PKLs (6 stages, train/val splits) |
| varasana_test | varasana | 900 | image_dir | Independent test set (BMP images) |
| varasana_annotations | varasana | — | labelbox_json | Original Labelbox annotations (varasana.json) |

### Interface Pages Updated

#### 0_Home.py
- Switched from `ModelRegistry`/`DatasetRegistry` to `ModelZoo`/`DatasetZoo`
- Dashboard now shows models and datasets from unified zoo

#### 3_Varasana_Training.py
- Updated `KNOWN_DATA_LOCATIONS` to prioritize candescence_new paths
- New candescence_new curriculum path checked first

#### 4_Model_Registry.py
- Fully rewritten to use `ModelZoo` instead of old `ModelRegistry`
- Dynamic project filter from `zoo.get_projects()`
- Shows checkpoint file existence status and architecture info

#### 5_Dataset_Manager.py
- Fully rewritten to use `DatasetZoo`
- Registration form with dataset_id and format fields
- Verify function scans for BMP files (important for Varasana)

### Population Script

**File:** `scripts/populate_zoo.py`

One-time script to register all known models and datasets:

```bash
python scripts/populate_zoo.py
```

Output:
```
=== Populating Model Zoo ===
Registered 4 models in zoo
  varasana_fcos_v1: Varasana FCOS v1 [varasana/production] ✅
  tendril_vae_v1: Tendril VAE v1 [tlv/production] ✅
  grace_tc_v1: Grace Cell Classifier v1 [grace/production] ✅
  grace_macro_v1: Grace Macro Classifier v1 [grace/production] ✅

=== Populating Dataset Zoo ===
Registered 3 datasets in zoo
  varasana_curriculum: Varasana Curriculum Training Data [varasana] (1096 samples) ✅
  varasana_test: Varasana Test Images [varasana] (900 samples) ✅
  varasana_annotations: Varasana Labelbox Annotations [varasana] (0 samples) ✅
```

### Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `src/candescence/core/model_zoo.py` | ~300 | Unified model registry |
| `src/candescence/core/dataset_zoo.py` | ~260 | Unified dataset registry |
| `scripts/populate_zoo.py` | ~210 | Zoo population script |

### Files Modified

| File | Changes |
|------|---------|
| `interface/pages/0_Home.py` | Use ModelZoo/DatasetZoo for dashboard |
| `interface/pages/3_Varasana_Training.py` | Updated KNOWN_DATA_LOCATIONS |
| `interface/pages/4_Model_Registry.py` | Rewritten to use ModelZoo |
| `interface/pages/5_Dataset_Manager.py` | Rewritten to use DatasetZoo |

### Registry Consolidation

| Before | After |
|--------|-------|
| `ProductionRegistry` (core/production_registry.py) | `ModelZoo` (core/model_zoo.py) |
| `ModelRegistry` (interface/core/model_registry.py) | `ModelZoo` (core/model_zoo.py) |
| `production_models.json` (refined root) | `zoo/registry.json` |
| `production_datasets.json` (refined root) | `zoo/datasets.json` |

### Next Steps

1. ~~Wire up Varasana training page to select datasets from DatasetZoo~~ ✅ Done (Phase 10b)
2. ~~Add dataset selector for evaluation (select test set from zoo)~~ ✅ Done (Phase 10b)
3. ~~Add model comparison across zoo entries~~ ✅ Done (Phase 10b)
4. ~~Deprecate old ProductionRegistry and ModelRegistry~~ ✅ Done (Phase 10b)

---

## Phase 10b: Zoo Integration, Model Comparison, and Registry Deprecation (2026-02-05)

### Overview

Extended the zoo integration into all training pages, added model comparison,
and deprecated the three old registry systems in favor of the unified `ModelZoo`
and `DatasetZoo`.

### Changes

#### 1. DatasetZoo Selector in Varasana Training

The Varasana training page (`3_Varasana_Training.py`) now offers a **"From Dataset Zoo"**
radio option alongside manual path entry. Users can select from registered Varasana
datasets with sample counts and path verification.

#### 2. Test Dataset Selector for Evaluation

The GT vs Predictions tab now includes a test dataset selector from the zoo, allowing
users to choose which registered test set to run evaluation against.

#### 3. Model Comparison (Model Registry Page)

Added a **comparison mode** to `4_Model_Registry.py`:
- Toggle "Enable model comparison" checkbox
- Multi-select models to compare
- Side-by-side table showing: project, type, version, architecture, all metrics,
  and all training config keys

#### 4. Model Saving Uses ModelZoo

Both training apps now register models directly into the unified `ModelZoo`:
- `3_Varasana_Training.py` `_save_model()` → `ModelZoo.register()`
- `interface/components/training_summary.py` → `ModelZoo.register()`
- `interface/apps/training_app.py` → `ModelZoo.register()`

#### 5. Old Registries Deprecated

Four old registry files moved to `obsolete/deprecated_registries/`:

| Old File | Replacement |
|----------|-------------|
| `core/production_registry.py` | `core/model_zoo.py` |
| `core/model_registry.py` | `core/model_zoo.py` |
| `interface/core/model_registry.py` | `core/model_zoo.py` |
| `interface/core/dataset_registry.py` | `core/dataset_zoo.py` |

All imports across the codebase updated. Old JSON registry files listed in
`src/ready_to_delete.md` for cleanup after 1 week.

#### 6. Shared Components Updated

- `interface/core/components.py` `render_model_selector()` → uses `ModelZoo`
- `interface/core/components.py` `render_dataset_selector()` → uses `DatasetZoo`
- `interface/core/__init__.py` → exports `ModelZoo`, `ZooEntry`, `DatasetZoo`, `DatasetEntry`
- `interface/app.py` `get_registry()` → returns `ModelZoo` instance
- `scripts/production/register_model.py` → uses `ModelZoo` CLI

### Files Modified

| File | Changes |
|------|---------|
| `interface/pages/3_Varasana_Training.py` | DatasetZoo selector, test dataset selector, ModelZoo for saving |
| `interface/pages/4_Model_Registry.py` | Model comparison feature |
| `interface/core/components.py` | ModelZoo/DatasetZoo imports |
| `interface/core/__init__.py` | Export new zoo classes |
| `interface/app.py` | ModelZoo in get_registry() |
| `interface/components/training_summary.py` | ModelZoo for production registration |
| `interface/apps/training_app.py` | ModelZoo for production registration |
| `scripts/production/register_model.py` | Rewritten for ModelZoo CLI |
| `src/ready_to_delete.md` | Track deprecated files |

### Files Moved to Obsolete

| Source | Destination |
|--------|-------------|
| `src/candescence/core/production_registry.py` | `obsolete/deprecated_registries/production_registry.py` |
| `src/candescence/core/model_registry.py` | `obsolete/deprecated_registries/core_model_registry.py` |
| `src/candescence/interface/core/model_registry.py` | `obsolete/deprecated_registries/interface_model_registry.py` |
| `src/candescence/interface/core/dataset_registry.py` | `obsolete/deprecated_registries/interface_dataset_registry.py` |

### Next Steps

1. End-to-end testing of Varasana training with zoo dataset selection
2. Wire actual MMDETECTION inference for evaluation
3. Add zoo-based dataset selection to TLV training app
4. Clean up deprecated JSON files after 1-week waiting period

---

## Phase 11: Unified Interface Polish (2026-02-05)

### Overview

Polished the unified Candescence Streamlit interface with consistent theming,
proper navigation, Home dashboard as landing page, and branding with lab/GitHub links.

### Key Changes

#### 1. Home Dashboard as Landing Page

Replaced the TLV Explorer splash page with a Home dashboard containing:
- Candescence logo (large, 450px)
- Title and subtitle
- "From the Hallett Lab" with clickable lab logo (SVG, base64-encoded)
- "Candescence @ GitHub" link to the repository
- Quick Start navigation cards (TLV Training, TLV Explorer, Varasana Training)
- Model Zoo and Dataset overview panels from unified zoo
- Recent Activity feed

**Architecture:** Created `interface/home.py` as an importable module used by both
`app.py` (main entry) and `pages/0_Home.py` (sidebar navigation entry).

#### 2. Dark Sidebar Theme

Changed sidebar from light gray (`#f5f5f5`) to dark navy (`#1E2A3A`) with:
- Light text (`#D0D8E0`) for general content
- White (`#FFFFFF`) for headers and active page links
- Muted blue (`#B0BEC5`) for navigation links with white hover
- Dark dividers (`#3A4A5C`)
- Muted captions (`#8899AA`)

#### 3. Sidebar Logo

Candescence logo rendered at the top of the sidebar via `apply_theme()` using
`use_container_width=True` to fill the sidebar width responsively.

#### 4. Navigation Fixes

- Created `pages/0_Home.py` so sidebar shows "Home" instead of "app"
- Added CSS to hide the default "app" entry from sidebar nav
- Moved `pages/tlv_explorer.py` to `interface/tlv_explorer.py` to prevent
  Streamlit from auto-discovering it as a duplicate "tlv explorer" page

#### 5. Font Size and Readability

- Bumped main content area base font size to 1.1rem via CSS
- Lab and GitHub link text uses `color: #222 !important; font-weight: 600`
  for strong contrast

#### 6. TLV Casing

Fixed inconsistent casing: "tlv" → "TLV" in user-visible labels.
Added `format_func=str.upper` to project selectbox in Dataset Manager.

### Files Created

| File | Purpose |
|------|---------|
| `src/candescence/interface/home.py` | Home dashboard rendering module |
| `src/candescence/interface/pages/0_Home.py` | Sidebar "Home" entry (delegates to home.py) |
| `assets/hallett-lab-logo.svg` | Hallett Lab logo (downloaded from lab website) |

### Files Modified

| File | Changes |
|------|---------|
| `interface/core/theme.py` | Dark sidebar, sidebar logo, CSS for hiding "app", font size bump |
| `interface/app.py` | Rewritten to render Home dashboard |
| `interface/pages/__init__.py` | Updated import path for tlv_explorer |
| `interface/pages/5_Dataset_Manager.py` | TLV casing fix |

### Files Moved

| From | To | Reason |
|------|-----|--------|
| `interface/pages/tlv_explorer.py` | `interface/tlv_explorer.py` | Prevent Streamlit auto-discovery as duplicate page |
