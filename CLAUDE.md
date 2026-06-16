# Candescence New - Claude Code Documentation

## Conventions

- **CC** refers to **Claude Code** (this CLI agent) throughout this repo's docs.
- **"Outstanding requests / jobs / tasks"** always means *this project's*
  pending work, tracked in the project-specific memory file
  `outstanding-jobs.md` (see the memory index `MEMORY.md`). It does **not**
  mean the wigamig receptionist's cross-group SEA inbound queue. When asked to
  "check / remember outstanding requests," CC reads and updates that project
  memory file — scoped to candescence only.

## Project Overview

**candescence_new** is an optimized and extended version of the Candescence project for *Candida albicans* morphology analysis. This repository builds upon candescence_master without modifying the original code.

## Repository Structure

```
candescence_new/
├── exp/                    # Experiments (format: {number}_{descriptive_name})
├── src/                    # Shared code and utilities
│   ├── init.py            # Python configuration class
│   ├── init.R             # R configuration script
│   └── ready_to_delete.md # Track files ready for deletion
├── obsolete/              # Deprecated code (pending deletion)
├── data/                  # Small data files only (< 1MB)
├── README.md
├── CLAUDE.md
└── .gitignore
```

## Data Paths

All paths resolve through the settings layer
(`src/candescence/core/settings.py`), with this precedence: explicit argument →
environment variable → `candescence.toml` → packaged default. Override any path
without editing code:

| Type | Env var | Packaged default |
|------|---------|------------------|
| Raw data | `CANDESCENCE_RAW` | `<lab raw>/candescence_new/` |
| Refined data | `CANDESCENCE_REFINED` | `<lab refined>/candescence_new/` |
| Images | `CANDESCENCE_IMAGES` | `<raw>/tlv_images/all-final/` |
| Model zoo | `CANDESCENCE_ZOO` | `<refined>/zoo/` |

See `candescence.toml.example` and `docs/tutorials/02_using_your_own_images.md`.

## Experiment Naming Convention

Experiments follow the pattern: `{number}_{descriptive_name}`
- `0_baseline` - First experiment
- `1_optimization` - Second experiment
- etc.

Each experiment in `exp/{number}_{name}/` has corresponding data under the
refined tree: `<refined>/candescence_new/{number}_{name}/`

## Configuration Usage

### Python
```python
from src.init import Init

config = Init(experiment_name="0_baseline")
print(config.refined_path)               # <refined>/candescence_new
print(config.get_experiment_refined_path())  # <refined>/candescence_new/0_baseline
```

### R
```r
source("src/init.R")
print(REFINED_PATH)  # <refined>/candescence_new (or $CANDESCENCE_REFINED)
print(get_experiment_refined_path("0_baseline"))
```

## Morphology Grades

The system classifies *Candida* morphologies into 6 grades:
1. white
2. opaque
3. gray
4. shmoo
5. pseudohyphae
6. hyphae

## Code Style

- Python: snake_case, type hints required, use pathlib
- R: tidyverse style, `<-` assignment
- All functions need docstrings/roxygen documentation
- Use logging module (Python) instead of print()
- No hardcoded absolute paths (use Init class)

## Entry Points

- Each experiment should have `run_all.py` or `run_all.ipynb` as the starting point
- Include `readme.md` in each experiment explaining purpose and parameters

## Key Technologies

From candescence_master:
- PyTorch (deep learning)
- MMDETECTION (object detection)
- VAE architectures (variational autoencoders)
- tidyverse/ggplot2 (R analysis)

## File Deletion Protocol

1. Mark files in `src/ready_to_delete.md`
2. Wait at least 1 week
3. Delete and update the log
