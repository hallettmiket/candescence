#!/usr/bin/env bash
#
# Build the isolated legacy MMDetection environment used by the Varasana and
# Grace FCOS detectors (candescence.detection.service). The main application runs
# on a modern torch stack that is incompatible with mmcv-full 1.2.5, so the
# detectors run in this separate, pinned venv via a subprocess worker.
#
# CPU-only: the legacy CUDA 10.2 stack predates this lab's A100 GPUs (sm_80
# needs CUDA 11+); single-image FCOS inference on CPU is fine for interactive use.
#
# Usage:   bash setup_legacy_env.sh [ENV_DIR]
#          (default ENV_DIR = <repo>/.venv-legacy)
# Then:    export CANDESCENCE_LEGACY_PYTHON="$ENV_DIR/bin/python"
#
set -euo pipefail

ENV="${1:-$(git rev-parse --show-toplevel)/.venv-legacy}"
PY="$ENV/bin/python"

echo ">>> Creating legacy detection env at: $ENV"
uv venv --python 3.8 "$ENV"

# Order matters: numpy must be pinned to 1.19.5 FIRST. mmcv-full 1.2.5 / mmdet 2.10
# use np.int / np.float, which were removed in numpy>=1.24, and the mmcv C
# extension is built against the 1.19 ABI.
echo ">>> [1/4] numpy 1.19.5"
uv pip install --python "$PY" "numpy==1.19.5"

echo ">>> [2/4] torch 1.7.1+cpu / torchvision 0.8.2+cpu"
uv pip install --python "$PY" "torch==1.7.1+cpu" "torchvision==0.8.2+cpu" \
  --extra-index-url https://download.pytorch.org/whl/cpu \
  --index-strategy unsafe-best-match

echo ">>> [3/4] mmcv-full 1.2.5 (cpu / torch1.7.0 prebuilt wheels)"
uv pip install --python "$PY" "mmcv-full==1.2.5" \
  -f https://download.openmmlab.com/mmcv/dist/cpu/torch1.7.0/index.html \
  --index-strategy unsafe-best-match

# mmdet 2.10 hard-depends on mmpycocotools (an old fork whose sdist no longer
# builds). Install mmdet without deps and supply numpy-1.19-compatible runtime
# deps using the standard pycocotools wheel. The worker shims
# pycocotools.__version__ so mmdet's import-time version assert passes.
echo ">>> [4/4] mmdet 2.10.0 (no deps) + runtime deps"
uv pip install --python "$PY" --no-deps "mmdet==2.10.0"
# setuptools is required at runtime (torch.utils.cpp_extension imports it); the
# pinned numpy must be repeated so the other deps don't pull it forward.
uv pip install --python "$PY" "numpy==1.19.5" "pycocotools" "matplotlib==3.3.4" \
  "terminaltables" "six" "setuptools" "wheel"

echo ">>> Verifying import + API..."
"$PY" - <<'PYEOF'
import pycocotools
if not hasattr(pycocotools, "__version__"):
    pycocotools.__version__ = "12.0.2"
import torch, mmcv, mmdet
from mmdet.apis import init_detector, inference_detector  # noqa: F401
print("legacy env OK: torch", torch.__version__,
      "| mmcv", mmcv.__version__, "| mmdet", mmdet.__version__)
PYEOF

echo
echo ">>> Done. Use it with:"
echo "      export CANDESCENCE_LEGACY_PYTHON=\"$PY\""
