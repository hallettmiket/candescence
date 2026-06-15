# Legacy detection environment (Varasana / Grace)

The Varasana (Bettauer et al. 2022) and Grace (Case, Westman et al. 2023) FCOS
detectors were built on **MMDetection** with a pinned stack:

| Package | Version |
|---|---|
| Python | 3.8 |
| numpy | 1.19.5 |
| torch / torchvision | 1.7.1+cpu / 0.8.2+cpu |
| mmcv-full | 1.2.5 |
| mmdet | 2.10.0 |
| pycocotools | (wheel; `__version__` shimmed) |
| matplotlib | 3.3.4 |

This is **incompatible** with Candescence's main environment (torch 2.x / numpy
≥ 2). Rather than reimplement or retrain the models, we keep them on their
original stack in an **isolated venv** and call them out-of-process
(Strategy A). The main app never imports `mmcv` / `mmdet`.

## CPU-only, by design

The legacy stack targets CUDA 10.2, which cannot drive this lab's A100 GPUs
(sm_80 needs CUDA 11+). Inference therefore runs on **CPU** — fine for the
interactive, one-image-at-a-time GUI use case. Reusing the published v1.0
weights unchanged keeps results faithful to the papers.

## Build it

```bash
bash src/candescence/detection/legacy/setup_legacy_env.sh   # -> <repo>/.venv-legacy
export CANDESCENCE_LEGACY_PYTHON="$(git rev-parse --show-toplevel)/.venv-legacy/bin/python"
```

`.venv-legacy/` is git-ignored. The path is also overridable via
`$CANDESCENCE_LEGACY_PYTHON` (e.g. to share one env across checkouts).

## How it is used

`candescence.detection.service.client.detect(config, checkpoint, images)` writes
a JSON request, runs `service/worker.py` under the legacy interpreter as a
subprocess, and parses the returned detections — see that package's docstrings.

Production configs + weights live (read-only) under the legacy refined tree:

| Model | config | checkpoint |
|---|---|---|
| Varasana | `production/varasana/config.py` | `production/varasana/model.pth` |
| Grace macro | `production/grace_macro/macro_config.py` | `production/grace_macro/macro_model.pth` |
| Grace TC | `production/grace_tc/tc_config.py` | `production/grace_tc/tc_model.pth` |

## Notes / gotchas

- **numpy must be pinned to 1.19.5 first.** mmcv/mmdet 2.x use `np.int` /
  `np.float` (removed in numpy ≥ 1.24); the mmcv C extension is built against the
  1.19 ABI. Newer matplotlib/pycocotools will try to pull numpy forward.
- **`mmpycocotools` does not build** (broken sdist). We use standard
  `pycocotools` and shim `pycocotools.__version__ = "12.0.2"` before importing
  mmdet (which asserts that version). Inference uses only bounding boxes, so the
  COCO-eval differences are irrelevant.
- Subprocess-per-call reloads the model each invocation (~seconds). If that
  becomes a bottleneck in the GUI, promote the worker to a persistent process.
