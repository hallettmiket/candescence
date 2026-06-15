"""
Purpose: Tests for the isolated legacy-detection service client.
Author: Hallett Lab
Date: 2026-06-15
Input: None (unit tests); the integration test is skipped unless the legacy env
       and production Varasana weights are present.
Output: Pytest assertions.
"""

import os
from pathlib import Path

import pytest

from candescence.detection.service import client


def test_legacy_python_path_env_override(monkeypatch):
    """The interpreter path honours $CANDESCENCE_LEGACY_PYTHON."""
    monkeypatch.setenv(client.ENV_LEGACY_PYTHON, "/some/where/python")
    assert client.legacy_python_path() == Path("/some/where/python")


def test_legacy_python_path_default(monkeypatch):
    """Without the override, the default is repo-local .venv-legacy."""
    monkeypatch.delenv(client.ENV_LEGACY_PYTHON, raising=False)
    path = client.legacy_python_path()
    assert path.name == "python"
    assert path.parent.parent.name == ".venv-legacy"


def test_detect_missing_interpreter_raises(tmp_path, monkeypatch):
    """A missing legacy interpreter raises a clear LegacyWorkerError."""
    monkeypatch.setenv(client.ENV_LEGACY_PYTHON, str(tmp_path / "nope" / "python"))
    with pytest.raises(client.LegacyWorkerError, match="not found"):
        client.detect("cfg.py", "model.pth", ["img.png"])


# --- Integration: only when the legacy env + production weights are present ---
_LEGACY_PY = client.legacy_python_path()
_VARASANA_CFG = Path(
    "/data/lab_vm/refined/candescence_master/production/varasana/config.py"
)
_VARASANA_CKPT = Path(
    "/data/lab_vm/refined/candescence_master/production/varasana/model.pth"
)
_SAMPLE_DIR = Path(__file__).resolve().parents[2] / "data" / "sample" / "images"


@pytest.mark.skipif(
    not (_LEGACY_PY.exists() and _VARASANA_CFG.exists() and _VARASANA_CKPT.exists()),
    reason="legacy detection env and/or Varasana production weights not available",
)
def test_detect_image_round_trip():
    """End-to-end: client -> legacy worker -> parsed detections."""
    images = sorted(_SAMPLE_DIR.glob("*.png"))
    assert images, "no sample images found"
    dets = client.detect_image(
        _VARASANA_CFG, _VARASANA_CKPT, images[0], score_thr=0.3
    )
    # Out-of-domain image, so we assert structure not content.
    for det in dets:
        assert len(det.bbox) == 4
        assert isinstance(det.label_name, str)
        assert 0.3 <= det.score <= 1.0
