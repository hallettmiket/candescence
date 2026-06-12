"""
Purpose: Guard the settings layer and the "no hard-coded lab paths" invariant.
Author: Hallett Lab
Date: 2026-06-12
Input: The candescence source tree and the settings module.
Output: Pytest assertions (no side effects).

These tests lock in Phase 1 of the production refactor: every filesystem path
must resolve through ``candescence.core.settings`` so that an external user can
relocate the data trees via environment variables or a config file, and no lab
machine-specific absolute path may remain anywhere under ``src/`` except inside
``settings.py`` (where the packaged defaults intentionally live).
"""

from __future__ import annotations

import os
import subprocess
from pathlib import Path

import pytest

from candescence.core import settings as settings_mod
from candescence.core.settings import load_settings


# Repo root = two levels up from this test file (tests/test_settings.py).
REPO_ROOT = Path(__file__).resolve().parents[1]
SRC_ROOT = REPO_ROOT / "src"

# Absolute path prefixes that must not be hard-coded anywhere in the tree
# (they leak the lab's internal server topology when the repo is public).
BANNED_PREFIXES = ("/home/UWO/mhallet", "/data/lab_vm")

# Files allowed to contain the packaged lab defaults: the Python + R config
# layers (single designated locations, each with env-var overrides) and this
# guard file itself (which necessarily spells out the banned patterns).
ALLOWLISTED_FILES = {
    SRC_ROOT / "candescence" / "core" / "settings.py",
    SRC_ROOT / "init.R",
    REPO_ROOT / "tests" / "test_settings.py",
}

# Text/source/config/doc extensions worth scanning for leaked paths.
_TEXT_SUFFIXES = {
    ".py", ".r", ".md", ".json", ".toml", ".yml", ".yaml",
    ".ipynb", ".txt", ".cfg", ".ini", ".sh",
}
_SKIP_DIRS = {".git", "__pycache__", ".pytest_cache", ".vscode", "node_modules"}


def _text_files() -> list[Path]:
    # Only git-TRACKED files ship publicly, so scan exactly those (gitignored
    # files like .claude/ or candescence.toml are intentionally excluded).
    result = subprocess.run(
        ["git", "ls-files"],
        cwd=REPO_ROOT, capture_output=True, text=True, check=True,
    )
    out = []
    for rel in result.stdout.splitlines():
        p = REPO_ROOT / rel
        if p.suffix.lower() not in _TEXT_SUFFIXES:
            continue
        if any(part in _SKIP_DIRS for part in p.parts):
            continue
        if p.is_file():
            out.append(p)
    return out


def test_no_hardcoded_lab_paths_in_repo() -> None:
    """No banned absolute path may appear anywhere except the allowlisted config files."""
    offenders: list[str] = []
    for path in _text_files():
        if path in ALLOWLISTED_FILES:
            continue
        text = path.read_text(encoding="utf-8", errors="ignore")
        for lineno, line in enumerate(text.splitlines(), start=1):
            if any(prefix in line for prefix in BANNED_PREFIXES):
                rel = path.relative_to(REPO_ROOT)
                offenders.append(f"{rel}:{lineno}: {line.strip()[:120]}")
    assert not offenders, "Hard-coded lab paths found:\n" + "\n".join(offenders)


def test_defaults_resolve_to_lab_layout() -> None:
    """With no overrides, settings resolve to the packaged lab defaults."""
    s = load_settings()
    assert s.raw_path == settings_mod.DEFAULT_RAW
    assert s.refined_path == settings_mod.DEFAULT_REFINED
    assert s.zoo_path == settings_mod.DEFAULT_REFINED / "zoo"
    assert s.image_dir == settings_mod.DEFAULT_RAW / "tlv_images" / "all-final"
    assert s.metadata_xlsx.name == "cleaned_candida_metadata.xlsx"
    assert s.manual_labels_csv.name == "manually_labelled_images.csv"


def test_env_override_cascades(monkeypatch: pytest.MonkeyPatch) -> None:
    """Overriding the refined root moves zoo + metadata coherently."""
    monkeypatch.setenv(settings_mod.ENV_REFINED, "/tmp/cand_test_refined")
    monkeypatch.setenv(settings_mod.ENV_RAW, "/tmp/cand_test_raw")
    s = load_settings()
    assert s.refined_path == Path("/tmp/cand_test_refined")
    assert s.zoo_path == Path("/tmp/cand_test_refined/zoo")
    assert s.metadata_xlsx == Path(
        "/tmp/cand_test_refined/data_files/cleaned_candida_metadata.xlsx"
    )
    assert s.image_dir == Path("/tmp/cand_test_raw/tlv_images/all-final")


def test_explicit_arg_beats_env(monkeypatch: pytest.MonkeyPatch) -> None:
    """An explicit argument takes precedence over the environment variable."""
    monkeypatch.setenv(settings_mod.ENV_REFINED, "/tmp/from_env")
    s = load_settings(refined_path="/tmp/from_arg")
    assert s.refined_path == Path("/tmp/from_arg")


def test_config_file_used_when_no_env(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A TOML config file is honoured when no env var is set."""
    for var in (settings_mod.ENV_REFINED, settings_mod.ENV_RAW):
        monkeypatch.delenv(var, raising=False)
    cfg = tmp_path / "candescence.toml"
    cfg.write_text('[paths]\nrefined = "/tmp/from_config_refined"\n')
    monkeypatch.setenv(settings_mod.ENV_CONFIG_FILE, str(cfg))
    s = load_settings()
    assert s.refined_path == Path("/tmp/from_config_refined")
    assert s.zoo_path == Path("/tmp/from_config_refined/zoo")


def test_env_beats_config_file(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Env var wins over the config file for the same key."""
    cfg = tmp_path / "candescence.toml"
    cfg.write_text('[paths]\nrefined = "/tmp/cfg_ref"\n')
    monkeypatch.setenv(settings_mod.ENV_CONFIG_FILE, str(cfg))
    monkeypatch.setenv(settings_mod.ENV_REFINED, "/tmp/env_ref")
    s = load_settings()
    assert s.refined_path == Path("/tmp/env_ref")


def test_legacy_helpers_overridable(monkeypatch: pytest.MonkeyPatch) -> None:
    """Legacy-tree helpers honour their env vars."""
    monkeypatch.setenv(settings_mod.ENV_LEGACY_REFINED, "/tmp/legacy_ref")
    monkeypatch.setenv(settings_mod.ENV_MASTER_REPO, "/tmp/master_repo")
    assert settings_mod.legacy_refined_root() == Path("/tmp/legacy_ref")
    assert settings_mod.master_repo_root() == Path("/tmp/master_repo")
