"""
Purpose: Centralized, override-able path configuration for Candescence.
Author: Hallett Lab
Date: 2026-06-12
Input: Optional environment variables and/or a TOML config file.
Output: A resolved ``CandescenceSettings`` object used by every module that
        needs a filesystem path, so that no path is hard-coded.

Resolution precedence (highest first) for every path:

1. Explicit argument passed to :func:`load_settings`.
2. Environment variable (e.g. ``CANDESCENCE_REFINED``).
3. A TOML config file (``[paths]`` table). Located via the
   ``CANDESCENCE_CONFIG`` env var, then ``<repo>/candescence.toml``, then
   ``~/.config/candescence/candescence.toml``.
4. The packaged default (the Hallett-lab VM layout).

External users override the lab defaults by exporting the env vars or
dropping a ``candescence.toml`` next to the repo. Lab machines keep working
unchanged because the defaults still point at ``/data/lab_vm``.
"""

from __future__ import annotations

import logging
import os
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import Any, Mapping, Optional

logger = logging.getLogger(__name__)

# TOML parser: stdlib ``tomllib`` on Python 3.11+, the ``tomli`` backport on
# 3.10. If neither is available, optional config-file support is disabled but
# env-var / default resolution still works.
try:  # Python 3.11+
    import tomllib as _toml
except ModuleNotFoundError:  # pragma: no cover - depends on interpreter version
    try:  # Python 3.10 backport
        import tomli as _toml
    except ModuleNotFoundError:
        _toml = None

# ---------------------------------------------------------------------------
# Environment variable names (the public override surface)
# ---------------------------------------------------------------------------
ENV_RAW = "CANDESCENCE_RAW"
ENV_REFINED = "CANDESCENCE_REFINED"
ENV_IMAGES = "CANDESCENCE_IMAGES"
ENV_ZOO = "CANDESCENCE_ZOO"
ENV_METADATA_XLSX = "CANDESCENCE_METADATA_XLSX"
ENV_MANUAL_LABELS_CSV = "CANDESCENCE_MANUAL_LABELS_CSV"
ENV_CONFIG_FILE = "CANDESCENCE_CONFIG"

# Legacy-tree overrides. These point at the *old* candescence_master /
# candescence refined trees, used only for backward-compatible discovery of
# pre-existing models/datasets and for unpickling old inference objects. They
# are always existence-checked at the call site, so external installs that lack
# these trees simply skip them.
ENV_LEGACY_REFINED = "CANDESCENCE_LEGACY_REFINED"
ENV_MASTER_REPO = "CANDESCENCE_MASTER_REPO"

# ---------------------------------------------------------------------------
# Packaged defaults: the Hallett-lab VM layout. These remain correct on the
# lab machines; everything below is overridable without touching code.
# ---------------------------------------------------------------------------
# Repo root = three parents up from this file (src/candescence/core/settings.py).
REPO_ROOT = Path(__file__).resolve().parents[3]

DEFAULT_RAW = Path("/data/lab_vm/raw/candescence_new")
DEFAULT_REFINED = Path("/data/lab_vm/refined/candescence_new")

# Legacy refined-tree default (lab layout). Overridable via CANDESCENCE_LEGACY_REFINED.
# (There is no baked-in default for the legacy master *repo*; it is opt-in via
# CANDESCENCE_MASTER_REPO so no machine-specific path lives in this file.)
DEFAULT_LEGACY_REFINED = Path("/data/lab_vm/refined")

# Sub-paths derived from refined/raw unless individually overridden.
_DEFAULT_IMAGES_SUBPATH = ("tlv_images", "all-final")
_DEFAULT_ZOO_SUBPATH = ("zoo",)
_DEFAULT_DATA_FILES_SUBPATH = ("data_files",)
_DEFAULT_METADATA_XLSX_NAME = "cleaned_candida_metadata.xlsx"
_DEFAULT_MANUAL_LABELS_NAME = "manually_labelled_images.csv"

_CONFIG_FILENAME = "candescence.toml"


@dataclass(frozen=True)
class CandescenceSettings:
    """
    Resolved filesystem configuration for a Candescence session.

    All attributes are absolute :class:`pathlib.Path` objects. Construct via
    :func:`load_settings` (or the cached :func:`get_settings`) rather than
    directly, so that the env-var / config-file / default precedence applies.

    Attributes
    ----------
    repo_path : Path
        Repository root (the working copy).
    raw_path : Path
        Immutable raw-data root (read-only by convention).
    refined_path : Path
        Refined-data root (append-only) where outputs are written.
    image_dir : Path
        Default directory of input colony images for the app/training.
    zoo_path : Path
        Model/dataset registry root.
    metadata_xlsx : Path
        "Big" cleaned metadata spreadsheet (strain, plate, niche, ...).
    manual_labels_csv : Path
        Manual morphology labels (file_name + morphology).
    """

    repo_path: Path
    raw_path: Path
    refined_path: Path
    image_dir: Path
    zoo_path: Path
    metadata_xlsx: Path
    manual_labels_csv: Path

    def as_dict(self) -> dict[str, str]:
        """Return a JSON-serialisable ``{name: str(path)}`` mapping."""
        return {
            "repo_path": str(self.repo_path),
            "raw_path": str(self.raw_path),
            "refined_path": str(self.refined_path),
            "image_dir": str(self.image_dir),
            "zoo_path": str(self.zoo_path),
            "metadata_xlsx": str(self.metadata_xlsx),
            "manual_labels_csv": str(self.manual_labels_csv),
        }


def _load_config_file(repo_path: Path) -> Mapping[str, Any]:
    """
    Locate and parse the optional TOML config file's ``[paths]`` table.

    Search order: ``$CANDESCENCE_CONFIG`` → ``<repo>/candescence.toml`` →
    ``~/.config/candescence/candescence.toml``. Returns an empty mapping when
    no file is found or the file has no ``[paths]`` table.
    """
    candidates = []
    env_cfg = os.environ.get(ENV_CONFIG_FILE)
    if env_cfg:
        candidates.append(Path(env_cfg).expanduser())
    candidates.append(repo_path / _CONFIG_FILENAME)
    candidates.append(Path.home() / ".config" / "candescence" / _CONFIG_FILENAME)

    for candidate in candidates:
        if candidate.is_file():
            if _toml is None:
                logger.warning(
                    "Found config file %s but no TOML parser is installed "
                    "(install 'tomli' on Python < 3.11); ignoring it.",
                    candidate,
                )
                return {}
            try:
                with candidate.open("rb") as handle:
                    data = _toml.load(handle)
            except (OSError, _toml.TOMLDecodeError) as exc:
                logger.warning("Could not parse config file %s: %s", candidate, exc)
                return {}
            logger.info("Loaded Candescence config from %s", candidate)
            paths = data.get("paths", {})
            return paths if isinstance(paths, Mapping) else {}
    return {}


def _resolve(
    *,
    arg: Optional[Path | str],
    env_var: str,
    config: Mapping[str, Any],
    config_key: str,
    default: Path,
) -> Path:
    """
    Resolve a single path following the documented precedence.

    arg → env var → config file → packaged default.
    """
    if arg is not None:
        return Path(arg).expanduser().resolve()
    env_val = os.environ.get(env_var)
    if env_val:
        return Path(env_val).expanduser().resolve()
    cfg_val = config.get(config_key)
    if cfg_val:
        return Path(str(cfg_val)).expanduser().resolve()
    return default


def load_settings(
    *,
    repo_path: Optional[Path | str] = None,
    raw_path: Optional[Path | str] = None,
    refined_path: Optional[Path | str] = None,
    image_dir: Optional[Path | str] = None,
    zoo_path: Optional[Path | str] = None,
    metadata_xlsx: Optional[Path | str] = None,
    manual_labels_csv: Optional[Path | str] = None,
) -> CandescenceSettings:
    """
    Build a :class:`CandescenceSettings` applying the override precedence.

    Any explicit argument wins; otherwise the env var, then the config file,
    then the packaged lab default is used. Derived paths (image_dir, zoo_path,
    metadata files) default relative to the resolved raw/refined roots so that
    overriding just ``CANDESCENCE_REFINED`` moves the whole tree coherently.

    Parameters
    ----------
    repo_path, raw_path, refined_path, image_dir, zoo_path, metadata_xlsx, \
    manual_labels_csv : Path or str, optional
        Explicit overrides; see module docstring for precedence.

    Returns
    -------
    CandescenceSettings
        Fully-resolved, absolute paths.
    """
    resolved_repo = (
        Path(repo_path).expanduser().resolve() if repo_path is not None else REPO_ROOT
    )
    config = _load_config_file(resolved_repo)

    resolved_raw = _resolve(
        arg=raw_path, env_var=ENV_RAW, config=config, config_key="raw", default=DEFAULT_RAW
    )
    resolved_refined = _resolve(
        arg=refined_path,
        env_var=ENV_REFINED,
        config=config,
        config_key="refined",
        default=DEFAULT_REFINED,
    )

    default_images = resolved_raw.joinpath(*_DEFAULT_IMAGES_SUBPATH)
    default_zoo = resolved_refined.joinpath(*_DEFAULT_ZOO_SUBPATH)
    default_data_files = resolved_refined.joinpath(*_DEFAULT_DATA_FILES_SUBPATH)
    default_xlsx = default_data_files / _DEFAULT_METADATA_XLSX_NAME
    default_csv = default_data_files / _DEFAULT_MANUAL_LABELS_NAME

    return CandescenceSettings(
        repo_path=resolved_repo,
        raw_path=resolved_raw,
        refined_path=resolved_refined,
        image_dir=_resolve(
            arg=image_dir,
            env_var=ENV_IMAGES,
            config=config,
            config_key="images",
            default=default_images,
        ),
        zoo_path=_resolve(
            arg=zoo_path, env_var=ENV_ZOO, config=config, config_key="zoo", default=default_zoo
        ),
        metadata_xlsx=_resolve(
            arg=metadata_xlsx,
            env_var=ENV_METADATA_XLSX,
            config=config,
            config_key="metadata_xlsx",
            default=default_xlsx,
        ),
        manual_labels_csv=_resolve(
            arg=manual_labels_csv,
            env_var=ENV_MANUAL_LABELS_CSV,
            config=config,
            config_key="manual_labels_csv",
            default=default_csv,
        ),
    )


@lru_cache(maxsize=1)
def get_settings() -> CandescenceSettings:
    """
    Return the process-wide cached settings (resolved from env/config/defaults).

    Use this from library and app code. Call :func:`reset_settings` in tests
    after mutating the environment.
    """
    return load_settings()


def reset_settings() -> None:
    """Clear the cached settings so the next :func:`get_settings` re-resolves."""
    get_settings.cache_clear()


def legacy_refined_root() -> Path:
    """
    Return the root of the legacy refined trees (``candescence_master`` /
    ``candescence``), from ``$CANDESCENCE_LEGACY_REFINED`` or the lab default.

    Used only for backward-compatible discovery of pre-existing assets; callers
    must existence-check the resulting paths.
    """
    return Path(os.environ.get(ENV_LEGACY_REFINED, str(DEFAULT_LEGACY_REFINED)))


def master_repo_root() -> Optional[Path]:
    """
    Return the legacy ``candescence_master`` repository root, or ``None``.

    Resolved from ``$CANDESCENCE_MASTER_REPO``; returns ``None`` when that env
    var is unset (no machine-specific default is baked into the codebase). Used
    only to unpickle old inference objects; the caller existence-checks it.
    """
    val = os.environ.get(ENV_MASTER_REPO)
    return Path(val) if val else None
