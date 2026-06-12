"""
Purpose: Unified model zoo for all Candescence projects
Author: Hallett Lab
Date: 2026-02-05

Flat model zoo with metadata registry. Each model gets a subdirectory
under the zoo root, with a single registry.json tracking all models
across projects (TLV, Varasana, Grace, etc.).

Usage:
    from candescence.core.model_zoo import ModelZoo

    zoo = ModelZoo()
    models = zoo.list_models(project="varasana")
    entry = zoo.get("varasana_fcos_v1")
    print(entry.get_checkpoint_path())
"""

import json
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from candescence.core.logging_config import get_logger
from candescence.core.settings import get_settings

logger = get_logger("candescence.core.model_zoo")

# Resolved from the settings layer (env var / config file / packaged default).
DEFAULT_ZOO_PATH = get_settings().zoo_path


@dataclass
class ZooEntry:
    """
    A model entry in the Candescence model zoo.

    Attributes
    ----------
    id : str
        Unique identifier (e.g. "varasana_fcos_v1")
    name : str
        Human-readable name
    project : str
        Project name: "varasana", "tlv", "grace", etc.
    model_type : str
        "research" or "production"
    version : str
        Version string (e.g. "1.0")
    architecture : str
        Model architecture: "fcos", "tendril_vae", "resnet", etc.
    checkpoint : str
        Checkpoint filename within the model directory (e.g. "model.pth")
    config_file : str
        Config filename within the model directory (e.g. "config.py")
    path : str
        Path to model directory in zoo
    created_at : str
        ISO timestamp of registration
    metrics : dict
        Performance metrics (mAP, loss, etc.)
    description : str
        Human-readable description
    tags : list
        Tags for filtering
    training_config : dict
        Training configuration used to produce this model
    """

    id: str
    name: str
    project: str
    model_type: str
    version: str
    architecture: str
    checkpoint: str
    config_file: str
    path: str
    created_at: str
    metrics: Dict[str, Any] = field(default_factory=dict)
    description: str = ""
    tags: List[str] = field(default_factory=list)
    training_config: Dict[str, Any] = field(default_factory=dict)

    def get_checkpoint_path(self) -> Path:
        """Get full path to the model checkpoint file."""
        return Path(self.path) / self.checkpoint

    def get_config_path(self) -> Path:
        """Get full path to the config file."""
        return Path(self.path) / self.config_file

    def exists(self) -> bool:
        """Check if the checkpoint file exists on disk."""
        return self.get_checkpoint_path().exists()


class ModelZoo:
    """
    Unified model zoo for all Candescence projects.

    Provides centralized registration and discovery of models across
    TLV, Varasana, Grace, and other subprojects. Models are stored
    in a flat directory structure with a single registry JSON.

    Parameters
    ----------
    zoo_path : Path, optional
        Root directory of the model zoo.
        Default: ``DEFAULT_ZOO_PATH`` (``<CANDESCENCE_REFINED>/zoo``).
    """

    def __init__(self, zoo_path: Optional[Path] = None) -> None:
        self.zoo_path = zoo_path or DEFAULT_ZOO_PATH
        self.registry_file = self.zoo_path / "registry.json"
        self._entries: Dict[str, ZooEntry] = {}
        self._load()

    def _load(self) -> None:
        """Load the registry from disk."""
        if self.registry_file.exists():
            try:
                with open(self.registry_file, "r") as f:
                    data = json.load(f)
                self._entries = {
                    entry_id: ZooEntry(**entry_data)
                    for entry_id, entry_data in data.get("models", {}).items()
                }
                logger.info(f"Loaded {len(self._entries)} models from zoo")
            except Exception as e:
                logger.error(f"Failed to load model zoo: {e}")
                self._entries = {}
        else:
            logger.info("No existing zoo registry found, starting fresh")
            self._entries = {}

    def _save(self) -> None:
        """Save the registry to disk."""
        try:
            self.zoo_path.mkdir(parents=True, exist_ok=True)
            data = {
                "models": {
                    entry_id: asdict(entry)
                    for entry_id, entry in self._entries.items()
                },
                "updated_at": datetime.now().isoformat(),
                "version": "1.0",
            }
            with open(self.registry_file, "w") as f:
                json.dump(data, f, indent=2)
            logger.info(f"Saved {len(self._entries)} models to zoo")
        except Exception as e:
            logger.error(f"Failed to save model zoo: {e}")

    def register(
        self,
        model_id: str,
        name: str,
        project: str,
        model_type: str,
        version: str,
        architecture: str,
        checkpoint: str,
        config_file: str,
        path: Optional[Path] = None,
        metrics: Optional[Dict[str, Any]] = None,
        description: str = "",
        tags: Optional[List[str]] = None,
        training_config: Optional[Dict[str, Any]] = None,
    ) -> ZooEntry:
        """
        Register a model in the zoo.

        Parameters
        ----------
        model_id : str
            Unique identifier (e.g. "varasana_fcos_v1")
        name : str
            Human-readable name
        project : str
            Project name: "varasana", "tlv", "grace"
        model_type : str
            "research" or "production"
        version : str
            Version string
        architecture : str
            Model architecture name
        checkpoint : str
            Checkpoint filename (e.g. "model.pth")
        config_file : str
            Config filename (e.g. "config.py")
        path : Path, optional
            Model directory. Defaults to zoo_path/model_id
        metrics : dict, optional
            Performance metrics
        description : str
            Description
        tags : list, optional
            Tags for filtering
        training_config : dict, optional
            Training configuration

        Returns
        -------
        ZooEntry
            The registered entry
        """
        model_path = path or (self.zoo_path / model_id)

        entry = ZooEntry(
            id=model_id,
            name=name,
            project=project,
            model_type=model_type,
            version=version,
            architecture=architecture,
            checkpoint=checkpoint,
            config_file=config_file,
            path=str(model_path),
            created_at=datetime.now().isoformat(),
            metrics=metrics or {},
            description=description,
            tags=tags or [],
            training_config=training_config or {},
        )

        self._entries[model_id] = entry
        self._save()
        logger.info(f"Registered model in zoo: {name} ({model_id})")
        return entry

    def get(self, model_id: str) -> Optional[ZooEntry]:
        """
        Get a model by ID.

        Parameters
        ----------
        model_id : str
            The model's unique ID

        Returns
        -------
        ZooEntry or None
        """
        return self._entries.get(model_id)

    def list_models(
        self,
        project: Optional[str] = None,
        model_type: Optional[str] = None,
        tags: Optional[List[str]] = None,
    ) -> List[ZooEntry]:
        """
        List models, optionally filtered.

        Parameters
        ----------
        project : str, optional
            Filter by project
        model_type : str, optional
            Filter by "research" or "production"
        tags : list, optional
            Filter by tags (must have all)

        Returns
        -------
        List[ZooEntry]
            Matching entries sorted by creation date (newest first)
        """
        results = list(self._entries.values())

        if project:
            results = [e for e in results if e.project == project]
        if model_type:
            results = [e for e in results if e.model_type == model_type]
        if tags:
            results = [e for e in results if all(t in e.tags for t in tags)]

        results.sort(key=lambda e: e.created_at, reverse=True)
        return results

    def remove(self, model_id: str) -> bool:
        """
        Remove a model from the registry (does not delete files).

        Parameters
        ----------
        model_id : str
            The model's unique ID

        Returns
        -------
        bool
            True if removed, False if not found
        """
        if model_id in self._entries:
            entry = self._entries.pop(model_id)
            self._save()
            logger.info(f"Removed model from zoo: {entry.name} ({model_id})")
            return True
        return False

    def get_model_path(self, model_id: str) -> Optional[Path]:
        """Get the full checkpoint path for a model."""
        entry = self._entries.get(model_id)
        if entry:
            return entry.get_checkpoint_path()
        return None

    def promote_to_production(self, model_id: str) -> bool:
        """Promote a research model to production."""
        entry = self._entries.get(model_id)
        if entry:
            entry.model_type = "production"
            self._save()
            logger.info(f"Promoted {entry.name} to production")
            return True
        return False

    def get_summary(self) -> Dict[str, Dict[str, int]]:
        """
        Get a summary of models by project and type.

        Returns
        -------
        dict
            {project: {"research": N, "production": N}}
        """
        summary: Dict[str, Dict[str, int]] = {}
        for entry in self._entries.values():
            if entry.project not in summary:
                summary[entry.project] = {"research": 0, "production": 0}
            summary[entry.project][entry.model_type] += 1
        return summary

    def get_projects(self) -> List[str]:
        """Get list of all project names in the zoo."""
        return sorted(set(e.project for e in self._entries.values()))

    def __len__(self) -> int:
        return len(self._entries)

    def __contains__(self, model_id: str) -> bool:
        return model_id in self._entries
