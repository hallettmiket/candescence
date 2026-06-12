"""
Purpose: Unified dataset registry for all Candescence projects
Author: Hallett Lab
Date: 2026-02-05

Flat dataset registry with metadata. Tracks datasets across all projects
(TLV, Varasana, Grace) in a single JSON file alongside the model zoo.

Usage:
    from candescence.core.dataset_zoo import DatasetZoo

    zoo = DatasetZoo()
    datasets = zoo.list_datasets(project="varasana")
    entry = zoo.get("varasana_curriculum")
    print(entry.path)
"""

import json
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from candescence.core.logging_config import get_logger
from candescence.core.settings import get_settings

logger = get_logger("candescence.core.dataset_zoo")

# Resolved from the settings layer (env var / config file / packaged default).
DEFAULT_ZOO_PATH = get_settings().zoo_path


@dataclass
class DatasetEntry:
    """
    A dataset entry in the Candescence dataset zoo.

    Attributes
    ----------
    id : str
        Unique identifier (e.g. "varasana_curriculum")
    name : str
        Human-readable name
    project : str
        Project name: "varasana", "tlv", "grace"
    path : str
        Path to dataset root directory
    num_samples : int
        Total number of samples
    splits : dict
        Sample counts per split (e.g. {"train": 775, "val": 200, "test": 900})
    format : str
        Data format: "curriculum_pkl", "coco_json", "image_dir", etc.
    classes : list
        Class names in the dataset
    description : str
        Human-readable description
    created_at : str
        ISO timestamp of registration
    metadata : dict
        Additional metadata (annotation type, image size, etc.)
    """

    id: str
    name: str
    project: str
    path: str
    num_samples: int
    splits: Dict[str, int] = field(default_factory=dict)
    format: str = ""
    classes: List[str] = field(default_factory=list)
    description: str = ""
    created_at: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)

    def get_path(self) -> Path:
        """Get the dataset root path."""
        return Path(self.path)

    def exists(self) -> bool:
        """Check if the dataset directory exists on disk."""
        return Path(self.path).exists()


class DatasetZoo:
    """
    Unified dataset registry for all Candescence projects.

    Parameters
    ----------
    zoo_path : Path, optional
        Root directory of the zoo.
        Default: ``DEFAULT_ZOO_PATH`` (``<CANDESCENCE_REFINED>/zoo``).
    """

    def __init__(self, zoo_path: Optional[Path] = None) -> None:
        self.zoo_path = zoo_path or DEFAULT_ZOO_PATH
        self.registry_file = self.zoo_path / "datasets.json"
        self._entries: Dict[str, DatasetEntry] = {}
        self._load()

    def _load(self) -> None:
        """Load the registry from disk."""
        if self.registry_file.exists():
            try:
                with open(self.registry_file, "r") as f:
                    data = json.load(f)
                self._entries = {
                    entry_id: DatasetEntry(**entry_data)
                    for entry_id, entry_data in data.get("datasets", {}).items()
                }
                logger.info(f"Loaded {len(self._entries)} datasets from zoo")
            except Exception as e:
                logger.error(f"Failed to load dataset zoo: {e}")
                self._entries = {}
        else:
            logger.info("No existing dataset registry found, starting fresh")
            self._entries = {}

    def _save(self) -> None:
        """Save the registry to disk."""
        try:
            self.zoo_path.mkdir(parents=True, exist_ok=True)
            data = {
                "datasets": {
                    entry_id: asdict(entry)
                    for entry_id, entry in self._entries.items()
                },
                "updated_at": datetime.now().isoformat(),
                "version": "1.0",
            }
            with open(self.registry_file, "w") as f:
                json.dump(data, f, indent=2)
            logger.info(f"Saved {len(self._entries)} datasets to zoo")
        except Exception as e:
            logger.error(f"Failed to save dataset zoo: {e}")

    def register(
        self,
        dataset_id: str,
        name: str,
        project: str,
        path: Path,
        num_samples: int,
        splits: Optional[Dict[str, int]] = None,
        format: str = "",
        classes: Optional[List[str]] = None,
        description: str = "",
        metadata: Optional[Dict[str, Any]] = None,
    ) -> DatasetEntry:
        """
        Register a dataset in the zoo.

        Parameters
        ----------
        dataset_id : str
            Unique identifier
        name : str
            Human-readable name
        project : str
            Project name
        path : Path
            Dataset root directory
        num_samples : int
            Total sample count
        splits : dict, optional
            Counts per split
        format : str
            Data format description
        classes : list, optional
            Class names
        description : str
            Description
        metadata : dict, optional
            Additional metadata

        Returns
        -------
        DatasetEntry
            The registered entry
        """
        entry = DatasetEntry(
            id=dataset_id,
            name=name,
            project=project,
            path=str(path),
            num_samples=num_samples,
            splits=splits or {},
            format=format,
            classes=classes or [],
            description=description,
            created_at=datetime.now().isoformat(),
            metadata=metadata or {},
        )

        self._entries[dataset_id] = entry
        self._save()
        logger.info(f"Registered dataset in zoo: {name} ({dataset_id})")
        return entry

    def get(self, dataset_id: str) -> Optional[DatasetEntry]:
        """Get a dataset by ID."""
        return self._entries.get(dataset_id)

    def list_datasets(
        self,
        project: Optional[str] = None,
    ) -> List[DatasetEntry]:
        """
        List datasets, optionally filtered by project.

        Parameters
        ----------
        project : str, optional
            Filter by project name

        Returns
        -------
        List[DatasetEntry]
            Matching entries sorted by creation date (newest first)
        """
        results = list(self._entries.values())

        if project:
            results = [e for e in results if e.project == project]

        results.sort(key=lambda e: e.created_at, reverse=True)
        return results

    def remove(self, dataset_id: str) -> bool:
        """Remove a dataset from the registry (does not delete files)."""
        if dataset_id in self._entries:
            entry = self._entries.pop(dataset_id)
            self._save()
            logger.info(f"Removed dataset from zoo: {entry.name} ({dataset_id})")
            return True
        return False

    def get_summary(self) -> Dict[str, int]:
        """
        Get a summary of datasets by project.

        Returns
        -------
        dict
            {project: count}
        """
        summary: Dict[str, int] = {}
        for entry in self._entries.values():
            summary[entry.project] = summary.get(entry.project, 0) + 1
        return summary

    def get_projects(self) -> List[str]:
        """Get list of all project names with datasets."""
        return sorted(set(e.project for e in self._entries.values()))

    def __len__(self) -> int:
        return len(self._entries)

    def __contains__(self, dataset_id: str) -> bool:
        return dataset_id in self._entries
