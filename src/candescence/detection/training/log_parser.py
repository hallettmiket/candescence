"""
Purpose: Parse MMDETECTION training logs for progress monitoring
Author: Hallett Lab
Date: 2026-02-04

Provides utilities for parsing MMDETECTION log files to extract loss values
and training progress for real-time monitoring in Streamlit.
"""

import json
import re
from pathlib import Path
from typing import Any, Dict, List, Optional

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.detection.training.log_parser")


class MMDetectionLogParser:
    """
    Parser for MMDETECTION training log files.

    MMDETECTION produces logs via TextLoggerHook with format like:
    2026-02-04 10:15:32,123 - mmdet - INFO - Epoch [1][100/500]  lr: 1.000e-02, ...
    """

    # Regex patterns for parsing log lines
    EPOCH_PATTERN = re.compile(
        r"Epoch\s*\[(\d+)\]\[(\d+)/(\d+)\]"
    )

    LOSS_PATTERN = re.compile(
        r"(loss_cls|loss_bbox|loss_centerness|loss):\s*([\d.]+)"
    )

    LR_PATTERN = re.compile(
        r"lr:\s*([\d.e+-]+)"
    )

    EVAL_PATTERN = re.compile(
        r"(mAP|AP50|AP75|bbox_mAP):\s*([\d.]+)"
    )

    def __init__(self, log_file: Optional[Path] = None) -> None:
        """
        Initialize the log parser.

        Args:
            log_file: Path to the log file to parse
        """
        self.log_file = log_file
        self.last_position = 0
        self._cached_metrics: List[Dict[str, Any]] = []

    def set_log_file(self, log_file: Path) -> None:
        """Set the log file to parse."""
        self.log_file = log_file
        self.last_position = 0
        self._cached_metrics = []

    def parse_all(self) -> List[Dict[str, Any]]:
        """
        Parse the entire log file.

        Returns:
            List of metric dictionaries, one per logged iteration
        """
        if self.log_file is None or not self.log_file.exists():
            return []

        metrics = []
        with open(self.log_file, "r") as f:
            for line in f:
                parsed = self._parse_line(line)
                if parsed:
                    metrics.append(parsed)

        return metrics

    def parse_new(self) -> List[Dict[str, Any]]:
        """
        Parse only new content since last read (incremental parsing).

        Returns:
            List of new metric dictionaries
        """
        if self.log_file is None or not self.log_file.exists():
            return []

        new_metrics = []
        try:
            with open(self.log_file, "r") as f:
                f.seek(self.last_position)
                for line in f:
                    parsed = self._parse_line(line)
                    if parsed:
                        new_metrics.append(parsed)
                self.last_position = f.tell()
        except Exception as e:
            logger.warning(f"Error reading log file: {e}")

        self._cached_metrics.extend(new_metrics)
        return new_metrics

    def get_latest(self) -> Optional[Dict[str, Any]]:
        """
        Get the most recent metrics from the log file.

        Returns:
            Most recent metric dictionary or None
        """
        new_metrics = self.parse_new()
        if new_metrics:
            return new_metrics[-1]
        if self._cached_metrics:
            return self._cached_metrics[-1]
        return None

    def get_epoch_summary(self, epoch: int) -> Dict[str, Any]:
        """
        Get aggregated metrics for a specific epoch.

        Args:
            epoch: Epoch number (1-indexed as in MMDETECTION logs)

        Returns:
            Dict with averaged loss values for the epoch
        """
        epoch_metrics = [
            m for m in self._cached_metrics
            if m.get("epoch") == epoch
        ]

        if not epoch_metrics:
            return {}

        # Average the losses
        summary = {
            "epoch": epoch,
            "num_iters": len(epoch_metrics),
        }

        for key in ["loss_cls", "loss_bbox", "loss_centerness", "loss"]:
            values = [m.get(key, 0) for m in epoch_metrics if key in m]
            if values:
                summary[key] = sum(values) / len(values)

        # Get final LR for epoch
        if "lr" in epoch_metrics[-1]:
            summary["lr"] = epoch_metrics[-1]["lr"]

        return summary

    def get_all_epoch_summaries(self) -> List[Dict[str, Any]]:
        """
        Get summaries for all completed epochs.

        Returns:
            List of epoch summary dictionaries
        """
        # Make sure we have all data
        self.parse_new()

        # Find all unique epochs
        epochs = sorted(set(m.get("epoch", 0) for m in self._cached_metrics))

        summaries = []
        for epoch in epochs:
            summary = self.get_epoch_summary(epoch)
            if summary:
                summaries.append(summary)

        return summaries

    def _parse_line(self, line: str) -> Optional[Dict[str, Any]]:
        """
        Parse a single log line.

        Args:
            line: Log line to parse

        Returns:
            Dict with extracted values or None if not a metrics line
        """
        # Check if this is an epoch training line
        epoch_match = self.EPOCH_PATTERN.search(line)
        if not epoch_match:
            return None

        metrics = {
            "epoch": int(epoch_match.group(1)),
            "iter": int(epoch_match.group(2)),
            "total_iters": int(epoch_match.group(3)),
        }

        # Extract learning rate
        lr_match = self.LR_PATTERN.search(line)
        if lr_match:
            metrics["lr"] = float(lr_match.group(1))

        # Extract all loss values
        for match in self.LOSS_PATTERN.finditer(line):
            key = match.group(1)
            value = float(match.group(2))
            metrics[key] = value

        return metrics

    @staticmethod
    def parse_eval_results(log_file: Path) -> Dict[str, float]:
        """
        Parse evaluation results from log file.

        Looks for COCO-style mAP metrics in the log.

        Args:
            log_file: Path to log file containing eval results

        Returns:
            Dict with mAP, AP50, AP75, etc.
        """
        results = {}

        if not log_file.exists():
            return results

        try:
            with open(log_file, "r") as f:
                content = f.read()

            # Look for COCO evaluation results
            # Format: Average Precision  (AP) @[ IoU=0.50:0.95 | area=   all | maxDets=100 ] = 0.xxx
            ap_pattern = re.compile(
                r"Average Precision.*IoU=0\.50:0\.95.*=\s*([\d.]+)"
            )
            ap50_pattern = re.compile(
                r"Average Precision.*IoU=0\.50\s.*=\s*([\d.]+)"
            )
            ap75_pattern = re.compile(
                r"Average Precision.*IoU=0\.75\s.*=\s*([\d.]+)"
            )

            match = ap_pattern.search(content)
            if match:
                results["mAP"] = float(match.group(1))

            match = ap50_pattern.search(content)
            if match:
                results["AP50"] = float(match.group(1))

            match = ap75_pattern.search(content)
            if match:
                results["AP75"] = float(match.group(1))

            # Also try simple format
            for match in MMDetectionLogParser.EVAL_PATTERN.finditer(content):
                key = match.group(1)
                if key == "bbox_mAP":
                    key = "mAP"
                results[key] = float(match.group(2))

        except Exception as e:
            logger.warning(f"Error parsing eval results: {e}")

        return results

    @staticmethod
    def parse_json_log(json_log_file: Path) -> List[Dict[str, Any]]:
        """
        Parse MMDETECTION JSON log file (if available).

        MMDETECTION can produce JSON logs with JsonLoggerHook.

        Args:
            json_log_file: Path to JSON log file

        Returns:
            List of metric dictionaries
        """
        if not json_log_file.exists():
            return []

        metrics = []
        try:
            with open(json_log_file, "r") as f:
                for line in f:
                    line = line.strip()
                    if line:
                        try:
                            entry = json.loads(line)
                            metrics.append(entry)
                        except json.JSONDecodeError:
                            continue
        except Exception as e:
            logger.warning(f"Error parsing JSON log: {e}")

        return metrics


def find_latest_log_file(work_dir: Path) -> Optional[Path]:
    """
    Find the most recent log file in a work directory.

    Args:
        work_dir: MMDETECTION work directory

    Returns:
        Path to most recent .log file or None
    """
    if not work_dir.exists():
        return None

    log_files = list(work_dir.glob("*.log"))
    if not log_files:
        # Try subdirectories
        log_files = list(work_dir.glob("**/*.log"))

    if not log_files:
        return None

    # Return most recently modified
    return max(log_files, key=lambda p: p.stat().st_mtime)


def find_latest_checkpoint(work_dir: Path) -> Optional[Path]:
    """
    Find the most recent checkpoint in a work directory.

    Args:
        work_dir: MMDETECTION work directory

    Returns:
        Path to most recent .pth checkpoint or None
    """
    if not work_dir.exists():
        return None

    # Look for epoch_*.pth or latest.pth
    checkpoints = list(work_dir.glob("epoch_*.pth"))

    if not checkpoints:
        latest = work_dir / "latest.pth"
        if latest.exists():
            return latest
        return None

    # Get highest epoch number
    def get_epoch(path: Path) -> int:
        match = re.search(r"epoch_(\d+)", path.name)
        return int(match.group(1)) if match else 0

    return max(checkpoints, key=get_epoch)
