"""
Purpose: Centralized logging configuration for Candescence
Author: Hallett Lab
Date: 2026-01-27
Input: Optional log file path and logging level
Output: Configured logger instance

This module replaces the 9+ hardcoded logging.basicConfig() calls
scattered across architecture files in the original codebase.
"""

import logging
import sys
from pathlib import Path
from typing import Optional


# Module-level registry of configured loggers
_configured_loggers: set = set()


def configure_logging(
    log_path: Optional[Path] = None,
    level: int = logging.INFO,
    name: str = "candescence",
    console: bool = True,
) -> logging.Logger:
    """
    Configure logging for Candescence experiments.

    This function provides a centralized way to configure logging,
    replacing the hardcoded logging.basicConfig() calls in the original
    architecture files.

    Parameters
    ----------
    log_path : Path, optional
        Path to log file. If None, only console logging is enabled.
    level : int
        Logging level (default: logging.INFO)
    name : str
        Logger name (default: "candescence")
    console : bool
        Whether to enable console output (default: True)

    Returns
    -------
    logging.Logger
        Configured logger instance

    Examples
    --------
    >>> from candescence.core.logging_config import configure_logging
    >>> logger = configure_logging(
    ...     log_path=Path("exp/training.log"),
    ...     level=logging.DEBUG
    ... )
    >>> logger.info("Training started")
    """
    logger = logging.getLogger(name)

    # Avoid reconfiguring the same logger multiple times
    if name in _configured_loggers:
        return logger

    logger.setLevel(level)

    # Clear any existing handlers
    logger.handlers.clear()

    # Formatter matching original style
    formatter = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )

    # Console handler
    if console:
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setLevel(level)
        console_handler.setFormatter(formatter)
        logger.addHandler(console_handler)

    # File handler
    if log_path is not None:
        log_path = Path(log_path)
        log_path.parent.mkdir(parents=True, exist_ok=True)

        file_handler = logging.FileHandler(log_path)
        file_handler.setLevel(logging.DEBUG)  # File gets everything
        file_handler.setFormatter(logging.Formatter(
            '%(asctime)s %(levelname)s %(name)s %(message)s'
        ))
        logger.addHandler(file_handler)

    # Prevent propagation to root logger
    logger.propagate = False

    _configured_loggers.add(name)

    return logger


def get_logger(name: str = "candescence") -> logging.Logger:
    """
    Get a logger instance, creating with defaults if needed.

    Parameters
    ----------
    name : str
        Logger name

    Returns
    -------
    logging.Logger
        Logger instance
    """
    logger = logging.getLogger(name)

    # If not configured, set up with defaults
    if name not in _configured_loggers and not logger.handlers:
        return configure_logging(name=name)

    return logger


def get_training_logger(
    experiment_path: Path,
    name: str = "training"
) -> logging.Logger:
    """
    Get a logger configured for training with file output.

    Parameters
    ----------
    experiment_path : Path
        Path to experiment directory (log saved to experiment_path/training.log)
    name : str
        Logger name suffix

    Returns
    -------
    logging.Logger
        Logger configured for training
    """
    log_path = Path(experiment_path) / "training.log"
    full_name = f"candescence.{name}"

    return configure_logging(
        log_path=log_path,
        level=logging.DEBUG,
        name=full_name,
    )
