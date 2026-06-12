#!/usr/bin/env python3
"""Quantify hue nuisance in TLV latent spaces.

Fits within-media OLS regressions of latent embeddings on HSV colour
features, computes residuals, and compares PCA explained-variance
ratios before and after residualization.

Usage
-----
Single space::

    python scripts/quantify_hue_nuisance.py \
        --embeddings run_mu.npy \
        --metadata   run_metadata.csv \
        --output-dir results/hue_qc

Multiple spaces (NPZ with keys ``primary``, ``tendril_0``, …)::

    python scripts/quantify_hue_nuisance.py \
        --embeddings run_mu.npz \
        --metadata   run_metadata.csv \
        --output-dir results/hue_qc \
        --spaces primary tendril_0 tendril_1

Author: Candescence team
Date:   2026-04-03
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

# Allow running from repo root without install.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

import numpy as np
import pandas as pd

from candescence.tlv.analysis.hue_nuisance_qc import (
    report_to_stdout,
    run_hue_nuisance_qc,
)

logger = logging.getLogger(__name__)


def _load_embeddings(path: Path, spaces: list[str] | None) -> dict[str, np.ndarray]:
    """Load embeddings from .npy or .npz file.

    Returns a dict mapping space-name → (N, D) array.
    """
    path = Path(path)
    if path.suffix == ".npz":
        data = np.load(path)
        if spaces:
            missing = set(spaces) - set(data.files)
            if missing:
                raise ValueError(
                    f"Requested spaces {missing} not found in {path}. "
                    f"Available keys: {data.files}"
                )
            return {k: data[k] for k in spaces}
        return dict(data)
    else:
        # .npy — single array
        arr = np.load(path)
        name = spaces[0] if spaces else "primary"
        return {name: arr}


def _load_metadata(path: Path) -> pd.DataFrame:
    """Load metadata from CSV or Parquet."""
    path = Path(path)
    if path.suffix == ".parquet":
        return pd.read_parquet(path)
    return pd.read_csv(path)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Quantify hue nuisance in TLV latent spaces.",
    )
    parser.add_argument(
        "--embeddings", required=True, type=Path,
        help="Path to .npy (single space) or .npz (multi-space) embeddings.",
    )
    parser.add_argument(
        "--metadata", required=True, type=Path,
        help="Path to CSV or Parquet with columns: media, average_hue, "
             "average_saturation, average_value.",
    )
    parser.add_argument(
        "--output-dir", type=Path, default=None,
        help="Directory for CSV summaries and scatter PNGs.",
    )
    parser.add_argument(
        "--spaces", nargs="+", default=None,
        help="Latent-space keys to process (NPZ only). Default: all keys.",
    )
    parser.add_argument(
        "--n-pcs", type=int, default=8,
        help="Number of PCA components to report (default: 8).",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true",
        help="Enable DEBUG logging.",
    )
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )

    embeddings = _load_embeddings(args.embeddings, args.spaces)
    metadata = _load_metadata(args.metadata)

    for space_name, M in sorted(embeddings.items()):
        logger.info("Processing space '%s'  shape=%s", space_name, M.shape)
        report = run_hue_nuisance_qc(
            M=M,
            metadata=metadata,
            space_name=space_name,
            n_pcs=args.n_pcs,
            output_dir=args.output_dir,
        )
        report_to_stdout(report)

    logger.info("Done.")


if __name__ == "__main__":
    main()
