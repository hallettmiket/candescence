#!/usr/bin/env python3
"""Export the manuscript latent-nuisance panels (hue + colony size).

Renders a 2-D projection of a latent space coloured by background hue and by
colony size, each annotated with the Mantel ``r``/``p`` that quantifies how
strongly the covariate structures the layout. This is the CLI twin of the
Streamlit "Export manuscript UMAP figures" button; both call the shared
renderer ``candescence.tlv.analysis.manuscript_figures.export_reference_umap_figures``.

Like the other analysis scripts in this repo (e.g. ``quantify_hue_nuisance.py``)
it is *precomputed-data driven*: it consumes embeddings + metadata that were
already encoded, rather than encoding a checkpoint on the fly. The projection
(PCA / UMAP / t-SNE) is fit here so the Mantel test runs on the exact 2-D
coordinates that are plotted.

Usage
-----
PCA (default; matches the manuscript model-quality tables)::

    python scripts/export_manuscript_umap_figures.py \
        --embeddings run_mu.npy \
        --metadata   run_metadata.csv \
        --output-dir results/manuscript_figures

Colony size comes from a metadata column (``--size-column``, default
``colony_size``) when present, or is computed from a preprocessed image stack::

    python scripts/export_manuscript_umap_figures.py \
        --embeddings run_mu.npz --space primary \
        --metadata   run_metadata.csv \
        --images     run_images.npy --size-threshold 0.5 \
        --projection umap \
        --output-dir results/manuscript_figures

Author: Candescence team
Date:   2026-06-22
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path
from typing import Optional

# Allow running from repo root without install.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

import numpy as np
import pandas as pd

from candescence.tlv.analysis.latent_space_metrics import (
    SkbioUnavailableError,
    mantel_latent_vs_numeric,
)
from candescence.tlv.analysis.manuscript_figures import (
    export_reference_umap_figures,
)

logger = logging.getLogger(__name__)

_AXIS_PREFIX = {"pca": "PC", "umap": "UMAP", "tsne": "t-SNE"}


def _load_embeddings(path: Path, space: Optional[str]) -> np.ndarray:
    """Load an (N, D) embedding array from .npy (single) or .npz (multi-space)."""
    path = Path(path)
    if path.suffix == ".npz":
        data = np.load(path)
        key = space or ("primary" if "primary" in data.files else data.files[0])
        if key not in data.files:
            raise ValueError(
                f"Space '{key}' not found in {path}. Available keys: {data.files}"
            )
        logger.info("Loaded embeddings space '%s' from %s", key, path)
        return data[key]
    arr = np.load(path)
    logger.info("Loaded embeddings from %s", path)
    return arr


def _load_metadata(path: Path) -> pd.DataFrame:
    """Load metadata from CSV or Parquet."""
    path = Path(path)
    if path.suffix == ".parquet":
        return pd.read_parquet(path)
    return pd.read_csv(path)


def _project(M: np.ndarray, method: str, seed: int) -> np.ndarray:
    """Fit a 2-D projection of *M*; mirrors the explorer's projection menu."""
    M = np.asarray(M, dtype=np.float64)
    if method == "pca":
        from sklearn.decomposition import PCA
        return PCA(n_components=2, random_state=seed).fit_transform(M)
    if method == "tsne":
        from sklearn.manifold import TSNE
        return TSNE(n_components=2, random_state=seed).fit_transform(M)
    if method == "umap":
        import umap  # umap-learn
        return umap.UMAP(n_components=2, random_state=seed).fit_transform(M)
    raise ValueError(f"Unknown projection '{method}'.")


def _size_vector(
    metadata: pd.DataFrame,
    size_column: str,
    images_path: Optional[Path],
    size_threshold: float,
    n_rows: int,
) -> Optional[np.ndarray]:
    """Resolve a per-image colony-size vector, or None if unavailable.

    Preference: explicit image stack (matches ``_get_size_proportion`` =
    fraction of pixels above the threshold) → metadata column → give up.
    """
    if images_path is not None:
        images = np.load(images_path)
        size = (images > size_threshold).reshape(images.shape[0], -1).mean(axis=1)
        logger.info("Computed colony size from %s (thresh=%.3f)", images_path, size_threshold)
        return size.astype(float)
    if size_column in metadata.columns:
        logger.info("Using colony size from metadata column '%s'", size_column)
        return metadata[size_column].to_numpy(dtype=float)
    logger.warning(
        "No colony-size source (no --images and no '%s' column); skipping size panel.",
        size_column,
    )
    return None


def _mantel_or_none(coords_2d, values, permutations, max_n, seed):
    """Run the Mantel test, degrading to None (no annotation) on failure."""
    if values is None:
        return None
    try:
        return mantel_latent_vs_numeric(
            coords_2d, values, permutations=permutations, max_n=max_n, seed=seed,
        )
    except SkbioUnavailableError:
        logger.warning("scikit-bio unavailable; figures rendered without Mantel annotation.")
        return None
    except Exception as exc:  # noqa: BLE001 - annotation is best-effort
        logger.warning("Mantel test failed (%s); rendering without annotation.", exc)
        return None


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Export manuscript latent-nuisance panels (hue + colony size).",
    )
    parser.add_argument(
        "--embeddings", required=True, type=Path,
        help="Path to .npy (single space) or .npz (multi-space) embeddings.",
    )
    parser.add_argument(
        "--metadata", required=True, type=Path,
        help="CSV/Parquet with an 'average_hue' column (and optionally a "
             "colony-size column).",
    )
    parser.add_argument(
        "--output-dir", required=True, type=Path,
        help="Directory for the PNG/PDF panels.",
    )
    parser.add_argument(
        "--space", default=None,
        help="Embedding key to use when --embeddings is an NPZ (default: "
             "'primary' or first key).",
    )
    parser.add_argument(
        "--projection", choices=("pca", "umap", "tsne"), default="pca",
        help="2-D projection to fit and plot (default: pca, matching the "
             "manuscript model-quality tables).",
    )
    parser.add_argument(
        "--size-column", default="colony_size",
        help="Metadata column holding colony size (used if --images is absent).",
    )
    parser.add_argument(
        "--images", type=Path, default=None,
        help="Optional .npy image stack (N, ...) to compute colony size as the "
             "fraction of pixels above --size-threshold (matches "
             "LearningDataset._get_size_proportion).",
    )
    parser.add_argument(
        "--size-threshold", type=float, default=0.5,
        help="Pixel threshold for colony size (default: 0.5).",
    )
    parser.add_argument(
        "--hue-column", default="average_hue",
        help="Metadata column holding the background hue (default: average_hue).",
    )
    parser.add_argument(
        "--permutations", type=int, default=999,
        help="Mantel permutations (default: 999).",
    )
    parser.add_argument(
        "--max-n", type=int, default=None,
        help="Optional subsample cap for the Mantel test (default: all rows).",
    )
    parser.add_argument(
        "--seed", type=int, default=42,
        help="Random seed for projection and Mantel permutations (default: 42).",
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

    M = _load_embeddings(args.embeddings, args.space)
    metadata = _load_metadata(args.metadata)

    if len(metadata) != M.shape[0]:
        raise ValueError(
            f"Row mismatch: embeddings have {M.shape[0]} rows but metadata has "
            f"{len(metadata)}. They must align one-to-one."
        )
    if args.hue_column not in metadata.columns:
        raise ValueError(
            f"Metadata is missing the hue column '{args.hue_column}'. "
            f"Available columns: {list(metadata.columns)}"
        )

    logger.info("Fitting %s projection on embeddings %s", args.projection.upper(), M.shape)
    coords_2d = _project(M, args.projection, args.seed)

    hue_vec = metadata[args.hue_column].to_numpy(dtype=float)
    size_vec = _size_vector(
        metadata, args.size_column, args.images, args.size_threshold, M.shape[0],
    )

    mantel_hue = _mantel_or_none(coords_2d, hue_vec, args.permutations, args.max_n, args.seed)
    mantel_size = _mantel_or_none(coords_2d, size_vec, args.permutations, args.max_n, args.seed)

    paths = export_reference_umap_figures(
        coords_2d=coords_2d,
        hue_values=hue_vec,
        size_values=size_vec,
        mantel_hue=mantel_hue,
        mantel_size=mantel_size,
        output_dir=args.output_dir,
        axis_prefix=_AXIS_PREFIX[args.projection],
    )

    for key, path in paths.items():
        logger.info("Wrote %s panel: %s", key, path)
    if mantel_hue is not None:
        logger.info("Mantel (hue):  r=%.3f  p=%.4f", mantel_hue.statistic, mantel_hue.p_value)
    if mantel_size is not None:
        logger.info("Mantel (size): r=%.3f  p=%.4f", mantel_size.statistic, mantel_size.p_value)
    logger.info("Done.")


if __name__ == "__main__":
    main()
