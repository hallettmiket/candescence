"""Manuscript reference figures for latent-space nuisance structure.

Shared renderer behind two callers:

* the Streamlit latent explorer
  (``_render_manuscript_figure_export`` in
  ``candescence.interface.apps.latent_explorer_app``), and
* the CLI ``scripts/export_manuscript_umap_figures.py``.

Both supply a 2-D projection of a latent space (``coords_2d``) together with a
per-image continuous covariate (background hue and/or colony size) and the
corresponding :class:`~candescence.tlv.analysis.latent_space_metrics.MantelResult`.
This module turns those into the two manuscript panels: a scatter of the 2-D
layout coloured by the covariate, annotated with the Mantel ``r``/``p`` that
quantifies how strongly the covariate structures the layout.

The renderer is deliberately projection-agnostic and computes nothing itself:
callers decide how ``coords_2d`` was produced (PCA / UMAP / t-SNE) and run the
Mantel test, so the figure always matches the statistic reported alongside it.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Dict, Optional, Sequence

import numpy as np

if TYPE_CHECKING:  # avoid importing at module load; only needed for type hints
    from candescence.tlv.analysis.latent_space_metrics import MantelResult

logger = logging.getLogger(__name__)


def _format_p(p: float) -> str:
    """Render a p-value the way the manuscript panels do.

    Permutation p-values bottom out at ``1/(permutations+1)``; show those as an
    upper bound rather than a misleading exact zero.
    """
    if p is None or not np.isfinite(p):
        return "n/a"
    if p < 1e-3:
        return "<0.001"
    return f"{p:.3f}"


def _mantel_annotation(mantel: "Optional[MantelResult]") -> Optional[str]:
    """Build the ``Mantel r=..., p=...`` annotation string, or None."""
    if mantel is None:
        return None
    return f"Mantel r={mantel.statistic:.2f}, p={_format_p(mantel.p_value)}"


def _render_panel(
    coords_2d: np.ndarray,
    values: np.ndarray,
    *,
    cmap: str,
    cbar_label: str,
    title: str,
    annotation: Optional[str],
    axis_prefix: str,
):
    """Draw one covariate-coloured scatter; returns a matplotlib Figure or None."""
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        logger.warning("matplotlib not installed; skipping figure render.")
        return None

    coords_2d = np.asarray(coords_2d, dtype=np.float64)
    values = np.asarray(values, dtype=np.float64)
    if coords_2d.ndim != 2 or coords_2d.shape[1] < 2:
        raise ValueError(
            f"coords_2d must be (N, >=2); got shape {coords_2d.shape!r}."
        )
    if values.shape[0] != coords_2d.shape[0]:
        raise ValueError(
            f"values length {values.shape[0]} != coords_2d rows "
            f"{coords_2d.shape[0]}."
        )

    fig, ax = plt.subplots(figsize=(7, 6))
    sc = ax.scatter(
        coords_2d[:, 0], coords_2d[:, 1],
        c=values, cmap=cmap, s=8, alpha=0.8,
    )
    fig.colorbar(sc, ax=ax, label=cbar_label)
    ax.set_xlabel(f"{axis_prefix} 1")
    ax.set_ylabel(f"{axis_prefix} 2")
    ax.set_title(title)
    if annotation is not None:
        ax.text(
            0.02, 0.98, annotation,
            transform=ax.transAxes, va="top",
            bbox=dict(
                boxstyle="round,pad=0.2",
                facecolor="white", alpha=0.7, linewidth=0,
            ),
        )
    fig.tight_layout()
    return fig


def _save(fig, output_dir: Path, stem: str, formats: Sequence[str], dpi: int) -> Path:
    """Save *fig* under *output_dir* in each format; return the first path."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    written = []
    for ext in formats:
        out_path = output_dir / f"{stem}.{ext}"
        fig.savefig(out_path, dpi=dpi, bbox_inches="tight")
        written.append(out_path)
        logger.info("Saved manuscript figure to %s", out_path)
    return written[0]


def export_reference_umap_figures(
    coords_2d: np.ndarray,
    hue_values: Optional[np.ndarray] = None,
    size_values: Optional[np.ndarray] = None,
    mantel_hue: "Optional[MantelResult]" = None,
    mantel_size: "Optional[MantelResult]" = None,
    output_dir: Path = Path("."),
    *,
    axis_prefix: str = "Component",
    hue_stem: str = "reference_umap_hue",
    size_stem: str = "reference_umap_size",
    formats: Sequence[str] = ("png", "pdf"),
    dpi: int = 200,
) -> Dict[str, Path]:
    """Render the manuscript nuisance panels (hue and/or colony size).

    Each panel scatters the 2-D latent layout coloured by the covariate and
    annotates it with the Mantel ``r``/``p`` describing how strongly that
    covariate structures the layout. Nothing is computed here: the caller
    supplies ``coords_2d``, the covariate vectors, and the Mantel results.

    Parameters
    ----------
    coords_2d
        ``(N, 2)`` projection of the latent space (PCA / UMAP / t-SNE — the
        caller's choice). The same coordinates the Mantel test was run on.
    hue_values, size_values
        ``(N,)`` per-image covariates. Either may be omitted (``None``) to skip
        that panel; at least one must be provided.
    mantel_hue, mantel_size
        Matching :class:`MantelResult` for the annotation. ``None`` renders the
        panel without an annotation.
    output_dir
        Directory to write into (created if needed).
    axis_prefix
        Axis-label prefix, e.g. ``"PC"`` for PCA or ``"UMAP"``. Defaults to the
        neutral ``"Component"`` so callers that mix projections are never wrong.
    hue_stem, size_stem
        Filename stems (without extension) for the two panels.
    formats
        Extensions to write (default PNG + PDF).
    dpi
        Raster resolution.

    Returns
    -------
    dict
        Maps ``"hue"`` and/or ``"size"`` to the primary (first-format) path
        written. Keys are present only for panels that were rendered.
    """
    if hue_values is None and size_values is None:
        raise ValueError("Provide at least one of hue_values or size_values.")

    paths: Dict[str, Path] = {}

    if hue_values is not None:
        fig = _render_panel(
            coords_2d, hue_values,
            cmap="coolwarm",
            cbar_label="background hue",
            title="Latent layout — background hue",
            annotation=_mantel_annotation(mantel_hue),
            axis_prefix=axis_prefix,
        )
        if fig is not None:
            paths["hue"] = _save(fig, output_dir, hue_stem, formats, dpi)
            import matplotlib.pyplot as plt
            plt.close(fig)

    if size_values is not None:
        fig = _render_panel(
            coords_2d, size_values,
            cmap="viridis",
            cbar_label="colony size (px frac)",
            title="Latent layout — colony size",
            annotation=_mantel_annotation(mantel_size),
            axis_prefix=axis_prefix,
        )
        if fig is not None:
            paths["size"] = _save(fig, output_dir, size_stem, formats, dpi)
            import matplotlib.pyplot as plt
            plt.close(fig)

    return paths
