"""
Purpose: Interactive Latent Space Explorer Streamlit Application
Author: Hallett Lab
Date: 2026-01-29

A standalone Streamlit app for interactive exploration of TLV model latent spaces.

Features:
- Click on data points to view real images
- Click on empty space to decode from that 2D position (k-NN weighted)
- Select two points to view interpolation between them
- Multiple projection methods (UMAP, t-SNE, PCA)
- Color points by metadata attributes

Launch with:
    streamlit run src/candescence/interface/apps/latent_explorer_app.py

Or from Python:
    from candescence.interface.apps import latent_explorer_main
    latent_explorer_main()
"""

import json
import pickle
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd
import streamlit as st
import torch

from candescence.core.logging_config import get_logger
from candescence.core.model_catalog import is_public_architecture
from candescence.core.settings import get_settings
from candescence.interface.core.components import (
    render_image_source_picker,
    render_research_mode_toggle,
    research_mode_enabled,
)
from candescence.interface.core.theme import page_header

_settings = get_settings()

logger = get_logger("candescence.interface.apps.latent_explorer")

# Stable color palette for morphology grades (consistent across runs/spaces)
MORPHOLOGY_PALETTE = {
    'white': '#1f77b4',
    'opaque': '#ff7f0e',
    'gray': '#2ca02c',
    'shmoo': '#d62728',
    'pseudohyphae': '#9467bd',
    'hyphae': '#8c564b',
    'unlabelled': '#cccccc',
}


def _hex_to_rgb(h: str) -> np.ndarray:
    """Convert ``#rrggbb`` to a uint8 RGB numpy array."""
    h = h.lstrip('#')
    return np.array([int(h[i:i + 2], 16) for i in (0, 2, 4)], dtype=np.uint8)


_UNLABELLED_TOKENS = {'', 'unlabelled', 'unlabeled', 'nan', 'none', 'na'}


def _normalize_label(label_val) -> Optional[str]:
    """Lower-cased string form of a label, or ``None`` if it represents 'no label'."""
    if label_val is None:
        return None
    try:
        s = str(label_val).strip().lower()
    except Exception:
        return None
    if s in _UNLABELLED_TOKENS:
        return None
    return s


def _build_label_color_map(labels: np.ndarray) -> Dict[str, np.ndarray]:
    """Assign an RGB color to each distinct labelled value.

    Known morphology names in :data:`MORPHOLOGY_PALETTE` keep their canonical hue;
    any other labels fall back to the plotly qualitative palette in a stable
    alphabetical order. Unlabelled / NaN values get no entry.
    """
    unique: List[str] = sorted({
        norm for v in labels if (norm := _normalize_label(v)) is not None
    })
    out: Dict[str, np.ndarray] = {}
    fallback_idx = 0
    for name in unique:
        if name in MORPHOLOGY_PALETTE and name != 'unlabelled':
            out[name] = _hex_to_rgb(MORPHOLOGY_PALETTE[name])
        else:
            hex_col = _PLOTLY_QUALITATIVE[fallback_idx % len(_PLOTLY_QUALITATIVE)]
            out[name] = _hex_to_rgb(hex_col)
            fallback_idx += 1
    return out

# Plotly qualitative fallback for non-morphology categorical columns
import plotly.express as px
_PLOTLY_QUALITATIVE = px.colors.qualitative.Plotly


def _is_categorical_column(series: pd.Series) -> bool:
    """Check if a pandas Series should be treated as categorical for coloring and stats.

    ``manual_formation`` and similar labels often load as int/float (especially float
    because of NaNs), not ``object`` — those must still count as categorical.
    """
    s = series
    if pd.api.types.is_bool_dtype(s):
        return True
    if (
        pd.api.types.is_object_dtype(s)
        or getattr(s.dtype, "name", "") == "category"
    ):
        return True
    if pd.api.types.is_string_dtype(s):
        return True

    nu = int(s.nunique(dropna=True))
    if nu < 2:
        return False

    if pd.api.types.is_integer_dtype(s):
        return nu <= 128

    if pd.api.types.is_float_dtype(s):
        # Float column with only whole numbers (e.g. grade codes + NaN) — common from CSV/Excel.
        if nu > 64:
            return False
        vals = s.dropna()
        if vals.empty:
            return False
        arr = vals.to_numpy(dtype=np.float64, copy=False)
        return bool(np.allclose(arr, np.round(arr), rtol=0, atol=1e-5))

    return False


# Columns that should be treated as label columns even if dtype is ambiguous
_LABEL_COLUMN_ALLOWLIST = {
    'manual_formation', 'formation', 'morphology', 'grade',
    'class', 'label', 'type', 'category', 'group',
    # Composite keys from _ensure_derived_metadata_columns (plate × media × day)
    'plate_media', 'plate_media_day',
}


def _metadata_label_columns(metadata_df: pd.DataFrame) -> List[str]:
    """Return categorical/label columns suitable for stratification and analysis.

    Combines ``_is_categorical_column`` detection with an allowlist for
    common morphology label column names.  Excludes ``id`` and columns
    that are all-NaN or have fewer than 2 unique non-null values.

    ``manual_formation`` is special-cased: if present with at least one
    non-null value, it is always listed so users can color by it even when
    every row is ``unlabelled`` (merge key mismatch) — otherwise the column
    would disappear from the UI.
    """
    # Columns holding non-scalar objects (images, tensors) are never labels
    _SKIP_COLS = {'id', 'transformed_image', 'rgb_image', 'hat_rgb_image',
                  'hat_hsv_image', 'hat_transformed_image'}
    result = []
    for c in metadata_df.columns:
        if c in _SKIP_COLS:
            continue
        s = metadata_df[c]
        # Skip columns whose first non-null value is an array-like (unhashable)
        first = s.dropna().iloc[0] if len(s.dropna()) > 0 else None
        if isinstance(first, (np.ndarray, list)):
            continue
        nu = int(s.nunique(dropna=True))
        if c == 'manual_formation':
            if nu >= 1:
                result.append(c)
            continue
        if nu < 2:
            continue
        if _is_categorical_column(s):
            result.append(c)
        elif c.lower() in _LABEL_COLUMN_ALLOWLIST and nu >= 2:
            # Allowlisted name with enough uniques — include even if dtype
            # didn't pass the general categorical check
            result.append(c)
    return result


def _order_label_columns_for_ui(cols: List[str]) -> List[str]:
    """Promote morphology and composite plate/media columns to the top of dropdowns."""
    if not cols:
        return cols
    priority = ('manual_formation', 'plate_media_day', 'plate_media')
    head: List[str] = []
    seen = set()
    for p in priority:
        if p in cols:
            head.append(p)
            seen.add(p)
    tail = [c for c in cols if c not in seen]
    return head + tail


def _normalize_merge_key(value) -> str:
    """Normalize a merge key to a stripped string stem for robust matching.

    Handles: int vs str, path-like filenames (strips directory + extension),
    extra whitespace.
    """
    if value is None:
        return ""
    s = str(value).strip()
    if not s:
        return ""
    # Strip directory and extension (e.g. "path/to/img_001.png" -> "img_001")
    p = Path(s)
    if p.suffix:
        return p.stem
    return s


def _merge_loaded_metadata(
    metadata_df: pd.DataFrame,
    loaded_metadata: pd.DataFrame,
) -> Tuple[pd.DataFrame, List[str]]:
    """Merge columns from *loaded_metadata* into *metadata_df* by ID matching.

    Tries direct key matching first, then falls back to stem-normalized
    matching if the direct match yields < 50% non-null rows.

    Returns
    -------
    metadata_df : updated DataFrame (mutated in place)
    merged_cols : list of column names that were successfully merged
    """
    _merge_cols = [
        c for c in loaded_metadata.columns
        if c not in metadata_df.columns and c not in ('id',)
    ]
    if not _merge_cols:
        return metadata_df, []

    # Find the best join column in loaded_metadata
    _match_col = None
    for candidate in ('image_filename', 'id', 'filename'):
        if candidate in loaded_metadata.columns:
            _match_col = candidate
            break
    if _match_col is None:
        logger.warning(
            "Metadata file has no 'image_filename', 'id', or 'filename' column; "
            f"available columns: {list(loaded_metadata.columns)}. Cannot merge."
        )
        return metadata_df, []

    # ── Attempt 1: direct string match ────────────────────────────────
    loaded_keys = loaded_metadata[_match_col].astype(str).str.strip()
    lookup_df = loaded_metadata.copy()
    lookup_df['_join_key'] = loaded_keys
    _lookup = lookup_df.set_index('_join_key')[_merge_cols]

    image_ids_str = metadata_df['id'].astype(str).str.strip()
    for col in _merge_cols:
        metadata_df[col] = image_ids_str.map(_lookup[col].to_dict())

    non_null_frac = metadata_df[_merge_cols].notna().any(axis=1).mean()

    # ── Attempt 2: stem-normalized match if direct match is poor ──────
    if non_null_frac < 0.5:
        logger.info(
            f"Direct merge matched only {non_null_frac:.0%} of rows; "
            "retrying with stem-normalized keys "
            f"(stripping directory/extension from '{_match_col}' and image IDs)."
        )
        norm_loaded_keys = loaded_keys.map(_normalize_merge_key)
        lookup_norm = loaded_metadata.copy()
        lookup_norm['_join_key'] = norm_loaded_keys
        # Drop duplicate stems (keep first)
        lookup_norm = lookup_norm.drop_duplicates(subset='_join_key', keep='first')
        _lookup_norm = lookup_norm.set_index('_join_key')[_merge_cols]

        norm_ids = image_ids_str.map(_normalize_merge_key)
        for col in _merge_cols:
            metadata_df[col] = norm_ids.map(_lookup_norm[col].to_dict())

        non_null_frac = metadata_df[_merge_cols].notna().any(axis=1).mean()
        if non_null_frac < 0.5:
            logger.warning(
                f"Stem-normalized merge still only matched {non_null_frac:.0%} of rows. "
                f"Check that '{_match_col}' values in the metadata file align with "
                f"image filenames. Examples — metadata keys: {norm_loaded_keys.head(3).tolist()}, "
                f"image IDs: {norm_ids.head(3).tolist()}"
            )

    # Report which columns survived
    _merged = metadata_df[_merge_cols].notna().any(axis=0)
    merged_cols = [c for c in _merge_cols if _merged[c]]
    _dropped = [c for c in _merge_cols if not _merged[c]]
    if merged_cols:
        logger.info(f"Merged metadata columns from file: {merged_cols}")
    if _dropped:
        logger.warning(
            f"Metadata columns all-NaN after merge (no matching IDs): {_dropped}"
        )
    return metadata_df, merged_cols


# Default path for lab VM layout; canonical source of truth is in core.config.
from candescence.core.config import DEFAULT_MANUALLY_LABELLED_IMAGES_CSV
DEFAULT_MANUAL_LABELS_CSV = str(DEFAULT_MANUALLY_LABELLED_IMAGES_CSV)


def merge_manual_labels_csv_into_metadata(
    metadata_df: pd.DataFrame,
    labels_path: Optional[Union[str, Path]],
    *,
    enabled: bool = True,
) -> pd.DataFrame:
    """Attach ``manual_formation`` by left-joining ``manually_labelled_images.csv``.

    Expects CSV columns ``file_name`` (or ``filename``) and ``morphology``, matching
    training/dataset conventions. Each row in ``metadata_df`` is matched on ``id``
    using :func:`_normalize_merge_key` (filename stem). Unmatched rows get
    ``\"unlabelled\"``.

    Parameters
    ----------
    metadata_df
        Must contain an ``id`` column whose values align with image stems used in
        the label file.
    labels_path
        Path to CSV. If missing or not a file, returns ``metadata_df`` unchanged.
    enabled
        If False, no-op.
    """
    if not enabled or metadata_df is None or metadata_df.empty:
        return metadata_df
    if "id" not in metadata_df.columns:
        logger.warning("merge_manual_labels: metadata_df has no 'id' column; skipping")
        return metadata_df
    if labels_path is None or str(labels_path).strip() == "":
        return metadata_df
    p = Path(labels_path).expanduser()
    if not p.is_file():
        logger.warning("merge_manual_labels: file not found: %s", p)
        return metadata_df

    raw = pd.read_csv(p)
    raw.columns = [str(c).strip().lower().replace(" ", "_") for c in raw.columns]
    fn_col = None
    for cand in ("file_name", "filename", "image_filename"):
        if cand in raw.columns:
            fn_col = cand
            break
    if fn_col is None:
        logger.warning(
            "merge_manual_labels: no file_name/filename column in %s; columns=%s",
            p,
            list(raw.columns),
        )
        return metadata_df
    if "morphology" not in raw.columns:
        logger.warning("merge_manual_labels: no morphology column in %s", p)
        return metadata_df

    lookup: Dict[str, str] = {}
    for _, row in raw.iterrows():
        fn = row[fn_col]
        if pd.isna(fn):
            continue
        key = _normalize_merge_key(fn)
        if not key:
            continue
        m = row["morphology"]
        if pd.isna(m):
            continue
        lookup[key] = str(m).strip()

    out = metadata_df.copy()
    labels: List[str] = []
    for val in out["id"]:
        k = _normalize_merge_key(val)
        labels.append(lookup.get(k, "unlabelled"))
    out["manual_formation"] = labels
    n_lab = sum(1 for x in labels if x != "unlabelled")
    logger.info(
        "merge_manual_labels: matched %s/%s rows from %s",
        n_lab,
        len(labels),
        p,
    )
    return out


def _maybe_merge_manual_labels_from_session(metadata_df: pd.DataFrame) -> pd.DataFrame:
    """Apply manual label CSV merge using sidebar session state."""
    enabled = st.session_state.get("merge_manual_labels", True)
    path = st.session_state.get("manual_labels_csv_path") or DEFAULT_MANUAL_LABELS_CSV
    return merge_manual_labels_csv_into_metadata(
        metadata_df, path, enabled=enabled
    )


def _no_labels_message(metadata_df: pd.DataFrame) -> str:
    """Build a helpful message when no label columns are found."""
    cols = [c for c in metadata_df.columns if c != 'id']
    msg = (
        "**No categorical label columns found in metadata.**\n\n"
        f"Current metadata columns: `{cols}`\n\n"
    )
    has_loaded = any(
        c not in ('id', 'average_hue', 'average_saturation', 'average_value')
        for c in metadata_df.columns
    )
    if not has_loaded:
        msg += (
            "Only conditioning columns are present — no label file was loaded or "
            "the merge found no matching rows. Check:\n"
            "- Enable **Manual morphology labels** in the sidebar and set the path to "
            "`manually_labelled_images.csv` (file_name + morphology).\n"
            "- Metadata file path in the model loader\n"
            "- That the file has an `image_filename`, `id`, or `filename` column "
            "whose values match image stems (e.g. `img_001`, not `path/to/img_001.png`)"
        )
    else:
        msg += (
            "Merged columns exist but none qualify as categorical. "
            "A label column needs ≥ 2 distinct non-null values."
        )
    return msg


# Media name normalisation (matches dataset.py conventions)
_MEDIA_NORMALIZE = {
    'spdr': 'spider', 'ctrl': 'control',
    'ypd': 'ypd', 'lees': 'lees', 'filament': 'filament',
}


def _media_from_filename(stem: str) -> Optional[str]:
    """Extract normalized media label from an image filename stem.

    Expects ``P{plate}_{media}_{condition}...`` convention.
    Returns ``None`` if the filename does not match.
    """
    parts = str(stem).split('_')
    if len(parts) < 3:
        return None
    raw = parts[1].lower()
    return _MEDIA_NORMALIZE.get(raw, raw)


def _day_from_filename(stem: str) -> Optional[str]:
    """Extract day token (``"2"``, ``"5"``, or ``"NA"``) from an image filename.

    Filenames use ``P{plate}_{media}_{condition}...`` where ``condition`` is
    either ``wash`` (no day) or ``day{N}``. Returns the stringified day
    value, or ``"NA"`` when the condition carries no day. Returns ``None``
    for filenames that don't match the convention at all.
    """
    parts = str(stem).split('_')
    if len(parts) < 3:
        return None
    cond_token = parts[2].lower()
    if cond_token == 'wash':
        return 'NA'
    if cond_token.startswith('day'):
        return cond_token[3:] or 'NA'
    return 'NA'


def _is_wash_filename(stem: str) -> Optional[bool]:
    """Return True if the filename's condition token is ``wash``, False otherwise.

    Returns ``None`` if the filename does not follow the
    ``P{plate}_{media}_{condition}...`` convention.
    """
    parts = str(stem).split('_')
    if len(parts) < 3:
        return None
    return parts[2].lower() == 'wash'


def _plate_phys_from_filename(stem: str) -> Optional[str]:
    """Extract ``plate_phys = plate:media:day`` from a filename stem.

    Mirrors the derivation in ``FullDataset._filenames_to_dataframe``:
    strip the leading ``P``/``Pl`` from the plate token, normalize
    media, and append the day token (``"2"``/``"5"``/``"NA"``) so that
    each (plate, medium, day) gets its own embedding — letting the
    plate_phys FiLM pathway also absorb the media-conditional
    day-on-hue interaction. Returns ``None`` when the filename
    doesn't match the ``P{plate}_{media}_{condition}...`` convention.
    """
    parts = str(stem).split('_')
    if len(parts) < 3:
        return None
    plate = parts[0]
    if plate.startswith('P') or plate.startswith('p'):
        plate = plate[1:]
    if plate.startswith('l') or plate.startswith('L'):
        plate = plate[1:]
    raw_media = parts[1].lower()
    media = _MEDIA_NORMALIZE.get(raw_media, raw_media)
    day = _day_from_filename(stem) or 'NA'
    return f"{plate}:{media}:{day}"


def _available_media_in_directory(images_path: str) -> List[str]:
    """Scan an images directory and return sorted unique media labels."""
    p = Path(images_path)
    if not p.is_dir():
        return []
    media_set: set = set()
    for f in p.iterdir():
        if f.suffix.lower() in ('.bmp', '.png'):
            m = _media_from_filename(f.stem)
            if m is not None:
                media_set.add(m)
    return sorted(media_set)


def _ensure_plate_media_columns(metadata_df: pd.DataFrame) -> bool:
    """Parse plate/media/day/replicate/row/col from the ``id`` column if not already present.

    Uses the same ``P{plate}_{media}_{condition}_{replicate}-r{row}-c{col}``
    convention as ``dataset.py._parse_filename``.

    Also derives ``my_rep`` (replicate grouping key) when plate, media, and
    position columns can be parsed: images sharing the same
    ``plate_media_day_row_col`` are biological replicates.

    Returns True if plate and media are available after the call.
    """
    if 'plate' in metadata_df.columns and 'media' in metadata_df.columns:
        # Still derive my_rep if missing
        _maybe_derive_my_rep(metadata_df)
        return True

    import re
    plates, medias, days = [], [], []
    replicates, rows, cols = [], [], []
    n_ok = 0
    for stem in metadata_df['id']:
        parts = str(stem).split('_')
        if len(parts) >= 3:
            plate = parts[0].lstrip('Pp').lstrip('l')
            raw_media = parts[1].lower()
            media = _MEDIA_NORMALIZE.get(raw_media, raw_media)
            cond = parts[2]
            day_match = re.match(r'day(\d+)', cond)
            day = int(day_match.group(1)) if day_match else None
            plates.append(plate)
            medias.append(media)
            days.append(day)

            # Parse replicate-row-col from 4th part if present
            if len(parts) >= 4:
                rep_part = parts[3]
                subparts = rep_part.split('-')
                try:
                    replicates.append(int(subparts[0]))
                    row = int(subparts[1].lstrip('rR')) if len(subparts) > 1 else None
                    col = int(subparts[2].lstrip('cC')) if len(subparts) > 2 else None
                    rows.append(row)
                    cols.append(col)
                except (ValueError, IndexError):
                    replicates.append(None)
                    rows.append(None)
                    cols.append(None)
            else:
                replicates.append(None)
                rows.append(None)
                cols.append(None)

            n_ok += 1
        else:
            plates.append(None)
            medias.append(None)
            days.append(None)
            replicates.append(None)
            rows.append(None)
            cols.append(None)

    if n_ok < len(metadata_df) * 0.5:
        logger.warning(
            f"Plate/media parsing succeeded for only {n_ok}/{len(metadata_df)} rows. "
            "Image IDs may not follow the P{{plate}}_{{media}}_{{condition}} convention."
        )
        return False

    metadata_df['plate'] = plates
    metadata_df['media'] = medias
    if any(d is not None for d in days):
        metadata_df['day'] = days
    if any(r is not None for r in replicates) and 'replicate' not in metadata_df.columns:
        metadata_df['replicate'] = replicates
    if any(r is not None for r in rows) and 'row' not in metadata_df.columns:
        metadata_df['row'] = rows
    if any(c is not None for c in cols) and 'column' not in metadata_df.columns:
        metadata_df['column'] = cols

    logger.info(
        f"Parsed plate/media from image IDs: "
        f"{metadata_df['plate'].nunique()} plates, "
        f"{metadata_df['media'].nunique()} media types"
    )

    _maybe_derive_my_rep(metadata_df)
    return True


def _maybe_derive_my_rep(metadata_df: pd.DataFrame) -> None:
    """Derive ``my_rep`` column if it doesn't exist.

    ``my_rep`` groups biological replicates — images that share everything
    except the replicate number.  The image ID convention is either:

    - ``P{plate}_{media}_{condition}_{replicate}-r{row}-c{col}``
      (with row/column position)
    - ``P{plate}_{media}_{condition}_{replicate}``
      (without row/column — e.g. ``Pl9_spider_day5_2``)

    In both cases the replicate number is the last component.  The grouping
    key is the ID with the replicate number stripped.

    Strategy:
    1. If ``row`` and ``column`` columns exist, build from
       ``plate + media + day + row + column``.
    2. Otherwise, strip the trailing ``_{replicate}`` (or ``_{rep}-r{}-c{}``)
       from the ``id`` string directly.
    """
    if 'my_rep' in metadata_df.columns:
        return

    # Strategy 1: full position key when row/column are available
    if all(c in metadata_df.columns for c in ('plate', 'media', 'row', 'column')):
        p = metadata_df['plate'].astype(str)
        m = metadata_df['media'].astype(str)
        r = metadata_df['row'].astype(str)
        c = metadata_df['column'].astype(str)
        if 'day' in metadata_df.columns:
            d = metadata_df['day'].fillna('wash').astype(str)
            metadata_df['my_rep'] = p + '_' + m + '_' + d + '_r' + r + '_c' + c
        else:
            metadata_df['my_rep'] = p + '_' + m + '_r' + r + '_c' + c
    else:
        # Strategy 2: strip the trailing replicate component from the ID.
        # ID format: "Pl9_spider_day5_2" → my_rep = "Pl9_spider_day5"
        # or "P12_ctrl_day2_1-r3-c4" → my_rep = "P12_ctrl_day2"
        import re

        def _strip_replicate(id_str: str) -> str:
            s = str(id_str)
            # Remove trailing _<digits>-r<digits>-c<digits> or _<digits>
            m = re.match(r'^(.+)_\d+(-r\d+-c\d+)?$', s)
            if m:
                return m.group(1)
            return s

        metadata_df['my_rep'] = metadata_df['id'].map(_strip_replicate)

    n_groups = metadata_df['my_rep'].nunique()
    n_pairs = int((metadata_df.groupby('my_rep').size() == 2).sum())
    logger.info(
        f"Derived my_rep: {n_groups} groups, {n_pairs} replicate pairs "
        f"(groups of exactly 2)"
    )


def _str_for_metadata_combo(x: Any) -> str:
    """Stringify plate/media values for composite label columns."""
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return "NA"
    return str(x).strip()


def _ensure_derived_metadata_columns(metadata_df: pd.DataFrame) -> None:
    """Add composite label columns ``plate_media`` and ``plate_media_day`` when possible.

    Requires ``plate`` and ``media`` (from merge or from :func:`_ensure_plate_media_columns`).
    ``plate_media`` is ``{plate}_{media}``; ``plate_media_day`` adds ``_day2`` / ``_day5`` /
    ``_wash`` when a ``day`` column exists. Overwrites these columns on each call so they
    stay aligned with base fields.
    """
    if metadata_df is None or metadata_df.empty:
        return
    _ensure_plate_media_columns(metadata_df)
    if 'plate' not in metadata_df.columns or 'media' not in metadata_df.columns:
        return
    p = metadata_df['plate'].map(_str_for_metadata_combo)
    m = metadata_df['media'].map(_str_for_metadata_combo)
    metadata_df['plate_media'] = p + '_' + m
    if 'day' in metadata_df.columns:
        def _day_label(val: Any) -> str:
            if val is None:
                return "wash"
            if isinstance(val, float) and np.isnan(val):
                return "wash"
            try:
                return f"day{int(val)}"
            except (TypeError, ValueError):
                return str(val)

        d = metadata_df['day'].map(_day_label)
        metadata_df['plate_media_day'] = p + '_' + m + '_' + d


def _sorted_unique_media(metadata_df: pd.DataFrame) -> List[str]:
    """Sorted unique string labels for ``media`` (drops NaN)."""
    if metadata_df is None or metadata_df.empty or "media" not in metadata_df.columns:
        return []
    s = metadata_df["media"].dropna()
    if s.empty:
        return []
    return sorted(s.astype(str).unique().tolist())


def _sync_media_subset_multiselect(metadata_df: pd.DataFrame) -> None:
    """When the table or media labels change, reset subset selection to all media."""
    if metadata_df is None or metadata_df.empty or "media" not in metadata_df.columns:
        return
    opts = _sorted_unique_media(metadata_df)
    sig = (len(metadata_df), tuple(opts))
    if st.session_state.get("_media_subset_sig") != sig:
        st.session_state._media_subset_sig = sig
        st.session_state.subset_media_multiselect = list(opts)


def _media_subset_mask(metadata_df: pd.DataFrame) -> np.ndarray:
    """Boolean mask of rows to show in scatter plots (True = draw point).

    If ``media`` is missing or all media are selected, all rows are visible.
    """
    if metadata_df is None:
        return np.array([], dtype=bool)
    n = len(metadata_df)
    if n == 0:
        return np.zeros(0, dtype=bool)
    if "media" not in metadata_df.columns:
        return np.ones(n, dtype=bool)
    sel = st.session_state.get("subset_media_multiselect")
    if sel is None:
        return np.ones(n, dtype=bool)
    opts = _sorted_unique_media(metadata_df)
    if not sel:
        return np.zeros(n, dtype=bool)
    if not opts or len(sel) >= len(opts):
        return np.ones(n, dtype=bool)
    med = metadata_df["media"].fillna("__missing__").astype(str)
    allowed = set(sel)
    return med.isin(allowed).values


def _dedupe_tendril_keys_preserve_order(keys: Optional[List[str]]) -> List[str]:
    """Return unique tendril keys in first-seen order.

    Duplicate keys (e.g. two ``"0"`` entries from the model) break Streamlit
    widget keys like ``mini_scatter_0`` and duplicate plots in the grid.
    """
    if not keys:
        return []
    seen: set = set()
    out: List[str] = []
    for k in keys:
        if k not in seen:
            seen.add(k)
            out.append(str(k))
    return out


def _global_indices_from_plotly_points(points: Any) -> List[int]:
    """Map Plotly selection points to global row indices using ``customdata`` when set."""
    out: List[int] = []
    if not points:
        return out
    for pt in points:
        if not isinstance(pt, dict):
            continue
        cd = pt.get("customdata")
        if cd is not None:
            arr = np.asarray(cd).ravel()
            if arr.size > 0 and np.isfinite(arr[0]):
                out.append(int(arr[0]))
                continue
        pi = pt.get("point_index")
        if pi is None:
            pi = pt.get("pointIndex")
        if pi is not None:
            out.append(int(pi))
    return out


def _build_categorical_marker(values: pd.Series, col_name: str) -> Tuple[list, dict]:
    """Build per-category traces for a categorical column.

    Returns (labels, color_map) where labels is a list of string labels
    and color_map maps each unique label to a color.
    """
    labels = values.fillna('unlabelled').astype(str).tolist()
    unique_labels = sorted(set(labels))

    # Use morphology palette if all labels are known morphology grades
    color_map = {}
    for i, label in enumerate(unique_labels):
        if label in MORPHOLOGY_PALETTE:
            color_map[label] = MORPHOLOGY_PALETTE[label]
        else:
            color_map[label] = _PLOTLY_QUALITATIVE[i % len(_PLOTLY_QUALITATIVE)]

    return labels, color_map


# Find logo path
LOGO_PATH = Path(__file__).parent.parent.parent.parent.parent / "assets" / "candescence-logo.png"

# Page configuration must be first - only when run standalone (not when imported by multipage app)
if __name__ == "__main__":
    st.set_page_config(
        page_title="TLV Explorer",
        page_icon=str(LOGO_PATH) if LOGO_PATH.exists() else "🔬",
        layout="wide",
        initial_sidebar_state="expanded"
    )


def main():
    """Main application entry point."""
    # Header matches the left-menu item ("TLV Explorer"), with TLV branding.
    page_header(
        "TLV Explorer",
        subproject="tlv",
        icon="🔬",
        description="Explore trained TLV (Tendril Latent VAE) latent spaces "
        "interactively.",
    )

    # Initialize session state
    _init_session_state()

    # Main content. Before any data is loaded, the whole setup experience lives
    # in the main window (the cramped sidebar made parameters unreadable); once
    # loaded, the explorer manages its own controls.
    if st.session_state.data_loaded:
        _render_explorer()
    else:
        _render_entry()


def _init_session_state():
    """Initialize session state variables."""
    defaults = {
        'data_loaded': False,
        'embeddings': None,
        'metadata_df': None,
        'images': None,
        'conditioning': None,
        'model': None,
        'coords_2d': None,
        'reduction_method': 'pca',
        'selected_points': [],  # List of tuples: ('data', index) or ('empty', (x, y, decoded_img, z, cond))
        'mu': None,
        'logvar': None,
        'color_by': None,
        'clicked_idx': None,  # Currently clicked/hovered point index
        'selected_region_indices': None,  # List of global row indices from box/lasso selection
        'selected_region_space': None,  # Which latent space the region selection came from
        # Feature 1: K-Means clustering
        'cluster_labels': None,
        'coloring_mode': 'Metadata',
        'n_clusters': 6,
        # Feature 2: Neighbor highlight
        'neighbor_indices': None,
        'neighbor_distances': None,
        # Feature 3: Dimension sweep
        'sweep_results': None,
        # Feature 4: Latent sliders
        'slider_z_values': None,
        'slider_base_index': 0,
        # Feature 5: Sprite map
        'sprite_map_image': None,
        # Training config from args.json (when loading from model)
        'training_config': None,
        # Multi-latent space support (tendrils)
        'active_latent_space': 'primary',
        'all_embeddings': {},
        'all_coords_2d': {},
        'all_mu': {},
        'all_logvar': {},
        'all_z': {},
        'tendril_keys': [],
        'use_stochastic_z': False,
        # PCA multi-component support
        'all_pca_scores': {},        # {space_name: (N, K) ndarray}
        'pca_explained_variance': {},  # {space_name: (K,) ndarray}
        'pca_x_component': 0,       # 0-based index (PC1 = 0)
        'pca_y_component': 1,       # 0-based index (PC2 = 1)
        # Manual morphology labels (manually_labelled_images.csv)
        'merge_manual_labels': True,
        'manual_labels_csv_path': DEFAULT_MANUAL_LABELS_CSV,
    }

    for key, default in defaults.items():
        if key not in st.session_state:
            st.session_state[key] = default


def _render_entry():
    """Render the main-window setup screen shown before any data is loaded.

    Two clear steps — pick a model, pick a dataset — plus a one-click button that
    loads Harry's thesis defaults (TLV Colony Image Corpus + newest Tendril VAE).
    Developer-grade loaders (inference objects, NPZ archives, demo data) are
    tucked behind an "Advanced" expander so they don't clutter the common path.
    """
    st.markdown(
        "Pick a **model** and a **dataset**, then load. New here? Use the "
        "one-click defaults."
    )

    # --- One-click startup (Harry's thesis setup) --------------------------- #
    col_btn, col_note = st.columns([1, 2])
    with col_btn:
        if st.button("Start exploring (TLV defaults)", type="primary",
                     use_container_width=True):
            _load_tlv_defaults()
    with col_note:
        st.caption(
            "Loads the newest **Tendril VAE** on the **TLV Colony Image Corpus** "
            "(Harry's thesis dataset) — no setup needed."
        )

    st.divider()

    # --- Step 1: model ------------------------------------------------------ #
    st.subheader("1 · Model")
    render_research_mode_toggle(location="main")
    model_path, family_key = _render_family_version_picker()

    st.divider()

    # --- Step 2: dataset ---------------------------------------------------- #
    st.subheader("2 · Dataset")
    images_path = render_image_source_picker(
        key_prefix="explorer_img",
        default_dir=str(_settings.image_dir),
        label="Images",
        project="tlv",
    )

    metadata_path = st.text_input(
        "Metadata file (optional)",
        value=str(_settings.metadata_xlsx),
        key="model_metadata_path",
        help="Spreadsheet of per-image attributes (hue, colony size, …) used for "
        "colouring points. Leave as-is to use the standard TLV metadata.",
    )

    max_samples = st.slider(
        "Max images to load",
        100, 5000, 1000,
        help="Caps how many images are encoded — lower is faster and lighter on "
        "memory.",
        key="max_samples",
    )

    # Optional filters (meaningful for the standard TLV corpus filenames).
    media_filter: Optional[List[str]] = None
    wash_choice = "Both"
    available_media = _available_media_in_directory(images_path)
    with st.expander("Filters (optional)", expanded=False):
        if available_media:
            media_filter = st.multiselect(
                "Growth media to include",
                options=available_media,
                default=available_media,
                key="load_media_filter",
                help="Only images from the selected media are loaded and encoded.",
            )
        wash_choice = st.radio(
            "Wash / non-wash images",
            options=["Both", "Non-wash only", "Wash only"],
            index=0,
            key="load_wash_filter",
            horizontal=True,
            help="Non-wash images are day2/day5; wash images carry 'wash' in the "
            "filename.",
        )
    wash_filter: Optional[bool] = None
    if wash_choice == "Non-wash only":
        wash_filter = False
    elif wash_choice == "Wash only":
        wash_filter = True

    with st.expander("Manual morphology labels (optional)", expanded=False):
        st.checkbox(
            "Merge manual labels into metadata after load",
            key="merge_manual_labels",
            help="Joins hand-labelled morphologies on image filename. Enables "
            "'Color by → manual_formation'. Rows not in the file become "
            "'unlabelled'.",
        )
        st.text_input(
            "Path to labels CSV (file_name + morphology columns)",
            key="manual_labels_csv_path",
        )

    st.divider()

    # --- Load --------------------------------------------------------------- #
    if family_key == "diffusion_vae":
        st.info(
            "Stable Diffusion models don't expose a latent encoder for the "
            "scatter view. Open **TLV Diffusion** to generate, reconstruct, and "
            "interpolate with these models."
        )
        try:
            st.page_link(
                "pages/12_TLV_Diffusion.py", label="Open TLV Diffusion", icon="✨"
            )
        except Exception:  # pragma: no cover - older Streamlit without page_link
            st.caption("Use the **TLV → Diffusion** page in the left menu.")
    else:
        if st.button("Load model + compute embeddings", type="primary",
                     disabled=not model_path):
            with st.spinner("Loading model and computing embeddings…"):
                try:
                    _load_from_model(
                        model_path, images_path, metadata_path, max_samples,
                        media_filter=media_filter, wash_filter=wash_filter,
                    )
                    st.session_state.data_loaded = True
                    st.rerun()
                except Exception as e:
                    st.error(f"Failed to load: {e}")
                    logger.exception("Failed to load model")

    # --- Advanced data sources ---------------------------------------------- #
    with st.expander("Advanced data sources", expanded=False):
        st.caption(
            "For pre-computed results or testing. Most users don't need these."
        )
        advanced = st.radio(
            "Source",
            ["Inference object (.pkl)", "Embeddings + images (.npz)", "Demo data"],
            key="advanced_load_source",
            horizontal=True,
        )
        if advanced == "Inference object (.pkl)":
            _render_inference_loader()
        elif advanced == "Embeddings + images (.npz)":
            _render_npz_loader()
        else:
            _render_demo_loader()


def _render_loaded_status():
    """Show a loaded-data summary + a reset button in the explorer sidebar.

    Migrated from the old sidebar loader so users keep the sample/latent-dim
    readout, the manual-label diagnostics, and a way to load different data.
    """
    st.success("Data loaded")
    st.metric("Samples", len(st.session_state.embeddings))
    st.metric("Latent dim", st.session_state.embeddings.shape[1])

    md = st.session_state.get("metadata_df")
    if md is not None and "manual_formation" in md.columns:
        vc = md["manual_formation"].astype(str).value_counts()
        n_lab = int((md["manual_formation"].astype(str) != "unlabelled").sum())
        st.caption(
            f"**manual_formation:** {n_lab}/{len(md)} rows labelled; "
            f"{len(vc)} distinct values. Set **Color by → manual_formation**."
        )

    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        model = st.session_state.model
        with st.expander(f"Tendril Info ({len(tendril_keys)} layers)",
                         expanded=False):
            if model is not None and model.has_tendrils():
                t_info = model.get_tendril_info()
                for key in tendril_keys:
                    info = t_info.get(key, {})
                    shape = info.get('input_shape', '?')
                    ldim = info.get('latent_dim', '?')
                    st.markdown(
                        f"**{key}**: {shape[0]}ch "
                        f"{shape[1]}x{shape[2]} -> {ldim}d latent"
                        if isinstance(shape, list) and len(shape) == 3
                        else f"**{key}**: latent_dim={ldim}"
                    )

    if st.button("Load different data", type="secondary", key="clear_data_btn"):
        _clear_data()
        st.rerun()


ZOO_BASE = _settings.zoo_path


def _model_architecture_hint(model_path: Path) -> Optional[str]:
    """
    Cheaply read a model's architecture from the sibling ``args.json``.

    Returns the architecture name, or ``None`` when no config is available
    (older runs often ship only ``model.pth``). Avoids loading the checkpoint.
    """
    config_path = model_path.parent / "args.json"
    if not config_path.is_file():
        return None
    try:
        with config_path.open() as handle:
            return json.load(handle).get("architecture")
    except (OSError, ValueError):
        return None


def _discover_models() -> List[str]:
    """
    Scan the model zoo for trained models, sorted newest first.

    In public mode (research mode off), models whose architecture is *known* to
    be research-tier are hidden; models with no discoverable architecture are
    kept (we cannot cheaply prove their tier).
    """
    if not ZOO_BASE.exists():
        return []

    model_files = sorted(
        ZOO_BASE.glob("*/*/models/model.pth"),
        key=lambda p: p.stat().st_mtime,
        reverse=True,
    )

    if not research_mode_enabled():
        def _is_visible(p: Path) -> bool:
            arch = _model_architecture_hint(p)
            return arch is None or is_public_architecture(arch)

        model_files = [p for p in model_files if _is_visible(p)]

    return [str(p) for p in model_files]


# User-facing model families, in display order. Each maps to the architecture
# name recorded in a model's args.json. Non-research users see only these four;
# research mode additionally surfaces every other discovered architecture.
_MODEL_FAMILIES: List[Tuple[str, str, str]] = [
    ("Tendril VAE", "tendril_vae",
     "Recommended — FiLM-conditioned VAE with per-layer ('tendril') latents."),
    ("VAE", "c_vae", "Basic convolutional VAE (simplest)."),
    ("UNet-VAE", "uc_vae", "U-Net VAE with skip connections."),
    ("Stable Diffusion", "diffusion_vae",
     "Conditional diffusion — explored on the separate TLV Diffusion page."),
]

# How an architecture name renders as a friendly family label.
_ARCH_TO_FAMILY: Dict[str, str] = {arch: name for name, arch, _ in _MODEL_FAMILIES}


def _format_version_label(model_path: str) -> str:
    """Label a trained run by its experiment/run, date, and key training options.

    Reads the sibling ``args.json`` once to surface what distinguishes one
    version of a family from another (conditioning, augmentation, adjustment,
    latent size), so users can tell otherwise-identical models apart.
    """
    p = Path(model_path)
    experiment = p.parent.parent.parent.name
    run = p.parent.parent.name
    label = f"{experiment} / {run}"

    try:
        mtime = datetime.fromtimestamp(p.stat().st_mtime)
        label += f" · {mtime:%Y-%m-%d}"
        if (datetime.now() - mtime).total_seconds() < 86400:
            label += " (new)"
    except OSError:
        pass

    config_path = p.parent / "args.json"
    if config_path.is_file():
        try:
            with config_path.open() as handle:
                cfg = json.load(handle)
        except (OSError, ValueError):
            cfg = {}
        tags: List[str] = []
        cond = cfg.get("conditional_variables") or []
        if cond:
            short = ",".join(str(c).replace("average_", "")[:3] for c in cond)
            tags.append(f"cond={short}")
        else:
            tags.append("uncond")
        if cfg.get("augment_images"):
            tags.append("+aug")
        if cfg.get("adjust_images"):
            tags.append("+adj")
        if cfg.get("latent_dim"):
            tags.append(f"z{cfg['latent_dim']}")
        if tags:
            label += "  ·  " + " ".join(tags)
    return label


def _models_by_family() -> Dict[str, List[str]]:
    """Group discovered model paths by architecture (read from args.json).

    Uses the same tier-filtered :func:`_discover_models` list, so research-only
    architectures are already excluded in public mode. Models with no
    discoverable architecture land under ``"(unknown)"``.
    """
    by_arch: Dict[str, List[str]] = {}
    for model_path in _discover_models():
        arch = _model_architecture_hint(Path(model_path)) or "(unknown)"
        by_arch.setdefault(arch, []).append(model_path)
    return by_arch


def _default_tendril_model() -> Optional[str]:
    """Return the newest Tendril VAE in the zoo (the 'recommended' default).

    Falls back to the newest model of any family, or ``None`` if the zoo is
    empty. No run id is hardcoded — it always tracks the latest good model.
    """
    models = _discover_models()  # newest-first
    for model_path in models:
        if _model_architecture_hint(Path(model_path)) == "tendril_vae":
            return model_path
    return models[0] if models else None


def _render_family_version_picker() -> Tuple[Optional[str], str]:
    """Two-level model picker: choose a family, then a trained version.

    Returns ``(model_path, family_key)``. ``model_path`` is ``None`` for the
    Stable Diffusion family (which is a redirect, not a load) or when no model is
    available. ``family_key`` is the architecture name of the chosen family.
    """
    by_arch = _models_by_family()

    # Build the family menu. Public mode: only the four curated families that
    # have at least one model (Stable Diffusion always shown — it's a redirect).
    # Research mode: append every other discovered architecture.
    family_options: List[Tuple[str, str]] = []  # (display label, arch key)
    for name, arch, _desc in _MODEL_FAMILIES:
        if arch == "diffusion_vae" or by_arch.get(arch):
            family_options.append((name, arch))
    if research_mode_enabled():
        for arch in sorted(by_arch):
            if arch not in _ARCH_TO_FAMILY:
                family_options.append((arch, arch))

    if not family_options:
        st.warning("No trained models found in the zoo.")
        return None, "(none)"

    col_pick, col_refresh = st.columns([4, 1])
    with col_refresh:
        st.write("")  # vertical nudge to align with the selectbox
        if st.button("Refresh", key="refresh_models_btn",
                     help="Rescan the zoo for new models"):
            st.rerun()
    with col_pick:
        labels = [name for name, _ in family_options]
        fam_idx = st.selectbox(
            "Model family",
            options=list(range(len(family_options))),
            format_func=lambda i: labels[i],
            key="model_family_select",
        )
    family_name, family_key = family_options[fam_idx]

    # Show the family's one-line description.
    desc = next((d for n, a, d in _MODEL_FAMILIES if a == family_key), "")
    if desc:
        st.caption(desc)

    # Stable Diffusion: redirect, no load path (handled by the caller).
    if family_key == "diffusion_vae":
        return None, "diffusion_vae"

    versions = by_arch.get(family_key, [])
    if not versions:
        st.warning(f"No trained {family_name} models found.")
        return None, family_key

    model_path = st.selectbox(
        f"{family_name} version",
        options=versions,  # already newest-first
        format_func=_format_version_label,
        key="model_version_select",
    )

    with st.expander("Or enter a custom model path"):
        custom_path = st.text_input(
            "Model path (.pth)", value="", key="custom_model_path_input"
        )
        if custom_path.strip():
            model_path = custom_path.strip()

    return model_path, family_key


def _load_tlv_defaults() -> None:
    """One-click startup: newest Tendril VAE on the TLV Colony Image Corpus.

    This is Harry's thesis setup — no parameters to fill in.
    """
    model_path = _default_tendril_model()
    if not model_path:
        st.error("No Tendril VAE found in the model zoo.")
        return

    # Resolve the canonical TLV corpus from the dataset registry, falling back to
    # the settings-resolved image dir if the registry entry is missing.
    images_path = str(_settings.image_dir)
    try:
        from candescence.core.dataset_zoo import DatasetZoo

        entry = DatasetZoo().get("tlv_images")
        if entry is not None and Path(entry.path).is_dir():
            images_path = entry.path
    except Exception:  # pragma: no cover - defensive
        logger.exception("Could not resolve tlv_images from the dataset zoo")

    with st.spinner("Loading TLV defaults (newest Tendril VAE on the colony "
                    "corpus)…"):
        try:
            _load_from_model(
                model_path, images_path, str(_settings.metadata_xlsx),
                max_samples=1000, media_filter=None, wash_filter=None,
            )
            st.session_state.data_loaded = True
            st.rerun()
        except Exception as e:
            st.error(f"Failed to load TLV defaults: {e}")
            logger.exception("Failed to load TLV defaults")


def _render_inference_loader():
    """Render inference object loader."""
    st.warning("⚠️ Old inference objects may have dependency issues. Prefer picking a model above instead.")

    inference_path = st.text_input(
        "Inference object path (.pkl)",
        value=f"{_settings.refined_path}/",
        key="inference_path"
    )

    if st.button("Load Inference Object", type="primary"):
        with st.spinner("Loading inference object..."):
            try:
                _load_from_inference(inference_path)
                st.session_state.data_loaded = True
                st.rerun()
            except Exception as e:
                st.error(f"Failed to load: {e}")
                logger.exception("Failed to load inference object")


def _render_npz_loader():
    """Render NPZ + images loader."""
    st.markdown("""
    **Load pre-extracted data files:**
    - Embeddings: latent vectors saved from inference
    - Images: original images for display
    - Metadata: sample attributes (hue, size, etc.)
    """)

    embeddings_path = st.text_input(
        "Embeddings file (.npz)",
        help="NumPy archive with 'embeddings' array of shape (N, latent_dim)",
        key="embeddings_path"
    )

    images_path = st.text_input(
        "Images file (.npy or .npz)",
        help="NumPy array of images, shape (N, C, H, W) or (N, H, W, C)",
        key="images_path"
    )

    metadata_path = st.text_input(
        "Metadata file (.csv, optional)",
        help="CSV with columns like 'id', 'average_hue', 'colony_size', etc.",
        key="metadata_path"
    )

    if st.button("Load Data", type="primary"):
        with st.spinner("Loading data..."):
            try:
                _load_from_files(embeddings_path, images_path, metadata_path)
                st.session_state.data_loaded = True
                st.rerun()
            except Exception as e:
                st.error(f"Failed to load: {e}")
                logger.exception("Failed to load data files")


def _render_demo_loader():
    """Render demo data loader for testing."""
    st.info("Load synthetic data for testing the interface")

    n_samples = st.slider("Number of samples", 100, 1000, 500, key="demo_n_samples")
    latent_dim = st.slider("Latent dimension", 8, 256, 128, key="demo_latent_dim")

    if st.button("Generate Demo Data", type="primary"):
        with st.spinner("Generating demo data..."):
            _generate_demo_data(n_samples, latent_dim)
            st.session_state.data_loaded = True
            st.rerun()


def _load_from_inference(path: str) -> None:
    """Load data from inference object pickle file."""
    import sys

    path = Path(path)

    # If directory, find first inference object
    if path.is_dir():
        pkl_files = list(path.rglob("inference_obj*.pkl"))
        if not pkl_files:
            raise FileNotFoundError(f"No inference objects found in {path}")
        path = pkl_files[0]
        logger.info(f"Found inference object: {path}")

    if not path.exists():
        raise FileNotFoundError(f"File not found: {path}")

    logger.info(f"Loading inference object from {path}")

    # Add the legacy candescence_master tree to sys.path so that *old* pickled
    # inference objects (created with the original codebase structure) can be
    # unpickled. This is a backward-compat shim only; the location is taken from
    # the CANDESCENCE_MASTER_REPO env var and silently skipped when absent, so
    # external installs are unaffected.
    from candescence.core.settings import master_repo_root

    master_repo = master_repo_root()
    master_paths = (
        [
            master_repo,
            master_repo / "projects",
            master_repo / "projects" / "tlv",
            master_repo / "projects" / "tlv" / "vae",
        ]
        if master_repo is not None
        else []
    )

    original_path = sys.path.copy()
    for mp in master_paths:
        if mp.exists() and str(mp) not in sys.path:
            sys.path.insert(0, str(mp))

    try:
        with open(path, 'rb') as f:
            inference_obj = pickle.load(f)
    finally:
        # Restore original path
        sys.path = original_path

    # Extract latent embeddings
    keys = inference_obj.get_latent_embedding_keys()
    if not keys:
        raise ValueError("No latent embeddings in inference object")

    le = inference_obj.get_latent_embedding(keys[0])
    le_df = le._latent_embedding

    # Extract embeddings
    latent_cols = [c for c in le_df.columns if c.startswith('latent_')]
    embeddings = le_df[latent_cols].values.astype(np.float32)

    # Extract metadata
    meta_cols = [c for c in le_df.columns if not c.startswith('latent_')]
    metadata_df = le_df[meta_cols].copy()

    # Add ID if not present
    if 'id' not in metadata_df.columns:
        metadata_df['id'] = range(len(metadata_df))

    # Add columns from inference df if available
    if hasattr(inference_obj, 'df'):
        for col in ['average_hue', 'average_saturation', 'average_value',
                    'colony_size', 'manual_formation', 'my_rep']:
            if col in inference_obj.df.columns and col not in metadata_df.columns:
                metadata_df[col] = inference_obj.df[col].values
        # Ensure manual_formation is never all-NaN (would be hidden from Color by)
        if 'manual_formation' in metadata_df.columns:
            metadata_df['manual_formation'] = metadata_df['manual_formation'].fillna('unlabelled')
        # Copy transformed_image for replicate diagnostics (per-row tensors)
        if 'transformed_image' in inference_obj.df.columns and 'transformed_image' not in metadata_df.columns:
            metadata_df['transformed_image'] = inference_obj.df['transformed_image'].values
        # Copy rgb_image (original RGB pixels) for UI display
        if 'rgb_image' in inference_obj.df.columns and 'rgb_image' not in metadata_df.columns:
            metadata_df['rgb_image'] = inference_obj.df['rgb_image'].values

    # Try to get images
    images = None
    if hasattr(inference_obj, 'df') and 'rgb_image' in inference_obj.df.columns:
        # Stack images into array
        images_list = inference_obj.df['rgb_image'].tolist()
        images = np.stack(images_list)
        logger.info(f"Loaded {len(images)} images")

    # Try to get conditioning
    conditioning = None
    if 'cond' in le_df.columns:
        conditioning = le_df['cond'].values.astype(np.float32)
        if len(conditioning.shape) == 1:
            conditioning = conditioning.reshape(-1, 1)

    metadata_df = _maybe_merge_manual_labels_from_session(metadata_df)

    # Populate transformed_image from images array if not already present
    # (needed for replicate diagnostics encoder pass)
    if 'transformed_image' not in metadata_df.columns and images is not None:
        metadata_df['transformed_image'] = [images[i] for i in range(len(images))]
        logger.info("Populated transformed_image column from loaded images array")

    st.session_state.embeddings = embeddings
    st.session_state.metadata_df = metadata_df
    st.session_state.images = images
    st.session_state.conditioning = conditioning
    st.session_state.coords_2d = None  # Will be recomputed
    st.session_state.training_config = None

    logger.info(f"Loaded {len(embeddings)} samples with {embeddings.shape[1]} latent dimensions")


def _load_from_model(
    model_path: str,
    images_path: str,
    metadata_path: Optional[str],
    max_samples: int = 1000,
    media_filter: Optional[List[str]] = None,
    wash_filter: Optional[bool] = None,
) -> None:
    """
    Load a trained model and compute embeddings from images.

    Args:
        model_path: Path to model.pth file
        images_path: Path to images directory or .npy file
        metadata_path: Optional path to metadata file
        max_samples: Maximum number of samples to process
        media_filter: If provided, only load images whose media matches one of
            these labels (parsed from filename). ``None`` or empty list means
            load all images.
        wash_filter: If ``True``, keep only wash images; if ``False``, keep only
            non-wash images; if ``None``, keep both. Determined from the
            condition token in the filename.
    """
    from candescence.interface.model_loader import load_tlv_model
    from PIL import Image as PILImage
    import json

    model_path = Path(model_path)
    images_path = Path(images_path)
    st.session_state.images_path = str(images_path)
    st.session_state.model_path = str(model_path)

    if not model_path.exists():
        raise FileNotFoundError(f"Model not found: {model_path}")

    # Load model
    logger.info(f"Loading model from {model_path}")
    model = load_tlv_model(model_path)

    # Load config for conditioning info
    config_path = model_path.parent / "args.json"
    conditional_variables = []
    training_config = None
    if config_path.exists():
        with open(config_path) as f:
            config = json.load(f)
        conditional_variables = config.get('conditional_variables', [])
        training_config = config
        logger.info(f"Model uses conditioning: {conditional_variables}")

    # Load metadata first (needed for conditioning)
    metadata_df = None
    if metadata_path and Path(metadata_path).exists():
        metadata_path = Path(metadata_path)
        logger.info(f"Loading metadata from {metadata_path}")

        if metadata_path.suffix == '.xlsx':
            metadata_df = pd.read_excel(metadata_path)
        else:
            metadata_df = pd.read_csv(metadata_path)

        # Create image filename column if needed
        if 'image_filename' not in metadata_df.columns and 'id' in metadata_df.columns:
            # Assume id can be used to match with images
            pass

    # Load config for fixed decoder values (e.g. average_hue=0.5 for decoder)
    fixed_decoder_values = {}
    if training_config:
        fixed_decoder_values = training_config.get('conditional_decoder_fixed_values', {})

    # Load images
    logger.info(f"Loading images from {images_path}")
    images_list = []
    image_ids = []
    conditioning_values = []
    per_image_hsv = []  # (hue, saturation, value) in 0-255 scale per image
    rgb_list = []  # original RGB uint8 (H, W, 3) for UI display, parallel to images_list

    # Strategies 15 & 16 use per-variable FiLM and require a conditioning
    # dict instead of a plain tensor. The active keys are read from
    # ``cond_keys`` in args.json (falls back to the full S15 set). Strategy
    # 16 drops ``day`` → no day_categories or day one-hots needed.
    _cfg_strategy = training_config.get('strategy') if training_config else None
    is_multi_cond = _cfg_strategy in (15, 16)
    is_strategy_15 = is_multi_cond  # legacy name; kept for downstream references
    if is_multi_cond:
        cond_keys = list(
            training_config.get('cond_keys')
            or (
                ['hue', 'media', 'plate_phys'] if _cfg_strategy == 16
                else ['hue', 'day', 'media']
            )
        )
    else:
        cond_keys = []
    use_day_film = 'day' in cond_keys
    use_media_film = 'media' in cond_keys
    use_plate_phys_film = 'plate_phys' in cond_keys
    day_categories = (
        list(training_config.get('day_categories', []) or [])
        if training_config else []
    )
    media_categories = (
        list(training_config.get('media_categories', []) or [])
        if training_config else []
    )
    plate_phys_categories = (
        list(training_config.get('plate_phys_categories', []) or [])
        if training_config else []
    )
    if is_multi_cond and use_day_film and not day_categories:
        raise ValueError(
            f"Strategy {_cfg_strategy} model is missing day_categories in "
            "args.json — cannot reconstruct one-hot conditioning. Re-save "
            "the checkpoint with the current training code (Factory._save_arguments)."
        )
    if is_multi_cond and use_media_film and not media_categories:
        raise ValueError(
            f"Strategy {_cfg_strategy} model is missing media_categories in "
            "args.json — cannot reconstruct one-hot conditioning. Re-save "
            "the checkpoint with the current training code (Factory._save_arguments)."
        )
    if is_multi_cond and use_plate_phys_film and not plate_phys_categories:
        raise ValueError(
            f"Strategy {_cfg_strategy} model is missing plate_phys_categories "
            "in args.json — cannot reconstruct one-hot conditioning. Re-save "
            "the checkpoint with the current training code (Factory._save_arguments)."
        )
    day_str_list: List[str] = []
    media_str_list: List[str] = []
    plate_phys_str_list: List[str] = []

    if images_path.is_dir():
        # Load from directory
        image_files = sorted(list(images_path.glob("*.bmp")) + list(images_path.glob("*.png")))

        if len(image_files) == 0:
            raise FileNotFoundError(f"No images found in {images_path}")

        # Filter by media before loading/encoding
        if media_filter:
            allowed = set(media_filter)
            before = len(image_files)
            image_files = [f for f in image_files if _media_from_filename(f.stem) in allowed]
            logger.info(
                f"Media filter {list(allowed)}: kept {len(image_files)}/{before} images"
            )
            if len(image_files) == 0:
                raise FileNotFoundError(
                    f"No images match the selected media filter {list(allowed)} "
                    f"in {images_path}"
                )

        # Filter by wash/non-wash before loading/encoding
        if wash_filter is not None:
            before = len(image_files)
            image_files = [
                f for f in image_files if _is_wash_filename(f.stem) == wash_filter
            ]
            label = "wash" if wash_filter else "non-wash"
            logger.info(
                f"Wash filter ({label}): kept {len(image_files)}/{before} images"
            )
            if len(image_files) == 0:
                raise FileNotFoundError(
                    f"No {label} images remain after filtering in {images_path}"
                )

        logger.info(f"Found {len(image_files)} images, loading up to {max_samples}")

        # Sample if too many
        if len(image_files) > max_samples:
            np.random.seed(42)
            indices = np.random.choice(len(image_files), max_samples, replace=False)
            image_files = [image_files[i] for i in sorted(indices)]

        for img_file in image_files:
            img = PILImage.open(img_file).convert('RGB')
            img = img.resize((128, 128))  # Resize to model input size

            # Capture original RGB pixels for UI display before any greyscale conversion
            rgb_list.append(np.array(img, dtype=np.uint8))

            # Compute per-image HSV from edge pixels (same method as dataset)
            img_hsv = img.convert('HSV')
            hsv_arr = np.array(img_hsv)
            dim = 128
            mask = np.zeros((dim, dim), dtype=bool)
            mask[:12, :] = True
            mask[-12:, :] = True
            mask[:, :12] = True
            mask[:, -12:] = True
            hue_vals = hsv_arr[..., 0][mask].astype(float)
            angles = hue_vals / 255.0 * 2 * np.pi
            mean_angle = np.arctan2(np.mean(np.sin(angles)), np.mean(np.cos(angles)))
            if mean_angle < 0:
                mean_angle += 2 * np.pi
            avg_hue = (mean_angle / (2 * np.pi)) * 255
            avg_sat = float(hsv_arr[..., 1][mask].mean())
            avg_val = float(hsv_arr[..., 2][mask].mean())
            per_image_hsv.append((avg_hue, avg_sat, avg_val))

            # Match training preprocessing
            encoder_img = img
            if training_config and training_config.get('grayscale', False):
                encoder_img = img.convert('L').convert('RGB')
                if training_config.get('grayscale_background_normalize', False):
                    from candescence.tlv.data.dataset import apply_grayscale_background_normalize
                    encoder_img = apply_grayscale_background_normalize(
                        encoder_img, 128,
                        target=training_config.get('grayscale_bg_target', 0.5),
                        border=int(training_config.get('grayscale_bg_border', 12)),
                    )

            img_array = np.array(encoder_img).astype(np.float32) / 255.0

            # Convert to channel-first format (C, H, W)
            img_array = np.transpose(img_array, (2, 0, 1))
            images_list.append(img_array)
            image_ids.append(img_file.stem)

            # Build conditioning from per-image HSV and fixed decoder values
            if conditional_variables:
                cond_values = []
                for var in conditional_variables:
                    if var == 'average_hue':
                        cond_values.append(avg_hue / 255.0)
                    elif var == 'average_saturation':
                        cond_values.append(avg_sat / 255.0)
                    elif var == 'average_value':
                        cond_values.append(avg_val / 255.0)
                    else:
                        cond_values.append(0.5)
                conditioning_values.append(cond_values)

            # Strategies 15/16 need per-image day/media/plate_phys strings
            # from filename. Strategy 16 skips day (no film_day) and adds
            # plate_phys.
            if is_multi_cond:
                if use_day_film:
                    day_str_list.append(_day_from_filename(img_file.stem) or 'NA')
                if use_media_film:
                    media_str = _media_from_filename(img_file.stem) or (
                        media_categories[0] if media_categories else 'control'
                    )
                    media_str_list.append(media_str)
                if use_plate_phys_film:
                    pp_str = _plate_phys_from_filename(img_file.stem) or (
                        plate_phys_categories[0] if plate_phys_categories else ''
                    )
                    plate_phys_str_list.append(pp_str)
    else:
        # Load from numpy file
        if images_path.suffix in ['.npy', '.npz']:
            data = np.load(images_path)
            if isinstance(data, np.lib.npyio.NpzFile):
                images_arr = data['images'] if 'images' in data else data[data.files[0]]
            else:
                images_arr = data

            # Sample if too many
            if len(images_arr) > max_samples:
                np.random.seed(42)
                indices = np.random.choice(len(images_arr), max_samples, replace=False)
                images_arr = images_arr[sorted(indices)]

            images_list = list(images_arr)
            image_ids = [f"sample_{i}" for i in range(len(images_list))]

            if conditional_variables:
                conditioning_values = [[0.5] * len(conditional_variables)] * len(images_list)

    images = np.stack(images_list)
    logger.info(f"Loaded {len(images)} images with shape {images.shape}")

    # Prepare conditioning tensor
    conditioning = None
    if conditioning_values:
        conditioning = np.array(conditioning_values, dtype=np.float32)
        logger.info(f"Conditioning shape: {conditioning.shape}")

    # Strategies 15/16: precompute one-hot day/media/plate_phys arrays
    # parallel to `images`. Strategy 16 produces media + plate_phys
    # one-hots (no day); Strategy 15 produces day + media.
    s15_day_onehot: Optional[np.ndarray] = None
    s15_media_onehot: Optional[np.ndarray] = None
    s16_plate_phys_onehot: Optional[np.ndarray] = None
    if is_multi_cond and (day_str_list or media_str_list or plate_phys_str_list):
        def _onehot(values: List[str], categories: List[str]) -> np.ndarray:
            out = np.zeros((len(values), len(categories)), dtype=np.float32)
            for i, v in enumerate(values):
                if v in categories:
                    out[i, categories.index(v)] = 1.0
            return out

        if use_day_film and day_str_list:
            s15_day_onehot = _onehot(day_str_list, day_categories)
        if use_media_film and media_str_list:
            s15_media_onehot = _onehot(media_str_list, media_categories)
        if use_plate_phys_film and plate_phys_str_list:
            s16_plate_phys_onehot = _onehot(
                plate_phys_str_list, plate_phys_categories,
            )
        logger.info(
            f"Strategy {_cfg_strategy} conditioning (cond_keys={cond_keys}): "
            f"hue={conditioning.shape if conditioning is not None else None}, "
            f"day={s15_day_onehot.shape if s15_day_onehot is not None else None}, "
            f"media={s15_media_onehot.shape if s15_media_onehot is not None else None}, "
            f"plate_phys={s16_plate_phys_onehot.shape if s16_plate_phys_onehot is not None else None}"
        )

    # Compute embeddings (and mu/logvar if supported)
    # Default to mu (posterior mean) for clustering parity with master;
    # z (stochastic sample) is cached separately for optional use.
    logger.info("Computing embeddings...")
    z_list = []
    mu_list = []
    logvar_list = []
    has_full_encoding = hasattr(model, 'encode_full') and model.encode_full is not None
    batch_size = 32
    use_z = st.session_state.get('use_stochastic_z', False)

    for i in range(0, len(images), batch_size):
        batch_images = torch.tensor(images[i:i+batch_size], dtype=torch.float32).to(model.device)

        batch_cond = None
        if is_multi_cond:
            hue_tensor = (
                torch.tensor(conditioning[i:i+batch_size], dtype=torch.float32).to(model.device)
                if conditioning is not None
                else torch.zeros((len(batch_images), 1), dtype=torch.float32, device=model.device)
            )
            batch_cond = {'hue': hue_tensor}
            if use_day_film and s15_day_onehot is not None:
                batch_cond['day'] = torch.tensor(
                    s15_day_onehot[i:i+batch_size], dtype=torch.float32
                ).to(model.device)
            if use_media_film and s15_media_onehot is not None:
                batch_cond['media'] = torch.tensor(
                    s15_media_onehot[i:i+batch_size], dtype=torch.float32
                ).to(model.device)
            if use_plate_phys_film and s16_plate_phys_onehot is not None:
                batch_cond['plate_phys'] = torch.tensor(
                    s16_plate_phys_onehot[i:i+batch_size], dtype=torch.float32
                ).to(model.device)
        elif conditioning is not None:
            batch_cond = torch.tensor(conditioning[i:i+batch_size], dtype=torch.float32).to(model.device)

        with torch.no_grad():
            if has_full_encoding:
                try:
                    z, mu, logvar = model.encode_full(batch_images, batch_cond)
                    z_list.append(z.cpu().numpy())
                    mu_list.append(mu.cpu().numpy())
                    logvar_list.append(logvar.cpu().numpy())
                except (RuntimeError, TypeError):
                    has_full_encoding = False
                    z = model.encode(batch_images, batch_cond)
                    z_list.append(z.cpu().numpy())
            else:
                z = model.encode(batch_images, batch_cond)
                z_list.append(z.cpu().numpy())

        if (i + batch_size) % 100 < batch_size:
            logger.info(f"Processed {min(i + batch_size, len(images))}/{len(images)} images")

    z_all = np.vstack(z_list)
    # Use mu when available and not explicitly requesting z
    if has_full_encoding and mu_list and not use_z:
        embeddings = np.vstack(mu_list)
        logger.info(f"Using mu (posterior mean) for embeddings — shape: {embeddings.shape}")
    else:
        embeddings = z_all
        logger.info(f"Using z (stochastic sample) for embeddings — shape: {embeddings.shape}")

    # Create metadata dataframe — start from image IDs, merge loaded metadata columns
    loaded_metadata = metadata_df  # may be None
    metadata_df = pd.DataFrame({'id': image_ids})

    # Add per-image HSV values computed from the actual image pixels (0-255 scale)
    if per_image_hsv:
        hsv_arr = np.array(per_image_hsv)
        metadata_df['average_hue'] = hsv_arr[:, 0]
        metadata_df['average_saturation'] = hsv_arr[:, 1]
        metadata_df['average_value'] = hsv_arr[:, 2]
        logger.info(
            f"Per-image HSV computed: hue range [{hsv_arr[:, 0].min():.1f}, {hsv_arr[:, 0].max():.1f}], "
            f"nunique={len(np.unique(np.round(hsv_arr[:, 0], 1)))}"
        )

    # Store conditioning values used for encoder (real per-image values in 0-1 scale)
    # and fixed decoder values separately for reference
    if conditioning is not None and conditional_variables:
        for i, var in enumerate(conditional_variables):
            metadata_df[f"cond_{var}"] = conditioning[:, i]
        if fixed_decoder_values:
            for var, val in fixed_decoder_values.items():
                metadata_df[f"decoder_{var}"] = val

    # Attach original RGB pixels for UI display (parallel to images rows).
    # Set BEFORE the metadata merge so an external CSV cannot overwrite it
    # (_merge_loaded_metadata skips columns already present on metadata_df).
    if rgb_list and len(rgb_list) == len(metadata_df):
        metadata_df['rgb_image'] = rgb_list

    # Merge additional columns from loaded metadata (manual_formation, plate, etc.)
    if loaded_metadata is not None:
        metadata_df, _ = _merge_loaded_metadata(metadata_df, loaded_metadata)
    else:
        logger.info("No metadata file loaded — only per-image HSV and conditioning columns available.")

    metadata_df = _maybe_merge_manual_labels_from_session(metadata_df)

    # Store primary embeddings in multi-space dict
    all_embeddings = {'primary': embeddings.astype(np.float32)}
    all_z = {'primary': z_all.astype(np.float32)}
    all_mu = {}
    all_logvar = {}

    if has_full_encoding and mu_list:
        primary_mu = np.vstack(mu_list).astype(np.float32)
        primary_logvar = np.vstack(logvar_list).astype(np.float32)
        all_mu['primary'] = primary_mu
        all_logvar['primary'] = primary_logvar

    # Compute tendril embeddings if available
    tendril_keys = []
    if model.has_tendrils():
        tendril_keys = _dedupe_tendril_keys_preserve_order(model.list_tendril_keys())
        logger.info(f"Computing tendril embeddings for keys: {tendril_keys}")

        tendril_emb = {key: [] for key in tendril_keys}
        tendril_mu = {key: [] for key in tendril_keys}
        tendril_logvar = {key: [] for key in tendril_keys}

        for i in range(0, len(images), batch_size):
            batch_images = torch.tensor(
                images[i:i+batch_size], dtype=torch.float32
            ).to(model.device)

            batch_cond = None
            if is_multi_cond:
                hue_tensor = (
                    torch.tensor(
                        conditioning[i:i+batch_size], dtype=torch.float32
                    ).to(model.device)
                    if conditioning is not None
                    else torch.zeros((len(batch_images), 1), dtype=torch.float32, device=model.device)
                )
                batch_cond = {'hue': hue_tensor}
                if use_day_film and s15_day_onehot is not None:
                    batch_cond['day'] = torch.tensor(
                        s15_day_onehot[i:i+batch_size], dtype=torch.float32
                    ).to(model.device)
                if use_media_film and s15_media_onehot is not None:
                    batch_cond['media'] = torch.tensor(
                        s15_media_onehot[i:i+batch_size], dtype=torch.float32
                    ).to(model.device)
                if use_plate_phys_film and s16_plate_phys_onehot is not None:
                    batch_cond['plate_phys'] = torch.tensor(
                        s16_plate_phys_onehot[i:i+batch_size], dtype=torch.float32
                    ).to(model.device)
            elif conditioning is not None:
                batch_cond = torch.tensor(
                    conditioning[i:i+batch_size], dtype=torch.float32
                ).to(model.device)

            with torch.no_grad():
                _ = model.encode(batch_images, batch_cond)
                skip = model.get_last_skip()

                # === DIAGNOSTIC: Verify skip indexing on first batch ===
                if i == 0:
                    logger.info(f"=== SKIP INDEXING VERIFICATION ===")
                    logger.info(f"  Number of skip tensors: {len(skip)}")
                    logger.info(f"  Number of tendril keys: {len(tendril_keys)}")
                    for j_dbg, key_dbg in enumerate(tendril_keys):
                        s = skip[j_dbg]
                        logger.info(
                            f"  skip[{j_dbg}] (key='{key_dbg}'): shape={list(s.shape)}, "
                            f"min={s.min().item():.6f}, max={s.max().item():.6f}, "
                            f"mean={s.mean().item():.6f}, std={s.std().item():.6f}"
                        )
                    logger.info(f"==================================")

                for j, key in enumerate(tendril_keys):
                    _, mu_t, logvar_t, z_t = model.encode_tendril(key, skip[j], cond=batch_cond)
                    tendril_emb[key].append(z_t.cpu().numpy())  # z cached for optional use
                    tendril_mu[key].append(mu_t.cpu().numpy())
                    tendril_logvar[key].append(logvar_t.cpu().numpy())

        # === DIAGNOSTIC: Log tendril embedding statistics ===
        logger.info("=== TENDRIL EMBEDDING DIAGNOSTICS ===")
        for key in tendril_keys:
            tendril_z = np.vstack(tendril_emb[key]).astype(np.float32)
            tendril_mu_arr = np.vstack(tendril_mu[key]).astype(np.float32)
            all_logvar[key] = np.vstack(tendril_logvar[key]).astype(np.float32)
            all_mu[key] = tendril_mu_arr
            all_z[key] = tendril_z
            # Default to mu for clustering; z available via toggle
            all_embeddings[key] = tendril_mu_arr if not use_z else tendril_z
            logger.info(f"Tendril '{key}' embeddings shape: {all_embeddings[key].shape}")

            z_arr = all_z[key]
            mu_arr = all_mu[key]
            lv_arr = all_logvar[key]
            logger.info(
                f"  '{key}' z:      mean={z_arr.mean():.6f}, std={z_arr.std():.6f}, "
                f"per-dim std range=[{z_arr.std(axis=0).min():.6f}, {z_arr.std(axis=0).max():.6f}]"
            )
            logger.info(
                f"  '{key}' mu:     mean={mu_arr.mean():.6f}, std={mu_arr.std():.6f}, "
                f"per-dim std range=[{mu_arr.std(axis=0).min():.6f}, {mu_arr.std(axis=0).max():.6f}]"
            )
            logger.info(
                f"  '{key}' logvar: mean={lv_arr.mean():.4f}"
            )
            if z_arr.std() < 1e-4:
                logger.warning(f"  '{key}' LIKELY COLLAPSED: near-zero variance ({z_arr.std():.6f})")
            if lv_arr.mean() < -10:
                logger.warning(f"  '{key}' LATENT COLLAPSE: logvar mean={lv_arr.mean():.2f} (posterior ≈ prior)")
        logger.info("======================================")

    # Populate transformed_image for replicate diagnostics encoder pass
    if 'transformed_image' not in metadata_df.columns:
        images_f32 = images.astype(np.float32)
        metadata_df['transformed_image'] = [images_f32[i] for i in range(len(images_f32))]
        logger.info("Populated transformed_image column from loaded images array")

    # Store in session state
    st.session_state.embeddings = all_embeddings['primary']
    st.session_state.metadata_df = metadata_df
    st.session_state.images = images.astype(np.float32)
    st.session_state.conditioning = conditioning
    # Names of the FiLM conditioning variables (e.g. ["average_hue", ...]) so
    # the latent-slider UI can offer per-variable conditioning sliders.
    st.session_state.conditional_variables = list(conditional_variables or [])
    st.session_state.model = model
    st.session_state.coords_2d = None  # Will be recomputed
    st.session_state.training_config = training_config

    # Multi-space state
    st.session_state.all_embeddings = all_embeddings
    st.session_state.all_z = all_z
    st.session_state.all_mu = all_mu
    st.session_state.all_logvar = all_logvar
    st.session_state.all_coords_2d = {}
    st.session_state.tendril_keys = tendril_keys
    st.session_state.active_latent_space = 'primary'

    # Store mu/logvar for posterior collapse diagnostics (active space)
    if 'primary' in all_mu:
        st.session_state.mu = all_mu['primary']
        st.session_state.logvar = all_logvar['primary']
        logger.info(f"Stored mu/logvar for posterior collapse diagnostics: shape {st.session_state.mu.shape}")
    else:
        st.session_state.mu = None
        st.session_state.logvar = None

    # Pre-compute 2D projections for all spaces
    method = st.session_state.get('reduction_method', 'pca')
    st.session_state.all_pca_scores = {}
    st.session_state.pca_explained_variance = {}
    for space_name, emb in all_embeddings.items():
        with st.spinner(f"Computing {method.upper()} for {space_name}..."):
            if method == 'pca':
                scores, ev = _fit_pca_full(emb)
                st.session_state.all_pca_scores[space_name] = scores
                st.session_state.pca_explained_variance[space_name] = ev
                st.session_state.all_coords_2d[space_name] = _slice_pca_coords(scores)
            else:
                coords = _project_embeddings(emb, method)
                st.session_state.all_coords_2d[space_name] = coords
    st.session_state.coords_2d = st.session_state.all_coords_2d.get('primary')

    logger.info(f"Successfully loaded model and computed embeddings for {len(embeddings)} samples")


def _load_from_files(
    embeddings_path: str,
    images_path: str,
    metadata_path: Optional[str]
) -> None:
    """Load data from separate files."""
    # Load embeddings
    embeddings_path = Path(embeddings_path)
    if not embeddings_path.exists():
        raise FileNotFoundError(f"Embeddings file not found: {embeddings_path}")

    if embeddings_path.suffix == '.npz':
        data = np.load(embeddings_path)
        embeddings = data['embeddings'] if 'embeddings' in data else data[data.files[0]]
    else:
        embeddings = np.load(embeddings_path)

    embeddings = embeddings.astype(np.float32)

    # Load images
    images = None
    if images_path:
        images_path = Path(images_path)
        if images_path.exists():
            if images_path.suffix == '.npz':
                data = np.load(images_path)
                images = data['images'] if 'images' in data else data[data.files[0]]
            else:
                images = np.load(images_path)

    # Load metadata
    if metadata_path and Path(metadata_path).exists():
        metadata_df = pd.read_csv(metadata_path)
    else:
        metadata_df = pd.DataFrame({'id': range(len(embeddings))})

    metadata_df = _maybe_merge_manual_labels_from_session(metadata_df)

    st.session_state.embeddings = embeddings
    st.session_state.metadata_df = metadata_df
    st.session_state.images = images
    st.session_state.conditioning = None
    st.session_state.coords_2d = None
    st.session_state.training_config = None


def _generate_demo_data(n_samples: int, latent_dim: int) -> None:
    """Generate synthetic demo data."""
    # Create structured embeddings (clusters)
    n_clusters = 5
    centers = np.random.randn(n_clusters, latent_dim) * 3
    cluster_labels = np.random.randint(0, n_clusters, n_samples)

    embeddings = np.zeros((n_samples, latent_dim), dtype=np.float32)
    for i, label in enumerate(cluster_labels):
        embeddings[i] = centers[label] + np.random.randn(latent_dim) * 0.5

    # Create synthetic metadata
    # HSV values use storage scale (0-255) consistent with real data from PIL HSV mode
    metadata_df = pd.DataFrame({
        'id': range(n_samples),
        'cluster': cluster_labels,
        'average_hue': np.random.rand(n_samples) * 255,  # Storage scale: 0-255
        'average_saturation': np.random.rand(n_samples) * 255,  # Storage scale: 0-255
        'average_value': np.random.rand(n_samples) * 255,  # Storage scale: 0-255
    })

    # Create synthetic images (colored squares based on metadata)
    images = np.zeros((n_samples, 3, 64, 64), dtype=np.float32)
    for i in range(n_samples):
        # Convert from storage scale (0-255) to 0-1 for HSV-to-RGB conversion
        hue = metadata_df.iloc[i]['average_hue'] / 255
        sat = metadata_df.iloc[i]['average_saturation'] / 255
        val = metadata_df.iloc[i]['average_value'] / 255

        # Simple HSV to RGB conversion
        c = val * sat
        x = c * (1 - abs((hue * 6) % 2 - 1))
        m = val - c

        if hue < 1/6:
            rgb = [c + m, x + m, m]
        elif hue < 2/6:
            rgb = [x + m, c + m, m]
        elif hue < 3/6:
            rgb = [m, c + m, x + m]
        elif hue < 4/6:
            rgb = [m, x + m, c + m]
        elif hue < 5/6:
            rgb = [x + m, m, c + m]
        else:
            rgb = [c + m, m, x + m]

        for ch in range(3):
            images[i, ch, :, :] = rgb[ch]

    st.session_state.embeddings = embeddings
    st.session_state.metadata_df = metadata_df
    st.session_state.images = images
    st.session_state.conditioning = None
    st.session_state.coords_2d = None
    st.session_state.training_config = None


def _clear_data():
    """Clear all loaded data."""
    st.session_state.data_loaded = False
    st.session_state.embeddings = None
    st.session_state.metadata_df = None
    st.session_state.images = None
    st.session_state.conditioning = None
    st.session_state.model = None
    st.session_state.coords_2d = None
    st.session_state.selected_points = []
    st.session_state.cluster_labels = None
    st.session_state.neighbor_indices = None
    st.session_state.neighbor_distances = None
    st.session_state.sweep_results = None
    st.session_state.slider_z_values = None
    st.session_state.sprite_map_image = None
    st.session_state.training_config = None
    # Multi-latent space state
    st.session_state.active_latent_space = 'primary'
    st.session_state.all_embeddings = {}
    st.session_state.all_coords_2d = {}
    st.session_state.all_mu = {}
    st.session_state.all_logvar = {}
    st.session_state.tendril_keys = []
    # PCA state
    st.session_state.all_pca_scores = {}
    st.session_state.pca_explained_variance = {}
    st.session_state.pca_x_component = 0
    st.session_state.pca_y_component = 1


def _render_training_params_section():
    """Render training parameters from args.json in an expander."""
    config = st.session_state.get('training_config')
    if config is None:
        # Show fallback only if a model was loaded (not embeddings/inference)
        if st.session_state.get('model') is not None:
            with st.expander("Training Parameters", expanded=False):
                st.caption("Training parameters not available (no args.json found for this model).")
        return

    # Keys to skip: internal paths, large objects, non-user-meaningful
    skip_keys = {
        'DEVICE', 'CANDESCENCE_ROOT', 'TLV', 'RAW_COLONY_IMAGES', 'METADATA',
        'MANUALLY_LABELLED_IMAGES', 'EXP', 'SAVE_PATH', 'ANALYSES', 'META_INFO',
        'LOSS', 'MODELS', 'GRID_SEARCH', 'IMAGES', 'AUGMENTED_IMAGES',
        'nicknames', '_random_indices', '_random_indices_spectrum',
    }

    # Group definitions: (group_name, key_list)
    groups = [
        ("Model", [
            'strategy', 'architecture', 'latent_dim', 'intermediate_dim',
            'conditional_variables', 'cond_dim', '_cond_dim',
        ]),
        ("Data", [
            'image_dimension', 'image_type', 'restrict_to_day', 'grayscale',
            'raw_images_path', 'day_filter', 'train_num', 'validation_num',
            'test_num', 'dataset_seed', 'merge_manually_labelled_images',
        ]),
        ("Training", [
            'number_epochs', 'number_epochs_vae', 'batch_size', 'vae_lr',
            'vae_loss_function', 'film_lr', 'weight_decay', 'training_seed',
            'kl_weight', 'MSE_weight', 'LPIPS_weight', 'SSIM_weight',
            'conditional_loss_weight', 'TC_weight',
            'kl_strategy_13_weights', 'kl_strategy_14_weights',
            'cycle_length', 'overall_rounds', 'number_epocs_discriminatory',
            'regressor_lr', 'regressor_mse_weight', 'regressor_loss_function',
            'regressor_penalty_weight', 'adversarial_prop',
        ]),
        ("Architecture Details", [
            'kernel_size', 'leaky_relu_slope', 'skip_weight',
            'reduction_ratio', 'spatial_reduction',
        ]),
        ("Tendril", [
            'tendril_num_epochs', 'tendril_batch_size', 'tendril_lr',
            'tendril_weight_decay', 'tendril_latent_dim', 'tendril_loss_fn',
            'tendril_MSE_weight', 'tendril_log_cosh_weight', 'tendril_KL_weight',
        ]),
        ("Augmentation", [
            'adjustment_variables', '_adjust_images', 'augmentation_variables',
            '_augment_images', '_augment_decoder_images', 'decoder_augmentation',
            'conditional_decoder_fixed_values',
        ]),
    ]

    # Collect grouped keys
    grouped_keys = set()
    for _, keys in groups:
        grouped_keys.update(keys)

    with st.expander("Training Parameters", expanded=False):
        # Strategy label at top
        strategy = config.get('strategy')
        arch = config.get('architecture', '')
        nicknames = config.get('nicknames', {})
        if strategy is not None:
            label = nicknames.get(str(strategy), arch)
            st.markdown(f"**Strategy {strategy}**: {label}")

        description = config.get('description')
        if description and description != 'experimental description':
            st.markdown(f"*{description}*")

        for group_name, keys in groups:
            # Only show group if at least one key is present
            present = {k: config[k] for k in keys if k in config}
            if not present:
                continue

            st.markdown(f"**{group_name}**")
            for key, value in present.items():
                # Format display value
                display_key = key.lstrip('_')
                if isinstance(value, dict):
                    st.text(f"  {display_key}:")
                    for k, v in value.items():
                        st.text(f"    {k}: {v}")
                elif isinstance(value, list) and len(value) > 6:
                    st.text(f"  {display_key}: [{len(value)} items]")
                else:
                    st.text(f"  {display_key}: {value}")

        # Ungrouped keys (anything not skipped and not already grouped)
        ungrouped = {
            k: v for k, v in config.items()
            if k not in grouped_keys and k not in skip_keys
            and k not in ('strategy', 'architecture', 'description')
        }
        if ungrouped:
            st.markdown("**Other**")
            for key, value in ungrouped.items():
                display_key = key.lstrip('_')
                st.text(f"  {display_key}: {value}")

        # Raw JSON download
        import json
        st.download_button(
            "Download args.json",
            data=json.dumps(config, indent=2, default=str),
            file_name="args.json",
            mime="application/json",
        )


def _render_explorer():
    """Render the main explorer interface."""
    if st.session_state.get('data_loaded') and st.session_state.get('metadata_df') is not None:
        _ensure_derived_metadata_columns(st.session_state.metadata_df)

    # Sidebar controls
    with st.sidebar:
        _render_loaded_status()
        st.divider()
        _render_visualization_controls()
        st.divider()
        _render_training_params_section()

    # Compute projection if needed
    if st.session_state.coords_2d is None:
        _compute_projection()

    # Verify we have coordinates
    if st.session_state.coords_2d is None:
        st.error("Failed to compute projection. Check that embeddings are valid.")
        return

    # Show workflow guide if in interpolation mode
    interp_active = st.session_state.get('use_plot_selection', False)
    if not interp_active:
        # Check tabbed keys for tendril models
        for tk in st.session_state.get('tendril_keys', []):
            if st.session_state.get(f'use_plot_selection_{tk}', False):
                interp_active = True
                break
        if not interp_active:
            interp_active = st.session_state.get('use_plot_selection_primary', False)
    if interp_active:
        with st.container():
            st.info("""
            **Interpolation Mode Active:**
            Scroll down to the **Interpolation** section and click on the dedicated plot to select start/end points!
            """)

    # Debug info (can be removed later)
    with st.expander("🔧 Debug Info (click if selection isn't working)", expanded=False):
        st.text(f"Embeddings shape: {st.session_state.embeddings.shape}")
        st.text(f"Coords 2D shape: {st.session_state.coords_2d.shape}")
        st.text(f"Metadata rows: {len(st.session_state.metadata_df)}")
        st.text(f"Images: {'loaded' if st.session_state.images is not None else 'none'}")
        st.text(f"Clicked idx: {st.session_state.get('clicked_idx', 'None')}")
        st.text(f"Selected points: {len(st.session_state.selected_points)}")
        st.text(f"Streamlit version: {st.__version__}")
        st.text(f"Use plot selection: {st.session_state.get('use_plot_selection', 'Not set')}")
        st.text(f"Active latent space: {st.session_state.get('active_latent_space', 'primary')}")
        st.text(f"Tendril keys: {st.session_state.get('tendril_keys', [])}")

    # Main layout
    col_left, col_right = st.columns([2, 1])

    with col_left:
        _render_scatter_plot()

    with col_right:
        _render_selection_panel()

    # Multi-space overview for tendril models
    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        st.divider()
        st.header("All Latent Spaces")
        st.caption(
            "Primary VAE latent space and all tendril VAE latent spaces. "
            "The large plot above shows the active space; this grid repeats all spaces "
            "for side-by-side comparison (Primary may appear in both)."
        )
        _render_all_latent_spaces()

    # Auto-compute k-NN after point selection (click-to-neighbors)
    _maybe_auto_compute_neighbors()

    # Neighbor highlight (thumbnails + controls) — placed here so click-to-neighbors
    # results are visible without scrolling past diagnostics sections.
    st.divider()
    _render_neighbor_highlight()

    # Cross-space neighbor comparison (tendrils only)
    st.divider()
    _render_cross_space_neighbor_panels()

    # Region metadata profile (box/lasso selection) — after All Latent Spaces
    # so it appears near whichever scatter the user selected from
    if st.session_state.get('selected_region_indices'):
        st.divider()
        _render_region_profile()

    # Cluster label enrichment analysis
    st.divider()
    _render_cluster_enrichment()

    # Silhouette & Mantel metrics
    st.divider()
    _render_silhouette_mantel()

    # Local label quality (k-NN purity & distance effect)
    st.divider()
    _render_local_label_metrics()

    # Rank change across latent spaces (unsupervised)
    st.divider()
    _render_rank_change_across_spaces()

    # Replicate & tendril layer diagnostics
    st.divider()
    _render_tendril_replicate_diagnostics()

    # Manuscript figure export (Fig 3A hue + Fig 3B colony size)
    st.divider()
    _render_manuscript_figure_export()

    # Sprite map (image mosaic)
    st.divider()
    _render_sprite_map()

    # Counterfactual conditioning swap diagnostic (Strategy 15 / 16)
    st.divider()
    from candescence.interface.tlv_explorer import render_counterfactual_swap_section
    render_counterfactual_swap_section(
        model=st.session_state.get('model'),
        metadata_df=st.session_state.metadata_df,
        images=st.session_state.images,
        conditioning=st.session_state.conditioning,
        clicked_idx_key='clicked_idx',
    )

    # Interpolation section (always show)
    st.divider()
    _render_interpolation()

    # Dimension sweep
    st.divider()
    _render_dimension_sweep()

    # Latent sliders
    st.divider()
    with st.expander("Latent Sliders", expanded=False):
        _render_latent_sliders()


def _switch_latent_space(space_name: str) -> None:
    """Switch the active latent space, updating embeddings and clearing projections."""
    all_emb = st.session_state.all_embeddings
    if space_name not in all_emb:
        return

    st.session_state.active_latent_space = space_name
    st.session_state.embeddings = all_emb[space_name]

    # Use cached 2D projection if available, else force recompute
    cached = st.session_state.all_coords_2d.get(space_name)
    if cached is not None:
        st.session_state.coords_2d = cached
    else:
        st.session_state.coords_2d = None

    # Update mu/logvar for posterior collapse diagnostics
    all_mu = st.session_state.all_mu
    all_logvar = st.session_state.all_logvar
    st.session_state.mu = all_mu.get(space_name)
    st.session_state.logvar = all_logvar.get(space_name)

    # Reset visualizations that depend on the embedding
    st.session_state.cluster_labels = None
    st.session_state.sweep_results = None
    st.session_state.slider_z_values = None
    # Sync unsuffixed neighbor keys from the new active space (if available)
    st.session_state.neighbor_indices = st.session_state.get(
        f'neighbor_indices_{space_name}'
    )
    st.session_state.neighbor_distances = st.session_state.get(
        f'neighbor_distances_{space_name}'
    )


def _render_visualization_controls():
    """Render visualization controls in sidebar."""
    st.header("Visualization")

    meta_df = st.session_state.get('metadata_df')
    if meta_df is not None and st.session_state.get('data_loaded'):
        _ensure_derived_metadata_columns(meta_df)

    # Subset scatter / grid by media (same convention as plate/media parsing from image IDs)
    if meta_df is not None and st.session_state.get('data_loaded'):
        _sync_media_subset_multiselect(meta_df)
        media_opts = _sorted_unique_media(meta_df)
        if media_opts:
            st.multiselect(
                "Show only these media (subset)",
                options=media_opts,
                key="subset_media_multiselect",
                help=(
                    "Restricts the main scatter and the All Latent Spaces grid to these "
                    "media. Projection (PCA / t-SNE / UMAP) is fit on the loaded points "
                    "(which may already be filtered by the media filter at load time)."
                ),
            )

    # Latent space selector (only show when tendrils are available)
    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        space_options = ['primary'] + tendril_keys
        active_space = st.selectbox(
            "Latent space",
            space_options,
            index=space_options.index(st.session_state.active_latent_space),
            format_func=lambda x: "Primary (mu)" if x == "primary" else f"Tendril: {x}",
            key="latent_space_select"
        )

        if active_space != st.session_state.active_latent_space:
            _switch_latent_space(active_space)
            st.rerun()

    # mu vs z toggle — default off (use mu for clustering parity with master)
    use_z = st.checkbox(
        "Use sampled z (stochastic)",
        value=st.session_state.get('use_stochastic_z', False),
        help="Default: mu (posterior mean) — deterministic, better for clustering. "
             "Enable to use z (reparameterized sample) instead.",
        key="use_stochastic_z_checkbox"
    )
    if use_z != st.session_state.get('use_stochastic_z', False):
        st.session_state.use_stochastic_z = use_z
        # Swap all_embeddings between mu and z caches
        all_z = st.session_state.get('all_z', {})
        all_mu = st.session_state.get('all_mu', {})
        for space_name in list(st.session_state.all_embeddings.keys()):
            if use_z and space_name in all_z:
                st.session_state.all_embeddings[space_name] = all_z[space_name]
            elif not use_z and space_name in all_mu:
                st.session_state.all_embeddings[space_name] = all_mu[space_name]
        # Refresh active embeddings and clear projections
        active = st.session_state.active_latent_space
        st.session_state.embeddings = st.session_state.all_embeddings.get(active, st.session_state.embeddings)
        st.session_state.coords_2d = None
        st.session_state.all_coords_2d = {}
        st.session_state.all_pca_scores = {}
        st.session_state.pca_explained_variance = {}
        st.session_state.cluster_labels = None
        st.rerun()

    method = st.selectbox(
        "Projection method",
        ["pca", "tsne", "umap"],
        index=["pca", "tsne", "umap"].index(st.session_state.reduction_method),
        key="reduction_method_select"
    )

    if method != st.session_state.reduction_method:
        st.session_state.reduction_method = method
        st.session_state.coords_2d = None  # Force recompute
        st.session_state.all_coords_2d = {}  # Clear all cached projections
        st.session_state.all_pca_scores = {}  # Clear PCA caches
        st.session_state.pca_explained_variance = {}
        st.session_state.pca_x_component = 0
        st.session_state.pca_y_component = 1
        st.session_state.cluster_labels = None  # Reset clusters on projection change
        st.rerun()

    # PCA axis selection (only when method is PCA)
    if method == 'pca':
        # Determine max K available across all fitted spaces
        all_pca_scores = st.session_state.get('all_pca_scores', {})
        max_k = 2
        for scores in all_pca_scores.values():
            max_k = max(max_k, scores.shape[1])

        pc_options = [f"PC{i+1}" for i in range(max_k)]

        col_x, col_y = st.columns(2)
        with col_x:
            pc_x = st.selectbox(
                "X axis",
                range(max_k),
                index=min(st.session_state.pca_x_component, max_k - 1),
                format_func=lambda i: pc_options[i],
                key="pca_x_select"
            )
        with col_y:
            pc_y = st.selectbox(
                "Y axis",
                range(max_k),
                index=min(st.session_state.pca_y_component, max_k - 1),
                format_func=lambda i: pc_options[i],
                key="pca_y_select"
            )

        if pc_x == pc_y:
            st.warning("X and Y axes must be different.")
        elif pc_x != st.session_state.pca_x_component or pc_y != st.session_state.pca_y_component:
            st.session_state.pca_x_component = pc_x
            st.session_state.pca_y_component = pc_y
            _apply_pca_axis_selection()
            st.session_state.cluster_labels = None
            st.rerun()

        # Show explained variance for active space
        active = st.session_state.get('active_latent_space', 'primary')
        ev = st.session_state.get('pca_explained_variance', {}).get(active)
        if ev is not None:
            ev_x = ev[pc_x] * 100 if pc_x < len(ev) else 0
            ev_y = ev[pc_y] * 100 if pc_y < len(ev) else 0
            st.caption(
                f"PC{pc_x+1}: {ev_x:.1f}% var | PC{pc_y+1}: {ev_y:.1f}% var"
            )
            with st.expander("Explained variance (all PCs)"):
                for i, v in enumerate(ev):
                    marker = " **←**" if i == pc_x or i == pc_y else ""
                    st.text(f"PC{i+1}: {v*100:.2f}%{marker}")

    # Coloring mode
    coloring_modes = ["Metadata", "K-Means (high-dim)", "K-Means (projected 2D)", "L2 norm", "Plain"]
    coloring_mode = st.radio(
        "Coloring mode",
        coloring_modes,
        index=coloring_modes.index(st.session_state.coloring_mode),
        key="coloring_mode_select"
    )
    st.session_state.coloring_mode = coloring_mode

    # Conditional controls based on coloring mode
    if coloring_mode == "Metadata":
        meta = st.session_state.metadata_df
        numeric_cols = meta.select_dtypes(
            include=[np.number]
        ).columns.tolist()
        categorical_cols = _order_label_columns_for_ui(_metadata_label_columns(meta))
        color_options = [None] + categorical_cols + numeric_cols

        # Fallback if previously selected column no longer exists
        prev = st.session_state.color_by
        default_idx = 0
        if prev in color_options:
            default_idx = color_options.index(prev)

        color_by = st.selectbox(
            "Color by",
            color_options,
            index=default_idx,
            format_func=lambda x: "None" if x is None else x,
            key="color_by_select"
        )
        st.session_state.color_by = color_by
        if any(c in categorical_cols for c in ('plate_media', 'plate_media_day')):
            st.caption(
                "Composite keys **plate_media** and **plate_media_day** (plate × agar × day) "
                "narrow coloring to a specific plate–media combination."
            )
    elif coloring_mode.startswith("K-Means"):
        n_clusters = st.slider(
            "Number of clusters (k)",
            min_value=2, max_value=20,
            value=st.session_state.n_clusters,
            key="n_clusters_slider"
        )
        st.session_state.n_clusters = n_clusters

    st.divider()

    st.header("Selection")
    st.text(f"Points selected: {len(st.session_state.selected_points)}")
    _ci = st.session_state.get('clicked_idx')
    if _ci is not None:
        st.caption(
            f"Neighbor query index (from plot): **{_ci}** — "
            "see **Neighbor Highlight** below for thumbnails."
        )

    target_space = st.session_state.get('neighbor_knn_target_space')
    if target_space and st.session_state.get('clicked_idx') is not None:
        st.caption(f"Last selection: index {st.session_state.clicked_idx} (space: {target_space})")
    st.caption("Auto-neighbor toggle is in the **Neighbor Highlight** section below.")

    if st.button("Clear Selection", type="secondary", key="clear_selection_btn"):
        st.session_state.selected_points = []
        st.session_state.clicked_idx = None
        st.session_state.neighbor_indices = None
        st.session_state.neighbor_distances = None
        st.session_state.neighbor_knn_target_space = None
        # Clear suffixed neighbor keys for all tendril spaces
        for tk in st.session_state.get('tendril_keys', []):
            st.session_state.pop(f'neighbor_indices_{tk}', None)
            st.session_state.pop(f'neighbor_distances_{tk}', None)
        st.session_state.pop('neighbor_indices_primary', None)
        st.session_state.pop('neighbor_distances_primary', None)
        st.session_state.pop('neighbor_multiselect_warning', None)
        st.rerun()


def _project_embeddings(embeddings: np.ndarray, method: str) -> np.ndarray:
    """Compute 2D projection of arbitrary embeddings array.

    Args:
        embeddings: Array of shape (n_samples, latent_dim).
        method: One of 'pca', 'tsne', or 'umap'.

    Returns:
        Array of shape (n_samples, 2) with 2D coordinates.
    """
    if method == 'pca':
        from sklearn.decomposition import PCA
        projector = PCA(n_components=2)
    elif method == 'tsne':
        from sklearn.manifold import TSNE
        projector = TSNE(
            n_components=2,
            random_state=42,
            perplexity=min(30, len(embeddings) - 1)
        )
    else:  # umap
        try:
            import umap
            projector = umap.UMAP(
                n_components=2,
                random_state=42,
                n_neighbors=15,
                min_dist=0.1
            )
        except ImportError:
            from sklearn.manifold import TSNE
            projector = TSNE(n_components=2, random_state=42)

    return projector.fit_transform(embeddings)


def _fit_pca_full(embeddings: np.ndarray, max_k: int = 32) -> Tuple[np.ndarray, np.ndarray]:
    """Fit PCA with up to max_k components and return all scores + explained variance.

    Args:
        embeddings: Array of shape (n_samples, latent_dim).
        max_k: Maximum number of principal components to retain.

    Returns:
        Tuple of (scores (N, K), explained_variance_ratio (K,)).
    """
    from sklearn.decomposition import PCA

    n_samples, latent_dim = embeddings.shape
    k = min(max_k, n_samples - 1, latent_dim)
    if k < 2:
        k = 2
    pca = PCA(n_components=k)
    scores = pca.fit_transform(embeddings)
    return scores, pca.explained_variance_ratio_


def _slice_pca_coords(scores: np.ndarray) -> np.ndarray:
    """Slice PCA scores to 2D using the currently selected PC axes.

    Args:
        scores: Array of shape (N, K) from _fit_pca_full.

    Returns:
        Array of shape (N, 2).
    """
    pc_x = st.session_state.pca_x_component
    pc_y = st.session_state.pca_y_component
    return scores[:, [pc_x, pc_y]]


def _apply_pca_axis_selection():
    """Re-slice all PCA scores for every space after axis change. No refit needed."""
    all_pca_scores = st.session_state.all_pca_scores
    for space_name, scores in all_pca_scores.items():
        st.session_state.all_coords_2d[space_name] = _slice_pca_coords(scores)
    active = st.session_state.get('active_latent_space', 'primary')
    st.session_state.coords_2d = st.session_state.all_coords_2d.get(active)


def _render_all_latent_spaces():
    """Render a grid of mini scatter plots for all latent spaces (primary + tendrils)."""
    import plotly.express as px

    all_embeddings = st.session_state.all_embeddings
    all_coords_2d = st.session_state.all_coords_2d
    _tk = st.session_state.get('tendril_keys', [])
    uq = _dedupe_tendril_keys_preserve_order(list(_tk))
    if uq != list(_tk):
        st.session_state.tendril_keys = uq
    tendril_keys = uq
    method = st.session_state.get('reduction_method', 'pca')
    metadata_df = st.session_state.metadata_df
    coloring_mode = st.session_state.coloring_mode
    color_by = st.session_state.color_by

    row_mask = _media_subset_mask(metadata_df)
    vis_idx = np.flatnonzero(row_mask)
    if vis_idx.size == 0:
        st.warning(
            "No points match the current media subset — cannot show the latent-space grid."
        )
        return
    n_vis = int(vis_idx.size)
    n_all = len(metadata_df)
    subset_only = n_vis < n_all
    if subset_only and "media" in metadata_df.columns:
        st.caption(
            f"Showing **{n_vis}** / {n_all} points (same media filter as the main plot)."
        )

    space_names = ['primary'] + _dedupe_tendril_keys_preserve_order(list(tendril_keys))

    # Ensure all projections are computed
    for space_name in space_names:
        if space_name not in all_coords_2d and space_name in all_embeddings:
            with st.spinner(f"Computing {method.upper()} for {space_name}..."):
                if method == 'pca':
                    scores, ev = _fit_pca_full(all_embeddings[space_name])
                    st.session_state.all_pca_scores[space_name] = scores
                    st.session_state.pca_explained_variance[space_name] = ev
                    coords = _slice_pca_coords(scores)
                else:
                    coords = _project_embeddings(all_embeddings[space_name], method)
                all_coords_2d[space_name] = coords
                st.session_state.all_coords_2d[space_name] = coords

    # Build color data for the plots
    color_col = None
    color_values = None
    colorscale = 'Viridis'
    multi_categorical = False
    multi_cat_labels = None
    multi_cat_color_map = None
    if coloring_mode == "Metadata" and color_by and color_by in metadata_df.columns:
        if _is_categorical_column(metadata_df[color_by]):
            multi_categorical = True
            multi_cat_labels, multi_cat_color_map = _build_categorical_marker(
                metadata_df[color_by], color_by
            )
            color_col = color_by
        else:
            color_values = metadata_df[color_by].values
            color_col = color_by
    elif coloring_mode == "L2 norm":
        color_col = "l2_norm"
    elif coloring_mode in ("K-Means (high-dim)", "K-Means (projected 2D)"):
        color_col = "cluster"
        colorscale = 'Rainbow'

    # Render in 3-column grid
    n_cols = 3
    active_space = st.session_state.get('active_latent_space', 'primary')

    for row_start in range(0, len(space_names), n_cols):
        row_spaces = space_names[row_start:row_start + n_cols]
        cols = st.columns(n_cols)
        for col_idx, space_name in enumerate(row_spaces):
            plot_slot = row_start + col_idx
            with cols[col_idx]:
                coords = all_coords_2d.get(space_name)
                if coords is None:
                    st.warning(f"No projection for {space_name}")
                    continue

                # Build dataframe for this plot (rows visible under media subset only)
                plot_df = pd.DataFrame({
                    'x': coords[vis_idx, 0],
                    'y': coords[vis_idx, 1],
                    '_global_idx': vis_idx,  # global row index for correct selection
                })

                # Determine color for this space
                plot_color = None
                if color_col == "l2_norm":
                    emb = all_embeddings.get(space_name)
                    if emb is not None:
                        plot_df['l2_norm'] = np.linalg.norm(emb[vis_idx], axis=1)
                        plot_color = 'l2_norm'
                elif color_col == "cluster":
                    from sklearn.cluster import KMeans
                    k = st.session_state.n_clusters
                    k_eff = min(k, max(1, n_vis)) if subset_only else k
                    if coloring_mode == "K-Means (high-dim)":
                        emb = all_embeddings.get(space_name)
                        if emb is not None:
                            labels = KMeans(
                                n_clusters=k_eff, random_state=42, n_init=10
                            ).fit_predict(emb[vis_idx])
                            plot_df['cluster'] = labels.astype(str)
                            plot_color = 'cluster'
                    else:
                        labels = KMeans(
                            n_clusters=k_eff, random_state=42, n_init=10
                        ).fit_predict(coords[vis_idx])
                        plot_df['cluster'] = labels.astype(str)
                        plot_color = 'cluster'
                elif color_col and color_col in metadata_df.columns:
                    if multi_categorical:
                        plot_df[color_col] = [multi_cat_labels[i] for i in vis_idx]
                    else:
                        plot_df[color_col] = metadata_df[color_col].values[vis_idx]
                    plot_color = color_col

                # Title
                if space_name == 'primary':
                    title = "Primary (z)"
                else:
                    title = f"Tendril: {space_name}"

                # Highlight active space
                if space_name == active_space:
                    title += " [active]"

                scatter_kwargs = dict(
                    data_frame=plot_df, x='x', y='y',
                    custom_data=['_global_idx'],
                    color=plot_color,
                    title=title,
                )
                if multi_categorical and plot_color == color_col:
                    scatter_kwargs['color_discrete_map'] = multi_cat_color_map
                    scatter_kwargs['category_orders'] = {color_col: sorted(multi_cat_color_map.keys())}
                elif color_col != 'cluster':
                    scatter_kwargs['color_continuous_scale'] = colorscale

                fig = px.scatter(**scatter_kwargs)
                fig.update_layout(
                    height=300,
                    margin=dict(l=10, r=10, t=35, b=10),
                    showlegend=False,
                    xaxis_title=None,
                    yaxis_title=None,
                )
                fig.update_traces(marker=dict(size=5, opacity=0.7))
                fig.update_layout(
                    clickmode='event+select',
                    dragmode='pan',
                )

                try:
                    event = st.plotly_chart(
                        fig,
                        use_container_width=True,
                        on_select="rerun",
                        selection_mode=["points", "box"],
                        key=f"mini_scatter_slot_{plot_slot}_{space_name}",
                    )

                    # Process selection event from mini scatter
                    if event is not None:
                        points = None
                        if hasattr(event, 'selection') and event.selection:
                            if hasattr(event.selection, 'points'):
                                points = event.selection.points
                            elif isinstance(event.selection, dict) and 'points' in event.selection:
                                points = event.selection['points']
                        if points is None and isinstance(event, dict):
                            if 'selection' in event and 'points' in event['selection']:
                                points = event['selection']['points']

                        if points and len(points) > 0:
                            global_indices = _global_indices_from_plotly_points(points)

                            if len(global_indices) == 1:
                                st.session_state.clicked_idx = global_indices[0]
                                st.session_state.selected_region_indices = None
                                st.session_state.selected_region_space = None
                                st.session_state.neighbor_knn_target_space = space_name
                                st.session_state.pop('neighbor_multiselect_warning', None)
                            elif len(global_indices) > 1:
                                st.session_state.selected_region_indices = global_indices
                                st.session_state.selected_region_space = space_name
                                st.session_state.neighbor_multiselect_warning = (
                                    f"Selected {len(global_indices)} points on {space_name} — "
                                    "select exactly **one** point for the neighborhood gallery."
                                )

                except (TypeError, AttributeError):
                    st.plotly_chart(
                        fig,
                        use_container_width=True,
                        key=f"mini_scatter_slot_{plot_slot}_{space_name}_fallback",
                    )

                # Button to switch active space
                if space_name != active_space:
                    if st.button("View", key=f"switch_{space_name}"):
                        _switch_latent_space(space_name)
                        st.rerun()


def _compute_projection():
    """Compute 2D projection of embeddings."""
    embeddings = st.session_state.embeddings
    method = st.session_state.reduction_method
    active = st.session_state.get('active_latent_space', 'primary')

    with st.spinner(f"Computing {method.upper()} projection..."):
        if method == 'pca':
            scores, ev = _fit_pca_full(embeddings)
            st.session_state.all_pca_scores[active] = scores
            st.session_state.pca_explained_variance[active] = ev
            coords_2d = _slice_pca_coords(scores)
        else:
            coords_2d = _project_embeddings(embeddings, method)

        st.session_state.coords_2d = coords_2d
        st.session_state.all_coords_2d[active] = coords_2d


def _render_scatter_plot():
    """Render the interactive scatter plot with click-to-select."""
    import plotly.graph_objects as go

    st.subheader("Latent Space")
    st.caption(
        "Explore the latent space — hover for index. Use the Plotly **Box Select** or "
        "**Lasso Select** tool and select **one point** to drive **Neighbor Highlight** "
        "below when auto-neighbors is on (sidebar → Selection)."
    )

    coords = st.session_state.coords_2d
    metadata_df = st.session_state.metadata_df
    color_by = st.session_state.color_by
    coloring_mode = st.session_state.coloring_mode

    # Verify data is valid
    if coords is None or len(coords) == 0:
        st.error("No coordinates to plot")
        return

    row_mask = _media_subset_mask(metadata_df)
    vis_idx = np.flatnonzero(row_mask)
    n_vis = int(vis_idx.size)
    n_all = len(coords)
    if "media" in metadata_df.columns and n_vis < n_all:
        st.caption(
            f"Media filter: showing **{n_vis}** / {n_all} points. "
            "Projection is fit on all loaded points (which may be pre-filtered at load time)."
        )
    if n_vis == 0:
        st.warning(
            "No points match the current media subset. "
            "Select at least one media in the sidebar multiselect."
        )
        return

    subset_only = n_vis < n_all
    emb = st.session_state.embeddings

    # Create figure with coloring mode
    categorical_metadata = False
    categorical_labels = None
    categorical_color_map = None
    if coloring_mode == "Metadata" and color_by and color_by in metadata_df.columns:
        if _is_categorical_column(metadata_df[color_by]):
            categorical_metadata = True
            categorical_labels, categorical_color_map = _build_categorical_marker(
                metadata_df[color_by], color_by
            )
            marker_config = None  # will use per-category traces below
        else:
            color_values = metadata_df[color_by].values
            marker_config = dict(
                size=10,
                color=color_values,
                colorscale='Viridis',
                showscale=True,
                colorbar=dict(title=color_by),
                opacity=0.8
            )
    elif coloring_mode == "K-Means (high-dim)":
        from sklearn.cluster import KMeans
        k = st.session_state.n_clusters
        if subset_only:
            k_eff = min(k, max(1, n_vis))
            labels = np.full(n_all, -1, dtype=int)
            labels[vis_idx] = KMeans(
                n_clusters=k_eff, random_state=42, n_init=10
            ).fit_predict(emb[vis_idx])
        else:
            labels = KMeans(
                n_clusters=k, random_state=42, n_init=10
            ).fit_predict(emb)
        st.session_state.cluster_labels = labels
        plot_colors = labels[vis_idx] if subset_only else labels
        marker_config = dict(
            size=10,
            color=plot_colors,
            colorscale='Rainbow',
            showscale=False,
            opacity=0.8
        )
    elif coloring_mode == "K-Means (projected 2D)":
        from sklearn.cluster import KMeans
        k = st.session_state.n_clusters
        if subset_only:
            k_eff = min(k, max(1, n_vis))
            labels = np.full(n_all, -1, dtype=int)
            labels[vis_idx] = KMeans(
                n_clusters=k_eff, random_state=42, n_init=10
            ).fit_predict(coords[vis_idx])
        else:
            labels = KMeans(
                n_clusters=k, random_state=42, n_init=10
            ).fit_predict(coords)
        st.session_state.cluster_labels = labels
        plot_colors = labels[vis_idx] if subset_only else labels
        marker_config = dict(
            size=10,
            color=plot_colors,
            colorscale='Rainbow',
            showscale=False,
            opacity=0.8
        )
    elif coloring_mode == "L2 norm":
        norms = np.linalg.norm(emb, axis=1)
        marker_config = dict(
            size=10,
            color=norms[vis_idx] if subset_only else norms,
            colorscale='Viridis',
            showscale=True,
            colorbar=dict(title='L2 norm'),
            opacity=0.8
        )
    else:
        # "Plain" or fallback
        marker_config = dict(
            size=10,
            color='steelblue',
            opacity=0.8
        )

    # Prepare hover template with coordinates (global index in label)
    hover_text: List[str] = []
    for i in range(len(coords)):
        text = f"Global index: {i}"
        text += f"<br>x: {coords[i, 0]:.3f}"
        text += f"<br>y: {coords[i, 1]:.3f}"
        if 'id' in metadata_df.columns:
            text += f"<br>ID: {metadata_df.iloc[i]['id']}"
        if color_by and color_by in metadata_df.columns:
            val = metadata_df.iloc[i][color_by]
            if isinstance(val, (int, float)) and not isinstance(val, bool):
                text += f"<br>{color_by}: {val:.3f}"
            else:
                text += f"<br>{color_by}: {val}"
        hover_text.append(text)

    fig = go.Figure()

    if categorical_metadata and categorical_labels is not None:
        # Add one trace per category for discrete legend
        labels_arr = np.array(categorical_labels)
        hover_arr = np.array(hover_text)
        for label in sorted(categorical_color_map.keys()):
            cat_mask = (labels_arr == label) & row_mask
            if not cat_mask.any():
                continue
            gidx = np.flatnonzero(cat_mask)
            fig.add_trace(go.Scatter(
                x=coords[gidx, 0],
                y=coords[gidx, 1],
                mode='markers',
                marker=dict(
                    size=10,
                    color=categorical_color_map[label],
                    opacity=0.8
                ),
                text=hover_arr[gidx].tolist(),
                customdata=gidx.reshape(-1, 1),
                hoverinfo='text',
                name=label
            ))
    else:
        cx = coords[vis_idx, 0]
        cy = coords[vis_idx, 1]
        mk = marker_config.copy()
        if coloring_mode == "Metadata" and color_by and color_by in metadata_df.columns:
            if not _is_categorical_column(metadata_df[color_by]) and "color" in mk:
                mk["color"] = np.asarray(metadata_df[color_by].values)[vis_idx]
        fig.add_trace(go.Scatter(
            x=cx,
            y=cy,
            mode='markers',
            marker=mk,
            text=[hover_text[i] for i in vis_idx],
            customdata=vis_idx.reshape(-1, 1),
            hoverinfo='text',
            name='samples'
        ))

    # Highlight clicked point (from session state)
    if 'clicked_idx' in st.session_state and st.session_state.clicked_idx is not None:
        idx = st.session_state.clicked_idx
        if 0 <= idx < n_all and row_mask[idx]:
            fig.add_trace(go.Scatter(
                x=[coords[idx, 0]],
                y=[coords[idx, 1]],
                mode='markers',
                marker=dict(
                    size=18,
                    color='orange',
                    symbol='circle-open',
                    line=dict(width=4)
                ),
                hoverinfo='skip',
                showlegend=False,
                name='clicked'
            ))

    # Highlight selected points (added to selection list)
    for point in st.session_state.selected_points:
        if point[0] == 'data':
            idx = point[1]
            if 0 <= idx < n_all and row_mask[idx]:
                fig.add_trace(go.Scatter(
                    x=[coords[idx, 0]],
                    y=[coords[idx, 1]],
                    mode='markers',
                    marker=dict(
                        size=15,
                        color='red',
                        symbol='circle-open',
                        line=dict(width=3)
                    ),
                    hoverinfo='skip',
                    showlegend=False
                ))
        elif point[0] == 'empty':
            x, y = point[1][0], point[1][1]
            fig.add_trace(go.Scatter(
                x=[x],
                y=[y],
                mode='markers',
                marker=dict(
                    size=15,
                    color='green',
                    symbol='x',
                    line=dict(width=3)
                ),
                text=[f"Empty Space Point<br>x: {x:.3f}<br>y: {y:.3f}<br>Type: k-NN interpolated"],
                hoverinfo='text',
                showlegend=False
            ))

    # Highlight neighbors (Feature 2) — only if visible under media filter
    # Resolve suffixed keys when in tendril mode so the overlay matches
    # whichever space neighbors were last computed for.
    active_space = st.session_state.get('active_latent_space', 'primary')
    _raw_nb = (
        st.session_state.get(f'neighbor_indices_{active_space}')
        or st.session_state.get('neighbor_indices')
    )
    neighbor_indices = None
    neighbor_distances: List[float] = []
    if _raw_nb:
        _nd_all = list(
            st.session_state.get(f'neighbor_distances_{active_space}')
            or st.session_state.get('neighbor_distances', [])
        )
        neighbor_indices = []
        neighbor_distances = []
        for j, i in enumerate(_raw_nb):
            ii = int(i)
            if 0 <= ii < n_all and row_mask[ii]:
                neighbor_indices.append(ii)
                neighbor_distances.append(
                    float(_nd_all[j]) if j < len(_nd_all) else 0.0
                )
    if neighbor_indices:
        nx = [coords[i, 0] for i in neighbor_indices]
        ny = [coords[i, 1] for i in neighbor_indices]
        n_text = [
            f"Neighbor idx={i}<br>dist={neighbor_distances[j]:.4f}"
            if j < len(neighbor_distances) else f"Neighbor idx={i}"
            for j, i in enumerate(neighbor_indices)
        ]
        fig.add_trace(go.Scatter(
            x=nx, y=ny,
            mode='markers',
            marker=dict(size=14, color='red', symbol='circle-open', line=dict(width=3)),
            text=n_text,
            hoverinfo='text',
            showlegend=False,
            name='neighbors'
        ))
        # Highlight the query point in green if clicked_idx is the source
        clicked_idx_for_nb = st.session_state.get('clicked_idx')
        if clicked_idx_for_nb is not None:
            fig.add_trace(go.Scatter(
                x=[coords[clicked_idx_for_nb, 0]],
                y=[coords[clicked_idx_for_nb, 1]],
                mode='markers',
                marker=dict(size=18, color='lime', symbol='star', line=dict(width=2, color='darkgreen')),
                hoverinfo='skip',
                showlegend=False,
                name='query_point'
            ))

    method_label = st.session_state.reduction_method.upper()
    if st.session_state.reduction_method == 'pca':
        pc_x = st.session_state.pca_x_component
        pc_y = st.session_state.pca_y_component
        x_label = f"PC{pc_x+1}"
        y_label = f"PC{pc_y+1}"
        title_label = f"Latent Space (PCA: {x_label} vs {y_label})"
    else:
        x_label = "Component 1"
        y_label = "Component 2"
        title_label = f"Latent Space ({method_label})"

    fig.update_layout(
        title=title_label,
        xaxis_title=x_label,
        yaxis_title=y_label,
        hovermode='closest',
        template='plotly_white',
        height=500,
        clickmode='event+select',  # Enable click events
        dragmode='pan'  # Use pan as default, selection via toolbar
    )

    # Use Streamlit's native selection feature
    try:
        # Try the newer on_select API (Streamlit >= 1.29)
        event = st.plotly_chart(
            fig,
            use_container_width=True,
            on_select="rerun",
            selection_mode=["points", "box", "lasso"],
            key="latent_scatter"
        )

        # Process selection event - check various possible structures
        if event is not None:
            # Debug: show what we got (expand Debug Info to see)
            with st.expander("Selection Event Debug", expanded=False):
                st.write("Event type:", type(event))
                st.write("Event:", event)

            # Try to extract selected points
            points = None

            # Method 1: event.selection.points (newer API)
            if hasattr(event, 'selection') and event.selection:
                if hasattr(event.selection, 'points'):
                    points = event.selection.points
                elif isinstance(event.selection, dict) and 'points' in event.selection:
                    points = event.selection['points']

            # Method 2: Direct dictionary access
            if points is None and isinstance(event, dict):
                if 'selection' in event and 'points' in event['selection']:
                    points = event['selection']['points']

            # Process points if we found any (customdata = global row index when set)
            if points and len(points) > 0:
                global_indices = _global_indices_from_plotly_points(points)

                if len(global_indices) == 1:
                    active_space = st.session_state.get('active_latent_space', 'primary')
                    st.session_state.clicked_idx = global_indices[0]
                    st.session_state.selected_region_indices = None
                    st.session_state.selected_region_space = None
                    st.session_state.neighbor_knn_target_space = active_space
                    st.session_state.pop('neighbor_multiselect_warning', None)
                elif len(global_indices) > 1:
                    active_space = st.session_state.get('active_latent_space', 'primary')
                    st.session_state.selected_region_indices = global_indices
                    st.session_state.selected_region_space = active_space
                    st.session_state.neighbor_multiselect_warning = (
                        f"Selected {len(global_indices)} points on {active_space} — "
                        "select exactly **one** point for the neighborhood gallery."
                    )

    except (TypeError, AttributeError) as e:
        # Fallback for older Streamlit versions or API issues
        st.plotly_chart(fig, use_container_width=True)
        st.info(f"Click selection unavailable. Using manual selection below.")

    # Manual selection fallback/alternative
    st.divider()
    _render_manual_selection()


def _render_manual_selection():
    """Render manual point selection."""
    st.subheader("Select Points")
    st.caption("💡 Hover over points in the plot to see their index, then enter it below to view details or save for later")

    col1, col2 = st.columns(2)

    with col1:
        st.markdown("**Select by Index** (real data point)")
        idx_input = st.number_input(
            "Point index (from hover)",
            min_value=0,
            max_value=len(st.session_state.embeddings) - 1,
            value=st.session_state.get('preview_idx', 0),
            key="manual_idx",
            help="Hover over a point in the plot to see its index"
        )

        # Store the preview index
        st.session_state.preview_idx = idx_input

        # Show preview image immediately
        st.markdown("**Preview:**")
        _render_preview_image(int(idx_input))

        # Check if already selected
        already_selected = any(p[0] == 'data' and p[1] == int(idx_input) 
                              for p in st.session_state.selected_points)
        
        button_label = "✓ Already Selected" if already_selected else "Add to Selection"
        
        if st.button(button_label, key="add_data_point", type="primary", disabled=already_selected):
            st.session_state.selected_points.append(('data', int(idx_input)))
            st.rerun()

    with col2:
        st.markdown("**Select by Coordinates** (empty space)")
        x_input = st.number_input("X coordinate", value=0.0, key="manual_x", format="%.3f")
        y_input = st.number_input("Y coordinate", value=0.0, key="manual_y", format="%.3f")
        
        st.caption("Enter coordinates from the plot axes")
        
        if st.button("Add Empty Space Point", key="add_empty_point"):
            decoded = _decode_from_2d(x_input, y_input)
            if decoded is not None:
                z_est, cond_est = _estimate_latent_from_2d(x_input, y_input)
                st.session_state.selected_points.append(
                    ('empty', (x_input, y_input, decoded, z_est, cond_est))
                )
                st.rerun()


def _render_preview_image(idx: int):
    """Render a preview image for the given index."""
    images = st.session_state.images
    coords = st.session_state.coords_2d

    if images is None:
        st.warning("No images loaded")
        return

    # Show coordinates
    x_coord = coords[idx, 0]
    y_coord = coords[idx, 1]
    st.caption(f"Index {idx} at ({x_coord:.3f}, {y_coord:.3f})")

    # Show image
    img = _image_array_for_display(idx, images)
    st.image(img, use_container_width=True)


def _handle_click(point: Dict[str, Any]):
    """Handle click event from plotly."""
    if 'pointIndex' in point:
        # Clicked on a data point
        idx = point['pointIndex']

        # Check if already selected
        for p in st.session_state.selected_points:
            if p[0] == 'data' and p[1] == idx:
                return  # Already selected

        if len(st.session_state.selected_points) < 2:
            st.session_state.selected_points.append(('data', idx))
            st.rerun()
    else:
        # Clicked on empty space
        x = point.get('x', 0)
        y = point.get('y', 0)

        if len(st.session_state.selected_points) < 2:
            # Decode from this position
            decoded = _decode_from_2d(x, y)
            z_est, cond_est = _estimate_latent_from_2d(x, y)

            st.session_state.selected_points.append(
                ('empty', (x, y, decoded, z_est, cond_est))
            )
            st.rerun()


def _estimate_latent_from_2d(
    x: float,
    y: float,
    k: int = 5
) -> Tuple[np.ndarray, Optional[np.ndarray]]:
    """Estimate latent vector from 2D coordinates using k-NN."""
    coords = st.session_state.coords_2d
    embeddings = st.session_state.embeddings
    conditioning = st.session_state.conditioning

    # Find k nearest neighbors
    point = np.array([[x, y]])
    distances = np.linalg.norm(coords - point, axis=1)
    nearest_indices = np.argsort(distances)[:k]

    # Compute weights (inverse distance)
    nearest_dists = distances[nearest_indices]
    weights = 1.0 / (nearest_dists + 1e-8)
    weights = weights / weights.sum()

    # Weighted average of latent vectors
    z_est = np.sum(
        embeddings[nearest_indices] * weights[:, np.newaxis],
        axis=0
    )

    # Weighted average of conditioning
    cond_est = None
    if conditioning is not None:
        cond_est = np.sum(
            conditioning[nearest_indices] * weights[:, np.newaxis],
            axis=0
        )

    return z_est, cond_est


def _decode_from_2d(x: float, y: float, k: int = 5) -> Optional[np.ndarray]:
    """Decode an image from 2D coordinates using k-NN weighted interpolation."""
    model = st.session_state.model
    images = st.session_state.images
    active_space = st.session_state.get('active_latent_space', 'primary')

    if model is None:
        # Without a model, return the nearest neighbor image
        if images is not None:
            coords = st.session_state.coords_2d
            point = np.array([[x, y]])
            distances = np.linalg.norm(coords - point, axis=1)
            nearest_idx = np.argmin(distances)
            return images[nearest_idx]
        return None

    z_est, cond_est = _estimate_latent_from_2d(x, y, k)

    cond_tensor = None
    if cond_est is not None:
        cond_tensor = torch.tensor(cond_est, dtype=torch.float32).unsqueeze(0).to(model.device)

    # Set skip connections using nearest neighbor
    nearest_idx = 0
    if images is not None:
        coords = st.session_state.coords_2d
        point = np.array([[x, y]])
        distances = np.linalg.norm(coords - point, axis=1)
        nearest_idx = np.argmin(distances)

        ref_img = torch.tensor(images[nearest_idx:nearest_idx+1], dtype=torch.float32).to(model.device)
        ref_cond = None
        if cond_est is not None:
            conditioning = st.session_state.conditioning
            ref_cond = torch.tensor(conditioning[nearest_idx:nearest_idx+1], dtype=torch.float32).to(model.device)

        with torch.no_grad():
            z_primary = model.encode(ref_img, ref_cond)

    if active_space != 'primary' and model.has_tendrils():
        # Tendril decode path: decode tendril z, replace skip, decode through main VAE
        z_tendril = torch.tensor(z_est, dtype=torch.float32).unsqueeze(0).to(model.device)
        skip = model.get_last_skip()
        with torch.no_grad():
            decoded = model.decode_with_tendril_modification(
                active_space, z_tendril, skip, z_primary, cond_tensor
            )
    else:
        # Primary decode path
        z_tensor = torch.tensor(z_est, dtype=torch.float32).unsqueeze(0).to(model.device)
        with torch.no_grad():
            decoded = model.decode(z_tensor, cond_tensor)

    return decoded.cpu().numpy().squeeze()


def _render_region_profile():
    """Render a metadata profile panel for a box/lasso-selected region of points.

    Sections: header with count + clear button, image gallery, categorical
    column summaries with fold enrichment, numeric column summaries, and
    top distinguishing features ranked by Jensen-Shannon divergence.
    """
    import plotly.graph_objects as go
    from scipy.spatial.distance import jensenshannon

    region_indices = st.session_state.get('selected_region_indices')
    if not region_indices:
        return

    metadata_df: pd.DataFrame = st.session_state.metadata_df
    images = st.session_state.images
    n_selected = len(region_indices)
    n_total = len(metadata_df)
    sel_df = metadata_df.iloc[region_indices]

    # ── Header + clear button ──────────────────────────────────────────
    region_space = st.session_state.get('selected_region_space', 'primary') or 'primary'
    space_label = "Primary (z)" if region_space == 'primary' else f"Tendril: {region_space}"
    st.subheader(f"Region Profile — {space_label}")
    col_info, col_clear = st.columns([3, 1])
    with col_info:
        st.markdown(f"**{n_selected}** points selected ({n_selected / n_total * 100:.1f}% of dataset)")
    with col_clear:
        if st.button("Clear Region", key="clear_region"):
            st.session_state.selected_region_indices = None
            st.session_state.selected_region_space = None
            st.rerun()

    # ── Image gallery ──────────────────────────────────────────────────
    if images is not None:
        max_thumbs = 30
        show_indices = region_indices if n_selected <= max_thumbs else list(
            np.random.default_rng(42).choice(region_indices, max_thumbs, replace=False)
        )
        with st.expander(f"Image Gallery ({min(n_selected, max_thumbs)} shown)", expanded=True):
            n_cols = 6
            for row_start in range(0, len(show_indices), n_cols):
                cols = st.columns(n_cols)
                for j, idx in enumerate(show_indices[row_start:row_start + n_cols]):
                    with cols[j]:
                        img = _image_array_for_display(idx, images)
                        st.image(img, caption=str(idx), use_container_width=True)

    # ── Categorical column summaries ───────────────────────────────────
    cat_cols = _order_label_columns_for_ui(_metadata_label_columns(metadata_df))
    jsd_scores: Dict[str, float] = {}

    if cat_cols:
        with st.expander("Categorical Column Summaries", expanded=True):
            for col_name in cat_cols:
                sel_counts = sel_df[col_name].value_counts(dropna=False)
                all_counts = metadata_df[col_name].value_counts(dropna=False)
                all_labels = all_counts.index.tolist()

                sel_pct = (sel_counts / n_selected * 100).reindex(all_labels, fill_value=0.0)
                all_pct = (all_counts / n_total * 100).reindex(all_labels, fill_value=0.0)

                # Fold enrichment (selection % / overall %, guarded against div-by-zero)
                fold = sel_pct / all_pct.replace(0, np.nan)

                summary = pd.DataFrame({
                    'Label': [str(l) for l in all_labels],
                    'Count (sel)': sel_counts.reindex(all_labels, fill_value=0).astype(int).values,
                    '% (sel)': sel_pct.values.round(1),
                    '% (all)': all_pct.values.round(1),
                    'Fold enrichment': fold.values.round(2),
                })

                # Jensen-Shannon divergence for ranking
                p = sel_pct.values / 100.0
                q = all_pct.values / 100.0
                # Ensure valid probability distributions
                p_sum = p.sum()
                q_sum = q.sum()
                if p_sum > 0 and q_sum > 0:
                    jsd_scores[col_name] = float(jensenshannon(p / p_sum, q / q_sum))
                else:
                    jsd_scores[col_name] = 0.0

                st.markdown(f"**{col_name}**")
                st.dataframe(summary, use_container_width=True, hide_index=True)

                # Flag notable enrichment / depletion (skip zero-count categories)
                enriched = summary[
                    (summary['Fold enrichment'] > 2.0) & (summary['Count (sel)'] > 0)
                ].sort_values('Fold enrichment', ascending=False)
                depleted = summary[
                    (summary['Fold enrichment'] < 0.5) & (summary['Count (sel)'] > 0)
                ].sort_values('Fold enrichment', ascending=True)

                max_show = 5
                flags = []
                for _, row in enriched.head(max_show).iterrows():
                    fe = row['Fold enrichment']
                    if pd.notna(fe):
                        flags.append(f"`{row['Label']}` {fe:.1f}x (enriched)")
                for _, row in depleted.head(max_show).iterrows():
                    fe = row['Fold enrichment']
                    if pd.notna(fe):
                        flags.append(f"`{row['Label']}` {fe:.1f}x (depleted)")

                omitted = max(0, len(enriched) - max_show) + max(0, len(depleted) - max_show)
                if flags:
                    msg = "Notable: " + " | ".join(flags)
                    if omitted > 0:
                        msg += f" | *…and {omitted} more*"
                    st.caption(msg)

    # ── Numeric column summaries ───────────────────────────────────────
    numeric_cols = [
        c for c in metadata_df.columns
        if c != 'id'
        and pd.api.types.is_numeric_dtype(metadata_df[c])
        and not _is_categorical_column(metadata_df[c])
        and metadata_df[c].notna().sum() > 0
    ]

    if numeric_cols:
        with st.expander("Numeric Column Summaries", expanded=False):
            rows = []
            for col_name in numeric_cols:
                sel_vals = sel_df[col_name].dropna()
                all_vals = metadata_df[col_name].dropna()
                if sel_vals.empty:
                    continue
                rows.append({
                    'Column': col_name,
                    'Mean (sel)': round(float(sel_vals.mean()), 4),
                    'Mean (all)': round(float(all_vals.mean()), 4),
                    'Median (sel)': round(float(sel_vals.median()), 4),
                    'Median (all)': round(float(all_vals.median()), 4),
                    'Std (sel)': round(float(sel_vals.std()), 4),
                    'Std (all)': round(float(all_vals.std()), 4),
                })
            if rows:
                st.dataframe(pd.DataFrame(rows), use_container_width=True, hide_index=True)

            # Histogram overlays for each numeric column
            for col_name in numeric_cols:
                sel_vals = sel_df[col_name].dropna().values
                all_vals = metadata_df[col_name].dropna().values
                if len(sel_vals) == 0:
                    continue
                fig_hist = go.Figure()
                fig_hist.add_trace(go.Histogram(
                    x=all_vals, name="All", opacity=0.5,
                    marker_color='#636EFA', histnorm='probability',
                ))
                fig_hist.add_trace(go.Histogram(
                    x=sel_vals, name="Selection", opacity=0.7,
                    marker_color='#EF553B', histnorm='probability',
                ))
                fig_hist.update_layout(
                    title=col_name, barmode='overlay',
                    height=250, margin=dict(l=40, r=20, t=40, b=30),
                    template='plotly_white', legend=dict(orientation='h'),
                )
                st.plotly_chart(fig_hist, use_container_width=True)

    # ── Top distinguishing features (by JSD) ──────────────────────────
    if jsd_scores:
        ranked = sorted(jsd_scores.items(), key=lambda kv: kv[1], reverse=True)
        top_n = min(3, len(ranked))
        top = ranked[:top_n]
        if top and top[0][1] > 0:
            st.markdown("**Top distinguishing categorical features:**")
            for rank, (col_name, jsd_val) in enumerate(top, 1):
                st.caption(f"{rank}. **{col_name}** (JSD = {jsd_val:.4f})")

    st.divider()


def _render_selection_panel():
    """Render the selection panel showing clicked and selected images."""
    # Show clicked point image prominently
    clicked_idx = st.session_state.get('clicked_idx')

    if clicked_idx is not None:
        st.subheader("Clicked Point")
        _render_data_point(clicked_idx)

        # Button to add to selection
        col1, col2 = st.columns(2)
        with col1:
            if st.button("Add to Selection", key="add_clicked_to_selection", type="primary"):
                # Check if not already in selection
                already_selected = any(
                    p[0] == 'data' and p[1] == clicked_idx
                    for p in st.session_state.selected_points
                )
                if not already_selected and len(st.session_state.selected_points) < 2:
                    st.session_state.selected_points.append(('data', clicked_idx))
                    st.rerun()
                elif already_selected:
                    st.warning("Point already in selection")
        with col2:
            if st.button("Clear Click", key="clear_click"):
                st.session_state.clicked_idx = None
                st.rerun()

        st.divider()

    # Show formal selection (for interpolation)
    if st.session_state.selected_points:
        st.subheader(f"Selection ({len(st.session_state.selected_points)}/2)")

        for i, point in enumerate(st.session_state.selected_points):
            with st.container():
                st.markdown(f"**Point {chr(65 + i)}**")  # A, B, C...

                if point[0] == 'data':
                    idx = point[1]
                    _render_data_point_compact(idx)
                else:  # empty
                    x, y, decoded, z, cond = point[1]
                    _render_empty_point(x, y, decoded)

                st.divider()
    elif clicked_idx is None:
        st.info("Click on a point in the scatter plot to view its image")


def _render_data_point(idx: int):
    """Render a selected data point with full details."""
    images = st.session_state.images
    metadata_df = st.session_state.metadata_df
    coords = st.session_state.coords_2d

    # Show coordinates prominently
    x_coord = coords[idx, 0]
    y_coord = coords[idx, 1]
    st.markdown(f"**Index:** {idx}")
    st.markdown(f"**Coordinates:** ({x_coord:.3f}, {y_coord:.3f})")

    # Show image if available (prominently)
    if images is not None:
        img = _image_array_for_display(idx, images)
        st.image(img, caption=f"Image {idx}", use_container_width=True)
    else:
        st.warning("No images available")

    # Show metadata in expander
    metadata = metadata_df.iloc[idx].to_dict()
    metadata['x_coord'] = x_coord
    metadata['y_coord'] = y_coord

    with st.expander("Metadata", expanded=False):
        for key, value in metadata.items():
            if isinstance(value, float):
                st.text(f"{key}: {value:.4f}")
            else:
                st.text(f"{key}: {value}")


def _render_data_point_compact(idx: int):
    """Render a selected data point in compact form (for selection list)."""
    images = st.session_state.images
    coords = st.session_state.coords_2d

    x_coord = coords[idx, 0]
    y_coord = coords[idx, 1]

    col1, col2 = st.columns([1, 2])
    with col1:
        if images is not None:
            img = _image_array_for_display(idx, images)
            st.image(img, width=100)
    with col2:
        st.text(f"Index: {idx}")
        st.text(f"({x_coord:.3f}, {y_coord:.3f})")


def _render_empty_point(x: float, y: float, decoded: Optional[np.ndarray]):
    """Render a selected empty space point."""
    st.text(f"Position: ({x:.3f}, {y:.3f})")
    st.text("Type: k-NN interpolated")

    if decoded is not None:
        img = _prepare_image(decoded)
        st.image(img, caption="Decoded image", use_container_width=True)
    else:
        st.warning("Could not decode image")


def _render_interpolation():
    """Render interpolation section with coordinate inputs."""
    st.subheader("Interpolation")
    st.caption("Interpolate between two points in the latent space.")

    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        space_names = ['primary'] + list(tendril_keys)
        tab_labels = ["Primary (z)"] + [f"Tendril: {k}" for k in tendril_keys]
        tabs = st.tabs(tab_labels)
        for tab, space_name in zip(tabs, space_names):
            with tab:
                _render_interpolation_for_space(
                    space_name=space_name,
                    embeddings=st.session_state.all_embeddings[space_name],
                    coords_2d=st.session_state.all_coords_2d.get(space_name),
                    key_suffix=f"_{space_name}",
                )
    else:
        _render_interpolation_for_space(
            space_name='primary',
            embeddings=st.session_state.embeddings,
            coords_2d=st.session_state.coords_2d,
            key_suffix="",
        )


def _render_interpolation_for_space(
    space_name: str,
    embeddings: np.ndarray,
    coords_2d: Optional[np.ndarray],
    key_suffix: str,
):
    """Render interpolation controls and results for a single latent space."""
    interp_start_key = f"interp_start{key_suffix}"
    interp_end_key = f"interp_end{key_suffix}"

    # Selection mode toggle
    use_plot_selection = st.checkbox(
        "Select start/end points by clicking on plot",
        value=False,
        key=f"use_plot_selection{key_suffix}",
        help="Show an interactive plot below where you can click to select start and end coordinates"
    )

    # Initialize interpolation coordinates in session state if not present
    if interp_start_key not in st.session_state:
        st.session_state[interp_start_key] = None
    if interp_end_key not in st.session_state:
        st.session_state[interp_end_key] = None

    # Get coordinates based on selection mode
    x_start, y_start, x_end, y_end = None, None, None, None

    if use_plot_selection:
        # Show interactive plot for selecting start/end points
        st.markdown("**Click on the plot below to select start and end points:**")

        # Show instructions
        if st.session_state[interp_start_key] is None:
            st.info("Step 1: Click anywhere on the plot to select the **start point**")
        elif st.session_state[interp_end_key] is None:
            st.info("Step 2: Click anywhere on the plot to select the **end point**")
        else:
            st.success("Both points selected! Adjust steps and generate interpolation below.")

        # Render the interpolation selection plot
        _render_interpolation_plot(
            coords_2d=coords_2d,
            interp_start_key=interp_start_key,
            interp_end_key=interp_end_key,
            key_suffix=key_suffix,
        )

        # Show selected coordinates
        if st.session_state[interp_start_key] is not None or st.session_state[interp_end_key] is not None:
            col1, col2 = st.columns(2)

            with col1:
                st.markdown("**Start Point**")
                if st.session_state[interp_start_key] is not None:
                    x_start, y_start = st.session_state[interp_start_key]
                    st.metric("X", f"{x_start:.3f}")
                    st.metric("Y", f"{y_start:.3f}")
                else:
                    st.caption("Not selected yet")

            with col2:
                st.markdown("**End Point**")
                if st.session_state[interp_end_key] is not None:
                    x_end, y_end = st.session_state[interp_end_key]
                    st.metric("X", f"{x_end:.3f}")
                    st.metric("Y", f"{y_end:.3f}")
                else:
                    st.caption("Not selected yet")

        # Clear button
        if st.button("Clear Selection", key=f"clear_interp_selection{key_suffix}"):
            st.session_state[interp_start_key] = None
            st.session_state[interp_end_key] = None
            st.rerun()

    else:
        # Manual coordinate entry mode
        st.info("Enter coordinates manually, or enable 'Select from plot' above")
        col1, col2 = st.columns(2)

        with col1:
            st.markdown("**Start Point**")
            x_start = st.number_input("X start", value=0.0, key=f"interp_x_start{key_suffix}", format="%.3f")
            y_start = st.number_input("Y start", value=0.0, key=f"interp_y_start{key_suffix}", format="%.3f")

        with col2:
            st.markdown("**End Point**")
            x_end = st.number_input("X end", value=1.0, key=f"interp_x_end{key_suffix}", format="%.3f")
            y_end = st.number_input("Y end", value=1.0, key=f"interp_y_end{key_suffix}", format="%.3f")

    # Interpolation controls (shown regardless of mode)
    n_steps = st.slider("Interpolation steps", 3, 15, 7, key=f"interp_steps{key_suffix}")

    # Only enable button if we have valid coordinates
    can_interpolate = (x_start is not None and y_start is not None and
                       x_end is not None and y_end is not None)

    button_disabled = not can_interpolate

    if st.button("Generate Interpolation", type="primary", key=f"gen_interp_btn{key_suffix}", disabled=button_disabled):
        with st.spinner("Generating interpolation..."):
            # Temporarily set active space, embeddings, and coords for decode
            orig_active = st.session_state.get('active_latent_space', 'primary')
            orig_emb = st.session_state.embeddings
            orig_coords = st.session_state.coords_2d
            st.session_state.active_latent_space = space_name
            st.session_state.embeddings = embeddings
            st.session_state.coords_2d = coords_2d

            # Generate interpolation with proper skip connection handling
            (interp_images, start_idx, end_idx,
             recon_start, recon_end) = _generate_interpolation_with_refs(
                x_start, y_start, x_end, y_end, n_steps
            )

            st.session_state.active_latent_space = orig_active
            st.session_state.embeddings = orig_emb
            st.session_state.coords_2d = orig_coords

            if interp_images:
                images = st.session_state.images

                # Show endpoints: Original | Reconstructed for both start and end
                st.markdown("### Endpoints")
                st.markdown("**Original images** (from dataset) vs **Reconstructed** (encode->decode):")

                # Start point
                st.markdown("#### Start Point")
                start_cols = st.columns(2)
                with start_cols[0]:
                    if images is not None:
                        st.image(
                            _image_array_for_display(start_idx, images),
                            caption=f"Original (index {start_idx})",
                            use_container_width=True
                        )
                with start_cols[1]:
                    if recon_start is not None:
                        st.image(
                            _prepare_image(recon_start),
                            caption=f"Reconstructed (a=0)",
                            use_container_width=True
                        )

                # End point
                st.markdown("#### End Point")
                end_cols = st.columns(2)
                with end_cols[0]:
                    if images is not None:
                        st.image(
                            _image_array_for_display(end_idx, images),
                            caption=f"Original (index {end_idx})",
                            use_container_width=True
                        )
                with end_cols[1]:
                    if recon_end is not None:
                        st.image(
                            _prepare_image(recon_end),
                            caption=f"Reconstructed (a=1)",
                            use_container_width=True
                        )

                # Display interpolation filmstrip
                st.markdown("### Interpolation Transition")
                st.caption("a=0.0 is start, a=1.0 is end. Both z and skip connections are interpolated.")

                cols = st.columns(n_steps)
                alphas = np.linspace(0, 1, n_steps)

                for i, (col, img) in enumerate(zip(cols, interp_images)):
                    with col:
                        display_img = _prepare_image(img)
                        st.image(display_img, caption=f"a={alphas[i]:.2f}")


def _render_interpolation_plot(
    coords_2d: Optional[np.ndarray] = None,
    interp_start_key: str = "interp_start",
    interp_end_key: str = "interp_end",
    key_suffix: str = "",
):
    """Render a clickable scatter plot specifically for selecting interpolation coordinates."""
    import plotly.graph_objects as go

    coords = coords_2d if coords_2d is not None else st.session_state.coords_2d
    if coords is None:
        st.warning("No projection available.")
        return

    metadata_df = st.session_state.metadata_df
    color_by = st.session_state.color_by

    # Create figure (same style as main plot but smaller)
    interp_categorical = False
    if color_by and color_by in metadata_df.columns:
        if _is_categorical_column(metadata_df[color_by]):
            interp_categorical = True
            interp_labels, interp_color_map = _build_categorical_marker(
                metadata_df[color_by], color_by
            )
            marker_config = None
        else:
            color_values = metadata_df[color_by].values
            marker_config = dict(
                size=8,
                color=color_values,
                colorscale='Viridis',
                showscale=True,
                colorbar=dict(title=color_by),
                opacity=0.6
            )
    else:
        marker_config = dict(
            size=8,
            color='lightsteelblue',
            opacity=0.6
        )

    fig = go.Figure()

    # Add main scatter
    if interp_categorical:
        labels_arr = np.array(interp_labels)
        for label in sorted(interp_color_map.keys()):
            mask = labels_arr == label
            if not mask.any():
                continue
            fig.add_trace(go.Scatter(
                x=coords[mask, 0],
                y=coords[mask, 1],
                mode='markers',
                marker=dict(
                    size=8,
                    color=interp_color_map[label],
                    opacity=0.6
                ),
                hovertemplate='x: %{x:.3f}<br>y: %{y:.3f}<extra></extra>',
                name=label
            ))
    else:
        fig.add_trace(go.Scatter(
            x=coords[:, 0],
            y=coords[:, 1],
            mode='markers',
            marker=marker_config,
            hovertemplate='x: %{x:.3f}<br>y: %{y:.3f}<extra></extra>',
            name='data'
        ))

    # Highlight start point if selected
    if st.session_state.get(interp_start_key) is not None:
        x_s, y_s = st.session_state[interp_start_key]
        fig.add_trace(go.Scatter(
            x=[x_s],
            y=[y_s],
            mode='markers+text',
            marker=dict(size=20, color='green', symbol='star', line=dict(width=2, color='darkgreen')),
            text=['START'],
            textposition='top center',
            textfont=dict(size=12, color='darkgreen'),
            hovertemplate=f'START<br>x: {x_s:.3f}<br>y: {y_s:.3f}<extra></extra>',
            showlegend=False
        ))

    # Highlight end point if selected
    if st.session_state.get(interp_end_key) is not None:
        x_e, y_e = st.session_state[interp_end_key]
        fig.add_trace(go.Scatter(
            x=[x_e],
            y=[y_e],
            mode='markers+text',
            marker=dict(size=20, color='red', symbol='star', line=dict(width=2, color='darkred')),
            text=['END'],
            textposition='top center',
            textfont=dict(size=12, color='darkred'),
            hovertemplate=f'END<br>x: {x_e:.3f}<br>y: {y_e:.3f}<extra></extra>',
            showlegend=False
        ))

    # Draw line between start and end if both selected
    if st.session_state.get(interp_start_key) is not None and st.session_state.get(interp_end_key) is not None:
        x_s, y_s = st.session_state[interp_start_key]
        x_e, y_e = st.session_state[interp_end_key]
        fig.add_trace(go.Scatter(
            x=[x_s, x_e],
            y=[y_s, y_e],
            mode='lines',
            line=dict(color='purple', width=3, dash='dash'),
            hoverinfo='skip',
            showlegend=False
        ))

    fig.update_layout(
        title="Click to Select Start and End Points",
        xaxis_title='Component 1',
        yaxis_title='Component 2',
        hovermode='closest',
        template='plotly_white',
        height=400,
        clickmode='event+select'
    )

    # Use plotly chart with click events
    try:
        selected = st.plotly_chart(
            fig,
            use_container_width=True,
            on_select="rerun",
            selection_mode="points",
            key=f"interp_scatter{key_suffix}"
        )

        # Process click event
        if selected is not None:
            # Try to extract clicked point coordinates
            points = None

            if hasattr(selected, 'selection') and selected.selection:
                if hasattr(selected.selection, 'points'):
                    points = selected.selection.points
                elif isinstance(selected.selection, dict) and 'points' in selected.selection:
                    points = selected.selection['points']

            if points is None and isinstance(selected, dict):
                if 'selection' in selected and 'points' in selected['selection']:
                    points = selected['selection']['points']

            if points and len(points) > 0:
                point = points[0]

                # Get the x, y coordinates from the click
                if isinstance(point, dict):
                    x_click = point.get('x')
                    y_click = point.get('y')
                    curve_num = point.get('curve_number', point.get('curveNumber', 0))

                    # Only process clicks on the main data trace (trace 0)
                    if x_click is not None and y_click is not None and curve_num == 0:
                        # Add to start or end depending on what's available
                        if st.session_state.get(interp_start_key) is None:
                            st.session_state[interp_start_key] = (float(x_click), float(y_click))
                            st.rerun()
                        elif st.session_state.get(interp_end_key) is None:
                            st.session_state[interp_end_key] = (float(x_click), float(y_click))
                            st.rerun()

    except (TypeError, AttributeError) as e:
        st.plotly_chart(fig, use_container_width=True)
        st.warning("Click events not available. Please use manual coordinate entry instead (uncheck the box above).")


def _find_nearest_data_index(x: float, y: float) -> int:
    """Find the nearest data point index for given 2D coordinates."""
    coords = st.session_state.coords_2d
    point = np.array([[x, y]])
    distances = np.linalg.norm(coords - point, axis=1)
    return int(np.argmin(distances))


def _generate_interpolation_with_refs(
    x_start: float,
    y_start: float,
    x_end: float,
    y_end: float,
    n_steps: int
) -> Tuple[List[np.ndarray], int, int, np.ndarray, np.ndarray]:
    """
    Generate interpolation between two 2D coordinates.

    This function interpolates BOTH the latent vector z AND the skip connections
    (feature pyramids) to produce smooth transitions between endpoints.

    Returns:
        interp_images: List of decoded images for each alpha step
        start_idx: Nearest data point index to start coordinates
        end_idx: Nearest data point index to end coordinates
        recon_start: Reconstructed image at start (using start's skip connections)
        recon_end: Reconstructed image at end (using end's skip connections)
    """
    model = st.session_state.model
    images = st.session_state.images
    conditioning = st.session_state.conditioning
    active_space = st.session_state.get('active_latent_space', 'primary')

    # Find nearest data points for start and end
    start_idx = _find_nearest_data_index(x_start, y_start)
    end_idx = _find_nearest_data_index(x_end, y_end)

    # Get latent vectors for start and end (from k-NN estimation)
    z_start, cond_start = _estimate_latent_from_2d(x_start, y_start)
    z_end, cond_end = _estimate_latent_from_2d(x_end, y_end)

    alphas = np.linspace(0, 1, n_steps)
    interp_images = []
    recon_start = None
    recon_end = None

    if model is None:
        # Without model, just use nearest neighbors
        if images is not None:
            embeddings = st.session_state.embeddings
            for alpha in alphas:
                z_interp = (1 - alpha) * z_start + alpha * z_end
                distances = np.linalg.norm(embeddings - z_interp, axis=1)
                nearest_idx = np.argmin(distances)
                interp_images.append(images[nearest_idx])
            recon_start = images[start_idx]
            recon_end = images[end_idx]
        return interp_images, start_idx, end_idx, recon_start, recon_end

    # With model: interpolate BOTH z and skip connections

    # Prepare START point tensors
    start_img_tensor = torch.tensor(
        images[start_idx:start_idx+1], dtype=torch.float32
    ).to(model.device)
    start_cond_tensor = None
    if conditioning is not None:
        start_cond_tensor = torch.tensor(
            conditioning[start_idx:start_idx+1], dtype=torch.float32
        ).to(model.device)

    # Encode START to get z and skip connections
    with torch.no_grad():
        z_start_primary = model.encode(start_img_tensor, start_cond_tensor)
        skip_start = model.get_last_skip()  # [x1, x2, x3, x4]
        # Decode with start's own skip connections for reconstruction
        recon_start = model.decode(z_start_primary, start_cond_tensor).cpu().numpy().squeeze()

        # Get tendril z for start if in tendril space
        if active_space != 'primary' and model.has_tendrils():
            tendril_keys = model.list_tendril_keys()
            t_idx = tendril_keys.index(active_space)
            _, _, _, zt_start = model.encode_tendril(active_space, skip_start[t_idx], cond=start_cond_tensor)

    # Prepare END point tensors
    end_img_tensor = torch.tensor(
        images[end_idx:end_idx+1], dtype=torch.float32
    ).to(model.device)
    end_cond_tensor = None
    if conditioning is not None:
        end_cond_tensor = torch.tensor(
            conditioning[end_idx:end_idx+1], dtype=torch.float32
        ).to(model.device)

    # Encode END to get z and skip connections
    with torch.no_grad():
        z_end_primary = model.encode(end_img_tensor, end_cond_tensor)
        skip_end = model.get_last_skip()  # [x1, x2, x3, x4]
        # Decode with end's own skip connections for reconstruction
        recon_end = model.decode(z_end_primary, end_cond_tensor).cpu().numpy().squeeze()

        # Get tendril z for end if in tendril space
        if active_space != 'primary' and model.has_tendrils():
            _, _, _, zt_end = model.encode_tendril(active_space, skip_end[t_idx], cond=end_cond_tensor)

    # Generate interpolation by interpolating BOTH z and skip connections
    for alpha in alphas:
        # Interpolate skip connections (feature pyramids)
        skip_interp = []
        for skip_s, skip_e in zip(skip_start, skip_end):
            skip_i = (1 - alpha) * skip_s + alpha * skip_e
            skip_interp.append(skip_i)

        # Interpolate conditioning
        cond_tensor = None
        if cond_start is not None and cond_end is not None:
            cond_interp = (1 - alpha) * cond_start + alpha * cond_end
            cond_tensor = torch.tensor(cond_interp, dtype=torch.float32).unsqueeze(0).to(model.device)

        with torch.no_grad():
            if active_space != 'primary' and model.has_tendrils():
                # Interpolate in tendril latent space
                zt_interp = (1 - alpha) * zt_start + alpha * zt_end
                z_primary_interp = (1 - alpha) * z_start_primary + alpha * z_end_primary
                decoded = model.decode_with_tendril_modification(
                    active_space, zt_interp, skip_interp, z_primary_interp, cond_tensor
                )
            else:
                # Interpolate primary z vectors
                z_interp = (1 - alpha) * z_start_primary + alpha * z_end_primary
                decoded = model.decode_with_skip(z_interp, skip_interp, cond_tensor)

        interp_images.append(decoded.cpu().numpy().squeeze())

    return interp_images, start_idx, end_idx, recon_start, recon_end


############################################################################
# Feature 2: Neighbor Highlight
############################################################################

def _find_neighbors(
    index: int,
    n_neighbors: int,
    use_projected: bool = False
) -> Tuple[List[int], List[float]]:
    """Find k nearest neighbors of a point in latent or projected space."""
    embeddings = st.session_state.embeddings
    coords_2d = st.session_state.coords_2d

    space = coords_2d if use_projected else embeddings
    query = space[index]
    distances = np.linalg.norm(space - query, axis=1)
    order = np.argsort(distances)
    neighbor_indices = [int(i) for i in order if i != index][:n_neighbors]
    neighbor_distances = [float(distances[i]) for i in neighbor_indices]
    return neighbor_indices, neighbor_distances


def _maybe_auto_compute_neighbors():
    """Auto-compute k-NN when a single point was selected, if the toggle is on.

    Called once per rerun after all scatter plots have processed their selection
    events.  Uses the ``neighbor_knn_target_space`` written by the selection
    handler to decide which latent space to query.
    """
    if not st.session_state.get('auto_neighbors_enabled', True):
        return
    clicked_idx = st.session_state.get('clicked_idx')
    if clicked_idx is None:
        return
    target_space = st.session_state.get(
        'neighbor_knn_target_space',
        st.session_state.get('active_latent_space', 'primary'),
    )
    # Determine n_neighbors — reuse whichever slider the user last touched
    # (check suffixed key first, then unsuffixed, fallback to 4).
    # Use explicit None checks (not ``or``) so slider values are respected.
    n_neighbors = st.session_state.get(f'n_neighbors_slider_{target_space}')
    if n_neighbors is None:
        n_neighbors = st.session_state.get('n_neighbors_slider')
    if n_neighbors is None:
        n_neighbors = 4
    _compute_neighbors_in_space(
        space_name=target_space,
        query_idx=clicked_idx,
        n_neighbors=int(n_neighbors),
        use_projected=False,
    )


def _compute_neighbors_in_space(
    space_name: str,
    query_idx: int,
    n_neighbors: int,
    use_projected: bool = False,
) -> Tuple[List[int], List[float]]:
    """Compute k-NN for a query point in the given latent space.

    Writes results to session state under both suffixed keys (for tendril mode)
    and unsuffixed keys (when space_name is the active space), so the main
    scatter overlay and neighbor gallery stay in sync.
    """
    embeddings = st.session_state.all_embeddings.get(space_name)
    coords_2d = st.session_state.all_coords_2d.get(space_name)
    space = coords_2d if (use_projected and coords_2d is not None) else embeddings
    if space is None:
        return [], []

    query = space[int(query_idx)]
    distances_arr = np.linalg.norm(space - query, axis=1)
    order = np.argsort(distances_arr)
    indices = [int(i) for i in order if i != int(query_idx)][:n_neighbors]
    distances_list = [float(distances_arr[i]) for i in indices]

    # Store under suffixed keys (tendril-aware)
    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        st.session_state[f"neighbor_indices_{space_name}"] = indices
        st.session_state[f"neighbor_distances_{space_name}"] = distances_list

    # Mirror to unsuffixed keys when this is the active space so the main
    # scatter overlay picks them up
    active = st.session_state.get('active_latent_space', 'primary')
    if space_name == active or not tendril_keys:
        st.session_state.neighbor_indices = indices
        st.session_state.neighbor_distances = distances_list

    return indices, distances_list


def _render_neighbor_highlight():
    """Render the neighbor highlight section."""
    st.subheader("Neighbor Highlight")

    # --- Auto-neighbors checkbox (moved here from sidebar Selection) ---
    st.checkbox(
        "Auto-update neighbors on click",
        value=True,
        key="auto_neighbors_enabled",
        help="When enabled, clicking a single point auto-computes k-NN in the active latent space.",
    )

    st.caption(
        "**How to use:** 1) Enable *Auto-update neighbors on click* above. "
        "2) On any scatter plot, click a single marker or draw a tiny box around one point "
        "(use the Plotly toolbar **Box Select** tool if click does not register). "
        "3) The neighbourhood gallery appears below automatically. "
        "Select exactly **one** point — multi-point selections trigger a region profile instead. "
        "For manual lookup by image index use the per-space tabs further down."
    )

    # --- Unified gallery driven by last plot selection ---
    clicked_idx = st.session_state.get('clicked_idx')
    target_space = st.session_state.get(
        'neighbor_knn_target_space',
        st.session_state.get('active_latent_space', 'primary'),
    )

    # Resolve neighbour keys for the target space
    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        indices_key = f"neighbor_indices_{target_space}"
        distances_key = f"neighbor_distances_{target_space}"
    else:
        indices_key = "neighbor_indices"
        distances_key = "neighbor_distances"

    neighbor_indices = st.session_state.get(indices_key)
    neighbor_distances = st.session_state.get(distances_key)

    # Track which space the unified panel rendered so tabs can skip duplicates
    unified_rendered_space = None

    if clicked_idx is not None and neighbor_indices:
        space_label = "Primary (z)" if target_space == "primary" else f"Tendril: {target_space}"
        st.markdown(f"#### Neighbourhood (from plot selection — {space_label})")
        _render_neighbor_gallery(
            clicked_idx,
            neighbor_indices,
            neighbor_distances,
            label=space_label,
        )
        unified_rendered_space = target_space
        st.divider()
    elif (
        clicked_idx is not None
        and not neighbor_indices
        and st.session_state.get('auto_neighbors_enabled', True)
    ):
        st.warning(
            f"Plot selection recorded (index **{clicked_idx}**, space **{target_space}**), "
            "but neighbors were not computed. Ensure embeddings exist for that space, "
            "images are loaded, and try **Find Neighbors** in a tab below."
        )

    # Show multi-select warning if applicable
    multiselect_warn = st.session_state.get('neighbor_multiselect_warning')
    if multiselect_warn:
        st.warning(multiselect_warn)

    # --- Per-space tabs (manual Find Neighbors + controls) ---
    if tendril_keys:
        _uq = _dedupe_tendril_keys_preserve_order(list(tendril_keys))
        space_names = ['primary'] + _uq
        tab_labels = ["Primary (z)"] + [f"Tendril: {k}" for k in _uq]
        tabs = st.tabs(tab_labels)
        for tab, space_name in zip(tabs, space_names):
            with tab:
                _render_neighbor_highlight_for_space(
                    space_name=space_name,
                    embeddings=st.session_state.all_embeddings[space_name],
                    coords_2d=st.session_state.all_coords_2d.get(space_name),
                    key_suffix=f"_{space_name}",
                    skip_gallery_for_space=unified_rendered_space,
                )
    else:
        _render_neighbor_highlight_for_space(
            space_name='primary',
            embeddings=st.session_state.embeddings,
            coords_2d=st.session_state.coords_2d,
            key_suffix="",
            skip_gallery_for_space=unified_rendered_space,
        )


def _render_neighbor_gallery(
    query_idx: int,
    indices: List[int],
    distances: List[float],
    *,
    label: str = "",
) -> bool:
    """Render a query-image + k-neighbor thumbnail grid.

    Returns True if images were rendered, False if no images are loaded.
    """
    images = st.session_state.images
    if images is None:
        st.warning("No images loaded to display neighbors.")
        return False

    header = f"**Query image (index {query_idx}) and {len(indices)} nearest neighbors"
    if label:
        header += f" — {label}"
    header += ":**"
    st.markdown(header)

    n_total = 1 + len(indices)
    cols = st.columns(min(n_total, 8))

    with cols[0]:
        st.image(
            _image_array_for_display(query_idx, images),
            caption=f"Query [{query_idx}]",
            use_container_width=True,
        )

    for j, (ni, nd) in enumerate(zip(indices, distances)):
        col_idx = (j + 1) % len(cols)
        with cols[col_idx]:
            st.image(
                _image_array_for_display(ni, images),
                caption=f"[{ni}] d={nd:.3f}",
                use_container_width=True,
            )

    if n_total > 8:
        remaining = list(zip(indices, distances))[7:]
        cols2 = st.columns(min(len(remaining), 8))
        for j, (ni, nd) in enumerate(remaining):
            with cols2[j % len(cols2)]:
                st.image(
                    _image_array_for_display(ni, images),
                    caption=f"[{ni}] d={nd:.3f}",
                    use_container_width=True,
                )

    return True


def _render_neighbor_highlight_for_space(
    space_name: str,
    embeddings: np.ndarray,
    coords_2d: Optional[np.ndarray],
    key_suffix: str,
    skip_gallery_for_space: Optional[str] = None,
):
    """Render neighbor highlight controls and results for a single latent space."""
    clicked_idx = st.session_state.get('clicked_idx')

    indices_key = f"neighbor_indices{key_suffix}"
    distances_key = f"neighbor_distances{key_suffix}"

    col1, col2, col3 = st.columns(3)
    with col1:
        default_idx = clicked_idx if clicked_idx is not None else 0
        neighbor_query_idx = st.number_input(
            "Image index",
            min_value=0,
            max_value=len(embeddings) - 1,
            value=default_idx,
            key=f"neighbor_query_idx{key_suffix}"
        )
    with col2:
        n_neighbors = st.slider(
            "Number of neighbors",
            min_value=1, max_value=16, value=4,
            key=f"n_neighbors_slider{key_suffix}"
        )
    with col3:
        neighbor_space = st.radio(
            "Neighbor space",
            ["High-dimensional", "Projected 2D"],
            key=f"neighbor_space_radio{key_suffix}"
        )

    if st.button("Find Neighbors", key=f"find_neighbors_btn{key_suffix}", type="primary"):
        use_projected = (neighbor_space == "Projected 2D")
        # Use the space-specific embeddings and coords
        space = coords_2d if use_projected else embeddings
        if space is not None:
            query = space[int(neighbor_query_idx)]
            distances_arr = np.linalg.norm(space - query, axis=1)
            order = np.argsort(distances_arr)
            indices = [int(i) for i in order if i != int(neighbor_query_idx)][:n_neighbors]
            distances_list = [float(distances_arr[i]) for i in indices]
            st.session_state[indices_key] = indices
            st.session_state[distances_key] = distances_list
            # Set clicked_idx so the scatter plot highlights the query
            st.session_state.clicked_idx = int(neighbor_query_idx)
            st.rerun()

    # Display results (skip gallery if unified panel already rendered for this space)
    neighbor_indices = st.session_state.get(indices_key)
    neighbor_distances = st.session_state.get(distances_key)

    if neighbor_indices:
        if skip_gallery_for_space == space_name:
            st.caption("Gallery shown in the unified section above.")
        else:
            query_idx = st.session_state.get('clicked_idx', neighbor_query_idx)
            _render_neighbor_gallery(query_idx, neighbor_indices, neighbor_distances)

        if st.button("Clear Neighbors", key=f"clear_neighbors_btn{key_suffix}"):
            st.session_state[indices_key] = None
            st.session_state[distances_key] = None
            st.rerun()


############################################################################
# Cross-space k-NN panels
############################################################################


def _render_cross_space_neighbor_panels():
    """Compare k-NN neighbourhoods across all latent spaces for anchor points."""
    with st.expander("Cross-Space Neighbor Comparison", expanded=False):
        images = st.session_state.images
        if images is None:
            st.info("Load images first.")
            return

        all_embeddings = st.session_state.get('all_embeddings', {})
        tendril_keys = st.session_state.get('tendril_keys', [])
        if not tendril_keys:
            st.info("Load a tendril model to compare spaces.")
            return

        space_names = ['primary'] + list(tendril_keys)
        n_images = len(images)

        # --- Controls ---
        ctrl1, ctrl2 = st.columns(2)
        with ctrl1:
            anchor_text = st.text_input(
                "Anchor indices (comma-separated)",
                value=str(st.session_state.get('clicked_idx', 0)),
                key="cross_knn_anchors",
            )
        with ctrl2:
            k = st.slider(
                "Neighbors (k)",
                min_value=1, max_value=12, value=4,
                key="cross_knn_k",
            )

        # Parse anchor indices
        try:
            anchors = [
                int(x.strip())
                for x in anchor_text.split(",")
                if x.strip().isdigit()
            ]
            anchors = [a for a in anchors if 0 <= a < n_images]
        except ValueError:
            anchors = []

        if not anchors:
            st.warning("Enter valid image indices.")
            return

        if len(anchors) > 5:
            st.warning("Capped to first 5 anchors for performance.")
            anchors = anchors[:5]

        if not st.button("Compare Neighborhoods", key="cross_knn_run", type="primary"):
            return

        # --- Compute and render ---
        for anchor_idx in anchors:
            st.markdown(f"#### Anchor: image {anchor_idx}")

            space_cols = st.columns(len(space_names))
            for col, space_name in zip(space_cols, space_names):
                with col:
                    space_label = (
                        "Primary (z)" if space_name == "primary"
                        else f"Tendril: {space_name}"
                    )
                    st.markdown(f"**{space_label}**")

                    emb = all_embeddings.get(space_name)
                    if emb is None:
                        st.caption("No embeddings")
                        continue

                    query = emb[anchor_idx]
                    dists = np.linalg.norm(emb - query, axis=1)
                    order = np.argsort(dists)
                    indices = [int(i) for i in order if i != anchor_idx][:k]
                    distances = [float(dists[i]) for i in indices]

                    # Query thumbnail
                    st.image(
                        _image_array_for_display(anchor_idx, images),
                        caption=f"Query [{anchor_idx}]",
                        use_container_width=True,
                    )
                    # Neighbor thumbnails
                    for ni, nd in zip(indices, distances):
                        st.image(
                            _image_array_for_display(ni, images),
                            caption=f"[{ni}] d={nd:.3f}",
                            use_container_width=True,
                        )

            if anchor_idx != anchors[-1]:
                st.divider()


############################################################################
# Feature 3: Latent Dimension Sweep
############################################################################

def _generate_dimension_sweep(
    index: int,
    step_size: float,
    n_steps: int,
    dim_indices: Optional[List[int]] = None
) -> Tuple[Optional[List[List[np.ndarray]]], Optional[List[float]], Optional[List[int]]]:
    """
    Generate dimension sweep images.

    Args:
        index: Image index to sweep from
        step_size: Step size for perturbation
        n_steps: Steps per side (total columns = 2*n_steps + 1)
        dim_indices: Optional list of dimension indices to sweep (None = all)

    Returns:
        all_decoded: List of rows, each row is a list of images
        offsets: List of offset values
        swept_dims: List of dimension indices that were swept
    """
    model = st.session_state.model
    images = st.session_state.images
    conditioning = st.session_state.conditioning
    embeddings = st.session_state.embeddings

    active_space = st.session_state.get('active_latent_space', 'primary')

    if model is None:
        st.error("Model required for dimension sweep.")
        return None, None, None

    base_mu = embeddings[index]
    latent_dim = len(base_mu)

    if dim_indices is None:
        dim_indices = list(range(latent_dim))

    # Encode the image to cache skip connections
    img_tensor = torch.tensor(
        images[index:index+1], dtype=torch.float32
    ).to(model.device)

    cond_tensor = None
    if conditioning is not None:
        cond_tensor = torch.tensor(
            conditioning[index:index+1], dtype=torch.float32
        ).to(model.device)

    with torch.no_grad():
        z_primary = model.encode(img_tensor, cond_tensor)
        skip = model.get_last_skip()

    # Generate offsets
    offsets = [round(-n_steps * step_size + i * step_size, 4)
               for i in range(2 * n_steps + 1)]

    all_decoded = []
    total_images = len(dim_indices) * len(offsets)
    progress_bar = st.progress(0, text="Generating dimension sweep...")

    for row_idx, d in enumerate(dim_indices):
        row_images = []
        for off in offsets:
            z = base_mu.copy()
            z[d] += off
            z_tensor = torch.tensor(z, dtype=torch.float32).unsqueeze(0).to(model.device)

            with torch.no_grad():
                if active_space != 'primary' and model.has_tendrils():
                    decoded = model.decode_with_tendril_modification(
                        active_space, z_tensor, skip, z_primary, cond_tensor
                    )
                else:
                    decoded = model.decode_with_skip(z_tensor, skip, cond_tensor)

            img = decoded.cpu().numpy().squeeze()
            row_images.append(img)

        all_decoded.append(row_images)

        # Update progress
        done = (row_idx + 1) * len(offsets)
        progress_bar.progress(done / total_images, text=f"Dimension z[{d}] done ({done}/{total_images})")

    progress_bar.empty()
    return all_decoded, offsets, dim_indices


def _render_dimension_sweep():
    """Render the dimension sweep section."""
    st.subheader("Latent Dimension Sweep")
    st.caption(
        "Perturb each latent dimension individually to see what it controls. "
        "Each row is one dimension; the center column (highlighted) is the unperturbed reconstruction."
    )

    model = st.session_state.model
    if model is None:
        st.warning("Load a model to use this feature.")
        return

    images = st.session_state.images
    if images is None:
        st.warning("No images loaded.")
        return

    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        space_names = ['primary'] + list(tendril_keys)
        tab_labels = ["Primary (z)"] + [f"Tendril: {k}" for k in tendril_keys]
        tabs = st.tabs(tab_labels)
        for tab, space_name in zip(tabs, space_names):
            with tab:
                _render_dimension_sweep_for_space(
                    space_name=space_name,
                    embeddings=st.session_state.all_embeddings[space_name],
                    key_suffix=f"_{space_name}",
                )
    else:
        _render_dimension_sweep_for_space(
            space_name='primary',
            embeddings=st.session_state.embeddings,
            key_suffix="",
        )


def _render_dimension_sweep_for_space(
    space_name: str,
    embeddings: np.ndarray,
    key_suffix: str,
):
    """Render dimension sweep controls and results for a single latent space."""
    latent_dim = embeddings.shape[1]
    clicked_idx = st.session_state.get('clicked_idx')

    result_key = f"sweep_results{key_suffix}"

    # Controls
    col1, col2, col3 = st.columns(3)
    with col1:
        sweep_idx = st.number_input(
            "Image index",
            min_value=0,
            max_value=len(embeddings) - 1,
            value=clicked_idx if clicked_idx is not None else 0,
            key=f"sweep_idx{key_suffix}"
        )
    with col2:
        step_size = st.slider(
            "Step size",
            min_value=0.1, max_value=2.0, value=0.5, step=0.1,
            key=f"sweep_step_size{key_suffix}"
        )
    with col3:
        n_steps = st.slider(
            "Steps per side",
            min_value=1, max_value=5, value=2,
            key=f"sweep_n_steps{key_suffix}"
        )

    # Variance-based filtering
    limit_dims = st.checkbox(
        "Limit to top-K most variable dimensions",
        value=False,
        key=f"sweep_limit_dims{key_suffix}"
    )
    top_k = latent_dim
    if limit_dims:
        top_k = st.slider(
            "Top K dimensions by variance",
            min_value=1, max_value=latent_dim, value=min(8, latent_dim),
            key=f"sweep_top_k{key_suffix}"
        )

    total_images = (top_k if limit_dims else latent_dim) * (2 * n_steps + 1)
    st.caption(f"Will generate {total_images} images ({top_k if limit_dims else latent_dim} dims x {2*n_steps+1} offsets)")

    if st.button("Generate Dimension Sweep", type="primary", key=f"gen_sweep_btn{key_suffix}"):
        # Determine which dimensions to sweep
        dim_indices = None
        if limit_dims:
            var_per_dim = np.var(embeddings, axis=0)
            dim_indices = list(np.argsort(var_per_dim)[::-1][:top_k])
            dim_indices.sort()  # Sort by index for display

        # Temporarily set active space and embeddings for decode
        orig_active = st.session_state.get('active_latent_space', 'primary')
        orig_emb = st.session_state.embeddings
        st.session_state.active_latent_space = space_name
        st.session_state.embeddings = embeddings

        all_decoded, offsets, swept_dims = _generate_dimension_sweep(
            int(sweep_idx), step_size, n_steps, dim_indices
        )

        st.session_state.active_latent_space = orig_active
        st.session_state.embeddings = orig_emb

        if all_decoded is not None:
            st.session_state[result_key] = {
                'all_decoded': all_decoded,
                'offsets': offsets,
                'swept_dims': swept_dims,
                'index': int(sweep_idx),
                'step_size': step_size,
                'n_steps': n_steps,
            }
            st.rerun()

    # Display results
    sweep = st.session_state.get(result_key)
    if sweep is not None:
        all_decoded = sweep['all_decoded']
        offsets = sweep['offsets']
        swept_dims = sweep['swept_dims']

        st.markdown(
            f"**Image index:** {sweep['index']} | "
            f"**Dims swept:** {len(swept_dims)} | "
            f"**Step:** {sweep['step_size']} | "
            f"**Total images:** {len(swept_dims) * len(offsets)}"
        )

        center_col_idx = len(offsets) // 2  # Index of the 0-offset column

        # Column headers
        n_cols = len(offsets) + 1  # +1 for row label
        header_cols = st.columns(n_cols)
        with header_cols[0]:
            st.markdown("**Dim**")
        for j, off in enumerate(offsets):
            with header_cols[j + 1]:
                label = f"**{off:+.2f}**" if off != 0 else f"***0.00***"
                st.markdown(label)

        # Rows
        for row_idx, (d, row_images) in enumerate(zip(swept_dims, all_decoded)):
            row_cols = st.columns(n_cols)
            with row_cols[0]:
                st.markdown(f"**z[{d}]**")
            for j, img in enumerate(row_images):
                with row_cols[j + 1]:
                    display_img = _prepare_image(img)
                    caption = f"{offsets[j]:+.2f}"
                    if j == center_col_idx:
                        # Highlight center column with a border via HTML
                        import base64
                        from io import BytesIO
                        from PIL import Image as PILImage
                        pil_img = PILImage.fromarray(display_img)
                        buf = BytesIO()
                        pil_img.save(buf, format='PNG')
                        img_b64 = base64.b64encode(buf.getvalue()).decode()
                        st.markdown(
                            f'<div style="border:3px solid lime; padding:2px; display:inline-block;">'
                            f'<img src="data:image/png;base64,{img_b64}" style="width:100%;">'
                            f'</div>',
                            unsafe_allow_html=True
                        )
                        st.caption("center")
                    else:
                        st.image(display_img, use_container_width=True)

        if st.button("Clear Sweep Results", key=f"clear_sweep_btn{key_suffix}"):
            st.session_state[result_key] = None
            st.rerun()


############################################################################
# Feature 4: Latent Sliders
############################################################################

def _decode_from_sliders(
    base_index: int,
    z_values: List[float],
    cond_override: Optional[List[float]] = None,
) -> Optional[np.ndarray]:
    """
    Decode an image from slider z values, using skip connections from base image.

    The base image is always encoded with its *own* conditioning (to produce
    faithful skip connections). When ``cond_override`` is given, the decode step
    instead uses those conditioning values (0-1 per variable), so the user can
    see how changing H/S/V conditioning re-colours the reconstruction at fixed z.
    """
    model = st.session_state.model
    images = st.session_state.images
    conditioning = st.session_state.conditioning
    active_space = st.session_state.get('active_latent_space', 'primary')

    if model is None:
        return None

    # Encode base image to get skip connections (use its real conditioning)
    img_tensor = torch.tensor(
        images[base_index:base_index+1], dtype=torch.float32
    ).to(model.device)
    cond_tensor = None
    if conditioning is not None:
        cond_tensor = torch.tensor(
            conditioning[base_index:base_index+1], dtype=torch.float32
        ).to(model.device)

    # Conditioning used for decoding (may be overridden by the user's sliders).
    decode_cond_tensor = cond_tensor
    if cond_override is not None and conditioning is not None:
        decode_cond_tensor = torch.tensor(
            [cond_override], dtype=torch.float32
        ).to(model.device)

    with torch.no_grad():
        z_primary = model.encode(img_tensor, cond_tensor)
        skip = model.get_last_skip()

    z_tensor = torch.tensor(
        z_values, dtype=torch.float32
    ).unsqueeze(0).to(model.device)

    with torch.no_grad():
        if active_space != 'primary' and model.has_tendrils():
            decoded = model.decode_with_tendril_modification(
                active_space, z_tensor, skip, z_primary, decode_cond_tensor
            )
        else:
            decoded = model.decode_with_skip(z_tensor, skip, decode_cond_tensor)

    return decoded.cpu().numpy().squeeze()


def _render_latent_sliders():
    """Render the latent sliders section."""
    st.caption("Manually adjust individual latent dimensions and decode the result.")

    model = st.session_state.model
    if model is None:
        st.warning("Load a model to use this feature.")
        return

    images = st.session_state.images
    if images is None:
        st.warning("No images loaded.")
        return

    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        space_names = ['primary'] + list(tendril_keys)
        tab_labels = ["Primary (z)"] + [f"Tendril: {k}" for k in tendril_keys]
        tabs = st.tabs(tab_labels)
        for tab, space_name in zip(tabs, space_names):
            with tab:
                _render_latent_sliders_for_space(
                    space_name=space_name,
                    embeddings=st.session_state.all_embeddings[space_name],
                    key_suffix=f"_{space_name}",
                )
    else:
        _render_latent_sliders_for_space(
            space_name='primary',
            embeddings=st.session_state.embeddings,
            key_suffix="",
        )


def _render_latent_sliders_for_space(
    space_name: str,
    embeddings: np.ndarray,
    key_suffix: str,
):
    """Render latent sliders for a single latent space."""
    images = st.session_state.images
    latent_dim = embeddings.shape[1]
    clicked_idx = st.session_state.get('clicked_idx')

    z_values_key = f"slider_z_values{key_suffix}"
    base_index_key = f"slider_base_index{key_suffix}"
    decoded_img_key = f"slider_decoded_img{key_suffix}"

    # Initialize per-space state
    if z_values_key not in st.session_state:
        st.session_state[z_values_key] = None
    if base_index_key not in st.session_state:
        st.session_state[base_index_key] = 0

    # Controls
    col1, col2, col3 = st.columns(3)
    with col1:
        slider_base = st.number_input(
            "Base image index",
            min_value=0,
            max_value=len(embeddings) - 1,
            value=st.session_state[base_index_key],
            key=f"slider_base_input{key_suffix}"
        )
    with col2:
        if st.button("Load latent vector", key=f"load_latent_btn{key_suffix}", type="primary"):
            base_mu = embeddings[int(slider_base)].tolist()
            st.session_state[z_values_key] = base_mu
            st.session_state[base_index_key] = int(slider_base)
            st.rerun()
    with col3:
        if st.button("Reset to original", key=f"reset_sliders_btn{key_suffix}"):
            base_mu = embeddings[st.session_state[base_index_key]].tolist()
            st.session_state[z_values_key] = base_mu
            st.rerun()

    # Initialize slider values if not set
    if st.session_state[z_values_key] is None:
        idx = clicked_idx if clicked_idx is not None else 0
        st.session_state[z_values_key] = embeddings[idx].tolist()
        st.session_state[base_index_key] = idx

    z_values = st.session_state[z_values_key]

    # Show original image for reference
    col_orig, col_decoded = st.columns(2)
    with col_orig:
        st.markdown("**Original image:**")
        st.image(
            _image_array_for_display(st.session_state[base_index_key], images),
            caption=f"Index {st.session_state[base_index_key]}",
            use_container_width=True
        )

    # Sliders - use expander if many dims
    if latent_dim > 16:
        with st.expander(f"Latent sliders ({latent_dim} dimensions)", expanded=True):
            _render_slider_grid(z_values, latent_dim, embeddings, key_suffix=key_suffix)
    else:
        _render_slider_grid(z_values, latent_dim, embeddings, key_suffix=key_suffix)

    # Optional conditioning (H/S/V) override — only for conditional models.
    cond_override = _render_conditioning_sliders(
        base_index=st.session_state[base_index_key],
        key_suffix=key_suffix,
    )

    # Decode button
    if st.button("Decode", type="primary", key=f"decode_sliders_btn{key_suffix}"):
        with st.spinner("Decoding..."):
            # Temporarily set active space for decode
            orig_active = st.session_state.get('active_latent_space', 'primary')
            st.session_state.active_latent_space = space_name
            decoded = _decode_from_sliders(
                st.session_state[base_index_key],
                st.session_state[z_values_key],
                cond_override=cond_override,
            )
            st.session_state.active_latent_space = orig_active
            if decoded is not None:
                st.session_state[decoded_img_key] = decoded
                st.rerun()
            else:
                st.error("Decoding failed.")

    # Show decoded result
    decoded_img = st.session_state.get(decoded_img_key)
    with col_decoded:
        if decoded_img is not None:
            st.markdown("**Decoded from sliders:**")
            st.image(
                _prepare_image(decoded_img),
                caption="Slider decode",
                use_container_width=True
            )
        else:
            st.markdown("**Decoded from sliders:**")
            st.caption("Click 'Decode' to generate")


def _render_slider_grid(
    z_values: List[float],
    latent_dim: int,
    embeddings: np.ndarray,
    key_suffix: str = "",
):
    """Render a grid of latent dimension sliders."""
    # Compute variance per dimension for context
    var_per_dim = np.var(embeddings, axis=0)

    z_values_key = f"slider_z_values{key_suffix}"

    # Layout: 2 sliders per row
    updated = False
    for row_start in range(0, latent_dim, 2):
        cols = st.columns(2)
        for col_offset in range(2):
            d = row_start + col_offset
            if d >= latent_dim:
                break
            with cols[col_offset]:
                new_val = st.slider(
                    f"z[{d}] (var={var_per_dim[d]:.3f})",
                    min_value=-4.0,
                    max_value=4.0,
                    value=float(z_values[d]),
                    step=0.05,
                    key=f"z_slider_{d}{key_suffix}"
                )
                if new_val != z_values[d]:
                    z_values[d] = new_val
                    updated = True

    if updated:
        st.session_state[z_values_key] = z_values


def _render_conditioning_sliders(
    base_index: int,
    key_suffix: str = "",
) -> Optional[List[float]]:
    """
    Render H/S/V conditioning sliders for conditional models; return the override.

    For a FiLM-conditioned model, the user can vary the background
    Hue/Saturation/Value fed to the decoder (0-1 per variable) to see how
    conditioning re-colours the reconstruction at fixed latent z. Returns the
    list of conditioning values, or ``None`` for unconditional models (in which
    case the decode falls back to the base image's own conditioning).
    """
    cond_vars = st.session_state.get('conditional_variables') or []
    conditioning = st.session_state.get('conditioning')
    if not cond_vars or conditioning is None:
        return None

    defaults = conditioning[base_index]
    st.markdown("**Conditioning (decoder background HSV):**")
    st.caption(
        "Override the background colour fed to the decoder. Latent z stays "
        "fixed, so you isolate the effect of conditioning."
    )
    values: List[float] = []
    cols = st.columns(len(cond_vars))
    for i, var in enumerate(cond_vars):
        with cols[i]:
            label = var.replace('average_', '').title()
            default_val = float(defaults[i]) if i < len(defaults) else 0.5
            values.append(
                st.slider(
                    label,
                    min_value=0.0,
                    max_value=1.0,
                    value=max(0.0, min(1.0, default_val)),
                    step=0.05,
                    key=f"cond_slider_{var}{key_suffix}",
                )
            )
    return values


############################################################################
# Feature 5: Sprite Map (Image Mosaic on Embedding Layout)
############################################################################

_HIGH_CONTRAST_COLORS = [
    (230, 25, 75),    # Red
    (60, 180, 75),    # Green
    (255, 225, 25),   # Yellow
    (0, 130, 200),    # Blue
    (245, 130, 48),   # Orange
    (145, 30, 180),   # Purple
    (70, 240, 240),   # Cyan
    (240, 50, 230),   # Magenta
    (210, 245, 60),   # Lime
    (250, 190, 212),  # Pink
    (0, 128, 128),    # Teal
    (220, 190, 255),  # Lavender
]


def _get_cluster_colors(n_clusters: int) -> List[Tuple[int, int, int]]:
    """Get high-contrast colors for cluster visualization."""
    if n_clusters <= len(_HIGH_CONTRAST_COLORS):
        return _HIGH_CONTRAST_COLORS[:n_clusters]
    import colorsys
    colors = list(_HIGH_CONTRAST_COLORS)
    for i in range(len(_HIGH_CONTRAST_COLORS), n_clusters):
        hue = (i * 0.618033988749895) % 1.0
        sat = 0.7 + (i % 3) * 0.1
        val = 0.9 - (i % 2) * 0.15
        r, g, b = colorsys.hsv_to_rgb(hue, sat, val)
        colors.append((int(r * 255), int(g * 255), int(b * 255)))
    return colors


def _normalize_sprite_labels(raw: np.ndarray) -> np.ndarray:
    """String labels for sprite sampling (NaN -> 'unlabelled')."""
    s = pd.Series(raw)
    return s.fillna('unlabelled').astype(str).str.strip().replace('', 'unlabelled').values


def _sprite_map_pick_indices(
    n_total: int,
    max_samples: int,
    labels: Optional[np.ndarray],
    mode: str,
    exclude_unlabelled: bool,
    filter_labels: Optional[List[str]],
    rng: np.random.Generator,
) -> np.ndarray:
    """
    Choose row indices for the sprite map.

    Args:
        n_total: Number of points (length of coords / images).
        max_samples: Upper bound on count (always <= n_total after pooling).
        labels: Length-n_total label array, or None for uniform-only.
        mode: 'uniform' | 'stratified' | 'filtered'.
        exclude_unlabelled: If True, drop rows labelled 'unlabelled' (stratified/filtered).
        filter_labels: For mode 'filtered', only these label strings (after normalization).
        rng: NumPy random generator.

    Returns:
        Sorted 1D int array of indices into the full dataset.
    """
    if mode == 'uniform' or labels is None:
        if max_samples >= n_total:
            return np.arange(n_total)
        return np.sort(rng.choice(n_total, max_samples, replace=False))

    lab = _normalize_sprite_labels(np.asarray(labels))
    pool = np.arange(n_total)

    if exclude_unlabelled:
        m = lab != 'unlabelled'
        pool = pool[m]
        lab = lab[m]

    if len(pool) == 0:
        raise ValueError(
            "No samples left after excluding unlabelled rows. "
            "Disable 'Exclude unlabelled' or choose a column with more labels."
        )

    if mode == 'filtered':
        if not filter_labels:
            raise ValueError("Select at least one morphology label to include.")
        want = set(str(x) for x in filter_labels)
        keep_mask = np.array([li in want for li in lab])
        keep = pool[keep_mask]
        if len(keep) == 0:
            raise ValueError(
                "No rows match the selected labels. Check label spelling or column."
            )
        if len(keep) <= max_samples:
            return np.sort(keep)
        picked = rng.choice(keep, max_samples, replace=False)
        return np.sort(picked)

    # stratified: as-equal-as-possible per class
    unique = np.unique(lab)
    if len(unique) < 1:
        raise ValueError("No label groups found for stratified sampling.")

    per_class: Dict[str, List[int]] = {u: [] for u in unique}
    for global_idx, label_str in zip(pool, lab):
        per_class[label_str].append(int(global_idx))

    indices_list: List[int] = []
    budget = min(max_samples, len(pool))
    base = budget // len(unique)
    remainder = budget % len(unique)
    # Random order for which classes get +1 from remainder
    class_order = list(unique)
    rng.shuffle(class_order)
    extra = {c: base + (1 if i < remainder else 0) for i, c in enumerate(class_order)}

    for cls in unique:
        idxs = np.array(per_class[cls], dtype=int)
        cap = min(extra[cls], len(idxs))
        if cap <= 0:
            continue
        take = rng.choice(idxs, size=cap, replace=False)
        indices_list.extend(take.tolist())

    # If some classes had too few rows, we may be under budget; fill from pool
    if len(indices_list) < budget:
        have = set(indices_list)
        rest = [i for i in pool if i not in have]
        if rest:
            need = budget - len(indices_list)
            add = rng.choice(rest, min(need, len(rest)), replace=False)
            indices_list.extend(int(x) for x in add)

    return np.sort(np.array(indices_list, dtype=int))


def _generate_sprite_map(
    grid_size: int = 30,
    cell_size: int = 48,
    max_samples: Optional[int] = None,
    show_borders: bool = False,
    coords_2d_override: Optional[np.ndarray] = None,
    sample_indices: Optional[np.ndarray] = None,
    morph_labels: Optional[np.ndarray] = None,
    morph_color_map: Optional[Dict[str, np.ndarray]] = None,
    hd_tiebreak: bool = False,
) -> np.ndarray:
    """
    Generate a sprite map: a mosaic of actual images placed at their 2D
    embedding positions on a grid.

    Args:
        grid_size: Number of cells per side of the grid.
        cell_size: Pixel size of each image cell.
        max_samples: Limit samples when ``sample_indices`` is None (None = use all).
        show_borders: Draw cluster-colored borders if cluster_labels exist.
        sample_indices: Optional precomputed row indices into the full dataset;
            if set, ``max_samples`` is ignored for subsampling.
        morph_labels: Optional per-row morphology label strings (indexed by the
            full dataset, same length as ``coords_2d``). When provided together
            with ``morph_color_map``, cells whose normalized label appears in
            the map receive a colored frame. Unlabelled / NaN rows get no
            frame. Morphology borders take precedence over cluster borders.
        morph_color_map: Mapping of normalized label string -> uint8 RGB array.
            Built from the full label column (see :func:`_build_label_color_map`).
        hd_tiebreak: When True, cell collisions are decided by full-dim latent
            coherence with already-placed neighbors (radius-1 ring) instead of
            first-come-first-served. Falls back to FCFS when the colliding cell
            has no occupied neighbors yet.

    Returns:
        sprite_map as uint8 RGB array (H, W, 3).
    """
    from PIL import Image as PILImage

    images = st.session_state.images
    coords_2d = coords_2d_override if coords_2d_override is not None else st.session_state.coords_2d
    cluster_labels = st.session_state.get('cluster_labels')

    n_total = len(coords_2d)

    if sample_indices is not None:
        sample_indices = np.asarray(sample_indices, dtype=int)
        if sample_indices.size == 0:
            raise ValueError("sample_indices is empty.")
        if sample_indices.min() < 0 or sample_indices.max() >= n_total:
            raise ValueError("sample_indices out of range for current coordinates.")
    elif max_samples is not None and max_samples < n_total:
        rng = np.random.default_rng(42)
        sample_indices = np.sort(rng.choice(n_total, max_samples, replace=False))
    else:
        sample_indices = np.arange(n_total)

    coords_subset = coords_2d[sample_indices]

    # Normalize coordinates to [0, grid_size-1]
    mins = coords_subset.min(axis=0)
    maxs = coords_subset.max(axis=0)
    span = maxs - mins + 1e-8
    coords_norm = (coords_subset - mins) / span
    grid_coords = np.clip(
        (coords_norm * (grid_size - 1)).astype(int), 0, grid_size - 1
    )

    # Assign each sample to a grid cell, resolving collisions
    grid = np.full((grid_size, grid_size), -1, dtype=int)
    grid_positions = {}

    # Pull full-dim latents for the high-dim tiebreak if requested.
    hd_embeddings: Optional[np.ndarray] = None
    if hd_tiebreak:
        _emb = st.session_state.get('embeddings')
        if _emb is not None and len(_emb) == n_total:
            hd_embeddings = np.asarray(_emb)
        else:
            logger.warning(
                "hd_tiebreak requested but session embeddings are missing or "
                "misaligned; falling back to first-come-first-served."
            )

    def _neighbor_latents(gx_c: int, gy_c: int) -> Optional[np.ndarray]:
        """Latents of images already placed in the 8 cells around (gx_c, gy_c)."""
        if hd_embeddings is None:
            return None
        rows = []
        for dy in (-1, 0, 1):
            for dx in (-1, 0, 1):
                if dx == 0 and dy == 0:
                    continue
                nx, ny = gx_c + dx, gy_c + dy
                if 0 <= nx < grid_size and 0 <= ny < grid_size:
                    occ_local = grid[ny, nx]
                    if occ_local != -1:
                        rows.append(hd_embeddings[sample_indices[occ_local]])
        if not rows:
            return None
        return np.stack(rows, axis=0)

    def _spiral_place(idx_to_place: int, gx_c: int, gy_c: int) -> None:
        """Place ``idx_to_place`` in the nearest empty cell within radius 3."""
        for radius in range(1, 4):
            for dx in range(-radius, radius + 1):
                for dy in range(-radius, radius + 1):
                    if abs(dx) != radius and abs(dy) != radius:
                        continue
                    nx, ny = gx_c + dx, gy_c + dy
                    if 0 <= nx < grid_size and 0 <= ny < grid_size and grid[ny, nx] == -1:
                        grid[ny, nx] = idx_to_place
                        grid_positions[idx_to_place] = (nx, ny)
                        return

    for local_idx, (gx, gy) in enumerate(grid_coords):
        gx_i, gy_i = int(gx), int(gy)
        if grid[gy_i, gx_i] == -1:
            grid[gy_i, gx_i] = local_idx
            grid_positions[local_idx] = (gx_i, gy_i)
            continue

        # Collision. Decide who owns the exact cell.
        incumbent = int(grid[gy_i, gx_i])
        evict_incumbent = False
        if hd_embeddings is not None:
            neigh = _neighbor_latents(gx_i, gy_i)
            if neigh is not None:
                new_lat = hd_embeddings[sample_indices[local_idx]]
                inc_lat = hd_embeddings[sample_indices[incumbent]]
                d_new = float(np.linalg.norm(neigh - new_lat, axis=1).mean())
                d_inc = float(np.linalg.norm(neigh - inc_lat, axis=1).mean())
                if d_new < d_inc:
                    evict_incumbent = True

        if evict_incumbent:
            del grid_positions[incumbent]
            grid[gy_i, gx_i] = local_idx
            grid_positions[local_idx] = (gx_i, gy_i)
            _spiral_place(incumbent, gx_i, gy_i)
        else:
            _spiral_place(local_idx, gx_i, gy_i)

    # Build the mosaic image
    map_h = grid_size * cell_size
    map_w = grid_size * cell_size
    sprite_map = np.ones((map_h, map_w, 3), dtype=np.uint8) * 242  # light gray bg

    # Cluster colors
    cluster_colors = None
    if show_borders and cluster_labels is not None:
        n_clusters = int(cluster_labels.max()) + 1
        cluster_colors = _get_cluster_colors(n_clusters)

    progress = st.progress(0, text="Building sprite map...")
    placed_count = len(grid_positions)

    for count, (local_idx, (gx, gy)) in enumerate(grid_positions.items()):
        global_idx = sample_indices[local_idx]

        # Prepare the image
        raw_img = _image_array_for_display(global_idx, images)
        pil_img = PILImage.fromarray(raw_img).resize(
            (cell_size, cell_size), PILImage.LANCZOS
        )
        cell_arr = np.array(pil_img)

        x_start = gx * cell_size
        y_start = (grid_size - 1 - gy) * cell_size  # flip y so up = higher values

        sprite_map[y_start:y_start + cell_size, x_start:x_start + cell_size] = cell_arr

        # Pick a border color: morphology label wins over cluster.
        border_color: Optional[np.ndarray] = None
        border_width: int = max(2, cell_size // 16)
        if morph_labels is not None and morph_color_map:
            norm = _normalize_label(morph_labels[global_idx])
            if norm is not None and norm in morph_color_map:
                border_color = morph_color_map[norm]
                border_width = max(2, cell_size // 20)
        if border_color is None and cluster_colors is not None and cluster_labels is not None:
            label = int(cluster_labels[global_idx])
            border_color = cluster_colors[label]

        if border_color is not None:
            bw = border_width
            # Top and bottom edges
            for b in range(bw):
                if y_start + b < map_h:
                    sprite_map[y_start + b, x_start:x_start + cell_size] = border_color
                if y_start + cell_size - 1 - b >= 0:
                    sprite_map[y_start + cell_size - 1 - b, x_start:x_start + cell_size] = border_color
            # Left and right edges
            for b in range(bw):
                if x_start + b < map_w:
                    sprite_map[y_start:y_start + cell_size, x_start + b] = border_color
                if x_start + cell_size - 1 - b >= 0:
                    sprite_map[y_start:y_start + cell_size, x_start + cell_size - 1 - b] = border_color

        if (count + 1) % 50 == 0 or count == placed_count - 1:
            progress.progress(
                (count + 1) / placed_count,
                text=f"Placing images... {count + 1}/{placed_count}"
            )

    progress.empty()
    return sprite_map


def _render_cluster_enrichment():
    """Render cluster–label hypergeometric enrichment analysis."""
    import plotly.express as px

    from candescence.tlv.analysis.hypergeometric_enrichment import (
        compute_hypergeometric_enrichment,
        summarize_cluster_enrichment,
    )

    with st.expander("Cluster Label Enrichment (Hypergeometric)", expanded=False):
        metadata_df = st.session_state.metadata_df
        if metadata_df is None:
            st.info("Load data first to use enrichment analysis.")
            return

        # --- Label column selector ---
        categorical_cols = _order_label_columns_for_ui(_metadata_label_columns(metadata_df))
        if not categorical_cols:
            st.warning(_no_labels_message(metadata_df))
            return

        default_idx = 0
        if 'manual_formation' in categorical_cols:
            default_idx = categorical_cols.index('manual_formation')

        label_col = st.selectbox(
            "Label column",
            categorical_cols,
            index=default_idx,
            key="enrichment_label_col",
        )

        # --- Cluster source selector ---
        cluster_source = st.selectbox(
            "Cluster source",
            ["K-Means (high-dim)", "K-Means (projected 2D)"],
            key="enrichment_cluster_source",
        )

        col_a, col_b, col_c = st.columns(3)
        with col_a:
            n_clusters = st.slider(
                "k (clusters)",
                min_value=2, max_value=20,
                value=st.session_state.n_clusters,
                key="enrichment_n_clusters",
            )
        with col_b:
            exclude_unlabelled = st.checkbox(
                "Exclude unlabelled", value=False,
                key="enrichment_exclude_unlabelled",
            )
        with col_c:
            q_threshold = st.number_input(
                "Significance threshold (q)",
                min_value=0.001, max_value=1.0,
                value=0.05, step=0.01,
                key="enrichment_q_threshold",
            )

        if st.button("Run enrichment analysis", key="enrichment_run"):
            # Compute clusters
            from sklearn.cluster import KMeans

            if cluster_source == "K-Means (high-dim)":
                data = st.session_state.embeddings
            else:
                data = st.session_state.coords_2d

            if data is None:
                st.error("No embedding data available.")
                return

            cluster_labels = KMeans(
                n_clusters=n_clusters, random_state=42, n_init=10
            ).fit_predict(data)

            labels = metadata_df[label_col].values

            # Run enrichment
            enrichment_df = compute_hypergeometric_enrichment(
                labels, cluster_labels,
                exclude_unlabelled=exclude_unlabelled,
            )

            if enrichment_df.empty:
                st.warning(
                    "No enrichment results. Check that the label column "
                    "has at least 2 distinct classes."
                )
                return

            # Store for persistence across reruns
            st.session_state['enrichment_results'] = enrichment_df
            st.session_state['enrichment_summary'] = summarize_cluster_enrichment(
                enrichment_df, significance_threshold=q_threshold
            )
            # Must not assign to enrichment_q_threshold — that key is owned by the widget above.
            st.session_state['enrichment_applied_q'] = q_threshold

        # Display results if available
        enrichment_df = st.session_state.get('enrichment_results')
        if enrichment_df is not None and not enrichment_df.empty:
            summary_df = st.session_state.get('enrichment_summary')
            q_thresh = st.session_state.get(
                'enrichment_applied_q',
                st.session_state.get('enrichment_q_threshold', 0.05),
            )

            st.markdown("#### Per-cluster summary")
            if summary_df is not None and not summary_df.empty:
                st.dataframe(summary_df, use_container_width=True)
            else:
                st.info("No summary available.")

            st.markdown("#### Full results")
            sig_only = st.checkbox(
                "Show significant only", value=False,
                key="enrichment_sig_only",
            )
            display_df = enrichment_df.copy()
            if sig_only:
                display_df = display_df[display_df["q_value"] < q_thresh]
            st.dataframe(
                display_df.sort_values(["cluster", "q_value"]),
                use_container_width=True,
            )

            # Heatmap of -log10(q_value) by cluster x label
            st.markdown("#### Enrichment heatmap")
            pivot = enrichment_df.pivot_table(
                index="cluster", columns="label",
                values="q_value", aggfunc="min",
            )
            # -log10 transform, cap at 10 for readability
            heatmap_data = -np.log10(pivot.clip(lower=1e-10))
            heatmap_data = heatmap_data.clip(upper=10)

            fig = px.imshow(
                heatmap_data,
                labels=dict(x="Label", y="Cluster", color="-log10(q)"),
                color_continuous_scale="YlOrRd",
                aspect="auto",
            )
            fig.update_layout(
                height=max(250, 50 * len(heatmap_data)),
                margin=dict(l=10, r=10, t=30, b=10),
            )
            st.plotly_chart(fig, use_container_width=True)

            st.caption(
                "**Interpretation:** Small q-value + enrichment ratio > 1 indicates "
                "label over-representation in that cluster beyond chance. "
                "Heatmap shows -log10(q): higher values = stronger enrichment."
            )


def _render_silhouette_mantel():
    """Render silhouette and Mantel test analysis section."""
    import plotly.graph_objects as go

    from candescence.tlv.analysis.latent_space_metrics import (
        SkbioUnavailableError,
        mantel_latent_vs_numeric,
        silhouette_metrics,
    )

    with st.expander("Silhouette & Mantel (Latent vs Metadata)", expanded=False):
        metadata_df = st.session_state.metadata_df
        if metadata_df is None:
            st.info("Load data first.")
            return

        all_embeddings = st.session_state.get('all_embeddings', {})
        if not all_embeddings:
            st.info("No embeddings loaded.")
            return

        # --- Latent space selector ---
        tendril_keys = st.session_state.get('tendril_keys', [])
        space_options = ['primary'] + list(tendril_keys)
        space_name = st.selectbox(
            "Latent space",
            space_options,
            format_func=lambda x: "Primary (mu)" if x == "primary" else f"Tendril: {x}",
            key="sil_mantel_space",
        )
        X = all_embeddings.get(space_name)
        if X is None:
            st.warning(f"No embeddings for space '{space_name}'.")
            return

        coords_2d = st.session_state.get('all_coords_2d', {}).get(space_name)

        # ============================================================
        # Silhouette sub-section
        # ============================================================
        st.markdown("### Silhouette analysis")

        label_source = st.radio(
            "Label source",
            ["Metadata column", "K-Means (high-dim)", "K-Means (projected 2D)"],
            key="sil_label_source",
        )

        sil_col_a, sil_col_b = st.columns(2)
        sil_labels = None

        if label_source == "Metadata column":
            cat_cols = _order_label_columns_for_ui(_metadata_label_columns(metadata_df))
            if not cat_cols:
                st.warning(_no_labels_message(metadata_df))
            else:
                default_idx = 0
                if 'manual_formation' in cat_cols:
                    default_idx = cat_cols.index('manual_formation')
                with sil_col_a:
                    sil_label_col = st.selectbox(
                        "Label column",
                        cat_cols,
                        index=default_idx,
                        key="sil_label_col",
                    )
                with sil_col_b:
                    sil_exclude = st.checkbox(
                        "Exclude unlabelled",
                        value=False,
                        key="sil_exclude_unlabelled",
                    )
                sil_labels = metadata_df[sil_label_col].values
        else:
            # K-Means labels
            with sil_col_a:
                sil_k = st.slider(
                    "k (clusters)",
                    min_value=2, max_value=20,
                    value=st.session_state.n_clusters,
                    key="sil_n_clusters",
                )
            sil_exclude = False
            from sklearn.cluster import KMeans
            if label_source == "K-Means (high-dim)":
                sil_data = X
            else:
                sil_data = coords_2d
                if sil_data is None:
                    st.warning("No 2D projection available for this space.")

            if sil_data is not None:
                sil_labels = KMeans(
                    n_clusters=sil_k, random_state=42, n_init=10
                ).fit_predict(sil_data).astype(str)

        if st.button("Run silhouette", key="sil_run"):
            if sil_labels is None:
                st.error("No labels available.")
            else:
                try:
                    sil_X = X
                    result = silhouette_metrics(
                        sil_X, sil_labels,
                        exclude_unlabelled=(
                            sil_exclude if label_source == "Metadata column" else False
                        ),
                    )
                    st.session_state['sil_result'] = result
                    st.session_state['sil_space'] = space_name
                except ValueError as e:
                    st.error(str(e))

        # Display silhouette results
        sil_result = st.session_state.get('sil_result')
        if sil_result is not None and st.session_state.get('sil_space') == space_name:
            m_col1, m_col2 = st.columns(2)
            with m_col1:
                st.metric("Overall silhouette score", f"{sil_result.overall_score:.4f}")
            with m_col2:
                st.metric("Samples used", f"{len(sil_result.sample_scores)}")

            # Silhouette bar chart (per-class sorted bars)
            unique_labels = np.unique(sil_result.labels)
            bar_data = []
            y_lower = 0
            for label in sorted(unique_labels):
                cluster_vals = sil_result.sample_scores[sil_result.labels == label]
                cluster_vals = np.sort(cluster_vals)
                for val in cluster_vals:
                    bar_data.append((label, y_lower, val))
                    y_lower += 1

            if bar_data:
                fig = go.Figure()
                # Group by label for colored horizontal bars
                for label in sorted(unique_labels):
                    entries = [(y, v) for (l, y, v) in bar_data if l == label]
                    ys = [e[0] for e in entries]
                    vals = [e[1] for e in entries]
                    color = MORPHOLOGY_PALETTE.get(
                        str(label),
                        _PLOTLY_QUALITATIVE[
                            list(sorted(unique_labels)).index(label)
                            % len(_PLOTLY_QUALITATIVE)
                        ],
                    )
                    fig.add_trace(go.Bar(
                        y=ys, x=vals,
                        orientation='h',
                        name=str(label),
                        marker_color=color,
                        showlegend=True,
                    ))
                # Red dashed line at overall score
                fig.add_vline(
                    x=sil_result.overall_score,
                    line_dash="dash", line_color="red",
                    annotation_text=f"avg = {sil_result.overall_score:.3f}",
                )
                fig.update_layout(
                    title="Silhouette values per sample",
                    xaxis_title="Silhouette coefficient",
                    yaxis_title="Sample (sorted by class)",
                    height=max(300, 4 * len(bar_data)),
                    barmode='stack',
                    bargap=0,
                    showlegend=True,
                    yaxis=dict(showticklabels=False),
                    margin=dict(l=10, r=10, t=40, b=10),
                )
                st.plotly_chart(fig, use_container_width=True)

            st.caption(
                "**Silhouette score** measures how similar each sample is to its own "
                "class compared to the nearest other class. Range [-1, 1]; higher = "
                "better separation."
            )

        # ============================================================
        # Mantel sub-section
        # ============================================================
        st.markdown("---")
        st.markdown("### Mantel test (latent distance vs trait distance)")

        numeric_cols = metadata_df.select_dtypes(include=[np.number]).columns.tolist()
        if not numeric_cols:
            st.info("No numeric metadata columns available for Mantel test.")
            return

        man_col_a, man_col_b, man_col_c = st.columns(3)
        with man_col_a:
            mantel_col = st.selectbox(
                "Numeric column",
                numeric_cols,
                key="mantel_col",
            )
        with man_col_b:
            mantel_perms = st.number_input(
                "Permutations",
                min_value=99, max_value=9999,
                value=999, step=100,
                key="mantel_perms",
            )
        with man_col_c:
            mantel_max_n = st.number_input(
                "Max samples (0 = all)",
                min_value=0, max_value=10000,
                value=500, step=100,
                key="mantel_max_n",
                help="Mantel is O(N²) memory. Subsample for large datasets.",
            )

        n_total = len(X)
        effective_max = mantel_max_n if mantel_max_n > 0 else None
        if effective_max and n_total > effective_max:
            st.warning(
                f"N = {n_total} > max_n = {effective_max}. "
                f"Results will be based on a random subsample of {effective_max} points."
            )
        elif n_total > 500 and effective_max is None:
            st.warning(
                f"N = {n_total} is large for Mantel (O(N²) memory). "
                "Consider setting a max sample size."
            )

        if st.button("Run Mantel test", key="mantel_run"):
            y = metadata_df[mantel_col].values.astype(float)
            try:
                result = mantel_latent_vs_numeric(
                    X, y,
                    permutations=int(mantel_perms),
                    max_n=effective_max,
                )
                st.session_state['mantel_result'] = result
                st.session_state['mantel_space'] = space_name
                st.session_state['mantel_col_name'] = mantel_col
            except SkbioUnavailableError:
                st.info(
                    "scikit-bio is required for the Mantel test. "
                    "Install with: `pip install 'candescence[analysis]'` "
                    "or `pip install scikit-bio`."
                )
            except ValueError as e:
                st.error(str(e))

        # Display Mantel results
        mantel_result = st.session_state.get('mantel_result')
        if (
            mantel_result is not None
            and st.session_state.get('mantel_space') == space_name
        ):
            m1, m2, m3 = st.columns(3)
            with m1:
                st.metric("Mantel r", f"{mantel_result.statistic:.4f}")
            with m2:
                st.metric("p-value", f"{mantel_result.p_value:.4f}")
            with m3:
                st.metric(
                    "N used",
                    f"{mantel_result.n_samples}"
                    + (" (subsampled)" if mantel_result.subsampled else ""),
                )

            col_name = st.session_state.get('mantel_col_name', '?')
            st.caption(
                f"**Mantel test** measures the Pearson correlation between pairwise "
                f"latent-space distances and pairwise |{col_name}| differences. "
                f"p-value from {mantel_result.permutations} permutations. "
                f"Significant r > 0 means samples closer in latent space tend to "
                f"have more similar {col_name} values."
            )

        # ============================================================
        # Replicate consistency sub-section
        # ============================================================
        st.markdown("---")
        st.markdown("### Replicate consistency (latent distances)")
        st.caption(
            "Distance between replicate-pair embeddings vs distance between "
            "random non-replicate pairs. AUROC near 1.0 and large positive "
            "Cohen's d mean replicates land close together in this latent space."
        )

        if 'my_rep' not in metadata_df.columns:
            st.info("Metadata has no `my_rep` column — replicate consistency unavailable.")
        else:
            rep_col_a, rep_col_b = st.columns(2)
            with rep_col_a:
                rep_n_rand = st.number_input(
                    "Random pairs to sample",
                    min_value=10, max_value=20000, value=500, step=50,
                    key="rep_n_rand",
                )
            with rep_col_b:
                rep_seed = st.number_input(
                    "Random seed", min_value=0, max_value=10_000,
                    value=42, step=1, key="rep_seed",
                )

            if st.button("Run replicate consistency", key="rep_run"):
                from candescence.tlv.inference.tendril_replicate_diagnostics import (
                    build_nonrep_pairs,
                    build_replicate_pairs,
                )
                from candescence.tlv.analysis.replicate_consistency import (
                    ReplicateConsistencyResult,
                    replicate_random_auroc,
                    replicate_random_cohens_d,
                )

                # Need an `id` column with values aligned to embedding rows
                df_for_pairs = metadata_df.copy()
                if 'id' not in df_for_pairs.columns:
                    df_for_pairs['id'] = np.arange(len(df_for_pairs))
                # Map id → row index for embedding lookup
                id_to_row = {v: i for i, v in enumerate(df_for_pairs['id'].values)}

                rep_pairs = build_replicate_pairs(df_for_pairs, rep_col='my_rep')
                if len(rep_pairs) < 2:
                    st.warning(
                        f"Found only {len(rep_pairs)} replicate pairs in `my_rep`; "
                        "need at least 2."
                    )
                else:
                    rand_pairs = build_nonrep_pairs(
                        df_for_pairs,
                        n_pairs=int(rep_n_rand),
                        rep_col='my_rep',
                        random_state=int(rep_seed),
                    )

                    def _pair_dists(pairs):
                        out = []
                        for a, b in pairs:
                            ia = id_to_row.get(a)
                            ib = id_to_row.get(b)
                            if ia is None or ib is None:
                                continue
                            out.append(float(np.linalg.norm(X[ia] - X[ib])))
                        return np.asarray(out, dtype=np.float64)

                    rep_d = _pair_dists(rep_pairs)
                    rand_d = _pair_dists(rand_pairs)

                    result = ReplicateConsistencyResult(
                        auroc=replicate_random_auroc(rep_d, rand_d),
                        cohens_d=replicate_random_cohens_d(rep_d, rand_d),
                        mean_rep=float(np.mean(rep_d)) if rep_d.size else float("nan"),
                        mean_rand=float(np.mean(rand_d)) if rand_d.size else float("nan"),
                        n_rep_pairs=int(rep_d.size),
                        n_rand_pairs=int(rand_d.size),
                    )
                    cache = st.session_state.setdefault(
                        'replicate_consistency_cache', {}
                    )
                    cache[(space_name, int(rep_n_rand), int(rep_seed))] = result
                    st.session_state['rep_consistency_last_key'] = (
                        space_name, int(rep_n_rand), int(rep_seed)
                    )

            last_key = st.session_state.get('rep_consistency_last_key')
            cache = st.session_state.get('replicate_consistency_cache', {})
            if last_key and last_key[0] == space_name and last_key in cache:
                rc = cache[last_key]
                rc1, rc2, rc3, rc4 = st.columns(4)
                rc1.metric("AUROC", f"{rc.auroc:.4f}")
                rc2.metric("Cohen's d", f"{rc.cohens_d:.3f}")
                rc3.metric("Mean rep dist", f"{rc.mean_rep:.4f}")
                rc4.metric("Mean rand dist", f"{rc.mean_rand:.4f}")
                st.caption(
                    f"{rc.n_rep_pairs} replicate pairs vs {rc.n_rand_pairs} random pairs."
                )

        # ============================================================
        # Mantel vs image space sub-section
        # ============================================================
        st.markdown("---")
        st.markdown("### Mantel test (latent vs image space)")
        st.caption(
            "Correlate the latent distance matrix against pairwise distances "
            "computed directly from images. Useful to ask: does the encoder "
            "preserve raw-pixel geometry, or hand-crafted feature geometry, "
            "after greyscale/normalisation?"
        )

        images_arr = st.session_state.get('images')
        if images_arr is None:
            st.info("No image array loaded — image-space Mantel unavailable.")
        else:
            img_modes = st.multiselect(
                "Image space",
                ["Raw pixels", "Hand-crafted features"],
                default=["Raw pixels", "Hand-crafted features"],
                key="mantel_img_modes",
            )
            mi_col_a, mi_col_b = st.columns(2)
            with mi_col_a:
                mantel_img_perms = st.number_input(
                    "Permutations",
                    min_value=99, max_value=9999, value=999, step=100,
                    key="mantel_img_perms",
                )
            with mi_col_b:
                mantel_img_max_n = st.number_input(
                    "Max samples (0 = all)",
                    min_value=0, max_value=10000, value=500, step=100,
                    key="mantel_img_max_n",
                    help="Image-space Mantel is O(N²) memory; subsample for large datasets.",
                )

            n_total = len(X)
            effective_max_img = mantel_img_max_n if mantel_img_max_n > 0 else None
            if effective_max_img and n_total > effective_max_img:
                st.warning(
                    f"N = {n_total} > max_n = {effective_max_img}. "
                    f"Each selected matrix will be subsampled to {effective_max_img} rows."
                )
            elif n_total > 500 and effective_max_img is None:
                st.warning(
                    f"N = {n_total} is large for Mantel (O(N²) memory). "
                    "Consider setting a max sample size."
                )

            if st.button("Run image-space Mantel", key="mantel_img_run"):
                from candescence.tlv.analysis.image_distance_metrics import (
                    feature_distance_matrix,
                    raw_pixel_distance_matrix,
                )
                from candescence.tlv.analysis.latent_space_metrics import (
                    mantel_latent_vs_matrix,
                )

                if not img_modes:
                    st.warning("Select at least one image space.")
                else:
                    # Optional shared subsample so the latent + image matrices align
                    if effective_max_img and n_total > effective_max_img:
                        rng = np.random.default_rng(42)
                        idx = np.sort(rng.choice(n_total, size=effective_max_img, replace=False))
                        X_sub = X[idx]
                        imgs_sub = images_arr[idx]
                        subsampled = True
                    else:
                        X_sub = X
                        imgs_sub = images_arr
                        subsampled = False

                    rows = []
                    try:
                        for mode in img_modes:
                            if mode == "Raw pixels":
                                D_img = raw_pixel_distance_matrix(imgs_sub)
                            else:
                                D_img = feature_distance_matrix(imgs_sub)
                            res = mantel_latent_vs_matrix(
                                X_sub, D_img,
                                permutations=int(mantel_img_perms),
                                max_n=None,  # already subsampled
                            )
                            rows.append({
                                "image_space": mode,
                                "mantel_r": res.statistic,
                                "p_value": res.p_value,
                                "n_samples": res.n_samples,
                                "subsampled": subsampled,
                            })
                    except SkbioUnavailableError:
                        st.info(
                            "scikit-bio is required for the Mantel test. "
                            "Install with: `pip install 'candescence[analysis]'` "
                            "or `pip install scikit-bio`."
                        )
                        rows = []
                    except ValueError as e:
                        st.error(str(e))
                        rows = []

                    if rows:
                        cache = st.session_state.setdefault(
                            'mantel_image_cache', {}
                        )
                        cache_key = (
                            space_name,
                            tuple(img_modes),
                            int(mantel_img_perms),
                            int(mantel_img_max_n),
                        )
                        cache[cache_key] = pd.DataFrame(rows)
                        st.session_state['mantel_image_last_key'] = cache_key

            last_img_key = st.session_state.get('mantel_image_last_key')
            cache_img = st.session_state.get('mantel_image_cache', {})
            if (
                last_img_key
                and last_img_key[0] == space_name
                and last_img_key in cache_img
            ):
                st.dataframe(cache_img[last_img_key], use_container_width=True)


def _render_local_label_metrics():
    """Render k-NN label purity and within/between distance effect analysis."""
    import plotly.graph_objects as go

    from candescence.tlv.analysis.latent_space_metrics import (
        knn_label_purity,
        within_between_distance_effect,
    )

    with st.expander("Local label quality (k-NN & distance effect)", expanded=False):
        metadata_df = st.session_state.metadata_df
        if metadata_df is None:
            st.info("Load data first.")
            return

        all_embeddings = st.session_state.get('all_embeddings', {})
        if not all_embeddings:
            st.info("No embeddings loaded.")
            return

        # --- Latent space selector ---
        tendril_keys = st.session_state.get('tendril_keys', [])
        space_options = ['primary'] + list(tendril_keys)
        space_name = st.selectbox(
            "Latent space",
            space_options,
            format_func=lambda x: "Primary (mu)" if x == "primary" else f"Tendril: {x}",
            key="knn_dist_space",
        )
        X = all_embeddings.get(space_name)
        if X is None:
            st.warning(f"No embeddings for space '{space_name}'.")
            return

        # --- Label column selector ---
        cat_cols = _order_label_columns_for_ui(_metadata_label_columns(metadata_df))
        if not cat_cols:
            st.warning(_no_labels_message(metadata_df))
            return

        default_idx = 0
        if 'manual_formation' in cat_cols:
            default_idx = cat_cols.index('manual_formation')

        col_a, col_b = st.columns(2)
        with col_a:
            label_col = st.selectbox(
                "Label column",
                cat_cols,
                index=default_idx,
                key="knn_dist_label_col",
            )
        with col_b:
            exclude_unlabelled = st.checkbox(
                "Exclude unlabelled",
                value=False,
                key="knn_dist_exclude_unlabelled",
            )

        labels = metadata_df[label_col].values

        # ============================================================
        # k-NN purity sub-section
        # ============================================================
        st.markdown("### k-NN label purity")

        knn_col_a, knn_col_b = st.columns(2)
        with knn_col_a:
            k_val = st.slider(
                "k (neighbors)",
                min_value=1, max_value=50,
                value=10,
                key="knn_k_slider",
            )

        if st.button("Run k-NN purity", key="knn_purity_run"):
            try:
                result = knn_label_purity(
                    X, labels, k=k_val,
                    exclude_unlabelled=exclude_unlabelled,
                )
                st.session_state['knn_result'] = result
                st.session_state['knn_space'] = space_name
            except ValueError as e:
                st.error(str(e))

        knn_result = st.session_state.get('knn_result')
        if knn_result is not None and st.session_state.get('knn_space') == space_name:
            m1, m2, m3 = st.columns(3)
            with m1:
                st.metric("Mean purity", f"{knn_result.mean_purity:.4f}")
            with m2:
                st.metric("k used", f"{knn_result.k_used}")
            with m3:
                st.metric("Samples / classes", f"{knn_result.n_samples} / {knn_result.n_classes}")

            # Histogram of per-sample purity
            fig = go.Figure(data=[go.Histogram(
                x=knn_result.per_sample_purity,
                nbinsx=30,
                marker_color='steelblue',
            )])
            fig.update_layout(
                title="Per-sample k-NN purity distribution",
                xaxis_title="Purity (fraction of k neighbors with same label)",
                yaxis_title="Count",
                height=300,
                margin=dict(l=10, r=10, t=40, b=10),
            )
            st.plotly_chart(fig, use_container_width=True)

            st.caption(
                "**k-NN purity** measures the fraction of each point's *k* nearest "
                "neighbors that share its label. High purity (close to 1.0) means "
                "labels are locally consistent in the latent space."
            )

        # ============================================================
        # Within/between distance effect sub-section
        # ============================================================
        st.markdown("---")
        st.markdown("### Within / between-class distance effect")

        wb_col_a, wb_col_b = st.columns(2)
        with wb_col_a:
            max_pairs = st.number_input(
                "Max pairs",
                min_value=1000, max_value=500_000,
                value=100_000, step=10_000,
                key="wb_max_pairs",
            )

        if st.button("Run distance effect", key="wb_run"):
            try:
                result = within_between_distance_effect(
                    X, labels,
                    exclude_unlabelled=exclude_unlabelled,
                    max_pairs=int(max_pairs),
                )
                st.session_state['wb_result'] = result
                st.session_state['wb_space'] = space_name
            except ValueError as e:
                st.error(str(e))

        wb_result = st.session_state.get('wb_result')
        if wb_result is not None and st.session_state.get('wb_space') == space_name:
            m1, m2, m3, m4 = st.columns(4)
            with m1:
                st.metric("Mean within", f"{wb_result.mean_within:.4f}")
            with m2:
                st.metric("Mean between", f"{wb_result.mean_between:.4f}")
            with m3:
                st.metric("Ratio (B/W)", f"{wb_result.ratio:.3f}")
            with m4:
                st.metric("Cohen's d", f"{wb_result.cohens_d:.3f}")

            st.caption(
                f"Sampled {wb_result.n_within_pairs:,} within-class and "
                f"{wb_result.n_between_pairs:,} between-class pairs. "
                f"**Ratio > 1** means between-class distances are larger than "
                f"within-class (good separation). **Cohen's d** measures effect size: "
                f"small ≈ 0.2, medium ≈ 0.5, large ≈ 0.8+."
            )


def _render_rank_change_across_spaces():
    """Render rank-change analysis across multiple latent spaces."""
    from candescence.tlv.analysis.tendril_rank_change import rank_change_across_spaces

    all_embeddings = st.session_state.get('all_embeddings', {})

    with st.expander("Rank change across latent spaces", expanded=False):
        if len(all_embeddings) < 2:
            st.info(
                "Load a model with tendrils or ensure multiple spaces are available. "
                "Rank-change analysis requires at least 2 latent spaces."
            )
            return

        # --- Space selection and ordering ---
        tendril_keys = st.session_state.get('tendril_keys', [])
        available_spaces = ['primary'] + sorted(tendril_keys)

        selected_spaces = st.multiselect(
            "Spaces (order matters)",
            available_spaces,
            default=available_spaces,
            key="rc_spaces",
        )

        if len(selected_spaces) < 2:
            st.warning("Select at least 2 spaces.")
            return

        # Validate all selected spaces have embeddings
        for s in selected_spaces:
            if s not in all_embeddings:
                st.warning(f"No embeddings for space '{s}'.")
                return

        # --- Controls ---
        rc_col_a, rc_col_b = st.columns(2)
        with rc_col_a:
            n_pairs = st.slider(
                "Number of pairs",
                min_value=100, max_value=5000,
                value=2000, step=100,
                key="rc_n_pairs",
            )
        with rc_col_b:
            seed = st.number_input(
                "Random seed",
                min_value=0, max_value=9999,
                value=42,
                key="rc_seed",
            )

        if st.button("Run rank-change analysis", key="rc_run"):
            emb_dict = {s: all_embeddings[s] for s in selected_spaces}
            try:
                result = rank_change_across_spaces(
                    emb_dict,
                    n_pairs=n_pairs,
                    random_state=int(seed),
                )
                st.session_state['rc_result'] = result
                st.session_state['rc_spaces_used'] = selected_spaces
            except ValueError as e:
                st.error(str(e))

        rc_result = st.session_state.get('rc_result')
        if rc_result is not None and st.session_state.get('rc_spaces_used') == selected_spaces:
            ov = rc_result.overall

            # Overall metrics
            m1, m2, m3 = st.columns(3)
            with m1:
                st.metric("Mean rank variance", f"{ov['mean_rank_var']:.2f}")
            with m2:
                st.metric("Median rank variance", f"{ov['median_rank_var']:.2f}")
            with m3:
                st.metric("Mean rank range", f"{ov['mean_rank_range']:.1f}")

            st.caption(
                f"Computed over {ov['n_pairs']:,} pairs across {ov['n_spaces']} spaces. "
                "**Low rank variance** means pairwise distance orderings are stable "
                "across spaces (tendrils preserve relative structure). "
                "**High rank variance** means the ordering of which pairs are "
                "'close' vs 'far' reshuffles between spaces."
            )

            # Moves table
            st.markdown("#### Layer-to-layer transitions")
            st.dataframe(
                rc_result.moves.set_index('transition'),
                use_container_width=True,
            )
            st.caption(
                "**Mean |Δrank|** shows how much pair rankings shift on average "
                "between consecutive spaces. Large values indicate structural "
                "reorganization at that transition."
            )

            # Download options
            st.markdown("#### Export")
            dl_col_a, dl_col_b = st.columns(2)
            with dl_col_a:
                csv_summary = rc_result.pair_summary.to_csv(index=False)
                st.download_button(
                    "Download pair summary (CSV)",
                    data=csv_summary,
                    file_name="rank_change_pair_summary.csv",
                    mime="text/csv",
                    key="rc_dl_summary",
                )
            with dl_col_b:
                csv_ranks = rc_result.rank_table.to_csv(index=False)
                st.download_button(
                    "Download rank table (CSV)",
                    data=csv_ranks,
                    file_name="rank_change_rank_table.csv",
                    mime="text/csv",
                    key="rc_dl_ranks",
                )


def _strip_rep(stem: str) -> str:
    """Replicate key = everything except the replicate number.

    ``"P12_ctrl_day2_1-r3-c4"`` -> ``"P12_ctrl_day2-r3-c4"`` (keep position)
    ``"Pl9_spider_day5_2"``     -> ``"Pl9_spider_day5"``       (no position)
    """
    import re
    m = re.match(r'^(.+)_\d+(-r\d+-c\d+)$', stem)
    if m:
        return m.group(1) + m.group(2)
    m2 = re.match(r'^(.+)_\d+$', stem)
    return m2.group(1) if m2 else stem


def _scan_replicate_pairs_from_disk(
    images_dir: Path,
) -> Tuple[List[Tuple[str, str]], Dict[str, Path]]:
    """Fast metadata-only scan: find replicate pairs without loading pixels.

    Returns ``(rep_pairs, stem_to_file)`` where *rep_pairs* is a list of
    ``(stem_a, stem_b)`` tuples for size-2 replicate groups.
    """
    from collections import defaultdict

    all_files = sorted(
        list(images_dir.glob("*.bmp")) + list(images_dir.glob("*.png"))
    )
    logger.info(f"Replicate scan: {len(all_files)} files in {images_dir}")

    stem_to_file: Dict[str, Path] = {f.stem: f for f in all_files}
    rep_keys = {stem: _strip_rep(stem) for stem in stem_to_file}

    groups: dict = defaultdict(list)
    for stem, key in rep_keys.items():
        groups[key].append(stem)

    rep_pairs: List[Tuple[str, str]] = []
    for _key, members in groups.items():
        if len(members) == 2:
            rep_pairs.append((members[0], members[1]))

    logger.info(f"Replicate scan: {len(rep_pairs)} complete pairs found")
    return rep_pairs, stem_to_file


def _load_images_for_stems(
    stems: List[str],
    stem_to_file: Dict[str, Path],
    conditional_variables: list,
    grayscale: bool = False,
    grayscale_background_normalize: bool = False,
    grayscale_bg_target: float = 0.5,
    grayscale_bg_border: int = 12,
) -> Tuple[pd.DataFrame, Optional[np.ndarray]]:
    """Load images from disk for the given stems only.

    Returns ``(pair_df, conditioning)`` where *pair_df* has columns
    ``id``, ``transformed_image``, ``rgb_image``, ``my_rep``,
    ``average_hue``, ``average_saturation``, ``average_value``.
    """
    from PIL import Image as PILImage

    from candescence.tlv.data.dataset import (
        _edge_band_mask,
        apply_grayscale_background_normalize,
    )

    rows = []
    cond_values = []
    for stem in stems:
        img_file = stem_to_file[stem]
        img = PILImage.open(img_file).convert('RGB')
        img = img.resize((128, 128))

        # HSV from edge pixels — always computed from color image
        # (before greyscale conversion) so hue/sat are meaningful
        img_hsv = img.convert('HSV')
        hsv_arr = np.array(img_hsv)
        mask = _edge_band_mask(128, 128, border=12)
        hue_vals = hsv_arr[..., 0][mask].astype(float)
        angles = hue_vals / 255.0 * 2 * np.pi
        mean_angle = np.arctan2(np.mean(np.sin(angles)), np.mean(np.cos(angles)))
        if mean_angle < 0:
            mean_angle += 2 * np.pi
        avg_hue = (mean_angle / (2 * np.pi)) * 255
        avg_sat = float(hsv_arr[..., 1][mask].mean())
        avg_val = float(hsv_arr[..., 2][mask].mean())

        # Match training: convert to greyscale if model was trained that way
        if grayscale:
            encoder_img = img.convert('L').convert('RGB')
            if grayscale_background_normalize:
                encoder_img = apply_grayscale_background_normalize(
                    encoder_img, 128,
                    target=grayscale_bg_target,
                    border=grayscale_bg_border,
                )
        else:
            encoder_img = img

        img_array = np.array(encoder_img).astype(np.float32) / 255.0
        tensor_chw = torch.tensor(np.transpose(img_array, (2, 0, 1)), dtype=torch.float32)

        rgb_array = np.array(img, dtype=np.uint8)

        rows.append({
            "id": stem,
            "my_rep": _strip_rep(stem),
            "transformed_image": tensor_chw,
            "rgb_image": rgb_array,
            "average_hue": avg_hue,
            "average_saturation": avg_sat,
            "average_value": avg_val,
        })

        if conditional_variables:
            cv = []
            for var in conditional_variables:
                if var == 'average_hue':
                    cv.append(avg_hue / 255.0)
                elif var == 'average_saturation':
                    cv.append(avg_sat / 255.0)
                elif var == 'average_value':
                    cv.append(avg_val / 255.0)
                else:
                    cv.append(0.5)
            cond_values.append(cv)

    pair_df = pd.DataFrame(rows)
    conditioning = np.array(cond_values, dtype=np.float32) if cond_values else None
    return pair_df, conditioning


def _build_replicate_df_from_disk(
    images_dir: Path,
    conditional_variables: list,
    grayscale: bool = False,
    grayscale_background_normalize: bool = False,
    grayscale_bg_target: float = 0.5,
    grayscale_bg_border: int = 12,
) -> Tuple[pd.DataFrame, np.ndarray, list]:
    """Scan *all* images on disk, find complete replicate pairs, load only those.

    Returns ``(pair_df, conditioning, rep_pairs)`` where *pair_df* has columns
    ``id``, ``transformed_image``, ``rgb_image``, ``my_rep`` and one row per
    image that belongs to a complete pair.
    """
    rep_pairs, stem_to_file = _scan_replicate_pairs_from_disk(images_dir)
    all_stems = [s for pair in rep_pairs for s in pair]
    pair_df, conditioning = _load_images_for_stems(
        all_stems, stem_to_file, conditional_variables,
        grayscale=grayscale,
        grayscale_background_normalize=grayscale_background_normalize,
        grayscale_bg_target=grayscale_bg_target,
        grayscale_bg_border=grayscale_bg_border,
    )
    return pair_df, conditioning, rep_pairs


def _render_tendril_replicate_diagnostics():
    """Render replicate & tendril layer diagnostics (ported from master)."""
    import matplotlib.pyplot as _mpl_plt

    from candescence.tlv.inference.tendril_replicate_diagnostics import (
        build_nonrep_pairs,
        compute_intermediate_distances_for_pairs,
        plot_intermediate_layer_outliers,
        plot_replicate_distance_distribution_per_layer,
    )

    with st.expander("Replicate & tendril layer diagnostics", expanded=False):
        model_wrapper = st.session_state.get("model") or st.session_state.get("model_wrapper")
        if model_wrapper is None:
            st.warning("No model loaded.")
            return

        images_path = st.session_state.get("images_path")
        if not images_path or not Path(images_path).is_dir():
            st.warning("Images directory not available.")
            return

        vae = model_wrapper._vae
        device = model_wrapper.device
        strategy = model_wrapper._strategy

        training_config = st.session_state.get("training_config") or {}
        conditional_variables = training_config.get("conditional_variables", [])
        is_grayscale = training_config.get("grayscale", False)
        gs_bg_norm = training_config.get("grayscale_background_normalize", False)
        gs_bg_target = training_config.get("grayscale_bg_target", 0.5)
        gs_bg_border = int(training_config.get("grayscale_bg_border", 12))

        # ── Controls ────────────────────────────────────────────
        c1, c2, c3, c4 = st.columns(4)
        with c1:
            pool_hw = st.slider(
                "Pool HW", min_value=2, max_value=16, value=8,
                key="trd_pool_hw",
                help="Adaptive average pool target size for skip features.",
            )
        with c2:
            n_nonrep = st.slider(
                "Random pairs", min_value=10, max_value=500, value=200,
                key="trd_n_nonrep",
            )
        with c3:
            n_outliers = st.slider(
                "Pairs per extreme", min_value=2, max_value=10, value=5,
                key="trd_n_outliers",
            )
        with c4:
            show_layers = st.multiselect(
                "Layers",
                ["x0", "x1", "x2", "x3"],
                default=["x0", "x1", "x2", "x3"],
                key="trd_outlier_layers",
            )

        st.caption(
            "Scans filenames to find replicate pairs, then samples down "
            "to match the requested pair count before loading images."
        )

        # ── Compute ────────────────────────────────────────────
        if st.button("Compute intermediate distances", key="trd_compute_dist"):
            # Phase 1: fast filename-only scan
            with st.spinner("Scanning disk for replicate pairs..."):
                all_rep_pairs, stem_to_file = _scan_replicate_pairs_from_disk(
                    Path(images_path),
                )
            n_total_pairs = len(all_rep_pairs)
            if n_total_pairs < 2:
                st.warning(f"Only {n_total_pairs} replicate pair(s) found on disk.")
                return

            # Sample replicate pairs to match requested count
            n_sample = min(n_nonrep, n_total_pairs)
            rng = np.random.default_rng(42)
            if n_sample < n_total_pairs:
                idx = rng.choice(n_total_pairs, size=n_sample, replace=False)
                sampled_rep_pairs = [all_rep_pairs[i] for i in idx]
            else:
                sampled_rep_pairs = list(all_rep_pairs)

            # Phase 2: load only images needed for sampled pairs
            stems_needed = sorted({s for pair in sampled_rep_pairs for s in pair})
            with st.spinner(f"Loading {len(stems_needed)} images ({n_sample} of {n_total_pairs} replicate pairs)..."):
                pair_df, pair_cond = _load_images_for_stems(
                    stems_needed, stem_to_file, conditional_variables,
                    grayscale=is_grayscale,
                    grayscale_background_normalize=gs_bg_norm,
                    grayscale_bg_target=gs_bg_target,
                    grayscale_bg_border=gs_bg_border,
                )

            st.info(
                f"Found **{n_total_pairs}** replicate pairs total; "
                f"sampled **{n_sample}** ({len(pair_df)} images loaded)."
            )

            with st.spinner(f"Computing distances for {n_sample} replicate pairs..."):
                rep_dist = compute_intermediate_distances_for_pairs(
                    pair_df, vae, sampled_rep_pairs,
                    device=device, pool_hw=pool_hw, strategy=strategy,
                    conditioning=pair_cond,
                )

            with st.spinner("Computing distances for random non-replicate pairs..."):
                nonrep_pairs = build_nonrep_pairs(
                    pair_df, n_pairs=n_sample,
                    rep_col="my_rep",
                )
                nonrep_dist = compute_intermediate_distances_for_pairs(
                    pair_df, vae, nonrep_pairs,
                    device=device, pool_hw=pool_hw, strategy=strategy,
                    conditioning=pair_cond,
                )

            st.session_state["trd_rep_dist"] = rep_dist
            st.session_state["trd_nonrep_dist"] = nonrep_dist
            st.session_state["trd_pair_df"] = pair_df

        rep_dist = st.session_state.get("trd_rep_dist")
        nonrep_dist = st.session_state.get("trd_nonrep_dist")
        pair_df = st.session_state.get("trd_pair_df")

        if rep_dist is None or nonrep_dist is None:
            return

        # ── Distance distributions ─────────────────────────────
        st.markdown("#### Distance distributions (replicate vs random)")
        hist_figs = plot_replicate_distance_distribution_per_layer(
            rep_dist, nonrep_dist,
        )
        for lname, fig in hist_figs.items():
            st.pyplot(fig)
            _mpl_plt.close(fig)

        if pair_df is None or "rgb_image" not in pair_df.columns or not show_layers:
            return

        # ── Replicate outlier pairs ────────────────────────────
        st.markdown("#### Replicate outlier pairs")
        rep_outlier_figs = plot_intermediate_layer_outliers(
            rep_dist, pair_df,
            layers=tuple(show_layers),
            n_outliers=n_outliers,
            label="Replicate",
        )
        for lname, (left_fig, right_fig) in rep_outlier_figs.items():
            st.pyplot(left_fig)
            _mpl_plt.close(left_fig)
            st.pyplot(right_fig)
            _mpl_plt.close(right_fig)

        # ── Random outlier pairs ───────────────────────────────
        st.markdown("#### Random outlier pairs")
        rand_outlier_figs = plot_intermediate_layer_outliers(
            nonrep_dist, pair_df,
            layers=tuple(show_layers),
            n_outliers=n_outliers,
            label="Random",
        )
        for lname, (left_fig, right_fig) in rand_outlier_figs.items():
            st.pyplot(left_fig)
            _mpl_plt.close(left_fig)
            st.pyplot(right_fig)
            _mpl_plt.close(right_fig)


def _render_manuscript_figure_export():
    """Export the two manuscript UMAP panels (hue + colony size) to disk.

    Mirrors the CLI in ``scripts/export_manuscript_umap_figures.py`` by calling
    the same shared renderer (``manuscript_figures.export_reference_umap_figures``)
    with session-state embeddings, UMAP coords, and metadata.
    """
    with st.expander("📄 Export manuscript UMAP figures (Fig 3A/3B)", expanded=False):
        st.caption(
            "Writes static PNGs colored by average hue (coolwarm) and estimated "
            "colony size (viridis), with a Mantel r/p annotation. Uses the current "
            "active latent space's UMAP coords."
        )

        embeddings = st.session_state.get('embeddings')
        coords_2d = st.session_state.get('coords_2d')
        images = st.session_state.get('images')
        metadata_df = st.session_state.get('metadata_df')
        model_path_str = st.session_state.get('model_path')

        if (
            embeddings is None or coords_2d is None
            or images is None or metadata_df is None
        ):
            st.info("Load a model + images and compute a UMAP projection first.")
            return

        if 'average_hue' not in metadata_df.columns:
            st.warning("`average_hue` not found in metadata — cannot render Fig 3A.")
            return

        default_out = ""
        if model_path_str:
            mp = Path(model_path_str)
            if mp.parent.name == "models":
                default_out = str(mp.parent.parent / "analyses")
            else:
                default_out = str(mp.parent / "analyses")

        output_dir_str = st.text_input(
            "Output directory",
            value=default_out,
            key="manuscript_export_output_dir",
            help="Defaults to <run-root>/analyses when the model was loaded from "
                 "a standard zoo layout.",
        )

        if not st.button("Export figures", key="manuscript_export_btn"):
            return

        if not output_dir_str.strip():
            st.error("Please provide an output directory.")
            return

        try:
            from candescence.tlv.analysis.latent_space_metrics import (
                mantel_latent_vs_numeric,
            )
            from candescence.tlv.analysis.manuscript_figures import (
                export_reference_umap_figures,
            )

            hue_vec = metadata_df['average_hue'].to_numpy(dtype=float)
            # Colony-size proportion: matches LearningDataset._get_size_proportion
            # (pixels > 0.5 on the encoder-input tensor, normalized 0-1).
            training_config = st.session_state.get('training_config') or {}
            thresh = float(training_config.get('size_threshold', 0.5))
            size_vec = (images > thresh).reshape(len(images), -1).mean(axis=1).astype(float)

            with st.spinner("Running Mantel tests and rendering figures..."):
                # Match figure3.py: Mantel is computed against the 2D UMAP
                # layout distances, not the full latent embedding distances.
                mantel_hue = mantel_latent_vs_numeric(
                    coords_2d, hue_vec, permutations=999, seed=42,
                )
                mantel_size = mantel_latent_vs_numeric(
                    coords_2d, size_vec, permutations=999, seed=42,
                )
                paths = export_reference_umap_figures(
                    coords_2d=coords_2d,
                    hue_values=hue_vec,
                    size_values=size_vec,
                    mantel_hue=mantel_hue,
                    mantel_size=mantel_size,
                    output_dir=Path(output_dir_str),
                )

            st.success(
                f"Saved:\n- {paths['hue']}\n- {paths['size']}\n\n"
                f"Mantel (hue): r={mantel_hue.statistic:.3f}, p={mantel_hue.p_value:.3f}  •  "
                f"Mantel (size): r={mantel_size.statistic:.3f}, p={mantel_size.p_value:.3f}"
            )
        except Exception as exc:
            st.error(f"Export failed: {exc}")
            logger.exception("Manuscript figure export failed")


def _render_sprite_map():
    """Render the sprite map / image mosaic section."""
    st.subheader("Sprite Map (Image Mosaic)")
    st.caption(
        "Arrange actual images on a grid matching their 2D embedding positions. "
        "Each cell shows the real colony image placed at its latent space location. "
        "Random sampling reflects class frequencies—use **Balance label groups** or "
        "**Only selected labels** (e.g. hyphae, pseudohyphae) below to emphasize "
        "filamentous morphologies."
    )

    images = st.session_state.images
    if images is None:
        st.warning("No images loaded. Load images to generate a sprite map.")
        return

    tendril_keys = st.session_state.get('tendril_keys', [])
    if tendril_keys:
        space_names = ['primary'] + list(tendril_keys)
        tab_labels = ["Primary (z)"] + [f"Tendril: {k}" for k in tendril_keys]
        tabs = st.tabs(tab_labels)
        for tab, space_name in zip(tabs, space_names):
            with tab:
                _render_sprite_map_for_space(
                    space_name=space_name,
                    coords_2d=st.session_state.all_coords_2d.get(space_name),
                    key_suffix=f"_{space_name}",
                )
    else:
        _render_sprite_map_for_space(
            space_name='primary',
            coords_2d=st.session_state.coords_2d,
            key_suffix="",
        )


def _render_sprite_map_for_space(
    space_name: str,
    coords_2d: Optional[np.ndarray],
    key_suffix: str,
):
    """Render sprite map controls and output for a single space."""
    if coords_2d is None:
        st.warning("Compute a projection first.")
        return

    n_samples = len(coords_2d)
    metadata_df = st.session_state.metadata_df
    emb_space = st.session_state.get("all_embeddings", {}).get(space_name)
    n_emb = len(emb_space) if emb_space is not None else n_samples
    # Stratified / filtered modes need one metadata row per latent row (same order as encode).
    meta_ok = (
        metadata_df is not None
        and not metadata_df.empty
        and len(metadata_df) == n_samples
        and n_samples == n_emb
    )

    # Controls
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        grid_size = st.slider(
            "Grid size",
            min_value=10, max_value=60, value=30,
            help="Number of cells per side. Larger = more space for images but sparser.",
            key=f"sprite_grid_size{key_suffix}"
        )
    with col2:
        cell_size = st.slider(
            "Cell size (px)",
            min_value=24, max_value=96, value=48, step=8,
            help="Pixel size of each image thumbnail.",
            key=f"sprite_cell_size{key_suffix}"
        )
    with col3:
        max_samples_val = st.slider(
            "Max samples",
            min_value=50, max_value=min(n_samples, 2000), value=min(n_samples, 500),
            help="Cap on how many images are placed (after sampling mode).",
            key=f"sprite_max_samples{key_suffix}"
        )
    with col4:
        cluster_labels = st.session_state.get('cluster_labels')
        show_borders = st.checkbox(
            "Cluster borders",
            value=False,
            disabled=(cluster_labels is None),
            help="Color image borders by cluster. Enable K-Means coloring first.",
            key=f"sprite_show_borders{key_suffix}"
        )
        morph_col_available = (
            meta_ok
            and metadata_df is not None
            and 'manual_formation' in metadata_df.columns
        )
        hd_tiebreak = st.checkbox(
            "High-dim collision tiebreak",
            value=True,
            help=(
                "When two images map to the same grid cell, keep the one whose "
                "full-dim latent is more coherent with already-placed neighbors "
                "(mean Euclidean distance to the 8-cell ring). Falls back to "
                "first-come-first-served when the neighborhood is still empty. "
                "Uncheck to restore the previous index-order behavior."
            ),
            key=f"sprite_hd_tiebreak{key_suffix}"
        )
        show_morph_borders = st.checkbox(
            "Morphology label borders",
            value=False,
            disabled=not morph_col_available,
            help=(
                "Frame each labelled image by its manual_formation value. "
                "Canonical morphology names (white/opaque/gray/shmoo/pseudohyphae/"
                "hyphae) use their fixed palette; any other labels get a stable "
                "fallback color. Unlabelled rows are left without a frame. "
                "Takes precedence over cluster borders."
            ),
            key=f"sprite_show_morph_borders{key_suffix}"
        )

    sampling_labels = [
        "Uniform (random)",
        "Balance label groups",
        "Only selected labels",
    ]
    sampling_mode_label = st.radio(
        "Image sampling",
        sampling_labels,
        index=0,
        horizontal=True,
        key=f"sprite_sampling_mode{key_suffix}",
        help="Uniform matches dataset frequencies (many round colonies). Balance gives each "
             "label a similar quota. Selected labels restricts to morphologies you pick.",
    )
    mode_internal = {
        "Uniform (random)": "uniform",
        "Balance label groups": "stratified",
        "Only selected labels": "filtered",
    }[sampling_mode_label]

    sprite_label_col: Optional[str] = None
    sprite_exclude_unlab = False
    sprite_filter: Optional[List[str]] = None

    if mode_internal != "uniform":
        if not meta_ok:
            if metadata_df is None or metadata_df.empty:
                st.warning(
                    "**No metadata loaded.** Balance / filtered sprite modes need a metadata "
                    "table with one row per image (same order as encoding). "
                    "Use **Uniform (random)**, or reload by picking a model with a merged "
                    "metadata file."
                )
            else:
                md_n = len(metadata_df)
                st.warning(
                    "**Label column is unavailable** until metadata rows match embeddings.\n\n"
                    f"- Metadata rows: **{md_n}**\n"
                    f"- Points in this projection: **{n_samples}**\n"
                    f"- Embedding rows (`{space_name}`): **{n_emb}**\n\n"
                    "These three counts must be **equal** (one row per image, same order). "
                    "Common causes: metadata file not merged, wrong file, or subset of images "
                    "encoded. Fix the merge in the loader, or use **Uniform (random)** sampling."
                )
        else:
            assert metadata_df is not None
            _ensure_derived_metadata_columns(metadata_df)
            cat_cols = _order_label_columns_for_ui(_metadata_label_columns(metadata_df))
            if not cat_cols:
                st.warning(_no_labels_message(metadata_df))
            else:
                default_ix = 0
                if 'manual_formation' in cat_cols:
                    default_ix = cat_cols.index('manual_formation')
                c1, c2 = st.columns(2)
                with c1:
                    sprite_label_col = st.selectbox(
                        "Label column",
                        cat_cols,
                        index=default_ix,
                        key=f"sprite_label_col{key_suffix}",
                    )
                with c2:
                    sprite_exclude_unlab = st.checkbox(
                        "Exclude unlabelled",
                        value=True,
                        key=f"sprite_exclude_unlab{key_suffix}",
                    )
                if mode_internal == "filtered":
                    uniq = sorted(
                        str(x) for x in metadata_df[sprite_label_col].dropna().unique()
                    )
                    hints = ('hyph', 'pseudo', 'filam', 'shmoo')
                    default_pick = [u for u in uniq if any(h in u.lower() for h in hints)]
                    if not default_pick:
                        default_pick = uniq[: min(3, len(uniq))]
                    sprite_filter = st.multiselect(
                        "Include only these labels",
                        options=uniq,
                        default=default_pick,
                        key=f"sprite_filter_labels{key_suffix}",
                    )

    total_px = grid_size * cell_size
    method_upper = st.session_state.reduction_method.upper()
    if st.session_state.reduction_method == 'pca':
        pc_x = st.session_state.pca_x_component
        pc_y = st.session_state.pca_y_component
        proj_label = f"PCA: PC{pc_x+1} vs PC{pc_y+1}"
    else:
        proj_label = method_upper
    sample_caption = min(max_samples_val, n_samples)
    if mode_internal == "filtered" and meta_ok and sprite_label_col and sprite_filter:
        lab_ser = _normalize_sprite_labels(
            metadata_df[sprite_label_col].values
        )
        n_pool = int(np.sum(np.isin(lab_ser, sprite_filter)))
        sample_caption = min(max_samples_val, n_pool) if n_pool else 0
    st.caption(
        f"Output: {grid_size}x{grid_size} grid = {total_px}x{total_px}px image | "
        f"Up to {sample_caption} images from {n_samples} rows | "
        f"Projection: {proj_label}"
    )

    result_key = f"sprite_map_image{key_suffix}"

    morph_labels_arr: Optional[np.ndarray] = None
    morph_color_map: Optional[Dict[str, np.ndarray]] = None
    if show_morph_borders and morph_col_available:
        assert metadata_df is not None
        morph_labels_arr = metadata_df['manual_formation'].values
        morph_color_map = _build_label_color_map(morph_labels_arr)
        n_labelled = int(sum(
            1 for v in morph_labels_arr if _normalize_label(v) in morph_color_map
        ))
        # Render a compact legend (swatch + count) for the labels that will get borders.
        counts: Dict[str, int] = {}
        for v in morph_labels_arr:
            n = _normalize_label(v)
            if n is not None and n in morph_color_map:
                counts[n] = counts.get(n, 0) + 1
        legend_chunks = [
            f'<span style="display:inline-block;width:10px;height:10px;'
            f'background:rgb({r},{g},{b});border:1px solid #444;'
            f'margin-right:4px;vertical-align:middle;"></span>{name} ({counts[name]})'
            for name, (r, g, b) in (
                (nm, tuple(int(c) for c in morph_color_map[nm]))
                for nm in sorted(counts, key=lambda k: -counts[k])
            )
        ]
        legend_html = (
            f"<div style='font-size:0.85em;'>Morphology border legend — "
            f"{n_labelled}/{len(morph_labels_arr)} labelled rows: "
            + " &nbsp; ".join(legend_chunks)
            + "</div>"
        )
        st.markdown(legend_html, unsafe_allow_html=True)

    if st.button("Generate Sprite Map", type="primary", key=f"gen_sprite_btn{key_suffix}"):
        with st.spinner("Generating sprite map..."):
            rng = np.random.default_rng(42)
            try:
                if mode_internal == "uniform":
                    sprite_img = _generate_sprite_map(
                        grid_size=grid_size,
                        cell_size=cell_size,
                        max_samples=max_samples_val if max_samples_val < n_samples else None,
                        show_borders=show_borders,
                        coords_2d_override=coords_2d,
                        sample_indices=None,
                        morph_labels=morph_labels_arr,
                        morph_color_map=morph_color_map,
                        hd_tiebreak=hd_tiebreak,
                    )
                else:
                    if not meta_ok or sprite_label_col is None:
                        st.error(
                            "Label-based sampling requires metadata with one row per image."
                        )
                        sprite_img = None
                    else:
                        assert metadata_df is not None
                        labels_arr = metadata_df[sprite_label_col].values
                        pick = _sprite_map_pick_indices(
                            n_total=n_samples,
                            max_samples=max_samples_val,
                            labels=labels_arr,
                            mode=mode_internal,
                            exclude_unlabelled=sprite_exclude_unlab,
                            filter_labels=sprite_filter,
                            rng=rng,
                        )
                        sprite_img = _generate_sprite_map(
                            grid_size=grid_size,
                            cell_size=cell_size,
                            max_samples=None,
                            show_borders=show_borders,
                            coords_2d_override=coords_2d,
                            sample_indices=pick,
                            morph_labels=morph_labels_arr,
                        morph_color_map=morph_color_map,
                        hd_tiebreak=hd_tiebreak,
                        )
                if sprite_img is not None:
                    st.session_state[result_key] = sprite_img
                    st.rerun()
            except ValueError as e:
                st.error(str(e))

    # Display result
    sprite_img = st.session_state.get(result_key)
    if sprite_img is not None:
        if st.session_state.reduction_method == 'pca':
            _pc_x = st.session_state.pca_x_component
            _pc_y = st.session_state.pca_y_component
            _method_caption = f"PCA: PC{_pc_x+1} vs PC{_pc_y+1}"
        else:
            _method_caption = st.session_state.reduction_method.upper()
        st.image(
            sprite_img,
            caption=f"Sprite Map ({_method_caption}) — {sprite_img.shape[1]}x{sprite_img.shape[0]}px",
            use_container_width=True
        )

        # Download as PNG
        import io
        from PIL import Image as _PILImage
        buf = io.BytesIO()
        _PILImage.fromarray(sprite_img).save(buf, format="PNG")
        st.download_button(
            label="Download as PNG",
            data=buf.getvalue(),
            file_name=f"sprite_map_{_method_caption.replace(' ', '_').replace(':', '')}.png",
            mime="image/png",
            key=f"download_sprite_btn{key_suffix}",
        )

        if st.button("Clear Sprite Map", key=f"clear_sprite_btn{key_suffix}"):
            st.session_state[result_key] = None
            st.rerun()


############################################################################


def _image_array_for_display(idx: int, images: Optional[np.ndarray] = None) -> np.ndarray:
    """Return a display-ready uint8 RGB array for sample ``idx``.

    Prefers the original RGB pixels stored on ``metadata_df['rgb_image']``
    (captured before greyscale conversion) so the UI shows camera color even
    when the encoder was trained on greyscale. Falls back to the encoder
    tensor in ``st.session_state.images`` (greyscale-equivalent) when the
    RGB column is unavailable.
    """
    metadata_df = st.session_state.get('metadata_df', None)
    if images is None:
        images = st.session_state.get('images', None)

    if (
        metadata_df is not None
        and 'rgb_image' in metadata_df.columns
        and images is not None
        and len(metadata_df) == len(images)
        and 0 <= idx < len(metadata_df)
    ):
        rgb = metadata_df['rgb_image'].iloc[idx]
        if isinstance(rgb, np.ndarray):
            return _prepare_image(rgb)

    if images is None:
        raise IndexError("No images available for display")
    return _prepare_image(images[int(idx)])


def _prepare_image(image: np.ndarray) -> np.ndarray:
    """Prepare image for display."""
    # Handle tensor format (B, C, H, W) or (C, H, W)
    if len(image.shape) == 4:
        image = image[0]

    if len(image.shape) == 3 and image.shape[0] in [1, 3]:
        # Channel-first to channel-last
        image = np.transpose(image, (1, 2, 0))

    # Handle single channel
    if len(image.shape) == 2:
        image = np.stack([image] * 3, axis=-1)
    elif image.shape[-1] == 1:
        image = np.repeat(image, 3, axis=-1)

    # Normalize to 0-255
    if image.dtype in [np.float32, np.float64]:
        if image.max() <= 1.0:
            image = (image * 255).astype(np.uint8)
        else:
            image = np.clip(image, 0, 255).astype(np.uint8)
    elif image.dtype != np.uint8:
        image = image.astype(np.uint8)

    return image


if __name__ == "__main__":
    main()
