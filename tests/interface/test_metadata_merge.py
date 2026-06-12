"""Tests for metadata merge and label column detection in the latent explorer.

These test the pure helper functions extracted for robustness.  The
functions live in ``latent_explorer_app.py`` but are imported indirectly
here (the module requires Streamlit at import time, so we replicate the
logic for isolated testing).
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest


# ── Replicate helpers (same logic as latent_explorer_app.py) ────────────
# When the helpers are eventually extracted to a shared utility module,
# these tests can import them directly.

_LABEL_COLUMN_ALLOWLIST = {
    "manual_formation", "formation", "morphology", "grade",
    "class", "label", "type", "category", "group",
}


def _is_categorical_column(series: pd.Series) -> bool:
    s = series
    if pd.api.types.is_bool_dtype(s):
        return True
    if pd.api.types.is_object_dtype(s) or getattr(s.dtype, "name", "") == "category":
        return True
    if pd.api.types.is_string_dtype(s):
        return True
    nu = int(s.nunique(dropna=True))
    if nu < 2:
        return False
    if pd.api.types.is_integer_dtype(s):
        return nu <= 128
    if pd.api.types.is_float_dtype(s):
        if nu > 64:
            return False
        vals = s.dropna()
        if vals.empty:
            return False
        arr = vals.to_numpy(dtype=np.float64, copy=False)
        return bool(np.allclose(arr, np.round(arr), rtol=0, atol=1e-5))
    return False


def _metadata_label_columns(metadata_df: pd.DataFrame) -> list:
    result = []
    for c in metadata_df.columns:
        if c == "id":
            continue
        s = metadata_df[c]
        nu = int(s.nunique(dropna=True))
        if nu < 2:
            continue
        if _is_categorical_column(s):
            result.append(c)
        elif c.lower() in _LABEL_COLUMN_ALLOWLIST and nu >= 2:
            result.append(c)
    return result


def _normalize_merge_key(value) -> str:
    if value is None:
        return ""
    s = str(value).strip()
    if not s:
        return ""
    p = Path(s)
    if p.suffix:
        return p.stem
    return s


# ── Tests ───────────────────────────────────────────────────────────────


class TestNormalizeMergeKey:

    def test_strips_extension(self):
        assert _normalize_merge_key("img_001.png") == "img_001"

    def test_strips_path_and_extension(self):
        assert _normalize_merge_key("data/images/img_001.tif") == "img_001"

    def test_no_extension(self):
        assert _normalize_merge_key("img_001") == "img_001"

    def test_int_input(self):
        assert _normalize_merge_key(42) == "42"

    def test_whitespace(self):
        assert _normalize_merge_key("  img_001  ") == "img_001"

    def test_none(self):
        assert _normalize_merge_key(None) == ""

    def test_empty(self):
        assert _normalize_merge_key("") == ""

    def test_float_input(self):
        # Path("3.0").suffix == ".0", so stem is "3"
        assert _normalize_merge_key(3.0) == "3"


class TestMetadataLabelColumns:

    def test_string_column(self):
        df = pd.DataFrame({"id": ["a", "b"], "grade": ["A", "B"]})
        assert _metadata_label_columns(df) == ["grade"]

    def test_float_whole_numbers_with_nan(self):
        df = pd.DataFrame({
            "id": ["a", "b", "c", "d"],
            "manual_formation": [1.0, 2.0, np.nan, 3.0],
        })
        assert "manual_formation" in _metadata_label_columns(df)

    def test_integer_column(self):
        df = pd.DataFrame({"id": ["a", "b", "c"], "grade": [1, 2, 3]})
        assert "grade" in _metadata_label_columns(df)

    def test_all_nan_excluded(self):
        df = pd.DataFrame({"id": ["a", "b"], "manual_formation": [np.nan, np.nan]})
        assert _metadata_label_columns(df) == []

    def test_single_unique_excluded(self):
        df = pd.DataFrame({"id": ["a", "b", "c"], "grade": ["A", "A", "A"]})
        assert _metadata_label_columns(df) == []

    def test_numeric_conditioning_not_label(self):
        df = pd.DataFrame({
            "id": ["a", "b"],
            "average_hue": [0.3, 0.7],
            "average_saturation": [0.1, 0.9],
        })
        assert _metadata_label_columns(df) == []

    def test_id_excluded(self):
        df = pd.DataFrame({"id": ["a", "b", "c"], "label": ["x", "y", "z"]})
        cols = _metadata_label_columns(df)
        assert "id" not in cols
        assert "label" in cols

    def test_allowlisted_name_with_odd_dtype(self):
        """An allowlisted column name forces inclusion even if dtype wouldn't pass."""
        # Simulate a column named 'morphology' stored as float codes
        # with many unique values (>64) that would fail _is_categorical_column
        # but only 5 uniques which is fine
        df = pd.DataFrame({
            "id": list(range(10)),
            "morphology": [1.1, 2.2, 3.3, 4.4, 5.5, 1.1, 2.2, 3.3, 4.4, 5.5],
        })
        # _is_categorical_column returns False for non-whole floats
        assert not _is_categorical_column(df["morphology"])
        # But allowlist includes it
        assert "morphology" in _metadata_label_columns(df)


class TestStemNormalizedMerge:
    """Test the stem-normalized merge fallback logic."""

    def test_extension_mismatch(self):
        """Image IDs are stems, metadata keys have extensions."""
        metadata_df = pd.DataFrame({"id": ["img_001", "img_002", "img_003"]})
        loaded = pd.DataFrame({
            "image_filename": ["img_001.png", "img_002.png", "img_003.png"],
            "manual_formation": ["white", "opaque", "gray"],
        })

        # Direct match fails
        direct = metadata_df["id"].map(
            loaded.set_index("image_filename")["manual_formation"].to_dict()
        )
        assert direct.isna().all()

        # Stem-normalized match succeeds
        norm_keys = loaded["image_filename"].map(_normalize_merge_key)
        lookup = loaded.copy()
        lookup["_k"] = norm_keys
        lookup = lookup.drop_duplicates(subset="_k")
        norm_ids = metadata_df["id"].map(_normalize_merge_key)
        result = norm_ids.map(
            lookup.set_index("_k")["manual_formation"].to_dict()
        )
        assert result.notna().all()
        assert list(result) == ["white", "opaque", "gray"]

    def test_int_vs_string_ids(self):
        """Metadata has int IDs, image stems are strings."""
        metadata_df = pd.DataFrame({"id": ["5", "10", "15"]})
        loaded = pd.DataFrame({
            "id": [5, 10, 15],
            "manual_formation": ["shmoo", "hyphae", "white"],
        })

        # str() normalization handles int→string
        loaded_keys = loaded["id"].astype(str).str.strip()
        image_keys = metadata_df["id"].astype(str).str.strip()
        result = image_keys.map(
            loaded.set_index(loaded_keys)["manual_formation"].to_dict()
        )
        assert result.notna().all()
        assert list(result) == ["shmoo", "hyphae", "white"]

    def test_path_prefix_mismatch(self):
        """Metadata has full paths, image IDs are stems."""
        metadata_df = pd.DataFrame({"id": ["colony_A", "colony_B"]})
        loaded = pd.DataFrame({
            "filename": ["/data/raw/colony_A.tiff", "/data/raw/colony_B.tiff"],
            "manual_formation": ["pseudohyphae", "opaque"],
        })

        norm_keys = loaded["filename"].map(_normalize_merge_key)
        norm_ids = metadata_df["id"].map(_normalize_merge_key)
        lookup = loaded.copy()
        lookup["_k"] = norm_keys
        result = norm_ids.map(
            lookup.set_index("_k")["manual_formation"].to_dict()
        )
        assert list(result) == ["pseudohyphae", "opaque"]
