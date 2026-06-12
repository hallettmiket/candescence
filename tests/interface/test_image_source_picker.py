"""
Purpose: Render-path test for the directory-or-upload image source picker.
Author: Hallett Lab
Date: 2026-06-12
Input: render_image_source_picker, exercised under the Streamlit runtime.
Output: Pytest assertions.

No files are uploaded in these tests, so (with the lazy-create staging dir) they
have no filesystem side effects.
"""

from __future__ import annotations

import json

import pytest

pytest.importorskip("streamlit")
from streamlit.testing.v1 import AppTest  # noqa: E402

from candescence.core.settings import get_settings  # noqa: E402

_SCRIPT = """
import streamlit as st
from candescence.interface.core.components import render_image_source_picker
path = render_image_source_picker(
    key_prefix="t", default_dir="/some/default/dir", label="Imgs"
)
st.json({"path": path})
"""


def _path(at) -> str:
    value = at.json[0].value
    if isinstance(value, str):
        value = json.loads(value)
    return value["path"]


def test_directory_mode_returns_default() -> None:
    at = AppTest.from_string(_SCRIPT).run()
    assert not at.exception
    assert at.radio[0].options == ["Directory path", "Upload images"]
    assert _path(at) == "/some/default/dir"


def test_upload_mode_stages_under_refined_not_raw() -> None:
    at = AppTest.from_string(_SCRIPT).run()
    at.radio[0].set_value("Upload images").run()
    assert not at.exception
    staged = _path(at)
    refined = str(get_settings().refined_path)
    assert staged.startswith(refined)
    assert "_uploads" in staged
    assert "/raw/" not in staged
