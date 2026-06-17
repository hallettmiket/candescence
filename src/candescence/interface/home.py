"""
Purpose: Home dashboard content for Candescence unified interface
Author: Hallett Lab
Date: 2026-02-04

Importable module containing the home page rendering logic.
Used by both app.py (main entry point) and pages/00_Home.py (sidebar nav).
"""

from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

import streamlit as st

from candescence.interface.core.theme import apply_theme, status_badge
from candescence.core.model_zoo import ModelZoo, ZooEntry
from candescence.core.dataset_zoo import DatasetZoo, DatasetEntry
from candescence.core.projects import all_projects, project_color

import base64

LOGO_PATH = Path(__file__).parent.parent.parent.parent / "assets" / "candescence-logo.png"
LAB_LOGO_PATH = Path(__file__).parent.parent.parent.parent / "assets" / "hallett-lab-logo.svg"
LAB_URL = "https://www.mikehallett.science/"
GITHUB_URL = "https://github.com/hallettmiket/candescence"


def _encode_svg(svg_path: Path) -> str:
    """Read an SVG file and return its base64 encoding for inline HTML."""
    return base64.b64encode(svg_path.read_bytes()).decode("utf-8")


def render_home_page() -> None:
    """Render the home dashboard page."""
    apply_theme()

    # Header with logo
    col_logo, col_title = st.columns([2, 3])

    with col_logo:
        if LOGO_PATH.exists():
            st.image(str(LOGO_PATH), width=450)

    with col_title:
        st.markdown(
            """
            <div style="padding: 1.5rem 0 0 0;">
                <h1 style="margin: 0; font-size: 2.5rem;">Candescence</h1>
                <p style="color: #666; margin-top: 0.25rem; font-size: 1.1rem;">
                    Unified interface for <em>Candida albicans</em> morphology analysis
                </p>
            </div>
            """,
            unsafe_allow_html=True,
        )

    # Lab + GitHub links
    link_col1, link_col2, link_col3 = st.columns([4, 2, 2])

    with link_col1:
        if LAB_LOGO_PATH.exists():
            st.markdown(
                f'<div style="display: flex; align-items: center; gap: 1.5rem;">'
                f'<a href="{LAB_URL}" target="_blank">'
                f'<img src="data:image/svg+xml;base64,'
                f'{_encode_svg(LAB_LOGO_PATH)}" '
                f'style="width: 100%; max-width: 400px;" alt="Hallett Lab" />'
                f'</a>'
                f'<a href="{LAB_URL}" target="_blank" '
                f'style="text-decoration: none; color: #222 !important; font-size: 1.2rem; font-weight: 600;">'
                f'From the Hallett Lab'
                f'</a>'
                f'</div>',
                unsafe_allow_html=True,
            )
        else:
            st.markdown(f"[From the Hallett Lab]({LAB_URL})")

    with link_col2:
        st.markdown(
            f'<div style="padding-top: 0.5rem;">'
            f'<a href="{GITHUB_URL}" target="_blank" '
            f'style="text-decoration: none; color: #222 !important; font-size: 1.2rem; font-weight: 600;">'
            f'Candescence @ GitHub'
            f'</a></div>',
            unsafe_allow_html=True,
        )

    st.divider()

    # Project launcher (driven by the central project registry)
    _render_projects_launcher()

    st.divider()

    # Model and Dataset overview
    col_models, col_datasets = st.columns(2)

    with col_models:
        _render_models_overview()

    with col_datasets:
        _render_datasets_overview()

    st.divider()

    # Recent activity
    _render_recent_activity()


def _render_projects_launcher() -> None:
    """Render a launcher with one column per registered project.

    Driven entirely by ``candescence.core.projects`` — adding a project (or a
    page to one) is a registry edit, not a change here.
    """
    st.subheader("Projects")

    projects = all_projects()
    cols = st.columns(len(projects))

    for col, project in zip(cols, projects):
        with col:
            status_note = "" if project.status == "active" else (
                '<div style="color: #999; font-size: 0.8rem; margin-top: 0.5rem;">'
                "Integration in progress</div>"
            )
            st.markdown(
                f"""
                <div style="
                    background-color: {project.color}15;
                    border-radius: 8px;
                    padding: 0.75rem 1rem;
                    border-left: 4px solid {project.color};
                    margin-bottom: 0.75rem;
                ">
                    <span style="color: {project.color}; font-weight: 700;
                        text-transform: uppercase; font-size: 0.75rem;">
                        {project.kind}</span>
                    <h3 style="margin: 0.15rem 0 0 0;">{project.label}</h3>
                    <div style="color: #666; font-size: 0.85rem;">{project.full_name}</div>
                    <p style="color: #666; font-size: 0.85rem; margin: 0.5rem 0 0 0;">
                        {project.description}</p>
                    {status_note}
                </div>
                """,
                unsafe_allow_html=True,
            )

            for page in project.pages:
                st.markdown(
                    f"""
                    <div style="padding: 0.35rem 0; border-bottom: 1px solid #eee;">
                        <span style="font-weight: 600;">{page.icon} {page.title}</span><br/>
                        <span style="color: #888; font-size: 0.8rem;">{page.description}</span>
                    </div>
                    """,
                    unsafe_allow_html=True,
                )

            if not project.pages:
                st.caption("No pages yet.")


def _get_project_color(project: str) -> str:
    """Get theme color for a project (delegates to the project registry)."""
    return project_color(project)


def _render_models_overview() -> None:
    """Render overview of registered models from the model zoo."""
    st.subheader("Model Zoo")

    try:
        zoo = ModelZoo()
        summary = zoo.get_summary()

        if not summary:
            st.info("No models registered yet. Train a model to get started!")
            return

        for project, counts in sorted(summary.items()):
            color = _get_project_color(project)

            col1, col2, col3 = st.columns([2, 1, 1])

            with col1:
                st.markdown(
                    f"""
                    <span style="
                        background-color: {color}20;
                        color: {color};
                        padding: 2px 8px;
                        border-radius: 4px;
                        font-weight: 500;
                    ">{project.upper()}</span>
                    """,
                    unsafe_allow_html=True,
                )

            with col2:
                st.metric("Research", counts.get("research", 0))

            with col3:
                st.metric("Production", counts.get("production", 0))

        # Show all models, grouped by project (type) then name.
        all_models = sorted(
            zoo.list_models(), key=lambda e: (e.project.lower(), e.name.lower())
        )
        if all_models:
            with st.expander("All Models", expanded=False):
                for entry in all_models:
                    _render_model_row(entry)

    except Exception as e:
        st.error(f"Error loading model zoo: {e}")


def _render_model_row(entry: ZooEntry) -> None:
    """Render a single model row."""
    color = _get_project_color(entry.project)

    col1, col2, col3 = st.columns([3, 1, 1])

    with col1:
        st.markdown(f"**{entry.name}** (v{entry.version})")

    with col2:
        st.markdown(
            f"""<span style="color: {color}; font-size: 0.8rem;">
            {entry.project.upper()}</span>""",
            unsafe_allow_html=True,
        )

    with col3:
        st.markdown(status_badge(entry.model_type), unsafe_allow_html=True)


def _render_datasets_overview() -> None:
    """Render overview of registered datasets from the dataset zoo."""
    st.subheader("Datasets")

    try:
        zoo = DatasetZoo()
        summary = zoo.get_summary()

        if not summary:
            st.info("No datasets registered yet.")
            return

        for project, count in sorted(summary.items()):
            color = _get_project_color(project)

            # Get total samples for this project
            project_datasets = zoo.list_datasets(project=project)
            total_samples = sum(ds.num_samples for ds in project_datasets)

            col1, col2, col3 = st.columns([2, 1, 1])

            with col1:
                st.markdown(
                    f"""
                    <span style="
                        background-color: {color}20;
                        color: {color};
                        padding: 2px 8px;
                        border-radius: 4px;
                        font-weight: 500;
                    ">{project.upper()}</span>
                    """,
                    unsafe_allow_html=True,
                )

            with col2:
                st.metric("Datasets", count)

            with col3:
                st.metric("Samples", f"{total_samples:,}")

        # Show all datasets, grouped by project (type) then name.
        all_datasets = sorted(
            zoo.list_datasets(), key=lambda d: (d.project.lower(), d.name.lower())
        )
        if all_datasets:
            with st.expander("All Datasets", expanded=False):
                for ds in all_datasets:
                    _render_dataset_row(ds)

    except Exception as e:
        st.error(f"Error loading dataset zoo: {e}")


def _render_dataset_row(ds: DatasetEntry) -> None:
    """Render a single dataset row."""
    color = _get_project_color(ds.project)

    col1, col2, col3 = st.columns([3, 1, 1])

    with col1:
        st.markdown(f"**{ds.name}**")

    with col2:
        st.markdown(
            f"""<span style="color: {color}; font-size: 0.8rem;">
            {ds.project.upper()}</span>""",
            unsafe_allow_html=True,
        )

    with col3:
        st.caption(f"{ds.num_samples:,} samples")


def _render_recent_activity() -> None:
    """Render recent activity section."""
    st.subheader("Recent Activity")

    try:
        model_zoo = ModelZoo()
        dataset_zoo = DatasetZoo()

        # Combine recent models and datasets
        activities = []

        for entry in model_zoo.list_models()[:5]:
            activities.append({
                "type": "model",
                "name": entry.name,
                "project": entry.project,
                "action": f"registered as {entry.model_type}",
                "timestamp": entry.created_at,
            })

        for ds in dataset_zoo.list_datasets()[:5]:
            activities.append({
                "type": "dataset",
                "name": ds.name,
                "project": ds.project,
                "action": f"registered ({ds.num_samples:,} samples)",
                "timestamp": ds.created_at,
            })

        if not activities:
            st.info("No recent activity. Start by training a model or registering a dataset!")
            return

        # Sort by timestamp
        activities.sort(key=lambda x: x["timestamp"], reverse=True)

        for activity in activities[:10]:
            icon = "model" if activity["type"] == "model" else "dataset"
            color = _get_project_color(activity["project"])

            # Format timestamp
            try:
                ts = datetime.fromisoformat(activity["timestamp"])
                time_str = ts.strftime("%Y-%m-%d %H:%M")
            except Exception:
                time_str = activity["timestamp"][:16]

            st.markdown(
                f"""
                <div style="
                    padding: 0.5rem 0;
                    border-bottom: 1px solid #eee;
                ">
                    <strong>{activity['name']}</strong>
                    <span style="color: {color}; font-size: 0.8rem;">
                        [{activity['project'].upper()}]
                    </span>
                    {activity['action']}
                    <span style="color: #999; font-size: 0.8rem; float: right;">
                        {time_str}
                    </span>
                </div>
                """,
                unsafe_allow_html=True,
            )

    except Exception as e:
        st.warning(f"Could not load recent activity: {e}")
