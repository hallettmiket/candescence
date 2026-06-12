"""
Purpose: Model Zoo Page for Candescence unified interface
Author: Hallett Lab
Date: 2026-02-05

View, manage, and promote registered models across all projects
(TLV, Varasana, Grace) using the unified model zoo.
"""

from datetime import datetime
from pathlib import Path
from typing import List, Optional

import pandas as pd
import streamlit as st

from candescence.interface.core.theme import (
    THEME,
    apply_theme,
    page_header,
    status_badge,
    get_subproject_color,
)
from candescence.core.model_zoo import ModelZoo, ZooEntry
from candescence.core.model_catalog import is_public_architecture
from candescence.interface.core.components import (
    render_research_mode_toggle,
    research_mode_enabled,
)

# Page configuration
st.set_page_config(
    page_title="Model Registry | Candescence",
    page_icon="📦",
    layout="wide",
    initial_sidebar_state="expanded"
)


def main() -> None:
    """Main application entry point."""
    apply_theme()

    page_header(
        title="Model Registry",
        icon="📦",
        description="View and manage trained models across all projects"
    )

    # Load zoo
    try:
        zoo = ModelZoo()
    except Exception as e:
        st.error(f"Failed to load model zoo: {e}")
        return

    # Sidebar filters
    with st.sidebar:
        st.subheader("Filters")

        # Curate by architecture tier: end users see only public models
        # (Strategy 0/1/14); research mode reveals every architecture.
        render_research_mode_toggle()

        # Build project options from actual data
        projects = ["All"] + [p.upper() for p in zoo.get_projects()]
        project_filter = st.selectbox(
            "Project",
            options=projects,
            index=0,
        )

        type_filter = st.selectbox(
            "Model Type",
            options=["All", "Research", "Production"],
            index=0,
        )

        st.divider()

        # Summary stats
        st.subheader("Summary")
        summary = zoo.get_summary()

        for project, counts in sorted(summary.items()):
            color = _get_project_color(project)
            st.markdown(
                f"**{project.upper()}**: "
                f"{counts.get('research', 0)} research, "
                f"{counts.get('production', 0)} production"
            )

        st.divider()
        st.caption(f"Total: {len(zoo)} models in zoo")

    # Apply filters
    project = None if project_filter == "All" else project_filter.lower()
    model_type = None if type_filter == "All" else type_filter.lower()

    models = zoo.list_models(project=project, model_type=model_type)

    # Hide research-tier architectures unless research mode is enabled. Non-TLV
    # models (e.g. Varasana detection) have no VAE architecture tier, so they
    # are always shown.
    if not research_mode_enabled():
        models = [
            m
            for m in models
            if m.project != "tlv" or is_public_architecture(m.architecture)
        ]

    # Main content
    if not models:
        st.info("No models found matching the filters.")
        st.markdown(
            """
            ### Getting Started

            Train a model using one of the training apps:
            - **TLV Training**: Train Tendril VAE models for latent space analysis
            - **Varasana Training**: Train FCOS object detection models

            Once trained, models will appear here for management.

            You can also register existing models via:
            ```python
            from candescence.core.model_zoo import ModelZoo
            zoo = ModelZoo()
            zoo.register(model_id="my_model", ...)
            ```
            """
        )
        return

    st.markdown(f"**{len(models)} models found**")

    # Comparison mode toggle
    compare_mode = st.checkbox("Enable model comparison", key="compare_mode")

    if compare_mode:
        _render_model_comparison(models, zoo)
        st.divider()

    # Model list
    for entry in models:
        _render_model_card(entry, zoo)


def _get_project_color(project: str) -> str:
    """Get theme color for a project."""
    color_map = {
        "tlv": THEME["tlv_color"],
        "varasana": THEME["varasana_color"],
        "grace": THEME.get("grace_color", "#1ABC9C"),
    }
    return color_map.get(project, "#666666")


def _render_model_comparison(models: List[ZooEntry], zoo: ModelZoo) -> None:
    """Render side-by-side model comparison table."""
    st.subheader("Model Comparison")

    if len(models) < 2:
        st.info("Need at least 2 models to compare.")
        return

    # Multi-select for models to compare
    model_options = {m.id: f"{m.name} ({m.project}/{m.model_type})" for m in models}
    selected_ids = st.multiselect(
        "Select models to compare",
        options=list(model_options.keys()),
        format_func=lambda x: model_options[x],
        default=list(model_options.keys())[:min(3, len(model_options))],
        key="compare_models",
    )

    if len(selected_ids) < 2:
        st.info("Select at least 2 models to compare.")
        return

    selected_models = [zoo.get(mid) for mid in selected_ids if zoo.get(mid)]

    # Build comparison table
    rows = []

    # Basic info rows
    rows.append({"Attribute": "Project", **{m.name: m.project.upper() for m in selected_models}})
    rows.append({"Attribute": "Type", **{m.name: m.model_type for m in selected_models}})
    rows.append({"Attribute": "Version", **{m.name: m.version for m in selected_models}})
    rows.append({"Attribute": "Architecture", **{m.name: m.architecture for m in selected_models}})
    rows.append({
        "Attribute": "Checkpoint Exists",
        **{m.name: ("Yes" if m.exists() else "No") for m in selected_models},
    })

    # Collect all metric keys across selected models
    all_metric_keys = set()
    for m in selected_models:
        all_metric_keys.update(m.metrics.keys())

    # Metric rows
    for key in sorted(all_metric_keys):
        row = {"Attribute": f"Metric: {key}"}
        for m in selected_models:
            val = m.metrics.get(key, "—")
            if isinstance(val, float):
                val = f"{val:.4f}"
            row[m.name] = val
        rows.append(row)

    # Training config comparison
    all_config_keys = set()
    for m in selected_models:
        all_config_keys.update(m.training_config.keys())

    for key in sorted(all_config_keys):
        row = {"Attribute": f"Config: {key}"}
        for m in selected_models:
            val = m.training_config.get(key, "—")
            if isinstance(val, list):
                val = ", ".join(str(v) for v in val)
            row[m.name] = str(val)
        rows.append(row)

    df = pd.DataFrame(rows)
    st.dataframe(df, hide_index=True, use_container_width=True)


def _render_model_card(entry: ZooEntry, zoo: ModelZoo) -> None:
    """Render a model card with details and actions."""
    color = _get_project_color(entry.project)

    with st.container():
        # Header row
        col1, col2, col3, col4 = st.columns([3, 1, 1, 1])

        with col1:
            st.markdown(f"### {entry.name}")
            st.caption(
                f"ID: {entry.id} | Version: {entry.version} | "
                f"Architecture: {entry.architecture}"
            )

        with col2:
            st.markdown(
                f"""<span style="
                    background-color: {color}20;
                    color: {color};
                    padding: 4px 12px;
                    border-radius: 4px;
                    font-weight: 500;
                ">{entry.project.upper()}</span>""",
                unsafe_allow_html=True,
            )

        with col3:
            st.markdown(status_badge(entry.model_type), unsafe_allow_html=True)

        with col4:
            try:
                created = datetime.fromisoformat(entry.created_at)
                date_str = created.strftime("%Y-%m-%d")
            except Exception:
                date_str = entry.created_at[:10]
            st.caption(f"Created: {date_str}")

        # File status
        if entry.exists():
            st.success(f"Checkpoint: `{entry.get_checkpoint_path()}`", icon="✅")
        else:
            st.warning(f"Checkpoint not found: `{entry.get_checkpoint_path()}`", icon="⚠️")

        # Details
        with st.expander("Details", expanded=False):
            col1, col2 = st.columns(2)

            with col1:
                st.markdown("**Description**")
                st.write(entry.description or "No description")

                if entry.tags:
                    st.markdown("**Tags**")
                    st.write(", ".join(entry.tags))

                st.markdown("**Files**")
                st.text(f"Checkpoint: {entry.checkpoint}")
                st.text(f"Config: {entry.config_file}")
                st.text(f"Directory: {entry.path}")

            with col2:
                st.markdown("**Metrics**")
                if entry.metrics:
                    for key, value in entry.metrics.items():
                        if isinstance(value, float):
                            st.metric(key, f"{value:.4f}")
                        else:
                            st.metric(key, value)
                else:
                    st.write("No metrics recorded")

                st.markdown("**Training Configuration**")
                if entry.training_config:
                    with st.expander("View Config"):
                        st.json(entry.training_config)
                else:
                    st.write("No configuration saved")

            # Actions
            st.divider()
            action_col1, action_col2, action_col3 = st.columns(3)

            with action_col1:
                if entry.model_type == "research":
                    if st.button(
                        "⬆️ Promote to Production",
                        key=f"promote_{entry.id}",
                        use_container_width=True,
                    ):
                        if zoo.promote_to_production(entry.id):
                            st.success("Model promoted to production!")
                            st.rerun()
                        else:
                            st.error("Failed to promote model")

            with action_col2:
                if st.button(
                    "📋 Copy Path",
                    key=f"copy_{entry.id}",
                    use_container_width=True,
                ):
                    st.code(str(entry.get_checkpoint_path()))
                    st.info("Path shown above - copy it manually")

            with action_col3:
                if st.button(
                    "🗑️ Remove",
                    key=f"delete_{entry.id}",
                    type="secondary",
                    use_container_width=True,
                ):
                    st.session_state[f"confirm_delete_{entry.id}"] = True

            # Confirm delete
            if st.session_state.get(f"confirm_delete_{entry.id}"):
                st.warning("Are you sure? This removes the registry entry, not the model files.")

                confirm_col1, confirm_col2 = st.columns(2)
                with confirm_col1:
                    if st.button("Yes, Remove", key=f"confirm_yes_{entry.id}"):
                        if zoo.remove(entry.id):
                            st.success("Model removed from zoo")
                            del st.session_state[f"confirm_delete_{entry.id}"]
                            st.rerun()
                with confirm_col2:
                    if st.button("Cancel", key=f"confirm_no_{entry.id}"):
                        del st.session_state[f"confirm_delete_{entry.id}"]
                        st.rerun()

        st.divider()


if __name__ == "__main__":
    main()
else:
    main()
