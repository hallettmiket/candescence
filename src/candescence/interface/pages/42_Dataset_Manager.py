"""
Purpose: Dataset Manager Page for Candescence unified interface
Author: Hallett Lab
Date: 2026-02-05

View and manage registered datasets across all projects using the unified dataset zoo.
"""

from datetime import datetime
from pathlib import Path
from typing import Optional

import streamlit as st

from candescence.interface.core.theme import (
    apply_theme,
    page_header,
)
from candescence.core.dataset_zoo import DatasetZoo, DatasetEntry
from candescence.core.projects import project_color
from candescence.core.settings import get_settings

_settings = get_settings()

# Page configuration

def _get_project_color(project: str) -> str:
    """Get theme color for a project (delegates to the project registry)."""
    return project_color(project)


def main() -> None:
    """Main application entry point."""
    apply_theme()

    page_header(
        title="Dataset Manager",
        icon="📁",
        description="View and manage training datasets across all projects"
    )

    # Load zoo
    try:
        zoo = DatasetZoo()
    except Exception as e:
        st.error(f"Failed to load dataset zoo: {e}")
        return

    # Sidebar
    with st.sidebar:
        st.subheader("Filters")

        projects = ["All"] + [p.upper() for p in zoo.get_projects()]
        project_filter = st.selectbox(
            "Project",
            options=projects,
            index=0,
        )

        st.divider()

        # Summary stats
        st.subheader("Summary")
        summary = zoo.get_summary()

        total_datasets = 0
        total_samples = 0

        for project, count in sorted(summary.items()):
            color = _get_project_color(project)
            project_datasets = zoo.list_datasets(project=project)
            samples = sum(ds.num_samples for ds in project_datasets)
            total_datasets += count
            total_samples += samples

            st.markdown(
                f"**{project.upper()}**: "
                f"{count} datasets, {samples:,} samples"
            )

        st.divider()
        st.metric("Total Datasets", total_datasets)
        st.metric("Total Samples", f"{total_samples:,}")

        st.divider()

        # Register new dataset button
        st.subheader("Actions")
        if st.button("➕ Register Dataset", use_container_width=True):
            st.session_state["show_register_form"] = True

    # Apply filters
    project = None if project_filter == "All" else project_filter.lower()
    datasets = zoo.list_datasets(project=project)

    # Registration form
    if st.session_state.get("show_register_form"):
        _render_register_form(zoo)
        st.divider()

    # Main content
    if not datasets:
        st.info("No datasets registered yet.")
        st.markdown(
            f"""
            ### Getting Started

            Register a dataset to track your training data:

            1. Click **Register Dataset** in the sidebar
            2. Enter the dataset path and details
            3. The dataset will appear here for reference

            **Common Dataset Locations:**
            - Varasana curriculum: `{_settings.refined_path}/varasana_data/curriculum/`
            - TLV images: `{_settings.image_dir}`
            """
        )
        return

    st.markdown(f"**{len(datasets)} datasets found**")

    # Dataset list
    for ds in datasets:
        _render_dataset_card(ds, zoo)


def _render_register_form(zoo: DatasetZoo) -> None:
    """Render the dataset registration form."""
    st.subheader("Register New Dataset")

    with st.form("register_dataset"):
        col1, col2 = st.columns(2)

        with col1:
            dataset_id = st.text_input(
                "Dataset ID",
                placeholder="e.g., varasana_curriculum_v2",
                help="Unique identifier (lowercase, underscores)",
            )

            name = st.text_input(
                "Dataset Name",
                placeholder="e.g., Varasana Curriculum v2",
            )

            project = st.selectbox(
                "Project",
                options=["varasana", "tlv", "grace"],
                format_func=str.upper,
            )

            path = st.text_input(
                "Dataset Path",
                placeholder=f"{_settings.refined_path}/...",
            )

        with col2:
            num_samples = st.number_input(
                "Number of Samples",
                min_value=0,
                value=0,
            )

            format_type = st.selectbox(
                "Format",
                options=["curriculum_pkl", "coco_json", "image_dir", "other"],
            )

            description = st.text_area(
                "Description",
                placeholder="Brief description of the dataset",
            )

        # Splits
        st.markdown("**Data Splits** (optional)")
        split_col1, split_col2, split_col3 = st.columns(3)

        with split_col1:
            train_count = st.number_input("Train", min_value=0, value=0)
        with split_col2:
            val_count = st.number_input("Validation", min_value=0, value=0)
        with split_col3:
            test_count = st.number_input("Test", min_value=0, value=0)

        col1, col2 = st.columns(2)

        with col1:
            submitted = st.form_submit_button(
                "Register Dataset",
                type="primary",
                use_container_width=True,
            )

        with col2:
            if st.form_submit_button("Cancel", use_container_width=True):
                st.session_state["show_register_form"] = False
                st.rerun()

        if submitted:
            if not dataset_id:
                st.error("Dataset ID is required")
            elif not name:
                st.error("Dataset name is required")
            elif not path:
                st.error("Dataset path is required")
            else:
                # Build splits dict
                splits = {}
                if train_count > 0:
                    splits["train"] = train_count
                if val_count > 0:
                    splits["val"] = val_count
                if test_count > 0:
                    splits["test"] = test_count

                # Auto-calculate num_samples if not provided
                if num_samples == 0 and splits:
                    num_samples = sum(splits.values())

                try:
                    zoo.register(
                        dataset_id=dataset_id,
                        name=name,
                        project=project,
                        path=Path(path),
                        num_samples=num_samples,
                        splits=splits,
                        format=format_type,
                        description=description,
                    )
                    st.success(f"Dataset registered! ID: {dataset_id}")
                    st.session_state["show_register_form"] = False
                    st.rerun()
                except Exception as e:
                    st.error(f"Failed to register dataset: {e}")


def _render_dataset_card(ds: DatasetEntry, zoo: DatasetZoo) -> None:
    """Render a dataset card with details and actions."""
    color = _get_project_color(ds.project)

    with st.container():
        # Header row
        col1, col2, col3, col4 = st.columns([3, 1, 1, 1])

        with col1:
            st.markdown(f"### {ds.name}")
            st.caption(f"ID: {ds.id} | Format: {ds.format}")

        with col2:
            st.markdown(
                f"""<span style="
                    background-color: {color}20;
                    color: {color};
                    padding: 4px 12px;
                    border-radius: 4px;
                    font-weight: 500;
                ">{ds.project.upper()}</span>""",
                unsafe_allow_html=True,
            )

        with col3:
            st.metric("Samples", f"{ds.num_samples:,}")

        with col4:
            try:
                created = datetime.fromisoformat(ds.created_at)
                date_str = created.strftime("%Y-%m-%d")
            except Exception:
                date_str = ds.created_at[:10]
            st.caption(f"Created: {date_str}")

        # Path status
        if ds.exists():
            st.success(f"Path: `{ds.path}`", icon="✅")
        else:
            st.warning(f"Path not found: `{ds.path}`", icon="⚠️")

        # Details
        with st.expander("Details", expanded=False):
            col1, col2 = st.columns(2)

            with col1:
                st.markdown("**Description**")
                st.write(ds.description or "No description")

                if ds.classes:
                    st.markdown("**Classes**")
                    st.write(", ".join(ds.classes[:15]))
                    if len(ds.classes) > 15:
                        st.caption(f"... and {len(ds.classes) - 15} more")

            with col2:
                st.markdown("**Splits**")
                if ds.splits:
                    for split_name, count in ds.splits.items():
                        st.metric(split_name.title(), f"{count:,}")
                else:
                    st.write("No split information")

                if ds.metadata:
                    st.markdown("**Metadata**")
                    st.json(ds.metadata)

            # Actions
            st.divider()
            action_col1, action_col2, action_col3 = st.columns(3)

            with action_col1:
                if st.button(
                    "📋 Copy Path",
                    key=f"copy_ds_{ds.id}",
                    use_container_width=True,
                ):
                    st.code(ds.path)

            with action_col2:
                if st.button(
                    "🔍 Verify",
                    key=f"verify_{ds.id}",
                    use_container_width=True,
                ):
                    _verify_dataset(ds)

            with action_col3:
                if st.button(
                    "🗑️ Remove",
                    key=f"delete_ds_{ds.id}",
                    type="secondary",
                    use_container_width=True,
                ):
                    st.session_state[f"confirm_delete_ds_{ds.id}"] = True

            # Confirm delete
            if st.session_state.get(f"confirm_delete_ds_{ds.id}"):
                st.warning("Are you sure? This removes the registry entry, not the data files.")

                confirm_col1, confirm_col2 = st.columns(2)
                with confirm_col1:
                    if st.button("Yes, Remove", key=f"confirm_yes_ds_{ds.id}"):
                        if zoo.remove(ds.id):
                            st.success("Dataset removed from zoo")
                            del st.session_state[f"confirm_delete_ds_{ds.id}"]
                            st.rerun()
                with confirm_col2:
                    if st.button("Cancel", key=f"confirm_no_ds_{ds.id}"):
                        del st.session_state[f"confirm_delete_ds_{ds.id}"]
                        st.rerun()

        st.divider()


def _verify_dataset(ds: DatasetEntry) -> None:
    """Verify a dataset exists and show statistics."""
    path = Path(ds.path)

    if not path.exists():
        st.error(f"Dataset path not found: {path}")
        return

    st.success(f"✅ Path verified: {path}")

    try:
        if path.is_dir():
            # Count images (including BMP for Varasana)
            image_extensions = {".jpg", ".jpeg", ".png", ".tif", ".tiff", ".bmp"}
            images = [
                f for f in path.rglob("*")
                if f.suffix.lower() in image_extensions
            ]
            if images:
                st.metric("Images Found", len(images))

            # Count annotations
            pkl_files = list(path.glob("*.pkl"))
            json_files = list(path.glob("*.json"))
            if pkl_files:
                st.metric("PKL Annotation Files", len(pkl_files))
            if json_files:
                st.metric("JSON Files", len(json_files))

            # Show directory structure
            subdirs = [d for d in path.iterdir() if d.is_dir()]
            if subdirs:
                st.markdown("**Subdirectories:**")
                for subdir in sorted(subdirs)[:10]:
                    st.text(f"  📁 {subdir.name}")
        else:
            st.info("Path points to a file, not a directory")

    except Exception as e:
        st.error(f"Error scanning dataset: {e}")


if __name__ == "__main__":
    main()
else:
    main()
