"""
Purpose: Train a new object detector on the MODERN track (torchvision FCOS, GPU,
         in-process) — distinct from the legacy mmdet detectors, which are
         inference-only. Trains on the legacy pickle annotation format and
         registers the result in the model zoo.
Author: Hallett Lab
Date: 2026-06-16
Input: An annotation pickle + image directory, class names, and hyperparameters.
Output: A trained checkpoint in the zoo + a registered zoo entry.

Training runs in a background thread (see interface.core.training_jobs), so the
UI stays responsive: live loss/progress, a working Stop button, and the freedom
to switch pages while a run is in flight.

Launch via the unified interface:
    streamlit run src/candescence/interface/app.py
"""

import time
from pathlib import Path

import torch
import streamlit as st

from candescence.core.dataset_zoo import DatasetZoo
from candescence.core.logging_config import get_logger
from candescence.core.model_zoo import ModelZoo
from candescence.core.morphology import CLASS_NAMES
from candescence.detection.modern import SUPPORTED_BACKBONES
from candescence.interface.core.theme import apply_theme, page_header
from candescence.interface.core.training_jobs import start_training_job

logger = get_logger("candescence.interface.train_detector")

# How often (seconds) the page re-polls a running background job.
_POLL_SECONDS = 0.7

# session_state keys for the in-flight job and its captured registration config.
_JOB = "train_job"
_JOB_CFG = "train_job_cfg"
_JOB_FINALIZED = "train_job_finalized"
_LAST_RESULT = "train_detector_result"


def _parse_classes(text: str) -> list:
    parts = [p.strip() for chunk in text.splitlines() for p in chunk.split(",")]
    return [p for p in parts if p]


def _detection_datasets() -> list:
    """Registered datasets that can drive detection training (have a train pkl)."""
    try:
        return [
            e for e in DatasetZoo().list_datasets()
            if e.metadata.get("engine") == "detection" and e.metadata.get("train_pkl")
        ]
    except Exception as exc:  # pragma: no cover - defensive
        logger.warning("dataset zoo unavailable: %s", exc)
        return []


def _default_paths() -> tuple:
    """Seed (annotation pkl, image dir) from a registered detection dataset.

    Prefers the Varasana curriculum set; falls back to any detection dataset, or
    empty strings (no hard-coded paths — everything resolves through the zoo).
    """
    datasets = _detection_datasets()
    entry = next((e for e in datasets if e.id == "varasana_curriculum"), None)
    entry = entry or (datasets[0] if datasets else None)
    if entry is None:
        return "", ""
    return entry.metadata.get("train_pkl", ""), entry.metadata.get("train_image_dir", "")


def _render_dataset_picker(*, disabled: bool) -> None:
    """Offer the project's detection datasets from the zoo and prefill the inputs.

    Surfaces the right data for this analysis: only detection datasets (Varasana,
    Grace) appear here — the TLV (VAE) datasets belong to the TLV pages instead.
    """
    datasets = _detection_datasets()
    if not datasets:
        st.caption("No detection datasets registered in the zoo yet — enter paths "
                   "manually below, or run `scripts/populate_zoo.py`.")
        return

    labels = {e.id: f"{e.name} · {e.project}" for e in datasets}
    col_pick, col_use = st.columns([4, 1])
    pick = col_pick.selectbox(
        "Load from a registered detection dataset",
        ["—"] + [e.id for e in datasets],
        format_func=lambda x: "—" if x == "—" else labels[x],
        help="Detection datasets from the dataset zoo. Choosing one fills in the "
             "annotation pickle, image directory, class names, and project below.",
        disabled=disabled,
    )
    if pick != "—":
        entry = DatasetZoo().get(pick)
        if entry is not None:
            st.caption(entry.description)
        if col_use.button("Use dataset", use_container_width=True, disabled=disabled):
            meta = entry.metadata if entry else {}
            st.session_state["td_ann"] = meta.get("train_pkl", "")
            st.session_state["td_img"] = meta.get("train_image_dir", "")
            if entry and entry.classes:
                st.session_state["td_classes"] = ", ".join(entry.classes)
            if entry and entry.project in ("varasana", "grace", "tlv"):
                st.session_state["td_project"] = entry.project
            st.rerun()


def main() -> None:
    apply_theme()
    page_header(
        "Train a detector",
        icon="🧪",
        description=(
            "Train a new FCOS detector on the **modern** track (torchvision, GPU, "
            "in-process). New models run natively — no legacy worker. The legacy "
            "Varasana/Grace models stay inference-only."
        ),
    )

    job = st.session_state.get(_JOB)
    running = job is not None and job.is_running()

    device = "cuda:0" if torch.cuda.is_available() else "cpu"
    st.caption(f"Compute device: **{device}**"
               + ("" if device.startswith("cuda") else " — training on CPU is slow."))
    if running:
        st.info("A training job is running below — the UI stays responsive; you "
                "can watch progress, stop it, or visit other pages.")

    st.subheader("Dataset")
    # Seed the editable fields once; the zoo picker (below) overwrites them on use.
    default_ann, default_img = _default_paths()
    st.session_state.setdefault("td_ann", default_ann)
    st.session_state.setdefault("td_img", default_img)
    st.session_state.setdefault("td_classes", ", ".join(CLASS_NAMES))
    st.session_state.setdefault("td_project", "varasana")

    _render_dataset_picker(disabled=running)

    ann_pkl = st.text_input("Annotation pickle (.pkl)", key="td_ann")
    img_dir = st.text_input("Image directory", key="td_img")
    classes_text = st.text_area(
        "Class names (comma/newline separated; order = label index)",
        key="td_classes", height=80,
    )
    class_names = _parse_classes(classes_text)
    st.caption(f"{len(class_names)} classes")

    st.subheader("Hyperparameters")
    c1, c2, c3 = st.columns(3)
    epochs = c1.number_input("Epochs", 1, 100, 2, 1)
    lr = c2.number_input("Learning rate", 1e-5, 1.0, 5e-3, format="%.5f")
    batch_size = c3.number_input("Batch size", 1, 8, 2, 1)
    c4, c5, c6 = st.columns(3)
    max_images = c4.number_input(
        "Max images (0 = all)", 0, 100000, 0, 1,
        help="Cap the dataset for a quick run; 0 uses every annotated image.",
    )
    backbone = c5.selectbox(
        "Backbone", list(SUPPORTED_BACKBONES), index=0,
        help="resnet101 matches the published Varasana/Grace detectors "
             "(heavier; slower to train than resnet50).",
    )
    pretrained = c6.checkbox(
        "Pretrained backbone", value=True,
        help="Initialise the backbone from ImageNet weights (recommended).",
    )

    st.subheader("Output")
    o1, o2 = st.columns(2)
    model_id = o1.text_input("Model id", value="my_detector_v1")
    project = o2.selectbox(
        "Project", ["varasana", "grace", "tlv", "other"], key="td_project")
    register = st.checkbox("Register in the model zoo when done", value=True)

    start = st.button("Start training", type="primary", disabled=running)
    if start:
        _start(
            ann_pkl=ann_pkl, img_dir=img_dir, class_names=class_names,
            epochs=int(epochs), lr=float(lr), batch_size=int(batch_size),
            max_images=(int(max_images) or None), backbone=backbone,
            pretrained=pretrained, device=device, model_id=model_id,
            project=project, register=register,
        )
        return

    if job is not None:
        _render_job(job)
    else:
        _show_last_result()


def _start(*, ann_pkl, img_dir, class_names, epochs, lr, batch_size, max_images,
           backbone, pretrained, device, model_id, project, register) -> None:
    """Validate inputs, launch a background training job, and rerun to monitor."""
    if not Path(ann_pkl).exists():
        st.error(f"Annotation pickle not found: {ann_pkl}")
        return
    if not Path(img_dir).is_dir():
        st.error(f"Image directory not found: {img_dir}")
        return

    out_dir = ModelZoo().zoo_path / model_id
    job = start_training_job(
        ann_pkl=ann_pkl, img_dir=img_dir, out_dir=str(out_dir),
        class_names=class_names, epochs=epochs, lr=lr, batch_size=batch_size,
        max_images=max_images, backbone=backbone,
        pretrained_backbone=pretrained, device=device,
    )
    st.session_state[_JOB] = job
    st.session_state[_JOB_CFG] = {
        "model_id": model_id, "project": project, "register": register,
        "epochs": epochs, "lr": lr, "batch_size": batch_size,
        "backbone": backbone, "out_dir": str(out_dir),
    }
    st.session_state[_JOB_FINALIZED] = False
    st.rerun()


def _render_job(job) -> None:
    """Render the live monitor for a running job, or the outcome of a finished one."""
    job.drain()
    st.divider()
    st.subheader("Training job")

    if job.is_running():
        st.progress(job.fraction_done())
        event = job.last_event
        if event:
            st.write(
                f"Epoch {event['epoch'] + 1}/{event['epochs']} · "
                f"step {event['step'] + 1}/{event['total_steps']} · "
                f"loss {event['loss']:.3f}"
            )
        else:
            st.write("Starting…")
        if job.losses:
            st.line_chart(job.losses)

        if job.stop_requested:
            st.caption("Stop requested — finishing the current step…")
        elif st.button("Stop training"):
            job.request_stop()
            st.rerun()

        # Poll: re-run the script so the snapshot refreshes while we wait.
        time.sleep(_POLL_SECONDS)
        st.rerun()
        return

    # Finished (success, error, or stopped early). Finalize once.
    if not st.session_state.get(_JOB_FINALIZED):
        _finalize(job)
        st.session_state[_JOB_FINALIZED] = True

    if job.error:
        st.error(f"Training failed: {job.error}")
    else:
        if job.losses:
            st.line_chart(job.losses)
        summary = st.session_state.get(_LAST_RESULT, {})
        if summary.get("stopped_early"):
            st.warning("Stopped early — checkpoint saved with the steps completed.")
        else:
            st.success("Training complete.")
        st.json(summary)

    if st.button("Train another"):
        for key in (_JOB, _JOB_CFG, _JOB_FINALIZED):
            st.session_state.pop(key, None)
        st.rerun()


def _finalize(job) -> None:
    """Build the summary and (optionally) register the trained model in the zoo."""
    cfg = st.session_state.get(_JOB_CFG, {})
    if job.error or job.result is None:
        return

    result = job.result
    final_loss = result.history[-1]["epoch_loss"] if result.history else None
    summary = {
        "checkpoint": result.checkpoint_path,
        "images": result.num_images,
        "classes": result.num_classes,
        "architecture": result.architecture,
        "backbone": cfg.get("backbone"),
        "final_epoch_loss": final_loss,
        "stopped_early": job.stop_requested,
    }

    if cfg.get("register"):
        try:
            zoo = ModelZoo()
            zoo.register(
                model_id=cfg["model_id"], name=cfg["model_id"],
                project=cfg["project"], model_type="research", version="1.0",
                architecture=result.architecture, checkpoint="model.pth",
                config_file="", path=Path(cfg["out_dir"]),
                metrics={"final_epoch_loss": final_loss} if final_loss else {},
                description="Modern torchvision FCOS detector.",
                tags=["fcos", "detection", "modern", "torchvision", cfg["backbone"]],
                training_config={"epochs": cfg["epochs"], "lr": cfg["lr"],
                                 "batch_size": cfg["batch_size"],
                                 "backbone": cfg["backbone"]},
            )
            summary["registered_as"] = cfg["model_id"]
        except Exception as exc:  # registration is best-effort
            st.warning(f"Trained, but zoo registration failed: {exc}")

    st.session_state[_LAST_RESULT] = summary


def _show_last_result() -> None:
    result = st.session_state.get(_LAST_RESULT)
    if result:
        st.info("Last training result:")
        st.json(result)


main()
