"""
Purpose: Train a new object detector on the MODERN track (torchvision FCOS, GPU,
         in-process) — distinct from the legacy mmdet detectors, which are
         inference-only. Trains on the legacy pickle annotation format and
         registers the result in the model zoo.
Author: Hallett Lab
Date: 2026-06-16
Input: An annotation pickle + image directory, class names, and hyperparameters.
Output: A trained checkpoint in the zoo + a registered zoo entry.

Launch via the unified interface:
    streamlit run src/candescence/interface/app.py
"""

from pathlib import Path

import torch
import streamlit as st

from candescence.core.logging_config import get_logger
from candescence.core.model_zoo import ModelZoo
from candescence.core.morphology import CLASS_NAMES
from candescence.detection.modern import MODERN_ARCHITECTURE, train_detector
from candescence.interface.core.theme import apply_theme, page_header

logger = get_logger("candescence.interface.train_detector")

st.set_page_config(
    page_title="Train Detector | Candescence",
    page_icon="🧪",
    layout="wide",
    initial_sidebar_state="expanded",
)

_DEFAULT_ANN = (
    "/data/lab_vm/refined/candescence_master/projects/varasana/"
    "train-data/train_hyphae.pkl"
)
_DEFAULT_IMG = (
    "/data/lab_vm/refined/candescence_master/projects/varasana/train-data/train"
)


def _parse_classes(text: str) -> list:
    parts = [p.strip() for chunk in text.splitlines() for p in chunk.split(",")]
    return [p for p in parts if p]


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

    device = "cuda:0" if torch.cuda.is_available() else "cpu"
    st.caption(f"Compute device: **{device}**"
               + ("" if device.startswith("cuda") else " — training on CPU is slow."))

    st.subheader("Dataset")
    ann_pkl = st.text_input("Annotation pickle (.pkl)", value=_DEFAULT_ANN)
    img_dir = st.text_input("Image directory", value=_DEFAULT_IMG)
    classes_text = st.text_area(
        "Class names (comma/newline separated; order = label index)",
        value=", ".join(CLASS_NAMES), height=80,
    )
    class_names = _parse_classes(classes_text)
    st.caption(f"{len(class_names)} classes")

    st.subheader("Hyperparameters")
    c1, c2, c3 = st.columns(3)
    epochs = c1.number_input("Epochs", 1, 100, 2, 1)
    lr = c2.number_input("Learning rate", 1e-5, 1.0, 5e-3, format="%.5f")
    batch_size = c3.number_input("Batch size", 1, 8, 2, 1)
    c4, c5 = st.columns(2)
    max_images = c4.number_input(
        "Max images (0 = all)", 0, 100000, 0, 1,
        help="Cap the dataset for a quick run; 0 uses every annotated image.",
    )
    pretrained = c5.checkbox("Pretrained ResNet-50 backbone", value=True)

    st.subheader("Output")
    o1, o2 = st.columns(2)
    model_id = o1.text_input("Model id", value="my_detector_v1")
    project = o2.selectbox("Project", ["varasana", "grace", "tlv", "other"], index=0)
    register = st.checkbox("Register in the model zoo when done", value=True)

    if not st.button("Start training", type="primary"):
        _show_last_result()
        return

    if not Path(ann_pkl).exists():
        st.error(f"Annotation pickle not found: {ann_pkl}")
        return
    if not Path(img_dir).is_dir():
        st.error(f"Image directory not found: {img_dir}")
        return

    progress_bar = st.progress(0.0)
    status = st.empty()
    chart = st.empty()
    losses: list = []

    def on_step(event: dict) -> None:
        done = event["epoch"] * event["total_steps"] + event["step"] + 1
        total = event["epochs"] * event["total_steps"]
        progress_bar.progress(min(1.0, done / max(1, total)))
        status.write(
            f"Epoch {event['epoch'] + 1}/{event['epochs']} · "
            f"step {event['step'] + 1}/{event['total_steps']} · "
            f"loss {event['loss']:.3f}"
        )
        losses.append(event["loss"])
        if len(losses) % 5 == 0:
            chart.line_chart(losses)

    zoo = ModelZoo()
    out_dir = zoo.zoo_path / model_id
    try:
        with st.spinner("Training… (the app is busy until this finishes)"):
            result = train_detector(
                ann_pkl, img_dir, str(out_dir),
                class_names=class_names,
                epochs=int(epochs), lr=float(lr), batch_size=int(batch_size),
                max_images=(int(max_images) or None),
                pretrained_backbone=pretrained, device=device,
                progress=on_step,
            )
    except Exception as exc:  # surface training errors in the UI
        st.error(f"Training failed: {exc}")
        logger.exception("training failed")
        return

    chart.line_chart(losses)
    final_loss = result.history[-1]["epoch_loss"] if result.history else None
    summary = {
        "checkpoint": result.checkpoint_path,
        "images": result.num_images,
        "classes": result.num_classes,
        "final_epoch_loss": final_loss,
    }

    if register:
        try:
            zoo.register(
                model_id=model_id, name=model_id, project=project,
                model_type="research", version="1.0",
                architecture=MODERN_ARCHITECTURE, checkpoint="model.pth",
                config_file="", path=out_dir,
                metrics={"final_epoch_loss": final_loss} if final_loss else {},
                description="Modern torchvision FCOS detector.",
                tags=["fcos", "detection", "modern", "torchvision"],
                training_config={"epochs": int(epochs), "lr": float(lr),
                                 "batch_size": int(batch_size)},
            )
            summary["registered_as"] = model_id
        except Exception as exc:  # registration is best-effort
            st.warning(f"Trained, but zoo registration failed: {exc}")

    st.session_state["train_detector_result"] = summary
    st.success("Training complete.")
    st.json(summary)


def _show_last_result() -> None:
    result = st.session_state.get("train_detector_result")
    if result:
        st.info("Last training result:")
        st.json(result)


main()
