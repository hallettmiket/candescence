"""
Purpose: Tests for the modern (torchvision) detection track — dataset loading,
         model build, and a GPU-guarded train+infer round trip.
Author: Hallett Lab
Date: 2026-06-16
Input: A synthetic pickle dataset built in a tmp dir.
Output: Pytest assertions.
"""

import pickle

import numpy as np
import pytest
import torch
from PIL import Image

from candescence.detection.modern import (
    PickleDetectionDataset,
    build_fcos,
    load_modern_model,
    train_detector,
)
from candescence.detection.modern import inference as modern_inference


def _make_dataset(tmp_path, n_images=3, size=64):
    img_dir = tmp_path / "imgs"
    img_dir.mkdir()
    records = []
    for i in range(n_images):
        name = f"img_{i}.png"
        Image.new("RGB", (size, size), (i * 30 % 255, 60, 120)).save(img_dir / name)
        records.append({
            "filename": f"/old/server/path/{name}",  # stale path -> resolved by basename
            "width": size, "height": size,
            "ann": {
                "bboxes": np.array([[5, 5, 30, 30], [20, 20, 50, 55]], dtype=np.float32),
                "labels": np.array([0, 1], dtype=np.int64),
            },
        })
    ann_pkl = tmp_path / "ann.pkl"
    with open(ann_pkl, "wb") as handle:
        pickle.dump(records, handle)
    return str(ann_pkl), str(img_dir)


def test_dataset_loads_and_resolves_by_basename(tmp_path):
    ann_pkl, img_dir = _make_dataset(tmp_path)
    ds = PickleDetectionDataset(ann_pkl, img_dir)
    assert len(ds) == 3
    assert ds.num_classes == 2
    image, target = ds[0]
    assert image.ndim == 3 and image.shape[0] == 3
    assert target["boxes"].shape[1] == 4
    assert target["labels"].dtype == torch.int64


def test_dataset_skips_degenerate_boxes(tmp_path):
    ann_pkl, img_dir = _make_dataset(tmp_path, n_images=1)
    # Rewrite with one good and one zero-area box.
    with open(ann_pkl, "rb") as handle:
        records = pickle.load(handle)
    records[0]["ann"]["bboxes"] = np.array(
        [[5, 5, 30, 30], [10, 10, 10, 10]], dtype=np.float32)
    records[0]["ann"]["labels"] = np.array([0, 1], dtype=np.int64)
    with open(ann_pkl, "wb") as handle:
        pickle.dump(records, handle)
    ds = PickleDetectionDataset(ann_pkl, img_dir)
    _, target = ds[0]
    assert len(target["boxes"]) == 1  # degenerate box dropped


def test_build_fcos_num_classes():
    model = build_fcos(7, pretrained_backbone=False)
    assert model.head.classification_head.num_classes == 7


@pytest.mark.skipif(not torch.cuda.is_available(),
                    reason="GPU not available for the modern train+infer round trip")
def test_train_and_infer_round_trip(tmp_path):
    ann_pkl, img_dir = _make_dataset(tmp_path, n_images=4)
    out_dir = tmp_path / "model"
    result = train_detector(
        ann_pkl, img_dir, str(out_dir),
        class_names=["a", "b"], epochs=1, batch_size=2,
        pretrained_backbone=False, num_workers=0,
    )
    assert result.num_classes == 2
    assert result.history and result.history[-1]["epoch_loss"] == result.history[-1]["epoch_loss"]

    loaded = load_modern_model(result.checkpoint_path)
    assert loaded.classes == ["a", "b"]
    dets = modern_inference.detect(loaded, f"{img_dir}/img_0.png", score_thr=0.0)
    for d in dets:
        assert len(d.bbox) == 4 and isinstance(d.label_name, str)
