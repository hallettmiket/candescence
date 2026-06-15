#!/usr/bin/env python
"""
Purpose: Self-contained FCOS inference worker for the legacy MMDetection stack
         (Varasana / Grace detectors). Runs *inside* the isolated legacy venv
         (Python 3.8 / torch 1.7.1 / mmcv-full 1.2.5 / mmdet 2.10), invoked as a
         subprocess by candescence.detection.service.client.
Author: Hallett Lab
Date: 2026-06-15
Input: A JSON request (``--request-file`` path, or ``-`` for stdin) with keys:
         config (str), checkpoint (str), images (list[str]),
         score_thr (float, default 0.3), device (str, default "cpu").
Output: A JSON response written to ``--output-file`` (keeps stdout free of the
         CUDA/UserWarning chatter mmdet emits). Schema:
         {"classes": [...], "results": [{"image": str,
           "detections": [{"bbox": [x1,y1,x2,y2], "label": int,
                            "label_name": str, "score": float}, ...]}]}.

IMPORTANT: this module MUST remain free of any ``candescence`` imports — the
legacy venv contains only mmdetection and its dependencies.
"""

import argparse
import json
import sys


def _load_request(path: str) -> dict:
    raw = sys.stdin.read() if path == "-" else open(path).read()
    return json.loads(raw)


def _run(req: dict) -> dict:
    # mmdet 2.10 asserts ``pycocotools.__version__ >= '12.0.2'`` at import time;
    # only the (unbuildable) mmpycocotools fork sets that attribute. Standard
    # pycocotools is fine for bounding-box inference, so shim the attribute
    # before importing mmdet.
    import pycocotools

    if not hasattr(pycocotools, "__version__"):
        pycocotools.__version__ = "12.0.2"

    from mmdet.apis import inference_detector, init_detector

    device = req.get("device", "cpu")
    score_thr = float(req.get("score_thr", 0.3))
    model = init_detector(req["config"], req["checkpoint"], device=device)
    classes = list(getattr(model, "CLASSES", []) or [])

    results = []
    for image in req["images"]:
        raw = inference_detector(model, image)
        # FCOS returns a list of per-class (N, 5) arrays; segmentation models
        # return a (bbox, segm) tuple — keep only the bbox branch.
        per_class = raw[0] if isinstance(raw, tuple) else raw

        detections = []
        for label, arr in enumerate(per_class):
            for row in arr:
                score = float(row[4])
                if score < score_thr:
                    continue
                detections.append(
                    {
                        "bbox": [float(row[0]), float(row[1]),
                                 float(row[2]), float(row[3])],
                        "label": int(label),
                        "label_name": classes[label] if label < len(classes)
                        else str(label),
                        "score": score,
                    }
                )
        results.append({"image": image, "detections": detections})

    return {"classes": classes, "results": results}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--request-file", default="-",
                        help="Path to JSON request, or '-' for stdin.")
    parser.add_argument("--output-file", required=True,
                        help="Path to write the JSON response to.")
    args = parser.parse_args()

    response = _run(_load_request(args.request_file))
    with open(args.output_file, "w") as handle:
        json.dump(response, handle)


if __name__ == "__main__":
    main()
