#!/usr/bin/env python3
"""
Write args.json for uc_vae model that was trained without args.json.
Usage: python scripts/write_args_uc_vae.py /path/to/models/dir
"""
import json
import sys
from pathlib import Path

def main():
    if len(sys.argv) < 2:
        print("Usage: python write_args_uc_vae.py <models_directory>")
        print("Example: python write_args_uc_vae.py /path/to/zoo/<experiment>/<run>/models")
        sys.exit(1)
    models_dir = Path(sys.argv[1])
    if not models_dir.exists():
        print(f"Error: {models_dir} does not exist")
        sys.exit(1)
    model_pth = models_dir / "model.pth"
    if not model_pth.exists():
        print(f"Error: {model_pth} does not exist")
        sys.exit(1)
    args = {
        "architecture": "uc_vae",
        "strategy": 1,
        "latent_dim": 128,
        "intermediate_dim": 256,
        "image_dimension": 128,
        "leaky_relu_slope": 0.02,
        "kernel_size": 3,
        "cond_dim": 1,
    }
    out_path = models_dir / "args.json"
    with open(out_path, "w") as f:
        json.dump(args, f, indent=2)
    print(f"Wrote {out_path}")

if __name__ == "__main__":
    main()
