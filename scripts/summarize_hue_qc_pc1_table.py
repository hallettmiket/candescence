#!/usr/bin/env python3
"""Summarize hue QC results into one wide table.

Reads summary_*.csv files from one or more directories and produces a
consolidated CSV + Markdown table of key metrics per model x space.

Usage::

    python scripts/summarize_hue_qc_pc1_table.py \
        --dirs s14_run=/data/.../s14_run/analyses/hue_nuisance_qc \
               greyscale_s14=/data/.../greyscale_s14/analyses/hue_nuisance_qc \
        --output hue_qc_pc1_hue_summary.csv
"""
from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

METRICS_OF_INTEREST = [
    "pc1_hue_corr_raw_overall",
    "pc1_hue_corr_residual_overall",
    "delta_evr_pc1",
    "evr_pc1_raw",
    "evr_pc1_residual",
]


def _parse_summary(csv_path: Path) -> dict[str, str | float]:
    """Parse a long-format summary CSV into a metric -> value dict."""
    df = pd.read_csv(csv_path)
    return dict(zip(df["metric"], df["value"]))


def main() -> None:
    parser = argparse.ArgumentParser(description="Summarize hue QC PC1 metrics")
    parser.add_argument(
        "--dirs",
        nargs="+",
        required=True,
        help="label=path pairs, e.g. s14_run=/data/.../hue_nuisance_qc",
    )
    parser.add_argument("--output", type=Path, default=None)
    args = parser.parse_args()

    rows = []
    for entry in args.dirs:
        if "=" in entry:
            label, dirpath = entry.split("=", 1)
        else:
            dirpath = entry
            label = Path(dirpath).parent.parent.name
        dirpath = Path(dirpath)

        for csv_path in sorted(dirpath.glob("summary_*.csv")):
            space = csv_path.stem.replace("summary_", "")
            metrics = _parse_summary(csv_path)
            row = {"model": label, "space": space}
            for m in METRICS_OF_INTEREST:
                row[m] = metrics.get(m, "")
            rows.append(row)

    df = pd.DataFrame(rows)

    # Print Markdown table (manual formatting to avoid tabulate dependency)
    print()
    cols = df.columns.tolist()
    header = "| " + " | ".join(cols) + " |"
    sep = "| " + " | ".join("---" for _ in cols) + " |"
    print(header)
    print(sep)
    for _, row in df.iterrows():
        line = "| " + " | ".join(str(row[c]) for c in cols) + " |"
        print(line)
    print()

    if args.output:
        df.to_csv(args.output, index=False)
        print(f"Saved to {args.output}")


if __name__ == "__main__":
    main()
