"""
Purpose: Clean and standardize the Calb_Master metadata file
Author: Hallett Lab
Date: 2026-01-29

Applies data cleaning to address identified issues:
- Inconsistent missing value encoding (NA, N/A, unknown)
- Non-ASCII characters in text fields
- Documents duplicates

Input: the raw Calb_Master labelled TSV (default: <legacy_refined>/candescence_master/
       projects/tlv/data_files/Calb_Master_10062022_Labeled.tsv; override with --input)
Output: the cleaned TSV (default: <refined>/data_files/calb_master_cleaned.tsv;
        override with --output)
"""

import argparse
import logging
from pathlib import Path

import numpy as np
import pandas as pd

from candescence.core.settings import get_settings, legacy_refined_root

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def clean_non_ascii_string(s: str) -> str:
    """
    Remove non-ASCII characters and clean up common encoding issues.

    Args:
        s: Input string

    Returns:
        Cleaned string with ASCII-only characters
    """
    if not isinstance(s, str):
        return s

    # Replace common special characters with ASCII equivalents
    replacements = {
        'ä': 'a',
        'ö': 'o',
        'ü': 'u',
        'é': 'e',
        'è': 'e',
        'ê': 'e',
        'à': 'a',
        'â': 'a',
        'ô': 'o',
        'î': 'i',
        'ï': 'i',
        'ç': 'c',
        'ñ': 'n',
        '©': '',  # Remove copyright symbol
        '®': '',  # Remove registered trademark
        '™': '',  # Remove trademark
        '\xa0': ' ',  # Non-breaking space
        '\u2019': "'",  # Smart quote
        '\u2018': "'",  # Smart quote
        '\u201c': '"',  # Smart quote
        '\u201d': '"',  # Smart quote
    }

    for char, replacement in replacements.items():
        s = s.replace(char, replacement)

    # Remove any remaining non-ASCII
    s = s.encode('ascii', 'ignore').decode('ascii')

    return s.strip()


def standardize_missing_values(df: pd.DataFrame) -> pd.DataFrame:
    """
    Standardize all missing value representations to np.nan.

    Args:
        df: Input DataFrame

    Returns:
        DataFrame with standardized missing values
    """
    # Patterns to treat as missing
    missing_patterns = [
        'NA', 'N/A', 'na', 'n/a', 'NaN', 'nan',
        'unknown', 'Unknown', 'UNKNOWN',
        'none', 'None', 'NONE',
        '', ' ', '  ', 'NULL', 'null',
    ]

    df = df.copy()

    for col in df.columns:
        if df[col].dtype == 'object':
            # Replace explicit missing patterns
            df[col] = df[col].replace(missing_patterns, np.nan)

            # Handle whitespace-only values
            mask = df[col].apply(
                lambda x: isinstance(x, str) and x.strip() == ''
            )
            df.loc[mask, col] = np.nan

    return df


def clean_metadata_file(
    input_path: Path,
    output_path: Path
) -> dict:
    """
    Clean the metadata file and save to output location.

    Args:
        input_path: Path to original TSV file
        output_path: Path for cleaned output

    Returns:
        Dictionary with cleaning statistics
    """
    stats = {
        'original_rows': 0,
        'original_cols': 0,
        'missing_standardized': 0,
        'non_ascii_cleaned': 0,
        'duplicate_keys': [],
    }

    logger.info(f"Reading from: {input_path}")
    # Try different encodings to handle non-UTF-8 characters
    encodings_to_try = ['utf-8', 'latin-1', 'cp1252', 'iso-8859-1']
    df = None
    for encoding in encodings_to_try:
        try:
            df = pd.read_csv(input_path, sep='\t', encoding=encoding)
            logger.info(f"Successfully read file with encoding: {encoding}")
            break
        except UnicodeDecodeError:
            logger.warning(f"Failed to read with encoding: {encoding}")
            continue

    if df is None:
        raise ValueError(f"Could not read file with any of: {encodings_to_try}")

    stats['original_rows'] = len(df)
    stats['original_cols'] = len(df.columns)

    logger.info(f"Loaded {len(df)} rows, {len(df.columns)} columns")

    # Count original missing patterns before standardization
    missing_before = 0
    for col in df.select_dtypes(include=['object']).columns:
        missing_before += df[col].isin(['NA', 'N/A', 'unknown', 'Unknown', '']).sum()

    # Step 1: Standardize missing values
    logger.info("Standardizing missing values...")
    df = standardize_missing_values(df)

    missing_after = df.isna().sum().sum()
    stats['missing_standardized'] = int(missing_before)
    logger.info(f"Standardized {missing_before} missing value patterns")

    # Step 2: Clean non-ASCII characters
    logger.info("Cleaning non-ASCII characters...")
    non_ascii_count = 0
    for col in df.select_dtypes(include=['object']).columns:
        original_values = df[col].dropna().tolist()
        df[col] = df[col].apply(
            lambda x: clean_non_ascii_string(x) if isinstance(x, str) else x
        )
        cleaned_values = df[col].dropna().tolist()

        # Count differences
        for orig, clean in zip(original_values, cleaned_values):
            if orig != clean:
                non_ascii_count += 1

    stats['non_ascii_cleaned'] = non_ascii_count
    logger.info(f"Cleaned {non_ascii_count} cells with non-ASCII characters")

    # Step 3: Check for duplicate keys
    logger.info("Checking for duplicate Plate.Position values...")
    if 'Plate.Position' in df.columns:
        duplicates = df[df.duplicated(subset=['Plate.Position'], keep=False)]
        if len(duplicates) > 0:
            dup_positions = duplicates['Plate.Position'].unique().tolist()
            stats['duplicate_keys'] = dup_positions
            logger.warning(
                f"Found {len(duplicates)} rows with {len(dup_positions)} "
                f"duplicate Plate.Position values"
            )
            logger.warning(f"Duplicate positions: {dup_positions[:10]}...")  # Show first 10

    # Step 4: Standardize column names (replace dots with underscores)
    logger.info("Standardizing column names...")
    df.columns = [col.replace('.', '_') for col in df.columns]

    # Step 5: Report missing value summary
    logger.info("\nMissing value summary:")
    missing_summary = df.isna().sum()
    missing_pct = (missing_summary / len(df) * 100).round(1)
    for col in df.columns:
        if missing_summary[col] > 0:
            logger.info(f"  {col}: {missing_summary[col]} ({missing_pct[col]}%)")

    # Create output directory if needed
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Save cleaned file
    logger.info(f"\nSaving cleaned file to: {output_path}")
    df.to_csv(output_path, sep='\t', index=False)

    # Also save as CSV for broader compatibility
    csv_path = output_path.with_suffix('.csv')
    df.to_csv(csv_path, index=False)
    logger.info(f"Also saved as CSV: {csv_path}")

    # Generate cleaning report
    report_path = output_path.parent / 'calb_master_cleaning_report.txt'
    with open(report_path, 'w') as f:
        f.write("=" * 60 + "\n")
        f.write("CALB_MASTER METADATA CLEANING REPORT\n")
        f.write("=" * 60 + "\n\n")
        f.write(f"Original file: {input_path}\n")
        f.write(f"Cleaned file: {output_path}\n")
        f.write(f"Date: 2026-01-29\n\n")

        f.write("STATISTICS:\n")
        f.write("-" * 40 + "\n")
        f.write(f"Original rows: {stats['original_rows']}\n")
        f.write(f"Original columns: {stats['original_cols']}\n")
        f.write(f"Missing values standardized: {stats['missing_standardized']}\n")
        f.write(f"Non-ASCII cells cleaned: {stats['non_ascii_cleaned']}\n")
        f.write(f"Duplicate Plate_Position values: {len(stats['duplicate_keys'])}\n\n")

        f.write("TRANSFORMATIONS APPLIED:\n")
        f.write("-" * 40 + "\n")
        f.write("1. Standardized missing values:\n")
        f.write("   - 'NA', 'N/A', 'unknown', 'Unknown', '' -> NaN\n")
        f.write("2. Cleaned non-ASCII characters:\n")
        f.write("   - Replaced umlauts (a,o,u) with ASCII equivalents\n")
        f.write("   - Removed copyright/trademark symbols\n")
        f.write("   - Removed other non-ASCII characters\n")
        f.write("3. Standardized column names:\n")
        f.write("   - Replaced '.' with '_' in column names\n\n")

        f.write("MISSING VALUE SUMMARY:\n")
        f.write("-" * 40 + "\n")
        for col in df.columns:
            if missing_summary[col] > 0:
                f.write(f"  {col}: {missing_summary[col]} ({missing_pct[col]}%)\n")

        if stats['duplicate_keys']:
            f.write("\nDUPLICATE PLATE_POSITION VALUES:\n")
            f.write("-" * 40 + "\n")
            f.write("Note: These duplicates were NOT removed.\n")
            f.write("Review manually if deduplication is needed.\n\n")
            for pos in stats['duplicate_keys'][:20]:
                f.write(f"  - {pos}\n")
            if len(stats['duplicate_keys']) > 20:
                f.write(f"  ... and {len(stats['duplicate_keys']) - 20} more\n")

    logger.info(f"Cleaning report saved: {report_path}")

    return stats


def main():
    """Main entry point. Paths default from the settings layer; override via CLI."""
    default_input = (
        legacy_refined_root()
        / "candescence_master/projects/tlv/data_files/Calb_Master_10062022_Labeled.tsv"
    )
    default_output = get_settings().refined_path / "data_files/calb_master_cleaned.tsv"

    parser = argparse.ArgumentParser(description="Clean the Calb_Master metadata TSV.")
    parser.add_argument(
        "--input", type=Path, default=default_input, help="Raw labelled TSV to clean."
    )
    parser.add_argument(
        "--output", type=Path, default=default_output, help="Destination cleaned TSV."
    )
    args = parser.parse_args()
    input_path, output_path = args.input, args.output

    stats = clean_metadata_file(input_path, output_path)

    print("\n" + "=" * 60)
    print("CLEANING COMPLETE")
    print("=" * 60)
    print(f"Output: {output_path}")
    print(f"Rows: {stats['original_rows']}")
    print(f"Missing values standardized: {stats['missing_standardized']}")
    print(f"Non-ASCII cells cleaned: {stats['non_ascii_cleaned']}")
    print(f"Duplicate keys found: {len(stats['duplicate_keys'])}")


if __name__ == "__main__":
    main()
