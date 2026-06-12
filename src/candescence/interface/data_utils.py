"""
Purpose: Data preprocessing utilities for Candescence Interface
Author: Hallett Lab
Date: 2026-01-28

Handles data cleaning and preprocessing for interface data loading.
Addresses issues identified in data quality assessment:
- Inconsistent missing value encoding (NA, N/A, unknown)
- Non-ASCII characters in text fields
- Filtering for labeled/unlabeled samples
"""

from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple, Union

import numpy as np
import pandas as pd

from candescence.core.logging_config import get_logger
from candescence.core.settings import get_settings

logger = get_logger("candescence.interface.data_utils")


# Missing value patterns to standardize
MISSING_VALUE_PATTERNS = [
    'NA', 'N/A', 'na', 'n/a', 'NaN', 'nan',
    'unknown', 'Unknown', 'UNKNOWN',
    'none', 'None', 'NONE',
    '', ' ', '  ',
]


def standardize_missing_values(
    df: pd.DataFrame,
    columns: Optional[List[str]] = None,
    patterns: Optional[List[str]] = None
) -> pd.DataFrame:
    """
    Standardize missing value representations to np.nan.

    Args:
        df: DataFrame to clean
        columns: Specific columns to clean (None = all object columns)
        patterns: Additional patterns to treat as missing

    Returns:
        Cleaned DataFrame with standardized missing values
    """
    df = df.copy()

    all_patterns = set(MISSING_VALUE_PATTERNS)
    if patterns:
        all_patterns.update(patterns)

    if columns is None:
        columns = df.select_dtypes(include=['object']).columns.tolist()

    for col in columns:
        if col in df.columns:
            # Replace string patterns with NaN
            df[col] = df[col].replace(list(all_patterns), np.nan)

            # Handle whitespace-only strings
            if df[col].dtype == 'object':
                mask = df[col].apply(
                    lambda x: isinstance(x, str) and x.strip() == ''
                )
                df.loc[mask, col] = np.nan

    n_nulls = df[columns].isna().sum().sum()
    logger.info(f"Standardized missing values in {len(columns)} columns, {n_nulls} total nulls")

    return df


def clean_non_ascii(
    df: pd.DataFrame,
    columns: Optional[List[str]] = None,
    replacement: str = ''
) -> pd.DataFrame:
    """
    Remove or replace non-ASCII characters from text columns.

    Args:
        df: DataFrame to clean
        columns: Specific columns to clean (None = all object columns)
        replacement: String to replace non-ASCII characters with

    Returns:
        Cleaned DataFrame
    """
    df = df.copy()

    if columns is None:
        columns = df.select_dtypes(include=['object']).columns.tolist()

    for col in columns:
        if col in df.columns and df[col].dtype == 'object':
            df[col] = df[col].apply(
                lambda x: x.encode('ascii', 'ignore').decode('ascii')
                if isinstance(x, str) else x
            )

    return df


def filter_labeled_samples(
    df: pd.DataFrame,
    label_column: str,
    unlabeled_values: Optional[List[str]] = None
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Split DataFrame into labeled and unlabeled samples.

    Args:
        df: DataFrame to filter
        label_column: Column containing labels
        unlabeled_values: Values to treat as unlabeled (default: None, nan, '')

    Returns:
        Tuple of (labeled_df, unlabeled_df)
    """
    if unlabeled_values is None:
        unlabeled_values = ['None', 'none', 'NONE', '', 'unknown', 'Unknown']

    # Create mask for unlabeled
    is_unlabeled = (
        df[label_column].isna() |
        df[label_column].isin(unlabeled_values)
    )

    labeled_df = df[~is_unlabeled].copy()
    unlabeled_df = df[is_unlabeled].copy()

    logger.info(
        f"Split {len(df)} samples: {len(labeled_df)} labeled, "
        f"{len(unlabeled_df)} unlabeled ({100*len(unlabeled_df)/len(df):.1f}%)"
    )

    return labeled_df, unlabeled_df


def get_class_distribution(
    df: pd.DataFrame,
    label_column: str
) -> pd.Series:
    """
    Get the distribution of classes in a label column.

    Args:
        df: DataFrame with labels
        label_column: Column containing class labels

    Returns:
        Series with class counts, sorted descending
    """
    distribution = df[label_column].value_counts(dropna=False)
    return distribution


def compute_class_weights(
    df: pd.DataFrame,
    label_column: str,
    strategy: str = 'balanced'
) -> Dict[str, float]:
    """
    Compute class weights for handling imbalanced data.

    Args:
        df: DataFrame with labels
        label_column: Column containing class labels
        strategy: Weighting strategy ('balanced', 'sqrt', 'equal')

    Returns:
        Dictionary mapping class labels to weights
    """
    counts = df[label_column].value_counts()
    total = counts.sum()
    n_classes = len(counts)

    weights = {}

    if strategy == 'balanced':
        # sklearn balanced formula: n_samples / (n_classes * n_samples_per_class)
        for label, count in counts.items():
            weights[label] = total / (n_classes * count)
    elif strategy == 'sqrt':
        # Square root dampening
        for label, count in counts.items():
            weights[label] = np.sqrt(total / count)
    elif strategy == 'equal':
        for label in counts.index:
            weights[label] = 1.0
    else:
        raise ValueError(f"Unknown strategy: {strategy}")

    # Normalize so max weight is 1.0
    max_weight = max(weights.values())
    weights = {k: v / max_weight for k, v in weights.items()}

    return weights


def load_and_clean_metadata(
    path: Union[str, Path],
    label_column: Optional[str] = None,
    filter_labeled: bool = False
) -> pd.DataFrame:
    """
    Load and clean metadata file with standard preprocessing.

    Args:
        path: Path to metadata file (CSV or TSV)
        label_column: Column for label filtering (if filter_labeled=True)
        filter_labeled: Whether to filter to only labeled samples

    Returns:
        Cleaned DataFrame
    """
    path = Path(path)

    # Detect separator
    if path.suffix == '.tsv':
        df = pd.read_csv(path, sep='\t')
    else:
        df = pd.read_csv(path)

    logger.info(f"Loaded {len(df)} rows from {path.name}")

    # Standard cleaning pipeline
    df = standardize_missing_values(df)
    df = clean_non_ascii(df)

    # Optional: filter to labeled only
    if filter_labeled and label_column:
        df, _ = filter_labeled_samples(df, label_column)

    return df


def load_morphology_labels(
    path: Union[str, Path],
    filter_labeled: bool = True
) -> pd.DataFrame:
    """
    Load and clean the morphology labels file.

    This is specialized for the manually_labelled_images.csv format.

    Args:
        path: Path to morphology labels file
        filter_labeled: Whether to filter out unlabeled samples

    Returns:
        Cleaned DataFrame with file_name and morphology columns
    """
    df = pd.read_csv(path)

    # Standardize column names
    df.columns = df.columns.str.lower().str.replace(' ', '_')

    # Clean morphology column
    if 'morphology' in df.columns:
        df = standardize_missing_values(df, columns=['morphology'])

        if filter_labeled:
            df, unlabeled = filter_labeled_samples(df, 'morphology')
            logger.info(f"Filtered to {len(df)} labeled morphology samples")

    return df


def merge_metadata_with_labels(
    metadata_df: pd.DataFrame,
    labels_df: pd.DataFrame,
    merge_key: str = 'file_name'
) -> pd.DataFrame:
    """
    Merge metadata with morphology labels.

    Args:
        metadata_df: Main metadata DataFrame
        labels_df: Morphology labels DataFrame
        merge_key: Column to merge on

    Returns:
        Merged DataFrame
    """
    merged = pd.merge(
        metadata_df,
        labels_df,
        on=merge_key,
        how='left'
    )

    n_labeled = merged['morphology'].notna().sum() if 'morphology' in merged else 0
    logger.info(
        f"Merged {len(metadata_df)} metadata rows with labels: "
        f"{n_labeled} have morphology labels"
    )

    return merged


class DataQualityReport:
    """
    Generate data quality reports for metadata files.

    Example:
        >>> report = DataQualityReport(df)
        >>> report.run_all_checks()
        >>> print(report.summary())
    """

    def __init__(self, df: pd.DataFrame) -> None:
        """
        Initialize with DataFrame to analyze.

        Args:
            df: DataFrame to check
        """
        self.df = df
        self.issues: List[Dict[str, Any]] = []
        self.stats: Dict[str, Any] = {}

    def check_missing_values(self) -> None:
        """Check for missing values in all columns."""
        missing = self.df.isna().sum()
        missing_pct = (missing / len(self.df) * 100).round(2)

        self.stats['missing_values'] = {
            col: {'count': int(missing[col]), 'percent': float(missing_pct[col])}
            for col in self.df.columns
            if missing[col] > 0
        }

        high_missing = missing_pct[missing_pct > 50]
        for col in high_missing.index:
            self.issues.append({
                'type': 'high_missing',
                'column': col,
                'percent': float(high_missing[col]),
                'severity': 'warning'
            })

    def check_duplicates(self, key_columns: Optional[List[str]] = None) -> None:
        """Check for duplicate rows."""
        try:
            if key_columns:
                dups = self.df.duplicated(subset=key_columns, keep=False)
            else:
                # Exclude columns with unhashable types (e.g., images, arrays)
                hashable_cols = []
                for col in self.df.columns:
                    try:
                        # Test if column values are hashable
                        self.df[col].iloc[:1].apply(hash)
                        hashable_cols.append(col)
                    except (TypeError, ValueError):
                        pass

                if not hashable_cols:
                    logger.debug("No hashable columns for duplicate check")
                    self.stats['duplicates'] = {'count': 0, 'percent': 0.0, 'skipped': True}
                    return

                dups = self.df.duplicated(subset=hashable_cols, keep=False)

            n_dups = dups.sum()
        except TypeError as e:
            logger.warning(f"Could not check duplicates: {e}")
            self.stats['duplicates'] = {'count': 0, 'percent': 0.0, 'error': str(e)}
            return

        n_dups = dups.sum()
        self.stats['duplicates'] = {
            'count': int(n_dups),
            'percent': float(n_dups / len(self.df) * 100)
        }

        if n_dups > 0:
            self.issues.append({
                'type': 'duplicates',
                'count': int(n_dups),
                'columns': key_columns,
                'severity': 'warning'
            })

    def check_non_ascii(self) -> None:
        """Check for non-ASCII characters in text columns."""
        text_cols = self.df.select_dtypes(include=['object']).columns

        non_ascii_cols = []
        for col in text_cols:
            has_non_ascii = self.df[col].apply(
                lambda x: bool(x.encode('ascii', 'ignore').decode() != x)
                if isinstance(x, str) else False
            ).any()
            if has_non_ascii:
                non_ascii_cols.append(col)

        self.stats['non_ascii_columns'] = non_ascii_cols

        if non_ascii_cols:
            self.issues.append({
                'type': 'non_ascii',
                'columns': non_ascii_cols,
                'severity': 'info'
            })

    def check_class_balance(
        self,
        label_column: str,
        imbalance_threshold: float = 10.0
    ) -> None:
        """
        Check for class imbalance in a label column.

        Args:
            label_column: Column to check
            imbalance_threshold: Max ratio before flagging imbalance
        """
        if label_column not in self.df.columns:
            return

        counts = self.df[label_column].value_counts(dropna=True)
        if len(counts) == 0:
            return

        max_count = counts.max()
        min_count = counts.min()
        ratio = max_count / min_count if min_count > 0 else float('inf')

        self.stats['class_balance'] = {
            'column': label_column,
            'n_classes': len(counts),
            'max_count': int(max_count),
            'min_count': int(min_count),
            'imbalance_ratio': float(ratio),
            'distribution': counts.to_dict()
        }

        if ratio > imbalance_threshold:
            self.issues.append({
                'type': 'class_imbalance',
                'column': label_column,
                'ratio': float(ratio),
                'severity': 'warning'
            })

    def run_all_checks(
        self,
        key_columns: Optional[List[str]] = None,
        label_column: Optional[str] = None
    ) -> None:
        """
        Run all quality checks.

        Args:
            key_columns: Columns to check for duplicates
            label_column: Column to check for class balance
        """
        self.check_missing_values()
        self.check_duplicates(key_columns)
        self.check_non_ascii()

        if label_column:
            self.check_class_balance(label_column)

    def summary(self) -> str:
        """Generate summary report string."""
        lines = [
            "=" * 60,
            "DATA QUALITY REPORT",
            "=" * 60,
            f"Total rows: {len(self.df)}",
            f"Total columns: {len(self.df.columns)}",
            "",
        ]

        if self.issues:
            lines.append(f"Issues found: {len(self.issues)}")
            lines.append("-" * 40)
            for issue in self.issues:
                severity = issue.get('severity', 'info').upper()
                issue_type = issue.get('type', 'unknown')
                lines.append(f"[{severity}] {issue_type}: {issue}")
        else:
            lines.append("No issues found.")

        lines.append("=" * 60)

        return "\n".join(lines)

    def get_issues(self, severity: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Get list of issues, optionally filtered by severity.

        Args:
            severity: Filter by severity ('warning', 'info', 'error')

        Returns:
            List of issue dictionaries
        """
        if severity:
            return [i for i in self.issues if i.get('severity') == severity]
        return self.issues


# =============================================================================
# Convenience functions for loading cleaned shared data
# =============================================================================

# Default paths for cleaned shared data (resolved from the settings layer).
CLEANED_DATA_DIR = get_settings().metadata_xlsx.parent
CALB_MASTER_CLEANED_PATH = CLEANED_DATA_DIR / "calb_master_cleaned.tsv"
CALB_MASTER_CLEANED_CSV = CLEANED_DATA_DIR / "calb_master_cleaned.csv"


def load_calb_master(
    use_csv: bool = False,
    filter_with_images: bool = False
) -> pd.DataFrame:
    """
    Load the cleaned Calb_Master metadata file.

    This is the shared strain metadata for all Candescence experiments.
    The file has been cleaned to:
    - Standardize missing values (NA, N/A, unknown → NaN)
    - Remove non-ASCII characters
    - Use underscore column names

    Args:
        use_csv: Load CSV instead of TSV (default: TSV)
        filter_with_images: Only return rows that have file_name (image data)

    Returns:
        Cleaned DataFrame with strain metadata

    Example:
        >>> from candescence.interface.data_utils import load_calb_master
        >>> df = load_calb_master()
        >>> df_with_images = load_calb_master(filter_with_images=True)
    """
    path = CALB_MASTER_CLEANED_CSV if use_csv else CALB_MASTER_CLEANED_PATH

    if not path.exists():
        raise FileNotFoundError(
            f"Cleaned Calb_Master file not found at {path}. "
            f"Run 'python -m candescence.scripts.clean_metadata' to generate it."
        )

    if use_csv:
        df = pd.read_csv(path)
    else:
        df = pd.read_csv(path, sep='\t')

    logger.info(f"Loaded {len(df)} rows from {path.name}")

    if filter_with_images:
        df = df[df['file_name'].notna()].copy()
        logger.info(f"Filtered to {len(df)} rows with image data")

    return df


def get_calb_master_path(use_csv: bool = False) -> Path:
    """
    Get the path to the cleaned Calb_Master file.

    Args:
        use_csv: Return CSV path instead of TSV

    Returns:
        Path to cleaned data file
    """
    return CALB_MASTER_CLEANED_CSV if use_csv else CALB_MASTER_CLEANED_PATH
