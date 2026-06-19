"""
Purpose: Dataset handling for TLV VAE training
Author: Hallett Lab
Date: 2026-01-27

Provides FullDataset and LearningDataset classes for managing image data,
augmentation, and train/validation/test splits.

HSV Scale Conventions
---------------------
This module uses two scales for HSV (Hue, Saturation, Value) values:

1. **Storage scale (0-255)**: The DataFrame columns `average_hue`,
   `average_saturation`, and `average_value` store raw values in 0-255 range.
   This matches PIL's HSV mode where all three channels use 0-255.

2. **Conditioning scale (0-1)**: When HSV values are used for model conditioning
   (passed to encoder/decoder), they are normalized to 0-1 by dividing by 255.
   Config settings like `target_hue` and `conditional_decoder_fixed_values`
   also use the 0-1 scale.

Example:
    - Stored: average_hue = 127.5 (in DataFrame)
    - Conditioning: ave_hue = 0.5 (passed to model)
    - Config: conditional_decoder_fixed_values = {'average_hue': 0.5}

To convert between scales:
    - Storage to conditioning: value / 255.0
    - Conditioning to storage: value * 255.0
"""

import glob
import math
import os
import pickle
import random
import shutil
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import torch
import torchvision.transforms.functional as TF
from PIL import Image
from skimage.color import rgb2hsv
from torch.utils.data import DataLoader, Dataset, Subset, random_split
from torchvision import datasets, transforms
from torchvision.transforms import ToPILImage
from torchvision.transforms.functional import to_pil_image

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.tlv.data.dataset")


# ═══════════════════════════════════════════════════════════════════════
# Shared image-processing helpers
# ═══════════════════════════════════════════════════════════════════════

def _edge_band_mask(height: int, width: int, border: int = 12) -> np.ndarray:
    """Return a boolean mask selecting the *border*-pixel band on all four sides."""
    mask = np.zeros((height, width), dtype=bool)
    mask[:border, :] = True
    mask[-border:, :] = True
    mask[:, :border] = True
    mask[:, -border:] = True
    return mask


def _one_hot(value: Any, categories: List[Any]) -> torch.Tensor:
    """One-hot encode *value* against a fixed *categories* list.

    Comparison is done on the string form so ints, floats, and strings
    (e.g. day values 2, 5, 'NA') all match correctly. If *value* is not
    in *categories*, the returned tensor is all zeros — a neutral
    conditioning input that won't shift the model but is also not
    meaningful. Callers should keep *categories* in sync with what the
    dataset was fit on.
    """
    vec = torch.zeros(len(categories), dtype=torch.float)
    key = str(value)
    for i, c in enumerate(categories):
        if str(c) == key:
            vec[i] = 1.0
            return vec
    return vec


def apply_grayscale_background_normalize(
    pil_rgb: Image.Image,
    dim: int,
    target: float = 0.5,
    border: int = 12,
    eps: float = 1e-6,
) -> Image.Image:
    """Shift greyscale luminance so edge-band mean lands on *target*.

    Expects a 3-channel greyscale PIL image (R=G=B) as produced by
    ``img.convert('L').convert('RGB')``.  Returns a PIL RGB image with
    the same property (3 identical channels).

    Only the mean is shifted (``Y' = Y - mu_edge + target``); the
    relative spread of pixel values is preserved.
    """
    arr = np.array(pil_rgb).astype(np.float32) / 255.0
    # Luminance is identical across channels; use channel 0
    lum = arr[:, :, 0]
    mask = _edge_band_mask(dim, dim, border)
    mu_edge = lum[mask].mean()
    shifted = lum - mu_edge + target
    shifted = np.clip(shifted, 0.0, 1.0)
    out = (shifted * 255.0).astype(np.uint8)
    rgb = np.stack([out, out, out], axis=-1)
    return Image.fromarray(rgb, mode='RGB')


class LearningDataset(Dataset):
    """
    Dataset for training/validation/test splits.

    Provides conditional tensors based on strategy for different VAE architectures.
    """

    def __init__(
        self,
        config: Any,
        meta_df_subset: pd.DataFrame,
        target_df: pd.DataFrame
    ) -> None:
        """
        Initialize LearningDataset.

        Args:
            config: Configuration object with strategy and conditional variables.
            meta_df_subset: Subset of metadata for this split.
            target_df: Full target DataFrame for parent lookups.
        """
        super(LearningDataset, self).__init__()
        self.config = config
        self.strategy = getattr(config, 'strategy', 0)
        self.conditional_variables = getattr(config, 'conditional_variables', [])
        self.meta_df_subset = meta_df_subset.copy()
        self.target_df = target_df.copy()
        # Fitted category vocabularies for Strategy 15/16 one-hot encoding.
        # Populated by FullDataset before splits are created; empty for
        # strategies that don't use categorical conditioning.
        self.day_categories: List[Any] = list(getattr(config, 'day_categories', []) or [])
        self.media_categories: List[Any] = list(getattr(config, 'media_categories', []) or [])
        self.plate_phys_categories: List[Any] = list(
            getattr(config, 'plate_phys_categories', []) or []
        )
        # Plate + morphology vocabularies for Strategy 17 invariance training.
        self.plate_categories: List[Any] = list(getattr(config, 'plate_categories', []) or [])
        self.morphology_categories: List[Any] = list(
            getattr(config, 'morphology_categories', []) or []
        )

    def __getitem__(
        self,
        index: int,
        override: bool = False,
        alt_df: Optional[pd.DataFrame] = None
    ) -> Tuple:
        """
        Get item by index.

        Args:
            index: Row index.
            override: Use alternative DataFrame.
            alt_df: Alternative DataFrame when override is True.

        Returns:
            Tuple of (image, [conditions], index) based on strategy.
        """
        if not override:
            if index < 0 or index >= len(self.meta_df_subset):
                raise IndexError("Row index out of range")
            target = self.meta_df_subset
        else:
            if alt_df is None:
                raise ValueError("alt_df must be provided when override is True")
            if index < 0 or index >= len(alt_df):
                raise IndexError("Row index out of range")
            target = alt_df

        img = target.iloc[index]['transformed_image']
        # Convert from storage scale (0-255) to conditioning scale (0-1)
        ave_hue = target.iloc[index]['average_hue'] / 255
        ave_sat = target.iloc[index]['average_saturation'] / 255
        ave_val = target.iloc[index]['average_value'] / 255

        if not (0.0 <= ave_hue <= 1.0 and 0.0 <= ave_sat <= 1.0 and 0.0 <= ave_val <= 1.0):
            raise ValueError("One or more HSV values are out of range [0, 1].")

        if img is None:
            raise ValueError("No transformed image found")
        if not isinstance(img, torch.Tensor):
            img = torch.tensor(img)

        # Handle different strategies
        if self.strategy in (0, 1, 2, 3, 4, 5, 6, 11, 12):
            return img, index

        elif self.strategy in (7, 8):
            cond = self._build_condition_tensor(ave_hue, ave_sat, ave_val)
            return img, cond, index

        elif self.strategy in (15, 16):
            # Strategies 15 & 16: per-variable FiLM conditioning — return a
            # dict of tensors for encoder and decoder sides. Day and media
            # are always identical on both sides (sample identity, unchanged
            # by HSV augmentation). Strategy 16's model ignores the 'day'
            # entry — leaving it in the dict keeps the dataset strategy-agnostic.
            #
            # Denoising direction: on an augmented row, the encoder sees
            # the augmented hue and the decoder is told the parent (real)
            # image's hue, so reconstruction targets the canonical image.
            cond_encoder = self._build_multi_cond_dict(target.iloc[index])
            row = target.iloc[index]
            if self._is_augmented_row(row):
                parent_row = self._lookup_parent_row(row)
                cond_decoder = self._build_multi_cond_dict(parent_row)
            else:
                cond_decoder = {k: v.clone() for k, v in cond_encoder.items()}
            return img, cond_encoder, cond_decoder, index

        elif self.strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 17):
            # Denoising direction: on an augmented row, the encoder sees
            # the augmented hue and the decoder is given the parent (real)
            # row's hue. On non-augmented rows both sides match.
            cond_encoder = self._build_condition_tensor(ave_hue, ave_sat, ave_val)
            row = target.iloc[index]
            if self._is_augmented_row(row):
                parent_row = self._lookup_parent_row(row)
                p_hue = float(parent_row['average_hue']) / 255
                p_sat = float(parent_row['average_saturation']) / 255
                p_val = float(parent_row['average_value']) / 255
                cond_decoder = self._build_condition_tensor(p_hue, p_sat, p_val)
            else:
                cond_decoder = cond_encoder.clone()
            return img, cond_encoder, cond_decoder, index

        elif self.strategy in (9, 10):
            cond_encoder = self._build_condition_tensor(ave_hue, ave_sat, ave_val)

            parent_id = target.iloc[index]['parent_id']
            parent_row = self.target_df.loc[self.target_df['id'] == parent_id]

            parent_cond = []
            if 'average_hue' in self.conditional_variables:
                parent_cond.append(parent_row['average_hue'].values[0] / 255)
            if 'average_saturation' in self.conditional_variables:
                parent_cond.append(parent_row['average_saturation'].values[0] / 255)
            if 'average_value' in self.conditional_variables:
                parent_cond.append(parent_row['average_value'].values[0] / 255)

            cond_decoder = torch.tensor(
                cond_encoder.tolist() + parent_cond,
                dtype=torch.float
            )
            return img, cond_encoder, cond_decoder, index

        else:
            raise ValueError(f"Unknown strategy: {self.strategy}")

    def _build_condition_tensor(
        self,
        ave_hue: float,
        ave_sat: float,
        ave_val: float
    ) -> torch.Tensor:
        """Build conditional tensor based on conditional variables."""
        cond = []
        if 'average_hue' in self.conditional_variables:
            cond.append(ave_hue)
        if 'average_saturation' in self.conditional_variables:
            cond.append(ave_sat)
        if 'average_value' in self.conditional_variables:
            cond.append(ave_val)
        return torch.tensor(cond, dtype=torch.float)

    def _build_multi_cond_dict(self, row: pd.Series) -> Dict[str, torch.Tensor]:
        """Build per-variable conditioning dict for Strategy 15/16.

        Returns a dict with keys 'hue', 'day', 'media', 'plate_phys':
          - 'hue' is the HSV sub-vector (entries selected by
            ``config.conditional_variables``, normalized to [0, 1]).
          - 'day', 'media', and 'plate_phys' are one-hot tensors over
            the fitted category vocabularies on the config.
        """
        ave_hue = float(row['average_hue']) / 255.0
        ave_sat = float(row['average_saturation']) / 255.0
        ave_val = float(row['average_value']) / 255.0

        hue_tensor = self._build_condition_tensor(ave_hue, ave_sat, ave_val)
        day_tensor = _one_hot(row['day'], self.day_categories)
        media_tensor = _one_hot(row['media'], self.media_categories)
        plate_phys_tensor = _one_hot(
            row['plate_phys'], self.plate_phys_categories
        )
        return {
            'hue': hue_tensor,
            'day': day_tensor,
            'media': media_tensor,
            'plate_phys': plate_phys_tensor,
        }

    def _is_augmented_row(self, row: pd.Series) -> bool:
        """True if ``row`` is an HSV-augmented child of a real image."""
        image_type = row.get('image_type', None) if hasattr(row, 'get') else None
        if image_type != 'augmented':
            return False
        parent_id = row.get('parent_id', None)
        return parent_id is not None and not pd.isnull(parent_id)

    def _lookup_parent_row(self, row: pd.Series) -> pd.Series:
        """Return the real-image parent row for an augmented ``row``."""
        parent_id = row['parent_id']
        matches = self.target_df.loc[self.target_df['id'] == parent_id]
        if matches.empty:
            raise ValueError(f"Parent row not found for parent_id={parent_id}")
        return matches.iloc[0]

    def __len__(self) -> int:
        return len(self.meta_df_subset)


class FullDataset(Dataset):
    """
    Full dataset manager for TLV experiments.

    Handles loading, augmentation, adjustment, and splitting of colony images.
    """

    def __init__(self, config: Any) -> None:
        """
        Initialize FullDataset.

        Args:
            config: Configuration object with paths and parameters.
        """
        super(FullDataset, self).__init__()

        self.config = config
        self.train_dataset: Optional[LearningDataset] = None
        self.validation_dataset: Optional[LearningDataset] = None
        self.test_dataset: Optional[LearningDataset] = None

        self._ensure_directories()
        self.to_tensor = transforms.ToTensor()
        self._working_df: Optional[pd.DataFrame] = None

        # Step 1: Get list of raw colony image filenames. Accept common image
        # formats (not just .bmp) so users can bring their own data.
        # Normalise to a Path on the config too: the Streamlit app sets this
        # from a text input (a str), and later stages (e.g. _load_images) do
        # ``self.config.raw_images_path / filename`` which requires a Path.
        raw_images_path = Path(config.raw_images_path)
        config.raw_images_path = raw_images_path
        image_extensions = ('bmp', 'png', 'jpg', 'jpeg', 'tif', 'tiff')
        raw_colony_image_files: list = []
        for ext in image_extensions:
            raw_colony_image_files.extend(raw_images_path.glob(f'*.{ext}'))
        raw_colony_image_filenames = [f.name for f in raw_colony_image_files]
        self.meta_df = self._filenames_to_dataframe(raw_colony_image_filenames)

        # Step 2: Merge metadata. The cleaned strain-level spreadsheet is
        # optional — external users (and the bundled sample) may not have it,
        # in which case we proceed with filename-derived metadata + HSV only.
        metadata_path = Path(config.metadata_path) if config.metadata_path else None
        if metadata_path is not None and metadata_path.is_file():
            metadata = pd.read_excel(metadata_path)
            self._merge_metadata(metadata)
        else:
            logger.warning(
                "Strain metadata not found (%s); proceeding without it.",
                metadata_path,
            )
        self._merge_manually_labelled_images()

        # Step 3: Select images
        n_required = config.train_num + config.validation_num + config.test_num
        self._select_images(n_required)

        # Step 4: Create target_df
        n_required_real = self._calculate_required_real_images(n_required)
        self.target_df = self.meta_df[self.meta_df['selected']].sample(
            n=n_required_real,
            random_state=config.dataset_seed
        )
        self.target_df['parent_id'] = None

        # Step 5: Load and process images
        self._load_images()
        self.target_df = self._estimate_backgrounds(self.target_df)

        if getattr(config, 'adjust_images', False):
            self._adjust_hsv()
            self.target_df = self._estimate_backgrounds(self.target_df)

        if getattr(config, 'augment_images', False):
            self._augment_images()
            self.target_df = self._estimate_backgrounds(self.target_df)

        self.target_df['colony_size'] = self.target_df['transformed_image'].apply(
            self._get_size_proportion
        )

        # Fit categorical vocabularies for Strategy 15 conditioning. Done on
        # the full target_df before splits so train/val/test share the same
        # one-hot indices. Stored on config so inference paths (model_loader,
        # factory) can recover them.
        self._fit_categorical_vocabularies()

        # Step 6: Create splits
        self._create_splits()

    def _fit_categorical_vocabularies(self) -> None:
        """Fit day, media, and plate_phys category vocabularies from ``target_df``.

        Stored on ``self`` and mirrored onto ``self.config`` so downstream
        consumers (factory, trainer, model_loader) can size FiLM inputs
        correctly.
        """
        day_values = self.target_df['day'].astype(str).unique().tolist()
        media_values = self.target_df['media'].astype(str).unique().tolist()
        plate_phys_values = (
            self.target_df['plate_phys'].astype(str).unique().tolist()
        )
        # Sort deterministically so the same training data always yields
        # the same one-hot ordering.
        self.day_categories: List[str] = sorted(day_values)
        self.media_categories: List[str] = sorted(media_values)
        self.plate_phys_categories: List[str] = sorted(plate_phys_values)
        # Plate-only vocabulary — the technical nuisance for invariant (Strategy
        # 17) training. Distinct from plate_phys (plate:media:day).
        self.plate_categories: List[str] = sorted(
            self.target_df['plate'].astype(str).unique().tolist()
        )
        # Morphology vocabulary for the optional supervised-contrastive term
        # (labelled rows only; 'unlabelled'/missing are excluded).
        if 'manual_formation' in self.target_df.columns:
            morph = [
                m for m in self.target_df['manual_formation'].astype(str).unique()
                if m not in ('unlabelled', 'nan', 'None', '')
            ]
            self.morphology_categories: List[str] = sorted(morph)
        else:
            self.morphology_categories = []

        # Mirror to config for inference-time reconstruction.
        self.config.day_categories = list(self.day_categories)
        self.config.media_categories = list(self.media_categories)
        self.config.plate_phys_categories = list(self.plate_phys_categories)
        self.config.plate_categories = list(self.plate_categories)
        self.config.morphology_categories = list(self.morphology_categories)
        logger.info(
            f"Fitted categorical vocabularies: day={self.day_categories}, "
            f"media={self.media_categories}, "
            f"plate_phys=[{len(self.plate_phys_categories)} categories], "
            f"plate=[{len(self.plate_categories)} categories], "
            f"morphology={self.morphology_categories}"
        )

    def _ensure_directories(self) -> None:
        """Ensure output directories exist."""
        for path in [
            self.config.analyses_path,
            self.config.models_path,
            self.config.loss_path,
            self.config.images_path,
        ]:
            path.mkdir(parents=True, exist_ok=True)

    def _filenames_to_dataframe(self, filenames: List[str]) -> pd.DataFrame:
        """Convert filenames to metadata DataFrame."""
        data = []
        for idx, fname in enumerate(filenames):
            plate, media, wash, day, replicate, row, col = self._parse_filename(fname)
            record = {
                "id": idx,
                "filename": fname,
                "plate": plate,
                "media": media,
                "wash": wash,
                "day": day,
                "replicate": replicate,
                "row": row,
                "column": col,
                "image_type": 'real'
            }
            data.append(record)

        df = pd.DataFrame(data)
        df['media'] = df['media'].replace({'ctrl': 'control', 'spdr': 'spider'})
        df['day'] = df['day'].fillna('NA')
        df['day'] = df['day'].replace({2.0: 2, 5.0: 5})
        df['plate_phys'] = (
            df['plate'].astype(str) + ':'
            + df['media'].astype(str) + ':'
            + df['day'].astype(str)
        )
        return df

    def _parse_filename(self, filename: str) -> Tuple:
        """Parse colony image filename into components."""
        name = filename.rsplit(".", 1)[0]
        parts = name.split("_")
        plate = parts[0][1:]
        if isinstance(plate, str) and plate.startswith('l'):
            plate = plate[1:]
        media = parts[1]
        condition = parts[2]
        if condition == "wash":
            wash = True
            day = None
        elif condition.startswith("day"):
            wash = False
            day = int(condition[3:])
        else:
            wash = False
            day = None
        replicate_part = parts[3]
        subparts = replicate_part.split("-")
        replicate = int(subparts[0])
        row = int(subparts[1].lstrip("r"))
        column = int(subparts[2].lstrip("c"))
        return plate, media, wash, day, replicate, row, column

    def _merge_metadata(self, metadata: pd.DataFrame) -> None:
        """Merge external metadata with image metadata."""
        metadata.rename(columns={'col': 'column'}, inplace=True)
        self.meta_df['plate'] = self.meta_df['plate'].astype(str)
        metadata['plate'] = metadata['plate'].astype(str)
        self.meta_df = pd.merge(
            self.meta_df, metadata,
            on=['plate', 'row', 'column'],
            how='left'
        )

    def _merge_manually_labelled_images(self) -> None:
        """Merge manually labelled morphology data into meta_df.

        Reads a CSV with columns (file_name, morphology), merges on filename,
        and adds a 'manual_formation' column.  Supports both config attribute
        names ('manually_labelled_path' and 'manual_labels_path').
        """
        if not getattr(self.config, 'merge_manually_labelled', False):
            logger.info("Manual label merge disabled (merge_manually_labelled=False)")
            return

        # Resolve path — accept either attribute name
        labelled_path = getattr(self.config, 'manually_labelled_path', None)
        if labelled_path is None:
            labelled_path = getattr(self.config, 'manual_labels_path', None)
        if labelled_path is not None:
            labelled_path = Path(labelled_path)

        if labelled_path is None or not labelled_path.exists():
            logger.warning(
                f"Manual labels file not found: {labelled_path} — skipping merge"
            )
            return

        df = pd.read_csv(labelled_path)
        logger.info(f"Manual labels: loaded {len(df)} rows from {labelled_path}")

        # Validate expected columns
        required_cols = {'file_name', 'morphology'}
        missing = required_cols - set(df.columns)
        if missing:
            logger.warning(
                f"Manual labels CSV missing columns {missing} — skipping merge. "
                f"Available columns: {list(df.columns)}"
            )
            return

        # Normalize join key
        df = df.rename(columns={'file_name': 'filename'})
        df['filename'] = df['filename'].str.strip()

        n_before = len(self.meta_df)
        result = pd.merge(self.meta_df, df[['filename', 'morphology']], on='filename', how='left')

        # Resolve duplicate id columns created by merge
        if 'id_x' in result.columns:
            result = result.rename(columns={'id_x': 'id'})
        if 'id_y' in result.columns:
            result = result.drop(columns=['id_y'])

        # Standardize output column
        result = result.rename(columns={'morphology': 'manual_formation'})
        result['manual_formation'] = result['manual_formation'].fillna('unlabelled')

        n_labelled = (result['manual_formation'] != 'unlabelled').sum()
        logger.info(
            f"Manual labels: matched {n_labelled}/{len(result)} filenames "
            f"({n_labelled/len(result)*100:.1f}% coverage)"
        )

        self.meta_df = result

    def _select_images(self, n_required: int) -> None:
        """Select images based on configuration."""
        self.meta_df['selected'] = self.meta_df['image_type'] == 'real'

        image_type = getattr(self.config, 'image_type', 'non-washed')
        self.meta_df['selected'] &= self.meta_df['wash'] == (image_type == 'washed')

        restrict_day = getattr(self.config, 'restrict_to_day', None)
        if restrict_day is not None:
            self.meta_df['selected'] &= self.meta_df['day'] == restrict_day

        media_filter = getattr(self.config, 'media_filter', None)
        if media_filter and media_filter != 'all':
            # meta_df['media'] preserves filename case (e.g. 'RPMI', 'YPD')
            # but the UI normalises to lowercase, so match case-insensitively.
            target = media_filter.lower()
            self.meta_df['selected'] &= (
                self.meta_df['media'].astype(str).str.lower() == target
            )
            logger.info(
                "media_filter='%s' applied: %d images selected",
                media_filter, int(self.meta_df['selected'].sum()),
            )

        self.meta_df['rgb_image'] = None
        self.meta_df['hsv_image'] = None
        self.meta_df['transformed_image'] = None

    def _calculate_required_real_images(self, n_required: int) -> int:
        """Calculate number of real images needed based on augmentation."""
        if getattr(self.config, 'augment_images', False):
            strategy = getattr(self.config, 'strategy', 0)
            if strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16):
                return n_required
            arity = getattr(self.config, 'augmentation_arity', 1)
            n_required_real = math.ceil(n_required / arity)
            current_possible = arity * self.meta_df['selected'].sum()
            if current_possible < n_required:
                raise ValueError(
                    f"Arity is not large enough: needed {n_required}, "
                    f"current possible: {current_possible}"
                )
            return n_required_real
        return n_required

    def _load_images(self) -> None:
        """Load and resize images."""
        image_dim = self.config.image_dimension

        for idx, row in self.target_df.iterrows():
            if not row['wash']:
                file_path = self.config.raw_images_path / row['filename']
                try:
                    img = Image.open(file_path)
                    img = img.convert('RGB')
                    img = img.resize((image_dim, image_dim), Image.LANCZOS)
                    # HSV for FiLM / metadata: always from color before grayscale so hue/sat are
                    # meaningful when grayscale=True (encoder still sees luminance-only tensor).
                    self.target_df.at[idx, 'hsv_image'] = img.convert('HSV')
                    if getattr(self.config, 'grayscale', False):
                        img = img.convert('L').convert('RGB')
                        if getattr(self.config, 'grayscale_background_normalize', False):
                            img = apply_grayscale_background_normalize(
                                img, image_dim,
                                target=getattr(self.config, 'grayscale_bg_target', 0.5),
                                border=getattr(self.config, 'grayscale_bg_border', 12),
                            )
                    self.target_df.at[idx, 'rgb_image'] = img
                    self.target_df.at[idx, 'transformed_image'] = self.to_tensor(img)

                except Exception as e:
                    logger.error(f"Error loading image {file_path}: {e}")

    def _estimate_backgrounds(self, df: pd.DataFrame) -> pd.DataFrame:
        """Estimate background HSV values for all images."""
        result_df = df.copy()
        image_dim = self.config.image_dimension

        for index, row in result_df.iterrows():
            if row['hsv_image'] is not None:
                avg_hue, avg_sat, avg_val = self._estimate_background_image(
                    row['hsv_image'], image_dim
                )
                result_df.at[index, 'average_hue'] = avg_hue
                result_df.at[index, 'average_saturation'] = avg_sat
                result_df.at[index, 'average_value'] = avg_val

        return result_df

    @staticmethod
    def _estimate_background_image(img: Image.Image, dim: int) -> Tuple[float, float, float]:
        """
        Estimate background color from image edges.

        Returns HSV values in storage scale (0-255), not conditioning scale (0-1).
        To use for model conditioning, divide by 255.
        """
        hsv_arr = np.array(img)
        mask = _edge_band_mask(dim, dim, border=12)

        hue_values = hsv_arr[..., 0][mask]
        angles = hue_values.astype(float) / 255.0 * 2 * np.pi
        mean_sin = np.mean(np.sin(angles))
        mean_cos = np.mean(np.cos(angles))
        mean_angle = np.arctan2(mean_sin, mean_cos)
        if mean_angle < 0:
            mean_angle += 2 * np.pi
        mean_hue_circular = (mean_angle / (2 * np.pi)) * 255

        mean_saturation = hsv_arr[..., 1][mask].mean()
        mean_value = hsv_arr[..., 2][mask].mean()

        return mean_hue_circular, mean_saturation, mean_value

    @staticmethod
    def estimate_background_image_tensor_helper(
        rgb_tensor_img: torch.Tensor,
        dim: int
    ) -> Tuple:
        """
        Estimate background from tensor image(s).

        Args:
            rgb_tensor_img: RGB tensor with shape [3, H, W] or [B, 3, H, W].
            dim: Image dimension.

        Returns:
            Tuple of (hue, saturation, value) in conditioning scale (0-1),
            ready for direct use as model conditioning input.
        """
        if rgb_tensor_img.dim() == 3:
            pil_rgb = ToPILImage()(rgb_tensor_img)
            pil_hsv = pil_rgb.convert("HSV")
            avg_hue, avg_sat, avg_val = FullDataset._estimate_background_image(pil_hsv, dim)
            return (avg_hue / 255.0, avg_sat / 255.0, avg_val / 255.0)

        elif rgb_tensor_img.dim() == 4:
            hues, sats, vals = [], [], []
            for img in rgb_tensor_img:
                pil_rgb = ToPILImage()(img)
                pil_hsv = pil_rgb.convert("HSV")
                avg_hue, avg_sat, avg_val = FullDataset._estimate_background_image(pil_hsv, dim)
                hues.append(avg_hue)
                sats.append(avg_sat)
                vals.append(avg_val)

            return (
                torch.tensor(hues, dtype=torch.float) / 255,
                torch.tensor(sats, dtype=torch.float) / 255,
                torch.tensor(vals, dtype=torch.float) / 255
            )

        raise ValueError(f"Unexpected tensor dimensions: {rgb_tensor_img.dim()}")

    def _adjust_hsv(self) -> None:
        """Adjust HSV values to target values."""
        max_id = self.target_df['id'].max() if not self.target_df.empty else 0
        adjusted_rows = []

        for _, row in self.target_df.iterrows():
            if row['hsv_image'] is None or row['transformed_image'] is None:
                continue

            adjusted_hsv, adjusted_rgb, adjusted_tensor = self._adjust_row(row)
            if adjusted_hsv is None:
                continue

            adjusted_row = row.copy()
            max_id += 1
            adjusted_row['id'] = max_id
            adjusted_row['image_type'] = 'adjusted'
            adjusted_row['parent_id'] = row['id']
            adjusted_row['hsv_image'] = adjusted_hsv
            adjusted_row['rgb_image'] = adjusted_rgb
            adjusted_row['transformed_image'] = adjusted_tensor

            adjusted_rows.append(adjusted_row)

        if adjusted_rows:
            self.target_df = pd.concat(
                [self.target_df, pd.DataFrame(adjusted_rows)],
                ignore_index=True
            )

    def _adjust_row(self, row: pd.Series) -> Tuple:
        """Adjust a single row's HSV values."""
        hsv_tensor = TF.to_tensor(row['hsv_image'])
        hue_adj, sat_adj, val_adj = self._calculate_hsv_adjustments(row)
        adjusted_hsv = self._adjust_hsv_helper(hsv_tensor, hue_adj, sat_adj, val_adj)
        adjusted_hsv_pil = TF.to_pil_image(adjusted_hsv, mode='HSV')
        adjusted_rgb_pil = adjusted_hsv_pil.convert('RGB')
        adjusted_rgb_tensor = TF.to_tensor(adjusted_rgb_pil)
        return adjusted_hsv_pil, adjusted_rgb_pil, adjusted_rgb_tensor

    def _calculate_hsv_adjustments(self, row: pd.Series) -> Tuple[float, float, float]:
        """Calculate HSV adjustments needed."""
        current_hue = row['average_hue'] / 255
        current_sat = row['average_saturation'] / 255
        current_val = row['average_value'] / 255

        target_hue = getattr(self.config, 'target_hue', current_hue)
        target_sat = getattr(self.config, 'target_saturation', current_sat)
        target_val = getattr(self.config, 'target_value', current_val)

        hue_adj = (target_hue - current_hue) % 1.0
        sat_adj = target_sat - current_sat
        val_adj = target_val - current_val

        return hue_adj, sat_adj, val_adj

    @staticmethod
    def _adjust_hsv_helper(
        hsv_tensor: torch.Tensor,
        hue_factor: float,
        saturation_factor: float,
        value_factor: float
    ) -> torch.Tensor:
        """Apply HSV adjustments to tensor."""
        adjusted = hsv_tensor.clone()
        adjusted[0] = (adjusted[0] + hue_factor) % 1.0
        adjusted[1] = torch.clamp(adjusted[1] + saturation_factor, 0, 1)
        adjusted[2] = torch.clamp(adjusted[2] + value_factor, 0, 1)
        return adjusted

    def _augment_images(self) -> None:
        """Augment images with random HSV perturbations."""
        aug_vars = getattr(self.config, 'augmentation_variables', [])
        spread = getattr(self.config, 'augmentation_spread', 0.1)
        arity = getattr(self.config, 'augmentation_arity', 1)

        max_id = self.target_df['id'].max() if not self.target_df.empty else 0
        augmented_rows = []

        adjust_images = getattr(self.config, 'adjust_images', False)

        for _, row in self.target_df.iterrows():
            if row['hsv_image'] is None:
                continue

            if (not adjust_images and row['image_type'] == 'real') or \
               (adjust_images and row['image_type'] == 'adjusted'):
                for _ in range(arity):
                    augmented_row = row.copy()
                    augmented_row['image_type'] = 'augmented'
                    augmented_row['parent_id'] = row['id']

                    max_id += 1
                    augmented_row['id'] = max_id

                    hsv_arr = self._randomly_perturb_image(
                        row['hsv_image'], aug_vars, spread
                    )
                    pil_hsv = Image.fromarray(hsv_arr, mode='HSV')
                    augmented_row['hsv_image'] = pil_hsv
                    rgb_aug = pil_hsv.convert('RGB')
                    if getattr(self.config, 'grayscale', False):
                        rgb_aug = rgb_aug.convert('L').convert('RGB')
                        if getattr(self.config, 'grayscale_background_normalize', False):
                            image_dim = self.config.image_dimension
                            rgb_aug = apply_grayscale_background_normalize(
                                rgb_aug, image_dim,
                                target=getattr(self.config, 'grayscale_bg_target', 0.5),
                                border=getattr(self.config, 'grayscale_bg_border', 12),
                            )
                    augmented_row['rgb_image'] = rgb_aug
                    augmented_row['transformed_image'] = self.to_tensor(rgb_aug)

                    augmented_rows.append(augmented_row)

        if augmented_rows:
            self.target_df = pd.concat(
                [self.target_df, pd.DataFrame(augmented_rows)],
                ignore_index=True
            )

    @staticmethod
    def _randomly_perturb_image(
        img: Image.Image,
        dims: List[str],
        spread: float
    ) -> np.ndarray:
        """Apply random perturbations to HSV image."""
        hsv_arr = np.array(img, dtype=np.uint8)

        if 'average_hue' in dims:
            epsilon = random.gauss(0, spread)
            new_hue = hsv_arr[..., 0].astype(np.float32) + (epsilon * 255)
            hsv_arr[..., 0] = np.mod(np.round(new_hue), 256).astype(np.uint8)

        if 'average_saturation' in dims:
            epsilon = random.gauss(0, spread)
            new_sat = hsv_arr[..., 1].astype(np.float32) + (epsilon * 255)
            hsv_arr[..., 1] = np.clip(np.round(new_sat), 0, 255).astype(np.uint8)

        if 'average_value' in dims:
            epsilon = random.gauss(0, spread)
            new_val = hsv_arr[..., 2].astype(np.float32) + (epsilon * 255)
            hsv_arr[..., 2] = np.clip(np.round(new_val), 0, 255).astype(np.uint8)

        return hsv_arr

    def _create_splits(self) -> None:
        """Create train/validation/test splits."""
        strategy = getattr(self.config, 'strategy', 0)
        adjust_images = getattr(self.config, 'adjust_images', False)
        augment_images = getattr(self.config, 'augment_images', False)

        tmp_df = self.target_df.copy()

        if strategy in (9.5, 9.6, 9.7, 9.8, 9.9, 13, 14, 15, 16):
            # Denoising-AE direction: when augmenting, iterate over
            # augmented rows so the encoder sees diverse HSV inputs and
            # the loss target (looked up via parent_id) is the canonical
            # real image. This is the only path that produces brightness
            # invariance in the latent.
            if augment_images:
                tmp_df = tmp_df.query("image_type == 'augmented'")
            elif adjust_images:
                tmp_df = tmp_df.query("image_type == 'adjusted'")
            else:
                tmp_df = tmp_df.query("image_type == 'real'")
        elif augment_images:
            tmp_df = tmp_df.query("image_type == 'augmented'")
        elif adjust_images:
            tmp_df = tmp_df.query("image_type == 'adjusted'")

        train_df = tmp_df.sample(
            n=self.config.train_num,
            random_state=self.config.dataset_seed
        )
        remaining = tmp_df.drop(train_df.index)

        val_df = remaining.sample(
            n=self.config.validation_num,
            random_state=self.config.dataset_seed
        )
        remaining = remaining.drop(val_df.index)

        test_df = remaining.sample(
            n=self.config.test_num,
            random_state=self.config.dataset_seed
        )

        self.train_dataset = LearningDataset(self.config, train_df, self.target_df)
        self.validation_dataset = LearningDataset(self.config, val_df, self.target_df)
        self.test_dataset = LearningDataset(self.config, test_df, self.target_df)

    def _get_size_proportion(self, img_tensor: torch.Tensor) -> float:
        """Calculate proportion of pixels above threshold."""
        thresh = getattr(self.config, 'size_threshold', 0.5)
        binary = (img_tensor > thresh).float()
        return (binary.sum() / binary.numel()).item()

    def __len__(self) -> int:
        return len(self.target_df)

    def report(self) -> None:
        """Print dataset statistics."""
        logger.info(f"Number of rows in meta_df: {len(self.meta_df)}")
        logger.info(f"Number of rows in target_df: {len(self.target_df)}")

        for img_type in ['real', 'adjusted', 'augmented']:
            count = len(self.target_df.query(f"image_type == '{img_type}'"))
            logger.info(f"Number of {img_type} images: {count}")

        if self.train_dataset:
            logger.info(f"Training samples: {len(self.train_dataset)}")
        if self.validation_dataset:
            logger.info(f"Validation samples: {len(self.validation_dataset)}")
        if self.test_dataset:
            logger.info(f"Test samples: {len(self.test_dataset)}")

    def save(self, save_path: Path) -> None:
        """Save dataset state to disk."""
        save_path.mkdir(parents=True, exist_ok=True)

        # Save config
        config_path = save_path / 'config.pkl'
        with open(config_path, 'wb') as f:
            pickle.dump(self.config, f)

        # Save metadata (without large image objects)
        clean_df = self.target_df.drop(
            columns=['rgb_image', 'hsv_image', 'transformed_image'],
            errors='ignore'
        )
        clean_df.to_pickle(save_path / 'metadata.pkl')

        # Save split indices
        splits = {
            'train': self.train_dataset.meta_df_subset.index.tolist(),
            'val': self.validation_dataset.meta_df_subset.index.tolist(),
            'test': self.test_dataset.meta_df_subset.index.tolist(),
        }
        with open(save_path / 'splits.pkl', 'wb') as f:
            pickle.dump(splits, f)

        logger.info(f"Dataset saved to {save_path}")

    @classmethod
    def load(cls, save_path: Path) -> 'FullDataset':
        """Load dataset from disk."""
        with open(save_path / 'config.pkl', 'rb') as f:
            config = pickle.load(f)

        with open(save_path / 'splits.pkl', 'rb') as f:
            splits = pickle.load(f)

        meta_df = pd.read_pickle(save_path / 'metadata.pkl')

        dataset = cls.__new__(cls)
        dataset.config = config
        dataset.meta_df = meta_df
        dataset.target_df = meta_df.copy()
        dataset.to_tensor = transforms.ToTensor()

        # Reload images
        for idx, row in dataset.target_df.iterrows():
            try:
                if row['image_type'] == 'real':
                    img_path = config.raw_images_path / row['filename']
                else:
                    img_path = config.augmented_images_path / row['filename']

                img = Image.open(img_path)
                img = img.convert('RGB')
                img = img.resize(
                    (config.image_dimension, config.image_dimension),
                    Image.LANCZOS
                )
                dataset.target_df.at[idx, 'hsv_image'] = img.convert('HSV')
                if getattr(config, 'grayscale', False):
                    img = img.convert('L').convert('RGB')
                    if getattr(config, 'grayscale_background_normalize', False):
                        img = apply_grayscale_background_normalize(
                            img, config.image_dimension,
                            target=getattr(config, 'grayscale_bg_target', 0.5),
                            border=getattr(config, 'grayscale_bg_border', 12),
                        )
                dataset.target_df.at[idx, 'rgb_image'] = img
                dataset.target_df.at[idx, 'transformed_image'] = dataset.to_tensor(img)
            except Exception as e:
                logger.error(f"Error loading {row['filename']}: {e}")

        # Refit Strategy 15 categorical vocabularies from the loaded target_df,
        # unless the config already carries them from training.
        if not getattr(config, 'day_categories', None):
            dataset._fit_categorical_vocabularies()
        else:
            dataset.day_categories = list(config.day_categories)
            dataset.media_categories = list(config.media_categories)

        # Rebuild learning datasets
        dataset.train_dataset = LearningDataset(
            config, dataset.target_df.loc[splits['train']], dataset.target_df
        )
        dataset.validation_dataset = LearningDataset(
            config, dataset.target_df.loc[splits['val']], dataset.target_df
        )
        dataset.test_dataset = LearningDataset(
            config, dataset.target_df.loc[splits['test']], dataset.target_df
        )

        logger.info(f"Dataset loaded from {save_path}")
        return dataset
