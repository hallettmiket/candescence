"""
Purpose: Utility functions for TLV module
Author: Hallett Lab
Date: 2026-01-27

Provides image conversion, visualization, DataFrame handling, and plotting utilities.
"""

import os
import pickle
import re
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import matplotlib.colors as mcolors
import matplotlib.offsetbox as offsetbox
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import psutil
import seaborn as sns
import torch
from PIL import Image
from scipy.ndimage import zoom
from sklearn.cluster import KMeans
from sklearn.preprocessing import OneHotEncoder
from torchvision.transforms import ToPILImage

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.tlv.utilities")


def convert_hsv_transformed_2_rgb(img: torch.Tensor) -> Image.Image:
    """
    Convert HSV tensor to RGB PIL Image.

    Args:
        img: Image tensor in HSV format (C, H, W).

    Returns:
        PIL RGB image.
    """
    hsv_np = img.permute(1, 2, 0).numpy()
    rgb_np = mcolors.hsv_to_rgb(hsv_np)
    rgb_uint8 = (rgb_np * 255).astype(np.uint8)
    pil_rgb_img = Image.fromarray(rgb_uint8, mode='RGB')
    return pil_rgb_img


def convert_rgb_transformed_2_rgb(img: torch.Tensor) -> Image.Image:
    """
    Convert RGB tensor to RGB PIL Image.

    Args:
        img: Image tensor in RGB format (C, H, W).

    Returns:
        PIL RGB image.
    """
    if torch.isnan(img).any() or torch.isinf(img).any():
        logger.warning(
            "convert_rgb_transformed_2_rgb: NaN/inf detected in tensor "
            f"(nan={torch.isnan(img).sum().item()}, inf={torch.isinf(img).sum().item()}). "
            "Replacing with zeros and clamping to [0,1]."
        )
        img = torch.nan_to_num(img, nan=0.0, posinf=1.0, neginf=0.0)
        img = torch.clamp(img, 0.0, 1.0)
    to_pil = ToPILImage(mode='RGB')
    pil_img = to_pil(img)
    return pil_img


def adjust_process_priority(nice_value: int) -> None:
    """
    Adjust the priority of the current process.

    Args:
        nice_value: The nice value to set. Positive values lower priority.
    """
    try:
        pid = os.getpid()
        process = psutil.Process(pid)
        os.nice(nice_value)
        logger.info(f"Process priority adjusted to nice value: {nice_value}")
    except Exception as e:
        logger.error(f"Error adjusting process priority: {e}")


def jeffreys_distance(
    mu1: float,
    sigma1: float,
    mu2: float,
    sigma2: float
) -> float:
    """
    Compute Jeffreys distance (symmetric KL divergence) between two univariate normals.

    Args:
        mu1, sigma1: Mean and std of first distribution.
        mu2, sigma2: Mean and std of second distribution.

    Returns:
        Jeffreys distance as a scalar.
    """
    kl_pq = np.log(sigma2 / sigma1) + (sigma1**2 + (mu1 - mu2)**2) / (2 * sigma2**2) - 0.5
    kl_qp = np.log(sigma1 / sigma2) + (sigma2**2 + (mu1 - mu2)**2) / (2 * sigma1**2) - 0.5
    return kl_pq + kl_qp


def save_dataframe_with_tensors(
    df: pd.DataFrame,
    file_path: Path,
    analyses_dir: Optional[Path] = None
) -> None:
    """
    Save DataFrame containing tensors and images to pickle.

    Args:
        df: DataFrame to save.
        file_path: Output file path.
        analyses_dir: Directory to ensure exists (optional).
    """
    if analyses_dir is not None:
        analyses_dir.mkdir(parents=True, exist_ok=True)

    with open(file_path, 'wb') as f:
        pickle.dump(df, f)
    logger.info(f"DataFrame saved to {file_path}")


def load_dataframe_with_tensors(file_path: Path) -> pd.DataFrame:
    """
    Load DataFrame from pickle file.

    Args:
        file_path: Path to pickle file.

    Returns:
        Loaded DataFrame.
    """
    with open(file_path, 'rb') as f:
        df = pickle.load(f)
    logger.info(f"DataFrame loaded from {file_path}")
    return df


def check_for_file_and_load(file_path: Path) -> Optional[pd.DataFrame]:
    """
    Check if file exists and load if present.

    Args:
        file_path: Path to check.

    Returns:
        Loaded DataFrame or None.
    """
    if file_path.exists():
        logger.info(f"File {file_path} exists. Loading from file.")
        return load_dataframe_with_tensors(file_path)
    return None


def get_latent_vectors(
    dataset: Any,
    indices: List[int],
    encoder: torch.nn.Module,
    device: torch.device
) -> pd.DataFrame:
    """
    Extract latent vectors from dataset using encoder.

    Args:
        dataset: Dataset object with __getitem__ method.
        indices: List of indices to process.
        encoder: Encoder model with encode method.
        device: Device for computation.

    Returns:
        DataFrame with latent vectors and filenames.
    """
    image_filenames = dataset.get_image_filename(indices)
    latent_vectors = []
    image_oh_pairs = [dataset[i] for i in indices]

    with torch.no_grad():
        for img, oh_tensor in image_oh_pairs:
            img = img.to(device)
            oh_tensor = oh_tensor.to(device)

            latent_vector = encoder.encode(img.unsqueeze(0), oh_tensor)
            latent_vector = latent_vector.squeeze().cpu().numpy()
            latent_vectors.append(latent_vector)

    df = pd.DataFrame(
        latent_vectors,
        columns=[f'V{i+1}' for i in range(len(latent_vectors[0]))]
    )
    df['file_name'] = image_filenames

    return df


def dataframe_w_latentvecs(
    metadata_path: Path,
    labeled_data_path: Path,
    dataset: Any,
    indices: List[int],
    model: torch.nn.Module,
    device: torch.device
) -> pd.DataFrame:
    """
    Create DataFrame with latent vectors merged with metadata.

    Args:
        metadata_path: Path to metadata CSV file.
        labeled_data_path: Path to labeled data CSV file.
        dataset: Dataset object.
        indices: Indices to process.
        model: VAE model with encoder.
        device: Device for computation.

    Returns:
        Merged DataFrame with latent vectors and metadata.
    """
    meta_table = pd.read_csv(metadata_path, sep='\t', encoding='ISO-8859-1')
    vectors_w_files = get_latent_vectors(dataset, indices, model.encoder, device)

    # Extract plate number
    plate_numbers = vectors_w_files['file_name'].str.extract(
        r'Pl(\d+)|P(\d+)|Pwt', expand=False
    )
    vectors_w_files['Plate'] = np.where(
        plate_numbers[0].notnull(),
        plate_numbers[0],
        plate_numbers[1]
    )
    vectors_w_files['Plate'] = vectors_w_files['Plate'].fillna(-1).astype(int)

    # Extract medium
    vectors_w_files['medium'] = vectors_w_files['file_name'].str.extract(
        r'(spider|ctrl|spdr|control|serum|RPMI|YPD)', expand=False
    )
    vectors_w_files['medium'] = vectors_w_files['medium'].replace({
        'spdr': 'spider',
        'ctrl': 'control'
    })

    # Extract day
    vectors_w_files['day'] = vectors_w_files['file_name'].str.extract(
        r'day(\d+)', expand=False
    ).astype(int)

    # Extract replicate
    vectors_w_files['replicate'] = vectors_w_files['file_name'].str.extract(
        r'_(\d+)-', expand=False
    ).fillna('-1').astype(int)

    # Extract position
    vectors_w_files['Position'] = vectors_w_files['file_name'].str.extract(
        r'-(.*)\\.', expand=False
    )

    def get_row_col(pos: str) -> str:
        m = re.search(r"r(\d+)-c(\d+)", pos)
        return f"{chr(int(m.group(1)) + 64)}{m.group(2)}" if m else pos

    vectors_w_files['Position'] = vectors_w_files['Position'].apply(get_row_col)

    # Load labeled data
    labeled_data = pd.read_csv(labeled_data_path)
    vectors_w_files = pd.merge(
        vectors_w_files,
        labeled_data[['file_name', 'Morphology']],
        on='file_name',
        how='left'
    )

    # Merge with metadata
    master = pd.merge(
        vectors_w_files,
        meta_table,
        on=["Plate", "Position"],
        how='inner'
    )

    return master


def create_img_scatterplot(
    df: pd.DataFrame,
    reduction_technique: str,
    save_dir: Path,
    exp_name: str,
    after_discriminator: bool = False
) -> None:
    """
    Create scatterplot with images as points.

    Args:
        df: DataFrame with coordinates and file_name column.
        reduction_technique: 'tsne' or 'umap'.
        save_dir: Directory to save plot.
        exp_name: Experiment name for file naming.
        after_discriminator: Whether this is after discriminator training.
    """
    technique = reduction_technique.lower()
    if technique == 'tsne':
        dim1, dim2 = 'tsne-2d-one', 'tsne-2d-two'
    elif technique == 'umap':
        dim1, dim2 = 'umap-2d-one', 'umap-2d-two'
    else:
        raise ValueError(f"Invalid reduction technique: {reduction_technique}")

    fig, ax = plt.subplots(1, figsize=(12, 9))

    for i, row in df.iterrows():
        img = Image.open(row['file_name'])
        img.thumbnail((50, 50))
        img = np.array(img)

        img_offset = offsetbox.OffsetImage(img, zoom=0.5)
        img_box = offsetbox.AnnotationBbox(
            img_offset,
            (row[dim1], row[dim2]),
            box_alignment=(0, 0),
            pad=0,
            frameon=False
        )
        ax.add_artist(img_box)

    ax.set_xlim(df[dim1].min(), df[dim2].max())
    ax.set_ylim(df[dim1].min(), df[dim2].max())

    file_name = f"{exp_name}_{technique}_img-scatter"
    if after_discriminator:
        file_name += "_afterdiscrim"
    file_name += ".jpg"

    save_dir.mkdir(parents=True, exist_ok=True)
    fig.savefig(save_dir / file_name)
    logger.info(f"Plot saved to {save_dir / file_name}")
    plt.show()


def create_img_scatterplot_reconstructed(
    df: pd.DataFrame,
    reduction_technique: str,
    save_dir: Path,
    exp_name: str,
    dataset: Any,
    encoder: torch.nn.Module,
    decoder: torch.nn.Module,
    device: torch.device,
    after_discriminator: bool = False,
    n_samples: Optional[int] = None,
    img_size: Tuple[int, int] = (50, 50),
    zoom_level: float = 0.5
) -> None:
    """
    Create scatterplot with reconstructed images as points.

    Args:
        df: DataFrame with coordinates.
        reduction_technique: 'tsne' or 'umap'.
        save_dir: Directory to save plot.
        exp_name: Experiment name.
        dataset: Dataset object.
        encoder: Encoder model.
        decoder: Decoder model.
        device: Device for computation.
        after_discriminator: Whether after discriminator training.
        n_samples: Number of samples to plot (None for all).
        img_size: Thumbnail size.
        zoom_level: Zoom level for images.
    """
    technique = reduction_technique.lower()
    if technique == 'tsne':
        dim1, dim2 = 'tsne-2d-one', 'tsne-2d-two'
    elif technique == 'umap':
        dim1, dim2 = 'umap-2d-one', 'umap-2d-two'
    else:
        raise ValueError(f"Invalid reduction technique: {reduction_technique}")

    fig, ax = plt.subplots(1, figsize=(12, 9))

    max_samples = min(len(dataset), len(df))
    if max_samples == 0:
        raise ValueError("Both dataset and DataFrame must have at least 1 sample")

    logger.info(f"Plotting {max_samples} samples")

    if n_samples is None:
        indices = range(max_samples)
    else:
        indices = np.random.choice(
            max_samples,
            size=min(n_samples, max_samples),
            replace=False
        )

    encoder.eval()
    decoder.eval()

    for position_idx in indices:
        try:
            img_tensor, oh_tensor = dataset[position_idx]
            row = df.iloc[position_idx]
            x_coord = row[dim1]
            y_coord = row[dim2]

            with torch.no_grad():
                img = img_tensor.unsqueeze(0).to(device)
                oh = oh_tensor.unsqueeze(0).to(device)

                z, embedding, _, _ = encoder(img, oh)
                rec_img = decoder(z, embedding).cpu().squeeze()

            rec_img = rec_img.numpy()
            if rec_img.ndim == 3:
                rec_img = rec_img.transpose(1, 2, 0)
            rec_img = np.clip(rec_img, 0, 1)

            img_box = offsetbox.AnnotationBbox(
                offsetbox.OffsetImage(rec_img, zoom=zoom_level, cmap='gray'),
                (x_coord, y_coord),
                frameon=False
            )
            ax.add_artist(img_box)

        except Exception as e:
            logger.warning(f"Error processing index {position_idx}: {e}")
            continue

    ax.set_xlim(df[dim1].min() - 0.1, df[dim1].max() + 0.1)
    ax.set_ylim(df[dim2].min() - 0.1, df[dim2].max() + 0.1)

    save_dir.mkdir(parents=True, exist_ok=True)
    fname = f"{exp_name}_{technique}_reconstructed_scatter"
    if after_discriminator:
        fname += "_afterdiscrim"

    plt.savefig(save_dir / f"{fname}.jpg", bbox_inches='tight')
    plt.show()


def create_annotated_scatterplot_umap(
    df: pd.DataFrame,
    anno_type: str,
    save_dir: Path,
    exp_name: str,
    after_discriminator: bool = False
) -> None:
    """
    Create UMAP scatterplot colored by annotation type.

    Args:
        df: DataFrame with UMAP coordinates.
        anno_type: Column name for annotation.
        save_dir: Directory to save plot.
        exp_name: Experiment name.
        after_discriminator: Whether after discriminator training.
    """
    fig, ax = plt.subplots(1, figsize=(12, 9))

    palette = sns.color_palette('tab10', n_colors=len(df[anno_type].unique()))
    color_dict = dict(zip(df[anno_type].unique(), palette))

    for key in ['nan', 'NaN', 'unknown']:
        if key in color_dict:
            color_dict[key] = (0.5, 0.5, 0.5)

    sns.scatterplot(
        data=df,
        x='umap-2d-one',
        y='umap-2d-two',
        hue=anno_type,
        ax=ax,
        palette=color_dict
    )

    legend_labels = {
        key: 'Unknown' if key in ['nan', 'NaN', 'unknown'] else key
        for key in df[anno_type].unique()
    }
    for t, l in zip(ax.get_legend().texts, ax.get_legend().get_lines()):
        t.set_text(legend_labels[t.get_text()])

    ax.set_xlim(df['umap-2d-one'].min(), df['umap-2d-one'].max())
    ax.set_ylim(df['umap-2d-two'].min(), df['umap-2d-two'].max())

    save_dir.mkdir(parents=True, exist_ok=True)

    file_name = f"{exp_name}_umap_{anno_type}_scatter_annotated"
    if after_discriminator:
        file_name += "_afterdiscrim"
    file_name += ".jpg"

    try:
        fig.savefig(save_dir / file_name)
        logger.info(f"File saved successfully at {save_dir / file_name}")
    except Exception as e:
        logger.error(f"Failed to save file: {e}")

    plt.show()


def get_one_hot_encoder(categorical_info: List[List]) -> OneHotEncoder:
    """
    Create a one-hot encoder from categorical information.

    Args:
        categorical_info: List of lists containing category values.

    Returns:
        Fitted OneHotEncoder.
    """
    one_hot_encoder = OneHotEncoder(sparse_output=False)

    categorical_info_as_strings = [
        f"{info[0]}_{info[1]}_{info[2]}" for info in categorical_info
    ]
    categorical_info_2d = np.array(categorical_info_as_strings).reshape(-1, 1)

    one_hot_encoder.fit(categorical_info_2d)
    return one_hot_encoder


def get_intensity_onehot_from_edge(
    img_tensor: torch.Tensor,
    intensity_bins: int
) -> torch.Tensor:
    """
    Create one-hot encoding based on edge pixel intensity.

    Args:
        img_tensor: Image tensor (H, W) or (C, H, W).
        intensity_bins: Number of intensity bins.

    Returns:
        One-hot encoded tensor.
    """
    if img_tensor.dim() == 3:
        img_tensor = img_tensor.mean(dim=0)

    top_edge = img_tensor[0, :]
    bottom_edge = img_tensor[-1, :]
    left_edge = img_tensor[1:-1, 0]
    right_edge = img_tensor[1:-1, -1]

    edge_pixels = torch.cat((top_edge, bottom_edge, left_edge, right_edge))
    avg_intensity = edge_pixels.mean().item()

    buckets = np.linspace(0, 1, intensity_bins + 1)[1:]

    bin_id = 1
    for bucket in buckets:
        if avg_intensity <= bucket:
            break
        bin_id += 1

    ids_for_fit = np.arange(1, intensity_bins + 1).reshape(-1, 1)
    encoder = OneHotEncoder(sparse_output=False)
    encoder.fit(ids_for_fit)

    one_hot = encoder.transform(np.array([[bin_id]]))
    one_hot_tensor = torch.from_numpy(one_hot.astype(np.float32))

    return one_hot_tensor


def create_loss_graph(
    *loss_arrays,
    labels: List[str],
    save_dir: Path,
    exp_name: str
) -> None:
    """
    Create loss graph with multiple loss curves.

    Args:
        *loss_arrays: Variable number of loss arrays to plot.
        labels: Labels for each loss array.
        save_dir: Directory to save plot.
        exp_name: Experiment name.
    """
    plt.figure(figsize=(10, 5))

    assert len(loss_arrays) == len(labels)
    line_styles = ['-', '--', ':', '-.']

    for i, loss_array in enumerate(loss_arrays):
        line_style = line_styles[i % len(line_styles)]
        plt.plot(loss_array, label=labels[i], linestyle=line_style)

    plt.yscale('log')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.title('VAE Loss over Epochs')
    plt.legend()

    save_dir.mkdir(parents=True, exist_ok=True)
    file_name = f"{exp_name}_loss-graph.jpg"
    plt.savefig(save_dir / file_name)
    plt.show()


def create_custom_size_onehot(size_index: int, num_sizes: int) -> torch.Tensor:
    """
    Create one-hot tensor for size category.

    Args:
        size_index: Index of size category.
        num_sizes: Total number of size categories.

    Returns:
        One-hot encoded tensor.
    """
    one_hot = torch.zeros(num_sizes)
    one_hot[size_index] = 1
    return one_hot


def save_epoch_loss_plots(
    save_dir: Path,
    exp_name: str,
    disc_epoch_losses: List[float],
    vae_epoch_losses: List[float],
    kl_epoch_losses: List[float],
    ssim_epoch_losses: List[float]
) -> None:
    """
    Save epoch-wise loss plots for discriminator and VAE.

    Args:
        save_dir: Directory to save plot.
        exp_name: Experiment name.
        disc_epoch_losses: Discriminator losses per epoch.
        vae_epoch_losses: VAE total losses per epoch.
        kl_epoch_losses: KL divergence losses per epoch.
        ssim_epoch_losses: SSIM losses per epoch.
    """
    epochs = range(1, len(disc_epoch_losses) + 1)

    plt.figure(figsize=(12, 8))
    plt.plot(epochs, disc_epoch_losses, label='Discriminator BCE Loss')
    plt.plot(epochs, vae_epoch_losses, label='VAE Total Loss')
    plt.plot(epochs, kl_epoch_losses, label='VAE KL Loss')
    plt.plot(epochs, ssim_epoch_losses, label='VAE SSIM Loss')

    plt.title('Losses per Epoch')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.legend()
    plt.tight_layout()

    save_dir.mkdir(parents=True, exist_ok=True)
    file_name = f"{exp_name}_per_epoch_losses-graph.jpg"
    plt.savefig(save_dir / file_name)
    plt.show()


def create_annotated_scatterplot_umap_intensity(
    df: pd.DataFrame,
    save_dir: Path,
    exp_name: str,
    after_discriminator: bool = False
) -> None:
    """
    Create UMAP scatterplot colored by intensity.

    Args:
        df: DataFrame with UMAP coordinates and intensity column.
        save_dir: Directory to save plot.
        exp_name: Experiment name.
        after_discriminator: Whether after discriminator training.
    """
    fig, ax = plt.subplots(figsize=(8, 6))

    cmap = plt.cm.plasma
    norm = plt.Normalize(vmin=df['intensity'].min(), vmax=df['intensity'].max())

    sc = ax.scatter(
        df['umap-2d-one'],
        df['umap-2d-two'],
        c=df['intensity'],
        cmap=cmap,
        norm=norm,
        s=10
    )

    ax.set_title("UMAP Scatterplot")
    ax.set_xlabel("UMAP-1")
    ax.set_ylabel("UMAP-2")
    ax.set_xlim(df['umap-2d-one'].min(), df['umap-2d-one'].max())
    ax.set_ylim(df['umap-2d-two'].min(), df['umap-2d-two'].max())

    fig.colorbar(sc, ax=ax, label='Intensity')
    plt.tight_layout()

    save_dir.mkdir(parents=True, exist_ok=True)
    file_name = f"{exp_name}_umap_intensity_scatter"
    if after_discriminator:
        file_name += "_afterdiscrim"
    file_name += ".jpg"

    plt.savefig(save_dir / file_name, dpi=300, bbox_inches='tight', format='jpg')
    plt.show()
    plt.close()

    logger.info(f"Plot saved to: {save_dir / file_name}")


def create_separate_annotated_scatterplot_umap_morphology(
    df: pd.DataFrame,
    anno_type: str,
    save_dir: Path,
    exp_name: str,
    after_discriminator: bool = False
) -> None:
    """
    Create separate UMAP scatterplots for each morphology category.

    Args:
        df: DataFrame with UMAP coordinates and morphology column.
        anno_type: Column name for annotation.
        save_dir: Directory to save plot.
        exp_name: Experiment name.
        after_discriminator: Whether after discriminator training.
    """
    categories = df[anno_type].dropna().unique()
    n_rows = int(len(categories) / 2) + (len(categories) % 2)

    fig, axes = plt.subplots(n_rows, 2, figsize=(20, 5 * n_rows))
    axes = axes.flatten()

    palette = sns.color_palette("tab10", n_colors=len(categories))
    color_dict = dict(zip(categories, palette))

    if 'No Data Available' in color_dict:
        color_dict['No Data Available'] = (0.5, 0.5, 0.5)

    for i, category in enumerate(categories):
        ax = axes[i]
        subset = df[df[anno_type] == category]

        sns.scatterplot(
            data=subset,
            x='umap-2d-one',
            y='umap-2d-two',
            ax=ax,
            label=category,
            color=color_dict[category],
            s=10
        )

        ax.set_title(f"{anno_type}: {category}")
        ax.set_xlim(df['umap-2d-one'].min(), df['umap-2d-one'].max())
        ax.set_ylim(df['umap-2d-two'].min(), df['umap-2d-two'].max())

    if len(categories) % 2 != 0:
        axes[-1].set_visible(False)

    plt.tight_layout()

    save_dir.mkdir(parents=True, exist_ok=True)
    file_name = f"{exp_name}_umap_{anno_type}_scatter"
    if after_discriminator:
        file_name += "_afterdiscrim"
    file_name += ".jpg"

    plt.savefig(save_dir / file_name, dpi=300, bbox_inches='tight')
    plt.show()
    plt.close()

    logger.info(f"Plot saved to: {save_dir / file_name}")
