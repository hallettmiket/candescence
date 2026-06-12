"""Latent-space trajectory visualization (PCA + generative interpolation).

Ported from candescence_master ``vae/diffusion_utilities.py::visualize_rich_interpolation``
and adapted for ``TLVModelWrapper``. Produces the standard "trajectory in
semantic space" panel used to qualitatively assess whether the latent space
is smooth between two morphologies: top row shows start image, PCA scatter
of all latents with the start->end trajectory line overlaid, and end image;
bottom row shows the decoded interpolation strip.
"""
from __future__ import annotations

from pathlib import Path
from typing import List, Optional, Tuple, Union

import matplotlib.pyplot as plt
import numpy as np
import torch
from matplotlib.figure import Figure
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from torchvision.utils import make_grid

from candescence.interface.model_loader import TLVModelWrapper


def _to_01(img: torch.Tensor) -> torch.Tensor:
    """Bring a tensor image into [0, 1] for matplotlib display."""
    img = img.detach().cpu()
    if img.min() < 0 or img.max() > 1:
        img = (img + 1) / 2.0
    return torch.clamp(img, 0.0, 1.0)


def _interp_skip(
    skip_a: List[torch.Tensor],
    skip_b: List[torch.Tensor],
    alpha: float,
) -> List[torch.Tensor]:
    return [(1 - alpha) * a + alpha * b for a, b in zip(skip_a, skip_b)]


def _encode_with_skip(
    model: TLVModelWrapper,
    x: torch.Tensor,
    cond: Optional[torch.Tensor],
) -> Tuple[torch.Tensor, List[torch.Tensor], Optional[Tuple[int, ...]]]:
    """Encode a single batch and return (mu, skip_connections_cpu, fmap_shape).

    Skip connections are pulled off the wrapper's cache and copied to CPU so
    the next encode call doesn't overwrite them. ``fmap_shape`` is captured
    likewise. For strategies without skip connections, ``skip`` will be an
    empty list and ``fmap_shape`` will be ``None``.
    """
    z, mu, _ = model.encode_full(x, cond)
    skip = model.get_last_skip()
    fmap_shape = getattr(model, "_last_fmap_shape", None)
    skip_cpu: List[torch.Tensor] = (
        [s.detach().cpu() for s in skip] if skip is not None else []
    )
    return mu.detach().cpu(), skip_cpu, fmap_shape


def visualize_rich_interpolation(
    model: TLVModelWrapper,
    images: torch.Tensor,
    conds: Optional[torch.Tensor] = None,
    n_steps: int = 10,
    start_idx: Optional[int] = None,
    end_idx: Optional[int] = None,
    batch_size: int = 32,
    standardize_latents: bool = True,
    seed: Optional[int] = 42,
    save_path: Optional[Union[str, Path]] = None,
    title_suffix: Optional[str] = None,
) -> Tuple[Figure, dict]:
    """Render a PCA scatter + interpolation strip for a TLV VAE wrapper.

    Parameters
    ----------
    model
        ``TLVModelWrapper`` (from ``candescence.interface.model_loader``).
    images
        ``(N, 3, H, W)`` float tensor on CPU. Encoder normalisation must
        already match training (e.g. greyscale conversion done upstream).
    conds
        ``(N, cond_dim)`` float tensor of conditioning vectors on the
        0-1 scale, or ``None`` for unconditional models.
    n_steps
        Number of interpolated samples between start and end (inclusive).
    start_idx, end_idx
        Indices into ``images``. If either is None they are sampled
        uniformly at random (using ``seed``).
    batch_size
        Encoder batch size.
    standardize_latents
        If True, fit StandardScaler on latents before PCA.
    save_path
        Optional path to write a PNG (parents created automatically).

    Returns
    -------
    (fig, info) where ``info`` carries the chosen indices and PCA coords
    so callers can chain other plots from the same projection.
    """
    device = model.device

    n = images.shape[0]
    if n < 2:
        raise ValueError(f"Need >= 2 images, got {n}")

    cond_dim = conds.shape[1] if conds is not None else 0

    # Encode all images, capturing per-image mu and skip connections.
    mu_chunks: List[torch.Tensor] = []
    skip_chunks: List[List[torch.Tensor]] = []
    fmap_shape: Optional[Tuple[int, ...]] = None
    has_skip = True

    for i in range(0, n, batch_size):
        x = images[i:i + batch_size].to(device)
        c = conds[i:i + batch_size].to(device) if conds is not None else None
        mu_b, skip_b, fmap_b = _encode_with_skip(model, x, c)
        mu_chunks.append(mu_b)
        if not skip_b:
            has_skip = False
        else:
            skip_chunks.append(skip_b)
        if fmap_b is not None:
            fmap_shape = tuple(fmap_b)

    Z = torch.cat(mu_chunks, dim=0).numpy()  # (N, latent_dim)

    skip_levels: List[torch.Tensor] = []
    if has_skip and skip_chunks:
        n_levels = len(skip_chunks[0])
        skip_levels = [
            torch.cat([batch_skip[level] for batch_skip in skip_chunks], dim=0)
            for level in range(n_levels)
        ]

    # PCA layout (always fit here — keeps the function self-contained).
    if standardize_latents:
        scaler = StandardScaler()
        Z_for_pca = scaler.fit_transform(Z)
    else:
        Z_for_pca = Z
    pca = PCA(n_components=2)
    coords = pca.fit_transform(Z_for_pca)  # (N, 2)

    # Choose endpoints.
    if start_idx is None or end_idx is None:
        rng = np.random.default_rng(seed)
        pair = rng.choice(n, size=2, replace=False)
        if start_idx is None:
            start_idx = int(pair[0])
        if end_idx is None:
            end_idx = int(pair[1])

    z_start = torch.from_numpy(Z[start_idx]).float()
    z_end = torch.from_numpy(Z[end_idx]).float()

    img_start = _to_01(images[start_idx])
    img_end = _to_01(images[end_idx])

    skip_start: Optional[List[torch.Tensor]] = None
    skip_end: Optional[List[torch.Tensor]] = None
    if skip_levels:
        skip_start = [s[start_idx:start_idx + 1] for s in skip_levels]
        skip_end = [s[end_idx:end_idx + 1] for s in skip_levels]

    cond_start = None
    cond_end = None
    if conds is not None:
        cond_start = conds[start_idx:start_idx + 1].to(device).float()
        cond_end = conds[end_idx:end_idx + 1].to(device).float()

    # Decode interpolation. Linear interp in latent, skip, and cond space.
    alphas = torch.linspace(0, 1, steps=n_steps)
    interp_imgs: List[torch.Tensor] = []
    # fmap_shape may need to be set on the wrapper for decode_with_skip.
    if fmap_shape is not None:
        model._last_fmap_shape = fmap_shape

    for alpha in alphas:
        a = float(alpha)
        z_mix = ((1 - a) * z_start + a * z_end).unsqueeze(0).to(device)

        cond_mix = None
        if cond_start is not None and cond_end is not None:
            cond_mix = (1 - a) * cond_start + a * cond_end

        if skip_start is not None and skip_end is not None:
            skip_mix = [
                ((1 - a) * sa + a * sb).to(device)
                for sa, sb in zip(skip_start, skip_end)
            ]
            gen = model.decode_with_skip(z_mix, skip_mix, cond_mix)
        else:
            gen = model.decode(z_mix, cond_mix)

        interp_imgs.append(_to_01(gen))

    # Layout.
    fig = plt.figure(figsize=(12, 6))
    gs = fig.add_gridspec(2, 4, height_ratios=[2, 1])

    coord_start = coords[start_idx]
    coord_end = coords[end_idx]

    ax_start = fig.add_subplot(gs[0, 0])
    ax_start.imshow(img_start.permute(1, 2, 0).numpy(), cmap="gray")
    ax_start.set_title(
        f"START (Idx {start_idx})\n"
        f"PC1,PC2: ({coord_start[0]:.1f}, {coord_start[1]:.1f})",
        fontsize=10,
        color="blue",
    )
    ax_start.axis("off")

    ax_end = fig.add_subplot(gs[0, 3])
    ax_end.imshow(img_end.permute(1, 2, 0).numpy(), cmap="gray")
    ax_end.set_title(
        f"END (Idx {end_idx})\n"
        f"PC1,PC2: ({coord_end[0]:.1f}, {coord_end[1]:.1f})",
        fontsize=10,
        color="red",
    )
    ax_end.axis("off")

    ax_scatter = fig.add_subplot(gs[0, 1:3])
    ax_scatter.scatter(
        coords[:, 0], coords[:, 1],
        c="lightgrey", alpha=0.4, s=8, label="Dataset",
    )
    ax_scatter.plot(
        [coord_start[0], coord_end[0]],
        [coord_start[1], coord_end[1]],
        "k--", linewidth=2, label="Interpolation Path",
    )
    ax_scatter.scatter(
        coord_start[0], coord_start[1],
        c="blue", s=140, edgecolors="k", label="Start", zorder=5,
    )
    ax_scatter.scatter(
        coord_end[0], coord_end[1],
        c="red", s=140, edgecolors="k", label="End", zorder=5,
    )
    title = "Trajectory in Semantic Space (PCA Projection)"
    if title_suffix:
        title = f"{title} — {title_suffix}"
    ax_scatter.set_title(title)
    ax_scatter.set_xlabel(f"PC1 ({pca.explained_variance_ratio_[0] * 100:.1f}%)")
    ax_scatter.set_ylabel(f"PC2 ({pca.explained_variance_ratio_[1] * 100:.1f}%)")
    ax_scatter.grid(True, alpha=0.3)
    ax_scatter.legend(loc="best")

    ax_strip = fig.add_subplot(gs[1, :])
    grid_tensor = make_grid(torch.cat(interp_imgs, dim=0), nrow=n_steps, padding=2)
    ax_strip.imshow(grid_tensor.permute(1, 2, 0).numpy())
    ax_strip.set_title(f"Generative Interpolation ({n_steps} steps)")
    ax_strip.axis("off")

    plt.tight_layout()

    if save_path is not None:
        save_path = Path(save_path)
        save_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(save_path, dpi=300, bbox_inches="tight")

    info = {
        "start_idx": start_idx,
        "end_idx": end_idx,
        "coords": coords,
        "explained_variance_ratio": pca.explained_variance_ratio_,
    }
    return fig, info
