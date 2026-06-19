"""
Purpose: Loss functions for VAE training
Author: Hallett Lab
Date: 2026-01-27

Provides various loss functions including KL divergence, LPIPS, SSIM, and MSE
for training variational autoencoders.
"""

from typing import List, Optional, Tuple, Union

import lpips
import torch
import torch.nn as nn
import torch.nn.functional as F
from piqa import SSIM
from torchmetrics.functional.regression import log_cosh_error

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.tlv.losses")


def get_kl_weight(
    epoch: int,
    cycle_length: int,
    max_weight: float = 1e-3,
    min_weight: float = 0
) -> float:
    """
    Calculate cyclical KL weight for beta-VAE training.

    Args:
        epoch: Current training epoch.
        cycle_length: Number of epochs per cycle.
        max_weight: Maximum KL weight value.
        min_weight: Minimum KL weight value.

    Returns:
        KL weight for current epoch.
    """
    cycle_pos = (epoch % cycle_length) + 1
    half_cycle = cycle_length // 2

    if cycle_pos <= half_cycle:
        kl_weight = min_weight + (max_weight - min_weight) * (cycle_pos / half_cycle)
    else:
        kl_weight = max_weight - (max_weight - min_weight) * ((cycle_pos - half_cycle) / half_cycle)

    return kl_weight


def compute_kl_divergence(
    mu: Union[torch.Tensor, List[torch.Tensor]],
    logvar: Union[torch.Tensor, List[torch.Tensor]],
    arguments: dict
) -> torch.Tensor:
    """
    Compute KL divergence for hierarchical or single latent variables.

    Args:
        mu: Either a single mean tensor or a list of mean tensors.
        logvar: Either a single log variance tensor or a list of log variance tensors.
        arguments: Dictionary containing model configuration.

    Returns:
        Total KL divergence as a scalar tensor.
    """
    if arguments.get('strategy') == 13:
        weights = arguments.get('kl_strategy_13_weights', [1.0] * len(mu))
    else:
        weights = [1.0] * len(mu) if isinstance(mu, list) else [1.0]

    if isinstance(mu, list) and isinstance(logvar, list):
        kl_loss = 0.0
        for mu_i, logvar_i, weight in zip(mu, logvar, weights):
            kl = -0.5 * torch.sum(1 + logvar_i - mu_i.pow(2) - torch.exp(logvar_i), dim=1)
            kl_loss += weight * kl.mean()
        return kl_loss.sum()

    elif isinstance(mu, torch.Tensor) and isinstance(logvar, torch.Tensor):
        kl_loss = -0.5 * torch.sum(1 + logvar - mu.pow(2) - torch.exp(logvar), dim=1)
        return kl_loss.mean()

    else:
        raise TypeError("mu and logvar must either be both tensors or both lists of tensors.")


def get_kl_recon_loss(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    z: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    arguments: dict,
    epoch: int,
    loss_model: nn.Module
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute combined KL divergence and reconstruction loss.

    Args:
        x: Original input images.
        x_hat: Reconstructed images.
        z: Latent representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.
        loss_model: Perceptual loss model (LPIPS or SSIM).

    Returns:
        Tuple of (total_loss, kl_loss, reconstruction_loss).
    """
    loss_function = arguments.get('vae_loss_function', 'mse')

    if loss_function == 'mse':
        return get_kl_mse_loss(x, x_hat, z, mu, logvar, arguments, epoch)
    elif loss_function == 'lpips':
        return get_kl_lpips_loss(x, x_hat, z, mu, logvar, arguments, epoch, loss_model)
    elif loss_function == 'ssim':
        return get_kl_ssim_loss(x, x_hat, z, mu, logvar, arguments, epoch, loss_model)
    else:
        raise ValueError(f"Invalid loss function: {loss_function}")


def cond_loss(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    arguments: dict,
    estimate_fn: callable
) -> torch.Tensor:
    """
    Compute conditional loss based on image properties.

    Args:
        x: Original input images.
        x_hat: Reconstructed images.
        arguments: Model configuration dictionary.
        estimate_fn: Function to estimate image properties (from FullDataset).

    Returns:
        Weighted conditional loss.
    """
    image_dim = arguments.get('image_dimension')
    x_estimates = estimate_fn(x, image_dim)
    x_hat_estimates = estimate_fn(x_hat, image_dim)

    loss_components = []
    conditional_vars = arguments.get('conditional_variables', [])
    # Grayscale tensors have no meaningful hue/sat in PIL HSV; FiLM still uses color HSV from
    # the dataset. Skip hue/sat terms so cond_loss does not penalize arbitrary angles.
    grayscale = bool(arguments.get('grayscale', False))

    if 'average_hue' in conditional_vars and not grayscale:
        loss_components.append((x_estimates[0] - x_hat_estimates[0]) ** 2)
    if 'average_saturation' in conditional_vars and not grayscale:
        loss_components.append((x_estimates[1] - x_hat_estimates[1]) ** 2)
    if 'average_value' in conditional_vars:
        loss_components.append((x_estimates[2] - x_hat_estimates[2]) ** 2)

    if loss_components:
        total_loss = sum(loss_components)
    else:
        total_loss = torch.zeros(x.shape[0], device=x.device)

    # Guard against NaN/inf from estimate_fn (e.g. when x_hat contains bad values)
    if torch.is_tensor(total_loss) and (torch.isnan(total_loss).any() or torch.isinf(total_loss).any()):
        logger.warning("cond_loss: NaN/inf detected in conditional loss, replacing with zero")
        total_loss = torch.nan_to_num(total_loss, nan=0.0, posinf=0.0, neginf=0.0)

    avg_total_loss = total_loss.mean()
    adjusted_loss = avg_total_loss * arguments.get('conditional_loss_weight', 1.0)

    return adjusted_loss


def get_kl_recon_cond_loss(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    z: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    arguments: dict,
    epoch: int,
    loss_model: nn.Module,
    estimate_fn: callable
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute combined KL, reconstruction, and conditional loss.

    Args:
        x: Original input images.
        x_hat: Reconstructed images.
        z: Latent representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.
        loss_model: Perceptual loss model.
        estimate_fn: Function to estimate image properties.

    Returns:
        Tuple of (total_loss, kl_loss, recon_loss, cond_loss).
    """
    c_loss = cond_loss(x, x_hat, arguments, estimate_fn)

    total_loss, kl_loss, recon_loss = get_kl_recon_loss(
        x, x_hat, z, mu, logvar, arguments, epoch, loss_model
    )

    total_loss = total_loss + c_loss

    return total_loss, kl_loss, recon_loss, c_loss


def get_kl_recon_skip_cond_loss(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    z: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    from_skip: List[torch.Tensor],
    to_skip: List[torch.Tensor],
    arguments: dict,
    epoch: int,
    loss_model: nn.Module,
    estimate_fn: callable
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute combined KL, reconstruction, skip connection, and conditional loss.

    Args:
        x: Original input images.
        x_hat: Reconstructed images.
        z: Latent representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        from_skip: List of skip connection tensors from encoder.
        to_skip: List of skip connection tensors to decoder.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.
        loss_model: Perceptual loss model.
        estimate_fn: Function to estimate image properties.

    Returns:
        Tuple of (total_loss, kl_loss, recon_loss, cond_loss, skip_loss).
    """
    c_loss = cond_loss(x, x_hat, arguments, estimate_fn)

    skip_weight = arguments.get('skip_weight', 1.0)
    skip_losses = [F.mse_loss(from_s, to_s) for from_s, to_s in zip(from_skip, to_skip)]
    skip_connection_loss = sum(skip_losses) * skip_weight

    total_loss, kl_loss, recon_loss = get_kl_recon_loss(
        x, x_hat, z, mu, logvar, arguments, epoch, loss_model
    )

    total_loss = total_loss + c_loss + skip_connection_loss

    return total_loss, kl_loss, recon_loss, c_loss, skip_connection_loss


class SSIMLoss(SSIM):
    """SSIM-based loss (1 - SSIM)."""

    def forward(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return 1.0 - super().forward(x, y)


def get_kl_ssim_loss(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    z: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    arguments: dict,
    epoch: int,
    ssim_model: nn.Module
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute KL divergence and SSIM reconstruction loss.

    Args:
        x: Original input images.
        x_hat: Reconstructed images.
        z: Latent representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.
        ssim_model: SSIM loss model.

    Returns:
        Tuple of (total_loss, kl_loss, ssim_loss).
    """
    ssim_weight = arguments.get('SSIM_weight', 1.0)
    max_kl_weight = arguments.get('kl_weight', 1.0)
    cycle_length = arguments.get('cycle_length', 10)

    ssim_loss = ssim_model(x, x_hat)
    scaled_ssim_loss = ssim_loss * ssim_weight

    dim_wise_kl_loss = compute_kl_divergence(mu, logvar, arguments)
    kl_weight = get_kl_weight(epoch, cycle_length, max_kl_weight)
    scaled_dim_wise_kl_loss = dim_wise_kl_loss * kl_weight

    total_loss = scaled_ssim_loss + scaled_dim_wise_kl_loss

    logger.debug(f"SSIM Loss (Unscaled/Scaled): {ssim_loss}/{scaled_ssim_loss}")
    logger.debug(f"KL Loss (Unscaled/Scaled): {dim_wise_kl_loss}/{scaled_dim_wise_kl_loss}")
    logger.debug(f"KL Weight (scheduled): {kl_weight}")
    logger.debug(f"Total Loss: {total_loss}")

    return total_loss, scaled_dim_wise_kl_loss, scaled_ssim_loss


def get_kl_mse_loss_tendril(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    arguments: dict,
    epoch: int
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute KL divergence and MSE loss for tendril VAE.

    Args:
        x: Original input (latent representation).
        x_hat: Reconstructed representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.

    Returns:
        Tuple of (total_loss, kl_loss, mse_loss).
    """
    mse_weight = arguments.get('tendril_MSE_weight', 1.0)
    kl_weight = arguments.get('tendril_KL_weight', 1.0)

    kl_loss = compute_kl_divergence(mu, logvar, arguments) * kl_weight
    mse_loss = torch.mean((x - x_hat) ** 2) * mse_weight

    total_loss = mse_loss + kl_loss

    logger.debug(f"Weighted MSE Loss: {mse_loss.item()}")
    logger.debug(f"Weighted KL Loss: {kl_loss.item()}")

    return total_loss, kl_loss, mse_loss


def get_kl_log_cosh_loss_tendril(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    arguments: dict,
    epoch: int
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute KL divergence and log-cosh loss for tendril VAE.

    Args:
        x: Original input (latent representation).
        x_hat: Reconstructed representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.

    Returns:
        Tuple of (total_loss, kl_loss, log_cosh_loss).
    """
    log_cosh_weight = arguments.get('tendril_log_cosh_weight', 1.0)
    kl_weight = arguments.get('tendril_KL_weight', 1.0)

    kl_loss = compute_kl_divergence(mu, logvar, arguments) * kl_weight

    x_flat = torch.flatten(x, start_dim=1)
    x_hat_flat = torch.flatten(x_hat, start_dim=1)
    log_cosh_loss = log_cosh_error(x_flat, x_hat_flat) * log_cosh_weight

    log_cosh_loss_scalar = log_cosh_loss.mean()
    total_loss = log_cosh_loss_scalar + kl_loss

    logger.debug(f"Weighted LOG-COSH Loss: {total_loss.item()}")
    logger.debug(f"Weighted KL Loss: {kl_loss.item()}")

    return total_loss, kl_loss, log_cosh_loss_scalar


# ===========================================================================
# VFAE-style nuisance-invariance penalties (Strategy 17 / Invariant Tendril VAE)
# ---------------------------------------------------------------------------
# These act on a tendril's latent ``mu`` to make it statistically independent
# of technical nuisances (plate, background H/S/V), optionally with a
# supervised-contrastive term that keeps morphology structure. They are added
# to the tendril loss when the corresponding weights are > 0; with all weights
# 0 they are never called and the tendril trains exactly as before.
# ===========================================================================


def _rbf_kernel(x: torch.Tensor) -> torch.Tensor:
    """RBF (Gaussian) kernel matrix with the median-distance heuristic bandwidth."""
    sq = torch.cdist(x, x) ** 2  # (n, n) squared Euclidean distances
    with torch.no_grad():
        pos = sq[sq > 0]
        med = torch.median(pos) if pos.numel() > 0 else torch.tensor(
            1.0, device=x.device, dtype=x.dtype
        )
    gamma = 1.0 / (med + 1e-8)
    return torch.exp(-gamma * sq)


def _hsic(k: torch.Tensor, ll: torch.Tensor) -> torch.Tensor:
    """Biased empirical HSIC between two kernel matrices (tr(HKH·L) / (n-1)^2)."""
    n = k.shape[0]
    kc = k - k.mean(0, keepdim=True) - k.mean(1, keepdim=True) + k.mean()
    return (kc * ll).sum() / (max(n - 1, 1) ** 2)


def hsic_penalty(z: torch.Tensor, s: torch.Tensor, categorical: bool) -> torch.Tensor:
    """Normalised HSIC dependence between latent ``z`` and nuisance ``s`` (in [0, 1]).

    ``s`` is a one-hot matrix (``categorical=True`` → delta/linear kernel) or a
    continuous matrix (``categorical=False`` → RBF kernel). 0 means independent.
    """
    if z.shape[0] < 4:
        return z.new_zeros(())
    k = _rbf_kernel(z)
    ll = (s @ s.t()) if categorical else _rbf_kernel(s)
    hsic_zs = _hsic(k, ll)
    denom = torch.sqrt(torch.clamp(_hsic(k, k) * _hsic(ll, ll), min=0.0)) + 1e-8
    return hsic_zs / denom


def mmd_penalty(z: torch.Tensor, group_onehot: torch.Tensor) -> torch.Tensor:
    """Mean RBF-MMD² between each group's latent distribution and the pooled one.

    A simpler alternative to HSIC for low-cardinality categorical nuisances.
    Groups with fewer than 2 members in the batch are skipped.
    """
    if z.shape[0] < 4:
        return z.new_zeros(())
    k = _rbf_kernel(z)
    n = z.shape[0]
    pooled = k.mean()
    total = z.new_zeros(())
    n_groups = 0
    labels = group_onehot.argmax(dim=1)
    for g in torch.unique(labels):
        mask = labels == g
        ng = int(mask.sum())
        if ng < 2:
            continue
        kgg = k[mask][:, mask].mean()
        kgp = k[mask].mean()
        total = total + (kgg - 2 * kgp + pooled)
        n_groups += 1
    return total / n_groups if n_groups > 0 else z.new_zeros(())


def supcon_penalty(
    z: torch.Tensor,
    labels: torch.Tensor,
    mask: torch.Tensor,
    temperature: float = 0.1,
) -> torch.Tensor:
    """Supervised-contrastive loss over labelled rows (``mask``).

    Pulls same-label latents together / pushes different-label apart. Returns 0
    when there are too few labelled rows or fewer than two distinct labels.
    """
    idx = mask.nonzero(as_tuple=False).flatten()
    if idx.numel() < 2:
        return z.new_zeros(())
    zl = F.normalize(z[idx], dim=1)
    yl = labels[idx]
    if torch.unique(yl).numel() < 2:
        return z.new_zeros(())
    sim = zl @ zl.t() / temperature
    sim = sim - sim.max(dim=1, keepdim=True).values.detach()
    n = zl.shape[0]
    self_mask = torch.eye(n, dtype=torch.bool, device=z.device)
    exp_sim = torch.exp(sim).masked_fill(self_mask, 0.0)
    log_prob = sim - torch.log(exp_sim.sum(dim=1, keepdim=True) + 1e-8)
    pos = (yl.unsqueeze(0) == yl.unsqueeze(1)) & ~self_mask
    pos_counts = pos.sum(dim=1)
    valid = pos_counts > 0
    if valid.sum() == 0:
        return z.new_zeros(())
    mean_log_prob_pos = (pos.float() * log_prob).sum(dim=1)[valid] / pos_counts[valid]
    return -mean_log_prob_pos.mean()


def tendril_invariance_loss(
    mu: torch.Tensor,
    nuisance: dict,
    arguments: dict,
) -> Tuple[torch.Tensor, dict]:
    """Assemble the VFAE-style invariance penalty for one tendril batch.

    ``nuisance`` carries (any of) ``'plate'`` (one-hot), ``'hsv'`` (N×3) and
    ``'morph'`` (N int label, ``-1`` = unlabelled). Returns ``(loss, parts)``
    where ``parts`` are detached floats for logging.
    """
    kernel = arguments.get('tendril_invariance_kernel', 'hsic')
    w_plate = float(arguments.get('tendril_invariance_plate_weight', 0.0) or 0.0)
    w_hsv = float(arguments.get('tendril_invariance_hsv_weight', 0.0) or 0.0)
    w_pheno = float(arguments.get('tendril_invariance_pheno_weight', 0.0) or 0.0)

    total = mu.new_zeros(())
    parts: dict = {}

    plate = nuisance.get('plate')
    if w_plate > 0 and plate is not None:
        p = mmd_penalty(mu, plate) if kernel == 'mmd' else hsic_penalty(mu, plate, True)
        parts['plate'] = float(p.detach())
        total = total + w_plate * p

    hsv = nuisance.get('hsv')
    if w_hsv > 0 and hsv is not None:
        h = hsic_penalty(mu, hsv, False)
        parts['hsv'] = float(h.detach())
        total = total + w_hsv * h

    morph = nuisance.get('morph')
    if w_pheno > 0 and morph is not None:
        labelled = morph >= 0
        sc = supcon_penalty(mu, morph, labelled)
        parts['pheno'] = float(sc.detach())
        total = total + w_pheno * sc

    return total, parts


def get_kl_mse_loss(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    z: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    arguments: dict,
    epoch: int
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute KL divergence and MSE reconstruction loss.

    Args:
        x: Original input images.
        x_hat: Reconstructed images.
        z: Latent representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.

    Returns:
        Tuple of (total_loss, kl_loss, mse_loss).
    """
    mse_weight = arguments.get('MSE_weight', 1.0)
    max_kl_weight = arguments.get('kl_weight', 1.0)
    cycle_length = arguments.get('cycle_length', 10)

    dim_wise_kl_loss = compute_kl_divergence(mu, logvar, arguments)
    kl_weight = get_kl_weight(epoch, cycle_length, max_kl_weight)
    scaled_dim_wise_kl_loss = dim_wise_kl_loss * kl_weight

    mse_loss = torch.mean((x - x_hat) ** 2) * mse_weight

    total_loss = mse_loss + scaled_dim_wise_kl_loss

    logger.debug(f"MSE Loss: {mse_loss.item()}")
    logger.debug(f"KL Loss (Unscaled/Scaled): {dim_wise_kl_loss.item()}/{scaled_dim_wise_kl_loss.item()}")
    logger.debug(f"KL Weight (scheduled): {kl_weight}")
    logger.debug(f"Total Loss: {total_loss.item()}")

    return total_loss, scaled_dim_wise_kl_loss, mse_loss


def get_kl_lpips_loss(
    x: torch.Tensor,
    x_hat: torch.Tensor,
    z: torch.Tensor,
    mu: torch.Tensor,
    logvar: torch.Tensor,
    arguments: dict,
    epoch: int,
    lpips_model: nn.Module
) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Compute KL divergence and LPIPS perceptual reconstruction loss.

    Args:
        x: Original input images.
        x_hat: Reconstructed images.
        z: Latent representation.
        mu: Mean of latent distribution.
        logvar: Log variance of latent distribution.
        arguments: Model configuration dictionary.
        epoch: Current training epoch.
        lpips_model: LPIPS perceptual loss model.

    Returns:
        Tuple of (total_loss, kl_loss, lpips_loss).
    """
    lpips_weight = arguments.get('LPIPS_weight', 1.0)
    max_kl_weight = arguments.get('kl_weight', 1.0)
    cycle_length = arguments.get('cycle_length', 10)

    # LPIPS expects input in [-1, 1] range
    x_scaled = x * 2 - 1
    x_hat_scaled = x_hat * 2 - 1

    dim_wise_kl_loss = compute_kl_divergence(mu, logvar, arguments)
    kl_weight = get_kl_weight(epoch, cycle_length, max_kl_weight)
    scaled_dim_wise_kl_loss = dim_wise_kl_loss * kl_weight

    lpips_loss = lpips_model(x_scaled, x_hat_scaled).mean() * lpips_weight

    total_loss = lpips_loss + scaled_dim_wise_kl_loss

    logger.debug(f"LPIPS Loss: {lpips_loss.item()}")
    logger.debug(f"KL Loss (Unscaled/Scaled): {dim_wise_kl_loss.item()}/{scaled_dim_wise_kl_loss.item()}")
    logger.debug(f"KL Weight (scheduled): {kl_weight}")
    logger.debug(f"Total Loss: {total_loss.item()}")

    return total_loss, scaled_dim_wise_kl_loss, lpips_loss


def get_regressor_loss(
    prop: torch.Tensor,
    property_hat: torch.Tensor,
    loss_model: nn.Module
) -> torch.Tensor:
    """
    Compute regressor loss for property prediction.

    Args:
        prop: Ground truth property values.
        property_hat: Predicted property values.
        loss_model: Loss function (e.g., MSELoss).

    Returns:
        Regressor loss value.
    """
    loss = loss_model(prop, property_hat)
    logger.debug(f"Regressor Loss: {loss.item()}")
    return loss


def create_lpips_model(device: torch.device, net: str = 'alex') -> nn.Module:
    """
    Create an LPIPS perceptual loss model.

    Args:
        device: Device to load model on.
        net: Network backbone ('alex', 'vgg', 'squeeze').

    Returns:
        LPIPS model.
    """
    model = lpips.LPIPS(net=net).to(device)
    model.eval()
    return model


def create_ssim_model(device: torch.device) -> nn.Module:
    """
    Create an SSIM loss model.

    Args:
        device: Device to load model on.

    Returns:
        SSIMLoss model.
    """
    model = SSIMLoss().to(device)
    return model
