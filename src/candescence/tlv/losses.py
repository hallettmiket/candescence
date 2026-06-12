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
