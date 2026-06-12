"""
Retired: Old Strategy 15 — FiLM-conditioned tendril sub-VAEs.

This module held the original Strategy 15 implementation where the inner
tendril sub-VAEs received FiLM conditioning on top of the outer Tendril
VAE. Strategy 15 has been reworked to instead add per-variable FiLM
modules (hue, day, media) on the outer VAE, with the inner tendrils
reverting to plain TendrilLayerVAE like Strategy 14.

Moved here 2026-04-14 from
    src/candescence/tlv/architectures/tendril_sub.py
Kept for reference / possible restoration. Not imported by the runtime.
"""

import json
from pathlib import Path

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import TensorDataset, DataLoader
from typing import Dict, Tuple, Optional, Callable

from candescence.core.logging_config import get_logger
from candescence.tlv.architectures.film import FiLM
from candescence.tlv.architectures.tendril_sub import Tendrils, initialize_weights

logger = get_logger("obsolete.strategy_15_cond_tendrils")


class CondTendrilLayerVAE(nn.Module):
    """Tendril sub-VAE with FiLM conditioning (old Strategy 15)."""

    def __init__(
        self,
        input_shape: Tuple[int, ...],
        latent_dim: int,
        cond_dim: int,
        dropout_rate: float = 0.3,
    ) -> None:
        super().__init__()
        self.input_shape = input_shape
        self.cond_dim = cond_dim

        self.enc_conv1 = nn.Conv2d(input_shape[0], 32, kernel_size=4, stride=2, padding=1)
        self.enc_bn1 = nn.BatchNorm2d(32)
        self.enc_film1 = FiLM(32, cond_dim)

        self.enc_conv2 = nn.Conv2d(32, 64, kernel_size=4, stride=2, padding=1)
        self.enc_bn2 = nn.BatchNorm2d(64)
        self.enc_film2 = FiLM(64, cond_dim)

        self.enc_conv3 = nn.Conv2d(64, 128, kernel_size=4, stride=2, padding=1)
        self.enc_bn3 = nn.BatchNorm2d(128)
        self.enc_film3 = FiLM(128, cond_dim)

        self.enc_drop = nn.Dropout2d(dropout_rate)
        self.enc_act = nn.ReLU()

        self.flatten = nn.Flatten()
        bottleneck_h = input_shape[1] // 8
        bottleneck_w = input_shape[2] // 8
        flat_size = 128 * bottleneck_h * bottleneck_w

        self.fc_mu = nn.Linear(flat_size, latent_dim)
        self.fc_logvar = nn.Linear(flat_size, latent_dim)
        self.fc_latent = nn.Linear(latent_dim, flat_size)

        self.dec_conv1 = nn.ConvTranspose2d(128, 64, kernel_size=4, stride=2, padding=1)
        self.dec_bn1 = nn.BatchNorm2d(64)
        self.dec_film1 = FiLM(64, cond_dim)

        self.dec_conv2 = nn.ConvTranspose2d(64, 32, kernel_size=4, stride=2, padding=1)
        self.dec_bn2 = nn.BatchNorm2d(32)
        self.dec_film2 = FiLM(32, cond_dim)

        self.dec_conv3 = nn.ConvTranspose2d(32, input_shape[0], kernel_size=4, stride=2, padding=1)

        self.dec_drop = nn.Dropout2d(dropout_rate)
        self.dec_act = nn.ReLU()
        self.sigmoid = nn.Sigmoid()

    def reparameterize(self, mu: torch.Tensor, logvar: torch.Tensor) -> torch.Tensor:
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std

    def forward(
        self,
        x: torch.Tensor,
        cond_enc: torch.Tensor,
        cond_dec: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        if cond_dec is None:
            cond_dec = cond_enc

        h = self.enc_drop(self.enc_film1(self.enc_act(self.enc_bn1(self.enc_conv1(x))), cond_enc))
        h = self.enc_drop(self.enc_film2(self.enc_act(self.enc_bn2(self.enc_conv2(h))), cond_enc))
        h = self.enc_drop(self.enc_film3(self.enc_act(self.enc_bn3(self.enc_conv3(h))), cond_enc))

        h_flat = self.flatten(h)
        mu = self.fc_mu(h_flat)
        logvar = self.fc_logvar(h_flat)
        z = self.reparameterize(mu, logvar)

        z_proj = self.fc_latent(z)
        z_proj = z_proj.view(-1, 128, self.input_shape[1] // 8, self.input_shape[2] // 8)

        d = self.dec_drop(self.dec_film1(self.dec_act(self.dec_bn1(self.dec_conv1(z_proj))), cond_dec))
        d = self.dec_drop(self.dec_film2(self.dec_act(self.dec_bn2(self.dec_conv2(d))), cond_dec))
        x_hat = self.sigmoid(self.dec_conv3(d))

        return x_hat, mu, logvar, z


class CondTendrils:
    """Old Strategy 15 tendril container with per-image FiLM conditioning."""

    def __init__(self, arguments: Optional[dict] = None) -> None:
        self.arguments = arguments if arguments is not None else {}
        self.tendril_trainers: Dict[str, "CondTendrilTrainer"] = {}

    normalize_per_image = Tendrils.normalize_per_image

    def create_dataloader(
        self,
        tensor: torch.Tensor,
        cond_enc: torch.Tensor,
        cond_dec: Optional[torch.Tensor] = None,
        shuffle: bool = True,
    ) -> DataLoader:
        batch_size = self.arguments.get('tendril_batch_size', 256)
        if cond_dec is None:
            cond_dec = cond_enc
        dataset = TensorDataset(tensor, cond_enc, cond_dec)
        return DataLoader(dataset, batch_size=batch_size, shuffle=shuffle)

    def add_tendril(
        self,
        key: str,
        train_dataset: torch.Tensor,
        validation_dataset: torch.Tensor,
        train_cond: torch.Tensor,
        val_cond: torch.Tensor,
        train_cond_dec: Optional[torch.Tensor] = None,
        val_cond_dec: Optional[torch.Tensor] = None,
    ) -> None:
        input_shape = tuple(train_dataset[0].shape)
        latent_dim = self.arguments['tendril_latent_dim']
        cond_dim = train_cond.shape[1]

        model = CondTendrilLayerVAE(
            input_shape=input_shape,
            latent_dim=latent_dim,
            cond_dim=cond_dim,
        )
        model.apply(initialize_weights)

        train_normalized = self.normalize_per_image(train_dataset)
        val_normalized = self.normalize_per_image(validation_dataset)

        train_dl = self.create_dataloader(train_normalized, train_cond, train_cond_dec, shuffle=True)
        val_dl = self.create_dataloader(val_normalized, val_cond, val_cond_dec, shuffle=False)

        trainer = CondTendrilTrainer(model, train_dl, val_dl, self.arguments)
        self.tendril_trainers[key] = trainer
        logger.info(f"[obsolete] Added cond tendril '{key}'")

    def get_tendril(self, key: str) -> "CondTendrilTrainer":
        return self.tendril_trainers[key]

    def remove_tendril(self, key: str) -> None:
        del self.tendril_trainers[key]

    def list_tendrils(self) -> list:
        return list(self.tendril_trainers.keys())

    def train_tendril(self, key: str, progress_callback: Optional[Callable] = None) -> None:
        trainer = self.tendril_trainers[key]
        trainer.train(key, progress_callback=progress_callback)

    def save(self, path: Path) -> None:
        path.mkdir(parents=True, exist_ok=True)
        meta = {}
        for key, trainer in self.tendril_trainers.items():
            torch.save(trainer.model.state_dict(), path / f"{key}.pth")
            meta[key] = {
                "input_shape": list(trainer.model.input_shape),
                "latent_dim": trainer.model.fc_mu.out_features,
                "cond_dim": trainer.model.cond_dim,
                "conditioned": True,
            }
        with open(path / "tendrils_meta.json", "w") as f:
            json.dump(meta, f, indent=2)

    @classmethod
    def load(cls, path: Path, device: torch.device) -> "CondTendrils":
        with open(path / "tendrils_meta.json") as f:
            meta = json.load(f)

        instance = cls()
        for key, info in meta.items():
            model = CondTendrilLayerVAE(
                input_shape=tuple(info["input_shape"]),
                latent_dim=info["latent_dim"],
                cond_dim=info["cond_dim"],
            )
            state = torch.load(path / f"{key}.pth", map_location=device, weights_only=True)
            model.load_state_dict(state)
            model.to(device)
            model.eval()
            instance.tendril_trainers[key] = _InferenceCondTendrilHolder(model, info)
        return instance


class _InferenceCondTendrilHolder:
    def __init__(self, model: CondTendrilLayerVAE, info: dict) -> None:
        self.model = model
        self.input_shape = tuple(info["input_shape"])
        self.latent_dim = info["latent_dim"]
        self.cond_dim = info["cond_dim"]


class CondTendrilTrainer:
    def __init__(
        self,
        model: CondTendrilLayerVAE,
        train_dataloader: DataLoader,
        validation_dataloader: DataLoader,
        arguments: dict,
    ) -> None:
        self.device = arguments['DEVICE']
        self.model = model.to(self.device)

        lr = arguments.get('tendril_lr', 1e-6)
        wd = arguments.get('tendril_weight_decay', 0)
        self.optimizer = optim.Adam(model.parameters(), lr=lr, weight_decay=wd)
        self.loss_fn = arguments['tendril_loss_fn']
        self.train_dataloader = train_dataloader
        self.validation_dataloader = validation_dataloader
        self.arguments = arguments
        self.final_loss: Optional[float] = None

    def train(self, key: str, progress_callback: Optional[Callable] = None) -> None:
        num_epochs = self.arguments.get('tendril_num_epochs', 50)

        for epoch in range(num_epochs):
            self.model.train()
            total_loss = 0.0
            kl_sum = 0.0
            recon_sum = 0.0

            for batch in self.train_dataloader:
                x = batch[0].to(self.device)
                cond_enc = batch[1].to(self.device)
                cond_dec = batch[2].to(self.device) if len(batch) > 2 else None

                x_hat, mu, logvar, _ = self.model(x, cond_enc, cond_dec)

                loss, kl_loss, recon_loss = self.loss_fn(
                    x=x, x_hat=x_hat, mu=mu, logvar=logvar,
                    arguments=self.arguments, epoch=epoch,
                )

                self.optimizer.zero_grad()
                loss.backward()
                self.optimizer.step()

                total_loss += loss.item()
                kl_sum += (kl_loss.item() if torch.is_tensor(kl_loss) else float(kl_loss))
                recon_sum += (recon_loss.item() if torch.is_tensor(recon_loss) else float(recon_loss))

            n_batches = len(self.train_dataloader)
            avg_loss = total_loss / n_batches

            if progress_callback:
                progress_callback({
                    "tendril_key": key,
                    "epoch": epoch,
                    "total_epochs": num_epochs,
                    "loss": avg_loss,
                    "kl_loss": kl_sum / n_batches,
                    "recon_loss": recon_sum / n_batches,
                })

        self.final_loss = avg_loss
