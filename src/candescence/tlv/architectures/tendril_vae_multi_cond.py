"""
Purpose: Tendril VAE with per-variable FiLM conditioning (Strategies 15 & 16).
Author: Hallett Lab
Date: 2026-04-14
Input: Images + a dict of conditioning vectors keyed by variable name
Output: Reconstructed images, latent representations, skip connections

Strategy 14 applies a single FiLM layer per block whose conditioning is a
concatenated HSV vector. Strategy 15 stacks separate FiLM modules per
variable (hue / day / media) at each block so each variable produces its
own affine shift on the feature maps. Strategy 16 drops ``day`` as a
standalone FiLM key (its main effect on hue is ~0%) and adds
``plate_phys`` — the physical plate-day identity (plate + media + day) —
to absorb the ~9% conditional-R² plate-level batch effect plus the
small but real media×day interaction (~0.2% of hue variance) in a
single embedding. See ``scripts/hue_day_medium_plate_variance.py`` for
the variance analysis.

The active FiLM pathways are controlled by the ``_cond_keys`` arguments
entry — a tuple of names drawn from :data:`COND_KEYS`. Default is the
full Strategy-15 set so pre-existing checkpoints load unchanged.
"""

import torch
import torch.nn as nn
from typing import Dict, List, Optional, Tuple, Union

from candescence.core.logging_config import get_logger
from .film import FiLM

logger = get_logger("candescence.architectures.tendril_vae_multi_cond")


# Canonical order in which per-variable FiLM modulations are applied at each
# block. Kept as a constant so encoder/decoder/wrapper agree on the vocabulary.
# Concrete runs may use a subset (e.g. Strategy 16 drops "day" and adds
# "plate_phys" — keyed on plate:media:day — for the physical-plate batch
# effect together with the media×day interaction).
COND_KEYS: Tuple[str, ...] = ("hue", "day", "media", "plate_phys")


def _resolve_cond_keys(arguments: dict) -> Tuple[str, ...]:
    """Return the FiLM conditioning keys active for this architecture instance.

    Reads ``_cond_keys`` from the arguments dict (preferred). When absent —
    i.e. pre-Strategy-16 checkpoints — falls back to the full
    :data:`COND_KEYS` tuple so Strategy 15 behavior is unchanged. Order
    within the returned tuple controls the order in which FiLM modules
    are applied inside :class:`_MultiFiLM`.
    """
    raw = arguments.get('_cond_keys')
    if raw is None:
        return COND_KEYS
    keys = tuple(str(k) for k in raw)
    unknown = [k for k in keys if k not in COND_KEYS]
    if unknown:
        raise ValueError(
            f"Unknown cond keys {unknown}; must be a subset of {COND_KEYS}"
        )
    return keys


def initialize_weights(m: nn.Module) -> None:
    """Kaiming-uniform for LeakyReLU activations."""
    if isinstance(m, (nn.Linear, nn.Conv2d, nn.ConvTranspose2d)):
        nn.init.kaiming_uniform_(m.weight, nonlinearity='leaky_relu')
        if m.bias is not None:
            nn.init.zeros_(m.bias)


def _cond_dims_from_args(arguments: dict) -> Dict[str, int]:
    """Pull per-variable cond dimensions from the arguments dict."""
    return {
        "hue": int(arguments.get('_cond_dim_hue', 0)),
        "day": int(arguments.get('_cond_dim_day', 0)),
        "media": int(arguments.get('_cond_dim_media', 0)),
        "plate_phys": int(arguments.get('_cond_dim_plate_phys', 0)),
    }


class _MultiFiLM(nn.Module):
    """Stack of per-variable FiLM modules applied sequentially.

    For each key in ``cond_keys`` (in order), ``forward`` applies the
    corresponding FiLM module to ``x`` using ``cond_dict[key]``. Each
    sub-FiLM uses the standard :class:`FiLM` implementation from
    ``film.py``.

    Attribute names ``film_hue`` / ``film_day`` / ``film_media`` are kept
    (rather than using a :class:`~torch.nn.ModuleDict`) so Strategy 15
    checkpoints continue to load into this class with the default
    ``cond_keys = COND_KEYS``. Strategy 16 omits ``day``: the
    ``film_day`` attribute is simply never created and never appears in
    the state dict.
    """

    def __init__(
        self,
        feature_dim: int,
        cond_dims: Dict[str, int],
        cond_keys: Tuple[str, ...] = COND_KEYS,
    ) -> None:
        super().__init__()
        self.feature_dim = feature_dim
        self.cond_keys = tuple(cond_keys)
        # Note: naming with ``film_<var>`` ensures the factory optimizer
        # picks these up via the substring 'film' check, and keeps the
        # state-dict key format stable across strategies.
        if "hue" in self.cond_keys:
            self.film_hue = FiLM(feature_dim, cond_dims["hue"])
        if "day" in self.cond_keys:
            self.film_day = FiLM(feature_dim, cond_dims["day"])
        if "media" in self.cond_keys:
            self.film_media = FiLM(feature_dim, cond_dims["media"])
        if "plate_phys" in self.cond_keys:
            self.film_plate_phys = FiLM(feature_dim, cond_dims["plate_phys"])

    def forward(
        self,
        x: torch.Tensor,
        cond_dict: Dict[str, torch.Tensor],
    ) -> torch.Tensor:
        for key in self.cond_keys:
            x = getattr(self, f"film_{key}")(x, cond_dict[key])
        return x


class MultiCondEncoder(nn.Module):
    """Tendril encoder with per-variable FiLM conditioning (Strategies 15 & 16)."""

    def __init__(self, arguments: dict) -> None:
        super().__init__()
        self.arguments = arguments
        self.device = torch.device(
            arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        self.cond_dims = _cond_dims_from_args(arguments)
        self.cond_keys = _resolve_cond_keys(arguments)
        self.latent_dim = self.arguments['latent_dim']

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])

        self.film1 = _MultiFiLM(64, self.cond_dims, self.cond_keys)
        self.film2 = _MultiFiLM(128, self.cond_dims, self.cond_keys)
        self.film3 = _MultiFiLM(256, self.cond_dims, self.cond_keys)
        self.film4 = _MultiFiLM(512, self.cond_dims, self.cond_keys)

        self.conv1 = nn.Conv2d(3, 64, 3, padding=1)
        self.batch1 = nn.BatchNorm2d(64)
        self.conv1_extra = nn.Conv2d(64, 64, 3, padding=1)
        self.batch1_extra = nn.BatchNorm2d(64)
        self.pool1 = nn.MaxPool2d(2, 2)

        self.conv2 = nn.Conv2d(64, 128, 3, padding=1)
        self.batch2 = nn.BatchNorm2d(128)
        self.conv2_extra = nn.Conv2d(128, 128, 3, padding=1)
        self.batch2_extra = nn.BatchNorm2d(128)
        self.pool2 = nn.MaxPool2d(2, 2)

        self.conv3 = nn.Conv2d(128, 256, 3, padding=1)
        self.batch3 = nn.BatchNorm2d(256)
        self.conv3_extra = nn.Conv2d(256, 256, 3, padding=1)
        self.batch3_extra = nn.BatchNorm2d(256)
        self.pool3 = nn.MaxPool2d(2, 2)

        self.conv4 = nn.Conv2d(256, 512, 3, padding=1)
        self.batch4 = nn.BatchNorm2d(512)
        self.conv4_extra = nn.Conv2d(512, 512, 3, padding=1)
        self.batch4_extra = nn.BatchNorm2d(512)

        self.fc_mu: Optional[nn.Linear] = None
        self.fc_logvar: Optional[nn.Linear] = None

        self.apply(initialize_weights)

    def forward_conv(
        self,
        x: torch.Tensor,
        cond: Dict[str, torch.Tensor],
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        x1 = self.act(self.batch1(self.conv1(x)))
        x1 = self.film1(x1, cond)
        x1 = self.act(self.batch1_extra(self.conv1_extra(x1)))
        x1_pooled = self.pool1(x1)

        x2 = self.act(self.batch2(self.conv2(x1_pooled)))
        x2 = self.film2(x2, cond)
        x2 = self.act(self.batch2_extra(self.conv2_extra(x2)))
        x2_pooled = self.pool2(x2)

        x3 = self.act(self.batch3(self.conv3(x2_pooled)))
        x3 = self.film3(x3, cond)
        x3 = self.act(self.batch3_extra(self.conv3_extra(x3)))
        x3_pooled = self.pool3(x3)

        x4 = self.act(self.batch4(self.conv4(x3_pooled)))
        x4 = self.film4(x4, cond)
        x4 = self.act(self.batch4_extra(self.conv4_extra(x4)))

        return x1, x2, x3, x4

    def reparameterize(self, mu: torch.Tensor, logvar: torch.Tensor) -> torch.Tensor:
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std

    def forward(
        self,
        x: torch.Tensor,
        cond: Dict[str, torch.Tensor],
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, List[torch.Tensor]]:
        x1, x2, x3, x4 = self.forward_conv(x, cond)

        x4_flat = torch.flatten(x4, start_dim=1)

        mu = self.fc_mu(x4_flat)
        logvar = torch.clamp(self.fc_logvar(x4_flat), min=-20, max=10)
        z = self.reparameterize(mu, logvar)

        return z, mu, logvar, [x1, x2, x3, x4]

    def encode(
        self,
        x: torch.Tensor,
        cond: Dict[str, torch.Tensor],
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        _, mu, logvar, _ = self.forward(x, cond)
        return mu, logvar


class MultiCondDecoder(nn.Module):
    """Tendril decoder with per-variable FiLM conditioning (Strategies 15 & 16)."""

    def __init__(self, arguments: dict) -> None:
        super().__init__()
        self.arguments = arguments
        self.latent_dim = self.arguments['latent_dim']
        self.cond_dims = _cond_dims_from_args(arguments)
        self.cond_keys = _resolve_cond_keys(arguments)

        self.act = nn.LeakyReLU(negative_slope=self.arguments['leaky_relu_slope'])
        self.sigmoid = nn.Sigmoid()

        self.film4 = _MultiFiLM(256, self.cond_dims, self.cond_keys)
        self.film3 = _MultiFiLM(128, self.cond_dims, self.cond_keys)
        self.film2 = _MultiFiLM(64, self.cond_dims, self.cond_keys)
        self.film1 = _MultiFiLM(32, self.cond_dims, self.cond_keys)

        bottleneck_size = (self.arguments['image_dimension'] // 8)
        self.fc = nn.Linear(self.latent_dim, 512 * bottleneck_size * bottleneck_size)

        self.convtrans4 = nn.ConvTranspose2d(512 + 512, 256, 3, stride=1, padding=1)
        self.bn4 = nn.BatchNorm2d(256)
        self.bn4_extra = nn.BatchNorm2d(256)
        self.convtrans4_extra = nn.ConvTranspose2d(256, 256, 3, stride=2, padding=1, output_padding=1)

        self.convtrans3 = nn.ConvTranspose2d(256 + 256, 128, 3, stride=2, padding=1, output_padding=1)
        self.bn3 = nn.BatchNorm2d(128)
        self.bn3_extra = nn.BatchNorm2d(128)
        self.convtrans3_extra = nn.Conv2d(128, 128, 3, padding=1)

        self.convtrans2 = nn.ConvTranspose2d(128 + 128, 64, 3, stride=2, padding=1, output_padding=1)
        self.bn2 = nn.BatchNorm2d(64)
        self.bn2_extra = nn.BatchNorm2d(64)
        self.convtrans2_extra = nn.Conv2d(64, 64, 3, padding=1)

        self.convtrans1 = nn.ConvTranspose2d(64 + 64, 32, 3, stride=1, padding=1)
        self.bn1 = nn.BatchNorm2d(32)
        self.bn1_extra = nn.BatchNorm2d(32)
        self.convtrans1_extra = nn.Conv2d(32, 32, 3, padding=1)

        self.convtrans_final = nn.ConvTranspose2d(32, 3, 3, stride=1, padding=1)

        self.apply(initialize_weights)

    def forward(
        self,
        z: torch.Tensor,
        skip_connections: List[torch.Tensor],
        cond: Dict[str, torch.Tensor],
    ) -> torch.Tensor:
        skip1, skip2, skip3, skip4 = skip_connections

        x = self.fc(z)
        x = x.view(-1, 512, 16, 16)

        x = torch.cat([x, skip4], dim=1)
        x = self.act(self.bn4(self.convtrans4(x)))
        x = self.film4(x, cond)
        x = self.act(self.bn4_extra(self.convtrans4_extra(x)))

        x = torch.cat([x, skip3], dim=1)
        x = self.act(self.bn3(self.convtrans3(x)))
        x = self.film3(x, cond)
        x = self.act(self.bn3_extra(self.convtrans3_extra(x)))

        x = torch.cat([x, skip2], dim=1)
        x = self.act(self.bn2(self.convtrans2(x)))
        x = self.act(self.bn2_extra(self.convtrans2_extra(x)))
        x = self.film2(x, cond)

        x = torch.cat([x, skip1], dim=1)
        x = self.act(self.bn1(self.convtrans1(x)))
        x = self.act(self.bn1_extra(self.convtrans1_extra(x)))
        x = self.film1(x, cond)

        x = self.convtrans_final(x)
        x = self.sigmoid(x)

        return x


class multi_cond_tendril_VAE(nn.Module):
    """Strategy-15 Tendril VAE with per-variable FiLM conditioning.

    Mirrors :class:`candescence.tlv.architectures.tendril_vae.tendril_VAE`
    but with encoder and decoder that expect a dict of conditioning
    tensors (keys ``'hue'``, ``'day'``, ``'media'``) instead of a single
    combined vector.
    """

    def __init__(self, arguments: dict) -> None:
        super().__init__()
        self.arguments = arguments
        self.device = torch.device(
            self.arguments['DEVICE'] if torch.cuda.is_available() else "cpu"
        )
        self.latent_dim = self.arguments['latent_dim']

        self.encoder = MultiCondEncoder(self.arguments).to(self.device)

        dummy_input = torch.ones(
            1, 3,
            self.arguments['image_dimension'],
            self.arguments['image_dimension']
        ).to(self.device)
        dummy_cond = self._make_dummy_cond().to_device(self.device)

        with torch.no_grad():
            x1, x2, x3, x4 = self.encoder.forward_conv(dummy_input, dummy_cond.as_dict())

        self.encoder.fc_mu = nn.Linear(x4.numel() // x4.shape[0], self.latent_dim)
        self.encoder.fc_logvar = nn.Linear(x4.numel() // x4.shape[0], self.latent_dim)

        self.decoder = MultiCondDecoder(self.arguments).to(self.device)

    def _make_dummy_cond(self) -> "_DummyCondBundle":
        dims = _cond_dims_from_args(self.arguments)
        keys = _resolve_cond_keys(self.arguments)
        return _DummyCondBundle(
            {k: torch.ones(1, max(dims[k], 1)) for k in keys}
        )

    def forward(
        self,
        x: torch.Tensor,
        cond: Union[Dict[str, torch.Tensor], List[Dict[str, torch.Tensor]]],
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor, List[torch.Tensor]]:
        x = x.to(self.device).float()

        if isinstance(cond, list) and len(cond) == 2:
            encoder_cond, decoder_cond = cond
        else:
            encoder_cond = cond
            decoder_cond = cond

        z, mu, logvar, skip_connections = self.encoder(x, encoder_cond)
        recon = self.decoder(z, skip_connections, decoder_cond)

        return recon, z, mu, logvar, skip_connections


class _DummyCondBundle:
    """Tiny wrapper so the ctor dummy-forward stays readable."""

    def __init__(self, d: Dict[str, torch.Tensor]) -> None:
        self._d = d

    def to_device(self, device: torch.device) -> "_DummyCondBundle":
        return _DummyCondBundle({k: v.to(device) for k, v in self._d.items()})

    def as_dict(self) -> Dict[str, torch.Tensor]:
        return self._d
