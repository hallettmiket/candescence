"""
Purpose: Helpers for inspecting and building per-variable conditioning
dicts for Strategy 15 / 16 multi-cond tendril VAE checkpoints.
Author: Hallett Lab
Date: 2026-04-20
Input: A loaded TLVModelWrapper plus metadata/conditioning arrays
Output: Tuple of active cond keys, dict of categorical orderings, or a
one-sample cond dict ready to feed into encoder/decoder forward.

The TLV latent explorer (and any future diagnostic page) needs to
reconstruct the exact ``{'hue', 'day', 'media'}`` conditioning dict that
the training pipeline built for each row. The inline construction lives at
``apps/latent_explorer_app.py`` around lines 1615-1663 but its ``_onehot``
is a local inner function. This module re-exposes the same logic.
"""

from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple, Union

import numpy as np
import torch

from candescence.core.logging_config import get_logger

logger = get_logger("candescence.interface.training.cond_utils")


CondVec = Union[np.ndarray, torch.Tensor, Sequence[float]]


def resolve_cond_keys(model: Any) -> Tuple[str, ...]:
    """Return the active FiLM cond keys for a loaded model.

    Prefers ``model._config.cond_keys`` (written at train time and loaded
    from args.json via ``model_loader._load_config``). Falls back to
    ``model._vae.encoder.cond_keys`` when the config attribute is absent
    (older checkpoints). Returns ``()`` for architectures that do not use
    per-variable FiLM (strategies other than 15/16).
    """
    strategy = getattr(getattr(model, '_config', None), 'strategy', None)
    if strategy not in (15, 16):
        return ()

    config_keys = getattr(model._config, 'cond_keys', None)
    if config_keys:
        return tuple(str(k) for k in config_keys)

    encoder = getattr(getattr(model, '_vae', None), 'encoder', None)
    encoder_keys = getattr(encoder, 'cond_keys', None)
    if encoder_keys:
        return tuple(str(k) for k in encoder_keys)

    return ('hue', 'day', 'media')


def resolve_categories(model: Any) -> Dict[str, List[str]]:
    """Return the canonical one-hot ordering for categorical cond keys.

    Returns ``{"media": [...], "day": [...], "plate_phys": [...]}`` with
    empty lists when the model config does not carry the attribute —
    callers should treat an empty list for a key that is active in
    ``resolve_cond_keys`` as a loading error (the same check
    ``latent_explorer_app`` does at lines 1453-1464).
    """
    cfg = getattr(model, '_config', None)
    return {
        "media": list(getattr(cfg, 'media_categories', []) or []),
        "day": list(getattr(cfg, 'day_categories', []) or []),
        "plate_phys": list(getattr(cfg, 'plate_phys_categories', []) or []),
    }


def _onehot(value: str, categories: Sequence[str]) -> np.ndarray:
    """Build a 1-D one-hot vector; all zeros if ``value`` is unknown."""
    out = np.zeros(len(categories), dtype=np.float32)
    if value in categories:
        out[categories.index(value)] = 1.0
    return out


def _row_get(metadata_row: Mapping[str, Any], key: str, default: Any = '') -> Any:
    """Read ``key`` from a pandas Series or plain dict, returning ``default``
    when missing. Pandas Series exposes ``.get``; plain dicts do too, but
    nested Mapping types sometimes don't, so fall through to ``[]`` access.
    """
    if hasattr(metadata_row, 'get'):
        return metadata_row.get(key, default)
    try:
        return metadata_row[key]
    except (KeyError, IndexError):
        return default


def build_multi_cond_dict(
    hue_vec: CondVec,
    metadata_row: Mapping[str, Any],
    cond_keys: Sequence[str],
    media_categories: Sequence[str],
    day_categories: Sequence[str],
    device: torch.device,
    plate_phys_categories: Optional[Sequence[str]] = None,
) -> Dict[str, torch.Tensor]:
    """Build a single-sample cond dict ready for encoder/decoder forward.

    Each returned tensor has shape ``(1, d_key)``, dtype float32, on
    ``device``. Mirrors the inline construction at
    ``apps/latent_explorer_app.py`` lines 1645-1663; the inline version
    iterates over a batch, this one handles one sample.

    Parameters
    ----------
    hue_vec:
        1-D hue conditioning vector of length ``cond_dim_hue``.
    metadata_row:
        Row-level metadata (e.g. ``metadata_df.iloc[i]`` as a ``pd.Series``
        or a dict). Must contain keys ``'media'``, ``'day'``, and
        ``'plate_phys'`` (or ``'plate'`` + ``'media'`` + ``'day'`` so we
        can derive it as ``plate:media:day``) when the corresponding
        FiLM pathway is active.
    cond_keys:
        Output of :func:`resolve_cond_keys`.
    media_categories, day_categories, plate_phys_categories:
        Canonical orderings from :func:`resolve_categories`. Pass
        ``plate_phys_categories=None`` when the model doesn't use that
        FiLM pathway.
    device:
        Target device for all tensors in the returned dict.
    """
    cond: Dict[str, torch.Tensor] = {}

    if 'hue' in cond_keys:
        hue_arr = np.asarray(hue_vec, dtype=np.float32).reshape(-1)
        cond['hue'] = torch.from_numpy(hue_arr).unsqueeze(0).to(device)

    if 'day' in cond_keys:
        day_str = str(_row_get(metadata_row, 'day', ''))
        day_oh = _onehot(day_str, day_categories)
        cond['day'] = torch.from_numpy(day_oh).unsqueeze(0).to(device)

    if 'media' in cond_keys:
        media_str = str(_row_get(metadata_row, 'media', ''))
        media_oh = _onehot(media_str, media_categories)
        cond['media'] = torch.from_numpy(media_oh).unsqueeze(0).to(device)

    if 'plate_phys' in cond_keys:
        pp_value = _row_get(metadata_row, 'plate_phys', None)
        if pp_value is None:
            # Fall back to the same derivation the dataset uses:
            # plate_phys = plate + ':' + media + ':' + day.
            day_value = _row_get(metadata_row, 'day', 'NA')
            pp_value = (
                f"{_row_get(metadata_row, 'plate', '')}:"
                f"{_row_get(metadata_row, 'media', '')}:"
                f"{day_value}"
            )
        pp_str = str(pp_value)
        pp_oh = _onehot(pp_str, plate_phys_categories or [])
        cond['plate_phys'] = torch.from_numpy(pp_oh).unsqueeze(0).to(device)

    return cond
