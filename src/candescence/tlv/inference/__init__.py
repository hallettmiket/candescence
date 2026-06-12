"""
TLV Inference module.

Provides inference and analysis capabilities for trained VAE models.
"""

from .inference import Inference
from .latent_embedding import LatentEmbedding
from .tendril_replicate_diagnostics import (
    NonrepRankResult,
    PeakInfo,
    RankChangeStatsResult,
    ReplicateRankResult,
    build_nonrep_pairs,
    build_replicate_pairs,
    compute_intermediate_distances_for_pairs,
    compute_nonrep_rank_matrices_per_layer,
    compute_rank_change_stats,
    compute_real_layer_embeddings,
    compute_replicate_rank_matrices_per_layer,
    compute_tendril_latent_embeddings,
    inspect_distance_peaks_for_layer,
    plot_intermediate_layer_outliers,
    plot_replicate_distance_distribution_per_layer,
    show_pair_reconstructions_grid,
)

__all__ = [
    "Inference",
    "LatentEmbedding",
    "build_replicate_pairs",
    "build_nonrep_pairs",
    "compute_intermediate_distances_for_pairs",
    "compute_replicate_rank_matrices_per_layer",
    "compute_nonrep_rank_matrices_per_layer",
    "compute_rank_change_stats",
    "compute_tendril_latent_embeddings",
    "compute_real_layer_embeddings",
    "plot_replicate_distance_distribution_per_layer",
    "plot_intermediate_layer_outliers",
    "inspect_distance_peaks_for_layer",
    "show_pair_reconstructions_grid",
    "ReplicateRankResult",
    "NonrepRankResult",
    "RankChangeStatsResult",
    "PeakInfo",
]
