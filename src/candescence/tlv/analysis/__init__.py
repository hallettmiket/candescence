"""Cluster and latent-space analysis utilities for TLV models."""

from candescence.tlv.analysis.batch_effect_diagnostics import (
    CrossPlateResult,
    VarianceResult,
    WithinPlateResult,
    cross_plate_consistency,
    variance_partitioning,
    within_plate_separation,
)
from candescence.tlv.analysis.hypergeometric_enrichment import (
    adjust_pvalues_bh,
    compute_hypergeometric_enrichment,
    summarize_cluster_enrichment,
)
from candescence.tlv.analysis.latent_space_metrics import (
    KNNPurityResult,
    MantelResult,
    SilhouetteResult,
    SkbioUnavailableError,
    WithinBetweenResult,
    knn_label_purity,
    mantel_latent_vs_matrix,
    mantel_latent_vs_numeric,
    silhouette_metrics,
    within_between_distance_effect,
)
from candescence.tlv.analysis.replicate_consistency import (
    ReplicateConsistencyResult,
    replicate_consistency_summary,
    replicate_random_auroc,
    replicate_random_cohens_d,
)
from candescence.tlv.analysis.image_distance_metrics import (
    feature_distance_matrix,
    raw_pixel_distance_matrix,
)
from candescence.tlv.analysis.hue_nuisance_qc import (
    HueNuisanceReport,
    PCAReportResult,
    StratumFitInfo,
    build_design_matrix,
    pca_report,
    plot_pc_scatter,
    report_to_stdout,
    run_hue_nuisance_qc,
    within_group_residuals,
)
from candescence.tlv.analysis.image_feature_table import (
    FEATURE_NAMES,
    compute_image_features_batch,
)
from candescence.tlv.analysis.pc_feature_correlation import (
    partial_corr_matrix,
    pc_feature_correlation,
    pearson_matrix,
)
from candescence.tlv.analysis.skip_channel_stats import (
    compute_skip_channel_stats,
    skip_pca_summary,
)
from candescence.tlv.analysis.skip_saliency import (
    compute_skip_saliency,
    compute_tendril_mu_saliency,
)
from candescence.tlv.analysis.skip_traversal import (
    skip_channels_to_rgb,
    tendril_latent_walk,
)
from candescence.tlv.analysis.tendril_rank_change import (
    RankChangeResult,
    pairwise_distances_for_index_pairs,
    rank_change_across_spaces,
    ranks_from_distances,
)

__all__ = [
    "compute_hypergeometric_enrichment",
    "adjust_pvalues_bh",
    "summarize_cluster_enrichment",
    "silhouette_metrics",
    "SilhouetteResult",
    "mantel_latent_vs_numeric",
    "mantel_latent_vs_matrix",
    "MantelResult",
    "replicate_random_auroc",
    "replicate_random_cohens_d",
    "replicate_consistency_summary",
    "ReplicateConsistencyResult",
    "raw_pixel_distance_matrix",
    "feature_distance_matrix",
    "SkbioUnavailableError",
    "knn_label_purity",
    "KNNPurityResult",
    "within_between_distance_effect",
    "WithinBetweenResult",
    "rank_change_across_spaces",
    "RankChangeResult",
    "pairwise_distances_for_index_pairs",
    "ranks_from_distances",
    "variance_partitioning",
    "VarianceResult",
    "within_plate_separation",
    "WithinPlateResult",
    "cross_plate_consistency",
    "CrossPlateResult",
    "build_design_matrix",
    "within_group_residuals",
    "pca_report",
    "plot_pc_scatter",
    "run_hue_nuisance_qc",
    "report_to_stdout",
    "HueNuisanceReport",
    "PCAReportResult",
    "StratumFitInfo",
    "FEATURE_NAMES",
    "compute_image_features_batch",
    "pearson_matrix",
    "partial_corr_matrix",
    "pc_feature_correlation",
    "tendril_latent_walk",
    "skip_channels_to_rgb",
    "compute_skip_saliency",
    "compute_tendril_mu_saliency",
    "compute_skip_channel_stats",
    "skip_pca_summary",
]
