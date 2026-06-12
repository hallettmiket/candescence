#!/usr/bin/env python3
"""Variance partitioning of edge-band hue across medium, day, and plate.

Analogous to ``scripts/hue_medium_vs_plate_variance.py`` but adds
``day`` as a second fixed factor. Fits a ladder of nested linear
mixed-effects models with ``(1|plate_phys)`` where
``plate_phys = plate + ':' + media`` (physical plate) and reports:

    - Variance components (sigma^2 plate, sigma^2 resid, ICC)
    - Marginal R^2 (fixed) and conditional R^2 (fixed + random),
      following Nakagawa & Schielzeth (2013).
    - Likelihood-ratio tests: does day add on top of medium?
      does medium add on top of day? does the media x day interaction
      add on top of the additive model?
    - OLS three-factor sequential R^2 decomposition as a cross-check.

Cohort: the same hue_table produced by the medium/plate analysis,
restricted to images whose filename condition is ``day2`` or ``day5``
(the ``wash`` images are dropped per user request).
"""
from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))
from candescence.core.settings import get_settings

logger = logging.getLogger(__name__)

_ZOO = get_settings().zoo_path
DEFAULT_HUE_TABLE = _ZOO / "hue_medium_vs_plate/hue_table.csv"
DEFAULT_OUTPUT = _ZOO / "hue_day_medium_plate"


def _day_from_filename(fn: str) -> str | None:
    """Return ``"2"`` / ``"5"`` for ``dayN`` condition, or ``None`` for wash/other."""
    parts = Path(fn).stem.split("_")
    if len(parts) < 3:
        return None
    tok = parts[2].lower()
    if tok.startswith("day"):
        return tok[3:] or None
    return None


def _fit_mixed(df: pd.DataFrame, formula: str, groups: str, reml: bool = False):
    import statsmodels.formula.api as smf
    return smf.mixedlm(formula, df, groups=df[groups]).fit(reml=reml)


def _r2_marginal_conditional(fit) -> tuple[float, float, float, float]:
    """Nakagawa & Schielzeth (2013) marginal / conditional R^2."""
    fe = fit.fe_params
    X = np.asarray(fit.model.exog)
    fixed_pred = X @ fe.values
    var_f = float(np.var(fixed_pred, ddof=0))
    sigma2_plate = float(fit.cov_re.iloc[0, 0])
    sigma2_resid = float(fit.scale)
    denom = var_f + sigma2_plate + sigma2_resid
    return sigma2_plate, sigma2_resid, var_f / denom, (var_f + sigma2_plate) / denom


def _nested_r2_ladder(df: pd.DataFrame) -> pd.DataFrame:
    """Opus's requested ladder: null -> +media -> +day -> +media:day -> +(1|plate).

    For each step fit a linear mixed model with ``(1|plate_phys)`` and
    report Nakagawa & Schielzeth marginal/conditional R^2. The last row
    captures the plate contribution as ``conditional - marginal`` of the
    full fixed-effects model (the random effect adds variance explained
    by plate, no additional fixed term).
    """
    steps = [
        ("null",            "average_hue ~ 1"),
        ("+media",          "average_hue ~ C(media)"),
        ("+day",            "average_hue ~ C(media) + C(day)"),
        ("+media:day",      "average_hue ~ C(media) * C(day)"),
    ]
    rows = []
    prev_marg = 0.0
    for label, formula in steps:
        fit = _fit_mixed(df, formula, "plate_phys", reml=True)
        sig_p, sig_r, r2_m, r2_c = _r2_marginal_conditional(fit)
        rows.append({
            "step": label,
            "formula": formula + "  + (1|plate_phys)",
            "R2_marginal": r2_m,
            "R2_conditional": r2_c,
            "delta_R2_marginal": r2_m - prev_marg,
            "sigma2_plate": sig_p,
            "sigma2_resid": sig_r,
        })
        prev_marg = r2_m
    # Last step: plate random effect. Fixed-effect structure is the full
    # media*day interaction; the random intercept adds plate variance.
    last = rows[-1]
    rows.append({
        "step": "+(1|plate)",
        "formula": "average_hue ~ C(media) * C(day)  + (1|plate_phys)",
        "R2_marginal": last["R2_marginal"],
        "R2_conditional": last["R2_conditional"],
        "delta_R2_marginal": last["R2_conditional"] - last["R2_marginal"],
        "sigma2_plate": last["sigma2_plate"],
        "sigma2_resid": last["sigma2_resid"],
    })
    return pd.DataFrame(rows)


def _lrt(full, null) -> tuple[float, int, float]:
    from scipy.stats import chi2
    lr = 2 * (full.llf - null.llf)
    dfd = int(full.df_modelwc - null.df_modelwc)
    p = float(chi2.sf(lr, dfd)) if dfd > 0 else float("nan")
    return lr, dfd, p


def _ols_three_way_r2(df: pd.DataFrame, response: str = "average_hue") -> dict:
    """Sequential R^2 and unique contributions for media, day, plate."""
    import statsmodels.formula.api as smf

    def r2(formula: str) -> float:
        return float(smf.ols(formula, df).fit().rsquared)

    r2_m = r2(f"{response} ~ C(media)")
    r2_d = r2(f"{response} ~ C(day)")
    r2_p = r2(f"{response} ~ C(plate_phys)")
    r2_md = r2(f"{response} ~ C(media) + C(day)")
    r2_mp = r2(f"{response} ~ C(media) + C(plate_phys)")
    r2_dp = r2(f"{response} ~ C(day) + C(plate_phys)")
    r2_mdp = r2(f"{response} ~ C(media) + C(day) + C(plate_phys)")
    r2_full = r2(f"{response} ~ C(media) * C(day) + C(plate_phys)")
    return {
        "r2_media": r2_m,
        "r2_day": r2_d,
        "r2_plate": r2_p,
        "r2_media_day": r2_md,
        "r2_media_plate": r2_mp,
        "r2_day_plate": r2_dp,
        "r2_media_day_plate": r2_mdp,
        "r2_media_x_day_plus_plate": r2_full,
        # Type-III unique contributions (each term added last).
        "unique_media": r2_mdp - r2_dp,
        "unique_day": r2_mdp - r2_mp,
        "unique_plate": r2_mdp - r2_md,
        "interaction_media_day": r2_full - r2_mdp,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--hue-table", type=Path, default=DEFAULT_HUE_TABLE)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )
    args.output_dir.mkdir(parents=True, exist_ok=True)

    logger.info("Loading hue table from %s", args.hue_table)
    df = pd.read_csv(args.hue_table)
    df["plate"] = df["plate"].astype(str)
    df["media"] = df["media"].astype(str)
    df["day"] = df["filename"].map(_day_from_filename)

    before = len(df)
    df = df.dropna(subset=["day"]).copy()
    logger.info("Dropped %d wash/other rows; kept %d rows (day2/day5 only)",
                before - len(df), len(df))
    df["day"] = df["day"].astype(str)
    df["plate_phys"] = df["plate"] + ":" + df["media"]

    logger.info("Rows=%d  plates=%d  physical_plates=%d  media=%d  days=%d",
                len(df), df["plate"].nunique(), df["plate_phys"].nunique(),
                df["media"].nunique(), df["day"].nunique())
    logger.info("Media counts:\n%s", df["media"].value_counts().to_string())
    logger.info("Day counts:\n%s", df["day"].value_counts().to_string())
    logger.info("Media x day:\n%s",
                pd.crosstab(df["media"], df["day"]).to_string())

    # ------------------------------------------------------------------
    # Mixed-effects ladder (ML for LRT, REML for variance components).
    # ------------------------------------------------------------------
    logger.info("Fitting mixed-effects models ...")
    null = _fit_mixed(df, "average_hue ~ 1", "plate_phys", reml=False)
    m_only = _fit_mixed(df, "average_hue ~ C(media)", "plate_phys", reml=False)
    d_only = _fit_mixed(df, "average_hue ~ C(day)", "plate_phys", reml=False)
    add = _fit_mixed(df, "average_hue ~ C(media) + C(day)", "plate_phys", reml=False)
    inter = _fit_mixed(df, "average_hue ~ C(media) * C(day)", "plate_phys", reml=False)
    add_reml = _fit_mixed(df, "average_hue ~ C(media) + C(day)",
                          "plate_phys", reml=True)

    sigma2_plate, sigma2_resid, r2_marg, r2_cond = _r2_marginal_conditional(add_reml)
    icc = sigma2_plate / (sigma2_plate + sigma2_resid)

    # LR tests
    lr_day_given_m, df_day_given_m, p_day_given_m = _lrt(add, m_only)
    lr_m_given_d, df_m_given_d, p_m_given_d = _lrt(add, d_only)
    lr_inter, df_inter, p_inter = _lrt(inter, add)
    lr_m_vs_null, df_m_vs_null, p_m_vs_null = _lrt(m_only, null)
    lr_d_vs_null, df_d_vs_null, p_d_vs_null = _lrt(d_only, null)

    logger.info("Running OLS three-way R^2 decomposition ...")
    ols = _ols_three_way_r2(df)

    logger.info("Computing nested R^2 ladder ...")
    ladder = _nested_r2_ladder(df)
    logger.info("\n%s", ladder.to_string(index=False))

    # ------------------------------------------------------------------
    # Write outputs
    # ------------------------------------------------------------------
    out = args.output_dir
    (out / "mixedlm_additive_summary.txt").write_text(str(add_reml.summary()))
    (out / "mixedlm_interaction_summary.txt").write_text(str(inter.summary()))

    pd.DataFrame({
        "component": ["sigma2_plate", "sigma2_resid", "ICC",
                      "R2_marginal_mediaday",
                      "R2_conditional_mediaday_plate"],
        "value": [sigma2_plate, sigma2_resid, icc, r2_marg, r2_cond],
    }).to_csv(out / "variance_components.csv", index=False)

    fe_df = pd.DataFrame({
        "term": add_reml.fe_params.index,
        "estimate": add_reml.fe_params.values,
        "std_err": add_reml.bse_fe.values,
        "z": add_reml.tvalues[: len(add_reml.fe_params)].values,
        "p_value": add_reml.pvalues[: len(add_reml.fe_params)].values,
    })
    fe_df.to_csv(out / "fixed_effects_additive.csv", index=False)

    pd.DataFrame([
        {"test": "media | day (LRT)", "chi2": lr_m_given_d,
         "df": df_m_given_d, "p": p_m_given_d},
        {"test": "day | media (LRT)", "chi2": lr_day_given_m,
         "df": df_day_given_m, "p": p_day_given_m},
        {"test": "media:day interaction (LRT)", "chi2": lr_inter,
         "df": df_inter, "p": p_inter},
        {"test": "media vs null (LRT)", "chi2": lr_m_vs_null,
         "df": df_m_vs_null, "p": p_m_vs_null},
        {"test": "day vs null (LRT)", "chi2": lr_d_vs_null,
         "df": df_d_vs_null, "p": p_d_vs_null},
    ]).to_csv(out / "lrt_tests.csv", index=False)

    pd.DataFrame([ols]).to_csv(out / "ols_three_way_r2.csv", index=False)
    ladder.to_csv(out / "nested_r2_ladder.csv", index=False)

    # Plot: hue by media split by day
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        import seaborn as sns
        fig, ax = plt.subplots(figsize=(11, 5))
        sns.stripplot(data=df, x="media", y="average_hue", hue="day",
                      ax=ax, size=2, alpha=0.5, dodge=True)
        ax.set_title("Edge-band hue by medium and day (wash excluded)")
        ax.legend(title="day", loc="best")
        fig.tight_layout()
        fig.savefig(out / "hue_by_media_day.png", dpi=120)
        plt.close(fig)
    except Exception as e:
        logger.warning("Plot failed: %s", e)

    # Pull ladder values for the verdict line.
    lookup = {row["step"]: row for _, row in ladder.iterrows()}
    verdict = (
        f"VERDICT (nested ladder, marginal R^2): "
        f"media={lookup['+media']['R2_marginal']:.3f}, "
        f"+day={lookup['+day']['R2_marginal']:.3f} "
        f"(delta={lookup['+day']['delta_R2_marginal']:.3f}), "
        f"+media:day={lookup['+media:day']['R2_marginal']:.3f} "
        f"(delta={lookup['+media:day']['delta_R2_marginal']:.3f}); "
        f"conditional with plate={lookup['+(1|plate)']['R2_conditional']:.3f} "
        f"(plate adds delta={lookup['+(1|plate)']['delta_R2_marginal']:.3f}); "
        f"plate ICC (after media+day) = {icc:.3f} "
        f"(plate sigma^2={sigma2_plate:.2f}, resid sigma^2={sigma2_resid:.2f}); "
        f"LRT day|media chi^2({df_day_given_m}) = {lr_day_given_m:.1f}, "
        f"p = {p_day_given_m:.3g}; "
        f"LRT media:day chi^2({df_inter}) = {lr_inter:.1f}, "
        f"p = {p_inter:.3g}."
    )
    ols_line = (
        f"OLS three-way: r2(media)={ols['r2_media']:.3f}, "
        f"r2(day)={ols['r2_day']:.3f}, r2(plate)={ols['r2_plate']:.3f}; "
        f"unique: media={ols['unique_media']:.3f}, "
        f"day={ols['unique_day']:.3f}, plate={ols['unique_plate']:.3f}; "
        f"media*day interaction adds {ols['interaction_media_day']:.3f}."
    )
    print("\n" + verdict)
    print(ols_line)
    (out / "verdict.txt").write_text(verdict + "\n" + ols_line + "\n")


if __name__ == "__main__":
    main()
