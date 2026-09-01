# ============================================================
# Model catalog + growth functions (logistic / piecewise-linear)
# ============================================================
load_catalog <- function(path = "inst/model_catalog.rds") {
  readRDS(path)
}

# 40 residual-bootstrap (a, b, c) triples per Season x Cultivar for the LGG
# (logistic) family, re-run in an isolated copy of the manuscript's own
# fitting pipeline expressly to get a real JOINT uncertainty band -- see
# predict_growth_band()'s doc comment for why the marginal-CI-based band
# wasn't good enough. Each row is one bootstrap replicate's correlated
# parameter triple, not an independent draw per parameter.
load_lgg_boot_draws <- function(path = "inst/lgg_boot_draws_gdd.rds") {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

# ============================================================
# Trial-site auto-selection: which of the two real field trials' calibration
# to apply, based on the water stress the CURRENT scenario actually computes
# to -- not a self-reported "rainfed/irrigated" label.
#
# Reference values are the mean smoothed WSI (compute_wsi_daily()'s `WSI`
# column, no Kc-adjustment -- matching the LGG/logistic default path) that
# ACTUALLY occurred during each trial, computed from its real station
# weather + real irrigation events + its own SLLL/SDUL/BD, root_depth=60cm:
#   Caswell 2021   (rain-dominant, ~25mm supplemental irrigation all season)
#   Sandhills 2022 (weekly-irrigated, ~381mm total, water-buffered)
# A scenario's own computed mean WSI is matched to whichever is closer.
# The pooled "Seasons 2021-2022" fit is intentionally not an auto-selection
# target -- the manuscript flags it as an approximation, not a verified
# replication, so it's only used when a researcher picks it explicitly.
# ============================================================
WSI_REF_CASWELL   <- 0.2568
WSI_REF_SANDHILLS <- 0.7870

pick_season_by_wsi <- function(mean_wsi) {
  if (abs(mean_wsi - WSI_REF_CASWELL) <= abs(mean_wsi - WSI_REF_SANDHILLS)) {
    "Caswell 2021"
  } else {
    "Sandhills 2022"
  }
}

logistic_fun <- function(x, L, k, x0) L / (1 + exp(-k * (x - x0)))
logistic_deriv <- function(x, L, k, x0) {
  ex <- exp(-k * (x - x0))
  (L * k * ex) / (1 + ex)^2
}
piecewise_fun <- function(x, m1, bp, b2, m2) ifelse(x <= bp, m1 * x, b2 + m2 * x)

# Uncertainty envelope around the prediction curve.
#
# For LGG (logistic), when `boot_draws` has rows (40 residual-bootstrap
# (a,b,c) triples for this exact Season x Cultivar -- see
# load_lgg_boot_draws()), the curve is evaluated once per bootstrap
# replicate and the band is the pointwise `band_level` central interval
# across those 40 correlated curves (e.g. band_level=0.5 -> 25th/75th
# percentile): a real joint bootstrap prediction interval, the same logic
# sigmoid_models_final.Rmd itself used for the parameter CIs, just not
# collapsed to marginal quantiles before it reaches the curve.
#
# Default band_level is 0.5 (an interquartile "typical range"), not the
# usual 95%: several cultivar/season fits have only ~8-9 sampling points,
# and a handful of the 40 bootstrap refits land on the nls grid-search's
# starting-value boundary rather than a genuinely converged optimum (most
# visibly in x0, the inflection point) -- real behavior of the underlying
# fit, not a bug here, but it makes the 95% tails swing enormously wide
# without being a very useful "typical" range for a stakeholder. The 95%
# band is still there if you widen band_level; it just isn't the default.
#
# Falls back to a one-parameter-at-a-time (OAT) envelope from the catalog's
# marginal 95% CI columns when no draws are available for this row
# (piecewise always -- only its breakpoint got a CI, not a bootstrap -- or
# an older catalog missing CI columns; band_level does not apply to this
# fallback, only to the real bootstrap-draws path). OAT perturbs one
# parameter to its bound at a time and takes the pointwise min/max; it's
# not a joint interval, but it's honest about that, unlike naively
# combining every parameter's bound simultaneously (full-factorial
# "corners"), which stacks up worst-cases a correlated draw would
# essentially never produce together and visibly overstates the band.
# Returns NA_real_ everywhere if neither is available.
predict_growth_band <- function(df_stress, row, model_type, apply_stress,
                                 w1, w2, fmin, use_fw_trapz, y0, boot_draws = NULL, band_level = 0.5) {
  na_band <- rep(NA_real_, nrow(df_stress))
  has_col <- function(cc) cc %in% names(row) && !is.na(row[[cc]])

  if (model_type == "piecewise") {
    if (!has_col("bp_lower") || !has_col("bp_upper")) {
      return(list(lower = na_band, upper = na_band))
    }
    # re-anchor b2 to keep the line continuous at each shifted breakpoint
    # (bp is the only PWL parameter with a CI, so OAT and full-factorial coincide here)
    corner <- function(bp) {
      b2 <- row$m1 * bp - row$m2 * bp
      piecewise_fun(df_stress$GDD_eff, row$m1, bp, b2, row$m2)
    }
    mat <- cbind(corner(row$bp_lower), corner(row$bp_upper))
    return(list(lower = apply(mat, 1, min), upper = apply(mat, 1, max)))
  }

  eval_params <- function(L, k, x0) {
    if (isTRUE(apply_stress)) {
      fn <- make_stressed_pred(df_stress, "GDD_cum", "WSI", logistic_deriv, w1, w2, fmin, y0)
      fn(df_stress$GDD_cum, L, k, x0)
    } else {
      logistic_fun(df_stress$GDD_cum, L, k, x0)
    }
  }

  if (!is.null(boot_draws) && nrow(boot_draws) > 0) {
    mat <- do.call(cbind, Map(eval_params, boot_draws$a, boot_draws$b, boot_draws$c))
    tail_prob <- (1 - band_level) / 2
    return(list(
      lower = apply(mat, 1, quantile, probs = tail_prob, na.rm = TRUE),
      upper = apply(mat, 1, quantile, probs = 1 - tail_prob, na.rm = TRUE)
    ))
  }

  ci_cols <- c("L_lower", "L_upper", "k_lower", "k_upper", "x0_lower", "x0_upper")
  if (!all(vapply(ci_cols, has_col, logical(1)))) {
    return(list(lower = na_band, upper = na_band))
  }
  oat <- list(
    c(row$L_lower, row$k, row$x0), c(row$L_upper, row$k, row$x0),
    c(row$L, row$k_lower, row$x0), c(row$L, row$k_upper, row$x0),
    c(row$L, row$k, row$x0_lower), c(row$L, row$k, row$x0_upper)
  )
  mat <- do.call(cbind, lapply(oat, function(p) eval_params(p[1], p[2], p[3])))
  list(lower = apply(mat, 1, min), upper = apply(mat, 1, max))
}

# Runs the selected model (logistic or piecewise) over a daily GDD/WSI series
# and returns the input df with `pred`/`pred_lower`/`pred_upper` columns +
# an "equation" attribute. `boot_draws`, if given, should already be filtered
# to this row's Season + Cultivar (see load_lgg_boot_draws()).
predict_growth <- function(df_stress, row, model_type, apply_stress,
                            w1, w2, fmin, use_fw_trapz, y0, boot_draws = NULL, band_level = 0.5) {
  if (model_type == "piecewise") {
    pred <- piecewise_fun(df_stress$GDD_eff, row$m1, row$bp, row$b2, row$m2)
    pred_df <- df_stress %>% mutate(pred = pred)
    attr(pred_df, "equation") <- row$piecewise_eq
  } else {
    if (isTRUE(apply_stress)) {
      stressed_pred_fun <- make_stressed_pred(
        df_daily = df_stress,
        x_col = "GDD_cum",
        wsi_col = "WSI",
        deriv_fun = logistic_deriv,
        w1 = w1, w2 = w2, f_min = fmin,
        y0 = y0
      )
      pred <- stressed_pred_fun(df_stress$GDD_cum, row$L, row$k, row$x0)
    } else {
      pred <- logistic_fun(df_stress$GDD_cum, row$L, row$k, row$x0)
    }
    pred_df <- df_stress %>% mutate(pred = pred)
    attr(pred_df, "equation") <- row$logistic_eq
  }

  band <- predict_growth_band(df_stress, row, model_type, apply_stress, w1, w2, fmin, use_fw_trapz, y0, boot_draws, band_level)
  pred_df$pred_lower <- band$lower
  pred_df$pred_upper <- band$upper
  pred_df
}
