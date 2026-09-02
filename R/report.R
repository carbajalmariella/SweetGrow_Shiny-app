# ============================================================
# "What does this mean?" plain-language panel + downloadable report
# ============================================================

# Translates the numeric run outputs into a short, non-technical explanation
# a grower/extension agent can read without knowing what GDD or WSI mean.
explain_plain_language <- function(res) {
  df <- res$df
  pred <- res$pred

  wsi_mean <- mean(df$WSI, na.rm = TRUE)
  wsi_min <- min(df$WSI, na.rm = TRUE)
  stress_days <- sum(df$WSI < 0.8, na.rm = TRUE)

  stress_txt <- if (wsi_mean >= 0.9) {
    "The crop had very little water stress during this season -- soil moisture stayed close to its full water-holding capacity most days."
  } else if (wsi_mean >= 0.7) {
    "The crop experienced mild, intermittent water stress -- there were stretches where available soil water ran low, which can slow root growth a bit."
  } else {
    "The crop experienced substantial water stress for a large part of the season -- available soil water was frequently low, which is expected to reduce storage root growth compared to a well-watered scenario."
  }

  final_pred <- pred$pred[which.max(pred$date)]
  final_lower <- pred$pred_lower[which.max(pred$date)]
  final_upper <- pred$pred_upper[which.max(pred$date)]
  final_fw <- pred$pred_fw[which.max(pred$date)]
  yield_txt <- if (!is.null(final_fw) && !is.na(final_fw)) {
    sprintf(
      "That's roughly %.2f t/ha of estimated fresh (marketable) yield, converted from dry weight using this cultivar's dry-matter fraction (published where available, otherwise this trial's own estimate) -- a rougher approximation than the growth curve itself (see Methodology).",
      final_fw
    )
  } else {
    NULL
  }
  uncertainty_txt <- if (!is.na(final_lower) && !is.na(final_upper)) {
    if (isTRUE(res$band_is_bootstrap)) {
      sprintf(
        "The harvest-day prediction has a 50%% bootstrap uncertainty range (interquartile) of %.2f to %.2f (from 40 resampled refits of the fitted model).",
        final_lower, final_upper
      )
    } else {
      sprintf(
        "The harvest-day prediction has a rough uncertainty range of %.2f to %.2f, based on how uncertain the fitted model's own parameters are -- not a full statistical prediction interval, just an approximate envelope.",
        final_lower, final_upper
      )
    }
  } else {
    NULL
  }

  soil_txt <- sprintf(
    "Soil water-holding was estimated between a wilting point of %.2f and a field capacity of %.2f (fraction of soil volume that is water), using a %.0f cm rooting depth and a bulk density of %.2f g/cm3 -- source: %s.",
    res$soil_agg$SLLL, res$soil_agg$SDUL, res$root_depth, res$soil_agg$bulk_density_g_cm3, res$soil_agg$soil_source
  )

  bellevue_txt <- if (isTRUE(res$cultivar == "Bellevue")) {
    "Low reliability for Bellevue: its logistic (LGG) fit doesn't transfer across trials at all (R2 = -13.07 predicting Sandhills from the Caswell fit), so this prediction uses the piecewise (PWL) model instead, which itself only transfers moderately well (R2 = 0.35-0.64, vs. >= 0.82 for the other cultivars). Treat this prediction with extra caution."
  } else {
    NULL
  }

  calibration_txt <- if (!is.null(res$season_used)) {
    if (isTRUE(res$season_auto_selected)) {
      sprintf(
        "Growth calibration: %s. This was picked automatically because your scenario's computed water stress (%.2f) is closer to what actually occurred during that trial than the other one.",
        res$season_used, wsi_mean
      )
    } else {
      sprintf("Growth calibration: %s (manually selected).", res$season_used)
    }
  } else {
    NULL
  }

  htmltools::tags$div(
    htmltools::tags$ul(
      htmltools::tags$li(sprintf("Predicted storage root dry weight at harvest: %.2f t/ha.", final_pred)),
      if (!is.null(bellevue_txt)) htmltools::tags$li(bellevue_txt),
      if (!is.null(yield_txt)) htmltools::tags$li(yield_txt),
      if (!is.null(uncertainty_txt)) htmltools::tags$li(uncertainty_txt),
      htmltools::tags$li(sprintf(
        "Water stress index averaged %.2f over the season (1.0 = no stress, 0 = maximum stress); it dropped as low as %.2f on the driest days, with %d day(s) below the 0.8 comfort threshold.",
        wsi_mean, wsi_min, stress_days
      )),
      htmltools::tags$li(stress_txt),
      htmltools::tags$li(soil_txt),
      if (!is.null(calibration_txt)) htmltools::tags$li(calibration_txt)
    )
  ) %>% as.character()
}

# Renders the HTML report to a temp file and returns its path.
render_report <- function(results, input_snapshot, out_file) {
  rmarkdown::render(
    input = "inst/report_template.Rmd",
    output_file = out_file,
    params = list(
      summary_tbl = input_snapshot$summary_tbl,
      climate_summary_tbl = input_snapshot$climate_summary_tbl,
      soil_profile_tbl = input_snapshot$soil_profile_tbl,
      soil_agg_tbl = input_snapshot$soil_agg_tbl,
      plain_language_html = explain_plain_language(results),
      equation = attr(results$pred, "equation"),
      pred_plot = input_snapshot$pred_plot,
      pred_tbl = input_snapshot$pred_tbl
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  out_file
}

# CSV export of the day-by-day prediction series.
pred_to_csv <- function(results, out_file) {
  results$pred %>%
    select(date, pred, pred_lower, pred_upper, pred_fw, pred_fw_lower, pred_fw_upper,
           GDD_cum, GDD_eff, WSI, precip_mm, irrigation_mm, et_mm) %>%
    mutate(date = as.Date(date)) %>%
    write.csv(out_file, row.names = FALSE)
  out_file
}
