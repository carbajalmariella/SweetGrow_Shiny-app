make_synthetic_results <- function(wsi_values, pred_values, soil_source = "SSURGO (dominant component)",
                                    season_used = NULL, season_auto_selected = NULL,
                                    pred_lower = NA_real_, pred_upper = NA_real_, band_is_bootstrap = FALSE,
                                    pred_fw = NA_real_, cultivar = NULL) {
  list(
    df = tibble::tibble(WSI = wsi_values),
    pred = tibble::tibble(
      date = as.Date("2024-06-01") + seq_along(pred_values) - 1,
      pred = pred_values,
      pred_lower = pred_lower,
      pred_upper = pred_upper,
      pred_fw = pred_fw
    ),
    soil_agg = list(SLLL = 0.11, SDUL = 0.24, bulk_density_g_cm3 = 1.6, soil_source = soil_source),
    root_depth = 60,
    season_used = season_used,
    season_auto_selected = season_auto_selected,
    band_is_bootstrap = band_is_bootstrap,
    cultivar = cultivar
  )
}

test_that("explain_plain_language warns about Bellevue's unreliable cross-trial transfer", {
  res <- make_synthetic_results(wsi_values = c(1, 1, 0.9), pred_values = c(1, 2, 5.5), cultivar = "Bellevue")
  html <- explain_plain_language(res)
  expect_match(html, "Low reliability for Bellevue", fixed = TRUE)
})

test_that("explain_plain_language omits the Bellevue warning for other cultivars", {
  res <- make_synthetic_results(wsi_values = c(1, 1, 0.9), pred_values = c(1, 2, 5.5), cultivar = "Covington")
  html <- explain_plain_language(res)
  expect_false(grepl("Low reliability for Bellevue", html, fixed = TRUE))
})

test_that("explain_plain_language reports the harvest-date prediction and soil inputs", {
  res <- make_synthetic_results(wsi_values = c(1, 1, 0.9, 0.5, 0.3), pred_values = c(1, 2, 3, 4, 5.5))
  html <- explain_plain_language(res)

  expect_type(html, "character")
  expect_match(html, "Predicted storage root dry weight at harvest: 5.5 t/ha", fixed = TRUE)
  expect_match(html, "wilting point of 0.11 and a field capacity of 0.24", fixed = TRUE)
  expect_match(html, "60 cm rooting depth", fixed = TRUE)
  expect_match(html, "SSURGO (dominant component)", fixed = TRUE)
})

test_that("explain_plain_language reports estimated fresh yield when pred_fw is present", {
  res <- make_synthetic_results(wsi_values = c(1, 1, 0.9), pred_values = c(1, 2, 5.5),
                                 pred_fw = c(3.7, 7.4, 20.37))
  html <- explain_plain_language(res)
  expect_match(html, "20.4 t/ha of estimated fresh", fixed = TRUE)
})

test_that("explain_plain_language omits the fresh-yield line when pred_fw is all NA", {
  res <- make_synthetic_results(wsi_values = c(1, 1, 0.9), pred_values = c(1, 2, 5.5))
  html <- explain_plain_language(res)
  expect_false(grepl("estimated fresh", html, fixed = TRUE))
})

test_that("explain_plain_language picks the right stress narrative for a low-stress season", {
  res <- make_synthetic_results(wsi_values = rep(0.95, 10), pred_values = c(1, 2))
  html <- explain_plain_language(res)
  expect_match(html, "very little water stress", fixed = TRUE)
})

test_that("explain_plain_language picks the right stress narrative for a high-stress season", {
  res <- make_synthetic_results(wsi_values = rep(0.2, 10), pred_values = c(1, 2))
  html <- explain_plain_language(res)
  expect_match(html, "substantial water stress", fixed = TRUE)
})

test_that("explain_plain_language explains an auto-selected calibration", {
  res <- make_synthetic_results(wsi_values = rep(0.2, 10), pred_values = c(1, 2),
                                 season_used = "Caswell 2021", season_auto_selected = TRUE)
  html <- explain_plain_language(res)
  expect_match(html, "Growth calibration: Caswell 2021", fixed = TRUE)
  expect_match(html, "picked automatically", fixed = TRUE)
})

test_that("explain_plain_language labels a manually-chosen calibration differently", {
  res <- make_synthetic_results(wsi_values = rep(0.2, 10), pred_values = c(1, 2),
                                 season_used = "Sandhills 2022", season_auto_selected = FALSE)
  html <- explain_plain_language(res)
  expect_match(html, "Growth calibration: Sandhills 2022 (manually selected)", fixed = TRUE)
})

test_that("explain_plain_language omits the calibration line when season_used is absent", {
  res <- make_synthetic_results(wsi_values = rep(0.2, 10), pred_values = c(1, 2))
  html <- explain_plain_language(res)
  expect_false(grepl("Growth calibration", html, fixed = TRUE))
})

test_that("explain_plain_language reports the uncertainty range when the band is present (OAT phrasing)", {
  res <- make_synthetic_results(wsi_values = rep(0.5, 5), pred_values = c(1, 2, 3),
                                 pred_lower = c(0.8, 1.7, 2.5), pred_upper = c(1.2, 2.3, 3.6))
  html <- explain_plain_language(res)
  expect_match(html, "rough uncertainty range of 2.5 to 3.6", fixed = TRUE)
})

test_that("explain_plain_language uses bootstrap phrasing when band_is_bootstrap is TRUE", {
  res <- make_synthetic_results(wsi_values = rep(0.5, 5), pred_values = c(1, 2, 3),
                                 pred_lower = c(0.8, 1.7, 2.5), pred_upper = c(1.2, 2.3, 3.6),
                                 band_is_bootstrap = TRUE)
  html <- explain_plain_language(res)
  expect_match(html, "50% bootstrap uncertainty range (interquartile) of 2.5 to 3.6", fixed = TRUE)
})

test_that("explain_plain_language omits the uncertainty line when the band is all NA", {
  res <- make_synthetic_results(wsi_values = rep(0.5, 5), pred_values = c(1, 2, 3))
  html <- explain_plain_language(res)
  expect_false(grepl("uncertainty range", html, fixed = TRUE))
})
