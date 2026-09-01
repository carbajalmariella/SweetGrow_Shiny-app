test_that("predict_growth_band returns NA for piecewise when the catalog row has no breakpoint CI", {
  df_stress <- tibble::tibble(GDD_eff = c(0, 100, 200), GDD_cum = c(0, 100, 200), WSI = 1)
  row <- tibble::tibble(m1 = 0.01, bp = 100, b2 = -1, m2 = 0.02)
  band <- predict_growth_band(df_stress, row, "piecewise", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05)
  expect_true(all(is.na(band$lower)))
  expect_true(all(is.na(band$upper)))
})

test_that("predict_growth_band brackets the point estimate for piecewise with a real breakpoint CI", {
  df_stress <- tibble::tibble(GDD_eff = seq(0, 400, by = 50), GDD_cum = seq(0, 400, by = 50), WSI = 1)
  m1 <- 0.01; bp <- 200; m2 <- 0.02
  b2 <- m1 * bp - m2 * bp  # continuity-consistent, like a real fitted catalog row
  row <- tibble::tibble(m1 = m1, bp = bp, b2 = b2, m2 = m2, bp_lower = 150, bp_upper = 250)
  point <- piecewise_fun(df_stress$GDD_eff, row$m1, row$bp, row$b2, row$m2)
  band <- predict_growth_band(df_stress, row, "piecewise", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05)

  expect_true(all(band$lower <= point + 1e-8))
  expect_true(all(band$upper >= point - 1e-8))
})

test_that("predict_growth_band returns NA for logistic when the catalog row has no parameter CI", {
  df_stress <- tibble::tibble(GDD_eff = c(0, 100, 200), GDD_cum = c(0, 100, 200), WSI = 1)
  row <- tibble::tibble(L = 10, k = 0.01, x0 = 500)
  band <- predict_growth_band(df_stress, row, "logistic", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05)
  expect_true(all(is.na(band$lower)))
})

test_that("predict_growth_band envelopes the point estimate for logistic with a real parameter CI", {
  df_stress <- tibble::tibble(GDD_cum = seq(0, 1000, by = 100), GDD_eff = seq(0, 1000, by = 100), WSI = 1)
  row <- tibble::tibble(L = 10, k = 0.01, x0 = 500,
                         L_lower = 8, L_upper = 12, k_lower = 0.008, k_upper = 0.012,
                         x0_lower = 450, x0_upper = 550)
  point <- logistic_fun(df_stress$GDD_cum, row$L, row$k, row$x0)
  band <- predict_growth_band(df_stress, row, "logistic", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05)

  expect_true(all(band$lower <= point + 1e-8))
  expect_true(all(band$upper >= point - 1e-8))
  expect_true(all(band$lower <= band$upper))
})

test_that("load_lgg_boot_draws returns NULL for a missing file instead of erroring", {
  expect_null(load_lgg_boot_draws("no/such/file.rds"))
})

test_that("predict_growth_band uses boot_draws (joint) over the marginal-CI OAT fallback when both are present", {
  df_stress <- tibble::tibble(GDD_cum = seq(0, 1000, by = 100), GDD_eff = seq(0, 1000, by = 100), WSI = 1)
  row <- tibble::tibble(L = 10, k = 0.01, x0 = 500,
                         L_lower = 1, L_upper = 100, k_lower = 0.0001, k_upper = 1,
                         x0_lower = -1000, x0_upper = 5000)  # deliberately huge OAT band
  # tight boot draws clustered right around the point estimate
  boot_draws <- tibble::tibble(a = row$L + c(-0.1, 0.1), b = row$k + c(-0.0001, 0.0001), c = row$x0 + c(-5, 5))

  band_boot <- predict_growth_band(df_stress, row, "logistic", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05, boot_draws = boot_draws)
  band_oat  <- predict_growth_band(df_stress, row, "logistic", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05, boot_draws = NULL)

  expect_true(all((band_boot$upper - band_boot$lower) < (band_oat$upper - band_oat$lower)))
})

test_that("predict_growth_band's bootstrap band defaults to the 25th/75th percentile (band_level = 0.5)", {
  df_stress <- tibble::tibble(GDD_cum = 500, GDD_eff = 500, WSI = 1)
  # 100 draws with a known, evenly-spread L so the quantiles are checkable
  boot_draws <- tibble::tibble(a = seq(1, 100, by = 1), b = 0.01, c = 500)
  band <- predict_growth_band(df_stress, tibble::tibble(L = 50, k = 0.01, x0 = 500), "logistic", FALSE,
                               0.25, 0.55, 0.55, FALSE, 0.05, boot_draws = boot_draws)

  vals <- sort(logistic_fun(500, boot_draws$a, boot_draws$b, boot_draws$c))
  expect_equal(band$lower, unname(quantile(vals, 0.25)), tolerance = 1e-6)
  expect_equal(band$upper, unname(quantile(vals, 0.75)), tolerance = 1e-6)
})

test_that("predict_growth_band's bootstrap band widens for a higher band_level", {
  df_stress <- tibble::tibble(GDD_cum = 500, GDD_eff = 500, WSI = 1)
  boot_draws <- tibble::tibble(a = seq(1, 100, by = 1), b = 0.01, c = 500)
  row <- tibble::tibble(L = 50, k = 0.01, x0 = 500)

  band_50 <- predict_growth_band(df_stress, row, "logistic", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05,
                                  boot_draws = boot_draws, band_level = 0.5)
  band_95 <- predict_growth_band(df_stress, row, "logistic", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05,
                                  boot_draws = boot_draws, band_level = 0.95)

  expect_true((band_95$upper - band_95$lower) > (band_50$upper - band_50$lower))

  vals <- sort(logistic_fun(500, boot_draws$a, boot_draws$b, boot_draws$c))
  expect_equal(band_95$lower, unname(quantile(vals, 0.025)), tolerance = 1e-6)
  expect_equal(band_95$upper, unname(quantile(vals, 0.975)), tolerance = 1e-6)
})

test_that("predict_growth attaches pred_lower/pred_upper alongside pred", {
  df_stress <- tibble::tibble(GDD_cum = seq(0, 1000, by = 100), GDD_eff = seq(0, 1000, by = 100), WSI = 1)
  row <- tibble::tibble(L = 10, k = 0.01, x0 = 500,
                         L_lower = 8, L_upper = 12, k_lower = 0.008, k_upper = 0.012,
                         x0_lower = 450, x0_upper = 550,
                         m1 = 0.01, bp = 500, b2 = -1, m2 = 0.02,
                         logistic_eq = "eq", piecewise_eq = "eq")
  out <- predict_growth(df_stress, row, "logistic", FALSE, 0.25, 0.55, 0.55, FALSE, 0.05)
  expect_true(all(c("pred_lower", "pred_upper") %in% names(out)))
  expect_true(all(out$pred_lower <= out$pred & out$pred <= out$pred_upper))
})

test_that("pick_season_by_wsi matches whichever trial's realized water stress is closer", {
  expect_equal(pick_season_by_wsi(0.10), "Caswell 2021")     # clearly rain-stressed, like Caswell
  expect_equal(pick_season_by_wsi(WSI_REF_CASWELL), "Caswell 2021")
  expect_equal(pick_season_by_wsi(0.95), "Sandhills 2022")   # clearly buffered, like Sandhills
  expect_equal(pick_season_by_wsi(WSI_REF_SANDHILLS), "Sandhills 2022")

  midpoint <- (WSI_REF_CASWELL + WSI_REF_SANDHILLS) / 2
  expect_equal(pick_season_by_wsi(midpoint - 0.01), "Caswell 2021")
  expect_equal(pick_season_by_wsi(midpoint + 0.01), "Sandhills 2022")
})

test_that("logistic_fun and logistic_deriv are consistent (derivative matches slope)", {
  L <- 15; k <- 0.01; x0 <- 500

  expect_equal(logistic_fun(x0, L, k, x0), L / 2)

  # numeric derivative should match the analytic logistic_deriv
  h <- 1e-4
  x <- 300
  numeric_slope <- (logistic_fun(x + h, L, k, x0) - logistic_fun(x - h, L, k, x0)) / (2 * h)
  expect_equal(logistic_deriv(x, L, k, x0), numeric_slope, tolerance = 1e-4)
})

test_that("piecewise_fun switches slope at the breakpoint and is continuous when fit that way", {
  m1 <- 0.01; bp <- 500; m2 <- 0.03
  b2 <- m1 * bp - m2 * bp  # forces continuity at bp, as the fitted catalog values do

  expect_equal(piecewise_fun(bp, m1, bp, b2, m2), m1 * bp)
  expect_equal(piecewise_fun(bp - 1, m1, bp, b2, m2), m1 * (bp - 1))
  expect_equal(piecewise_fun(bp + 1, m1, bp, b2, m2), b2 + m2 * (bp + 1))
  # continuous at the breakpoint
  expect_equal(piecewise_fun(bp, m1, bp, b2, m2), piecewise_fun(bp + 1e-9, m1, bp, b2, m2), tolerance = 1e-6)
})

test_that("predict_growth dispatches to piecewise on GDD_eff and tags the right equation", {
  df_stress <- tibble::tibble(
    GDD_cum = c(0, 100, 200),
    GDD_eff = c(0, 90, 180),
    WSI = c(1, 1, 1)
  )
  row <- tibble::tibble(
    L = 10, k = 0.01, x0 = 500,
    m1 = 0.01, bp = 100, b2 = -1, m2 = 0.02,
    logistic_eq = "logistic-eq-text",
    piecewise_eq = "piecewise-eq-text"
  )

  out <- predict_growth(df_stress, row, model_type = "piecewise", apply_stress = FALSE,
                         w1 = 0.25, w2 = 0.55, fmin = 0.55, use_fw_trapz = FALSE, y0 = 0.05)

  expect_equal(out$pred, piecewise_fun(df_stress$GDD_eff, row$m1, row$bp, row$b2, row$m2))
  expect_equal(attr(out, "equation"), "piecewise-eq-text")
})

test_that("predict_growth dispatches to logistic on GDD_cum without stress", {
  df_stress <- tibble::tibble(
    GDD_cum = c(0, 250, 500, 750),
    GDD_eff = c(0, 250, 500, 750),
    WSI = c(1, 1, 1, 1)
  )
  row <- tibble::tibble(
    L = 10, k = 0.01, x0 = 500,
    m1 = 0.01, bp = 100, b2 = -1, m2 = 0.02,
    logistic_eq = "logistic-eq-text",
    piecewise_eq = "piecewise-eq-text"
  )

  out <- predict_growth(df_stress, row, model_type = "logistic", apply_stress = FALSE,
                         w1 = 0.25, w2 = 0.55, fmin = 0.55, use_fw_trapz = FALSE, y0 = 0.05)

  expect_equal(out$pred, logistic_fun(df_stress$GDD_cum, row$L, row$k, row$x0))
  expect_equal(attr(out, "equation"), "logistic-eq-text")
})

test_that("predict_growth under water stress never exceeds the unstressed logistic prediction", {
  df_stress <- tibble::tibble(
    GDD_cum = seq(0, 900, by = 30),
    GDD_eff = seq(0, 900, by = 30),
    WSI = rep(0.2, 31)  # constant, fairly severe stress
  )
  row <- tibble::tibble(
    L = 10, k = 0.01, x0 = 500,
    m1 = 0.01, bp = 100, b2 = -1, m2 = 0.02,
    logistic_eq = "logistic-eq-text",
    piecewise_eq = "piecewise-eq-text"
  )

  stressed <- predict_growth(df_stress, row, model_type = "logistic", apply_stress = TRUE,
                              w1 = 0.25, w2 = 0.55, fmin = 0.55, use_fw_trapz = FALSE, y0 = 0.05)
  unstressed <- logistic_fun(df_stress$GDD_cum, row$L, row$k, row$x0)

  expect_true(all(stressed$pred <= unstressed + 1e-8))
})
