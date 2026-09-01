test_that("power_json_to_df reshapes the POWER JSON payload into a tidy daily data.frame", {
  js <- list(properties = list(parameter = list(
    T2M_MAX = c("20240601" = 25.5, "20240602" = 26.0),
    T2M_MIN = c("20240601" = 15.5, "20240602" = 16.0),
    PRECTOTCORR = c("20240601" = 0, "20240602" = 5.2),
    RH2M = c("20240601" = 70, "20240602" = 72),
    WS2M = c("20240601" = 2.1, "20240602" = 2.3),
    ALLSKY_SFC_SW_DWN = c("20240601" = 20.5, "20240602" = 21.0)
  )))

  out <- power_json_to_df(js)

  expect_equal(out$date, as.Date(c("2024-06-01", "2024-06-02")))
  expect_equal(out$tmax_c, c(25.5, 26.0))
  expect_equal(out$tmin_c, c(15.5, 16.0))
  expect_equal(out$precip_mm, c(0, 5.2))
  expect_equal(out$rh_pct, c(70, 72))
  expect_equal(out$wind2m_ms, c(2.1, 2.3))
  expect_equal(out$rs_mj, c(20.5, 21.0))
})

test_that("clean_power_daily_strict flags POWER's -999 sentinel as NA across all raw fields", {
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + 0:2,
    tmax_c = c(25, -999, 27),
    tmin_c = c(15, 16, 17),
    precip_mm = c(0, 0, 0),
    rh_pct = c(70, -999, 72),
    wind2m_ms = c(2, 2, 2),
    rs_mj = c(20, 20, 20)
  )
  out <- clean_power_daily_strict(df)
  expect_true(is.na(out$tmax_c[2]))
  expect_true(is.na(out$rh_pct[2]))
})

test_that("clean_power_daily_strict truncates the trailing run of missing days after data starts", {
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + 0:6,
    tmax_c = c(-999, -999, 25, 26, 27, -999, -999),
    tmin_c = rep(15, 7),
    precip_mm = rep(0, 7),
    rh_pct = rep(70, 7),
    wind2m_ms = rep(2, 7),
    rs_mj = rep(20, 7)
  )
  out <- clean_power_daily_strict(df)

  expect_equal(nrow(out), 5)
  expect_equal(out$tmax_c[5], 27)
  expect_true(all(is.na(out$tmax_c[1:2])))
})

test_that("clean_power_daily_strict returns an empty frame when there is no valid data at all", {
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + 0:2,
    tmax_c = rep(-999, 3),
    tmin_c = rep(-999, 3),
    precip_mm = rep(0, 3),
    rh_pct = rep(-999, 3),
    wind2m_ms = rep(-999, 3),
    rs_mj = rep(-999, 3)
  )
  out <- clean_power_daily_strict(df)
  expect_equal(nrow(out), 0)
})

test_that("add_et0 computes a positive ET0 from raw meteorological fields and drops the doy helper column", {
  df <- tibble::tibble(
    date = as.Date("2024-07-05") + 0:1,
    tmax_c = c(30, 31), tmin_c = c(20, 21),
    rh_pct = c(70, 65), wind2m_ms = c(2, 2.5), rs_mj = c(22, 23)
  )
  out <- add_et0(df, lat = 35.3, elev_m = 20)

  expect_true("et_mm" %in% names(out))
  expect_false("doy" %in% names(out))
  expect_true(all(out$et_mm > 0))
})

test_that("add_et0 propagates NA meteorological inputs to NA et_mm instead of silently guessing", {
  df <- tibble::tibble(
    date = as.Date("2024-07-05"),
    tmax_c = 30, tmin_c = 20,
    rh_pct = NA_real_, wind2m_ms = 2, rs_mj = 22
  )
  out <- add_et0(df, lat = 35.3, elev_m = 20)
  expect_true(is.na(out$et_mm))
})
