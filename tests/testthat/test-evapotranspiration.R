test_that("compute_et0_pm reproduces FAO-56's own worked example (Example 18, Brussels)", {
  # Allen et al. (1998), FAO Irrigation and Drainage Paper 56, ch. 4, Example 18:
  # tmax=21.5, tmin=12.3, RHmax=84, RHmin=63, wind (10m)->2.078 m/s at 2m,
  # rs=22.07 MJ/m2/day (from sunshine hours), lat=50.80N, elev=100m, DOY=187.
  # Published ETo = 3.9 mm/day, using ea from RHmax/RHmin separately (FAO-56 eq 17).
  # We only have POWER's single daily-mean RH2M, so we use FAO-56 eq 19's
  # mean-RH fallback instead -- expect to land close to, not exactly at, 3.9.
  et0 <- compute_et0_pm(
    tmax_c = 21.5, tmin_c = 12.3, rh_mean_pct = (84 + 63) / 2,
    wind2m_ms = 2.078, rs_mj = 22.07, lat_deg = 50.80, elev_m = 100, doy = 187
  )
  expect_equal(et0, 3.9, tolerance = 0.05)  # ~3% off is the expected mean-RH-vs-RHmax/RHmin precision gap
})

test_that("compute_et0_pm is always non-negative and responds physically to its inputs", {
  base <- compute_et0_pm(tmax_c = 30, tmin_c = 20, rh_mean_pct = 60,
                          wind2m_ms = 2, rs_mj = 22, lat_deg = 35.3, elev_m = 20, doy = 180)
  expect_true(base > 0)

  more_radiation <- compute_et0_pm(tmax_c = 30, tmin_c = 20, rh_mean_pct = 60,
                                    wind2m_ms = 2, rs_mj = 28, lat_deg = 35.3, elev_m = 20, doy = 180)
  expect_true(more_radiation > base)

  more_wind <- compute_et0_pm(tmax_c = 30, tmin_c = 20, rh_mean_pct = 60,
                               wind2m_ms = 5, rs_mj = 22, lat_deg = 35.3, elev_m = 20, doy = 180)
  expect_true(more_wind > base)

  drier_air <- compute_et0_pm(tmax_c = 30, tmin_c = 20, rh_mean_pct = 30,
                               wind2m_ms = 2, rs_mj = 22, lat_deg = 35.3, elev_m = 20, doy = 180)
  expect_true(drier_air > base)

  # fully saturated air with zero wind and modest radiation should still be non-negative
  no_deficit <- compute_et0_pm(tmax_c = 20, tmin_c = 20, rh_mean_pct = 100,
                                wind2m_ms = 0, rs_mj = 5, lat_deg = 35.3, elev_m = 20, doy = 180)
  expect_true(no_deficit >= 0)
})

test_that("compute_et0_pm is vectorized over a daily series", {
  out <- compute_et0_pm(
    tmax_c = c(28, 32), tmin_c = c(18, 22), rh_mean_pct = c(65, 55),
    wind2m_ms = c(1.5, 2.5), rs_mj = c(20, 24), lat_deg = 35.3, elev_m = 20, doy = c(160, 161)
  )
  expect_length(out, 2)
  expect_true(all(out > 0))
})
