test_that("GDD2_C caps at the thermal ceiling and floors below base temp", {
  expect_equal(GDD2_C(35, 20), 29.2 - 16.9)   # tmax above ceiling, tmin above base
  expect_equal(GDD2_C(35, 10), 0)             # tmax above ceiling, tmin below base -> 0
  expect_equal(GDD2_C(25, 18), 25 - 16.9)     # normal case, both within range
  expect_equal(GDD2_C(25, 10), 0)             # tmin below base -> 0
  expect_true(is.na(GDD2_C(NA, 20)))
  expect_true(is.na(GDD2_C(25, NA)))
})

test_that("compute_gdd2 accumulates daily GDD correctly", {
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + 0:3,
    tmax_in = c(25, 35, 25, 10),
    tmin_in = c(18, 20, 10, 5)
  )
  out <- compute_gdd2(df, temp_unit = "C")

  expect_equal(out$gdd, c(25 - 16.9, 29.2 - 16.9, 0, 0))
  expect_equal(out$GDD_cum, cumsum(out$gdd))
})

test_that("compute_gdd2 converts Fahrenheit inputs before computing GDD", {
  df_c <- tibble::tibble(date = as.Date("2024-06-01"), tmax_in = 25, tmin_in = 18)
  df_f <- tibble::tibble(date = as.Date("2024-06-01"), tmax_in = c_to_f(25), tmin_in = c_to_f(18))

  out_c <- compute_gdd2(df_c, temp_unit = "C")
  out_f <- compute_gdd2(df_f, temp_unit = "F")

  expect_equal(out_c$gdd, out_f$gdd, tolerance = 1e-8)
})

test_that("fw_trap is flat below w1, flat at 1 above w2, and linear between", {
  w1 <- 0.25; w2 <- 0.55; f_min <- 0.55

  expect_equal(fw_trap(0.1, w1, w2, f_min), f_min)
  expect_equal(fw_trap(0.6, w1, w2, f_min), 1)
  expect_equal(
    fw_trap(0.4, w1, w2, f_min),
    f_min + (1 - f_min) * (0.4 - w1) / (w2 - w1)
  )
  # vectorized
  expect_equal(fw_trap(c(0, 1), w1, w2, f_min), c(f_min, 1))
})

test_that("make_irrigation_schedule fires on the right day offsets", {
  dates <- as.Date("2024-06-01") + 0:13
  sched <- make_irrigation_schedule(dates, planting_date = as.Date("2024-06-01"),
                                     every_n = 7, amount_mm = 10, offset_days = 0)

  expect_equal(sched$irrigation_mm[c(1, 8)], c(10, 10))       # day 0 and day 7
  expect_true(all(sched$irrigation_mm[-c(1, 8)] == 0))
})

test_that("make_irrigation_schedule respects a start offset", {
  dates <- as.Date("2024-06-01") + 0:9
  sched <- make_irrigation_schedule(dates, planting_date = as.Date("2024-06-01"),
                                     every_n = 5, amount_mm = 20, offset_days = 2)

  expect_true(all(sched$irrigation_mm[1:2] == 0))              # before offset
  expect_equal(sched$irrigation_mm[3], 20)                     # day 2 (offset)
  expect_equal(sched$irrigation_mm[8], 20)                     # day 7 (offset + every_n)
})

test_that("compute_wsi_daily stays within [0, 1] and rejects invalid soil params", {
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + 0:9,
    water = c(20, 0, 0, 0, 0, 0, 0, 30, 0, 0),
    ET = rep(5, 10)
  )

  out <- compute_wsi_daily(df, root_depth_cm = 60, SLLL = 0.1, SDUL = 0.3, BD = 1.5)
  expect_true(all(out$WSI >= 0 & out$WSI <= 1))
  expect_true(all(c("Available_water", "Theta_rel", "WSI") %in% names(out)))

  expect_error(compute_wsi_daily(df, 60, SLLL = 0.3, SDUL = 0.3, BD = 1.5), "SDUL must be > SLLL")
  expect_error(compute_wsi_daily(df, 0, SLLL = 0.1, SDUL = 0.3, BD = 1.5), "root_depth_cm must be > 0")
  expect_error(compute_wsi_daily(df, 60, SLLL = 0.1, SDUL = 0.3, BD = 0), "Bulk density must be > 0")
})

test_that("compute_wsi_daily's tipping bucket applies the cm-to-mm unit fix and caps at AW_max", {
  # AW_max = (SDUL - SLLL) * root_depth_cm * BD * 10 = 0.2 * 60 * 1.5 * 10 = 180 mm.
  # Without the *10 fix this would be 18 mm and every one of these rain events would
  # instantly saturate the bucket -- that's the bug the manuscript repo already fixed.
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + 0:9,
    water = c(20, 0, 0, 0, 0, 0, 0, 30, 0, 0),
    ET = rep(5, 10)
  )
  out <- compute_wsi_daily(df, root_depth_cm = 60, SLLL = 0.1, SDUL = 0.3, BD = 1.5)

  expect_equal(out$Available_water, c(15, 10, 5, 0, 0, 0, 0, 25, 20, 15))
  expect_equal(out$WSI_daily, c(15, 10, 5, 0, 0, 0, 0, 25, 20, 15) / 180)
})

test_that("compute_wsi_daily's bucket never goes negative or exceeds AW_max mid-season (day-by-day recursion, not a single cumsum)", {
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + 0:4,
    water = c(0, 0, 0, 500, 0),   # a big deficit, then a huge rain event
    ET = rep(10, 5)
  )
  out <- compute_wsi_daily(df, root_depth_cm = 60, SLLL = 0.1, SDUL = 0.3, BD = 1.5)

  expect_true(all(out$Available_water >= 0))
  expect_true(all(out$Available_water <= 180 + 1e-8))
  # the big rain event should refill the bucket immediately, not stay depressed
  # while an old cumulative deficit gets "paid off" in the background
  expect_equal(out$Available_water[4], 180)
})

test_that("kc_ramp reproduces the FAO-56-style ramp breakpoints", {
  expect_equal(kc_ramp(0), 0.47)
  expect_equal(kc_ramp(0.15), 0.47)
  expect_equal(kc_ramp(0.70), 0.97, tolerance = 1e-8)   # mid_end: ramp reaches kc_mid
  expect_equal(kc_ramp(0.80), 0.97)
  expect_equal(kc_ramp(1.0), 0.44, tolerance = 1e-8)    # late_end: ramp reaches kc_end
})

test_that("compute_wsi_daily scales ET by kc_ramp(DAT fraction) when kc_fun is supplied", {
  df <- tibble::tibble(DAT = 0:2, water = c(10, 10, 10), ET = c(5, 5, 5))
  out <- compute_wsi_daily(df, root_depth_cm = 60, SLLL = 0.1, SDUL = 0.3, BD = 1.5, kc_fun = kc_ramp)

  expect_equal(out$Available_water, c(7.65, 13.70909, 21.50909), tolerance = 1e-4)
})
