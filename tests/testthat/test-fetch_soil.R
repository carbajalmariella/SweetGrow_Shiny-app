test_that("is_us_coordinate recognizes CONUS, Alaska and Hawaii", {
  expect_true(is_us_coordinate(35.7796, -78.6382))   # Raleigh, NC
  expect_true(is_us_coordinate(41.878, -93.098))      # rural Iowa
  expect_true(is_us_coordinate(64.2, -149.4))         # Fairbanks, AK
  expect_true(is_us_coordinate(21.3, -157.8))         # Honolulu, HI
})

test_that("is_us_coordinate rejects clearly non-US locations", {
  expect_false(is_us_coordinate(40.4168, -3.7038))    # Madrid, Spain
  expect_false(is_us_coordinate(-12.05, -77.03))      # Lima, Peru
  expect_false(is_us_coordinate(35.6762, 139.6503))   # Tokyo, Japan
})

test_that("aggregate_soil_to_depth computes a thickness-weighted average and clips at the target depth", {
  soil_df <- tibble::tibble(
    depth_top_cm = c(0, 10, 20),
    depth_bot_cm = c(10, 20, 40),
    SLLL = c(0.10, 0.15, 0.20),
    SDUL = c(0.25, 0.30, 0.35),
    bulk_density_g_cm3 = c(1.4, 1.5, 1.6),
    soil_source = "TEST"
  )

  out <- aggregate_soil_to_depth(soil_df, target_depth_cm = 25)

  expect_equal(out$agg_depth_cm, 25)
  expect_equal(out$soil_source, "TEST")
  expect_equal(out$SLLL, 0.14, tolerance = 1e-8)
  expect_equal(out$SDUL, 0.29, tolerance = 1e-8)
  expect_equal(out$bulk_density_g_cm3, 1.48, tolerance = 1e-8)
})

test_that("aggregate_soil_to_depth errors when no horizon overlaps the target depth", {
  soil_df <- tibble::tibble(
    depth_top_cm = 100, depth_bot_cm = 150,
    SLLL = 0.1, SDUL = 0.3, bulk_density_g_cm3 = 1.5, soil_source = "TEST"
  )
  expect_error(aggregate_soil_to_depth(soil_df, target_depth_cm = 30), "No horizons overlap")
})
