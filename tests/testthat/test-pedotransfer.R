test_that("rawls1982_theta matches the tabulated Rawls et al. (1982) coefficients directly", {
  # theta_330 = 0.2576 - 0.0020*sand + 0.0036*clay + 0.0299*OM (silt coefficient is 0)
  expect_equal(
    rawls1982_theta(sand_pct = 40, silt_pct = 40, clay_pct = 20, om_pct = 2, h_cm = "330"),
    0.2576 - 0.0020 * 40 + 0.0036 * 20 + 0.0299 * 2,
    tolerance = 1e-10
  )
  # theta_15000 = 0.0260 + 0.0050*clay + 0.0158*OM (sand and silt coefficients are both 0)
  expect_equal(
    rawls1982_theta(sand_pct = 40, silt_pct = 40, clay_pct = 20, om_pct = 2, h_cm = "15000"),
    0.0260 + 0.0050 * 20 + 0.0158 * 2,
    tolerance = 1e-10
  )
})

test_that("rawls1982_theta errors on an untabulated capillary pressure", {
  expect_error(rawls1982_theta(40, 40, 20, 2, h_cm = "999"), "no coefficients tabulated")
})

test_that("oc_pct_to_om_pct applies the van Bemmelen factor", {
  expect_equal(oc_pct_to_om_pct(2), 2 * 1.724)
})

test_that("soil_water_from_texture returns field capacity > wilting point for a plausible loam", {
  out <- soil_water_from_texture(sand_pct = 40, silt_pct = 40, clay_pct = 20, oc_pct = 2 / 1.724)
  expect_true(out$SDUL > out$SLLL)
  expect_equal(out$SDUL, 0.3094, tolerance = 1e-4)
  expect_equal(out$SLLL, 0.1576, tolerance = 1e-4)
})

test_that("soil_water_from_texture clamps extreme texture combinations to a physically sane range", {
  # a pathological input the raw linear regression could push out of [0,1]
  out <- soil_water_from_texture(sand_pct = 0, silt_pct = 0, clay_pct = 100, oc_pct = 20)
  expect_true(out$SDUL >= 0.01 && out$SDUL <= 0.6)
  expect_true(out$SLLL >= 0.01 && out$SLLL <= 0.6)
})

test_that("soil_water_from_texture is vectorized over multiple horizons", {
  out <- soil_water_from_texture(
    sand_pct = c(40, 80), silt_pct = c(40, 10), clay_pct = c(20, 10), oc_pct = c(1.2, 0.5)
  )
  expect_length(out$SDUL, 2)
  expect_length(out$SLLL, 2)
  expect_true(all(out$SDUL > out$SLLL))
})
