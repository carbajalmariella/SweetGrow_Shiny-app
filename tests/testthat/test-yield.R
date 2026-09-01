test_that("dw_to_fw divides by the cultivar's dry-matter fraction", {
  expect_equal(dw_to_fw(10, "Covington"), 10 / 0.198, tolerance = 1e-8)
  expect_equal(dw_to_fw(c(1, 2, 3), "Average"), c(1, 2, 3) / 0.237, tolerance = 1e-8)
})

test_that("dw_to_fw returns NA for an unknown cultivar instead of erroring", {
  expect_true(all(is.na(dw_to_fw(c(1, 2), "Not A Real Cultivar"))))
})

test_that("every DRY_MATTER_FRACTION value is a plausible dry-matter percentage", {
  expect_true(all(DRY_MATTER_FRACTION > 0 & DRY_MATTER_FRACTION < 1))
})
