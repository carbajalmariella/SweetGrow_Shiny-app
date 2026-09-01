test_that("kv_table formats dates, logicals and numerics into printable strings", {
  out <- kv_table(list(
    "A date" = as.Date("2024-06-01"),
    "A flag" = TRUE,
    "A number" = 3.14159
  ), digits = 2)

  expect_equal(out$Metric, c("A date", "A flag", "A number"))
  expect_equal(out$Value, c("2024-06-01", "Yes", "3.14"))
})

test_that("fmt2 rounds numerics and leaves other types untouched", {
  expect_equal(fmt2(3.14159), 3.14)
  expect_equal(fmt2("text"), "text")
})

test_that("pick_pred_at_date finds an exact match and the nearest date otherwise", {
  df <- tibble::tibble(
    date = as.Date("2024-06-01") + c(0, 5, 10),
    pred = c(1, 2, 3)
  )

  expect_equal(pick_pred_at_date(df, as.Date("2024-06-06")), 2)         # exact match
  expect_equal(pick_pred_at_date(df, as.Date("2024-06-08")), 2)         # closer to day 5 than day 10
  expect_equal(pick_pred_at_date(df, as.Date("2024-06-09")), 3)         # closer to day 10
})
