test_that("Various minor functions", {
  cf <- CFtime("days since 2001-01-01", "standard", 0:999)
  cf3 <- CFtime("days since 2001-01-01", "360_day", 0:999)
  cf4 <- CFtime("days since 2001-01-01", "365_day", 0:999)
  cf5 <- CFtime("days since 2001-01-01", "366_day", 0:999)

  # Days in a month
  expect_error(month_days("1-2-3"))
  expect_equal(month_days(cf), c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  expect_equal(month_days(cf3), rep(30, 12))
  expect_equal(month_days(cf4), c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  expect_equal(month_days(cf5), c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

  expect_error(month_days(cf, 4))
  expect_warning(month_days(cf, c("2001-01-01", "abc")), "^Some dates could not be parsed")
  x <- c("2021-11-27", "2021-12-10", "2022-02-14", "2022-02-18", "2024-02-03")
  expect_equal(month_days(cf, x), c(30, 31, 28, 28, 29))
  expect_equal(month_days(cf3, x), rep(30, 5))
  expect_equal(month_days(cf4, x), c(30, 31, 28, 28, 28))
  expect_equal(month_days(cf5, x), c(30, 31, 29, 29, 29))
})
