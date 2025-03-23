test_that("CFClimatology works", {
  # Must have bounds
  expect_error(CFClimatology$new("days since 2025-01-01", "360_day", 15 + 30 * 0:11))

  # Bad bounds
  expect_error(CFClimatology$new("days since 2025-01-01", "360_day", 15 + 30 * 0:11,
                                 matrix(c(80 * 0:11, 80 * 1:12), nrow = 2, byrow = T)))

  clim <- CFClimatology$new("days since 2025-01-01", "360_day", 15 + 30 * 0:11,
                            matrix(c(30 * 0:11, 30 * 1:12), nrow = 2, byrow = T))
  expect_true(inherits(clim, "CFClimatology"))
  expect_equal(clim$period, "month")
  expect_equal(clim$years, c(2025L, 2025L))
  x <- capture.output(clim$print())
  expect_match(x[6], "average of 30\\.000000 days between 12 elements\\)$")
  expect_equal(x[7], "  Period  : month")
  expect_equal(x[8], "  Year    : 2025")
})
