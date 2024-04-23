test_that("bounds works", {
  cf <- CFtime("days since 2024-01-01", "standard")
  off <- seq(from = 0.5, by = 1, length.out = 10)
  bnds <- rbind(0:9, 1:10)
  expect_null(bounds(cf)) # no offsets
  cf <- cf + off
  expect_null(bounds(cf)) # bounds not set
  bounds(cf) <- TRUE
  expect_equal(bounds(cf), bnds)
  expect_equal(bounds(cf, "%Y-%m-%d")[2,1:3], c("2024-01-02", "2024-01-03", "2024-01-04"))
  bounds(cf) <- FALSE
  expect_null(bounds(cf))

  expect_error(bounds(cf) <- matrix(1:12, nrow = 4))
  expect_error(bounds(cf) <- "plain wrong")
  expect_error(bounds(cf) <- bnds * 10)

  bounds(cf) <- bnds
  expect_match(capture_output(methods::show(cf)), "Bounds  : regular and consecutive$")

  hr6 <- rbind(off - 0.25, off + 0.25)
  bounds(cf) <- hr6
  expect_match(capture_output(methods::show(cf)), "Bounds  : irregular$")
  expect_equal(bounds(cf), hr6)
  expect_equal(bounds(cf, "%H")[,1], c("06", "18"))
})
