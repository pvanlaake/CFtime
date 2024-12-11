test_that("bounds works", {
  t <- CFtime("days since 2024-01-01", "standard")
  off <- seq(from = 0.5, by = 1, length.out = 10)
  bnds <- rbind(0:9, 1:10)

  expect_null(bounds(t))          # no offsets
  expect_error(bounds(t) <- bnds) #

  t <- t + off
  expect_null(bounds(t)) # bounds not set
  bounds(t) <- TRUE
  expect_equal(bounds(t), bnds)
  expect_equal(bounds(t, "%Y-%m-%d")[2,1:3], c("2024-01-02", "2024-01-03", "2024-01-04"))
  bounds(t) <- FALSE
  expect_null(bounds(t))

  expect_error(bounds(t) <- matrix(1:12, nrow = 4))
  expect_error(bounds(t) <- "plain wrong")
  expect_error(bounds(t) <- bnds * 10)

  bounds(t) <- bnds
  expect_match(capture_output(t$print()), "Bounds  : regular and consecutive$")

  hr6 <- rbind(off - 0.25, off + 0.25)
  bounds(t) <- hr6
  expect_match(capture_output(t$print()), "Bounds  : irregular$")
  expect_equal(bounds(t), hr6)
  expect_equal(bounds(t, "%H")[,1], c("06", "18"))
})

test_that("indexOf() works", {
  off <- (23*360):(24*360-1) + 0.5 # year 2024 days
  t <- CFtime("days since 2001-01-01", "360_day", off)
  x <- c("1999-02-12", # pre-origin
         "2023-06-23", # pre-time series
         "2024-01-30",
         "2024-01-31", # non-existent date
         "2024-02-01",
         "2024-02-30",
         "2024-03-01",
         "2025-01-01") # post-time series

  expect_error(indexOf(x, t, method = 4))
  expect_error(indexOf(TRUE, t))
  expect_error(indexOf(x, CFtime("months since 2001-01-01", "standard", 0:23)))
  expect_error(indexOf(x, t, nomatch = "July")) # must be able to coerce to numeric

  expect_equal(indexOf(x, t)[1:8], c(0, 0, 29, NA, 30, 59, 60, .Machine$integer.max))
  expect_equal(indexOf(x, t, method = "linear")[1:8], c(0, 0, 29.5, NA, 30.5, 59.5, 60.5, .Machine$integer.max))

  n <- 1:3
  out <- indexOf(n, t)
  outt <- attr(out, "CFTime")
  expect_equal(as_timestamp(t)[1:3], as_timestamp(outt))
  n <- c(-1, -2, -3)
  out <- indexOf(n, t)
  outt <- attr(out, "CFTime")
  expect_equal(length(outt), 360 - 3)
  expect_equal(as_timestamp(t)[4:6], as_timestamp(outt)[1:3])
  expect_error(indexOf(c(-1, 1), t))

  # Attached CFtime must have valid timestamps in `x`
  out <- indexOf(x, t)
  outt <- attr(out, "CFTime")
  expect_equal(as_timestamp(outt), x[!is.na(out) & out > 0 & out < .Machine$integer.max])

  bounds(t) <- TRUE
  expect_equal(indexOf(x, t)[1:8], c(0, 0, 30, NA, 31, 60, 61, .Machine$integer.max))
  expect_equal(indexOf(x, t, method = "linear")[1:8], c(0, 0, 30, NA, 31, 60, 61, .Machine$integer.max))
})
