test_that("CFfactor testing", {
  # No offsets
  cf <- CFtime("days since 2001-01-01", "365_day")
  expect_error(CFfactor(cf))
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  dekad_days <- c(10, 10, 11, 10, 10, 8, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 11)

  # Few offsets
  cf <- cf + 0:4
  expect_error(CFfactor(cf))

  cf <- cf + 5:7299 # 20 years of offsets

  # Regular factors for all available periods
  first <- c("2001", "2001-DJF", "2001-01", "2001D01", "2001-01-01")
  last <-  c("2020", "2021-DJF", "2020-12", "2020D36", "2020-12-31")
  for (p in 1:5) {
    f <- CFfactor(cf, CFt$factor_periods[p])
    expect_equal(as.character(f)[1], first[p])
    expect_equal(as.character(f)[7300], last[p])
  }

  # Epoch factors for all available periods
  epochs <- list(first = 2001, double = 2002:2003, final3 = 2018:2020, outside = 2022)
  lvls <- c(1, 4, 12, 36, 365)
  for (p in 1:5) {
    f <- CFfactor(cf, CFt$factor_periods[p], epochs)
    expect_equal(length(f), 4)
    expect_equal(length(f$first), 7300)
    if (p == 1) {
      expect_equal(length(levels(f$first)), 1)
      expect_equal(length(levels(f$double)), 2)
      expect_equal(length(levels(f$final3)), 3)
      expect_equal(length(levels(f$outside)), 0)
    } else if (p == 2) {
      expect_equal(length(levels(f$first)), 4)
      expect_equal(length(levels(f$double)), 4)
      expect_equal(length(levels(f$final3)), 4)
      expect_equal(length(levels(f$outside)), 0)
    } else {
      expect_equal(length(levels(f$first)), lvls[p])
      expect_equal(length(levels(f$double)), lvls[p])
      expect_equal(length(levels(f$final3)), lvls[p])
      expect_equal(length(levels(f$outside)), 0)
    }
  }

  # Units and coverage in factor levels
  f <- CFfactor(cf, "year")
  expect_true(all(CFfactor_units(cf, f) == 365))
  expect_true(all(CFfactor_coverage(cf, f, "absolute") == 365))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1))

  f <- CFfactor(cf, "season")
  expect_equal(sum(CFfactor_units(cf, f)), 7390) # 20 yrs * 365 plus 90 bc DJF season present on both ends
  expect_true(all(CFfactor_units(cf, f) %in% 90:92))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_equal(x[1], 59)  # Jan + Feb of first year
  expect_equal(x[81], 31) # Dec of last year
  expect_true(all(x[2:80] %in% 90:92))
  x <- CFfactor_coverage(cf, f, "relative")
  expect_equal(x[1] + x[81], 1)
  expect_true(all(x[2:80] == 1))

  f <- CFfactor(cf, "month")
  expect_equal(sum(CFfactor_units(cf, f)), 7300)
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x == month_days))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1))

  f <- CFfactor(cf, "dekad")
  expect_equal(sum(CFfactor_units(cf, f)), 7300)
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x == dekad_days))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1))

  f <- CFfactor(cf, "day")
  expect_equal(sum(CFfactor_units(cf, f)), 7300)
  expect_true(all(CFfactor_coverage(cf, f, "absolute") == 1))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1))

  # Units and coverage in factor levels with epochs
  f <- CFfactor(cf, "year", epochs)
  expect_true(all(unlist(CFfactor_units(cf, f)) == rep(365, 6)))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "absolute")) == c(rep(365, 6), 0)))
  expect_equal(sum(sapply(CFfactor_coverage(cf, f, "relative"), sum)), 3)

  f <- CFfactor(cf, "season", epochs)
  expect_true(all(sapply(CFfactor_units(cf, f), function(x) {all(x == c(90, 92, 92, 91))})))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_equal(x$first[1], 59)  # Jan + Feb of first year at beginning of time series
  expect_equal(x$double, c(180, 184, 184, 182)) # two full years
  expect_equal(x$final3, c(270, 276, 276, 273)) # three full years
  x <- unlist(CFfactor_coverage(cf, f, "relative"))
  #expect_equal(x[1], 59 / 90). # works in the console but not here
  expect_true(all(x[2:12] == 1))

  f <- CFfactor(cf, "month", epochs)
  expect_true(all(sapply(CFfactor_units(cf, f), function(x) {all(x == month_days)})))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x$first == month_days))
  expect_true(all(x$double == month_days * 2))
  expect_true(all(x$final3 == month_days * 3))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "relative")) == 1))

  f <- CFfactor(cf, "dekad", epochs)
  expect_true(all(sapply(CFfactor_units(cf, f), function(x) {all(x == dekad_days)})))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x$first == dekad_days))
  expect_true(all(x$double == dekad_days * 2))
  expect_true(all(x$final3 == dekad_days * 3))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "relative")) == 1))

  f <- CFfactor(cf, "day", epochs)
  expect_true(all(unlist(CFfactor_units(cf, f)) == 1))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x$first == 1))
  expect_true(all(x$double == 2))
  expect_true(all(x$final3 == 3))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "relative")) == 1))

  # Incomplete coverage
  n <- 365 * 20
  cov <- 0.8
  offsets <- sample(0:(n-1), n * cov)
  cf <- CFtime("days since 2020-01-01", "365_day", offsets)
  f <- CFfactor(cf, "month")
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_equal(sum(x), n * cov)
  x <- CFfactor_coverage(cf, f, "relative")
  expect_true((cov - 0.01) < mean(x) && mean(x) < (cov + 0.01))
})
