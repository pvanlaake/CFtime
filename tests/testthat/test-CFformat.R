test_that("CFfactor testing", {
  # No offsets
  cf <- CFtime("days since 2001-01-01", "365_day")
  expect_error(CFfactor(cf))

  # Few offsets
  cf <- cf + 0:4
  expect_error(CFfactor(cf))

  cf <- cf + 5:7299 # 20 years of offsets

  # Regular factors for all available periods
  period <- c("year", "season", "month", "dekad", "day")
  first <- c("2001", "2001-DJF", "2001-01", "2001D01", "2001-01-01")
  last <-  c("2020", "2021-DJF", "2020-12", "2020D36", "2020-12-31")
  for (p in 1:5) {
    expect_equal(as.character(CFfactor(cf, period[p]))[1], first[p])
    expect_equal(as.character(CFfactor(cf, period[p]))[7300], last[p])
  }

  # Epoch factors for all available periods
  period <- c("year", "season", "month", "dekad", "day")
  lvls <- c(1, 4, 12, 36, 365)
  for (p in 1:5) {
    f <- CFfactor(cf, period[p], list(first = 2001, double = 2002:2003, final3 = 2018:2020, outside = 2022))
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
})
