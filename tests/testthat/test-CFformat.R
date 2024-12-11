test_that("Creating timestamps", {
  expect_error(as_timestamp("1-2-3"))          # No CFtime as first argument

  cf <- CFtime("hours since 2001-01-01", "365_day")
  expect_null(as_timestamp(cf)) # No offsets
  cf <- cf + 0L:2399L
  expect_error(as_timestamp(cf, "d"))          # Wrong format specifier
  expect_error(as_timestamp(cf, asPOSIX = T))  # No POSIXt on a non-standard calendar
  expect_equal(length(as_timestamp(cf)), 2400L)

  cf <- CFtime("days since 2001-01-01", "standard", 0L:364L)
  expect_equal(length(as_timestamp(cf)), 365L)
  expect_equal(nchar(as_timestamp(cf)[1]), 10L) # date string
  expect_equal(length(as_timestamp(cf, "date", TRUE)), 365L)
  expect_equal(length(as_timestamp(cf, "timestamp", TRUE)), 365L)
})

test_that("Using format()", {
  cf <- CFtime("days since 2001-01-01 18:10:30-04", "365_day", 0:364)

  expect_equal(format(cf)[1], "2001-01-01 18:10:30")      # format parameter missing
  expect_error(format(cf, 123)) # format parameter must be character
  expect_error(format(cf, c("doesn't", "work", "either")))

  expect_equal(format(cf, "Timestamp is: %%%F%%")[1], "Timestamp is: %2001-01-01%")
  expect_equal(format(cf, "Timestamp is: %R")[1], "Timestamp is: 18:10")
  expect_equal(format(cf, "%T%z")[1], "18:10:30-0400")
  #expect_equal(format(cf, "%b")[c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)], month.abb)  # en_EN only
  #expect_equal(format(cf, "%B")[c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)], month.name) #
  expect_equal(format(cf, "%Od-%e-%I%p")[5], "05- 5-06PM")

})

test_that("CFfactor testing", {
  # No offsets
  cf <- CFtime("days since 2000-01-01", "365_day")
  expect_error(CFfactor())
  expect_error(CFfactor(cf))
  expect_error(CFfactor(cf, "zxcv"))
  expect_error(CFfactor(cf, c("day", "month")))
  expect_error(CFfactor(cf, "hour"))
  expect_error(CFfactor(cf, "month", "bad"))
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  leap_month_days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  dekad_days <- c(10, 10, 11, 10, 10, 8, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 11)
  leap_dekad_days <- c(10, 10, 11, 10, 10, 9, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 11)

  # Few offsets
  cf <- cf + 365L:370L
  expect_error(CFfactor(cf))

  cf <- cf + 371L:7664L # 20 years of offsets

  # Regular factors for all available periods
  np <- c(20, 81, 80, 240, 720, 7300)
  first <- c("2001", "2001S1", "2001Q1", "2001-01", "2001D01", "2001-01-01")
  last <-  c("2020", "2021S1", "2020Q4", "2020-12", "2020D36", "2020-12-31")
  for (p in 1:6) {
    f <- CFfactor(cf, CFt$factor_periods[p])
    expect_equal(as.character(f)[1L], first[p])
    expect_equal(as.character(f)[7300L], last[p])
    newcf <- attr(f, "CFTime")
    bnds <- bounds(newcf)
    expect_equal(definition(cf), definition(newcf))
    expect_true(is.matrix(bnds))
    expect_type(bnds, "double")
    expect_equal(dim(bnds), c(2, np[p]))
  }

  # Era factors for all available periods
  eras <- list(first = 2001L, double = 2002L:2003L, final3 = 2018L:2020L, outside = 2022L)
  lvls <- c(1L, 4L, 4L, 12L, 36L, 365L)
  for (p in 1:6) { # year, season, quarter, month, dekad, day
    f <- CFfactor(cf, CFt$factor_periods[p], eras)
    expect_type(f, "list")
    expect_equal(length(f), 4L)
    expect_equal(length(f$first), 7300L)
    expect_equal(attr(f$first, "period"), CFt$factor_periods[p])
    expect_equal(attr(f$first, "era"), 1L)
    expect_null(attr(f$first, "zxcv"))
    if (p == 1L) {
      expect_equal(length(levels(f$first)), 1L)
      expect_equal(length(levels(f$double)), 2L)
      expect_equal(length(levels(f$final3)), 3L)
      expect_equal(length(levels(f$outside)), 0L)
    } else if (p %in% c(2L, 3L)) {
      expect_equal(length(levels(f$first)), 4L)
      expect_equal(length(levels(f$double)), 4L)
      expect_equal(length(levels(f$final3)), 4L)
      expect_equal(length(levels(f$outside)), 0L)
    } else {
      expect_equal(length(levels(f$first)), lvls[p])
      expect_equal(length(levels(f$double)), lvls[p])
      expect_equal(length(levels(f$final3)), lvls[p])
      expect_equal(length(levels(f$outside)), 0L)
    }
  }

  # Single era value for all available periods
  for (p in 1:6) { # year, season, quarter, month, dekad, day
    f <- CFfactor(cf, CFt$factor_periods[p], 2002L)
    expect_s3_class(f, "factor")
    expect_equal(length(f), 7300L)
    expect_equal(length(levels(f)), lvls[p])
    expect_equal(attr(f, "period"), CFt$factor_periods[p])
    expect_equal(attr(f, "era"), 1L)
    expect_null(attr(f, "zxcv"))
  }

  # Units and coverage in factor levels
  expect_error(CFfactor_units("zxcv"))
  expect_error(CFfactor_units(cf, "zxcv"))
  expect_error((CFfactor_units(cf, list(12))))
  expect_error((CFfactor_units(cf, factor(letters))))
  expect_error((CFfactor_units(cf, list(factor(letters)))))

  f <- CFfactor(cf, "year")
  expect_true(all(CFfactor_units(cf, f) == 365L))
  expect_true(all(CFfactor_coverage(cf, f, "absolute") == 365L))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1L))

  f <- CFfactor(cf, "season")
  expect_equal(sum(CFfactor_units(cf, f)), 7390L) # 20 yrs * 365 plus 90 bc DJF season present on both ends
  expect_true(all(CFfactor_units(cf, f) %in% 90L:92L))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_equal(x[1L], 59L)  # Jan + Feb of first year
  expect_equal(x[81L], 31L) # Dec of last year
  expect_true(all(x[2L:80L] %in% 90L:92L))
  x <- CFfactor_coverage(cf, f, "relative")
  expect_equal(x[1L] + x[81L], 1L)
  expect_true(all(x[2L:80L] == 1L))

  f <- CFfactor(cf, "quarter")
  expect_equal(sum(CFfactor_units(cf, f)), 7300L)
  expect_true(all(CFfactor_units(cf, f) %in% 90L:92L))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_equal(x[1L], 90L)
  expect_equal(x[80L], 92L)
  expect_true(all(x %in% 90L:92L))
  x <- CFfactor_coverage(cf, f, "relative")
  expect_true(all(x == 1L))

  f <- CFfactor(cf, "month")
  expect_equal(sum(CFfactor_units(cf, f)), 7300L)
  expect_true(all(CFfactor_coverage(cf, f, "absolute") == month_days))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1L))

  f <- CFfactor(cf, "dekad")
  expect_equal(sum(CFfactor_units(cf, f)), 7300L)
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x == dekad_days))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1L))

  f <- CFfactor(cf, "day")
  expect_equal(sum(CFfactor_units(cf, f)), 7300L)
  expect_true(all(CFfactor_coverage(cf, f, "absolute") == 1L))
  expect_true(all(CFfactor_coverage(cf, f, "relative") == 1L))

  # 360_day calendar
  cf360 <- CFtime("days since 2001-01-01", "360_day", 0:7199)
  f <- CFfactor(cf360, "month")
  expect_equal(sum(CFfactor_units(cf360, f)), 7200L)
  expect_true(all(CFfactor_coverage(cf360, f, "absolute") == 30L))
  expect_true(all(CFfactor_coverage(cf360, f, "relative") == 1L))

  # Units and coverage in factor levels with eras
  f <- CFfactor(cf, "year", eras)
  expect_true(all(unlist(CFfactor_units(cf, f)) == rep(365L, 6L)))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "absolute")) == c(rep(365L, 6L), 0L)))
  expect_equal(sum(sapply(CFfactor_coverage(cf, f, "relative"), sum)), 3L)

  f <- CFfactor(cf, "season", eras)
  expect_true(all(sapply(CFfactor_units(cf, f), function(x) {all(x == c(90L, 92L, 92L, 91L))})))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_equal(x$first[1L], 59L)  # Jan + Feb of first year at beginning of time series
  expect_equal(x$double, c(180L, 184L, 184L, 182L)) # two full years
  expect_equal(x$final3, c(270L, 276L, 276L, 273L)) # three full years
  x <- unlist(CFfactor_coverage(cf, f, "relative"))
  #expect_equal(x[1], 59 / 90). # works in the console but not here
  expect_true(all(x[2L:12L] == 1L))

  f <- CFfactor(cf, "month", eras)
  expect_true(all(sapply(CFfactor_units(cf, f), function(x) {all(x == month_days)})))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x$first == month_days))
  expect_true(all(x$double == month_days * 2L))
  expect_true(all(x$final3 == month_days * 3L))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "relative")) == 1L))

  f <- CFfactor(cf, "dekad", eras)
  expect_true(all(sapply(CFfactor_units(cf, f), function(x) {all(x == dekad_days)})))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x$first == dekad_days))
  expect_true(all(x$double == dekad_days * 2L))
  expect_true(all(x$final3 == dekad_days * 3L))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "relative")) == 1L))

  f <- CFfactor(cf, "day", eras)
  expect_true(all(unlist(CFfactor_units(cf, f)) == 1L))
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_true(all(x$first == 1L))
  expect_true(all(x$double == 2L))
  expect_true(all(x$final3 == 3L))
  expect_true(all(unlist(CFfactor_coverage(cf, f, "relative")) == 1L))

  # all_leap calendar
  cf366 <- CFtime("days since 2001-01-01", "all_leap", 0:7319)
  f <- CFfactor(cf366, "year", eras)
  expect_true(all(unlist(CFfactor_units(cf366, f)) == rep(366L, 6L)))
  expect_true(all(unlist(CFfactor_coverage(cf366, f, "absolute")) == c(rep(366L, 6L), 0L)))
  expect_equal(sum(sapply(CFfactor_coverage(cf366, f, "relative"), sum)), 3L)

  f <- CFfactor(cf366, "season", eras)
  expect_true(all(sapply(CFfactor_units(cf366, f), function(x) {all(x == c(91L, 92L, 92L, 91L))})))
  x <- CFfactor_coverage(cf366, f, "absolute")
  expect_equal(x$first[1L], 60L)  # Jan + Feb of first year at beginning of time series
  expect_equal(x$double, c(182L, 184L, 184L, 182L)) # two full years
  expect_equal(x$final3, c(273L, 276L, 276L, 273L)) # three full years
  x <- unlist(CFfactor_coverage(cf366, f, "relative"))
  #expect_equal(x[1], 60 / 90). # works in the console but not here
  expect_true(all(x[2L:12L] == 1L))

  f <- CFfactor(cf366, "month", eras)
  expect_true(all(sapply(CFfactor_units(cf366, f), function(x) {all(x == leap_month_days)})))
  x <- CFfactor_coverage(cf366, f, "absolute")
  expect_true(all(x$first == leap_month_days))
  expect_true(all(x$double == leap_month_days * 2L))
  expect_true(all(x$final3 == leap_month_days * 3L))
  expect_true(all(unlist(CFfactor_coverage(cf366, f, "relative")) == 1L))

  f <- CFfactor(cf366, "dekad", eras)
  expect_true(all(sapply(CFfactor_units(cf366, f), function(x) {all(x == leap_dekad_days)})))
  x <- CFfactor_coverage(cf366, f, "absolute")
  expect_true(all(x$first == leap_dekad_days))
  expect_true(all(x$double == leap_dekad_days * 2L))
  expect_true(all(x$final3 == leap_dekad_days * 3L))
  expect_true(all(unlist(CFfactor_coverage(cf366, f, "relative")) == 1L))

  f <- CFfactor(cf366, "day", eras)
  expect_true(all(unlist(CFfactor_units(cf366, f)) == 1L))
  x <- CFfactor_coverage(cf366, f, "absolute")
  expect_true(all(x$first == 1L))
  expect_true(all(x$double == 2L))
  expect_true(all(x$final3 == 3L))
  expect_true(all(unlist(CFfactor_coverage(cf366, f, "relative")) == 1L))

  # Incomplete coverage
  n <- 365L * 20L
  cov <- 0.8
  offsets <- sample(0L:(n-1L), n * cov)
  expect_warning(cf <- CFtime("days since 2020-01-01", "365_day", offsets))
  f <- CFfactor(cf, "month")
  x <- CFfactor_coverage(cf, f, "absolute")
  expect_equal(sum(x), n * cov)
  x <- CFfactor_coverage(cf, f, "relative")
  expect_true((cov - 0.01) < mean(x) && mean(x) < (cov + 0.01))
})

test_that("cut() works", {
  cf <- CFtime("days since 2020-01-01", "360_day", 0:719)
  expect_error(cut("sfg"))
  expect_error(cut(cf))
  expect_error(cut(cf, breaks = 5))
  expect_error(cut(cf, ""))
  expect_error(cut(cf, "blah"))

  f <- cut(cf, "quarter")
  expect_equal(nlevels(f), 8)
  expect_equal(levels(f), c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2", "2021Q3", "2021Q4"))

  f <- cut(cf, c("2021-01-01", "2020-04-03")) # out of order
  expect_s3_class(f, "factor")
  expect_equal(levels(f), "2020-04-03")

  f <- cut(cf, c("2020-01-01", "2020-06-17", "2021-01-01", "2021-04-12", "2401-01-01"))
  expect_equal(levels(f), c("2020-01-01", "2020-06-17", "2021-01-01", "2021-04-12"))
})
