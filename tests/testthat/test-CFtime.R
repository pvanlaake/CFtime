test_that("test all variants of creating a CFtime object and useful functions", {
  # Call CFtime() without arguments
  expect_error(CFtime())

  # Call CFtime() with an invalid definition
  expect_error(CFtime("foo"))
  expect_error(CFtime("zxcv since 1991-01-01"))
  expect_error(CFtime("days since -991-01-01"))

  # Call CFtime() with a valid definition and an invalid calendar
  expect_error(CFtime("days since 1991-01-01", "foo"))
  expect_equal(calendar(CFtime("days since 1991-01-01", NULL, NULL)), "standard")
  expect_equal(definition(CFtime("days since 1991-01-01", NULL, NULL)), "days since 1991-01-01")
  expect_equal(resolution(CFtime("days since 1991-01-01", NULL, NULL)), NA_real_)

  # Call CFtime() with a valid definition and calendar but faulty offsets
  expect_error(CFtime("days since 1991-01-01", "standard", c(0:10, NA)))
  expect_error(CFtime("days since 1991-01-01", "utc", c(0:10, NA)))

  # CFtime() with only a definition
  t <- CFtime("d per 1991-01-01")

  expect_equal(origin(t)[1:3], data.frame(year = 1991, month = 1, day = 1))
  expect_equal(unit(t), "days")
  expect_equal(calendar(t), "standard")
  expect_equal(length(offsets(t)), 0L)

  # CFtime with only a definition and a calendar
  t <- CFtime("d per 1991-01-01", "julian")
  expect_match(capture_output(t$print()), "^CF calendar:")
  expect_match(capture_output(t$print()), "Elements: \\(no elements\\)\\n  Bounds  : \\(not set\\)$")

  expect_equal(origin(t)[1:3], data.frame(year = 1991, month = 1, day = 1))
  expect_equal(unit(t), "days")
  expect_equal(calendar(t), "julian")
  expect_equal(length(offsets(t)), 0L)

  # CFtime with a single offset
  t <- t + 15
  expect_equal(as_timestamp(t, "date"), "1991-01-16")
  expect_match(capture_output(t$print()), "Element : 1991-01-16 \\n  Bounds  : not set$")

  # Invalid offsets
  expect_error(CFtime("d per 1991-01-01", "julian", c(TRUE, FALSE, FALSE)))

  # Character offsets
  t <- CFtime("hours since 2023-01-01", "360_day", c("2023-01-01T00:00:00", "2023-04-30T23:00"))
  expect_equal(range(t, bounds = TRUE), c("2023-01-01T00:00:00", "2023-04-30T23:00:00")) # bounds have not been set
  expect_equal(length(as_timestamp(t, "timestamp")), 2)

  expect_warning(t <- CFtime("days since 2023-01-01", "366_day", c("2023-01-01", "2023-04-13", "2023-10-30", "2023-05-12")))
  expect_equal(range(t), c("2023-01-01", "2023-10-30"))

  # Merge two CFtime instances / extend offsets
  t1 <- CFtime("hours since 2001-01-01", "360_day", 0:99)
  t2 <- CFtime("hours since 2001-01-01", "julian", 100:199)
  expect_false(t1 == t2)
  expect_error(t1 + t2)

  t2 <- CFtime("hours since 2001-01-01", "360_day", 100:199)
  expect_false(t1 == t2)
  expect_equal(length(offsets(t1 + t2)), 200)

  expect_equal(length(offsets(t1 + 100:199)), 200)

  t1 <- CFtime("days since 2022-01-01", "365_day", 0:364)
  t2 <- CFtime("days since 2023-01-01", "365_day", 0:364)
  expect_match(capture_output(t1$print()), "between 365 elements\\)\\n  Bounds  : not set$")
  expect_true(length(offsets(t1 + t2)) == 730)
  expect_true(all(range(diff(offsets(t1 + t2))) == c(1, 1)))
  expect_warning(t3 <- t2 + t1)
  expect_true(length(offsets(t3)) == 730)
  expect_false((range(diff(offsets(t3))) == c(1, 1))[1])

  t2 <- t1 + c("2023-01-01", "2023-01-02")
  expect_match(capture_output(t1$print()), "between 365 elements\\)\\n  Bounds  : not set$")
  expect_equal(t2$resolution, 1)

  # Timezones
  expect_equal(timezone(t1), "+0000")
  expect_false(grepl("+0000", capture_output(t1$print()), fixed = TRUE))
  t1 <- CFtime("days since 2022-01-01 00:00:00+04", "365_day", 0:364)
  expect_true(grepl("+0400", capture_output(t1$print()), fixed = TRUE))

  # Time series completeness
  t <- CFtime("d per 1991-01-01", "julian")
  expect_error(is_complete("zxcv"))
  expect_true(is.na(is_complete(t)))
  expect_true(is_complete(t1))
  mid_months <- c("1950-01-16T12:00:00", "1950-02-15T00:00:00", "1950-03-16T12:00:00", "1950-04-16T00:00:00", "1950-05-16T12:00:00", "1950-06-16T00:00:00",
                  "1950-07-16T12:00:00", "1950-08-16T12:00:00", "1950-09-16T00:00:00", "1950-10-16T12:00:00", "1950-11-16T00:00:00", "1950-12-16T12:00:00")
  t <- CFtime("days since 1950-01-01", "standard", mid_months)
  expect_true(is_complete(t))
  ty <- CFtime("years since 2020-01-01", "standard", 0:19)
  expect_true(is_complete(ty))
  ty <- ty + 30:39
  expect_false(is_complete(ty))

  # Range
  t <- CFtime("days since 2001-01-01")
  expect_equal(range(t), c(NA_character_, NA_character_))
  t <- t + 0:1
  expect_error(range(t, 123))
  expect_error(range(t, c("asd %d", "%F")))
  expect_equal(range(t, "%Y-%B-%Od"), c("2001-January-01", "2001-January-02"))

  # Range on unsorted offsets
  random <- runif(100, min = 1, max = 99)
  expect_warning(t <- CFtime("days since 2001-01-01", offsets = c(0, random[1:50], 100, random[51:100])))
  expect_equal(range(t), c("2001-01-01", paste0(as.Date("2001-01-01") + 100)))

  # Subsetting
  t <- CFtime("hours since 2023-01-01 00:00:00", "standard", 0:239)
  expect_error(slice("zxcv"))
  expect_true(all(slice(t, c("2023-01-01", "2023-02-01"))))
  expect_true(length(which(slice(t, c("2023-01-01", "2023-01-01")))) == 1) # Identical extremes
  expect_true(length(which(slice(t, c("2023-01-01", "2023-05-01")))) == 240)
  expect_true(length(which(slice(t, c("2023-01-01 00:00", "2023-01-01 04:00")))) == 4)
  expect_true(length(which(slice(t, c("2023-01-01 04:00", "2023-01-01 00:00")))) == 4) # extremes in reverse order
  expect_true(slice(t, c("2022-01-01", "2023-01-02"))[1]) # early extreme before timeseries
  expect_true(all(!slice(t, c("2023-02-01", "2023-03-01")))) # both extremes outside time series
  expect_equal(sum(slice(t, c("2023-01-01 00:00", "2023-01-01 04:00", "2023-01-02 00:00"))), 24)
  expect_equal(sum(slice(t, c("2023-01-01 00:00", "2023-01-01 04:00", "2023-01-02 00:00"), TRUE)), 25)

  t$bounds <- TRUE
  s <- t$slice(c("2023-01-01", "2023-05-01"))
  st <- attr(s, "CFTime")
  expect_equal(st$bounds, t$bounds)
})

test_that("Leap years on some calendars", {
  t <- CFTime$new("days since 2025-01-01", "360_day")
  expect_true(all(!t$cal$leap_year(c(2000:2025))))
  t <- CFTime$new("days since 2025-01-01", "366_day")
  expect_true(all(t$cal$leap_year(c(2000:2025))))
})

test_that("Calendar 'none'", {
  t <- CFTime$new("days since 2025-01-01", "none")
  expect_true(inherits(t$cal, "CFCalendarNone"))
  t <- t + 0:4
  ymd <- data.frame(year = c(rep(2025, 5), NA), month = c(rep(1, 5), NA), day = c(1, 1, 1, 2, 3, NA))
  expect_equal(t$cal$valid_days(ymd), c(T, T, T, F, F, NA))
  expect_equal(t$cal$month_days(), rep(NA, 12))
  expect_equal(t$cal$month_days(ymd), rep(NA, 6))
  expect_equal(t$cal$leap_year(ymd$year), rep(NA, 6))
  expect_equal(t$cal$date2offset(ymd), c(0, 0, 0, 0, 0, NA))
  expect_equal(t$cal$offset2date(1:5), data.frame(year = rep(2025, 5), month = rep(1, 5), day = rep(1, 5)))
})
