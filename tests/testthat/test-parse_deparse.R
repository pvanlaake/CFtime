test_that("timestamp string parsing to offsets and deparsing of offsets to timestamps", {
  # This test tests: global CFt* constants, CFtime(), parse_timestamps(), as_timestamp():
  # decomposing offsets into timestamp elements, generating timestamp strings,
  # parsing timestamp strings back into timestamp elements, including negative
  # offsets.
  for (c in c("standard", "proleptic_gregorian", "julian", "tai", "360_day", "365_day", "366_day", "noleap", "all_leap")) {
    for (u in 1:4) {
      offsets <- -1000:1000
      def <- paste(CFt$units$name[u], "since 1992-08-20")
      t <- CFtime(def, c, offsets)
      time <- t$cal$offsets2time(t$offsets)
      ts <- as_timestamp(t, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- parse_timestamps(cf2, ts)
      expect_equal(tp, time)
    }
  }

  # Add no offsets, test return value
  t <- CFTime$new("days since 1992-08-20", "standard")
  res <- t$cal$offsets2time()
  expect_equal(nrow(res), 0L)
  t <- CFTime$new("days since 1992-08-20", "utc")
  res <- t$cal$offsets2time()
  expect_equal(nrow(res), 0L)
})

test_that("testing calendars with leap years", {
  # This test tests that for standard, proleptic_gregorian, julian and tai
  # calendars in leap years before/on/after the leap day function as needed.
  # Also testing year 2000 and 2100 offsets.
  for (c in c("standard", "proleptic_gregorian", "tai", "julian")) {
    for (d in c("1996-01-15", "1996-02-29", "1996-04-01")) {
      def <- paste("days since", d)
      t <- CFtime(def, c, c(1:2500, 36501:39000))
      time <- t$cal$offsets2time(t$offsets)
      ts <- as_timestamp(t, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- parse_timestamps(cf2, ts)
      expect_equal(tp, time)
    }
  }
})

# test_that("Testing milli-second timestamp string parsing to offsets and deparsing
#           of offsets to timestamps", {
#   # This test tests: global CFt* constants, CFtime(), parse_timestamps(), as_timestamp():
#   # decomposing offsets into milli-second timestamp elements, generating
#   # timestamp strings, parsing timestamp strings back into timestamp elements.
#   for (c in c("standard", "proleptic_gregorian", "julian", "tai", "360_day", "365_day", "366_day", "noleap", "all_leap")) {
#     for (u in 1:2) {
#       offsets <- runif(10000, max = 10000)
#       def <- paste(CFt$units$name[u], "since 1978-08-20 07:34:12.2")
#       expect_warning(t <- CFtime(def, c, offsets)) # not ordered
#       time <- t$cal$offsets2time(t$offsets)
#       ts <- as_timestamp(t, "timestamp")
#       cf2 <- CFtime(def, c)
#       tp <- parse_timestamps(cf2, ts)
#       expect_equal(round(tp[c("second", "offset")], 2), round(time[c("second", "offset")], 2))
#     }
#   }
# })

test_that("Disallow parsing of timestamps on month and year calendar units", {
  for (c in c("standard", "proleptic_gregorian", "julian", "tai", "360_day", "365_day", "366_day", "noleap", "all_leap")) {
    for (u in 5:6) {
      def <- paste(CFt$units$name[u], "since 2020-01-01")
      t <- CFtime(def, c, 0:23)
      ts <- as_timestamp(t, "timestamp")
      expect_error(parse_timestamps(t, ts))
    }
  }
})

test_that("Gregorian/Julian calendar gap in the standard calendar", {
  t <- CFtime("days since 1582-10-01", "standard", 0:3)
  ts <- as_timestamp(t)
  expect_equal(ts[4], "1582-10-04")
  expect_false(t$cal$POSIX_compatible(0:3))

  t <- t + 4:10
  ts <- as_timestamp(t)
  expect_equal(ts[5], "1582-10-15")
  expect_equal(ts[11], "1582-10-21")
  expect_equal(parse_timestamps(t, c("1582-09-30", ts))$offset, -1:10)

  t <- CFtime("days since 1582-10-20", "standard", -10:0)
  ts <- as_timestamp(t)
  expect_equal(ts[5], "1582-10-04")
  expect_equal(ts[6], "1582-10-15")

  expect_false(t$POSIX_compatible())            # use t$offsets
  expect_true(t$cal$POSIX_compatible(0:100))    # use supplied offsets
  expect_true(t$cal$POSIX_compatible(-5:100))
  expect_false(t$cal$POSIX_compatible(-6:100))
})

test_that("Leap seconds in the utc calendar work fine", {
  expect_error(CFtime("seconds since 1792-01-01", "utc")) # No UTC in 1792
  t <- CFTime$new("seconds since 1972-06-30 23:59:57", "utc", 1:4)
  expect_equal(as_timestamp(t), c("1972-06-30T23:59:58", "1972-06-30T23:59:59",
                                  "1972-06-30T23:59:60", "1972-07-01T00:00:00"))

  expect_error(CFTime$new("seconds since 1973-06-30 23:59:60", "utc", -1:2)) # Bad leap second
  t <- CFTime$new("seconds since 1972-06-30 23:59:60", "utc", -1:2)
  expect_equal(as_timestamp(t), c("1972-06-30T23:59:59", "1972-06-30T23:59:60",
                                  "1972-07-01T00:00:00", "1972-07-01T00:00:01"))

  t <- CFTime$new("seconds since 2016-12-31 23:59:58", "utc", 1:4)
  expect_equal(as_timestamp(t), c("2016-12-31T23:59:59", "2016-12-31T23:59:60",
                                  "2017-01-01T00:00:00", "2017-01-01T00:00:01"))

  t <- CFTime$new("minutes since 2016-12-31 23:58:00", "utc", 1:4)
  expect_equal(as_timestamp(t), c("2016-12-31T23:59:00", "2016-12-31T23:59:60",
                                  "2017-01-01T00:00:59", "2017-01-01T00:01:59"))

  t <- CFTime$new("hours since 2016-12-31 22:00", "utc", 1:4)
  expect_equal(as_timestamp(t), c("2016-12-31T23:00:00", "2016-12-31T23:59:60",
                                  "2017-01-01T00:59:59", "2017-01-01T01:59:59"))

  t <- CFTime$new("days since 2016-12-30", "utc", 1:4)
  expect_equal(as_timestamp(t), c("2016-12-31T00:00:00", "2016-12-31T23:59:60",
                                  "2017-01-01T23:59:59", "2017-01-02T23:59:59"))
})

test_that("Fractional time parts and replace Z timezone with offset", {
  offsets <- c("1980-05-06Z", "1980-05-06 12.32Z", "1980-05-06 12:54.38Z", "1980-05-06 12:54:12.32Z")
  res <- matrix(c(0.00, 0.00, 0.80, 0.32,
                  0.0000, 0.2000, 0.3800, 0.2053,
                  0.0000, 0.3200, 0.9063, 0.9034,
                  0.0000, 0.5133, 0.5378, 0.5376), ncol = 4)
  for (c in c("standard", "proleptic_gregorian", "julian", "tai", "360_day", "365_day", "366_day", "noleap", "all_leap")) {
    for (u in 1:4) {
      def <- paste(CFt$units$name[u], "since 1978-08-20")
      t <- CFtime(def, c)
      p <- t$cal$parse(offsets)
      expect_equal(round(p$offset %% 1, 4), res[,u])
      expect_equal(p$tz, rep("+0000", 4))
    }
  }

  offsets <- c("1980-05-06", "1980-05-06 12.32", "1980-05-06 12:54.38", "1980-05-06 12:54:12.32")
  res <- matrix(c(0.00, 0.00, 0.80, 0.32,
                  0.0333, 0.2333, 0.4133, 0.2387,
                  0.0006, 0.3206, 0.9069, 0.9040,
                  0.0000, 0.5134, 0.5378, 0.5377), ncol = 4)
  for (u in 1:4) {
    def <- paste(CFt$units$name[u], "since 1978-08-20")
    t <- CFtime(def, "utc")
    p <- t$cal$parse(offsets)
    expect_equal(round(p$offset %% 1, 4), res[,u])
  }

})
