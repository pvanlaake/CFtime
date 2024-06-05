test_that("timestamp string parsing to offsets and deparsing of offsets to timestamps", {
  # This test tests: global CFt* constants, CFtime(), CFparse(), as_timestamp():
  # decomposing offsets into timestamp elements, generating timestamp strings,
  # parsing timestamp strings back into timestamp elements.
  for (c in CFt$calendars$name) {
    for (u in 1:4) {
      offsets <- 1:10000
      def <- paste(CFt$units$name[u], "since 1953-08-20")
      cf <- CFtime(def, c, offsets)
      time <- .offsets2time(cf@offsets, cf@datum)
      ts <- as_timestamp(cf, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- CFparse(cf2, ts)
      expect_equal(tp, time)
    }
  }
})

test_that("testing calendars with leap years", {
  # This test tests that for standard and julian calendars datums in leap
  # years before/on/after the leap day function as needed. Also testing year
  # 2000 and 2100 offsets.
  for (c in c("standard", "julian")) {
    for (d in c("1996-01-15", "1996-02-29", "1996-04-01")) {
      def <- paste("days since", d)
      cf <- CFtime(def, c, c(1:2500, 36501:39000))
      time <- .offsets2time(cf@offsets, cf@datum)
      ts <- as_timestamp(cf, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- CFparse(cf2, ts)
      expect_equal(tp, time)
    }
  }
})

test_that("Testing milli-second timestamp string parsing to offsets and deparsing
          of offsets to timestamps", {
  # This test tests: global CFt* constants, CFtime(), CFparse(), as_timestamp():
  # decomposing offsets into milli-second timestamp elements, generating timestamp strings,
  # parsing timestamp strings back into timestamp elements.
  for (c in CFt$calendars$name) {
    for (u in 1:2) {
      offsets <- runif(10000, max = 10000)
      def <- paste(CFt$units$name[u], "since 1953-08-20 07:34:12.2")
      cf <- CFtime(def, c, offsets)
      time <- .offsets2time(cf@offsets, cf@datum)
      ts <- as_timestamp(cf, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- CFparse(cf2, ts)
      expect_equal(tp[1:6], time[1:6])
    }
  }
})

test_that("Disallow parsing of timestamps on month and year datums", {
  for (c in CFt$calendars$name) {
    for (u in 5:6) {
      def <- paste(CFt$units$name[u], "since 2020-01-01")
      cf <- CFtime(def, c, 0:23)
      ts <- as_timestamp(cf, "timestamp")
      expect_error(CFparse(cf, ts))
    }
  }
})

test_that("Things can go wrong too", {
  expect_false(.is_valid_calendar_date(0, 0, 0, 1))   # Bad year
  expect_false(.is_valid_calendar_date(5, 13, 0, 1))  # Bad month
  expect_true(.is_valid_calendar_date(5, 10, NA, 1))  # No day so ok
  expect_false(.is_valid_calendar_date(5, 10, 0, 1))  # Bad day
  expect_false(.is_valid_calendar_date(5, 2, 31, 1))  # No Feb 31
  expect_false(.is_valid_calendar_date(5, 2, 29, 4))  # noleap

  years <- c(1900, 2000, 2001, 2002, 2003, 2004, 2100)
  expect_equal(.is_leap_year(years, 1), c(F, T, F, F, F, T, F))
  expect_equal(.is_leap_year(years, 2), c(T, T, F, F, F, T, T))
  expect_equal(.is_leap_year(years, 3), c(F, F, F, F, F, F, F))
  expect_equal(.is_leap_year(years, 4), c(F, F, F, F, F, F, F))
  expect_equal(.is_leap_year(years, 5), c(T, T, T, T, T, T, T))

  cf <- CFtime("days since 0001-01-01", "proleptic_gregorian")
  suppressWarnings(expect_warning(CFparse(cf, c("12-1-23", "today")))) # Suppress timezone warning
  expect_warning(CFparse(cf, c("2022-08-16T11:07:34.45-10", "2022-08-16 10.5+04")))
})
