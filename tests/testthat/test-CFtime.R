test_that("test all variants of creating a CFtime object", {
  # Call CFtime() without arguments
  expect_error(CFtime())

  # Call CFtime() with an invalid definition
  expect_error(CFtime("foo"))

  # Call CFtime() with a valid definition and an invalid calendar
  expect_error(CFtime("days since 1991-01-01", "foo"))

  # Call CFtime() with a valid definition and calendar but faulty offsets
  expect_error(CFtime("days since 1991-01-01", "standard", -9:10))
  expect_error(CFtime("days since 1991-01-01", "standard", c(0:10, NA)))
  expect_error(CFtime("days since 1991-01-01", offsets = -9:10))

  # CFtime() with only a definition
  cf <- CFtime("d per 1991-01-01")

  expect_equal(CForigin(cf)[1:3], data.frame(year = 1991, month = 1, day = 1))
  expect_equal(CFunit(cf), "days")
  expect_equal(CFcalendar(cf), "standard")
  expect_null(CFoffsets(cf))

  # CFtime() with only a definition and a calendar
  cf <- CFtime("d per 1991-01-01", "julian")

  expect_equal(CForigin(cf)[1:3], data.frame(year = 1991, month = 1, day = 1))
  expect_equal(CFunit(cf), "days")
  expect_equal(CFcalendar(cf), "julian")
  expect_null(CFoffsets(cf))

  # Merge two CFtime instances / extend offsets
  cf1 <- CFtime("hours since 2001-01-01", "360_day", 0:99)
  cf2 <- CFtime("hours since 2001-01-01", "julian", 100:199)
  expect_error(cf1 + cf2)

  cf2 <- CFtime("hours since 2001-01-01", "360_day", 100:199)
  expect_equal(length(CFoffsets(cf1 + cf2)), 200)

  expect_equal(length(CFoffsets(cf1 + 100:199)), 200)

  # Time series completeness
  expect_true(is.na(CFcomplete(cf)))
  expect_true(CFcomplete(cf1))
  mid_months <- c("1950-01-16T12:00:00", "1950-02-15T00:00:00", "1950-03-16T12:00:00", "1950-04-16T00:00:00", "1950-05-16T12:00:00", "1950-06-16T00:00:00",
                  "1950-07-16T12:00:00", "1950-08-16T12:00:00", "1950-09-16T00:00:00", "1950-10-16T12:00:00", "1950-11-16T00:00:00", "1950-12-16T12:00:00")
  cf <- CFtime("days since 1950-01-01", "standard", mid_months)
  expect_true(CFcomplete(cf))
  cfy <- CFtime("years since 2020-01-01", "standard", 0:19)
  expect_true(CFcomplete(cfy))
  cfy <- cfy + 30:39
  expect_false(CFcomplete(cfy))

  # Range on unsorted offsets
  random <- runif(100, min = 1, max = 99)
  cf <- CFtime("days since 2001-01-01", offsets = c(0, random[1:50], 100, random[51:100]))
  expect_equal(CFrange(cf), c("2001-01-01T00:00:00", paste0(as.Date("2001-01-01") + 100, "T00:00:00")))

  # Character offsets
  cf <- CFtime("hours since 2023-01-01", "360_day", "2023-04-30T23:00")
  expect_equal(CFrange(cf), c("2023-01-01T00:00:00", "2023-04-30T23:00:00"))
  expect_equal(length(CFtimestamp(cf, "timestamp")), 4 * 30 * 24)

  expect_error(CFtime("days since 2023-01-01", "366_day", c("2021-01-01", "2021-04-13")))
  cf <- CFtime("days since 2023-01-01", "366_day", c("2023-01-01", "2023-04-13", "2023-10-30", "2023-05-12"))
  expect_equal(length(cf@time$year), 4)
  expect_equal(CFrange(cf), c("2023-01-01", "2023-10-30"))
})
