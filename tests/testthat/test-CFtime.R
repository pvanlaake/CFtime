test_that("test all variants of creating a CFtime object and useful functions", {
  # Call CFtime() without arguments
  expect_error(CFtime())

  # Call CFtime() with an invalid definition
  expect_error(CFtime("foo"))
  expect_error(CFtime("zxcv since 1991-01-01"))
  expect_error(CFtime("days since -991-01-01"))

  # Call CFtime() with a valid definition and an invalid calendar
  expect_error(CFtime("days since 1991-01-01", "foo"))
  expect_equal(CFcalendar(CFtime("days since 1991-01-01", NULL, NULL)), "standard")
  expect_equal(CFdefinition(CFtime("days since 1991-01-01", NULL, NULL)), "days since 1991-01-01")
  expect_equal(CFresolution(CFtime("days since 1991-01-01", NULL, NULL)), NA_real_)

  # Call CFtime() with a valid definition and calendar but faulty offsets
  expect_error(CFtime("days since 1991-01-01", "standard", -9:10))
  expect_error(CFtime("days since 1991-01-01", "standard", c(0:10, NA)))
  expect_error(CFtime("days since 1991-01-01", offsets = -9:10))
  expect_error(CFtime("days since 1991-01-01", offsets = 3.4e24))

  # CFtime() with only a definition
  cf <- CFtime("d per 1991-01-01")

  expect_equal(CForigin(cf)[1:3], data.frame(year = 1991, month = 1, day = 1))
  expect_equal(CFunit(cf), "days")
  expect_equal(CFcalendar(cf), "standard")
  expect_equal(length(CFoffsets(cf)), 0L)

  # CFtime with only a definition and a calendar
  cf <- CFtime("d per 1991-01-01", "julian")
  expect_match(capture_output(methods::show(cf)), "^CF datum of origin:")
  expect_match(capture_output(methods::show(cf)), "Elements: \\(no elements\\)$")

  expect_equal(CForigin(cf)[1:3], data.frame(year = 1991, month = 1, day = 1))
  expect_equal(CFunit(cf), "days")
  expect_equal(CFcalendar(cf), "julian")
  expect_equal(length(CFoffsets(cf)), 0L)

  # CFtime with a single offset
  cf <- cf + 15
  expect_equal(CFtimestamp(cf, "date"), "1991-01-16")
  expect_match(capture_output(methods::show(cf)), "Elements: 1991-01-16 $")

  # Invalid offsets
  expect_error(CFtime("d per 1991-01-01", "julian", c(TRUE, FALSE, FALSE)))

  # Character offsets
  cf <- CFtime("hours since 2023-01-01", "360_day", "2023-04-30T23:00")
  expect_equal(CFrange(cf), c("2023-01-01T00:00:00", "2023-04-30T23:00:00"))
  expect_equal(length(CFtimestamp(cf, "timestamp")), 4 * 30 * 24)

  expect_error(CFtime("days since 2023-01-01", "366_day", c("2021-01-01", "2021-04-13")))
  cf <- CFtime("days since 2023-01-01", "366_day", c("2023-01-01", "2023-04-13", "2023-10-30", "2023-05-12"))
  expect_equal(CFrange(cf), c("2023-01-01", "2023-10-30"))

  # Merge two CFtime instances / extend offsets
  cf1 <- CFtime("hours since 2001-01-01", "360_day", 0:99)
  cf2 <- CFtime("hours since 2001-01-01", "julian", 100:199)
  expect_false(cf1 == cf2)
  expect_error(cf1 + cf2)

  cf2 <- CFtime("hours since 2001-01-01", "360_day", 100:199)
  expect_false(cf1 == cf2)
  expect_equal(length(CFoffsets(cf1 + cf2)), 200)

  expect_equal(length(CFoffsets(cf1 + 100:199)), 200)

  cf1 <- CFtime("days since 2022-01-01", "365_day", 0:364)
  cf2 <- CFtime("days since 2023-01-01", "365_day", 0:364)
  expect_match(capture_output(methods::show(cf1)), "between 365 elements\\)$")
  expect_true(length(CFoffsets(cf1 + cf2)) == 730)
  expect_true(all(range(diff(CFoffsets(cf1 + cf2))) == c(1, 1)))
  expect_true(length(CFoffsets(cf2 + cf1)) == 730)
  expect_false((range(diff(CFoffsets(cf2 + cf1))) == c(1, 1))[1])

  # Time series completeness
  cf <- CFtime("d per 1991-01-01", "julian")
  expect_error(CFcomplete("zxcv"))
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
  expect_equal(CFrange(cf), c("2001-01-01", paste0(as.Date("2001-01-01") + 100)))

  # Subsetting
  cf <- CFtime("hours since 2023-01-01 00:00:00", "standard", 0:239)
  expect_error(CFsubset("zxcv"))
  x <- CFsubset(cf, c("2023-01-01", "2023-02-01"))
  expect_true(all(x))
  x <- CFsubset(cf, c("2023-01-01", "2023-05-01"))
  expect_true(length(which(x)) == 240)
  x <- CFsubset(cf, c("2023-01-01 00:00", "2023-01-01 04:00"))
  expect_true(length(which(x)) == 4)
  x <- CFsubset(cf, c("2023-01-01 04:00", "2023-01-01 00:00")) # extremes in reverse order
  expect_true(length(which(x)) == 4)
  expect_error(CFsubset(cf, c("2023-01-01 00:00", "2023-01-01 04:00", "2023-01-02 00:00"))) # 3 extremes
})

test_that("Working with files", {
  lf <- list.files(path = system.file("extdata", package = "CFtime"), full.names = TRUE)

  if (!requireNamespace("ncdf4")) return
  lapply(lf, function(f) {
    nc <- ncdf4::nc_open(lf[1])
    expect_s4_class(CFtime(nc$dim$time$units, nc$dim$time$calendar, nc$dim$time$vals), "CFtime")
    ncdf4::nc_close(nc)
  })
})
