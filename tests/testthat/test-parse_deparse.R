test_that("timestamp string parsing to offsets and deparsing of offsets to timestamps", {
  # This test tests: global CFt* constants, CFtime(), CFparse(), CFtimestamp():
  # decomposing offsets into timestamp elements, generating timestamp strings,
  # parsing timestamp strings back into timestamp elements.
  for (c in CFt$calendars$name) {
    for (u in 1:4) {
      offsets <- 1:10000
      def <- paste(CFt$units$name[u], "since 1953-08-20")
      cf <- CFtime(def, c, offsets)
      ts <- CFtimestamp(cf, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- CFparse(cf2, ts)
      expect_equal(tp, cf@time)
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
      ts <- CFtimestamp(cf, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- CFparse(cf2, ts)
      expect_equal(tp, cf@time)
    }
  }
})

test_that("Testing milli-second timestamp string parsing to offsets and deparsing
          of offsets to timestamps", {
  # This test tests: global CFt* constants, CFtime(), CFparse(), CFtimestamp():
  # decomposing offsets into milli-second timestamp elements, generating timestamp strings,
  # parsing timestamp strings back into timestamp elements.
  for (c in CFt$calendars$name) {
    for (u in 1:2) {
      offsets <- runif(10000, max = 10000)
      def <- paste(CFt$units$name[u], "since 1953-08-20 07:34:12.2")
      cf <- CFtime(def, c, offsets)
      ts <- CFtimestamp(cf, "timestamp")
      cf2 <- CFtime(def, c)
      tp <- CFparse(cf2, ts)
      expect_equal(tp[1:6], cf@time[1:6])
    }
  }
})

test_that("Disallow parsing of timestamps on month and year datums", {
  for (c in CFt$calendars$name) {
    for (u in 5:6) {
      def <- paste(CFt$units$name[u], "since 2020-01-01")
      cf <- CFtime(def, c, 0:23)
      ts <- CFtimestamp(cf, "timestamp")
      expect_error(CFparse(cf, ts))
    }
  }
})
