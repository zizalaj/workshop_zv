test_that("Czech date helpers are deterministic", {
  expect_equal(format_cz_date(as.Date("2026-09-01")), "1. 9. 2026")
  expect_equal(
    format_cz_datetime(as.POSIXct("2026-09-01 14:30:00", tz = "UTC")),
    "1. 9. 2026 14:30"
  )
  expect_equal(format_cz_month_label(as.Date("2026-09-01")), "zář 2026")
})
