test_that("W/m2 converts to MJ/m2/day with the FAO-56 factor", {
  expect_equal(convert_radiation(100), 8.64)            # 100 * 0.0864
  expect_equal(convert_radiation(c(0, 100, 250)), c(0, 8.64, 21.6))
})

test_that("conversion round-trips and identity holds", {
  expect_equal(convert_radiation(convert_radiation(100),
                                 from = "MJ/m2/day", to = "W/m2"), 100)
  expect_equal(convert_radiation(42, from = "W/m2", to = "W/m2"), 42)
})

test_that("unsupported conversions and bad input error", {
  expect_error(convert_radiation(1, from = "cal", to = "W/m2"), "Unsupported")
  expect_error(convert_radiation("x"), "numeric")
})
