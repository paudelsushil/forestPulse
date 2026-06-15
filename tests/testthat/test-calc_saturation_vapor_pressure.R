test_that("FAO56 saturation vapour pressure matches reference values", {
  # At 0 C the Tetens/FAO-56 form reduces to the leading constant.
  expect_equal(calc_saturation_vapor_pressure(0), 0.6108)
  # ~2.339 kPa at 20 C (FAO-56 table).
  expect_equal(calc_saturation_vapor_pressure(20), 2.3389, tolerance = 1e-3)
})

test_that("it is vectorised and method-aware", {
  out <- calc_saturation_vapor_pressure(c(0, 10, 20))
  expect_length(out, 3)
  expect_true(all(diff(out) > 0))                       # monotone increasing
  expect_false(isTRUE(all.equal(
    calc_saturation_vapor_pressure(20, "FAO56"),
    calc_saturation_vapor_pressure(20, "Murray"))))     # methods differ
})

test_that("non-numeric input errors", {
  expect_error(calc_saturation_vapor_pressure("warm"), "numeric")
})
