test_that("VPD is self-consistent with saturation vapour pressure", {
  # With tmin = tmax = T and rh_min = rh_max = RH, VPD = es(T) * (1 - RH/100).
  expect_equal(calc_vpd(20, 20, 50, 50),
               calc_saturation_vapor_pressure(20) * 0.5)
  # Saturated air (RH 100%) has zero deficit.
  expect_equal(calc_vpd(20, 20, 100, 100), 0)
})

test_that("clamp keeps VPD non-negative and is vectorised", {
  out <- calc_vpd(c(5, 10), c(20, 25), c(30, 25), c(90, 85))
  expect_length(out, 2)
  expect_true(all(out >= 0))
})

test_that("mismatched input lengths error", {
  expect_error(calc_vpd(c(5, 10), 20, 30, 90), "equal length")
})
