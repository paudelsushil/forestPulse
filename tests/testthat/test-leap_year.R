test_that("the internal leap-year test follows the Gregorian rule", {
  expect_equal(
    forestPulse:::.is_leap(c(2000L, 1900L, 2004L, 2003L, 2400L)),
    c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
})
