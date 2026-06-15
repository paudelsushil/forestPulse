test_that("years are binned into consecutive windows", {
  df  <- data.frame(year = c(1986, 1991, 2003, 2018))
  out <- create_timeclass(df, "year", start_date = 1985, end_date = 2020,
                          by = 5, types = "fire")
  expect_true("fireBy5yrs" %in% names(out))
  expect_equal(out$fireBy5yrs,
               c("1985-1989", "1990-1994", "2000-2004", "2015-2019"))
})

test_that("values outside every window become NA", {
  df  <- data.frame(year = c(1980, 2030))
  out <- create_timeclass(df, "year", 1985, 2020, 5, "fire")
  expect_true(all(is.na(out$fireBy5yrs)))
})

test_that("invalid arguments error", {
  df <- data.frame(year = 2000)
  expect_error(create_timeclass(df, "missing", 1985, 2020, 5, "fire"), "not found")
  expect_error(create_timeclass(df, "year", 1985, 2020, 0, "fire"), "positive")
})
