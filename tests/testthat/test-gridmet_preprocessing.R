test_that("gridMET preprocessing harmonises columns and converts units", {
  gm <- data.frame(
    cluster = "c1", date = as.Date("2000-01-01") + 0:1,
    tmmn = c(273.15, 283.15), tmmx = c(283.15, 293.15),
    pr = c(0, 5), srad = c(100, 200),
    rmin = c(30, 40), rmax = c(90, 95))

  out <- gridmet_preprocessing(gm, cluster_col = "cluster", date_col = "date",
                               temp_unit = "K", vpd_source = "compute")

  expect_true(all(c("cluster", "year", "month", "day", "min_temp", "max_temp",
                    "prec", "rad", "vpd") %in% names(out)))
  expect_equal(out$min_temp, c(0, 10))                 # K -> C
  expect_equal(out$max_temp, c(10, 20))
  expect_equal(out$rad, convert_radiation(c(100, 200)))
})

test_that("a missing required band is reported", {
  gm <- data.frame(cluster = "c1", date = as.Date("2000-01-01"),
                   tmmn = 273.15, tmmx = 283.15, pr = 0)   # srad missing
  expect_error(
    gridmet_preprocessing(gm, "cluster", "date", vpd_source = "gridmet"),
    "missing required column"
  )
})
