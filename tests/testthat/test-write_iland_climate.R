make_clim <- function() {
  data.frame(year = 2001L, month = 1:2, day = 1L,
             min_temp = c(-1, 0), max_temp = c(5, 6),
             prec = c(0, 2), rad = c(8, 9), vpd = c(0.3, 0.4))
}

test_that("a single validated table is written and read back", {
  skip_if_not_installed("RSQLite")
  f <- tempfile(fileext = ".sqlite")
  on.exit(unlink(f), add = TRUE)

  expect_equal(write_iland_climate(make_clim(), f, table_name = "climate1"),
               "climate1")

  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  got <- DBI::dbReadTable(con, "climate1")
  DBI::dbDisconnect(con)

  expect_equal(nrow(got), 2L)
  expect_setequal(names(got),
                  c("year", "month", "day", "min_temp", "max_temp",
                    "prec", "rad", "vpd"))
})

test_that("split_col writes one table per cluster", {
  skip_if_not_installed("RSQLite")
  long <- rbind(cbind(cluster = "c1", make_clim()),
                cbind(cluster = "c2", make_clim()))
  f <- tempfile(fileext = ".sqlite")
  on.exit(unlink(f), add = TRUE)

  tabs <- write_iland_climate(long, f, split_col = "cluster")
  expect_setequal(tabs, c("c1", "c2"))

  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_setequal(DBI::dbListTables(con), c("c1", "c2"))
  expect_false("cluster" %in% names(DBI::dbReadTable(con, "c1")))
})

test_that("missing required columns error before any write", {
  skip_if_not_installed("RSQLite")
  f <- tempfile(fileext = ".sqlite")
  on.exit(unlink(f), add = TRUE)
  bad <- data.frame(year = 2001L, month = 1L, day = 1L)  # temps/prec/rad/vpd absent
  expect_error(write_iland_climate(bad, f), "missing required column")
})
