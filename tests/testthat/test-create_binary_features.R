test_that("one 0/1 factor column is added per class", {
  df  <- data.frame(severity = c("Low", "High", "Low"))
  out <- suppressMessages(
    create_binary_features(df, "severity", c("Low", "High")))
  expect_true(all(c("Low", "High") %in% names(out)))
  expect_equal(as.character(out$Low),  c("1", "0", "1"))
  expect_equal(as.character(out$High), c("0", "1", "0"))
  expect_equal(levels(out$Low), c("0", "1"))
})

test_that("missing target column errors", {
  df <- data.frame(severity = c("Low", "High"))
  expect_error(create_binary_features(df, "nope"), "not found")
})
