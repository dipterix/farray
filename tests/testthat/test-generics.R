test_that("summary statistics", {
  x <-
    farray::farray(tempfile(),
                         dim = c(100, 100),
                         storage_format = 'double')
  x[, 1] <- 1:100
  x[, 2] <- NA
  x[, 3] <- c(0, rep(NA, 99))


  testthat::expect_true(is.na(min(x)))
  testthat::expect_true(is.na(max(x)))

  testthat::expect_equal(min(x, na.rm=TRUE), 0, ignore_attr = TRUE)
  testthat::expect_equal(max(x, na.rm=TRUE), 100, ignore_attr = TRUE)
  testthat::expect_equal(range(x, na.rm=TRUE), c(0, 100), ignore_attr = TRUE)
  testthat::expect_equal(range(x), c(NA, NA), ignore_attr = TRUE)

  testthat::expect_equal(mean(x, na.rm = TRUE), 50, ignore_attr = TRUE)
  testthat::expect_equal(sum(x, na.rm = TRUE), 5050, ignore_attr = TRUE)

  # partition_apply(x, mean)

})
