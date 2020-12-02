

test_that("Getter/Setter mode = 1", {

  self <- farray(tempfile(), storage_format = 'double', dim = c(1,3,4))
  private <- self$.__enclos_env__$private
  value <- array(1:4, c(1,2,2))

  expect_true(self$can_write)

  self[1.0, 1:2, 1:2] <- value
  # self$`@set_data`(value, 1.0, 1:2, 1:2)

  expect_equal(subsetFM(self$storage_path, list(1,2,1), self$dim, 14L, NULL, TRUE), 2)
  # expect_equal(self$`@get_data`(1, 2, 1), 2)

  expect_equal(subsetFM(self$storage_path, list(1, 1:3, 1), self$dim, 14L, NULL, drop=TRUE), c(1,2,NA))
  # expect_equal(self$`@get_data`(1, 1:3, 1), c(1,2,NA))

  expect_equal(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, rep(NA, 6)), c(1,3,4)), ignore_attr = TRUE)

  expect_error({ self[1, 1:2, c(3,10)] <- 1:4 })
  self[1, 1:2, c(3,4)] <- 1:4

  # change to read_only
  private$.read_only <- TRUE
  expect_false(self$can_write)

  expect_equal(self[drop = FALSE], array(c(1,2,NA, 3,4,NA, 1,2,NA, 3,4,NA), c(1,3,4)), ignore_attr = TRUE)

  expect_error({self[1, 1:2, c(3,4)] <- 1:4})

  # clean up
  self$remove_data()

  expect_false(dir.exists(private$.path))

  # operations will result in error
  expect_error(self[1,1,1])

})





