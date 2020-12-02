test_that("multiplication works", {
  on.exit({options('farray.chunk_memory' = 80)
    farray::set_farray_threads(0)})

  farray::set_farray_threads(1)


  options('farray.chunk_memory' = 0.00001)

  orig <- rnorm(1e4)

  orig1 <- orig; dim(orig1) <- c(10, 1000)

  weights <- rnorm(1000)

  # 0.001
  system.time({
    xy0 <- orig1 %*% (t(orig1) * weights)
  })


  y <- as.fmatrix(t(orig1))


  x <- as.fmatrix(t(orig1))

  # 0.002
  system.time({
    xy <- crossprod(x, weights = weights)
  })
  expect_equal(range(xy[] - xy0), c(0,0))
})


