test_that("Subset farray", {
  a <- array(rnorm(80), c(2,4,2,5))
  x <- as.farray(a)

  expect_equal(x[], a)

  # idx <- sample(length(x), length(x) * 10, replace = TRUE)
  # expect_equal(x[idx], a[idx])

  # matrix
  x <- as.fmatrix(a)
  b <- x[]

  expect_equal(as.integer(dim(b)), c(16,5))

  expect_equal(x[], b)

  # idx <- sample(length(x), length(x) * 10, replace = TRUE)
  # expect_equal(x[idx], b[idx])

  expect_equal(x[,1], b[,1])
  expect_equal(x[,1,drop=FALSE], b[,1,drop=FALSE])

  # x <- t(x)
  # b <- t(b)
  #
  # expect_equal(dim(x), c(5,16))
  #
  # expect_equal(x[], b)
  #
  # idx <- sample(length(x), length(x) * 10, replace = TRUE)
  # expect_equal(x[idx], b[idx])
  #
  # expect_equal(x[,1], b[,1])
  # expect_equal(x[,1,drop=FALSE], b[,1,drop=FALSE])

})

test_that("Subset filearray", {
  a <- array(rnorm(80), c(2,4,2,5))
  f <- tempfile()
  x <- FileArray$new(f, dim = dim(a), storage_format = 'double', read_only = FALSE)
  x[] <- a

  expect_equal(x[], a)

  i <- sample(4,replace = TRUE)
  i[sample(4,2)] <- NA
  expect_equal(x[,i,,i], a[,i,,i])

  expect_equal(x[,,,-i], a[,,,-i[!is.na(i)]])

  j <- sample(2,replace = TRUE)
  i <- sample(5,replace = TRUE)

  i <- 1:5
  expect_equal(x[,j,,-i], a[,j,,-i[!is.na(i)]])
  # expect_equal(x[j,j,j,-i], a[j,j,j,-i[!is.na(i)]])

  expect_equal(x[,j,,-i,drop=FALSE], a[,j,,-i[!is.na(i)],drop=FALSE])
  expect_equal(x[j,j,j,-i,drop=FALSE], a[j,j,j,-i[!is.na(i)],drop=FALSE])


  x[,,,1] <- a[,,,2]
  a[,,,1] <- a[,,,2]

  expect_equal(x[], a)

  i <- sample(5,replace = TRUE)
  i[sample(5,2)] <- NA
  expect_equal(x[,,,i], a[,,,i])

  expect_equal(x[,,,-i], a[,,,-i[!is.na(i)]])

  j <- sample(2,replace = TRUE)
  i <- sample(5,replace = TRUE)
  expect_equal(x[,j,,-i], a[,j,,-i[!is.na(i)]])
  expect_equal(x[j,j,j,-i], a[j,j,j,-i[!is.na(i)]])

  expect_error(x[,,,6])

  expect_equal(x[,,,-6], a)

})

