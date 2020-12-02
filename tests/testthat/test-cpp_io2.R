# devtools::load_all()

# require(testthat)




test_that("Loader2", {

  dim <- c(10,20,50)

  f <- normalizePath(tempfile(), mustWork = FALSE)
  on.exit({
    setFArrayBlockSize(-1)
    unlink(f)
  })

  x = array(1:prod(dim), dim)
  a = as.farray(x, path = f)
  loader_f <- function(..., samp, reshape, drop){
    farray:::subsetFM(a$storage_path, environment(), dim, getSexpType(samp), reshape, drop)
  }

  farray_test_unit <- function(samp_data, x_alt){
    loader_double = function(..., reshape = NULL, drop = FALSE){ loader_f(samp = samp_data, ..., reshape = reshape, drop = drop) }
    if(missing(x_alt)){
      x <- x; storage.mode(x) <- storage.mode(samp_data)
    } else {
      x <- x_alt; storage.mode(x) <- storage.mode(samp_data)
    }

    # 1. a()
    re <- loader_double()
    expect_equal(storage.mode(re), storage.mode(samp_data))
    expect_equal(re, x)
    expect_equal(dim(re), dim(x))

    # 2. a(i)
    # idx <- sample(length(x), size = 200, replace = TRUE)
    # idx[sample(200, 20)] = NA
    # re <- loader_double(idx)
    # cp <- x[idx]
    # expect_equivalent(re, cp)

    # 2. a(-i)
    # idx <- -sample(length(x), size = 200, replace = TRUE)
    # idx[sample(200, 20)] = NA
    # re <- loader_double(idx)
    # cp <- x[idx[!is.na(idx)]]
    # expect_equivalent(re, cp)

    # 2.1 large indices
    # re <- loader_double(10000000)
    # cp <- x[10000000]
    # expect_equivalent(re, cp)
    # expect_true(is.na(re))
    # re <- loader_double(-10000000)
    # cp <- x[-10000000]
    # expect_equal(re, cp)
    # expect_equal(dim(re), NULL)



    # 3. a(-i:i)
    expect_error(loader_double(-1:1))

    # 4. a(i,j,k)
    idx <- lapply(dim, sample, replace = TRUE, size = 6)
    re <- do.call(loader_double, idx)
    cp <- eval(as.call(c(list(quote(`[`), quote(x)), idx)))
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    # 5. negative subscripts
    ii <- sample(2)[[1]] + 1
    idx[[ii]] <- -idx[[ii]]
    re <- do.call(loader_double, idx)
    cp <- eval(as.call(c(list(quote(`[`), quote(x)), idx)))
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))
    ii <- 1
    idx[[ii]] <- -idx[[ii]]
    re <- do.call(loader_double, idx)
    cp <- eval(as.call(c(list(quote(`[`), quote(x)), idx)))
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    # 6. With missing
    re <- loader_double(,c(NA,1:0),c(2,NA,1))
    cp <- x[,c(NA,1:0),c(2,NA,1),drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(c(NA,1:0),c(2,NA,1),)
    cp <- x[c(NA,1:0),c(2,NA,1),,drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(c(NA,1:0),,c(2,NA,1))
    cp <- x[c(NA,1:0),,c(2,NA,1),drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(,,)
    cp <- x[,,,drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    # 7. drop
    re <- loader_double(,c(NA,1:0),c(2,NA,1), drop = TRUE)
    cp <- x[,c(NA,1:0),c(2,NA,1),drop = TRUE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))
    re <- loader_double(,c(NA,1:0),c(3), drop = TRUE)
    cp <- x[,c(NA,1:0),c(3),drop = TRUE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    # 8. reshape
    re <- loader_double(,c(NA,1:0),c(2,NA,1), reshape = c(20, 3))
    cp <- x[,c(NA,1:0),c(2,NA,1),drop = TRUE]
    expect_equal(as.vector(re), as.vector(cp))
    expect_equal(dim(re), c(20, 3))
    re <- loader_double(,c(NA,1:0),c(2,NA,1), reshape = c(60))
    expect_equal(dim(re), NULL)

    # 9. Negative with missing
    re <- loader_double(,-c(NA,1:0),c(2,NA,1))
    cp <- x[,-c(1:0),c(2,NA,1),drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(-c(NA,1:0),c(2,NA,1),)
    cp <- x[-c(1:0),c(2,NA,1),,drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(c(NA,1:0),,-c(2,NA,1))
    cp <- x[c(NA,1:0),,-c(2,1),drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(,,-1000)
    cp <- x[,,-1000,drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(,-1000,1)
    cp <- x[,-1000,1,drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))

    re <- loader_double(-1000,,)
    cp <- x[-1000,,,drop = FALSE]
    expect_equal(re, cp)
    expect_equal(dim(re), dim(cp))


    # Wrong usages
    expect_error(loader_double(100,,))
    expect_error(loader_double(1,1,1,1))
    expect_error(loader_double(1,1))
    expect_error(loader_double(,1,1,))
    expect_error(loader_double(,))
    expect_error(loader_double(,1000,))
    expect_error(loader_double(,,1000))
    expect_error(loader_double(1000,,1000))
    expect_error(loader_double(1000,1000,))
    expect_error(loader_double(,1000,1000))

  }

  #### no sub-blocks
  # farray_test_unit(0.1)
  farray_test_unit(1L)
  # farray_test_unit("")

  #### sub-blocks
  setFArrayBlockSize(1)
  # farray_test_unit(0.1)
  farray_test_unit(1L)
  # farray_test_unit("")

  setFArrayBlockSize(11)
  # farray_test_unit(0.1)
  farray_test_unit(1L)
  # farray_test_unit("")

  setFArrayBlockSize(201)
  # farray_test_unit(0.1)
  farray_test_unit(1L)
  # farray_test_unit("")

  setFArrayBlockSize(-1)

  #### sub-blocks With NAs
  unlink(a$get_partition_fpath(2))
  x[,,2] <- NA
  expect_equal(x[,,], a[])
  expect_equal(x[], a[])
  setFArrayBlockSize(1)
  expect_equal(x[,,], a[])
  expect_equal(x[], a[])
  # farray_test_unit(0.1, x)
  farray_test_unit(1L, x)
  # farray_test_unit("", x)

  setFArrayBlockSize(-1)
  x[,,2] <- NA
  expect_equal(x[,,], a[])
  expect_equal(x[], a[])
  # farray_test_unit(0.1, x)
  farray_test_unit(1L, x)
  # farray_test_unit("", x)


  ##### Complex?0
  # x = array(rnorm(prod(dim)), dim) + 1i * array(rnorm(prod(dim)), dim)
  # unlink(f, recursive = TRUE)
  # a = as.farray(x, path = f)
  # setFArrayBlockSize(-1)
  # farray_test_unit(x[[1]])
  #
  # setFArrayBlockSize(1)
  # farray_test_unit(x[[1]])
  #
  # setFArrayBlockSize(11)
  # farray_test_unit(x[[1]])
  #
  # setFArrayBlockSize(201)
  # farray_test_unit(x[[1]])
  #
  # setFArrayBlockSize(1)
  # unlink(a$get_partition_fpath(2))
  # x[,,2] <- NA
  # expect_equal(x[], a[])
  # farray_test_unit(x[[1]], x)
  #
  # setFArrayBlockSize(-1)
  # unlink(a$get_partition_fpath(2))
  # x[,,2] <- NA
  # expect_equal(x[], a[])
  # farray_test_unit(x[[1]], x)

  #### with matrix
  x <- matrix(1:16,4)
  a <- as.farray(x)
  loader_f <- function(..., drop = FALSE){
    farray:::subsetFM(a$storage_path, environment(), dim(x), getSexpType(1L), NULL, drop)
  }

  expect_equal(loader_f(), x)
  expect_equal(loader_f(,1), x[,1,drop=FALSE])
  expect_equal(loader_f(1,), x[1,,drop=FALSE])
  expect_equal(loader_f(,1,drop=TRUE), x[,1,drop=TRUE])
  expect_equal(loader_f(1,,drop=TRUE), x[1,,drop=TRUE])
})

