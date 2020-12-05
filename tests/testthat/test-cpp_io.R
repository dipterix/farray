# test_that("Low-level save functions", {
#   f <- tempfile()
#   f <- normalizePath(f, mustWork = FALSE)
#   cpp_fillPartition2(f, n = 100, NA_real_, overwrite = FALSE)
#   expect_equal(file.size(f), 800)
#
#   cpp_fillPartition2(f, n = 1000^2, NA_real_, overwrite = FALSE)
#   expect_equal(file.size(f), 8 * 1000^2)
# })

test_that("Saver", {

  f <- tempfile()
  dim <- c(100,100,100,2)
  x <- farray(f, dim = dim)
  reset_x <- function(){
    lapply(x$get_partition_fpath(), function(x){
      unlink(x)
    })
  }
  reset_x()
  a <- array(as.double(seq_len(prod(dim))), dim)

  expect_false(x$has_partition(1))
  x$initialize_partition(1, nofill = FALSE)
  expect_true(x$has_partition(1))
  expect_equal(file.size(x$get_partition_fpath(1)) / 8L, x$partition_length)

  # Check the total saving
  system.time({x[] <- a})
  expect_equal(x[], a[])

  # single element saving
  reset_x()
  system.time({x[] <- NA})
  expect_equal(sum(!is.na(x[])), 0)

  expect_true(x$has_partition(1))


  # -------- partial saving with sequential index and non-scheduled --------
  reset_x()
  expect_false(x$has_partition(1))
  farray:::setFArrayBlockSize(0L, 1L)
  expect_equal(farray:::getFArrayBlockSize(1), 1L)
  subparsed <- farray:::parseAndScheduleBlocks2(list(1:10,2:10,1:2,1), dim, forceSchedule = FALSE)
  expect_false(subparsed$schedule$block_indexed)

  reset_x()
  system.time({x[,,,] <- a})
  expect_equal(x[], a[])

  expect_true(x$has_partition(1))
  expect_equal(file.size(x$get_partition_fpath(1)) , 8 * x$partition_length)

  # partial saving with random index
  idx <- sample(100, 20)
  x[idx, idx, , ] <- a[idx, idx, , ]
  expect_equal(x[idx, idx, , ], a[idx, idx, , ])

  # partial saving with duplicated values
  reset_x()
  idx <- c(2, 2)
  system.time({x[idx, idx, , ] <- a[idx, idx, , ]})
  expect_equal(x[idx, idx, , ], a[idx, idx, , ])
  expect_equal(x[1:3,1:3,,] , x[c(NA, 2, NA), c(NA, 2, NA),,])

  # partial saving with duplicated values and NA
  reset_x()
  idx <- c(2, NA, 2)
  expect_error({
    system.time({x[idx, idx, , ] <- a[idx, idx, , ]})
  })

  # ------------------------ schedule blocks: indexed --------------------
  setFArrayBlockSize(-1,-1,-1)
  subparsed <- farray:::parseAndScheduleBlocks2(list(1:10,2:10,1:2,1), dim, forceSchedule = FALSE)
  expect_true(subparsed$schedule$block_indexed)

  reset_x()
  system.time({x[,,,] <- a})
  expect_true(x$has_partition(1))
  expect_equal(file.size(x$get_partition_fpath(1)) , 8 * x$partition_length)

  expect_equal(x[], a[])

  # partial saving with random index
  idx <- sample(100, 20); idx
  system.time({x[idx, idx, , ] <- a[idx, idx, , ]})
  expect_equal(x[idx, idx, , ], a[idx, idx, , ])

  # partial saving with duplicated values and NA
  reset_x()
  idx <- c(2, NA)
  expect_error({
    system.time({x[idx, idx, , ] <- a[idx, idx, , ]})
  })

  reset_x()
  idx <- c(2, 2)
  system.time({x[idx, idx, , ] <- a[idx, idx, , ]})
  expect_equal(file.size(x$get_partition_fpath(1)) , 8 * x$partition_length)
  expect_equal(x[idx, idx, , ], a[idx, idx, , ])
  expect_equal(x[1:3,1:3,,] , x[c(NA, 2, NA), c(NA, 2, NA),,])


  # system(sprintf('open "%s"', normalizePath(x$storage_path)))
  # junk <- readBin(x$get_partition_fpath(1), 'double', 1048576, 8L)

})
