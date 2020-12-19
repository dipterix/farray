test_that("collapse", {
  x <- rnorm(120)
  dim(x) <- 2:5
  x <- as.farray(x)

  along_idx <- list(
    1,2,3,4,c(1,2), c(2,3), c(3,4), c(1,3), c(2,4), c(1,4), c(1,2,3,4),
    c(1,2,3), c(2,3,4), c(1,3,4), c(1,2,4)
  )

  test_f <- function(along){
    keep <- 1:4; keep <- keep[!keep %in% along]
    res <- farray_collapse(x, along = along, method = 'sum')
    if(length(keep)){
      cmp <- apply(x[], keep, sum)
    } else {
      cmp <- sum(x[])
    }
    rg <- range(res[] - cmp)
    expect_lt(max(abs(rg)), 1e-9)
    if(inherits(res, 'FileArray')){
      res$remove_data(force = TRUE, warn = FALSE)
    }


    res <- farray_collapse(x, along = along, method = 'average')
    if(length(keep)){
      cmp <- apply(x[], keep, mean)
    } else {
      cmp <- mean(x[])
    }
    rg <- range(res[] - cmp)
    expect_lt(max(abs(rg)), 1e-9)
    if(inherits(res, 'FileArray')){
      res$remove_data(force = TRUE, warn = FALSE)
    }
  }

  lapply(along_idx, test_f)

  farray:::setFArrayBlockSize(1)
  lapply(along_idx, test_f)

  farray:::setFArrayBlockSize(1, 1)
  lapply(along_idx, test_f)

  farray:::setFArrayBlockSize(-1, -1)



})
