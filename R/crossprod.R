
#' Matrix Crossproduct
#' @param x a `farray` or an R matrix
#' @param y `NULL` or matrix
#' @param weights numeric vector used as weight
#' @param ... passed to further methods
#' @return Matrix of cross product
#' @name crossprod
#'
#' @examples
#'
#' x <- matrix(1:100, 50)
#' crossprod(x)
#'
#' fmat_x <- as.fmatrix(x)
#' crossprod(fmat_x)[]
#'
#' weights <- (1:50)/50
#'
#' t(x) %*% diag(weights) %*% x
#' crossprod(fmat_x, weights = weights)
#'
#' \dontrun{
#'
#' # large data set ~ 1.6GB
#' x <- as.fmatrix(matrix(rnorm(2e8), ncol = 2))
#'
#' crossprod(x)
#' }
#'
#'
NULL

#' @rdname crossprod
#' @exportMethod crossprod
setMethod("crossprod", signature(x="AbstractFArray", y = 'AbstractFArray'), function(x, y = NULL, weights = NULL, ...){
  farray_crossprod(x, y, weights = weights, ...)
})

#' @rdname crossprod
#' @exportMethod crossprod
setMethod("crossprod", signature(x="AbstractFArray", y = 'NULL'), function(x, y = NULL, weights = NULL, ...){
  farray_crossprod(x, NULL, weights = weights, ...)
})

#' @rdname crossprod
#' @exportMethod crossprod
setMethod("crossprod", signature(x="AbstractFArray", y = "missing"), function(x, y = NULL, weights = NULL, ...){
  farray_crossprod(x, NULL, weights = weights, ...)
})

#' @rdname crossprod
#' @exportMethod crossprod
setMethod("crossprod", signature(x="AbstractFArray", y = 'matrix'), function(x, y = NULL, weights = NULL, ...){
  if(!is.null(weights)){
    stopifnot(length(weights) == x$partition_length)
    res <- lapply(seq_len(x$npart), function(ii){
      x$get_partition_data(ii, reshape = c(1, x$partition_length)) %*% (y * weights)
    })
  } else {
    res <- lapply(seq_len(x$npart), function(ii){
      x$get_partition_data(ii, reshape = c(1, x$partition_length)) %*% y
    })
  }

  do.call('rbind', res)
})


farray_crossprod <- function(x, y = NULL, weights = NULL, ...){

  if(!is.null(weights)){
    stopifnot(length(weights) == x$partition_length)
  }

  new_x <- as.fmatrix(x)
  new_x$make_readonly()
  if(is.null(y)){
    yisx <- TRUE
    new_y <- new_x
  } else {
    yisx <- isTRUE(x$storage_path == y$storage_path && x$get_file_format() == y$get_file_format())
    new_y <- as.fmatrix(y)
  }

  if(length(weights)){
    ftile <- filematrix::fm.create(tempfile(), nrow = length(weights), ncol = 1)
    ftile[] <- weights
    on.exit(filematrix::close(ftile))

    chunk_map(new_x, map_fun = function(data, ii, idx_range){
      idx <- seq.int(idx_range[[1]], idx_range[[2]])
      if(yisx){
        return(crossprod(data, data * as.vector(ftile[idx,1])))
      } else {
        sub_y <- y[idx,,drop=FALSE] * as.vector(ftile[idx,1])
        return(crossprod(data, sub_y))
      }
    }, reduce = function(mapped){
      Reduce('+', mapped)
    }, ...)

  } else {
    chunk_map(new_x, map_fun = function(data, ii, idx_range){

      if(yisx){
        return(crossprod(data))
      } else {
        sub_y <- y[seq.int(idx_range[[1]], idx_range[[2]]),,drop=FALSE]
        return(crossprod(data, sub_y))
      }
    }, reduce = function(mapped){
      Reduce('+', mapped)
    }, ...)
  }

}
