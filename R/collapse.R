
#' @title Collapse an array along certain margins
#' @description Collapse an array by calculating summation or average
#' along certain dimensions.
#' @param x an array, a matrix, or a [`farray`] instance
#' @param along margin of the array
#' @param method whether to calculate summation or average, choices are
#' `'sum'` and `'average'`.
#' @param ... passed to other methods.
#' @details For matrices, we often calculate [`colSums`], [`colMeans`] (collapse by
#' first margin), or [`rowSums`], [`rowMeans`] (collapse by second margin). For
#' arrays, the collapse operation can be much more flexible than matrices.
#'
#' Suppose array `x` has dimension `100x200x300`, then collapse along the
#' first margin results in `200x300` matrix, and collapse along the first two
#' margins results in a vector of length `300`.
#'
#' For regular arrays, the dimensions of collapsed results equal to
#' `length(dim(x))-length(along)`. For `farray`s, the result dimension
#' will be at least two.
#'
#' @return A collapsed array. For [`farray`], returns a `farray` instance.
#' @examples
#'
#'
#' # ------------------------------ Example: matrix ---------------------------
#' x <- array(1:10, c(2,5))
#'
#' collapse2(x, 1)   # identical to colSums(x)
#'
#' collapse2(x, 2)   # identical to rowSums(x)
#'
#' collapse2(x, c(1,2)) # identical to sum(x)
#'
#' # ----------------------------------- farray -------------------------------
#'
#' x <- as.farray(array(1:100, c(2,5,10)))
#' x
#'
#' collapse2(x, 1)
#'
#' collapse2(x, c(1,3))
#'
#'
#' @export
collapse2 <- function(x, along, method = c("sum", "average"), ...){
  UseMethod("collapse2")
}

#' @rdname collapse2
#' @export
collapse2.default <- function(x, along, method = c("sum", "average"), ...){
  along <- unique(along)
  stopifnot(length(along) > 0)

  method <- match.arg(method)
  average <- method == "average"
  xdim <- dim(x)
  ndims <- length(xdim)
  keep <- seq_len(ndims)
  keep <- keep[!keep %in% along]
  if(!length(keep)){
    if(average){
      return(mean(average))
    } else {
      return(sum(x))
    }
  }
  if(has_dipsaus()){
    return(dipsaus::collapse(x, keep, average))
  }
  if(average){
    return(apply(x, keep, mean))
  } else {
    return(apply(x, keep, sum))
  }
}

#' @rdname collapse2
#' @param path where to store the results; default is [tempfile()]; see [farray]
#' @return Collapsed array in [farray] format.
#' @export
collapse2.AbstractFArray <- function(x, along, method = c("sum", "average"), path = tempfile(), ...){
  along <- unique(along)
  stopifnot(length(along) > 0)

  method <- match.arg(method)
  average <- method == "average"
  xdim <- dim(x)
  ndims <- length(xdim)
  keep <- seq_len(ndims)
  keep <- keep[!keep %in% along]


  if(!length(keep)){
    if(average){
      return(mean(x))
    } else {
      return(sum(x))
    }
  }

  # check the largest idx
  slice_margin <- keep[[length(keep)]]

  collapse_margin <- !is_fast_margin(xdim, slice_margin)
  if(collapse_margin){
    keep <- c(keep, length(xdim))
    slice_margin <- ndims
    tpath <- tempfile()
    on.exit({
      if(dir.exists(tpath)){
        unlink(tpath, recursive = TRUE, force = TRUE)
      }
    }, after = TRUE, add = TRUE)
  } else {
    tpath <- path
  }

  desc_dim <- xdim[keep]

  if(has_dipsaus()){
    collapse_func <- function(slice, keep){
      dipsaus::collapse(slice, keep, average = average)
    }
  } else if (average){
    collapse_func <- function(slice, keep){
      apply(slice, keep, mean)
    }
  } else {
    collapse_func <- function(slice, keep){
      apply(slice, keep, sum)
    }
  }

  # in favor of this slice type
  loc <- replicate(ndims, get_missing_value(), simplify = FALSE)
  getSlice <- function(idx){
    loc[[slice_margin]] <- idx
    expr <- as.call(c(list(quote(`[`), quote(x), drop=FALSE), loc))
    eval(expr)
  }

  if(length(desc_dim) == 1){
    # no need to collapse_margin
    res <- sapply(seq_len(xdim[slice_margin]), function(ii){
      slice <- getSlice(ii)
      collapse_func(slice, keep)
    }, simplify = TRUE)
    desc <- as.farray(res, path, dim = c(length(res), 1), storage_format = 'double')
  } else {
    if(length(desc_dim) != ndims){
      desc <- farray(tpath, dim = desc_dim, read_only = FALSE, storage_format = 'double')

      lapply2(seq_len(xdim[slice_margin]), function(ii){
        slice <- getSlice(ii)
        desc$set_partition_data(ii, collapse_func(slice, keep))
      })

    } else {
      desc <- x
    }

    if(collapse_margin){
      res <- array(0.0, desc$partition_dim())
      # res <- 0
      for(ii in seq_len(desc$npart)){
        addTo(res, desc$get_partition_data(part = ii))
        # res <- res + desc$get_partition_data(part = ii)
      }
      if(length(desc_dim) > 2){
        dim(res) <- desc_dim[-length(desc_dim)]
      }
      if(average){
        res <- res / desc$npart
      }
      desc <- as.farray(res, path = path, storage_format = 'double')
    }
  }


  desc

}
