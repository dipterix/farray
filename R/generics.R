
#' Generate partition summary statistics for array objects along the last
#' dimension
#' @param x an array or `farray`
#' @param na.rm whether to remove `NA` when calculating summary statistics
#' @param ... passed to other methods or ignored
#' @return A data frame with the following possible columns: `Min`,
#' `Max`, `Mean`, `Standard Deviation`, `NAs` (total number
#'  of `NA`), and `Length`.
#' @name partition_table
#' @examples
#'
#' # R array
#' x <- array(1:27, c(3,3,3))
#' partition_table(x)
#'
#' # farray
#' x <- farray(tempfile(), storage_format = 'double', dim = c(3,3,3))
#' x[] <- 1:27
#' partition_table(x, quiet=TRUE)
#'
#' @export
partition_table <- function(x, na.rm = FALSE, ...){
  UseMethod('partition_table')
}

#' @rdname partition_table
#' @export
partition_table.array <- function(x, na.rm = FALSE, ...){
  dim <- dim(x)
  suppressWarnings({
    smry <- t(apply(x, length(dim), function(d){
      c(
        min(d, na.rm = na.rm),
        max(d, na.rm = na.rm),
        mean(d, na.rm = na.rm),
        sd(d, na.rm = na.rm),
        sum(is.na(d)),
        length(d)
      )
    }))
  })
  smry <- as.data.frame(smry)
  names(smry) <- c('Min', 'Max', 'Mean', 'Standard Deviation', 'NAs', 'Length')
  smry
}

#' @rdname partition_table
#' @export
partition_table.AbstractFArray <- function(x, na.rm = FALSE, ...){
  smry <- summary(x, na.rm = FALSE, ...)
  re <- smry$partitions
  re$Count <- re$Length - re$NAs
  re
}


#' Apply function along the last dimension of an array and aggregate the results
#' @name partition_map
#' @param x R array or `farray`
#' @param map_fun function that takes in a slice of array and an optional
#' argument indicating current partition number
#' @param reduce function that accept a list of results returned by
#' `map_fun`, can be missing
#' @param partitions integers of partitions, i.e. the slices of array to be
#' applied to, can be missing. If missing, then applies to all partitions
#' @param ... internally used
#' @return If `reduce` is missing, returns a list of results. Each result
#' is returned by `map_fun`, and the total length equals to number of
#' partitions mapped. If `reduce` is a function, that list of results will
#' be passed to `reduce` and `partition_map` returns the results
#' generated from `reduce`.
#' @examples
#'
#' # -------------------------- Ordinary R array ---------------------------
#'
#' x <- array(1:24, c(2,3,4))
#' partition_map(x, function(slice, part){
#'   sum(slice)
#' })
#'
#' # When reduce and partitions are missing, the following code is equivalent
#' as.list(apply(x, 3, sum))
#'
#' # When reduce is present
#' partition_map(x, function(slice, part){
#'   sum(slice)
#' }, function(slice_sum){
#'   max(unlist(slice_sum))
#' })
#'
#' # equivalently, we could call
#' slice_sum <- partition_map(x, function(slice, part){
#'   sum(slice)
#' })
#' max(unlist(slice_sum))
#'
#' # When partition is specified
#' # Partition 1, 2, and 4 exist but 5 is missing
#' # when a partition is missing, the missing slice will be NA
#' partition_map(x, function(slice, part){
#'   sum(slice)
#' }, partitions = c(1,2,4,5))
#'
#' # -------------------------- farray ---------------------------
#' x <- farray(tempfile(), storage_format = 'integer', dim = c(2,3,4))
#' x[] <- 1:24
#'
#' partition_map(x, function(slice, part){
#'   slice[1, ,] * slice[2, ,]
#' }, reduce = function(mapped_prod){
#'   mean(unlist(mapped_prod))
#' })
#'
#'
#'
#' @export
partition_map <- function(x, map_fun, reduce, partitions, ...){
  UseMethod('partition_map')
}

#' @export
partition_map.array <- function(x, map_fun, reduce, partitions, ...){
  if(length(formals(map_fun)) == 1){
    mfun <- function(x, part){
      map_fun(x)
    }
  } else {
    mfun <- map_fun
  }
  dim <- dim(x)
  available_partitions <- seq_len(dim[[length(dim)]])
  substr <- paste(rep('', length(dim)), collapse = ',')

  if(missing(partitions)){
    partitions <- available_partitions
  }

  res <- lapply(partitions, function(part){
    if(part %in% available_partitions){
      slice <- eval(parse(text = sprintf('x[%s%d,drop=FALSE]', substr, part)))
    } else {
      slice <- array(NA, c(dim[-length(dim)], 1))
    }
    mfun(slice, part)
  })

  if(!missing(reduce) && is.function(reduce)){
    res <- reduce(res)
  }

  res
}

#' @export
partition_map.AbstractFArray <- function(x, map_fun, reduce, partitions, further_split = FALSE, ...){
  if(missing(partitions)){
    partitions <- seq_len(x$npart)
  } else {
    partitions <- as.integer(partitions)
    partitions <- partitions[partitions > 0 & partitions <= x$npart]
  }

  if(length(formals(map_fun)) == 1){
    mfun <- function(x, part){
      map_fun(x)
    }
  } else {
    mfun <- map_fun
  }

  mapped <- lapply2(partitions, function(part){
    mfun(x$get_partition_data(part), part)
  })

  if(!missing(reduce)){
    mapped <- reduce(mapped)
  }
  mapped

}


#' Apply functions to all partitions, but small chunks each time
#' @seealso [partition_map()]
#' @param x a `farray` or R array
#' @param map_fun function to apply to each chunk
#' @param reduce similar to `reduce` in [partition_map()]
#' @param max_nchunks maximum number of chunks. If number of chunks is too
#' large, then `chunk_size` will be re-calculated.
#' @param chunk_size integer chunk size. If `chunk_size` is too small, it
#' will be ignored
#' @param ... ignored or passed to other methods
#' @return If `reduce` is missing, returns a list of results. Each result
#' is returned by `map_fun`, and the total length equals to number of
#' chunks mapped. If `reduce` is a function, that list of results will
#' be passed to `reduce` and `chunk_map` returns the results
#' generated from `reduce`.
#' @details The difference between `chunk_map` and
#' `partition_map` is the margin or direction to apply mapping
#' functions. In `partition_map`, mapping function is applied to
#' each partition. If `x` is a matrix, this means applying to each column.
#' `chunk_map` generate small chunks along all dimensions except the last,
#' and apply mapping functions to each chunks. If `x` is a matrix, it
#' make chunks along rows and apply mapping functions along rows.
#' @examples
#'
#' x <- as.fmatrix(matrix(1:100, ncol = 2))
#' x
#'
#' # Set max_nchunks=Inf and chunk_size=10 to force total number of chunks
#' # is around nrow(x)/10 and each chunk contains at most 10 rows
#' chunk_map(x, function(chunk){chunk[1:2,]}, chunk_size = 10, max_nchunks = Inf)
#'
#' # For each chunks, calculate mean, then calculate the mean of chunk mean
#' chunk_map(x, function(chunk) {
#'   colMeans(chunk)
#' }, function(chunk_means) {
#'   Reduce('+', chunk_means) / length(chunk_means)
#' })
#'
#' colMeans(x[])
#'
#'
#' @export
chunk_map <- function(x, map_fun, reduce, max_nchunks, chunk_size, ...){
  UseMethod('chunk_map')
}


#' @export
chunk_map.AbstractFArray <- function(x, map_fun, reduce, max_nchunks, chunk_size, partitions = 'all', ...){

  if(missing(max_nchunks)){
    # calculate such that each chunk size is at most 0.5GB
    max_nchunks <- auto_chunks(x)
  }
  new_x <- as.fmatrix(x)
  new_x$make_readonly()

  if(missing(chunk_size)){
    chunk_size <- 1024L
  }
  mapped <- x$`@chunk_map`(map_function = map_fun, max_nchunks = max_nchunks, chunk_size = chunk_size, partitions = partitions)

  if(!missing(reduce)){
    mapped <- reduce(mapped)
  }

  return(mapped)
}


#' @export
as.vector.AbstractFArray <- function(x, mode = "any"){
  as.vector(x[drop=FALSE], mode = mode)
}

#' @export
as.array.AbstractFArray <- function(x, ...){
  x[drop=FALSE]
}

#' @export
as.matrix.AbstractFArray <- function(x, ...){
  dim <- c(x$partition_length, x$npart)
  x[reshape = dim]
}
