#' @noRd
#' @title Internal Class definition for `FileArray`
#' @author Zhengjia Wang
#' @description Internal class definition of `'farray'` objects
FileArray <- R6::R6Class(
  classname = "FileArray",
  portable = TRUE,
  inherit = AbstractFArray,
  private = list(
    .backend = "filearray"
  ),
  public = list(
    print = function(...){
      cat("<FileArray> (", private$.storage_format, ')\n', sep = '')
      cat('Dimension:\t', paste(sprintf('%d ', private$.dim), collapse = 'x '), '\n')
      cat('File format: bmat\n')
      invisible(self)
    },

    initialize = function(path, dim, storage_format = 'double',
                          read_only = TRUE, meta_name = 'farray.meta'){
      private$.file_format <- "bmat"
      if(missing(dim)){
        super$initialize(path = path, storage_format = storage_format,
                         read_only = read_only, meta_name = meta_name)
      } else {
        super$initialize(path = path, dim = dim, storage_format = storage_format,
                         read_only = read_only, meta_name = meta_name)
      }
      # cannot convert types
      if(length(self$raw_meta$storage_format) && storage_format != self$raw_meta$storage_format){
        stop("Data format inconsistent (cannot type-convert). Header info: ",
             self$raw_meta$storage_format, "; provided: ", storage_format)
      }
    },

    get_partition_fpath = function(part, full_path = TRUE, summary_file = FALSE, type = c('data', 'desc', 'combined')){
      type <- match.arg(type)
      if(missing(part)){
        part <- seq_len(self$npart)
      } else {
        part <- as.integer(part)
        if(base::anyNA(part) || any(part <= 0)){
          stop("partition number must be all positive: ", part)
        }
      }
      res <- sprintf('%s%s', part, self$get_file_format())
      if(full_path){
        res <- file.path(private$.path, res)
      }
      if(summary_file){
        res <- sprintf('%s.summary', res)
        return(res)
      }
      if(type == 'desc'){
        res <- sprintf('%s.desc.txt', part)
      } else if(type == 'combined'){
        res <- sprintf('%s', part)
      } else {
        res <- sprintf('%s%s', part, self$get_file_format())
      }
      if(full_path){
        res <- file.path(private$.path, res)
      }
      res
    },

    has_partition = function(part){
      stopifnot(length(part) == 1)
      file <- self$get_partition_fpath(part, full_path = TRUE)
      desc <- self$get_partition_fpath(part, full_path = TRUE, type = 'desc')
      if(file.exists(file) && file.exists(desc)){
        return(TRUE)
      }
      return(FALSE)
    },
    initialize_partition = function(part, nofill = FALSE){
      if(!self$has_partition(part)){
        file <- self$get_partition_fpath(part, full_path = TRUE, type = 'combined')
        ptr <- filematrix::fm.create(file, self$partition_length, 1, type = self$storage_format)
        on.exit({
          filematrix::close(ptr)
        }, add = TRUE)
        if(!nofill){
          ptr[] <- rep(self$sample_na, self$partition_length)
        }

        return(TRUE)
      }
      return(FALSE)
    },
    get_partition_data = function(part, reshape = NULL){
      if(self$has_partition(part)){

        idx <- replicate(self$ndim, {
          get_missing_value()
        })

        idx[[self$ndim]] <- part
        idx$reshape <- reshape
        idx$drop <- FALSE
        return(eval(as.call(c(list(quote(`[`), quote(self)), idx))))
      } else {
        array(self$sample_na, self$partition_dim())
      }
    },
    set_partition_data = function(part, data) {
      if(!length(data) %in% c(1, self$partition_length)){
        pfile <- self$get_partition_fpath(part = part, full_path = TRUE, type = 'combined')
        if(self$has_partition(part)){
          fm <- filematrix::fm.open(pfile, readonly = FALSE)
        } else {
          fm <- filematrix::fm.create(pfile, nrow = self$partition_length, ncol = 1, type = self$storage_format)
        }
        on.exit({
          filematrix::close(fm)
        }, add = TRUE)
        if(length(data) == 1){
          data <- rep(data, self$partition_length)
        }
        fm[] <- data
      }
      invisible()
    }
  )
)


#' @export
`[.FileArray` <- function(x, ..., drop = TRUE, reshape = NULL){
  if(!x$is_valid){
    stop("`[.FileArray`: x is no longer valid (data has been removed).")
  }
  if(!is.null(reshape)){
    reshape <- as.numeric(reshape)
    stopifnot(all(reshape>=0))
  }
  drop <- isTRUE(drop)

  # set block size to be the first margin to maximize reading speed
  # block_size <- getFArrayBlockSize()
  # if(block_size >= dim(x)[[1]]){
  #   block_size <- dim(x)[[1]]
  #   setFArrayBlockSize(block_size)
  #
  #   on.exit({
  #     # reset block size for farrays
  #     setFArrayBlockSize(-1)
  #   }, add = TRUE)
  # }

  subsetFM(rootPath = x$storage_path,listOrEnv = environment(),
           dim = x$dim,dtype = x$sexptype,reshape = reshape,drop = drop)
}

#' @export
`[<-.FileArray` <- function(x, ..., value){
  if(!x$is_valid){
    stop("`[<-.FileArray`: x is no longer valid (data has been removed).")
  }
  if(!x$can_write){
    stop("`[<-.FileArray`: x is read-only")
  }

  parsed <- parseAndScheduleBlocks2(environment(), x$dim, 0)
  # parsed <- parseAndScheduleBlocks2(list(1:10,2:10,3:10,4:5), x$dim, 0)
  # parsed <- parseAndScheduleBlocks2(list(idx,idx,get_missing_value(),get_missing_value()), x$dim, 0)

  if(parsed$subset_mode == 1){
    stop("FileArray does not support single subscript (x[i]<-v), try x[]<-v or x[i,j,k,...]<-v")
  }
  partition_length <- prod(x$partition_dim())

  # x[]
  if(parsed$subset_mode == 2){
    value <- array(value, dim = x$dim)
    fake_idx <- lapply(x$dim, function(x){ get_missing_value() })
    slice_value <- function(ii){
      fake_idx[[x$ndim]] <- ii
      do.call(`[`, c(list(quote(value)), fake_idx))
    }
    # copy all to re inplace
    lapply(seq_len(x$npart), function(ii){
      x$initialize_partition(part = ii, nofill = TRUE)
      file <- x$get_partition_fpath(ii, full_path = TRUE, type = 'combined')
      ptr_file <- filematrix::fm.open(file)
      on.exit({ filematrix::close(ptr_file) })
      ptr_file[] <- slice_value(ii)
    })
  } else {
    # x[i,j,k]
    loc <- parsed$location_indices
    if(!is.numeric(loc[[x$ndim]])){
      # missing, all partitions
      partitions <- seq_len(x$npart)
    } else {
      partitions <- loc[[x$ndim]]
    }
    # check if the schedule is made
    schedule <- parsed$schedule
    block_ndims <- schedule$block_ndims
    block_length_orig <- prod(schedule$block_dimension)  # prod(dim[1:block_ndims])

    blocksize <- schedule$block_expected_length  # prod(target_dim[1:block_ndims])

    if(schedule$block_indexed){
      value <- array(value, dim = c(blocksize, length(schedule$schedule_index), length(partitions)))
    } else {
      value <- array(value, dim = c(blocksize * length(schedule$schedule_index), length(partitions)))
    }


    lapply(seq_along(partitions), function(ff){
      file_ii <- partitions[[ff]]
      # No file, NA
      x$initialize_partition(part = file_ii)
      file <- x$get_partition_fpath(file_ii, full_path = TRUE, type = 'combined')
      ptr_file <- filematrix::fm.open(file, readonly = FALSE)
      on.exit({
        filematrix::close(ptr_file)
      }, add = TRUE)

      if(schedule$block_indexed){
        # file exists
        for(ii in seq_along(schedule$schedule_index)){
          schedule_ii <- schedule$schedule_index[[ii]]
          row_number <- block_length_orig * (schedule_ii-1) + schedule$block_schedule

          if(any(row_number <= 0)){
            stop("NAs are not allowed in subscripted assignments")
          }

          # sel <- row_number > 0
          sel <- !duplicated(row_number)
          ptr_file[row_number[sel], 1] <- as.vector(value[sel,ii,ff])
        }
      } else {
        # ndim == 2

        arr <- ptr_file[]
        dim(arr) <- x$partition_dim()
        loc_local <- loc
        loc_local[[x$ndim]] <- 1
        expr <- as.call(c(list(quote(`[<-`), quote(arr)), loc_local, list(quote(value[,ff]))))
        arr <- eval(expr)
        ptr_file[] <- as.vector(arr)

      }

    })

  }

  invisible(x)
}


# `[<-.FileArray` <- function(x, ..., value){
#   if(!x$is_valid){
#     stop("`[<-.FileArray`: x is no longer valid (data has been removed).")
#   }
#   if(!x$can_write){
#     stop("`[<-.FileArray`: x is read-only")
#   }
#
#   if(length(value)){
#
#     parsed <- parseAndScheduleBlocks2(environment(), x$dim)
#     # parsed <- parseAndScheduleBlocks2(list(1:10,2:10,3:10,4:10), x$dim, 1)
#     # parsed <- parseAndScheduleBlocks2(list(1,1,1,1), x$dim, 1)
#
#     if(parsed$subset_mode == 1){
#       stop("FileArray does not support single subscript (x[i]<-v), try x[]<-v or x[i,j,k,...]<-v")
#     }
#     partition_length <- prod(x$partition_dim())
#
#     # x[]
#     if(parsed$subset_mode == 2){
#       value <- array(value, dim = x$dim)
#       fake_idx <- lapply(x$dim, function(x){ get_missing_value() })
#       slice_value <- function(ii){
#         fake_idx[[x$ndim]] <- ii
#         re <- as.vector(do.call(`[`, c(list(quote(value)), fake_idx)))
#         storage.mode(re) <- x$storage_format
#         re
#       }
#       # copy all to re inplace
#       for(ii in seq_len(x$npart)){
#         file <- x$get_partition_fpath(ii, type = 'data', full_path = TRUE)
#         writeBin(object = slice_value(ii), con = file, endian = 'little')
#         # cpp_writeBin2(normalizePath(file, mustWork = TRUE), slice_value(ii), skip = 0)
#       }
#     } else {
#       # x[i,j,k]
#
#       if(storage.mode(value) != storage.mode(x$sample_na)){
#         storage.mode(value) <- storage.mode(x$sample_na)
#       }
#       # // ensure partition
#       # cpp_fillPartition(filePath, partition_length, na, false);
#       lapply2(parsed$schedule$partition_index, function(part){
#         x$initialize_partition(part, nofill = FALSE)
#       })
#
#       subsetAssignFM(normalizePath(x$storage_path), environment(), x$dim, as.vector(value));
#     }
#   }
#
#
#   invisible(x)
# }
