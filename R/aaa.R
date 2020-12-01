#' @import utils
#' @import stats
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @useDynLib farray, .registration = TRUE
NULL

#' Check if Package \code{'dipsaus'} has been installed
#' @export
has_dipsaus <- function(){
  system.file('', package = 'dipsaus') != ''
}

