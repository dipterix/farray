#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @importFrom yaml read_yaml
#' @importFrom yaml write_yaml
#' @import utils
#' @import stats
#' @importFrom methods setGeneric
#' @importFrom methods setMethod
#' @importFrom methods signature
#' @useDynLib farray, .registration = TRUE
NULL

setOldClass(c('FileArray', 'AbstractFArray', 'R6'))


setGeneric("typeof")
setGeneric("crossprod")




#' Type of `farray`
#' @param x a `farray` or an R object
#' @return The storage type of data stored in the input
#' @exportMethod typeof
setMethod("typeof", signature(x="AbstractFArray"), function(x){
  x$storage_format
})




