#' Investigate missingness
#' @description  Deprecated and moved to tidyext package.
#' @param x a vector with multiple elements
#'
#' @details These functions return the number of NA, NaN, or blank values in a
#'   vector.  Such information can serve as a diagnostic or used outright, e.g.
#'   in longitudinal settings. If you are dealing with singletons, you should
#'   just use the underlying function, e.g. is.na. For total blank values, the
#'   stringi package is required.
#'
#' @return The number of non-elements so defined
#'
#' @examples
#' \dontrun{
#' library(lazerhawk)
#' sum_blank(c('    ', '', 'c', 'd'))
#' sum_NA(c(NA, '', 'c', NA))
#' sum_NaN(c(1:3, NA, NaN))
#' sum_NA(1:4)
#'}
#'
#'
#' @export
sum_NA <- function(x) {
  .Deprecated('tidyext::sum_NA')
  sum(is.na(x))
}

#' @rdname sum_NA
#' @export
sum_NaN <- function(x) {
  .Deprecated('tidyext::sum_NaN')
  if(!is.numeric(x)) stop('x must potentially be a number in order to not be a number. ~ The Buddha')
  sum(is.nan(x))
}

#' @rdname sum_NA
#' @export
sum_blank <- function(x) {
  .Deprecated('tidyext::sum_blank')
  sum(stringi::stri_trim_both(x)=='')
}

