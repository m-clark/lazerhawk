#' Pairwise operations
#' @description Apply a function to all possible pairwise combinations of rows or columns.

#' @param x A matrix class object or data.frame that can be coerced to a numeric matrix.
#' @param margin Work on rows (1) or columns (2)
#' @param fun The function to apply. Must be applicable to numeric data.
#'
#' @details The function must work on all rows (columns) provided.  While a
#'   data.frame is accepted, it will be coerced to a matrix, and so its rows
#'   (columns) must be atomic vectors that are not character strings.  While it
#'   might be expanded to be more flexible at some point, it should be thought
#'   of as a way to create things like distance or covariance matrices with
#'   arbitrary functions. At some point a parallel version will be added.
#'
#' @examples
#' library(lazerhawk)
#' # dot product
#' pairwise(mtcars, 1,  function(x,y) sum(x*y))
#'
#' # correlation
#' pairwise(mtcars, 2,  cor)
#'
#' # correlation for factors
#' mydata = data.frame(a = factor(sample(letters[1:3], 10, replace=TRUE)),
#'                     b = factor(sample(letters[1:4], 10, replace=TRUE)))
#' pairwise(mydata, 2,  cor)

#' @importFrom utils combn

#' @export
pairwise <- function(x, margin=2, fun) {
  if (!all(sapply(x, class) != 'character'))
    stop('All elements must be coercible to numeric.')

  if (is.data.frame(x)) x = sapply(x, as.numeric)
  if(margin==1) x = t(x)

  nc = ncol(x); cnames = colnames(x)
  pairlist = combn(1:nc, 2)
  lt = apply(pairlist, 2, function(pair) fun(x[,pair[1]], x[,pair[2]]))
  diagonal = sapply(1:nc, function(elem) fun(x[,elem], x[,elem]))
  res = create_corr(lt, diagonal=diagonal)
  colnames(res) = rownames(res) = cnames

  res
}
