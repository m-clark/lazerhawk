#' Return a triangular matrix
#'
#' @description Returns the upper or lower triangle of a matrix.
#'
#'
#' @param x A matrix
#' @param diag logical. Include the diagonal? Default is false.
#' @param otherVal numeric or NA. what to make nonlower/upper values. \code{0} by
#'   default.
#' @param valuesOnly logical. Return only the lower/upper triangle values (including diag
#'   argument)
#'
#' @details This function returns the lower/upper triangle of a square matrix rather
#'   than just its indices (e.g. as lower.tri does). \code{otherVal} is only relevant
#'   if \code{valuesOnly} is \code{FALSE}.
#'
#' @seealso \code{\link[base]{lower.tri}},  \code{\link[base]{diag}},  \code{\link[base]{matrix}}
#'
#' @return A lower or upper triangular matrix, possibly with diagonal. If
#'   \code{valuesOnly} is \code{TRUE}, then only the vector of values are
#'   returned.
#'
#' @examples
#' library(lazerhawk)
#' (m <- matrix(1:9, 3, 3, byrow=TRUE))
#' lower.tri(m)
#' lowerTri(m)
#' lowerTri(m, diag=TRUE, otherVal=NA)
#' lowerTri(m, valuesOnly=TRUE)
#' upperTri(m, diag=TRUE, otherVal=NA)

#' @export
lowerTri = function(x, diag=FALSE, valuesOnly=FALSE, otherVal=0){
  # initial checks
  if (!is.matrix(x)) stop('Need a matrix')
  if (dim(x)[1] != dim(x)[2]) stop('non-square matrix')
  if (!is.numeric(otherVal) & !is.na(otherVal)) stop('Need a numeric value or NA for otherVal')

  x[row(x) < col(x)] = otherVal

  if(!diag){
    diag(x) = otherVal
  }

  if(!valuesOnly){
    x
  } else {
    x[lower.tri(x, diag=diag)]
  }
}

#' @rdname lowerTri
#' @export
upperTri = function(x, diag=FALSE, valuesOnly=FALSE, otherVal=0){
  # initial checks
  if (!is.matrix(x)) stop('Need a matrix')
  if (dim(x)[1] != dim(x)[2]) stop('non-square matrix')
  if (!is.numeric(otherVal) & !is.na(otherVal)) stop('Need a numeric value or NA for otherVal')

  x[col(x) < row(x)] = otherVal

  if(!diag){
    diag(x) = otherVal
  }

  if(!valuesOnly){
    x
  } else {
    x[upper.tri(x, diag=diag)]
  }
}

