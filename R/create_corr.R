#' Create a covariance/correlation matrix
#'
#' @description Creates a correlation or covariance matrix given a vector of
#'   values for the lower triangle.
#'
#' @param x A numeric vector
#' @param diagonal A vector of variances
#'
#' @details This function takes the provided values and creates a symmetric (covariance) matrix, e.g. for simulation or demonstration purposes.  The values are filled column-wise in the lower triangle first. For example \code{mat[1,2] = x[1]}, \code{mat[1,3] = x[2]} and so on.
#'
#' @examples
#' library(lazerhawk)
#' cors = runif(6, -1, 1)
#' create_corr(cors)
#' covs = runif(3, 1, 5)
#' create_corr(covs, diag=c(1,2,3))

#' @export
create_corr = function(x, diagonal=NULL){
  if(!is.numeric(x)) stop('x needs to be numeric.')
  if(any(is.na(x)))  stop('x cannot have missing values.')

  if (is.null(diagonal)) {
    nc = ceiling(sqrt(2*length(x)))
  } else {
    if(!is.numeric(diagonal)) stop('diagonal needs to be numeric.')
    nc = length(diagonal)
  }

  mat = matrix(NA, nc, nc)
  mat[lower.tri(mat)] = x
  mat[upper.tri(mat)] = t(mat)[upper.tri(t(mat))]
  if(is.null(diagonal)){
    diag(mat) = 1
  } else {
    diag(mat) = diagonal
  }
  mat
}

