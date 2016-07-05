#' Create an adjacency matrix, or convert one to an edgelist
#' @description Creates a symmetric adjacency matrix from a simple data frame or
#'   edgelist from a matrix object that notes the connections.
#'
#' @param data A data frame containing the nodes.
#' @param n1 column representing the first set of nodes.
#' @param n2 column representing the second set of nodes.
#' @param value column representing the weight (otherwise will be set to 1).
#' @param diagonal value desired for the diagonal (defaults to a value of 1).
#'
#' @details The idea is to have this functionality without requiring any special
#'   graph or network object, as typical graphs can be represented rather simply
#'   using a data frame. n1 and n2 specify columns where the nodes in n1 are
#'   connected to nodes in n2. Value is an optional column name that will
#'   specify the weight of the connection See get.adjacency in igraph for an
#'   alternative.
#'
#'   \code{createEdges} assumes an adjacency matrix like that produced by \code{createAdjacency}

#' @return A symmetric adjacency matrix with rows and columns pertaining to the
#'   unique values found in n1 and n2
#'
#' @examples
#' library(lazerhawk); library(dplyr)
#' nodeData = data.frame(pair=1:10, node1 = sample(letters[1:4], 10, replace=TRUE),
#'                       node2 = sample(LETTERS[1:4], 10, replace=TRUE))
#' adjmat = createAdjacency(nodeData, n1='node1', n2='node2')
#' adjmat
#'
#' createEdges(adjmat)
#' createEdges(adjmat, symmetric=TRUE)


#' @export
createAdjacency <- function(data, n1, n2, value=NULL, diagonal=NULL) {
  assertthat::assert_that(is.data.frame(data))
  nams = sort(unique(unlist(select_(data, n1, n2))))
  data = as.matrix(data)

  adjmat = diag(1, length(nams)); rownames(adjmat) = colnames(adjmat) = nams

  for (i in 1:nrow(data)){
    r = data[i, c(n1, n2)]
    adjmat[r, rev(r)] = ifelse(is.null(value), 1, data[,value][i])
  }

  if (!is.null(diagonal)) diag(adjmat) = diagonal
  adjmat
}

#' @rdname createAdjacency
#'
#' @param adjmat A matrix like that produced with \code{createAdjacency}
#' @param zeroEdges Include all possible edges or only those with values > 0?
#' @param symmetric Include both A -> B and B -> A? FALSE assumes an undirected graph.
#' @param diag Include diagonal (i.e. self connections)?
#'
#' @export
createEdges <- function(adjmat, zeroEdges=FALSE, symmetric=FALSE, diag=FALSE){
  assertthat::assert_that(!is.null(colnames(adjmat)))
  if(!symmetric & !diag) adjmat = lowerTri(adjmat)
  if(symmetric & !diag) diag(adjmat) = 0
  if(!symmetric & diag) adjmat = lowerTri(adjmat, diag=TRUE)

  edgeMat = tidyr::gather(data.frame(id=colnames(adjmat), adjmat), target, value, -id)
  edgeMat = dplyr::rename(edgeMat, source=id)

  if(zeroEdges){
    edgeMat
  } else{
    edgeMat[edgeMat$value != 0, ]
  }
}
