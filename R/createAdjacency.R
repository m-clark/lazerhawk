#' @description creates a symmetric adjacency matrix from a simple data frame or
#'   matrix object that notes the (undirected) connections.
#'
#' @details The idea is to have this functionality without requiring any
#' special graph or network object. n1 and n2 specify columns where the nodes in
#' n1 are connected to nodes in n2. Value is an optional column name that will
#' specify the weight of the connection See get.adjacency in igraph for an
#' alternative.
#' @value A symmetric adjacency matrix with rows and columns pertaining to the
#'   unique values found in n1 and n2

createAdjacency <- function(data, n1, n2, value=NULL, diagonal=NULL) {
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


createEdges <- function(adjmat){
  tidyr::gather(data.frame(id=colnames(adjmat), adjmat), target, value, -id) %>%
    filter(id!=target) %>%
    rename(source=id)
}
