#' One-hot encoding
#' @description  Add indicators for all desired variables in a data set.
#'
#' Deprecated and moved to tidyext package.
#'
#' @param data A data frame
#' @param var A character string/vector of names to be encoded. If NULL, the
#'   default, all character and factor variables will be encoded.
#' @param nas What to do with missing values. For na.omit and na.exclude, any
#'   observations with missing data will be removed from the result. With
#'   na.pass, the default, the result will retain the missing values. Otherwise,
#'   with na.fail, an error will be thrown.
#' @param sparse Logical (default FALSE). If true, will return only the encoded
#'   variables as a sparse matrix.
#' @param keep.original Logical (default FALSE). Keep the original variables?
#'   Not an option if sparse is TRUE.
#'
#' @details This function is a simple one-hot encoder, with a couple options
#'   that are commonly desired.  Takes the applicable variables and creates a
#'   binary indicator column for each unique value. If supplied
#'   non-factor/character variables, it will coerce them to characters and
#'   proceed accordingly. Will handle missingness, return a sparse matrix, or
#'   keep the original variable(s) as desired.
#'
#' @return A data.frame with the encoded variables, or a sparse matrix of only
#'   the encoded variables.
#' @seealso \code{\link[stats]{model.matrix}}
#' @importFrom stats model.matrix
#' @export
#'
#' @examples
#' \dontrun{
#' library(lazerhawk)
#' str(onehot(iris, keep.original = TRUE))
#' str(onehot(iris, sparse = TRUE))
#' str(onehot(mtcars, var = c('vs','cyl')))
#'
#' iris2 = iris
#' iris2[sample(1:150, 25),] = NA
#' str(onehot(iris2))
#' str(onehot(iris2, nas = 'na.omit'))
#' }


onehot <- function(data,
                   var=NULL,
                   nas='na.pass',
                   sparse=FALSE,
                   keep.original=FALSE) {

  .Deprecated('tidyext::onehot')

  if (!inherits(data,  'data.frame')) stop('Need a data frame.')
  if (sparse & keep.original==TRUE) message('Original data dropped when sparse is TRUE')

  if (is.null(var)) {
    f_c = sapply(data, inherits, c('factor', 'character'))
  } else {
    f_c = colnames(data) %in% var
  }

  if (all(!f_c)) stop("You didn't supply variable names,
                      and none of the data is character or factor.
                      If you really meant to do this,
                      supply colnames(data) to the var argument")

  constants = sapply(data[f_c], n_distinct) == 1
  if (any(constants)) {
    message('You have supplied a constant. It will be ignored.')
    f_c = f_c[!constants]
  }

  if (length(f_c) == 0) stop('No variables left to consider.')

  any_numeric = any(sapply(data[f_c], inherits, 'numeric'))
  if (any_numeric) message("
  You have supplied numeric variables.
  Attempts were made to keep the
  column names consistent, but you'll want to check.")

  # deal with NAs
  init_na = options('na.action')
  options(na.action = nas)
  if (nas %in% c('na.omit', 'na.exclude')) data = na.omit(data)
  on.exit(options(na.action = init_na$na.action))

  # encode
  if (sparse) {
    res = data[f_c] %>%
      mutate_all(as.character) %>%
      map(function(x) Matrix::sparse.model.matrix(~ x - 1, data=.))
  } else {
    res = data[f_c] %>%
      mutate_all(as.character) %>%
      map(function(x) model.matrix(~ x - 1, data=.) %>% as.data.frame())
  }

  # extract and fix names (if non-numeric)

  if (sparse) {
    l_names = names(res)

    for (i in seq_along(res)) {
      dimnames(res[[i]])[2][[1]] = paste0(l_names[i], '_', dimnames(res[[i]])[2][[1]])
    }

    res = do.call(cbind, res)
  } else {
    res = do.call(cbind, res)
    colnames(res) = gsub(colnames(res), pattern='.x|.X', replacement='_')
  }

  # return
  if (sparse) {
    # cbind(data.matrix(data[!f_c]), res)
    res
  } else {
    if (keep.original) {
      data.frame(data[!f_c], data[f_c], res)
    } else {
      data.frame(data[!f_c], res)
    }
  }
}
