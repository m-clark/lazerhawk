#' Combinations to columns
#'
#' @description Convert a character or factor of multiple labels.  Deprecated and moved to tidyext package.
#'
#' @param data The data frame in question.
#' @param var The quoted name for the variable in question. The variable can be
#'   character or factor.
#' @param sep The label separator, for example a comma or space.
#' @param collapse In the names of the new columns, how do you want the label
#'   combinations separated?
#' @param max_m The maximum number of possible combinations. Default is 1.
#' @param toInteger Convert the logical result to integers of 0,1.
#' @param sparse Return only the new indicators as a sparse matrix?
#'
#' @details This comes up every once in a while.  Someone has for whatever
#'   reason coded multiple labels into cells within a single column, and now you
#'   need those individual labels for analysis. This function will create
#'   indicator columns for every combination of labels up to max_m labels. It
#'   will also return a list column, called 'combo', a version of the original,
#'   but where the entries are more usable vectors of labels, which might be
#'   useful for further processing.
#'
#'   Note that the number of possible combinations grows very quickly when there
#'   are many unique labels, well more than your machine can handle, so use
#'   sensible values. Check with combn(n_unique_labels, n_combinations) if you
#'   think there might be an issue.
#'
#'   Usually this situation is a result of poor data entry, and you'll likely
#'   need to do a little text pre-processing just to get started.
#'
#'   This can actually be used for one hot encoding if max_m is set to 1, though
#'   I'll make a more efficient version of that process in a  later function.
#'   The combo column becomes superfluous in this case.
#'
#'   If you don't need combinations and each cell has the same pattern of entry,
#'   you could use \code{tidyr::separate}.
#'
#'   I tested this against a \code{model.matrix} approach and two text-analysis
#'   approaches (see examples), and with a problem that was notably more
#'   sizeable than the examples. Using \code{model.matrix} wasn't viable with
#'   even that size, and surprisingly, a simple tidytext approach was
#'   consistently fastest. However, this implementation is parallelizable in two
#'   parts, and requires nothing beyond what comes with a base R installation,
#'   so it wins.
#'
#' @return A data frame with the new indicator columns, or a sparse matrix of only the indicator columns.
#' @examples
#' \dontrun{
#' library(lazerhawk)
#' d = data.frame(id = 1:4,
#'                labs = c('A/B', 'B/C/D/E', 'A/E', 'D/E'))
#' test = combn_2_col(data=d, var='labs', max_m=3)
#' test
#' str(test)
#' d$labs =  c('A B', 'B C D E', 'A E', 'D E')
#' combn_2_col(data=d, var='labs', max_m=1)
#' d$labs =  c('Tom, Dick & Harriet', "J'Sean", "OBG, Andreas", NA)
#' combn_2_col(data=d, var='labs', sep=',', max_m=2, collapse='-')
#'
#' # requires at least tidytext
#' tidy_dtm <- function(data, var, sep='-', max_m=3) {
#'   init = stringr::str_split(data[[var]], pattern = sep) # creates a list of separated letters
#'
#'   # the following gets the combos with a dot separating drugs in a given combo
#'   # this first lapply could be parallelized if need be and is probably slowest
#'   # probably want to change to m = min(c(4, m)) so as to only limit to 4
#'   # see also, combinat::combn which is slightly faster than base R below
#'   observation_combos = init %>%
#'     lapply(function(x)
#'       sapply(seq_along(x), function(m)
#'         utils::combn(x,  min(max_m, m), FUN=paste, collapse = '_')))
#'
#'   # now we have a standard text analysis problem in need of a document term matrix
#'   documents = observation_combos %>% lapply(unlist)
#'
#'   # create a 'tidy' form of documents and terms; each term (i.e. combo) only
#'   occurs once in a document
#'   doc_df = data.frame(id=rep(data$id, sapply(documents, length)),
#'                       combos=unlist(documents),
#'                       count=1)  # each term only occurs once in the document
#'   doc_df %>%
#'     tidytext::cast_dfm(document=id, term=combos, value=count)
#'   }
#'
#' # requires at least text2vec
#' ttv <- function(data, var, sep='-', max_m=3) {
#'   docs = sapply(stringr::str_split(data[[var]], pattern=sep),
#'                 function(str_vec)
#'                   sapply(seq_along(str_vec),
#'                          function(m)
#'                            combn(str_vec,
#'                                  m = min(max_m, m),
#'                                  FUN = paste,
#'                                  collapse = '_')
#'                   ) %>% unlist()
#'   )
#'
#'   toks = itoken(docs, progressbar = FALSE)
#'   vocab = create_vocabulary(toks)
#'   create_dtm(toks, vectorizer = vocab_vectorizer(vocab), progressbar = FALSE) %>%
#'     as.matrix() %>%
#'     cbind(data,.)
#' }
#'
#' }
#'
#'
#'
#' @export
combn_2_col <- function(data,
                        var,
                        sep='[^[:alnum:]]+',
                        max_m=1,
                        collapse = '_',
                        toInteger=FALSE,
                        sparse=FALSE) {

  .Deprecated('tidyext::combn_2_col')

  if (is.null(data) | is.null(var)) stop('Need data and variable name to continue.')
  if (max_m < 1) stop('Need positive value for max_m.')

  data$combo =
    sapply(stringr::str_split(data[[var]], pattern=sep),
           function(str_vec)
             sapply(seq_along(str_vec),
                    function(m)
                      combn(str_vec,
                            m = min(max_m, m),
                            FUN = paste,
                            collapse = collapse)
             ) %>% unlist()
    )
  combo_cols = unique(unlist(data$combo))

  if (sparse) return(sapply(data$combo, function(x) as.integer(combo_cols %in% x)) %>%
                       t() %>%
                       Matrix::Matrix(sparse = T))

  if (toInteger) {
    data[, combo_cols] = sapply(data$combo, function(x) as.integer(combo_cols %in% x)) %>% t()
  } else {
    if (sparse) return()
    data[, combo_cols] = sapply(data$combo, function(x) combo_cols %in% x) %>% t()
  }
  data
}

