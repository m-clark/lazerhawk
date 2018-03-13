
#' combn_2_col
#' @description Convert a character or factor of multiple labels.
#'
#' @param data The data frame in question.
#' @param var The quoted name for the variable in question. The variable can be
#'   character or factor.
#' @param sep The label separator, for example a comma or space.
#' @param collapse In the names of the new columns, how do you want the label
#'   combinations separated?
#' @param max_m The maximum number of possible combinations. Default is 1.
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
#'   Usually in this situation it's a result of poor data entry, and you'll
#'   likely need to do a little text pre-processing just to get started.
#'
#'   This can actually be used for one hot encoding if max_m is set to 1, though
#'   I'll make a more efficient version of that process in a  later function.
#'   The combo column becomes superfluous in this case.
#'
#'   If you don't need combinations and each cell has the same pattern of entry,
#'   you could use \code{tidyr::separate}.
#' @return
#'
#' @examples
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
#' @export
combn_2_col <- function(data, var, sep='[^[:alnum:]]+', max_m=1, collapse = '_') {
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
  data[, combo_cols] = sapply(data$combo, function(x) combo_cols %in% x) %>% t()
  data
}

