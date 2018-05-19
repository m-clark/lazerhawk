#' @title Get numerical summaries
#' @description Essentially a slight tweak to the base R summary.
#'
#' @param x A numeric or logical variable
#' @param digits Argument to \code{round}. Default is 1.
#' @param extra Add things like number distinct, number of zeros, and more as I
#'   think of them.
#'
#' @details  Takes a numeric variable and provides sample size, mean, standard
#'   deviation, min, first quartile, median, third quartile, and max, and number
#'   of NAs. Extras include the number of distinct values, and the number of
#'   zeros.
#' @return A data.frame containing those values.
#' @importFrom stats na.omit
#' @examples
#' num_summary(c(1:10, NA))
#' num_summary(c('1','2','3'))
#' @export
num_summary <- function(x, digits=1, extra=F) {
  # initial check of variable type
  test_x = tryCatch(as.numeric(x), warning = function(c) {
    msg <- conditionMessage(c)
    invisible(structure(msg, class = "try-warning"))
  })
  if(class(test_x) == 'try-warning') stop('Need something numeric or which can reasonably be converted to such.
                                          \nThe current variable would introduce NAs by coercion.')

  x = as.numeric(x)
  d = data.frame(
    N = length(na.omit(x)),
    data.frame(t(c(summary(x)))),
    SD = sd(x, na.rm=T),
    Missing = sum_NA(x)
  ) %>%
    # currently there is a bug that keeps this from working
    # rename_all(funs(gsub(., pattern='.', replacement=''))) %>%
    rename(Q1 = X1st.Qu.,
           Q3 = X3rd.Qu.) %>%
    select(N, Mean, SD, everything(), -matches('NA'))   # drop summary() NA if present
  colnames(d) = gsub(colnames(d), pattern='\\.', replacement='')

  if (extra) {
    d$Distinct = n_distinct(x)
    d$Zeros = sum(x == 0, na.rm = T)
  }

  d %>% mutate_if(is.numeric, round, digits=digits)
}
