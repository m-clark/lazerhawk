#' @title num_summary
#' @description Essentially a slight tweak to the base R summary.
#'
#' @param x A numeric or logical variable
#' @param digits Argument to \code{round}. Default is 1.
#'
#' @details  Takes a numeric variable and provides sample size, mean, standard deviation, min, first quartile,
#'   median, third quartile, and max, and number of NAs.
#' @return A data.frame containing those values.
#' @importFrom stats na.omit
#' @examples
#' num_summary(c(1:10, NA))
#' num_summary(c('1','2','3'))
#' @export
num_summary <- function(x, digits=1) {
  # initial check of variable type
  test_x = tryCatch(as.numeric(x), warning = function(c) {
    msg <- conditionMessage(c)
    # if (!silent) message(c)
    invisible(structure(msg, class = "try-warning"))
  })
  if(class(test_x) == 'try-warning') stop('Need something numeric or which can reasonably be converted to such. \nThe current variable would introduce NAs by coercion.')

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
    select(N, Mean, SD, everything())
  colnames(d) = gsub(colnames(d), pattern='\\.', replacement='')

  d %>% mutate_if(is.numeric, round, digits=digits)
}
