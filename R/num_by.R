#' Summarize data
#'
#' @description Provide common numeric summary information.
#'
#' @param df data frame
#' @param main_var the main numeric variable to be summarised
#' @param group_var the optional grouping variable
#'
#' @details This function takes a numeric variable from a dataframe and provides
#'   sample size, mean, standard deviation, min, first quartile, median, third
#'   quartile, and max, possibly over a grouping variable. It works in the dplyr
#'   style using unquoted (bare) variable names.
#' @return data.frame/tibble with the corresponding summary statistics
#' @seealso describeAll
#' @importFrom stats quantile
#' @examples
#' library(dplyr)
#' df1 <- tibble(
#' g1 = sample(1:2, 50, replace = TRUE),
#' g2 = sample(1:4, 50, replace = TRUE),
#' a = rnorm(50),
#' b = rpois(50, 10)
#' )
#'
#'
#' num_by(df1, main_var = a)
#' num_by(df1, main_var = a, group_var = g2)
#'
#' num_by(df1, main_var = b)
#' num_by(df1, main_var = b, group_var = g1)
#'
#'
#' @export
num_by <- function(df, main_var, group_var) {
  if (nrow(df)==0) stop('No data to summarise.')

  mv <- enquo(main_var)
  if (!class(df %>% pull(!!mv)) %in% c('numeric', 'integer', 'logical')) stop('Need numeric variable for main_var.')

  if (!rlang::quo_is_missing(enquo(group_var))) {
    gv <- enquo(group_var)
    df %>%
      group_by(!!gv) %>%
      summarise(
        N = n()-sum(is.na(!!mv)),               # this was faster on 1mil observations than several alternatives
        Mean = mean(!!mv, na.rm = TRUE),
        SD = sd(!!mv, na.rm = TRUE),
        Min = min(!!mv, na.rm = TRUE),
        Q1 = quantile(!!mv, p=.25, na.rm = TRUE),
        Median = median(!!mv, na.rm = TRUE),
        Q3 = quantile(!!mv, p=.75, na.rm = TRUE),
        Max = max(!!mv, na.rm = TRUE)
      ) %>%
      mutate(Variable = c(quo_name(mv), rep('', nrow(.)-1))) %>%
      select(Variable, everything())
  } else {
    df %>%
      summarise(
        N = n()-sum(is.na(!!mv)),
        Mean = mean(!!mv, na.rm = TRUE),
        SD = sd(!!mv, na.rm = TRUE),
        Min = min(!!mv, na.rm = TRUE),
        Q1 = quantile(!!mv, p=.25, na.rm = TRUE),
        Median = median(!!mv, na.rm = TRUE),
        Q3 = quantile(!!mv, p=.75, na.rm = TRUE),
        Max = max(!!mv, na.rm = TRUE)
      ) %>%
      mutate(Variable = quo_name(mv)) %>%
      select(Variable, everything())
  }
}


