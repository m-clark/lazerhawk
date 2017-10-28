#' Summarize data
#'
#' @description Provide common numeric summary information.
#'
#' @param df data frame
#' @param main_var the main numeric variable to be summarised
#' @param group_var the optional grouping variable
#' @param perc_by_group when supplied a grouping variable for cat_by, do you
#'   want within group percentages (default) or out of total?
#' @param digits optional rounding
#'
#' @details The \code{num_by} function takes a numeric variable from a dataframe
#'   and provides sample size, mean, standard deviation, min, first quartile,
#'   median, third quartile, and max, possibly over a grouping variable. It
#'   works in the dplyr style using unquoted (bare) variable names.
#'
#'   For \code{cat_by}, frequencies and percentage (within the grouping variable
#'   or out of total) are returned. Warnings are given if there are more than 10
#'   levels or the data appears to be numeric.
#'
#' @return data.frame/tibble with the corresponding summary statistics
#' @seealso describeAll
#' @importFrom stats quantile
#' @examples
#' df1 <- data.frame(g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
#'                   g2 = sample(1:4, 50, replace = TRUE),
#'                   a = rnorm(50),
#'                   b = rpois(50, 10),
#'                   c = sample(letters, 50, replace=TRUE),
#'                   d = sample(c(TRUE, FALSE), 50, replace=TRUE)
#'                  )
#'
#'
#' num_by(df1, main_var = a)
#' num_by(df1, main_var = a, group_var = g2)
#'
#' num_by(df1, main_var = b)
#' num_by(df1, main_var = b, group_var = g1, digits=2)
#'
#' cat_by(df1, main_var = g1, group_var = g2, digits=1)
#' cat_by(df1, main_var = g1, group_var = g2, perc_by_group=FALSE)
#'
#' @export
num_by <- function(df, main_var, group_var, digits=FALSE) {
  if (nrow(df)==0) stop('No data to summarise.')

  mv <- enquo(main_var)
  if (!class(df %>% pull(!!mv)) %in% c('numeric', 'integer', 'logical')) stop('Need numeric variable for main_var.')

  if (!rlang::quo_is_missing(enquo(group_var))) {
    gv <- enquo(group_var)
    df= df %>%
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
    df= df %>%
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
  if (digits) df = df %>% mutate_if(is.numeric, round, digits=digits)
  df
}



#' @rdname num_by
#' @export
#'
cat_by <- function(df, main_var, group_var, digits=FALSE, perc_by_group=TRUE) {
  if (nrow(df)==0) stop('No data to summarise.')

  mv <- enquo(main_var)
  nlevs = df %>% pull(!!mv) %>% unique() %>% length()  # note that n_distinct doesn't play with !! or passing main_var

  if (!class(df %>% pull(!!mv)) %in% c('character', 'factor', 'logical') && nlevs > 10) warning("Are you sure you don't want num_by?")

  if (nlevs > 10) warning("Greater than 10 levels. This probably won't display well as a table.")

  if (!rlang::quo_is_missing(enquo(group_var))) {
    gv <- enquo(group_var)
    df = df %>%
      group_by(!!mv, !!gv) %>%
      summarise(N = n()-sum(is.na(!!mv))) %>%
      mutate(`%` = 100*N/sum(N))    # possible bug if done within summarise
    if (!perc_by_group) {
      df = df %>%
        ungroup() %>%
        mutate(`%` = 100*N/sum(N))
    }
  } else {
    df = df %>%
      group_by(!!mv) %>%
      summarise(N = n()-sum(is.na(!!mv))) %>%
      mutate(`%` = 100*N/sum(N))
  }
  if (digits) df = df %>% mutate_if(is.numeric, round, digits=digits)
  df
}
