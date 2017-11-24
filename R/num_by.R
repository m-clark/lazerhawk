#' Summarize data
#'
#' @description Provide common numeric summary information.
#'
#' @param df data frame
#' @param main_var the variable to be summarised. For num_by, this can be
#'   multiple variables using the vars() function.
#' @param group_var the (optional) grouping variable
#' @param perc_by_group when supplied a grouping variable for cat_by, do you
#'   want within group percentages (default) or out of total?
#' @param sort_by_group when supplied a grouping variable for cat_by, do you
#'   want result sorted on the grouping variable? Default is TRUE.
#' @param digits optional rounding
#'
#' @details The \code{num_by} function takes a numeric variable from a dataframe
#'   and provides sample size, mean, standard deviation, min, first quartile,
#'   median, third quartile, max, and number of missing values, possibly over a
#'   grouping variable. It works in the dplyr style using unquoted (bare)
#'   variable names, using the vars() function if there is more than one
#'   variable.  If using a grouping variable, it will treat missing values as a
#'   separate group.
#'
#'   For \code{cat_by}, frequencies and percentage (within the grouping variable
#'   or out of total) are returned. Warnings are given if there are more than 10
#'   levels or the data appears to be numeric. It will only work on a one or two (if group_var is supplied) categorical variables, as defining what percentages should be calculated is not obvious.
#'
#' @return data.frame/tibble with the corresponding summary statistics
#' @seealso describeAll
#' @importFrom stats quantile
#' @importFrom rlang is_quosures
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
#' num_by(df1, main_var = a, group_var = g2, digits=2)
#'
#' num_by(df1, main_var = dplyr::vars(a,b), group_var = g1, digits=1)
#'
#' cat_by(df1, main_var = g1, group_var = g2, digits=1)
#' cat_by(df1, main_var = g1, group_var = g2, perc_by_group=FALSE)
#'
#' @export
num_by <- function(df, main_var, group_var, digits=FALSE) {
  if (nrow(df)==0 | is.null(df)) stop('No data to summarise.')

  # for future reference; the tryCatch was just to make it easy to pass a single
  # variable name in lieu of using vars().
  # - summarise_* won't take single unquoted names nor is there a straightforward way to pass
  # one or even check if one is being passed.
  # - summarise_* also won't take an unquoted varname to vars, which is just a
  # wrapper to the quos function
  # - variants of vars do not return the same class object as vars()
  #
  # Other stuff:
  # you can't do !!enquo(x); you have to do z = enquo(x); !!z or !(!enquo(x))
  # though the latter likely would only work within a function
  # in retrospect, mutate_at + distinct would have likely worked better, but
  # distinct won't take a vars input, because why would it?
  # perhaps tidyselect will have some useful documentation in the future

  # Initial checks ------------------------------------------------------------
  if (!'data.frame' %in% class(df)) stop('Need a data.frame type object.')


  # section to deal with single variable name -------------------------------
  mv = enquo(main_var)  # has to be here or errs with 'has already been evaluated'

  x = tryCatch(rlang::is_quosures(main_var), error = function(c) {
    msg = conditionMessage(c)
    invisible(structure(msg, class = "try-error"))
  })

  if (class(x) == 'try-error') main_var = quos(!!mv)

  class_mv = df %>%
    summarise_at(main_var, funs(cls=class(.))) %>%
    unlist()
  if (!all(class_mv %in% c('numeric', 'integer', 'logical'))) stop('Non-numeric/logical variable detected.')


  # Main processing ---------------------------------------------------------

  if (!rlang::quo_is_missing(enquo(group_var))) {

    #• grouped result ----------------------------------------------------------

    gv = enquo(group_var)
    df = df %>%
      # filter(!is.na(!!gv)) %>%
      group_by(!!gv) %>%
      summarise_at(main_var,
                   funs(
                     N = n()-sum(is.na(.)),           # this was faster on 1mil observations than several alternatives
                     Mean = mean(., na.rm = TRUE),
                     SD = sd(., na.rm = TRUE),
                     Min = min(., na.rm = TRUE),
                     Q1 = quantile(., p=.25, na.rm = TRUE),
                     Median = median(., na.rm = TRUE),
                     Q3 = quantile(., p=.75, na.rm = TRUE),
                     Max = max(., na.rm = TRUE),
                     Missing = ifelse(any(is.na(.)), sum(is.na(.)), NA)
                   )
      )
    if (length(main_var) > 1) {
      df = df %>%
        tidyr::gather(key=results, value=value, -!!gv) %>%
        tidyr::separate(col=results, sep='_', into=c('Variable', 'result')) %>%
        tidyr::spread(result, value) %>%   # the obsession with auto alphabetical order in the tidyverse strikes once again
        select(!!gv, Variable, N,  Mean, SD, Min, Q1, Median, Q3, Max, Missing)
    }
  } else {

    #• non-grouped result ------------------------------------------------------

    df = df %>%
      summarise_at(main_var,
                   funs(
                     N = n()-sum(is.na(.)),
                     Mean = mean(., na.rm = TRUE),
                     SD = sd(., na.rm = TRUE),
                     Min = min(., na.rm = TRUE),
                     Q1 = quantile(., p=.25, na.rm = TRUE),
                     Median = median(., na.rm = TRUE),
                     Q3 = quantile(., p=.75, na.rm = TRUE),
                     Max = max(., na.rm = TRUE),
                     Missing = ifelse(any(is.na(.)), sum(is.na(.)), NA)
                   )
      )
    if (length(main_var) > 1) {
      # suppress warnings if mixed logical and numerics
      suppressWarnings({
        df = df %>%
          tidyr::gather(key=results, value=value) %>%
          tidyr::separate(col=results, sep='_', into=c('Variable', 'result')) %>%
          tidyr::spread(result, value) %>%
          select(Variable, N,  Mean, SD, Min, Q1, Median, Q3, Max, Missing)
      })
    }
  }

  if (digits) df = df %>% mutate_if(is.numeric, round, digits=digits)
  df
}



#' @rdname num_by
#' @export
#'
cat_by <- function(df, main_var, group_var, digits=FALSE, perc_by_group=TRUE, sort_by_group=TRUE) {
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
    if (sort_by_group){
      df = df %>%
        select(!!gv, everything()) %>%
        arrange(!!gv)
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
