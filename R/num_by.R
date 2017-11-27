#' Summarize data
#'
#' @description Provide common numeric summary information.
#'
#' @param data data frame
#' @param main_var the variable to be summarised. Multiple variables are
#'   supplied using the vars() function.
#' @param group_var the (optional) grouping variable.
#' @param perc_by_group when supplied a grouping variable for cat_by, do you
#'   want within group percentages also (default is TRUE)
#' @param sort_by_group when supplied a grouping variable for cat_by, do you
#'   want result sorted on the grouping variable? Default is TRUE.
#' @param digits optional rounding
#'
#' @details The \code{num_by} function takes a numeric variable from a dataframe
#'   and provides sample size, mean, standard deviation, min, first quartile,
#'   median, third quartile, max, and number of missing values, possibly over a
#'   grouping variable.
#'
#'   It works in the dplyr style using unquoted (bare)
#'   variable names, using the vars() function if there is more than one
#'   variable.  If using a grouping variable, it will treat missing values as a
#'   separate group.
#'
#'   For \code{cat_by}, frequencies and percentage (out of total or group_var)
#'   are returned. Warnings are given if any of the main
#'   variables are more than 10 levels, or the data appears to be non-categorical. The
#'   group_var argument is essentially just to provide percentages based on a
#'   focal grouping variable out of all those supplied.
#'
#'   Missing values are treated as separate groups, as it's often useful to
#'   explore the nature of the missingness. To avoid this, just use
#'   \code{na.omit} or \code{dplyr::drop_na} on the data frame first.
#'
#' @return data.frame/tibble with the corresponding summary statistics
#' @seealso describeAll
#' @importFrom stats quantile
#' @importFrom rlang is_quosures
#' @importFrom tidyselect vars_pull
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
#' cat_by(df1, main_var = dplyr::vars(g1,d), group_var = g2, perc_by_group=FALSE)
#'
#' @export
num_by <- function(data, main_var, group_var, digits=FALSE) {
  if (nrow(data)==0 | is.null(data)) stop('No data to summarise.')

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
  if (!'data.frame' %in% class(data)) stop('Need a data.frame type object.')


  # section to deal with single variable name -------------------------------
  mv = enquo(main_var)  # has to be here or errs with 'has already been evaluated'

  check_mv = tryCatch(rlang::is_quosures(main_var), error = function(c) {
    msg = conditionMessage(c)
    invisible(structure(msg, class = "try-error"))
  })

  if (class(check_mv) == 'try-error') main_var = quos(!!mv)

  class_mv = data %>%
    summarise_at(main_var, funs(cls=class(.))) %>%
    unlist()
  if (!all(class_mv %in% c('numeric', 'integer', 'logical'))) stop('Non-numeric/logical variable detected.')


  # Main processing ---------------------------------------------------------

  if (!rlang::quo_is_missing(enquo(group_var))) {

    #• grouped result ----------------------------------------------------------

    gv = enquo(group_var)
    data = data %>%
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
                     Missing = sum_NA(.)
                   )
      )
    if (length(main_var) > 1) {
      # regex will look for the last _ , in case there are variable names with underscores
      data = data %>%
        tidyr::gather(key=results, value=value, -!!gv) %>%
        tidyr::separate(col=results, sep='_(?=[^_]+$)', into=c('Variable', 'result')) %>%
        tidyr::spread(result, value) %>%   # the obsession with auto alphabetical order in the tidyverse strikes once again
        select(!!gv, Variable, N,  Mean, SD, Min, Q1, Median, Q3, Max, Missing)
    }
  } else {

    #• non-grouped result ------------------------------------------------------

    data = data %>%
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
                     Missing = sum_NA(.)
                   )
      )
    if (length(main_var) > 1) {
      # suppress warnings if mixed logical and numerics
      suppressWarnings({
        data = data %>%
          tidyr::gather(key=results, value=value) %>%
          tidyr::separate(col=results, sep='_(?=[^_]+$)', into=c('Variable', 'result')) %>%
          tidyr::spread(result, value) %>%
          select(Variable, N,  Mean, SD, Min, Q1, Median, Q3, Max, Missing)
      })
    }
  }

  if (digits) data = data %>% mutate_if(is.numeric, round, digits=digits)
  data
}



#' @rdname num_by
#' @export
#'
cat_by <- function(data, main_var, group_var, digits=FALSE, perc_by_group=TRUE, sort_by_group=TRUE) {
  if (nrow(data)==0 | is.null(data)) stop('No data to summarise.')

  mv = enquo(main_var)
  check_mv = tryCatch(rlang::is_quosures(main_var), error = function(c) {
    msg <- conditionMessage(c)
    invisible(structure(msg, class = "try-error"))
  })

  if (class(check_mv) == 'try-error' | is.logical(check_mv) && !check_mv) main_var = quos(!!mv)
  class_mv = data %>% summarise_at(main_var, funs(cls=class(.))) %>% unlist()
  nlevs = data %>% summarise_at(main_var, funs(n_distinct(.))) %>% unlist()

  if (!all(class_mv  %in% c('character', 'factor', 'logical'))) warning("Some of these do not appear to be character, factor, or logical variables.")

  if (any(nlevs > 10)) warning("Greater than 10 levels found for at least one variable. This probably won't display well as a table.")

  # use of group var --------------------------------------------------------
  gv_exists = !rlang::quo_is_missing(enquo(group_var))
  if (gv_exists) {
  # check that group_var is in main_var; if not add it
    gv = enquo(group_var)
    check_gv = tryCatch(tidyselect::vars_pull(names(select(data, !!mv)), !!gv), error = function(c) {
      msg <- conditionMessage(c)
      invisible(structure(msg, class = "try-error"))
    })

    if (class(check_gv) == 'try-error' | is.logical(check_gv) && !check_gv) main_var = c(main_var, vars(!!gv))

    data = data %>%
      select_at(main_var) %>%
      group_by_all() %>%
      summarise(N = n()-sum(is.na(!!mv))) %>%
      ungroup() %>%
      mutate(`% of Total` = 100*N/sum(N))

    if (perc_by_group) {
      varname = vars_pull(names(data), !!gv)
      data = data %>%
        group_by(!!gv) %>%
        mutate(gv_perc = 100*N/sum(N))
      colnames(data)[colnames(data) == 'gv_perc'] = paste0('% of ', varname)
    }

    if (sort_by_group){
      data = data %>%
        select(!!gv, everything()) %>%
        arrange(!!gv)
    }
  } else {
    # no group var --------------------------------------------------------
    data = data %>%
      group_by_at(main_var) %>%
      summarise(N = n()-sum(is.na(!!mv))) %>%
      ungroup() %>%
      mutate(`% of Total` = 100*N/sum(N))
  }
  if (digits) data = data %>% mutate_if(is.numeric, round, digits=digits)
  data
}
