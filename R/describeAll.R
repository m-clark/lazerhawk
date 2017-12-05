#' Describe your data cleanly and effectively
#'
#' @description Describe data sets with multiple variable types effectively.
#'
#' @param data The dataset, of class data.frame.
#' @param digits See \code{\link[base]{round}}.
#'
#' @import dplyr
#' @importFrom assertthat assert_that
#' @importFrom stats median sd
#'
#' @details This function comes out of my frustrations from various data set
#'   summaries either being inadequate for my needs, too 'busy' with output or
#'   unable to deal well with mixed data types. Numeric data is treated entirely
#'   separately, and provides the same information as in num_by.  Categorical
#'   variables are summarized with frequencies and percentages.
#'
#' @seealso \code{\link[base]{summary}} \code{\link[lazerhawk]{num_by}}
#'
#' @examples
#' library(lazerhawk); library(dplyr)
#' X = data.frame(f1 =gl(2, 1, 20, labels=c('A', 'B')), f2=gl(2, 2, 20, labels=c('X', 'Q')))
#' X = X %>% mutate(bin1 = rbinom(20, 1, p=.5),
#'                  logic1 = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'                  num1 = rnorm(20),
#'                  num2 = rpois(20, 5),
#'                  char1 = sample(letters, 20, replace = TRUE))
#' describeAll(X)
#'
#' describeAll(data.frame(x=factor(1:7)), digits=5)
#' @export
describeAll <- function(data, digits=2) {
  assertthat::assert_that(any(class(data) %in% c('data.frame', 'data.table')))

  nc_num = data %>%
    select_if(is.numeric) %>%
    ncol()
  nc_cat = data %>%
    select_if(function(x) !is.numeric(x)) %>%
    ncol()

  if (nc_num > 0) {
    data_num = data %>%
      select_if(is.numeric) %>%
      summarise_all(funs(
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
    if (nc_num > 1) {
      # suppress warnings if mixed logical and numerics
      suppressWarnings ({
        data_num = data_num %>%
          tidyr::gather(key=results, value=value) %>%
          tidyr::separate(col=results, sep='_(?=[^_]+$)', into=c('Variable', 'result')) %>%
          tidyr::spread(result, value) %>%
          select(Variable, N,  Mean, SD, Min, Q1, Median, Q3, Max, Missing)
      })

    }
    if (digits) data_num = data_num %>% mutate_if(is.numeric, round, digits=digits)
  } else {
    data_num = NULL
  }

  # todo: add missing data option
  if (nc_cat > 0) {
    data_cat = data %>%
      select_if(function(x) !is.numeric(x))

    cat_names = names(data_cat)
    nlevs = data_cat %>% purrr::map_int(n_distinct)

    data_cat = data_cat %>%
      purrr::map(function(x) data.frame(x=table(x), y=prop.table(table(x))) %>%
            select(-y.x) %>%
            rename(Group=x.x,
                   Frequency = x.Freq,
                   perc=y.Freq) %>%
            mutate(perc = 100*round(perc, digits))
      )

    data_cat = data.frame(Variable=rep(cat_names, nlevs),
                          suppressWarnings(bind_rows(data_cat))) %>% # suppress coerce to char message
      rename(`%` = perc)    # otherwise lose symbol on bind rows
  } else {
    data_cat = NULL
  }

  list(`Numeric Variables` = data_num, `Categorical Variables` = data_cat)
}
