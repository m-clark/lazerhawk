#' @title describe_all Describe your data cleanly and effectively
#'
#' @description Describe data sets with multiple variable types effectively.
#'   Deprecated and moved to tidyext package.
#'
#' @param data The dataset, of class data.frame.
#' @param digits See \code{\link[base]{round}}. Default is 2, which for
#'   categorical is applied to the proportion (i.e. before converting to
#'   percentage).
#' @param include_NAcat Include NA values as categorical levels? Default is
#'   TRUE.
#' @param NAcat_include Deprecated include_NAcat.
#' @param max_levels The maximum number of levels you want to display for
#'   categorical variables. Default is 10.
#' @param include_numeric For categorical summary, also include numeric
#'   variables with fewer or equal max_levels? Default is FALSE.
#' @param sort_by_freq Sort categorical result by frequency? Default is FALSE.
#' @param ...  Additional arguments passed to \code{num_summary}
#'
#' @import dplyr
#' @importFrom assertthat assert_that
#' @importFrom stats median sd
#' @importFrom purrr map map_int map_df
#'
#' @details This function comes out of my frustrations from various data set
#'   summaries either being inadequate for my needs, too 'busy' with output, or
#'   unable to deal well with mixed data types. Numeric data is treated
#'   separately from categorical, and provides the same information as in
#'   \code{\link[lazerhawk]{num_summary}}. Categorical variables are defined as anything with equal or
#'   fewer distinct values than \code{max_levels} combined with
#'   \code{include_numeric}. Categorical variables are summarized with
#'   frequencies and percentages. For empty categorical variables (e.g. after a
#'   subset), a warning is thrown.
#'
#'   The functions \code{describe_all_num} and  \code{describe_all_cat} will
#'   provide only numeric or only categorical data summaries respectively.
#'   \code{describeAll} is a deprecated alias.
#'
#' @return A list with two elements of summaries for numeric and other variables
#'   respectively.
#'
#' @seealso \code{\link[base]{summary}} \code{\link[lazerhawk]{num_by}}
#'   \code{\link[lazerhawk]{num_summary}}
#'
#' @examples
#' \dontrun{
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
#' }
#' @export
describe_all <- function(data,
                         digits=2,
                         include_NAcat=TRUE,
                         max_levels = 10,
                         include_numeric = FALSE,
                         sort_by_freq = FALSE,
                         NAcat_include = NULL,
                         ...) {
  .Deprecated('tidyext::describe_all')

  assertthat::assert_that(any(class(data) %in% c('data.frame', 'data.table')))

  if(!is.null(NAcat_include)) include_NAcat = NAcat_include

  data_num = describe_all_num(data, digits = digits, ...)

  data_cat = describe_all_cat(data,
                              digits = digits,
                              include_NAcat = include_NAcat,
                              include_numeric = include_numeric,
                              max_levels = max_levels,
                              sort_by_freq = sort_by_freq)

  list(`Numeric Variables` = data_num, `Categorical Variables` = data_cat)
}

#' @rdname describe_all
#' @export
describe_all_num <- function(data, digits=2, ...) {
  .Deprecated('tidyext::describe_all_num')

  assertthat::assert_that(any(class(data) %in% c('data.frame', 'data.table')))

  data_num = data %>%
    select_if(is.numeric)

  nc_num = ncol(data_num)

  if (nc_num > 0) {
    cnames = colnames(data_num)

    data_num = data_num %>%
      purrr::map_df(num_summary, digits=digits, ...) %>%
      mutate(Variable = cnames) %>%
      select(Variable, everything())
  } else {
    data_num = message('No numeric data.')
  }
  data_num
}


#' @rdname describe_all
#' @export
describe_all_cat <- function(data,
                             digits=2,
                             include_NAcat=TRUE,
                             max_levels = 10,
                             include_numeric = FALSE,
                             sort_by_freq = FALSE) {
  .Deprecated('tidyext::describe_all_cat')

  assertthat::assert_that(any(class(data) %in% c('data.frame', 'data.table')))

  data_cat = data %>%
    select_if(function(x) if_else(include_numeric,
                                  n_distinct(x) <= max_levels,
                                  n_distinct(x) <= max_levels & !is.numeric(x)))
  nc_cat = ncol(data_cat)

  if (nc_cat > 0) {
    data_cat = data_cat %>%
      mutate_all(as.character)

    cat_names = names(data_cat)
    nlevs = data_cat %>%
      purrr::map_int(function(x) if_else(all(is.na(x)), 0L,
                                         if_else(include_NAcat, n_distinct(x), n_distinct(na.omit(x)))))

    if (any(nlevs == 0)) warning(paste0(names(nlevs)[nlevs==0], ' have no category levels and will be dropped.\n'))

    if (any(nlevs > 0)){
      data_cat = data_cat %>%
        select_if(nlevs > 0) %>%
        purrr::map(function(x) data.frame(x=table(x, useNA = if_else(include_NAcat, 'ifany', 'no')),
                                          y=prop.table(table(x, useNA = if_else(include_NAcat, 'ifany', 'no')))) %>%
                     select(-y.x) %>%
                     rename(Group=x.x,
                            Frequency = x.Freq,
                            perc=y.Freq) %>%
                     mutate(perc = 100*round(perc, digits))
        )

      data_cat = data.frame(Variable=rep(cat_names, nlevs),
                            suppressWarnings(bind_rows(data_cat)),
                            stringsAsFactors=F) %>% # suppress coerce to char message
        rename(`%` = perc)    # otherwise lose symbol on bind rows
      if (sort_by_freq) {
        data_cat = data_cat %>%
          group_by(Variable) %>%
          arrange(desc(`%`), .by_group = TRUE) %>%
          ungroup()
      }
    } else {
      data_cat = message('No categorical data.')
    }
  } else {
    data_cat = message('No categorical data.')
  }
  data_cat
}



#' @rdname describe_all
#' @export
describeAll <- describe_all

