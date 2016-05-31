#' Describe your data cleanly and effectively
#'
#' @description Describe data sets with multiple variable types effectively.
#'
#' @param data The dataset, of class data.frame.
#' @param binlogasFactor Binary and/or Logical variables will be treated as factors
#' @param charIgnore Ignore variables that are character class?
#' @param digits See \code{\link[base]{round}}.
#'
#' @import dplyr
#' @importFrom assertthat assert_that
#' @importFrom stats median sd
#'
#' @details This function comes out of my frustrations from various data set
#'   summaries either being inadequate for my needs, too 'busy' or unable to
#'   deal well with mixed data types. Numeric data is treated entirely
#'   separately, and provides the sample size available for the variable in
#'   question, mean, standard deviation, median, min, and max.  Categorical
#'   variables are summarized via tables.  Options are available for some types
#'   that could go either way (e.g. binary variables).
#'
#' @seealso \code{\link[base]{summary}}
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
#' describeAll(X, binlogasFactor = FALSE, charIgnore = FALSE)
#'
#' # test missing
#' Xmiss = X
#' Xmiss[sample(1:20, 5), sample(1:ncol(X), 3)] = NA
#' describeAll(Xmiss)
#'
#' @export
describeAll <- function(data, binlogasFactor=TRUE, charIgnore=TRUE, digits=3) {
  assertthat::assert_that(any(class(data) %in% c('data.frame', 'data.table')))
  classList = sapply(data, class)
  cnames = colnames(data)

  binidx = sapply(data, function(x) suppressWarnings(all(sort(unique(x)) == c(0,1))))

  nums = cnames[classList %in% c('numeric', 'integer', 'logical')]
  factors = cnames[classList=='factor']
  chars = cnames[classList=='character']
  bins = cnames[binidx]


  # categorical
  if (length(factors) > 0) {
    factorData = data[,factors]

    # deal with logicals, characters
    if (length(chars) > 0 && !charIgnore) {
      charsFactor = sapply(data[, chars, drop=FALSE], as.factor)
      factorData = cbind(factorData, charsFactor)
    }

    if (length(bins) > 0 & binlogasFactor) {
      binsFactor = sapply(data[, bins, drop=FALSE], as.factor)
      factorData = cbind(factorData, binsFactor)
    }
    tabls = lapply(factorData, function(x) cbind(table(x), round(prop.table(table(x))*100, 2)))
    tabls = lapply(tabls, function(x) {colnames(x) <- c('Freq', 'Perc'); x})
  }


  # numeric
  if (length(nums) > 0) {
    numsData = data[,nums]
    numStats = numsData %>%
      summarize_each(funs(sum(!is.na(.)),
                          mean(., na.rm=TRUE),
                          sd(., na.rm=TRUE),
                          median(., na.rm=TRUE),
                          min(., na.rm=TRUE),
                          max(., na.rm=TRUE))) %>%
      unlist
    dim(numStats) = c(ncol(numsData), 6)

    colnames(numStats) = c('N', 'mean', 'sd', 'median', 'min', 'max')
    rownames(numStats) = colnames(numsData)
    numStats = round(numStats, digits)
    }


  if (!exists('tabls')) tabls = 'No categorical data.'
  if (!exists('numStats')) numStats = 'No numeric data.'
  return(list(Numeric = numStats,
              Categorical = tabls)
  )
}
