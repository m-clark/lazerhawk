#' Generate prediction data
#'
#' @description Straightforward way to quickly create data to make model
#'   predictions.
#'
#' @param model_data The original data. Ideally this would come from a model
#'   object.
#' @param conditional_data A data.frame constructed from something like
#'   \code{base::expand.grid}
#' @param num A function like mean or median to be applied to numeric data.
#'   Should return a single value. Default is mean.
#' @param cat Set categorical variables to the reference level ('ref') or the
#'   most frequently occuring category (most_common, the default).
#' @param ... Additional arguments to num, e.g. \code{na.rm=T}
#' @details Given data that was used in a model, create data that can be used
#'   for predictions at key values, especially as a prelude to visualization.
#'   Some package functions can be found that do this, but are specific to
#'   certain models or don't quite provide the flexibility I want. Specifically,
#'   this allows for an arbitrary function to apply to numeric variables, and
#'   for categorical(ish) variables, one has the option for the most common
#'   category (ties go to the first category), or the reference level if a
#'   factor. For now class Date is treated as categorical.
#'
#'   In addition, one may supply their own data to set certain variables at any
#'   particular values via the \code{conditional_data} argument, for example,
#'   using \code{expand.grid} or \code{tidyr::crossing}. Variables not supplied
#'   as columns in the \code{conditional_data} are treated as above.
#'
#' @return A data frame suitable for the newdata argument for predict functions.
#'
#' @examples
#' library(lazerhawk)
#' create_prediction_data(iris)
#' create_prediction_data(iris, num = median,
#'                        expand.grid(
#'                          Sepal.Width=c(0,3,5),
#'                          Species = c('setosa', 'virginica')
#'                          )
#'                        )
#' test_mod = lm(mpg  ~ wt + cyl, mtcars)
#' nd = create_prediction_data(test_mod$model)
#' predict(test_mod, newdata = nd)
#' @export
create_prediction_data <- function(model_data, conditional_data = NULL, num=mean, cat='most_common', ...) {
  .Deprecated('tidyext::create_prediction_data')

  if (cat == 'most_common') {
    catfun = function(x) {
      cx = class(x)
      x = suppressWarnings(names(table(x))[which.max(table(x))])

      # return to original class; leave factors as char
      if (cx == 'Date') {
          x = as.Date(x)    # dates require this approach
      } else if (cx != 'factor') {
        class(x) = cx
      }
      x
    }
  } else {
    # use reference level
    catfun =  function(x) {
      cx = class(x)
      x = levels(factor(x))[1]

      # return to original class; leave factors as char
      if (!cx %in% c('factor', 'character')) {
        class(x) = cx
      }
      x
    }
  }

  pred_data = model_data %>%
    select_if(! colnames(.) %in% names(conditional_data)) %>%
    mutate_if(function(x) is.numeric(x), num, ...) %>%
    mutate_if(function(x) inherits(x, c('factor', 'string', 'logical', 'Date')),
              catfun) %>%
    slice(1)

  if (!is.null(conditional_data)) {
    data.frame(conditional_data, pred_data)
  } else {
    data.frame(pred_data)   # tibbles don't always work with some predict methods
  }
}



