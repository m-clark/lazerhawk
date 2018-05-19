#' Summarize brms-fit objects
#'
#' @description Returns a summary table of model output from the brms package.
#'   Requires the brms package.
#'
#'
#' @param model A brmsfit object
#' @param formatOptions A named list(!) of options to pass to the format function
#' @param round See \code{\link[base]{round}}
#' @param astrology Put asterisks next to effects whose credible interval does not
#'   contain 0
#' @param hype Run one-sided hypothesis tests of whether effect is greater
#'   (less) than 0 if positive (negative)
#' @param panderize Create a markdown summary table. Requires the pander package
#' @param justify Additional argument to pass to \code{\link[pander]{pander}}
#' @param ... Additional arguments to pass to \code{\link[brms]{summary.brmsfit}}
#' @param seed Seed for hypothesis tests
#'
#' @details This function creates a data.frame summary object for a brms package
#'   model object.  As of now it only does so for the fixed effects part of the
#'   model.  It will star 'significant' effects, add results from one-sided
#'   hypothesis tests, and allow additional formating options.
#'
#'   For \code{hype = TRUE}, the returned object contains a column indicating
#'   the posterior probability under the hypothesis against its alternative as
#'   noted in the hypothesis function of the brms package. Both the evidence
#'   ratio and probability are produced. Note that for effects near zero, the
#'   probability can be < .5 due to the posterior samples not being perfectly
#'   symmetric.
#'
#' @seealso \code{\link[brms]{summary.brmsfit}} \code{\link[brms]{hypothesis}}
#'   \code{\link[rstan]{print.stanfit}} \code{\link[base]{format}}
#'   \code{\link[base]{round}}
#'
#' @return A data.frame, or Pandoc markdown table if panderize=T.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Probit regression using the binomial family
#' library(brms)
#' n <- sample(1:10, 100, TRUE)  # number of trials
#' success <- rbinom(100, size = n, prob = 0.4)
#' x <- rnorm(100)
#' d <- data.frame(n, success, x)
#' fit4 <- brm(success | trials(n) ~ x, data=d, family = binomial("probit"))
#' summary(fit4)
#'
#' library(lazerhawk)
#' brms_SummaryTable(fit4)
#' brms_SummaryTable(fit4, formatOptions=list(digits=5, nmsall=5))
#' brms_SummaryTable(fit4, astrology=TRUE, hype=TRUE)
#' brms_SummaryTable(fit4, astrology=TRUE, hype=TRUE, panderize=TRUE, justify='lrrrrclr')
#' }
#'
#' @export
brms_SummaryTable <- function(model,
                              formatOptions=list(digits=2, nsmall=2),
                              round=2,
                              astrology=FALSE,
                              hype=FALSE,
                              panderize=FALSE,
                              justify=NULL,
                              seed = 1234,
                              ...) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("brms package, along with related dependencies, is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!inherits(model, "brmsfit")) stop('Model is not a brmsfit class object.')

  fe = brms::fixef(model)
  partables_formatted = data.frame(Covariate=rownames(fe),
                                   do.call(format, list(x=round(fe, round), unlist(formatOptions))))
  rownames(partables_formatted) = NULL
  colnames(partables_formatted)[4:5] = c('l-95% CI', 'u-95% CI')

  if(!astrology & !hype & !panderize) {
    return(partables_formatted)
  }

  # conduct hypothesis tests
  if (hype) {
    testnams = mapply(function(coefname, sign)
      ifelse(sign>0, paste0(coefname, ' > 0'), paste0(coefname, ' < 0')), rownames(fe), sign(fe[,'Estimate']))
    hypetests = brms::hypothesis(x=model, testnams, seed=seed)
    ER = hypetests$hypothesis$Evid.Ratio
    partables_formatted$pvals = ER/(ER+1)
    partables_formatted$pvals[is.infinite(ER)] = 1
    partables_formatted$pvals = do.call(format, list(x=round(partables_formatted$pvals, round), formatOptions[[1]]))
    colnames(partables_formatted)[ncol(partables_formatted)] = 'B < > 0'
    partables_formatted[,'Evidence Ratio'] = do.call(format, list(x=round(ER, round), formatOptions[[1]]))
  }

  # if hype, make star based on brms result, else interval
  if (astrology && hype)  {
    partables_formatted$Notable = hypetests$hypothesis$Star
  } else if (astrology) {
    partables_formatted$Notable =  apply(sign(fe[,c('Q2.5', 'Q97.5')]),
                                         1,
                                         function(interval) ifelse(diff(interval)==0, '*', ''))
  }

  if (panderize) {
    if (is.null(justify)) {
      if (astrology) {
        justify = paste0('l', paste0(rep('r', ncol(partables_formatted)-2), collapse = ''), 'c')
      } else {
        justify = paste0('l', paste0(rep('r', ncol(partables_formatted)-1), collapse = ''))
      }
    }
    return(pander::pander(x=partables_formatted, justify=justify, split.tables=Inf))
  }

  partables_formatted
}


