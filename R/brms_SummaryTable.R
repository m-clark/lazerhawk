#' Summarize brms-fit objects
#'
#' @description Returns a summary table of model output from the brms package.
#'   Requires the brms package.
#'
#'
#' @param model A brmsfit object
#' @param formatOptions A list(!) of options to pass to the format function
#' @param round See code{\link[base]{round}}
#' @param astrology Put asterisks next to effects whose credible interval does not
#'   contain 0
#' @param hype Run one-sided hypothesis tests of whether effect is greater
#'   (less) than 0 if positive (negative)
#' @param panderize Create a markdown summary table. Requires the pander package
#' @param justify Additional argument to pass to \code{\link[pander]{pander}}
#' @param ... Additional arguments to pass to \code{\link[brms]{summary.brmsfit}}
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
#' brms_SummaryTable(fit4, astrology=TRUE, hype=TRUE)
#' brms_SummaryTable(fit4, astrology=TRUE, hype=TRUE, panderize=TRUE, justify='lrrrrclr')
#' }
#'
#' @export
brms_SummaryTable <- function(model, formatOptions=list(digits=2, nsmall=2), round=2,
                              astrology=F, hype=F, panderize=F, justify=NULL, ...) {

  if(class(model) != "brmsfit") stop('Model is not a brmsfit class object.')

  est = broom::tidy(model)
  fe_names0 = stringr::str_subset(est$term, pattern='^b(|cs|mo|me|m)_')
  fe_names = stringr::str_replace(fe_names0, pattern='b_', '')  # won't generalize to others, but fine for now
  partables = est[est$term %in% fe_names0, c('estimate', 'std.error', 'lower', 'upper')]
  partables_formatted = data.frame(Covariate=fe_names,
                                   do.call(format, list(x=round(partables, round), formatOptions[[1]])))

  if(!astrology & !hype) {
    colnames(partables_formatted)[-1] = c('Estimate', 'Est.Error', 'l-95% CI', 'u-95% CI')
    if (is.null(justify)) justify = paste0('l', paste0(rep('r', ncol(partables_formatted)-1), collapse = ''))
    if (panderize) return(pander::pander(partables_formatted, justify='lrrrr', split.tables=Inf))
    return(partables_formatted)
  }

  # star intervals not containing zero
  if (astrology) {
    sigeffects0 = apply(sign(partables[,c('lower', 'upper')]), 1,
                        function(interval) ifelse(diff(interval)==0, '*', ''))
    sigeffects =  data.frame(partables_formatted, sigeffects0)
    colnames(sigeffects) = c('Covariate', 'Estimate', 'Est.Error', 'l-95% CI', 'u-95% CI', 'Notable')
  } else {
    sigeffects =  partables_formatted
    colnames(sigeffects) = c('Covariate', 'Estimate', 'Est.Error', 'l-95% CI', 'u-95% CI')
  }


  # conduct hypothesis tests
  if (hype) {
    signcoefs = sign(partables[,'estimate'])
    testnams = mapply(function(coefname, sign)
      ifelse(sign>0, paste0(coefname, ' > 0'), paste0(coefname, ' < 0')), fe_names, signcoefs)
    hypetests = sapply(testnams, brms::hypothesis, x=model, simplify = F, alpha=.025, seed=NULL)
    ER = sapply(hypetests, function(test) test[['hypothesis']]['Evid.Ratio'])
    ER = unlist(ER)
    sigeffects$pvals = ER/(ER+1)
    sigeffects$pvals[is.infinite(ER)] = 1
    sigeffects$pvals = do.call(format, list(x=round(sigeffects$pvals, round), formatOptions[[1]]))

    colnames(sigeffects)[ncol(sigeffects)] = 'B < > 0'
    sigeffects[,'Evidence Ratio'] = do.call(format, list(x=round(ER, round), formatOptions[[1]]))
  }

  if (astrology && hype)  sigeffects[, 'Notable'] = ifelse(as.numeric(sigeffects[,'Evidence Ratio']) >= 19, '*', '')

  rownames(sigeffects) = NULL

  if (panderize) {
    if (is.null(justify)) justify = paste0('l', paste0(rep('r', ncol(sigeffects)-1), collapse = ''))
    return(pander::pander(x=sigeffects, justify=justify, split.tables=Inf))
  }

  sigeffects
}


