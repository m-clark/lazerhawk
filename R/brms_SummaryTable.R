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
#' ## Not run:
#'
#' ## Probit regression using the binomial family
#' library(brms)
#' n <- sample(1:10, 100, TRUE)  # number of trials
#' success <- rbinom(100, size = n, prob = 0.4)
#' x <- rnorm(100)
#' fit4 <- brm(success | trials(n) ~ x, family = binomial("probit"))
#' summary(fit4)
#'
#' library(lazerhawk)
#' brms_SummaryTable(fit4)
#' brms_SummaryTable(fit4, astrology=TRUE, hype=TRUE)
#' brms_SummaryTable(fit4, astrology=TRUE, hype=TRUE, panderize=TRUE, justify='lrrrrclr')

#'
#' @export
brms_SummaryTable <- function(model, formatOptions=list(digits=2, nsmall=2), round=2,
                              astrology=F, hype=F, panderize=F, justify=NULL, ...) {
  if(class(model) != "brmsfit") stop('Model is not a brmsfit class object.')

  est = brms:::summary.brmsfit(model, ...)$fixed
  partables = est[,c('Estimate', 'Est.Error', 'l-95% CI', 'u-95% CI')]
  estnames = rownames(partables)
  partables_formated = do.call(format, list(x=round(partables, round), formatOptions[[1]]))

  if(!astrology & !hype) {
    partables_formated = data.frame(partables_formated)
    colnames(partables_formated) = c('Estimate', 'Est.Error', 'l-95% CI', 'u-95% CI')
    if (panderize) return(pander::pander(partables_formated, justify='lrrrr'))
    return(partables_formated)
  }

  # star intervals not containing zero
  if (astrology){
    sigeffects0 = apply(sign(partables[,c('l-95% CI', 'u-95% CI')]), 1, function(interval) ifelse(diff(interval)==0, '*', ''))
    sigeffects =  data.frame(var=estnames, partables_formated, sigeffects0)
    colnames(sigeffects) = c('var', 'coef', 'se', '2.5%', '97.5%', '')
  } else {
    sigeffects =  data.frame(var=estnames, partables_formated)
    colnames(sigeffects) = c('var', 'coef', 'se', '2.5%', '97.5%')
  }


  # conduct hypothesis tests
  if(hype){
    signcoefs = sign(partables[,'Estimate'])
    testnams = mapply(function(coefname, sign) ifelse(sign>0, paste0(coefname, ' > 0'), paste0(coefname, ' < 0')),
                      estnames, signcoefs)
    hypetests = sapply(testnams, brms::hypothesis, x=model, simplify = F, alpha=.025)
    ER = sapply(hypetests, function(test) test[['hypothesis']]['Evid.Ratio'])
    ER = unlist(ER)
    sigeffects$pvals = ER/(ER+1)
    sigeffects$pvals[is.infinite(ER)] = 1
    sigeffects$pvals = do.call(format, list(x=round(sigeffects$pvals, round), formatOptions[[1]]))

    colnames(sigeffects)[ncol(sigeffects)] = 'B < > 0'
    sigeffects[,'Evidence Ratio'] = do.call(format, list(x=round(ER, round), formatOptions[[1]]))
  }

  rownames(sigeffects) = NULL

  if(panderize) return(pander::pander(x=sigeffects, justify=justify))
  sigeffects
}


