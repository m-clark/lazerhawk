#' @name addins
NULL
#' Rstudio addins
#'
#'
#' @description Rstudio addins, mostly for Rmarkdown html/css snippets
#'
#'
#' @details - insertImgCenter This function is an Rstudio addin that inserts <img src="" style="display:block; margin: 0 auto;"> so that one can have a centered image on the fly without an R chunk. You must supply the image location for src.
#'
#' - insertSpan Insert a span for a particular class.
#'
#'
#' @return <img src="" style="display:block; margin: 0 auto;"> as text.
#'
#' <span class=""></span> as text.
#'
#' @examples
#' \dontrun{
#' library(lazerhawk)
#' insertImgCenterAddin()
#' insertSpanAddin()
#' }
#'
#'
#' @rdname addins
#' @export
insertImgCenterAddin <- function() {
  rstudioapi::insertText("<img src=\"\" style=\"display:block; margin: 0 auto;\">")
}

#' @rdname addins
#' @export
insertSpanAddin <- function() {
  rstudioapi::insertText("<span class=\"\" style = \"\"></span>")
}

#' @rdname addins
#' @export
insertLaTeXAddin <- function() {
  rstudioapi::insertText("$\\LaTeX$")
}

#' @rdname addins
#' @export
insertRoxyLinkAddin <- function() {
  rstudioapi::insertText("\\code{\\link[package]{function}}")
}
