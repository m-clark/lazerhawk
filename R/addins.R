#' Return an img tag for a centered image
#'
#' @description Inserts html for a centered image
#'
#'
#'
#' @details This function is an Rstudio addin that inserts <img src="" style="display:block; margin: 0 auto;"> so that one can have a centered image on the fly without an R chunk. You must supply the image location for src.
#'
#'
#' @return <img src="" style="display:block; margin: 0 auto;"> as text.
#'
#' @examples
#' \dontrun{
#' library(lazerhawk)
#' insertImgCenterAddin()
#' }
#'
#' @export
insertImgCenterAddin <- function() {
  rstudioapi::insertText("<img src=\"\" style=\"display:block; margin: 0 auto;\">")
}
