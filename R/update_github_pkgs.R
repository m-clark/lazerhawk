#' Update GitHub packages.
#'
#' @description Updates packages installed from GitHub.
#' @details Updates packages installed from GitHub. Rather, it re-installs
#'   GitHub-only packages.  This is not quite as smooth a process as with CRAN,
#'   but you'll get a note of \code{FALSE} for the package if it failed.
#'
#' Taken entirely from user
#'   vh-d's response on
#'   \href{https://stackoverflow.com/questions/32538052/update-all-packages-from-github}{StackOverflow}
#'   with only a very slight modification.
#'
#'
#' @examples
#' \dontrun{
#' update_github_pkgs()
#' }
#' @importFrom utils installed.packages packageDescription
#' @importFrom devtools install_github
#' @export
update_github_pkgs <- function() {

  # check/load necessary packages
  # devtools package
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools package is needed for this function to work. Please install it.")
  }

  pkgs <- utils::installed.packages(fields = "RemoteType")
  github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]

  lapply(github_pkgs, function(pac) {
    message("Updating ", pac, " from GitHub...")

    repo = utils::packageDescription(pac, fields = "GithubRepo")
    username = utils::packageDescription(pac, fields = "GithubUsername")

    devtools::install_github(repo = paste0(username, "/", repo))
  })
}
