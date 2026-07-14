#' Core ecosystem packages attached by \code{library(Certara.R)}
#'
#' The attach-time subset of [certara_packages()]: the JFrog/CRAN-installable
#' Certara suite, excluding GitHub-only packages. Packages that are not
#' installed are skipped gracefully by [certara_attach()].
#'
#' @return Character vector of package names.
#' @examples
#' certara_core_packages()
#' @export
certara_core_packages <- function() {
  certara_packages(include_github = FALSE, include_JFROG_deps = FALSE)
}

#' Attach the Certara pharmacometrics ecosystem
#'
#' Attaches the installed member packages returned by
#' [certara_core_packages()] so a single `library(Certara.R)` brings the suite
#' onto the search path. Missing packages are skipped rather than raising an
#' error.
#'
#' @return Invisibly, a named logical vector indicating which packages were
#'   attached.
#' @examples
#' \dontrun{
#' certara_attach()
#' }
#' @export
certara_attach <- function() {
  to_load <- certara_core_packages()

  attached <- vapply(to_load, function(pkg) {
    if (!(pkg %in% loadedNamespaces()) && !requireNamespace(pkg, quietly = TRUE)) {
      return(FALSE)
    }
    suppressPackageStartupMessages(
      suppressWarnings(
        require(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
    )
  }, logical(1))

  invisible(attached)
}

.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("Certara.R.quiet"))) {
    invisible(certara_attach())
    return(invisible())
  }

  attached <- certara_attach()
  ok <- names(attached)[attached]
  missing <- names(attached)[!attached]

  msg <- paste0("Certara.R ", utils::packageVersion("Certara.R"),
                " - pharmacometrics ecosystem")
  if (length(ok)) {
    msg <- paste0(msg, "\nAttached: ", paste(ok, collapse = ", "))
  }
  if (length(missing)) {
    msg <- paste0(msg, "\nNot installed (skipped): ", paste(missing, collapse = ", "))
  }
  msg <- paste0(msg, "\nStart the MCP server with launch_certara_mcp(); ",
                "configure a client with write_mcp_config().")

  packageStartupMessage(msg)
}
