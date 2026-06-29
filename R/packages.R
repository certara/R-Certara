#' List all packages developed by Certara
#'
#' Returns the membership of the Certara pharmacometrics R ecosystem. This is
#' the attach manifest consulted by [certara_attach()]; it is distinct from the
#' set of MCP providers, which is discovered at runtime from installed packages
#' that ship an `inst/mcp/` slice.
#'
#' @param include_self Logical; Set to \code{TRUE} to include \code{Certara.R}
#' @param include_github Logical; Set to \code{TRUE} to include packages only available on GitHub
#' @param include_JFROG_deps Logical; Set to \code{TRUE} to include Certara package dependencies on Jfrog
#' @return Character vector of package names
#' @examples
#' certara_packages()
#' @export
certara_packages <- function(include_self = FALSE, include_github = TRUE, include_JFROG_deps = FALSE) {

  # Define character vector of pkgs installed via install.packages()
  pkgs <- c("Certara.RsNLME",
            "Certara.Xpose.NLME",
            "Certara.RsNLME.ModelExecutor",
            "Certara.ModelResults",
            "Certara.VPCResults",
            "Certara.RsNLME.ModelBuilder",
            "Certara.RDarwin",
            "Certara.DarwinReporter",
            "tidyvpc",
            "ggquickeda",
            "coveffectsplot")

  if (include_self) {
    pkgs <- c("Certara.R", pkgs)
  }

  if (include_github) {
    pkgs <- c(pkgs, names(certara_github_packages()))
  }

  if (include_JFROG_deps) {
    pkgs <- c(pkgs, certara_jfrog_deps())
  }

  return(pkgs)
}

certara_jfrog_deps <- function() {
  c("Certara.NLME8")
}

certara_github_packages <- function() {
  list(table1c = "certara/table1c",
       ggcertara = "certara/ggcertara")
}
