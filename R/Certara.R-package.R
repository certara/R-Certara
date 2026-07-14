#' Certara.R: Certara Pharmacometrics R Ecosystem and MCP Server
#'
#' Meta-package for the Certara pharmacometrics ecosystem. Attaching
#' `Certara.R` loads the suite of Certara modeling, simulation, and reporting
#' packages (see [certara_attach()]), and the package hosts the federated
#' Certara Model Context Protocol (MCP) server (see [launch_certara_mcp()]).
#'
#' `Certara.RsNLME` is declared in `Depends` so `library(Certara.R)` attaches
#' it for interactive use, matching the package's stated purpose; this package
#' itself calls provider packages generically by name (installed-package
#' discovery, `requireNamespace()`) rather than any specific `Certara.RsNLME`
#' function, so the `@import` below exists only to record that intentional
#' `Depends` relationship for `R CMD check`.
#'
#' @keywords internal
#' @import Certara.RsNLME
"_PACKAGE"
