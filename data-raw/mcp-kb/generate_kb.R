#!/usr/bin/env Rscript
# Regenerate the committed Certara.R host knowledge base from curated markdown
# sources. Run by maintainers after editing inst/mcp/kb/sources/*.md. CI re-runs
# this and fails if the committed output differs (see coverage_check.R).
# Deterministic: no AI, no network.
#
# The host owns generate_certara_kb(), so this script loads Certara.R itself
# (preferring the in-tree source via pkgload so unreleased generator/validator
# changes are exercised before install).
#
# Usage:
#   Rscript data-raw/mcp-kb/generate_kb.R [pkg_root]

args <- commandArgs(trailingOnly = TRUE)
pkg_root <- if (length(args) >= 1) args[[1]] else normalizePath(".")

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_root, quiet = TRUE)
} else if (!requireNamespace("Certara.R", quietly = TRUE)) {
  stop("Install Certara.R (or pkgload) to regenerate the host KB.", call. = FALSE)
}

pkg_version <- tryCatch(
  as.character(read.dcf(file.path(pkg_root, "DESCRIPTION"), "Version")),
  error = function(e) NA_character_
)

Certara.R::generate_certara_kb(
  pkg_root = pkg_root,
  package = "Certara.R",
  package_version = pkg_version,
  generator_version = "1.0.0"
)
