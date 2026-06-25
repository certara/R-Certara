# Provider KB tooling. RsNLME does NOT bundle surrogate KBs for other packages;
# instead each provider ships its own inst/mcp/kb/ generated with the same
# deterministic generator. scaffold_provider_kb() creates the source skeleton a
# provider drops into its repo; generate_certara_kb() then produces the JSONL.

# The Certara provider packages that should each ship an inst/mcp/kb/ slice.
.certara_provider_targets <- function() {
  c("Certara.NLME8", "Certara.RDarwin", "Certara.Xpose.NLME",
    "Certara.ModelResults", "ggcertara", "tidyvpc")
}

#' Scaffold a provider package's KB sources
#'
#' Creates `inst/mcp/kb/sources/` with a format README and a starter source file
#' in a provider package source tree, so the provider can author cited markdown
#' and run [generate_certara_kb()] to ship its own discoverable KB slice.
#'
#' @param pkg_root Path to the provider package source root.
#' @param package Provider package name (stamped on entries).
#' @param overwrite Overwrite an existing starter file.
#' @return Invisibly the sources directory path.
#' @examples
#' \dontrun{
#' scaffold_provider_kb(pkg_root = ".", package = "tidyvpc")
#' }
#' @keywords internal
#' @export
scaffold_provider_kb <- function(pkg_root, package, overwrite = FALSE) {
  src_dir <- file.path(pkg_root, "inst", "mcp", "kb", "sources")
  dir.create(src_dir, showWarnings = FALSE, recursive = TRUE)

  readme_src <- system.file("mcp/kb/sources/README.md",
                            package = "Certara.RsNLME")
  if (nzchar(readme_src) && file.exists(readme_src)) {
    file.copy(readme_src, file.path(src_dir, "README.md"), overwrite = TRUE)
  }

  starter <- file.path(src_dir, paste0(.kb_slug(package), ".md"))
  if (overwrite || !file.exists(starter)) {
    writeLines(.provider_starter_md(package), starter, useBytes = TRUE)
  }

  message(
    "Scaffolded ", package, " KB sources at ", src_dir, ".\n",
    "Edit the source markdown, then run:\n",
    sprintf("  Certara.RsNLME::generate_certara_kb('%s', '%s')",
            pkg_root, package)
  )
  invisible(src_dir)
}

.kb_slug <- function(package) tolower(gsub("[^A-Za-z0-9]+", "-", package))

.provider_starter_md <- function(package) {
  c(
    sprintf("# %s knowledge base", package),
    "",
    "Curated, source-cited entries for this package. Generated JSONL is never",
    "hand-edited; run generate_certara_kb() after editing.",
    "",
    sprintf("## %s.fn.example", package),
    "type: function_doc",
    "title: Example function",
    "summary: One-sentence summary of what the function does.",
    "keywords: example, keyword",
    "symbols: exampleFunction",
    sprintf("provenance.source_file: man/exampleFunction.Rd"),
    "source.kind: Rd",
    "",
    "Describe the function, its key arguments, and a short example.",
    "",
    "```r",
    "exampleFunction(x)",
    "```"
  )
}
