#!/usr/bin/env Rscript
# Trusted-KB CI gate for the Certara.R host KB slice. Proves the shipped KB is
# traceable, internally consistent, and not stale. Fails (non-zero exit) on any
# violation. Run in CI after generate_kb.R.
#
# The host KB documents host-owned MCP tools (session status, config). It has no
# PML grammar, guidance chapters, or engine-audited claims, so this gate is the
# generic subset of the provider gate:
#   1. Provenance  - every factual entry has provenance$source_file.
#   2. Cross-refs  - every SAME-package related/parent/step id resolves;
#                    cross-package refs are deferred to the host merge.
#   3. Freshness   - regenerated JSONL hash matches the committed one.
#
# Usage: Rscript data-raw/mcp-kb/coverage_check.R [pkg_root]

args <- commandArgs(trailingOnly = TRUE)
pkg_root <- if (length(args) >= 1) args[[1]] else normalizePath(".")

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_root, quiet = TRUE)
} else if (!requireNamespace("Certara.R", quietly = TRUE)) {
  stop("Install Certara.R (or pkgload) for the host KB coverage gate.",
       call. = FALSE)
}

fail <- character(0)
note <- function(...) cat(sprintf(...), "\n")

kb_dir <- file.path(pkg_root, "inst", "mcp", "kb")
jsonl <- file.path(kb_dir, "Certara.R.jsonl")
entries <- lapply(readLines(jsonl, warn = FALSE), function(l) {
  jsonlite::fromJSON(l, simplifyVector = FALSE)
})
ids <- vapply(entries, function(e) e$id, character(1))

# 1 + 2: provenance + cross-references (re-runs schema validation per entry).
for (e in entries) {
  p <- Certara.R:::.validate_kb_entry(e, known_ids = ids)
  if (length(p)) fail <- c(fail, sprintf("[%s] %s", e$id, p))
}
note("1-2. provenance + cross-refs: %d entries checked", length(entries))

# 3. Freshness: regenerate into a temp copy and compare committed hashes.
tmp <- file.path(tempdir(), "hostkbfresh")
unlink(tmp, recursive = TRUE)
dir.create(file.path(tmp, "inst", "mcp", "kb"), recursive = TRUE)
file.copy(file.path(kb_dir, "sources"), file.path(tmp, "inst", "mcp", "kb"),
          recursive = TRUE)
suppressMessages(Certara.R::generate_certara_kb(
  pkg_root = tmp, package = "Certara.R",
  package_version = as.character(
    read.dcf(file.path(pkg_root, "DESCRIPTION"), "Version")),
  quiet = TRUE
))
committed <- Certara.R:::.kb_file_sha(jsonl)
regenerated <- Certara.R:::.kb_file_sha(
  file.path(tmp, "inst", "mcp", "kb", "Certara.R.jsonl"))
if (!identical(committed, regenerated)) {
  fail <- c(fail, "Committed JSONL is stale; re-run generate_kb.R and commit.")
}
note("3. freshness: committed %s regenerated %s",
     substr(committed, 1, 12), substr(regenerated, 1, 12))

if (length(fail)) {
  cat("\nTRUSTED-KB GATE FAILED:\n")
  cat(paste0(" - ", fail, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat("\nTRUSTED-KB GATE PASSED\n")
