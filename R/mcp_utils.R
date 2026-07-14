# Small shared helpers for the Certara MCP host engine.

# Null-coalescing helper (kept local to avoid depending on rlang at runtime).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Non-empty length-1 character (JSON scalar string after jsonlite::fromJSON).
.is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && nzchar(x)
}

# ISO-8601 local timestamp, used in memory records and other audit fields.
.mcp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# Closed set of btw tool groups write_mcp_config() / launch_certara_mcp() may
# bake into the Rscript -e launch command.
.mcp_btw_groups <- function() {
  c("docs", "pkg", "env", "run", "files", "git", "github", "ide", "cran",
    "web", "sessioninfo")
}

# Reject unknown group names before they are interpolated into the launch expr.
.validate_btw_groups <- function(btw_groups) {
  if (length(btw_groups) == 0) {
    return(invisible(character(0)))
  }
  if (!is.character(btw_groups)) {
    stop("`btw_groups` must be a character vector.", call. = FALSE)
  }
  allowed <- .mcp_btw_groups()
  unknown <- setdiff(btw_groups, allowed)
  if (length(unknown)) {
    stop(
      sprintf(
        "Unknown btw_groups: %s. Allowed: %s.",
        paste(unknown, collapse = ", "),
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(btw_groups)
}

# Absolute path to the Rscript binary of the running R, baked into the launch
# command written by write_mcp_config().
.mcp_rscript_bin <- function() {
  bin <- file.path(R.home("bin"),
                   if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  normalizePath(bin, mustWork = FALSE)
}

# Parse an MCP tool argument that carries a JSON object/array as text (ellmer
# has no generic MCP object/map type here, so structured arguments - e.g. a
# provider's telemetry/session_provenance block passed to record_run() - are
# passed as JSON text). Already-a-list values (e.g. an interactive R caller)
# pass through unchanged; NULL/empty is NULL; unparsable text is kept as a
# best-effort `{"raw": <text>}` object rather than dropped or erroring.
.mcp_parse_json_arg <- function(x) {
  if (is.null(x) || (is.character(x) && !nzchar(trimws(x %||% "")))) {
    return(NULL)
  }
  if (!is.character(x)) {
    return(x)
  }
  tryCatch(
    jsonlite::fromJSON(x, simplifyVector = FALSE),
    error = function(e) list(raw = x)
  )
}
