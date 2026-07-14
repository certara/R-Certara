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

# Restrict server_name to characters that stay valid, unescaped, everywhere
# write_mcp_config() interpolates it verbatim: a JSON object key, a bare TOML
# table key (`[mcp_servers.<name>]`, which the Codex writer builds directly -
# see .write_codex_mcp_config()), a Cursor rule/tool identifier
# (`user-<name>`), and a single-quoted R string literal inside
# .mcp_launch_expr(). Rejecting anything else up front is simpler and more
# robust than escaping/unescaping it correctly in every one of those formats.
.validate_server_name <- function(server_name) {
  if (!.is_scalar_string(server_name) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", server_name)) {
    stop(
      "`server_name` must be a single non-empty string of letters, digits, ",
      "'-', or '_', starting with a letter or digit (it is written verbatim ",
      "as a JSON key, a TOML table name, and inside an R string literal).",
      call. = FALSE
    )
  }
  invisible(server_name)
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
# passed as JSON text). Already-a-list value (e.g. an interactive R caller)
# passes through unchanged; NULL/empty scalar string is NULL; unparsable text
# is kept as a best-effort `{"raw": <text>}` object rather than dropped or
# erroring. Any other shape (a bare scalar, a multi-element character vector,
# etc.) is also wrapped as `list(raw = x)` so callers - e.g. record_run()'s
# provenance - always receive either NULL or a list, never a bare vector.
.mcp_parse_json_arg <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L) {
    if (!nzchar(trimws(x))) {
      return(NULL)
    }
    return(tryCatch(
      jsonlite::fromJSON(x, simplifyVector = FALSE),
      error = function(e) list(raw = x)
    ))
  }
  list(raw = x)
}
