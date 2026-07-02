# Audit-ready reproducible-script recorder: a core MCP capability, not a
# provider feature. MCP tool calls are stateless and opaque to the user, so the
# host maintains a single runnable .R script per server session into which the
# host's own tools AND any provider's tools append the exact R code they ran.
# Re-running that script reproduces the whole MCP-driven analysis by hand, which
# is what makes the session auditable / QC-able.
#
# Providers record into the shared script via the exported SDK below
# (mcp_repro_record / mcp_repro_call / mcp_repro_sym), so the script is one
# coherent transcript across every package the host federates.

.mcp_repro_state <- new.env(parent = emptyenv())

# Reset all recorder state. Exposed for tests and a clean server start.
#' @rdname mcp_repro
#' @export
mcp_repro_reset <- function() {
  .mcp_repro_state$path <- NULL
  .mcp_repro_state$libraries <- character(0)
  .mcp_repro_state$body <- character(0)
  invisible(NULL)
}

.mcp_repro_ensure <- function() {
  if (is.null(.mcp_repro_state$body)) mcp_repro_reset()
  invisible(NULL)
}

#' Reproducible-script recorder
#'
#' A host-owned, provider-agnostic recorder that accumulates the exact runnable
#' R code executed by MCP tools into a single `.R` script per server session.
#' Host tools and provider tools both append to it, so re-running the script
#' reproduces the MCP-driven analysis for QC / audit.
#'
#' @param path New script path (`mcp_repro_path`); setting it resets contents.
#' @param code Character vector of runnable R code (`mcp_repro_record`).
#' @param libraries Package names to attach once at the top of the script.
#' @param fn Function name to render (`mcp_repro_call`).
#' @param args Named list of argument values to render. `NULL` values are
#'   dropped; an unnamed element is rendered positionally.
#' @param var Optional variable name to assign the rendered call to.
#' @param x Value to mark as a verbatim symbol (`mcp_repro_sym`).
#' @return `mcp_repro_path` returns the active path; `mcp_repro_read` the script
#'   text; `mcp_repro_info` a list of `path` + `contents`; `mcp_repro_call` a
#'   code string; others are called for their side effect.
#' @examples
#' mcp_repro_reset()
#' mcp_repro_record(mcp_repro_call("lm", list(formula = mcp_repro_sym("y ~ x")),
#'                                 var = "fit"))
#' cat(mcp_repro_read())
#' @name mcp_repro
NULL

# Resolve the active script path, defaulting lazily to a temp-dir file.
#' @rdname mcp_repro
#' @export
mcp_repro_path <- function(path = NULL) {
  .mcp_repro_ensure()
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
      stop("`path` must be a single non-empty string.", call. = FALSE)
    }
    .mcp_repro_state$path <- path
    .mcp_repro_state$libraries <- character(0)
    .mcp_repro_state$body <- character(0)
  } else if (is.null(.mcp_repro_state$path)) {
    scripts_dir <- mcp_session_scripts_dir()
    if (!is.null(scripts_dir)) {
      .mcp_repro_state$path <- file.path(scripts_dir, "certara_mcp_repro.R")
    } else {
      .mcp_repro_state$path <- file.path(tempdir(), "certara_mcp_repro.R")
    }
  }
  .mcp_repro_state$path
}

.mcp_repro_header <- function() {
  c(
    "# Reproducible R script recorded by Certara MCP tools.",
    "# Re-run top to bottom to reproduce the MCP-driven analysis."
  )
}

# Render the full script: header, attached libraries, then code blocks.
.mcp_repro_render <- function() {
  libs <- .mcp_repro_state$libraries
  lib_lines <- if (length(libs)) c(sprintf("library(%s)", libs), "") else character(0)
  c(.mcp_repro_header(), "", lib_lines, .mcp_repro_state$body)
}

# Append a runnable block and flush the whole script to disk.
#' @rdname mcp_repro
#' @export
mcp_repro_record <- function(code, libraries = character()) {
  .mcp_repro_ensure()
  code <- code[!is.null(code)]
  if (!length(code)) {
    return(invisible(mcp_repro_path()))
  }
  if (length(libraries)) {
    .mcp_repro_state$libraries <- unique(c(.mcp_repro_state$libraries,
                                           libraries))
  }
  .mcp_repro_state$body <- c(.mcp_repro_state$body, code, "")
  path <- mcp_repro_path()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(.mcp_repro_render(), path, useBytes = TRUE)
  invisible(path)
}

#' @rdname mcp_repro
#' @export
mcp_repro_read <- function() {
  .mcp_repro_ensure()
  if (!length(.mcp_repro_state$body)) {
    return("")
  }
  paste(.mcp_repro_render(), collapse = "\n")
}

#' @rdname mcp_repro
#' @export
mcp_repro_info <- function() {
  list(path = mcp_repro_path(), contents = mcp_repro_read())
}

# Mark a value to render verbatim (an R symbol/expression) rather than as a
# quoted literal -- used for object handles and variable names.
#' @rdname mcp_repro
#' @export
mcp_repro_sym <- function(x) structure(as.character(x), class = "mcp_repro_sym")

.mcp_repro_fmt <- function(v) {
  if (inherits(v, "mcp_repro_sym")) {
    return(unclass(v))
  }
  paste(deparse(v, width.cutoff = 500L), collapse = " ")
}

#' @rdname mcp_repro
#' @export
mcp_repro_call <- function(fn, args = list(), var = NULL) {
  if (!is.character(fn) || length(fn) != 1L || !nzchar(fn)) {
    stop("`fn` must be a single non-empty function name.", call. = FALSE)
  }
  args <- Filter(Negate(is.null), args)
  parts <- vapply(seq_along(args), function(i) {
    nm <- names(args)[i]
    val <- .mcp_repro_fmt(args[[i]])
    if (is.null(nm) || !nzchar(nm)) val else paste0(nm, " = ", val)
  }, character(1))
  call <- sprintf("%s(%s)", fn, paste(parts, collapse = ", "))
  if (!is.null(var)) {
    call <- paste0(var, " <- ", call)
  }
  call
}
