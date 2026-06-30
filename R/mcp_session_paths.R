# Session-root path contract: one project_dir per MCP server session so repro
# scripts/, figures/, reports/, and models/ co-locate. Providers and the
# report/repro recorders derive paths from here instead of scattering across
# tempdir().

.mcp_session_paths_state <- new.env(parent = emptyenv())

.mcp_session_paths_ensure <- function() {
  if (is.null(.mcp_session_paths_state$project_dir)) {
    .mcp_session_paths_state$project_dir <- NULL
  }
  invisible(NULL)
}

#' Session project root for MCP deliverables
#'
#' Get or set the session project root. When set, reproducible scripts,
#' figures, report Rmd files, and saved models are written under
#' `<dir>/scripts/`, `<dir>/figures/`, `<dir>/reports/`, and `<dir>/models/`
#' respectively.
#'
#' @param dir Optional new project root (single non-empty path). Setting it
#'   re-points the repro script and report Rmd and creates the subdirectories.
#' @return The active project root, or `NULL` when unset.
#' @examples
#' mcp_session_project_dir(tempdir())
#' mcp_session_figures_dir()
#' @keywords internal
#' @name mcp_session_paths
NULL

#' @rdname mcp_session_paths
#' @export
mcp_session_project_dir <- function(dir = NULL) {
  .mcp_session_paths_ensure()
  if (!is.null(dir)) {
    if (!is.character(dir) || length(dir) != 1L || !nzchar(dir)) {
      stop("`dir` must be a single non-empty string.", call. = FALSE)
    }
    dir <- gsub("\\\\", "/", dir)
    .mcp_session_paths_state$project_dir <- dir
    for (sub in c("scripts", "figures", "reports", "models")) {
      dir.create(file.path(dir, sub), showWarnings = FALSE, recursive = TRUE)
    }
    # Re-point repro + report under the new root.
    if (exists("mcp_repro_path", mode = "function")) {
      mcp_repro_path(file.path(mcp_session_scripts_dir(), "certara_mcp_repro.R"))
    }
    if (exists("mcp_report_path", mode = "function")) {
      mcp_report_path(file.path(mcp_session_reports_dir(), "modeling_report.Rmd"))
    }
  }
  .mcp_session_paths_state$project_dir
}

#' @rdname mcp_session_paths
#' @export
mcp_session_scripts_dir <- function() {
  root <- mcp_session_project_dir()
  if (is.null(root)) return(NULL)
  file.path(root, "scripts")
}

#' @rdname mcp_session_paths
#' @export
mcp_session_figures_dir <- function() {
  root <- mcp_session_project_dir()
  if (is.null(root)) return(NULL)
  file.path(root, "figures")
}

#' @rdname mcp_session_paths
#' @export
mcp_session_reports_dir <- function() {
  root <- mcp_session_project_dir()
  if (is.null(root)) return(NULL)
  file.path(root, "reports")
}

#' @rdname mcp_session_paths
#' @export
mcp_session_models_dir <- function() {
  root <- mcp_session_project_dir()
  if (is.null(root)) return(NULL)
  file.path(root, "models")
}

# Reset session paths (tests and clean server start).
#' @rdname mcp_session_paths
#' @export
mcp_session_paths_reset <- function() {
  .mcp_session_paths_state$project_dir <- NULL
  invisible(NULL)
}
