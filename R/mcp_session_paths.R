# Session-root path contract: one project_dir per MCP server session so repro
# scripts/, figures/, reports/, and models/ co-locate. Providers and the
# report/repro recorders derive paths from here instead of scattering across
# tempdir(). The pin is durable across server restarts: written to
# <project>/.certara-mcp/session.json and a user-level last-project pointer.

.mcp_session_paths_state <- new.env(parent = emptyenv())

.mcp_session_paths_ensure <- function() {
  if (is.null(.mcp_session_paths_state$project_dir)) {
    .mcp_session_paths_state$project_dir <- NULL
  }
  invisible(NULL)
}

.mcp_session_state_dir <- function(project_dir) {
  file.path(project_dir, ".certara-mcp")
}

.mcp_session_state_path <- function(project_dir) {
  file.path(.mcp_session_state_dir(project_dir), "session.json")
}

.mcp_session_last_project_path <- function() {
  file.path(tools::R_user_dir("Certara.R", "data"), "mcp-session",
            "last_project.json")
}

.mcp_session_write_pin <- function(dir) {
  state_dir <- .mcp_session_state_dir(dir)
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  payload <- list(
    project_dir = dir,
    pinned = .mcp_now(),
    package_version = as.character(utils::packageVersion("Certara.R"))
  )
  jsonlite::write_json(payload, .mcp_session_state_path(dir),
                       auto_unbox = TRUE, pretty = TRUE)
  last_path <- .mcp_session_last_project_path()
  dir.create(dirname(last_path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(list(project_dir = dir, pinned = payload$pinned),
                       last_path,
                       auto_unbox = TRUE, pretty = TRUE)
  invisible(dir)
}

# Restore the last pinned project when the in-memory pin is empty (server
# restart / new process). Returns the restored path or NULL.
.mcp_session_restore_pin <- function() {
  ptr <- .mcp_session_last_project_path()
  if (!file.exists(ptr)) return(NULL)
  info <- tryCatch(jsonlite::read_json(ptr, simplifyVector = TRUE),
                   error = function(e) NULL)
  dir <- info$project_dir %||% NULL
  if (!is.character(dir) || length(dir) != 1L || !nzchar(dir)) return(NULL)
  state <- .mcp_session_state_path(dir)
  if (!file.exists(state) || !dir.exists(dir)) return(NULL)
  dir
}

# True when the active deliverable root is under an R session tempdir
# (Rtmp*), and therefore not durable across restarts / process exits.
.mcp_path_is_ephemeral <- function(path) {
  if (is.null(path) || !is.character(path) || length(path) != 1L || !nzchar(path)) {
    return(TRUE)
  }
  norm <- gsub("\\\\", "/", path)
  grepl("/Rtmp[^/]*/", paste0(norm, "/"), ignore.case = TRUE) ||
    grepl("^/tmp/", norm, ignore.case = TRUE) ||
    identical(normalizePath(dirname(norm), winslash = "/", mustWork = FALSE),
              normalizePath(tempdir(), winslash = "/", mustWork = FALSE)) ||
    startsWith(normalizePath(norm, winslash = "/", mustWork = FALSE),
               normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
}

#' Durability of the current MCP audit trail
#'
#' @return A list with `durable` (logical), `project_dir`, `reason`, and when
#'   not durable a remediation `next_action`.
#' @keywords internal
#' @export
mcp_session_durability <- function() {
  root <- mcp_session_project_dir()
  if (is.null(root)) {
    return(list(
      durable = FALSE,
      project_dir = NULL,
      reason = paste(
        "No session project_dir is pinned; repro/report paths fall back to",
        "tempdir() and will be lost when the process exits."
      ),
      next_action = list(
        tool = "certara_session_project_dir",
        args = list(dir = "<your durable project root>"),
        description = paste(
          "Pin a durable project directory so scripts/, figures/, reports/,",
          "and models/ survive restarts.")
      )
    ))
  }
  if (.mcp_path_is_ephemeral(root)) {
    return(list(
      durable = FALSE,
      project_dir = root,
      reason = paste(
        "Session project_dir is under an R temporary directory (Rtmp*);",
        "deliverables will be reclaimed when the process exits."
      ),
      next_action = list(
        tool = "certara_session_project_dir",
        args = list(dir = "<your durable project root>"),
        description = paste(
          "Re-pin to a durable project directory outside tempdir()/Rtmp*.")
      )
    ))
  }
  list(durable = TRUE, project_dir = root, reason = NULL, next_action = NULL)
}

#' Session project root for MCP deliverables
#'
#' Get or set the session project root. When set, reproducible scripts,
#' figures, report Rmd files, and saved models are written under
#' `<dir>/scripts/`, `<dir>/figures/`, `<dir>/reports/`, and `<dir>/models/`
#' respectively. The pin is also written to `<dir>/.certara-mcp/session.json`
#' and restored automatically after a server restart.
#'
#' @param dir Optional new project root (single non-empty path). Setting it
#'   re-points the repro script and report Rmd and creates the subdirectories.
#' @return The active project root, or `NULL` when unset.
#' @examples
#' mcp_session_project_dir(tempdir())
#' mcp_session_project_dir()
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
    .mcp_session_write_pin(dir)
    # Re-point repro + report under the new root. Both live in this same
    # package, so call them directly rather than guarding with exists().
    mcp_repro_path(file.path(mcp_session_scripts_dir(), "certara_mcp_repro.R"))
    mcp_report_path(file.path(mcp_session_reports_dir(), "modeling_report.Rmd"))
  } else if (is.null(.mcp_session_paths_state$project_dir)) {
    restored <- .mcp_session_restore_pin()
    if (!is.null(restored)) {
      .mcp_session_paths_state$project_dir <- restored
      for (sub in c("scripts", "figures", "reports", "models")) {
        dir.create(file.path(restored, sub), showWarnings = FALSE,
                   recursive = TRUE)
      }
    }
  }
  .mcp_session_paths_state$project_dir
}

#' MCP session output directories
#'
#' Internal path helpers used by MCP tools and provider packages.
#'
#' @return The corresponding output directory, or `NULL` when the session
#'   project root is unset. `mcp_session_paths_reset()` returns invisibly.
#' @keywords internal
#' @name mcp_session_paths
NULL

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

# Reset session paths (tests and clean server start). Does not delete the
# on-disk pin files - those are what make the pin durable across restarts.
#' @rdname mcp_session_paths
#' @export
mcp_session_paths_reset <- function() {
  .mcp_session_paths_state$project_dir <- NULL
  invisible(NULL)
}
