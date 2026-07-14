# Per-user runtime registry for launch_certara_mcp() processes. MCP servers
# are stdio child processes with no shared port or OS-level registry, and a
# generic `Rscript -e ...` process listing cannot reliably identify which
# processes are Certara MCP servers (vs. any other Rscript invocation). So each
# server process writes its own record here at startup and removes it on a
# normal exit; list_certara_mcp_servers() reads the registry and verifies each
# entry against the live process before reporting it as running.

# Registry directory, overridable via option for test isolation (mirrors the
# tools::R_user_dir("Certara.R", "data") pattern used for mcp-memory, but under
# "cache" since these records are ephemeral runtime state, not user data).
.mcp_registry_dir <- function() {
  override <- getOption("Certara.R.mcp_registry_dir")
  dir <- if (.is_scalar_string(override)) {
    override
  } else {
    file.path(tools::R_user_dir("Certara.R", "cache"), "mcp-servers")
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

.mcp_registry_path <- function(pid = Sys.getpid()) {
  file.path(.mcp_registry_dir(), sprintf("pid-%d.json", as.integer(pid)))
}

# Process start time (seconds since epoch), used to detect PID reuse: a dead
# server's PID can be recycled by an unrelated process before the stale
# registry entry is pruned. NA when unavailable (e.g. permission issues),
# which disables the reuse check but still allows a plain liveness check.
.mcp_registry_create_time <- function(pid = Sys.getpid()) {
  tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle(pid))),
           error = function(e) NA_real_)
}

# Write this process's record atomically (write to a per-pid temp file, then
# rename) so a reader never sees a partially written JSON file. Best-effort:
# callers should not let a registry failure block the server from starting.
.mcp_registry_register <- function(server_name, btw_groups, session_tools,
                                   job_watch_wait_seconds, tool_profile) {
  pid <- Sys.getpid()
  record <- list(
    pid = pid,
    create_time = .mcp_registry_create_time(pid),
    server_name = server_name,
    started_at = .mcp_now(),
    package_version = as.character(utils::packageVersion("Certara.R")),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    tool_profile = tool_profile,
    session_tools = isTRUE(session_tools),
    btw_groups = as.list(btw_groups),
    job_watch_wait_seconds = job_watch_wait_seconds,
    working_dir = getwd()
  )
  path <- .mcp_registry_path(pid)
  tmp <- paste0(path, ".tmp-", pid)
  jsonlite::write_json(record, tmp, auto_unbox = TRUE, null = "null")
  file.rename(tmp, path)
  invisible(record)
}

.mcp_registry_unregister <- function(pid = Sys.getpid()) {
  path <- .mcp_registry_path(pid)
  if (file.exists(path)) {
    unlink(path)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# Read every registry file. Each element carries `valid` (FALSE for
# unparsable JSON, e.g. a write interrupted mid-process-crash) and `file` (its
# path, so callers can prune it). simplifyVector turns the homogeneous
# btw_groups array into a plain character vector.
.mcp_registry_read_all <- function() {
  files <- Sys.glob(file.path(.mcp_registry_dir(), "pid-*.json"))
  lapply(files, function(f) {
    rec <- tryCatch(jsonlite::read_json(f, simplifyVector = TRUE),
                    error = function(e) NULL)
    if (is.null(rec)) {
      return(list(file = f, valid = FALSE))
    }
    c(rec, list(file = f, valid = TRUE))
  })
}

# TRUE only when the PID is running AND its process start time matches the
# recorded one (within a small tolerance for clock/rounding differences) -
# guards against reporting an unrelated process that has since reused the PID.
.mcp_registry_alive <- function(rec) {
  pid <- suppressWarnings(as.integer(rec$pid %||% NA))
  if (is.na(pid)) {
    return(FALSE)
  }
  handle <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)
  if (is.null(handle) ||
      !isTRUE(tryCatch(ps::ps_is_running(handle), error = function(e) FALSE))) {
    return(FALSE)
  }
  recorded_ct <- suppressWarnings(as.numeric(rec$create_time %||% NA))
  if (!is.na(recorded_ct)) {
    actual_ct <- tryCatch(as.numeric(ps::ps_create_time(handle)),
                          error = function(e) NA_real_)
    if (!is.na(actual_ct) && abs(actual_ct - recorded_ct) > 2) {
      return(FALSE)
    }
  }
  TRUE
}

# Delete registry entries that are unparsable or whose process is no longer
# running. Called at the start of every launch_certara_mcp() so the registry
# self-heals after crashed/killed servers, without list_certara_mcp_servers()
# (a read-only diagnostic) having to write to disk.
.mcp_registry_prune <- function() {
  for (rec in .mcp_registry_read_all()) {
    if (!isTRUE(rec$valid) || !.mcp_registry_alive(rec)) {
      unlink(rec$file)
    }
  }
  invisible(NULL)
}

# Test-only: clear every registry entry regardless of liveness.
.mcp_registry_reset <- function() {
  unlink(Sys.glob(file.path(.mcp_registry_dir(), "pid-*.json")))
  invisible(NULL)
}

.mcp_registry_empty_frame <- function() {
  data.frame(
    server_name = character(0), pid = integer(0), status = character(0),
    started_at = character(0), package_version = character(0),
    r_version = character(0), tool_profile = character(0),
    session_tools = logical(0), btw_groups = character(0),
    job_watch_wait_seconds = numeric(0), working_dir = character(0),
    stringsAsFactors = FALSE
  )
}

#' List running Certara MCP server processes
#'
#' Reports Certara MCP server processes started with [launch_certara_mcp()] on
#' this machine, discovered from a per-user runtime registry written at server
#' startup - not by scanning system processes, which cannot reliably identify a
#' generic `Rscript` process as a Certara MCP server. Complements
#' [list_certara_mcp_configs()], which reports what is *configured* to launch;
#' this reports what has actually *started*.
#'
#' Each registry entry is verified against the live process (PID and process
#' start time) before being reported as `"running"`; entries whose process has
#' exited, or whose PID was since reused by an unrelated process, are treated
#' as stale and omitted unless `include_stale = TRUE`. Stale entries left by a
#' crashed or forcibly killed server are cleaned up automatically the next time
#' a Certara MCP server starts.
#'
#' @param include_stale Logical; also include registry entries whose process
#'   is no longer running (reported with `status = "stale"`). Default `FALSE`.
#' @return A data frame with one row per discovered server: `server_name`,
#'   `pid`, `status` (`"running"` or `"stale"`), `started_at`,
#'   `package_version`, `r_version`, `tool_profile`, `session_tools`,
#'   `btw_groups`, `job_watch_wait_seconds`, and `working_dir`. Zero rows when
#'   no server is running.
#' @seealso [list_certara_mcp_configs()], [certara_session_status()],
#'   [launch_certara_mcp()]
#' @examples
#' list_certara_mcp_servers()
#' @export
list_certara_mcp_servers <- function(include_stale = FALSE) {
  recs <- .mcp_registry_read_all()
  rows <- list()
  for (rec in recs) {
    if (!isTRUE(rec$valid)) {
      next
    }
    alive <- .mcp_registry_alive(rec)
    if (!alive && !isTRUE(include_stale)) {
      next
    }
    rows[[length(rows) + 1]] <- data.frame(
      server_name = rec$server_name %||% NA_character_,
      pid = suppressWarnings(as.integer(rec$pid %||% NA)),
      status = if (alive) "running" else "stale",
      started_at = rec$started_at %||% NA_character_,
      package_version = rec$package_version %||% NA_character_,
      r_version = rec$r_version %||% NA_character_,
      tool_profile = rec$tool_profile %||% NA_character_,
      session_tools = isTRUE(rec$session_tools),
      btw_groups = paste(rec$btw_groups %||% character(0), collapse = ", "),
      job_watch_wait_seconds = suppressWarnings(
        as.numeric(rec$job_watch_wait_seconds %||% NA)
      ),
      working_dir = rec$working_dir %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(.mcp_registry_empty_frame())
  }
  out <- do.call(rbind, rows)
  out[order(out$started_at, decreasing = TRUE), , drop = FALSE]
}
