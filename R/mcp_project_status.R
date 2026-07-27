# Cross-provider project status. Mirrors the KB federation (mcp_kb_index.R)
# and tool-provider discovery (mcp_tool_providers.R): a provider optionally
# declares a `status_hook` (an exported `function(project_dir)`) in its
# inst/mcp/tools/manifest.json, and the host merges whatever each discovered
# provider's hook returns without hardcoding any provider's own status shape
# (Certara.RDarwin's run registry looks nothing like Certara.RsNLME's job/VPC/
# sequential-LRT summary, and that is fine - each is reported under its own
# package key). A provider with no `status_hook` is simply absent; this is
# fully optional and additive to the existing tools-manifest schema.

# `status_hook` reuses the same manifest discovery as tool builders, so a
# provider ships one manifest.json for both. Invalid/missing hooks are
# reported as a per-provider `error`, never fatal to the merge.
.mcp_call_status_hook <- function(provider, project_dir) {
  hook <- provider$manifest$status_hook
  if (!.is_scalar_string(hook)) {
    return(NULL)
  }
  pkg <- provider$package
  fn <- tryCatch(getExportedValue(pkg, hook), error = function(e) NULL)
  if (!is.function(fn)) {
    return(list(package = pkg, status_hook = hook,
               error = sprintf("status_hook '%s' is not an exported function of '%s'.", hook, pkg)))
  }
  status <- tryCatch(fn(project_dir), error = function(e) NULL)
  if (is.null(status)) {
    return(list(package = pkg, status_hook = hook,
               error = sprintf("status_hook '%s' failed or returned NULL.", hook)))
  }
  list(package = pkg, status_hook = hook, status = status)
}

# normalizePath(mustWork = FALSE) only fully resolves the components of a
# path up to the first one that doesn't exist yet; for a not-yet-written
# repro script this can hand back Windows 8.3 short names (e.g. "MTOMAS~1")
# for parent directories that DO exist, while a separately-normalized
# project_dir resolves to the long name - a spurious mismatch. Recurse up to
# the nearest existing ancestor to canonicalize, then reattach the
# not-yet-existing remainder verbatim (nothing to resolve there).
.mcp_normalize_path_lenient <- function(path) {
  if (file.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  parent <- dirname(path)
  if (identical(parent, path)) return(gsub("\\\\", "/", path))
  file.path(.mcp_normalize_path_lenient(parent), basename(path))
}

# TRUE when `path` is `root` itself or a true descendant. Both sides are
# expected to already use `/` separators (as from .mcp_normalize_path_lenient()).
# A bare startsWith() would treat a sibling whose name merely begins with
# `root` (e.g. /tmp/proj vs /tmp/proj2) as rooted under it; requiring a path
# boundary after the root avoids that. On Windows the file system is
# case-insensitive, so compare case-folded.
.mcp_path_within <- function(path, root) {
  root <- sub("/+$", "", root)
  if (.Platform$OS.type == "windows") {
    path <- tolower(path)
    root <- tolower(root)
  }
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

# TRUE when the active repro script is not rooted under `project_dir` - e.g.
# a provider launched a job with a project_dir the session recorder was never
# pointed at (mcp_session_project_dir() was never called or was set for a
# different project), so the "executed script" a user would replay silently
# describes a different project than the one they asked about. Best-effort:
# an unresolvable directory is never flagged. Reads the recorder's internal
# path state directly (rather than mcp_repro_path(), which lazily creates a
# default under tempdir() the first time it's called) so a session where
# recording simply has not started yet is "no mismatch", not a false
# positive against tempdir().
.mcp_repro_project_mismatch <- function(project_dir) {
  repro <- .mcp_repro_state$path
  if (is.null(repro) || !nzchar(repro)) return(FALSE)
  proj_abs <- tryCatch(.mcp_normalize_path_lenient(project_dir),
                       error = function(e) NA_character_)
  repro_abs <- tryCatch(.mcp_normalize_path_lenient(repro),
                        error = function(e) NA_character_)
  if (is.na(proj_abs) || is.na(repro_abs)) return(FALSE)
  !.mcp_path_within(repro_abs, proj_abs)
}

# A best-effort "what's next" hint pulled from whichever provider's status
# names a `recommended_next_phase` (Certara.RsNLME's project-workflow status
# does); returns NA when no provider offers one. Provider-specific, so this
# stays a soft hint rather than a cross-provider guarantee.
.mcp_status_next_phase <- function(providers) {
  for (p in providers) {
    if (!is.list(p$status)) next
    phase <- p$status$recommended_next_phase
    if (.is_scalar_string(phase)) {
      return(list(package = p$package, phase = phase))
    }
  }
  NULL
}

#' Merged cross-provider project status
#'
#' Calls every discovered provider's optional `status_hook` (declared in its
#' `inst/mcp/tools/manifest.json`, see [.mcp_discover_tool_providers()]) with
#' `project_dir` and merges the results into one snapshot, so an agent can see
#' where a project stands across Certara.RDarwin searches,
#' Certara.RsNLME jobs/VPCs/sequential-LRT sessions, and any other provider
#' that ships a hook - without this host hardcoding any provider's own status
#' representation. A provider with no `status_hook` is absent from `providers`
#' rather than treated as an error. Nothing is written; provider-local
#' artifacts (Darwin run directories, LRT session/decision files, repro
#' scripts) remain the source of truth - this only surfaces the pointers each
#' provider's status already returns (e.g. `run_dir`, `session_dir`,
#' `plan_path`, `decisions_path`).
#'
#' @param project_dir Project root (default `"."`).
#' @param dev_roots Optional dev source-tree roots for provider discovery
#'   (see [launch_certara_mcp()]).
#' @return A list with `project_dir`, `providers` (one entry per provider that
#'   declares a `status_hook`: `package`, `status_hook`, and either `status`
#'   or `error`), `next_gated_phase` (a best-effort hint from whichever
#'   provider's status names one, or `NULL`), `repro_script`, `report_rmd`
#'   (this session's own accumulated artifacts), and
#'   `repro_project_mismatch` (`TRUE` when `repro_script` is not rooted under
#'   `project_dir` - the session recorder was never pointed at this project,
#'   so it is not a faithful replay of work done here).
#' @keywords internal
#' @export
get_certara_project_status <- function(project_dir = ".", dev_roots = character(0)) {
  disc <- .mcp_discover_tool_providers(dev_roots)
  providers <- list()
  for (p in disc$providers) {
    entry <- .mcp_call_status_hook(p, project_dir)
    if (!is.null(entry)) {
      providers[[length(providers) + 1L]] <- entry
    }
  }
  mismatch <- .mcp_repro_project_mismatch(project_dir)
  list(
    project_dir = normalizePath(project_dir, mustWork = FALSE),
    providers = providers,
    next_gated_phase = .mcp_status_next_phase(providers),
    repro_script = mcp_repro_path(),
    report_rmd = mcp_report_path(),
    repro_project_mismatch = mismatch,
    note = paste(
      "Merged from each discovered provider's own status_hook; a provider",
      "with no status_hook is simply absent here. Provider-local artifacts",
      "(run/session directories, plan/decisions files, repro scripts) remain",
      "the source of truth - this surfaces their pointers, not their",
      "content.",
      if (isTRUE(mismatch)) {
        paste(
          "WARNING: repro_script is not rooted under project_dir - it does",
          "not describe work done in this project. Call",
          "mcp_session_project_dir(project_dir) to re-point the recorder."
        )
      } else {
        NULL
      }
    )
  )
}
