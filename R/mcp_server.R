# MCP process entry point. Delegates protocol and transport to the mcptools
# package and exposes a curated subset of btw tools plus the host's own Certara
# tools (knowledge/memory/meta) and every discovered provider's tools.

# Curated tool subsets. Exposing fewer, more relevant tools improves the agent's
# tool-selection accuracy (large flat tool lists degrade it), and mcptools cannot
# change the advertised set after the server starts, so the operator picks a
# subset at launch. Each profile names the host tool groups to include and the
# provider group allowlist passed to discovery (NULL = all provider groups).
# `meta` is always present so find_certara_tools / capabilities / session_status
# stay reachable. Provider groups use the vocabulary providers expose via their
# builder's `groups` arg (or a declarative tool's `group`); a provider that
# offers none of the requested groups contributes nothing for that profile.
.mcp_tool_profiles <- function() {
  list(
    full        = list(host = c("meta", "knowledge", "memory"), provider_groups = NULL),
    core        = list(host = c("meta", "knowledge"),
                       provider_groups = c("knowledge", "data")),
    authoring   = list(host = c("meta", "knowledge"),
                       provider_groups = c("knowledge", "data", "comparison")),
    execution   = list(host = c("meta", "knowledge", "memory"),
                       provider_groups = c("knowledge", "data", "execution")),
    diagnostics = list(host = c("meta", "knowledge", "memory"),
                       provider_groups = c("knowledge", "data", "execution",
                                           "comparison", "interpretation"))
  )
}

.mcp_resolve_profile <- function(tool_profile) {
  profiles <- .mcp_tool_profiles()
  tool_profile <- match.arg(tool_profile, names(profiles))
  profiles[[tool_profile]]
}

#' Launch the Certara MCP server
#'
#' Starts an stdio MCP server exposing curated btw tools, the host's built-in
#' Certara knowledge / memory / meta tools, and the tools contributed by every
#' discovered provider package (e.g. Certara.RsNLME). Intended to be the
#' `command` an MCP client runs (see [write_mcp_config()]). Blocks serving
#' requests.
#'
#' @param btw_groups Which general-purpose `btw` R tool groups to expose
#'   alongside the Certara tools (default `"docs"`). `"docs"` lets the agent read
#'   R documentation; add `"pkg"` for package-development actions. For a live
#'   session bridged with `btw::btw_mcp_session()` (needs
#'   `session_tools = TRUE`), `"env"` exposes read-only object inspection and
#'   `"run"` exposes `btw_tool_run_r`. Other groups: `"files"`, `"git"`,
#'   `"github"`, `"ide"`, `"cran"`, `"web"`, `"sessioninfo"`. Use `character(0)`
#'   for none. Call [certara_session_status()] to see which are active.
#' @param session_tools Whether to expose tools that bridge to a separate, live
#'   R session registered with `btw::btw_mcp_session()`. Default `FALSE` uses
#'   only the server's own R process.
#' @param server_name MCP config key this process serves (default `"certara-r"`).
#'   Advertised in [certara_mcp_capabilities()] and used to build Cursor
#'   `CallMcpTool` routing (`user-<server_name>`).
#' @param job_watch_wait_seconds Per-call server-side watch budget (seconds) for
#'   provider long-job watch tools. Default `45` keeps a single call under
#'   Cursor's ~60s timeout; [write_mcp_config()] bakes a larger value (`600`)
#'   into the Claude Code and Codex launch commands. Clamped to `0..600`.
#' @param dev_roots Optional dev source-tree roots for KB and tool-provider
#'   discovery (unpublished provider trees).
#' @param providers Optional character vector of provider package names to
#'   include (default: every discovered provider).
#' @param tool_profile Which curated tool subset to expose, to keep the tool
#'   list focused: `"full"` (default, every tool), `"core"`
#'   (authoring/validation + data inspection, no job launches), `"authoring"`
#'   (core + model comparison), `"execution"` (core + fit/job tools), or
#'   `"diagnostics"` (core + execution + comparison + interpretation).
#'   [find_certara_tools()], [certara_mcp_capabilities()], and
#'   [certara_session_status()] are present in every profile.
#' @return Does not return under normal operation (serves until the client
#'   disconnects). Must be run non-interactively (the `Rscript -e` command an MCP
#'   client launches); calling it from an interactive R session is an error.
#' @examples
#' \dontrun{
#' # You do not call this in an interactive R session. Configure a client, which
#' # then launches the server as a non-interactive command:
#' write_mcp_config("cursor")
#'
#' # The client runs the equivalent of:
#' #   Rscript -e 'Certara.R::launch_certara_mcp(tool_profile = "core")'
#' }
#' @keywords internal
#' @export
launch_certara_mcp <- function(btw_groups = "docs",
                               session_tools = FALSE,
                               server_name = "certara-r",
                               job_watch_wait_seconds = 45,
                               dev_roots = character(0),
                               providers = NULL,
                               tool_profile = c("full", "core", "authoring",
                                                "execution", "diagnostics")) {
  # This serves a stdio JSON-RPC loop and is meant to be the command an MCP
  # client spawns (see write_mcp_config()), not something a user runs at the R
  # prompt. Fail early with an actionable message instead of mcptools' terse
  # "not intended for interactive use".
  .validate_btw_groups(btw_groups)
  if (interactive()) {
    stop("launch_certara_mcp() starts the stdio MCP server and must run ",
         "non-interactively - as the command an MCP client launches, e.g.\n",
         "  Rscript -e 'Certara.R::launch_certara_mcp()'\n",
         "Configure a client with write_mcp_config() and let it start the server.",
         call. = FALSE)
  }
  .mcp_set_server_name(server_name)
  profile <- .mcp_resolve_profile(tool_profile)
  tool_profile <- match.arg(tool_profile)
  if (length(dev_roots)) {
    # dev_roots load tool/KB providers from local source trees that are not
    # first-party-verified, so a stray path could inject untrusted tools. It is
    # explicitly opt-in; warn so it is never silent. Use only trusted paths.
    message(sprintf(
      "Certara MCP: loading providers from %d dev_root(s) - local, unverified source trees, not installed first-party packages. Use trusted paths only.",
      length(dev_roots)))
  }
  job_watch_wait_seconds <- suppressWarnings(as.numeric(job_watch_wait_seconds))
  if (length(job_watch_wait_seconds) != 1L || is.na(job_watch_wait_seconds)) {
    job_watch_wait_seconds <- 45
  }
  job_watch_wait_seconds <- max(0, min(job_watch_wait_seconds, 600))
  # Publish the job-watch budget to provider packages (e.g. Certara.RsNLME's
  # wait_for_nlme_job) via an env var rather than a cross-package internal call,
  # so the per-client budget actually reaches the provider's watch loop.
  Sys.setenv(CERTARA_MCP_JOB_WATCH_SECONDS = format(job_watch_wait_seconds,
                                                    scientific = FALSE, trim = TRUE))
  # Record the runtime launch options so certara_session_status() and
  # certara_mcp_capabilities() can report which features are actually wired up.
  .mcp_set_launch_config(btw_groups = btw_groups, session_tools = session_tools,
                         job_watch_wait_seconds = job_watch_wait_seconds,
                         tool_profile = tool_profile)

  # Warm the KB index once at startup so the first tool call is not penalized.
  invisible(tryCatch(
    .kb_build_index(dev_roots = dev_roots, refresh = TRUE),
    error = function(e) {
      message("Certara MCP: KB index build failed: ", conditionMessage(e))
    }
  ))

  prov <- .mcp_provider_tools(dev_roots = dev_roots, providers = providers,
                              provider_groups = profile$provider_groups)
  for (s in prov$skipped) {
    message(sprintf("Certara MCP: tool provider '%s' skipped: %s",
                    s$package %||% "<unknown>", s$reason))
  }

  tools <- c(
    if (length(btw_groups)) btw::btw_tools(btw_groups) else list(),
    .certara_host_tools(groups = profile$host),
    prov$tools
  )

  # Index the assembled catalog so find_certara_tools() retrieves over exactly
  # what this server exposes (not a stale or fuller set).
  .mcp_set_tool_index(.mcp_build_tool_index(tools))

  mcptools::mcp_server(tools = tools, session_tools = session_tools)
}
