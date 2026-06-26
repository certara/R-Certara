# Client configuration writer. Cursor and Claude Code keep config in JSON files
# we can merge idempotently, so those are written in place; Claude Code user and
# local scopes are registered through its CLI instead. Codex is configured by
# writing its TOML config directly, so per-tool MCP approval settings can be
# expressed. The optional tool_allowlist pre-authorizes the server's tools per
# client (Cursor permissions.json, Claude Code settings.json; Codex MCP tool
# approval settings). An absolute Rscript path is always used so the launch
# works regardless of the client's PATH.

#' Write MCP client configuration for the Certara server
#'
#' @param client One or more of `"cursor"`, `"claude-code"`, `"codex"`,
#'   `"claude-desktop"`. A bare call defaults to `"cursor"` only; pass a vector
#'   to configure several. `"claude-desktop"` targets Claude Desktop and its
#'   Cowork local-agent mode (user scope only).
#' @param scope One or more of `"project"`, `"user"`, `"local"`. A bare call
#'   defaults to `"project"` only. `"local"` is Claude Code-only (a per-project
#'   private entry in `~/.claude.json`); it is skipped with a warning for
#'   `"cursor"`, which has no local tier, and is irrelevant to `"codex"`.
#' @param btw_groups Which general-purpose `btw` R tool groups to expose to the
#'   agent alongside the Certara tools; the chosen groups are baked into the
#'   launch command. The default `"docs"` provides read-only R documentation
#'   lookup (help pages, help-topic listings, vignettes, release notes). Add
#'   `"pkg"` for package-development actions (document, check, test, coverage,
#'   load-all). Two groups enable working against a live session bridged with
#'   `btw::btw_mcp_session()` (set `session_tools = TRUE` as well): `"env"`
#'   inspects that session's objects (loaded data frames, fitted models) and
#'   `"run"` exposes `btw_tool_run_r`, which executes R code in that session's
#'   global environment - include `"run"` only when you want the agent to run
#'   arbitrary code there. Other groups `btw` offers include `"files"`,
#'   `"git"`, `"github"`, `"ide"`, `"cran"`, `"web"`, and `"sessioninfo"`. Use
#'   `"docs"` alone for the leanest, lowest-latency server, or `character(0)` to
#'   expose only the Certara tools.
#' @param session_tools Whether to expose tools that bridge to a separate, live
#'   R session you register with `btw::btw_mcp_session()` (for example your
#'   interactive RStudio session), letting the agent inspect that session's
#'   objects - loaded data frames, fitted models - and run code there. When
#'   `FALSE` (the default) the server uses only its own R process, which starts
#'   faster and avoids the session hop; enable it to work against your live
#'   workspace. When `TRUE`, pair it with `btw_groups` `"env"` (inspect objects)
#'   and/or `"run"` (run code via `btw_tool_run_r`), then run
#'   `btw::btw_mcp_session()` in the session you want reached; the call prints a
#'   post-setup checklist. [certara_session_status()] reports the live wiring.
#' @param tool_profile Curated tool subset baked into the launch command, to
#'   keep the tool list focused: `"full"` (default), `"core"`, `"authoring"`,
#'   `"execution"`, or `"diagnostics"`. See [launch_certara_mcp()].
#' @param server_name MCP server key (default `"certara-r"`).
#' @param project_dir Project root for project-scope files.
#' @param tool_allowlist When `TRUE`, pre-authorize the Certara MCP tools so they
#'   run without per-tool approval prompts, using each requested client's native
#'   mechanism for every requested scope:
#'   \itemize{
#'     \item Cursor: merge `server_name:*` into `permissions.json`
#'       (`<project>/.cursor/permissions.json` or `~/.cursor/permissions.json`);
#'       takes effect under Cursor Run Mode.
#'     \item Claude Code: merge `mcp__server_name` into `permissions$allow` of the
#'       scope-appropriate settings file (`<project>/.claude/settings.json` for
#'       project, `<project>/.claude/settings.local.json` for local,
#'       `~/.claude/settings.json` for user).
#'     \item Codex: write a user-scope managed block into
#'       `~/.codex/config.toml` with this MCP server and Codex MCP approval
#'       settings. Most Certara tools are approved, while privileged cleanup and
#'       arbitrary-R job tools stay prompt-gated.
#'   }
#'   Default `TRUE`.
#' @param run For CLI-managed targets (Claude Code user/local scope), actually
#'   execute the client CLI when it is on `PATH` instead of only printing the
#'   command. Falls back to printing if the CLI is not found. Default `FALSE`.
#'   Codex is configured by writing `~/.codex/config.toml` directly (the only way
#'   to set per-tool approvals), so `run` does not invoke the Codex CLI; the
#'   equivalent `codex mcp add` command is reported for reference.
#' @param job_watch_wait_seconds Optional override for the per-call server-side
#'   job-watch budget (seconds) baked into the launch command, used by
#'   [wait_for_nlme_job()]. When `NULL` (the default) each client gets a sensible
#'   default: `45` for Cursor (which caps a single MCP tool call at ~60s and has
#'   no timeout setting) and `600` for Claude Code and Codex (which allow
#'   long-running tool calls). Supply a single number to override every selected
#'   client; clamped to `0..600`.
#' @param agent Alias for `client`, for users who think of these as agent
#'   configuration targets. Use either `client` or `agent`, not both.
#' @return Invisibly, a list describing actions taken (written paths, snippets).
#' @details For `"claude-code"`, the routing/behavior contract is delivered as a
#'   `CLAUDE.md`-imported guidance doc (`.claude/certara-mcp-usage.md` for
#'   project/local, `~/.claude/certara-mcp-usage.md` for user), since Claude Code
#'   does not read Cursor `.mdc` rules; [remove_mcp_config()] removes it.
#'
#'   For `"claude-desktop"` (Claude Desktop and Cowork), the server is merged into
#'   `claude_desktop_config.json` (user scope only). On Windows the MSIX
#'   virtualized path Desktop actually reads is targeted when present. A companion
#'   `certara-mcp-usage.md` is written beside that file for manual reference -
#'   Desktop/Cowork do not auto-load it the way Claude Code imports `CLAUDE.md`,
#'   so call `certara_mcp_capabilities` at session start. There is no working
#'   config-file tool allowlist for Cowork, so `tool_allowlist` only prints a note
#'   (approve tools in the UI per session). Fully quit and relaunch Claude Desktop
#'   after config changes.
#'
#'   For the leanest, lowest-latency Claude Code or Desktop server, drop the
#'   package-development tools with `btw_groups = "docs"` (or `character(0)` for
#'   Certara tools only).
#' @examples
#' \dontrun{
#' # Cursor, project scope (the bare-call default).
#' write_mcp_config()
#'
#' # Lean Claude Code server: Certara tools + R docs only, no pkg-dev tools.
#' write_mcp_config("claude-code", btw_groups = "docs")
#'
#' # Claude Code at user scope, registering via the claude CLI when present.
#' write_mcp_config("claude-code", scope = "user", run = TRUE)
#'
#' # Claude Desktop / Cowork (merges claude_desktop_config.json).
#' write_mcp_config("claude-desktop")
#' }
#' @export
write_mcp_config <- function(client = c("cursor", "claude-code", "codex",
                                        "claude-desktop"),
                             scope = c("project", "user", "local"),
                             btw_groups = "docs",
                             session_tools = FALSE,
                             tool_profile = c("full", "core", "authoring",
                                              "execution", "diagnostics"),
                             server_name = "certara-r",
                             project_dir = ".",
                             tool_allowlist = TRUE,
                             run = FALSE,
                             job_watch_wait_seconds = NULL,
                             agent = NULL) {
  # several.ok = TRUE lets callers request multiple clients/scopes, but a bare
  # call should configure one sensible target rather than every client and scope
  # at once. Default to the first choice unless the caller is explicit.
  choices_client <- c("cursor", "claude-code", "codex", "claude-desktop")
  choices_scope <- c("project", "user", "local")
  client_missing <- missing(client)
  if (!is.null(agent)) {
    if (!client_missing) {
      stop("Use either 'client' or 'agent', not both.", call. = FALSE)
    }
    client <- agent
  }
  if (client_missing && is.null(agent)) client <- choices_client[1]
  if (missing(scope)) scope <- choices_scope[1]
  client <- match.arg(client, choices_client, several.ok = TRUE)
  scope <- match.arg(scope, choices_scope, several.ok = TRUE)
  tool_profile <- match.arg(tool_profile)
  .validate_btw_groups(btw_groups)
  if (!is.null(job_watch_wait_seconds)) {
    job_watch_wait_seconds <- suppressWarnings(as.numeric(job_watch_wait_seconds))
    if (length(job_watch_wait_seconds) != 1L || is.na(job_watch_wait_seconds)) {
      stop("`job_watch_wait_seconds` must be a single number.", call. = FALSE)
    }
  }

  command <- .mcp_rscript_bin()
  cursor_call_server <- paste0("user-", server_name)
  # Each client gets its own launch command: the server-side job-watch budget
  # (used by wait_for_nlme_job()) differs per client (see
  # .mcp_client_watch_seconds), so the launch expression is not shared.
  client_args <- function(cl) {
    c("-e", .mcp_launch_expr(
      btw_groups, session_tools, server_name,
      .mcp_client_watch_seconds(cl, job_watch_wait_seconds),
      tool_profile = tool_profile))
  }

  actions <- list()
  cursor_allowlist_note <- FALSE
  if ("cursor" %in% client) {
    cursor_args <- client_args("cursor")
    cursor_server_block <- list(command = command, args = as.list(cursor_args),
                                type = "stdio")
    for (sc in scope) {
      if (sc == "local") {
        warning("Cursor has no 'local' scope; skipping (use 'project' or 'user').",
                call. = FALSE)
        next
      }
      path <- .cursor_config_path(sc, project_dir)
      if (isTRUE(tool_allowlist)) {
        .assert_json_parsable(.cursor_permissions_path(project_dir, scope = sc))
      }
      existed <- isTRUE(.json_has_server(path, server_name))
      .merge_json_mcp(path, server_name, cursor_server_block)
      message(sprintf("Cursor (%s scope): %s '%s' in %s",
                      sc, if (existed) "updated" else "added", server_name, path))
      message(sprintf(
        "  CallMcpTool server id: '%s' (mcp.json key is '%s')",
        cursor_call_server, server_name
      ))
      rule_written <- NULL
      permissions_written <- NULL
      if (sc == "project") {
        rule_path <- .cursor_mcp_rule_path(project_dir)
        rule_written <- .write_cursor_mcp_rule(rule_path, server_name)
        message(sprintf("  Agent rule: %s", rule_path))
      }
      if (isTRUE(tool_allowlist)) {
        perm_path <- .cursor_permissions_path(project_dir, scope = sc)
        permissions_written <- .merge_cursor_permissions(perm_path, server_name)
        message(sprintf(
          "  MCP allowlist (%s scope): %s (%s)",
          sc, perm_path, .cursor_mcp_allowlist_entry(server_name)
        ))
        cursor_allowlist_note <- TRUE
      }
      actions[[length(actions) + 1]] <-
        list(client = "cursor", scope = sc, written = path,
             call_mcp_server = cursor_call_server,
             rule_written = rule_written,
             permissions_written = permissions_written)
    }
    if (cursor_allowlist_note) {
      message(paste(
        "  Enable Cursor Run Mode (Allowlist or Auto-review) so",
        "permissions.json takes effect."
      ))
    }
  }
  if ("claude-code" %in% client) {
    claude_budget <- .mcp_client_watch_seconds("claude-code", job_watch_wait_seconds)
    claude_args <- client_args("claude-code")
    for (sc in scope) {
      if (isTRUE(tool_allowlist)) {
        .assert_json_parsable(.claude_settings_path(sc, project_dir))
      }
      if (sc == "project") {
        path <- file.path(project_dir, ".mcp.json")
        existed <- isTRUE(.json_has_server(path, server_name))
        # Project scope is a JSON server block we own, so set a per-server
        # tool-call timeout (ms) large enough for a full watch budget plus
        # margin - the Claude Code analogue of Codex's tool_timeout_sec. The
        # CLI-managed user/local scopes rely on Claude Code's MCP_TOOL_TIMEOUT
        # default (~28h), which already covers the budget.
        claude_server_block <- list(command = command,
                                    args = as.list(claude_args), type = "stdio",
                                    timeout = claude_budget * 1000 + 60000)
        .merge_json_mcp(path, server_name, claude_server_block)
        message(sprintf("Claude Code (project scope): %s '%s' in %s",
                        if (existed) "updated" else "added", server_name, path))
        rec <- list(written = path)
      } else {
        # "user" and "local" are both managed by the claude CLI: user lives at
        # the top level of ~/.claude.json, local under the project key. Pass the
        # scope through explicitly rather than relying on the CLI default.
        rec <- .cli_command(sprintf("Claude Code (%s scope)", sc), "claude",
                            c("mcp", "add", "-s", sc, server_name, "--",
                              command, claude_args),
                            run)
      }
      permissions_written <- NULL
      if (isTRUE(tool_allowlist)) {
        perm_path <- .claude_settings_path(sc, project_dir)
        permissions_written <- .merge_claude_permissions(perm_path, server_name)
        message(sprintf(
          "  Tool allowlist (%s scope): %s (%s)",
          sc, perm_path, .claude_mcp_allowlist_entry(server_name)
        ))
      }
      # Claude Code has no .mdc rules; deliver the behavior contract as a
      # CLAUDE.md-imported guidance doc (the analog of the Cursor rule).
      guidance_written <- .write_claude_mcp_guidance(sc, project_dir, server_name)
      message(sprintf(
        "  MCP guidance (%s scope): %s (imported from %s)",
        sc, guidance_written$doc, guidance_written$memory
      ))
      actions[[length(actions) + 1]] <-
        c(list(client = "claude-code", scope = sc), rec,
          list(permissions_written = permissions_written,
               guidance_written = guidance_written))
    }
  }
  if ("claude-desktop" %in% client) {
    if (!"user" %in% scope) {
      message(paste(
        "Claude Desktop is configured at user scope only; configuring it",
        "regardless of the requested scope."
      ))
    }
    desktop_args <- client_args("claude-desktop")
    path <- .claude_desktop_config_path()
    existed <- isTRUE(.json_has_server(path, server_name))
    # Plain stdio block: Claude Desktop bridges it into the Cowork VM as an
    # "sdk" server. No per-server timeout key (unlike Claude Code project scope).
    desktop_server_block <- list(command = command, args = as.list(desktop_args),
                                 type = "stdio")
    .merge_json_mcp(path, server_name, desktop_server_block)
    message(sprintf("Claude Desktop (user scope): %s '%s' in %s",
                    if (existed) "updated" else "added", server_name, path))
    if (.claude_desktop_msix_active()) {
      message(paste(
        "  Windows MSIX: wrote the virtualized path Claude Desktop actually",
        "reads (not necessarily the file its 'Edit Config' button opens)."
      ))
    }
    if (isTRUE(tool_allowlist)) {
      # No config-file allowlist works for Cowork: per-session enabledMcpTools
      # resets and 'Always allow' is not persisted (anthropics/claude-code#24433,
      # #56954). So this is a note, never a file write.
      message(paste(
        "  Tool allowlist: Claude Desktop/Cowork has no working config-file",
        "allowlist - 'Always allow'/'Allow for all tasks' does not persist.",
        "Approve the Certara tools in the UI per session."
      ))
    }
    guidance_written <- .write_desktop_mcp_guidance(dirname(path), server_name)
    message(sprintf(
      "  MCP guidance (manual, not auto-loaded): %s - call certara_mcp_capabilities first.",
      guidance_written$doc
    ))
    message("  Fully quit and relaunch Claude Desktop for MCP changes to take effect.")
    actions[[length(actions) + 1]] <-
      list(client = "claude-desktop", scope = "user", written = path,
           guidance_written = guidance_written)
  }
  if ("codex" %in% client) {
    codex_args <- client_args("codex")
    gated <- .mcp_gated_tool_names()
    snippet <- .codex_snippet(server_name, command, codex_args, tool_allowlist,
                               gated = gated)
    config_written <- .write_codex_mcp_config(
      .codex_config_path(), server_name, command, codex_args, tool_allowlist,
      gated = gated
    )
    guidance_written <- .write_codex_agents_guidance(
      .codex_agents_path(), command
    )
    # The config-file write IS the configuration, and is the ONLY mechanism that
    # can set the per-tool approval modes. `codex mcp add` would append a SECOND
    # [mcp_servers.<name>] table (a duplicate-key TOML error) and cannot carry
    # the approvals, so we never invoke it - not even with run = TRUE. The
    # equivalent CLI command is reported for reference only.
    rec <- list(command = sprintf(
      "codex mcp add %s -- %s %s",
      server_name, command, paste(.quote_if_needed(codex_args), collapse = " ")
    ), ran = FALSE)
    message(sprintf("Codex (user scope): wrote '%s' in %s",
                    server_name, config_written))
    message(sprintf("  User guidance: %s", guidance_written))
    if (isTRUE(tool_allowlist)) {
      if (length(gated)) {
        gated_msg <- paste0(
          paste(gated, collapse = " and "), " left as prompt-gated tools."
        )
        message(paste(
          "  MCP tool approvals: default approve for this server, with",
          gated_msg
        ))
      } else {
        message(paste(
          "  MCP tool approvals: Codex will prompt for all tools",
          "(no provider tools marked gated)."
        ))
      }
    }
    actions[[length(actions) + 1]] <- c(
      list(client = "codex", toml = snippet,
           written = config_written,
           guidance_written = guidance_written),
      rec
    )
  }

  if (isTRUE(session_tools)) {
    for (line in .mcp_session_setup_checklist(btw_groups)) message(line)
  }

  invisible(actions)
}

# Post-setup checklist emitted when session_tools = TRUE: the file write alone
# does not bridge a live session, so spell out the remaining manual steps and
# flag missing btw groups that would leave inspection/execution tools absent.
.mcp_session_setup_checklist <- function(btw_groups) {
  lines <- c(
    "Live-session bridge (session_tools = TRUE) - finish setup:",
    "  1. Reload/restart the MCP server in your client so it reads the new config.",
    "  2. In the interactive R session you want reached, run: btw::btw_mcp_session()",
    "  3. Attach to it from the agent: list_r_sessions then select_r_session.",
    "  4. Verify the wiring anytime with the certara_session_status tool."
  )
  if (!"env" %in% btw_groups) {
    lines <- c(lines, paste(
      "  Note: btw_groups has no 'env' - object inspection tools",
      "(btw_tool_env_*) will be absent. Add 'env' to inspect session objects."
    ))
  }
  if (!"run" %in% btw_groups) {
    lines <- c(lines, paste(
      "  Note: btw_groups has no 'run' - btw_tool_run_r (run code in the",
      "session) will be absent. Add 'run' to execute code in the session."
    ))
  }
  lines
}

#' Remove the Certara MCP server from client configuration
#'
#' Inverse of [write_mcp_config()]. For file-based clients (Cursor, Claude Code
#' project scope) it removes the named server while preserving every other
#' server. For CLI-managed targets (Claude Code user/local scope, Codex) it
#' prints the removal command to run.
#'
#' @inheritParams write_mcp_config
#' @return Invisibly, a list describing actions taken (paths edited, commands).
#' @examples
#' \dontrun{
#' # Remove the Cursor project-scope config written by write_mcp_config().
#' remove_mcp_config("cursor")
#' }
#' @export
remove_mcp_config <- function(client = c("cursor", "claude-code", "codex",
                                         "claude-desktop"),
                              scope = c("project", "user", "local"),
                              server_name = "certara-r",
                              project_dir = ".",
                              run = FALSE) {
  choices_client <- c("cursor", "claude-code", "codex", "claude-desktop")
  choices_scope <- c("project", "user", "local")
  if (missing(client)) client <- choices_client[1]
  if (missing(scope)) scope <- choices_scope[1]
  client <- match.arg(client, choices_client, several.ok = TRUE)
  scope <- match.arg(scope, choices_scope, several.ok = TRUE)

  actions <- list()
  if ("cursor" %in% client) {
    for (sc in scope) {
      if (sc == "local") {
        warning("Cursor has no 'local' scope; skipping (use 'project' or 'user').",
                call. = FALSE)
        next
      }
      path <- .cursor_config_path(sc, project_dir)
      .assert_json_parsable(.cursor_permissions_path(project_dir, scope = sc))
      removed <- .unmerge_json_mcp(path, server_name)
      rule_removed <- NULL
      permissions_removed <- NULL
      if (sc == "project") {
        rule_removed <- .remove_cursor_mcp_rule(.cursor_mcp_rule_path(project_dir))
      }
      permissions_removed <- .unmerge_cursor_permissions(
        .cursor_permissions_path(project_dir, scope = sc), server_name
      )
      message(sprintf("Cursor (%s scope): %s in %s",
                      sc,
                      if (removed) sprintf("removed '%s'", server_name)
                      else sprintf("'%s' not present", server_name),
                      path))
      actions[[length(actions) + 1]] <-
        list(client = "cursor", scope = sc, path = path, removed = removed,
             rule_removed = rule_removed,
             permissions_removed = permissions_removed)
    }
  }
  if ("claude-code" %in% client) {
    for (sc in scope) {
      .assert_json_parsable(.claude_settings_path(sc, project_dir))
      if (sc == "project") {
        .assert_json_parsable(file.path(project_dir, ".mcp.json"))
      }
      permissions_removed <- .unmerge_claude_permissions(
        .claude_settings_path(sc, project_dir), server_name
      )
      guidance_removed <- .remove_claude_mcp_guidance(sc, project_dir, server_name)
      if (sc == "project") {
        path <- file.path(project_dir, ".mcp.json")
        removed <- .unmerge_json_mcp(path, server_name)
        message(sprintf("Claude Code (project scope): %s in %s",
                        if (removed) sprintf("removed '%s'", server_name)
                        else sprintf("'%s' not present", server_name),
                        path))
        rec <- list(path = path, removed = removed)
      } else {
        rec <- .cli_command(sprintf("Claude Code (%s scope)", sc), "claude",
                            c("mcp", "remove", "-s", sc, server_name), run)
      }
      actions[[length(actions) + 1]] <-
        c(list(client = "claude-code", scope = sc), rec,
          list(permissions_removed = permissions_removed,
               guidance_removed = guidance_removed))
    }
  }
  if ("claude-desktop" %in% client) {
    path <- .claude_desktop_config_path()
    removed <- .unmerge_json_mcp(path, server_name)
    guidance_removed <- .remove_desktop_mcp_guidance(dirname(path), server_name)
    message(sprintf("Claude Desktop (user scope): %s in %s",
                    if (removed) sprintf("removed '%s'", server_name)
                    else sprintf("'%s' not present", server_name),
                    path))
    actions[[length(actions) + 1]] <-
      list(client = "claude-desktop", scope = "user", path = path,
           removed = removed, guidance_removed = guidance_removed)
  }
  if ("codex" %in% client) {
    # write_mcp_config() configures Codex by writing the config file directly, so
    # the inverse strips the managed config block and the AGENTS.md note rather
    # than relying on `codex mcp remove` (which would only know the server entry,
    # not our approval subtables or guidance note).
    config_removed <- .remove_codex_mcp_config(.codex_config_path(), server_name)
    guidance_removed <- .remove_codex_agents_guidance(.codex_agents_path())
    message(sprintf(
      "Codex (user scope): %s '%s' in %s",
      if (config_removed) "removed" else "'no managed block for'",
      server_name, .codex_config_path()
    ))
    actions[[length(actions) + 1]] <- list(
      client = "codex",
      config_removed = config_removed,
      guidance_removed = guidance_removed,
      command = sprintf("codex mcp remove %s", server_name),
      ran = FALSE
    )
  }

  invisible(actions)
}

# Build the -e launch expression with the requested options inlined.
.mcp_launch_expr <- function(btw_groups, session_tools, server_name = "certara-r",
                             job_watch_wait_seconds = 45,
                             tool_profile = "full") {
  groups <- if (length(btw_groups)) {
    sprintf("c(%s)", paste(sprintf("'%s'", btw_groups), collapse = ", "))
  } else {
    "character(0)"
  }
  safe_name <- gsub("'", "\\\\'", server_name)
  safe_profile <- gsub("'", "\\\\'", tool_profile)
  sprintf(
    paste0(
      "Certara.R::launch_certara_mcp(btw_groups = %s, ",
      "session_tools = %s, server_name = '%s', job_watch_wait_seconds = %s, ",
      "tool_profile = '%s')"
    ),
    groups, if (isTRUE(session_tools)) "TRUE" else "FALSE", safe_name,
    format(job_watch_wait_seconds, scientific = FALSE, trim = TRUE),
    safe_profile
  )
}

# Per-call server-side watch budget (seconds) baked into each client's launch
# command. Cursor caps a single MCP tool call at ~60s and exposes no timeout
# setting, so its budget stays under that ceiling; Claude Code and Codex both
# allow long-running tool calls (configurable timeouts), so one
# wait_for_nlme_job() call can cover a typical fit. An explicit override applies
# to every selected client. Clamped to 0..600 to match launch_certara_mcp().
.mcp_client_watch_seconds <- function(client, override = NULL) {
  budget <- if (!is.null(override)) {
    override
  } else {
    switch(client, cursor = 45, `claude-code` = 600, codex = 600,
           `claude-desktop` = 600, 45)
  }
  max(0, min(budget, 600))
}

.cursor_mcp_rule_path <- function(project_dir) {
  file.path(project_dir, ".cursor", "rules", "certara-mcp-usage.mdc")
}

.cursor_permissions_path <- function(project_dir = ".", scope = "project") {
  if (scope == "user") {
    file.path(.mcp_user_home(), ".cursor", "permissions.json")
  } else {
    file.path(project_dir, ".cursor", "permissions.json")
  }
}

# Cursor permissions.json uses the mcp.json server key, not user-{key}.
.cursor_mcp_allowlist_entry <- function(server_name) {
  sprintf("%s:*", server_name)
}

.read_json_or_stop <- function(path) {
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      stop(
        sprintf(
          "Refusing to proceed: could not read '%s' (%s). Fix or remove it, then retry.",
          path, conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

# Validate-only preflight: stop on corrupt JSON before any mutating write.
.assert_json_parsable <- function(path) {
  if (file.exists(path)) {
    .read_json_or_stop(path)
  }
  invisible(path)
}

.merge_cursor_permissions <- function(path, server_name) {
  entry <- .cursor_mcp_allowlist_entry(server_name)
  existing <- if (file.exists(path)) {
    .read_json_or_stop(path)
  } else {
    list()
  }
  allow <- unlist(existing$mcpAllowlist %||% list(), use.names = FALSE)
  if (!entry %in% allow) {
    allow <- c(allow, entry)
  }
  existing$mcpAllowlist <- as.list(allow)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(existing, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  invisible(path)
}

.unmerge_cursor_permissions <- function(path, server_name) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  entry <- .cursor_mcp_allowlist_entry(server_name)
  existing <- .read_json_or_stop(path)
  allow <- unlist(existing$mcpAllowlist %||% list(), use.names = FALSE)
  if (!entry %in% allow) {
    return(FALSE)
  }
  allow <- setdiff(allow, entry)
  if (length(allow)) {
    existing$mcpAllowlist <- as.list(allow)
    jsonlite::write_json(existing, path, auto_unbox = TRUE, pretty = TRUE,
                         null = "null")
  } else {
    existing$mcpAllowlist <- NULL
    if (length(existing)) {
      jsonlite::write_json(existing, path, auto_unbox = TRUE, pretty = TRUE,
                           null = "null")
    } else {
      unlink(path)
    }
  }
  TRUE
}

# Claude Code stores tool pre-authorization in settings.json under
# permissions$allow. Scope picks the file: project -> .claude/settings.json,
# local -> .claude/settings.local.json (project-private), user -> the per-user
# ~/.claude/settings.json.
.claude_settings_path <- function(scope, project_dir = ".") {
  if (scope == "user") {
    file.path(.mcp_user_home(), ".claude", "settings.json")
  } else if (scope == "local") {
    file.path(project_dir, ".claude", "settings.local.json")
  } else {
    file.path(project_dir, ".claude", "settings.json")
  }
}

# Bare-server rule matches every tool the server exposes (the canonical form;
# Claude Code does not need a trailing wildcard).
.claude_mcp_allowlist_entry <- function(server_name) {
  sprintf("mcp__%s", server_name)
}

.merge_claude_permissions <- function(path, server_name) {
  entry <- .claude_mcp_allowlist_entry(server_name)
  existing <- if (file.exists(path)) {
    .read_json_or_stop(path)
  } else {
    list()
  }
  if (is.null(existing$permissions)) existing$permissions <- list()
  allow <- unlist(existing$permissions$allow %||% list(), use.names = FALSE)
  if (!entry %in% allow) {
    allow <- c(allow, entry)
  }
  existing$permissions$allow <- as.list(allow)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(existing, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  invisible(path)
}

.unmerge_claude_permissions <- function(path, server_name) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  entry <- .claude_mcp_allowlist_entry(server_name)
  existing <- .read_json_or_stop(path)
  allow <- unlist(existing$permissions$allow %||% list(), use.names = FALSE)
  if (!entry %in% allow) {
    return(FALSE)
  }
  allow <- setdiff(allow, entry)
  if (length(allow)) {
    existing$permissions$allow <- as.list(allow)
  } else {
    existing$permissions$allow <- NULL
    if (length(existing$permissions) == 0) existing$permissions <- NULL
  }
  if (length(existing)) {
    jsonlite::write_json(existing, path, auto_unbox = TRUE, pretty = TRUE,
                         null = "null")
  } else {
    unlink(path)
  }
  TRUE
}

# Claude Code reads CLAUDE.md (project), CLAUDE.local.md (project-private), and
# ~/.claude/CLAUDE.md (user) as always-on memory, but it does NOT read Cursor's
# .cursor/rules/*.mdc. So the routing/behavior contract is delivered here
# instead: the full guidance lives in a dedicated, auto-generated doc and the
# scope's CLAUDE.md gets a small managed block that @-imports it. This keeps the
# user's memory file light and the block idempotent and removable.
.claude_guidance_marker <- function() {
  "auto-generated by Certara.R::write_mcp_config"
}

.claude_guidance_doc_path <- function(scope, project_dir = ".") {
  if (scope == "user") {
    file.path(.mcp_user_home(), ".claude", "certara-mcp-usage.md")
  } else {
    file.path(project_dir, ".claude", "certara-mcp-usage.md")
  }
}

# The CLAUDE.md that imports the guidance doc, per scope.
.claude_memory_path <- function(scope, project_dir = ".") {
  if (scope == "user") {
    file.path(.mcp_user_home(), ".claude", "CLAUDE.md")
  } else if (scope == "local") {
    file.path(project_dir, "CLAUDE.local.md")
  } else {
    file.path(project_dir, "CLAUDE.md")
  }
}

# Import path is relative to the memory file's own directory. For user scope
# both files sit in ~/.claude; for project/local the doc is under .claude.
.claude_guidance_import <- function(scope) {
  if (scope == "user") "@certara-mcp-usage.md" else "@.claude/certara-mcp-usage.md"
}

.claude_guidance_block_markers <- function(server_name) {
  list(
    begin = sprintf("<!-- BEGIN %s MCP guidance (%s) -->",
                    server_name, .claude_guidance_marker()),
    end = sprintf("<!-- END %s MCP guidance -->", server_name)
  )
}

# Remove a begin..end marked region (inclusive) and the blank lines it leaves
# behind. Returns the lines unchanged when no begin marker is present.
.strip_marked_block <- function(lines, begin, end) {
  if (!length(lines)) {
    return(lines)
  }
  b <- which(lines == begin)
  if (!length(b)) {
    return(lines)
  }
  b <- b[1]
  before <- if (b > 1) lines[seq_len(b - 1)] else character(0)
  e <- which(lines == end & seq_along(lines) >= b)
  if (!length(e)) {
    warning(
      "Managed block begin marker found but end marker is missing; ",
      "removing only the begin line to preserve user content below.",
      call. = FALSE
    )
    after <- if (b < length(lines)) lines[(b + 1):length(lines)] else character(0)
  } else {
    e <- e[1]
    after <- if (e < length(lines)) lines[(e + 1):length(lines)] else character(0)
  }
  while (length(before) && !nzchar(before[length(before)])) {
    before <- before[-length(before)]
  }
  while (length(after) && !nzchar(after[1])) {
    after <- after[-1]
  }
  c(before, after)
}

# Behavior contract rendered for Claude Code. Reuses the same rule text the
# capabilities tool advertises, so guidance stays single-sourced.
.claude_mcp_guidance_body <- function(server_name) {
  ns <- sprintf("mcp__%s__", server_name)
  # Render the merged behavior rules (generic host rules plus whatever provider
  # capability fragments contribute) so this doc stays provider-agnostic and in
  # sync with certara_mcp_capabilities() instead of hard-coding domain rules.
  caps <- tryCatch(certara_mcp_capabilities(), error = function(e) NULL)
  rule_lines <- character(0)
  if (!is.null(caps) && length(caps$rules)) {
    for (id in names(caps$rules)) {
      label <- gsub("_", " ", id)
      label <- paste0(toupper(substring(label, 1, 1)), substring(label, 2))
      rule_lines <- c(rule_lines, paste0("- **", label, ".** ", caps$rules[[id]]))
    }
  } else {
    warning(
      "certara_mcp_capabilities() failed or returned no rules; ",
      "CLAUDE.md guidance will reference the capabilities tool only.",
      call. = FALSE
    )
    rule_lines <- "- See `certara_mcp_capabilities()$rules`."
  }
  c(
    sprintf("<!-- %s -->", .claude_guidance_marker()),
    "<!-- Regenerate with Certara.R::write_mcp_config(); manual edits are overwritten. -->",
    "",
    sprintf("# Certara MCP usage (server: `%s`)", server_name),
    "",
    sprintf(paste(
      "Tools from this server appear as `%s<tool>`. Prefer them (and the Certara",
      "KB) over ad hoc R for pharmacometrics work."), ns),
    "",
    "## Session start (do this first)",
    "",
    paste(
      "1. Call `certara_mcp_capabilities` once - it returns versions, discovered",
      "KB and tool providers, the concurrency policy, gated tools, and the",
      "behavior rules below."),
    paste(
      "2. If memory is enabled, call `get_user_preferences` and `get_lessons`",
      "before proposing a workflow."),
    paste(
      "3. Search the KB (`search_certara_kb`, `guide_pharmacometrics`) and use",
      "the provider tools listed by `certara_mcp_capabilities` for domain work."),
    "",
    "## Behavior rules",
    "",
    rule_lines
  )
}

# Write the guidance doc and ensure the scope's CLAUDE.md imports it. Returns the
# two paths touched.
.write_claude_mcp_guidance <- function(scope, project_dir, server_name) {
  doc <- .claude_guidance_doc_path(scope, project_dir)
  mem <- .claude_memory_path(scope, project_dir)

  dir.create(dirname(doc), showWarnings = FALSE, recursive = TRUE)
  writeLines(.claude_mcp_guidance_body(server_name), doc, useBytes = TRUE)

  markers <- .claude_guidance_block_markers(server_name)
  block <- c(markers$begin, .claude_guidance_import(scope), markers$end)
  existing <- if (file.exists(mem)) readLines(mem, warn = FALSE) else character(0)
  kept <- .strip_marked_block(existing, markers$begin, markers$end)
  new_lines <- if (length(kept) && any(nzchar(kept))) c(kept, "", block) else block
  dir.create(dirname(mem), showWarnings = FALSE, recursive = TRUE)
  writeLines(new_lines, mem, useBytes = TRUE)

  list(doc = doc, memory = mem)
}

# Inverse: delete our auto-generated doc and strip the import block, deleting the
# memory file only when it held nothing but our block. Returns TRUE if either
# the doc or the block was present.
.remove_claude_mcp_guidance <- function(scope, project_dir, server_name) {
  doc <- .claude_guidance_doc_path(scope, project_dir)
  mem <- .claude_memory_path(scope, project_dir)
  removed <- FALSE

  if (file.exists(doc) &&
      any(grepl(.claude_guidance_marker(), readLines(doc, warn = FALSE),
                fixed = TRUE))) {
    unlink(doc)
    removed <- TRUE
  }
  if (file.exists(mem)) {
    markers <- .claude_guidance_block_markers(server_name)
    lines <- readLines(mem, warn = FALSE)
    kept <- .strip_marked_block(lines, markers$begin, markers$end)
    if (!identical(kept, lines)) {
      removed <- TRUE
      if (length(kept) && any(nzchar(kept))) {
        writeLines(kept, mem, useBytes = TRUE)
      } else {
        unlink(mem)
      }
    }
  }
  removed
}

.cursor_mcp_rule_template_path <- function() {
  path <- system.file("mcp", "certara-mcp-usage.mdc", package = "Certara.R")
  if (nzchar(path)) {
    return(path)
  }
  # devtools::load_all() / test_check from package source tree
  desc <- file.path(getwd(), "DESCRIPTION")
  if (file.exists(desc)) {
    pkg <- tryCatch(as.character(read.dcf(desc)[1, "Package"]), error = function(e) NA)
    if (identical(pkg, "Certara.R")) {
      alt <- file.path(getwd(), "inst", "mcp", "certara-mcp-usage.mdc")
      if (file.exists(alt)) {
        return(normalizePath(alt, winslash = "/"))
      }
    }
  }
  ""
}

# Marker shared by the shipped template and .is_auto_cursor_mcp_rule().
.cursor_mcp_rule_marker <- function() {
  "auto-generated by Certara.R::write_mcp_config"
}

.write_cursor_mcp_rule <- function(path, server_name) {
  template <- .cursor_mcp_rule_template_path()
  if (!nzchar(template)) {
    warning("Certara MCP cursor rule template not found; rule file not written.",
            call. = FALSE)
    return(invisible(NULL))
  }
  body <- paste(readLines(template, warn = FALSE), collapse = "\n")
  body <- gsub("__SERVER_NAME__", server_name, body, fixed = TRUE)
  body <- gsub("__CALL_MCP_SERVER__", paste0("user-", server_name), body,
               fixed = TRUE)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(body, path, useBytes = TRUE)
  invisible(path)
}

.is_auto_cursor_mcp_rule <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  any(grepl(.cursor_mcp_rule_marker(), readLines(path, warn = FALSE),
            fixed = TRUE))
}

.remove_cursor_mcp_rule <- function(path) {
  if (!.is_auto_cursor_mcp_rule(path)) {
    return(FALSE)
  }
  unlink(path)
  TRUE
}

.cursor_config_path <- function(scope, project_dir) {
  if (scope == "user") {
    file.path(.mcp_user_home(), ".cursor", "mcp.json")
  } else {
    file.path(project_dir, ".cursor", "mcp.json")
  }
}

# Real per-user home. On Windows R's "~" resolves to Documents, but MCP clients
# read their user config from the profile root (e.g. %USERPROFILE%\.cursor), so
# resolve that explicitly to keep user-scope config in the location the client
# actually reads. On other platforms honor HOME directly rather than relying on
# path.expand("~"), which does not always reflect a HOME set in the current
# session.
.mcp_user_home <- function() {
  if (.Platform$OS.type == "windows") {
    profile <- Sys.getenv("USERPROFILE")
    if (nzchar(profile)) {
      return(profile)
    }
  } else {
    home <- Sys.getenv("HOME")
    if (nzchar(home)) {
      return(home)
    }
  }
  path.expand("~")
}

# ---- Claude Desktop / Cowork ------------------------------------------------
# Claude Desktop (and its Cowork local-agent mode) reads MCP servers from
# claude_desktop_config.json, NOT Claude Code's ~/.claude.json or settings.json.
# On Windows the app ships as an MSIX package whose writes to %APPDATA%\Claude are
# transparently redirected to a per-package virtualized store; the running app
# reads that virtualized copy, so we target it when present
# (anthropics/claude-code#26073). Other platforms use ~/.config/Claude.
.claude_desktop_config_dir <- function() {
  if (.Platform$OS.type == "windows") {
    local <- Sys.getenv("LOCALAPPDATA")
    if (nzchar(local)) {
      pkgs <- Sys.glob(file.path(local, "Packages", "Claude_*",
                                 "LocalCache", "Roaming", "Claude"))
      pkgs <- pkgs[dir.exists(pkgs)]
      if (length(pkgs)) {
        if (length(pkgs) > 1) {
          mtimes <- file.info(pkgs)$mtime
          if (all(is.na(mtimes))) {
            pkgs <- sort(pkgs)
            warning(
              "Multiple Claude Desktop MSIX config directories found; ",
              "could not determine modification times; using first by name: ",
              pkgs[1], "\n",
              "Candidates: ", paste(pkgs, collapse = ", "),
              call. = FALSE
            )
          } else {
            pkgs <- pkgs[order(mtimes, decreasing = TRUE, na.last = TRUE)]
            warning(
              "Multiple Claude Desktop MSIX config directories found; ",
              "using the most recently modified: ", pkgs[1], "\n",
              "Candidates: ", paste(pkgs, collapse = ", "),
              call. = FALSE
            )
          }
        }
        return(pkgs[1])
      }
    }
    appdata <- Sys.getenv("APPDATA")
    if (nzchar(appdata)) {
      return(file.path(appdata, "Claude"))
    }
    return(file.path(.mcp_user_home(), "AppData", "Roaming", "Claude"))
  }
  file.path(.mcp_user_home(), ".config", "Claude")
}

.claude_desktop_config_path <- function() {
  file.path(.claude_desktop_config_dir(), "claude_desktop_config.json")
}

# TRUE when the resolved config dir is the MSIX-virtualized location, so the user
# can be warned that Desktop's "Edit Config" button may open a different file.
.claude_desktop_msix_active <- function() {
  if (.Platform$OS.type != "windows") {
    return(FALSE)
  }
  grepl("[\\\\/]Packages[\\\\/]Claude_", .claude_desktop_config_dir())
}

# Standalone guidance doc beside the desktop config. Claude Desktop/Cowork has no
# CLAUDE.md analog to @-import it, so it is NOT auto-loaded: the preamble tells
# the agent to call certara_mcp_capabilities first. Reuses the Claude Code body
# so the behavior contract stays single-sourced.
.desktop_guidance_doc_path <- function(config_dir) {
  file.path(config_dir, "certara-mcp-usage.md")
}

.desktop_mcp_guidance_body <- function(server_name) {
  body <- .claude_mcp_guidance_body(server_name)
  preamble <- c(
    "<!-- Claude Desktop / Cowork does NOT auto-load this file (no CLAUDE.md import). -->",
    "<!-- Call certara_mcp_capabilities first; optionally paste the sections below into Cowork custom instructions. -->"
  )
  # body[1] is the auto-generated marker comment; keep it first so removal can
  # detect the file, then insert the Desktop-specific note.
  c(body[1], preamble, body[-1])
}

.write_desktop_mcp_guidance <- function(config_dir, server_name) {
  doc <- .desktop_guidance_doc_path(config_dir)
  dir.create(dirname(doc), showWarnings = FALSE, recursive = TRUE)
  writeLines(.desktop_mcp_guidance_body(server_name), doc, useBytes = TRUE)
  list(doc = doc)
}

.remove_desktop_mcp_guidance <- function(config_dir, server_name) {
  doc <- .desktop_guidance_doc_path(config_dir)
  if (file.exists(doc) &&
      any(grepl(.claude_guidance_marker(), readLines(doc, warn = FALSE),
                fixed = TRUE))) {
    unlink(doc)
    return(TRUE)
  }
  FALSE
}

# TRUE if the config file already defines a server under this name.
.json_has_server <- function(path, server_name) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  existing <- .read_json_or_stop(path)
  !is.null(existing$mcpServers[[server_name]])
}

# Read-modify-write merge: preserve other servers, set/replace ours.
.merge_json_mcp <- function(path, server_name, server_block) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  existing <- if (file.exists(path)) {
    .read_json_or_stop(path)
  } else {
    list()
  }
  if (is.null(existing$mcpServers)) existing$mcpServers <- list()
  existing$mcpServers[[server_name]] <- server_block
  jsonlite::write_json(existing, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  invisible(path)
}

# Read-modify-write removal: drop our server, preserve the rest. Returns TRUE if
# the server was present. An emptied mcpServers is written as an object ({}) so
# the client still parses the file.
.unmerge_json_mcp <- function(path, server_name) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  existing <- .read_json_or_stop(path)
  if (is.null(existing$mcpServers) ||
      is.null(existing$mcpServers[[server_name]])) {
    return(FALSE)
  }
  existing$mcpServers[[server_name]] <- NULL
  if (length(existing$mcpServers) == 0 && length(existing) == 1) {
    writeLines("{\n  \"mcpServers\": {}\n}", path)
  } else {
    jsonlite::write_json(existing, path, auto_unbox = TRUE, pretty = TRUE,
                         null = "null")
  }
  TRUE
}

# Run a client CLI when run = TRUE and the program is on PATH; otherwise print
# the command for the user. Tokens with spaces/quotes are quoted so the printed
# line is copy-pasteable and the executed args survive the shell intact.
.cli_command <- function(label, program, args, run) {
  args_fmt <- .quote_if_needed(args)
  pretty <- paste(program, paste(args_fmt, collapse = " "))
  if (!isTRUE(run)) {
    message(sprintf("%s - run:\n  %s", label, pretty))
    return(list(command = pretty, ran = FALSE))
  }
  if (!nzchar(Sys.which(program))) {
    message(sprintf("%s: '%s' not found on PATH - run manually:\n  %s",
                    label, program, pretty))
    return(list(command = pretty, ran = FALSE))
  }
  out <- tryCatch(
    system2(program, args = args_fmt, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(out, "status")
  ok <- is.null(status) || identical(as.integer(status), 0L)
  message(sprintf("%s: %s\n  %s", label,
                  if (ok) "ran" else "command FAILED", pretty))
  if (length(out)) message(paste0("  ", out, collapse = "\n"))
  list(command = pretty, ran = isTRUE(ok))
}

# Quote only the tokens that need it, so simple tokens (mcp, add, --) stay bare.
.quote_if_needed <- function(x) {
  qtype <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  vapply(x, function(tok) {
    if (grepl("[[:space:]\"]", tok)) shQuote(tok, type = qtype) else tok
  }, character(1), USE.NAMES = FALSE)
}

.codex_config_path <- function() {
  file.path(.mcp_user_home(), ".codex", "config.toml")
}

.codex_agents_path <- function() {
  file.path(.mcp_user_home(), ".codex", "AGENTS.md")
}

.codex_marker <- function(server_name) {
  list(
    begin = sprintf("# BEGIN %s MCP config (auto-generated by Certara.R::write_mcp_config)", server_name),
    end = sprintf("# END %s MCP config", server_name)
  )
}

.codex_agents_marker <- function() {
  list(
    begin = "<!-- BEGIN Certara.R Rscript note (auto-generated by Certara.R::write_mcp_config) -->",
    end = "<!-- END Certara.R Rscript note -->"
  )
}

.codex_rscript_dir <- function(command) {
  ws <- if (.Platform$OS.type == "windows") "\\" else "/"
  normalizePath(dirname(command), winslash = ws, mustWork = FALSE)
}

# Platform-appropriate "prepend this dir to PATH" one-liner for the AGENTS note.
.codex_path_prepend_hint <- function(rdir) {
  if (.Platform$OS.type == "windows") {
    sprintf("PowerShell: `$env:Path = '%s;' + $env:Path`.", rdir)
  } else {
    sprintf("shell: `export PATH=\"%s:$PATH\"`.", rdir)
  }
}

.codex_prompt_tables <- function(server_name, gated = .mcp_gated_tool_names()) {
  if (!length(gated)) {
    return(character(0))
  }
  unlist(lapply(gated, function(t) c(
    "",
    sprintf("[mcp_servers.%s.tools.%s]", server_name, t),
    'approval_mode = "prompt"'
  )), use.names = FALSE)
}

.codex_managed_block <- function(server_name, command, args,
                                 tool_allowlist = TRUE,
                                 gated = .mcp_gated_tool_names()) {
  c(
    .codex_marker(server_name)$begin,
    sprintf("[mcp_servers.%s]", server_name),
    sprintf('command = "%s"', gsub("\\\\", "\\\\\\\\", command)),
    sprintf("args = [%s]", .toml_string_array(args)),
    # Long NLME fits should be watched with wait_for_nlme_job() so the MCP
    # server blocks internally (up to its 600s budget) instead of making the
    # agent poll. Codex defaults MCP tool calls to 60s, too short for that
    # pattern, so raise the per-server timeout to comfortably exceed the budget.
    "tool_timeout_sec = 900",
    # NOTE: default_tools_approval_mode and per-tool [..tools.<t>] approval_mode
    # are recent Codex config keys (openai/codex#16501); a Codex too old to know
    # them ignores the approvals (tools stay prompt-gated) rather than breaking
    # the server entry. Only emitted when tool_allowlist = TRUE and at least one
    # gated tool is discovered; otherwise Codex falls back to prompting for all.
    if (isTRUE(tool_allowlist)) {
      prompt_tables <- .codex_prompt_tables(server_name, gated)
      if (length(prompt_tables)) {
        c('default_tools_approval_mode = "approve"', prompt_tables)
      } else character(0)
    } else character(0),
    .codex_marker(server_name)$end
  )
}

# Render a character vector as a TOML array of basic strings. Escapes backslashes
# first (so a Windows path in an arg stays valid TOML) then double quotes - the
# same order the `command` key uses.
.toml_string_array <- function(x) {
  esc <- gsub("\\\\", "\\\\\\\\", x)
  esc <- gsub('"', '\\\\"', esc)
  paste(sprintf('"%s"', esc), collapse = ", ")
}

.write_codex_mcp_config <- function(path, server_name, command, args,
                                    tool_allowlist = TRUE,
                                    gated = .mcp_gated_tool_names()) {
  markers <- .codex_marker(server_name)
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
  kept <- .strip_marked_block(lines, markers$begin, markers$end)
  kept <- .strip_codex_mcp_tables(kept, server_name)
  block <- .codex_managed_block(server_name, command, args, tool_allowlist,
                                gated = gated)
  new_lines <- if (length(kept) && any(nzchar(kept))) c(kept, "", block) else block
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(new_lines, path, useBytes = TRUE)
  invisible(path)
}

# Remove an existing Codex MCP server table tree before writing the managed
# replacement. This lets write_mcp_config() migrate hand-written Codex config
# without touching unrelated Codex settings or other MCP servers.
.strip_codex_mcp_tables <- function(lines, server_name) {
  if (!length(lines)) {
    return(lines)
  }
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", server_name)
  target_header_re <- sprintf(
    "^\\s*\\[\\s*mcp_servers\\.%s(?:\\s*\\]|\\.)",
    escaped
  )
  any_header_re <- "^\\s*\\["

  out <- character(0)
  skip <- FALSE
  for (line in lines) {
    if (grepl(target_header_re, line, perl = TRUE)) {
      skip <- TRUE
      next
    }
    if (skip && grepl(any_header_re, line)) {
      skip <- FALSE
    }
    if (!skip) {
      out <- c(out, line)
    }
  }

  while (length(out) && !nzchar(out[length(out)])) {
    out <- out[-length(out)]
  }
  out
}

.write_codex_agents_guidance <- function(path, command) {
  markers <- .codex_agents_marker()
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
  kept <- .strip_marked_block(lines, markers$begin, markers$end)
  rdir <- .codex_rscript_dir(command)
  block <- c(
    markers$begin,
    "## Certara R local R",
    "",
    sprintf("- Rscript is available under `%s`.", rdir),
    sprintf("- Before running this repository's R commands, prepend it to PATH - %s",
            .codex_path_prepend_hint(rdir)),
    "- Prefer that Rscript for `testthat`, package checks, and MCP-related R commands in Certara R workspaces.",
    "- For long-running provider jobs (fits/bootstrap/searches), watch with the provider's wait tool (it blocks server-side) rather than polling status in a loop.",
    markers$end
  )
  new_lines <- if (length(kept) && any(nzchar(kept))) c(kept, "", block) else block
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(new_lines, path, useBytes = TRUE)
  invisible(path)
}

# Inverse of .write_codex_mcp_config: strip the managed [mcp_servers.<name>]
# marker block (and its approval subtables) from config.toml. Deletes the file
# only when nothing but our block remained. Returns TRUE if a block was removed.
.remove_codex_mcp_config <- function(path, server_name) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  markers <- .codex_marker(server_name)
  lines <- readLines(path, warn = FALSE)
  kept <- .strip_marked_block(lines, markers$begin, markers$end)
  if (identical(kept, lines)) {
    return(FALSE)
  }
  if (length(kept) && any(nzchar(kept))) {
    writeLines(kept, path, useBytes = TRUE)
  } else {
    unlink(path)
  }
  TRUE
}

# Inverse of .write_codex_agents_guidance: strip the managed Rscript note.
.remove_codex_agents_guidance <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  markers <- .codex_agents_marker()
  lines <- readLines(path, warn = FALSE)
  kept <- .strip_marked_block(lines, markers$begin, markers$end)
  if (identical(kept, lines)) {
    return(FALSE)
  }
  if (length(kept) && any(nzchar(kept))) {
    writeLines(kept, path, useBytes = TRUE)
  } else {
    unlink(path)
  }
  TRUE
}

.codex_snippet <- function(server_name, command, args, tool_allowlist = TRUE,
                           gated = .mcp_gated_tool_names()) {
  args_toml <- .toml_string_array(args)
  allowlist_toml <- ""
  if (isTRUE(tool_allowlist)) {
    prompt_tables <- .codex_prompt_tables(server_name, gated)
    if (length(prompt_tables)) {
      allowlist_toml <- paste0(
        'default_tools_approval_mode = "approve"\n',
        paste(prompt_tables, collapse = "\n"),
        "\n"
      )
    }
  }
  paste0(
    sprintf("[mcp_servers.%s]\n", server_name),
    sprintf('command = "%s"\n', gsub("\\\\", "\\\\\\\\", command)),
    sprintf("args = [%s]\n", args_toml),
    "tool_timeout_sec = 900\n",
    allowlist_toml
  )
}
