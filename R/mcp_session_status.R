# Session / execution-context reporting. Agents repeatedly get confused about
# *which* R the server is acting on: the MCP server's own short-lived process,
# a separate live session bridged with btw::btw_mcp_session(), or the
# child process a start_nlme_job() spawns. This module records the runtime
# launch options and exposes them, plus the remediation steps, through a single
# tool so the agent can ask instead of guessing.

# Launch options are stashed in the shared .mcp_state env (defined in
# mcp_kb_index.R) at launch_certara_mcp() startup. Outside a running server
# (tests, ad hoc calls) nothing is stored and the getter reports "unknown".
.mcp_set_launch_config <- function(btw_groups = NULL, session_tools = NULL,
                                   job_watch_wait_seconds = NULL,
                                   tool_profile = NULL) {
  .mcp_state$launch_config <- list(
    btw_groups = btw_groups,
    session_tools = session_tools,
    job_watch_wait_seconds = job_watch_wait_seconds,
    tool_profile = tool_profile
  )
  invisible(.mcp_state$launch_config)
}

.mcp_launch_config <- function() {
  cfg <- .mcp_state$launch_config
  if (is.null(cfg)) {
    list(btw_groups = NULL, session_tools = NULL,
         job_watch_wait_seconds = NULL, tool_profile = NULL)
  } else {
    cfg
  }
}

# The three R execution contexts this server can touch. Single-sourced here so
# certara_session_status() and certara_mcp_capabilities() agree.
.mcp_execution_contexts <- function() {
  list(
    server_process = paste(
      "The MCP server's own R process. Runs the Certara control-plane tools",
      "(KB, validation, job control). Short-lived and stateless between calls;",
      "it is NOT your interactive workspace and cannot see objects you created",
      "in RStudio/Positron."
    ),
    live_session = paste(
      "A separate interactive R session you bridge by running",
      "btw::btw_mcp_session() in it. Only reachable when the server was started",
      "with session_tools = TRUE. The btw 'env' tools inspect its objects and",
      "the btw 'run' tool (btw_tool_run_r) executes code in its global",
      "environment - this is where your fitted models and data frames live."
    ),
    job_child = paste(
      "A fresh child R process spawned per start_nlme_* / start_nlme_job call",
      "for heavy compute. Isolated in a per-run sandbox; read results back with",
      "get_nlme_job_status() / collect_nlme_job(), not from the live session."
    )
  )
}

#' Report the Certara MCP session and execution context
#'
#' Answers "which R is this acting on, and what can it reach?" for the calling
#' agent. Returns the server's runtime launch options (whether the live-session
#' bridge is enabled, which `btw` tool groups are active), the resulting
#' capabilities (live R code execution, environment inspection), a description
#' of the three R execution contexts, and the exact steps to bridge an
#' interactive session. Designed to remove the recurring confusion about the
#' server process vs. a live `btw::btw_mcp_session()` session vs. a job child
#' process.
#'
#' Values are reported as `NA` when called outside a running server (for example
#' in tests), because the launch options are recorded only at
#' [launch_certara_mcp()] startup.
#'
#' @return A structured list with `server`, `session_tools_enabled`,
#'   `btw_groups`, `live_code_execution`, `environment_inspection`,
#'   `execution_contexts`, `connect_live_session`, and `next_steps`.
#' @seealso [certara_mcp_capabilities()], [launch_certara_mcp()],
#'   [write_mcp_config()]
#' @examples
#' st <- certara_session_status()
#' st$execution_contexts
#' @keywords internal
#' @export
certara_session_status <- function() {
  cfg <- .mcp_launch_config()
  server_name <- .mcp_server_name()

  session_tools <- cfg$session_tools
  groups <- cfg$btw_groups
  groups_known <- !is.null(groups)
  has_group <- function(g) if (groups_known) g %in% groups else NA

  run_enabled <- if (isTRUE(session_tools)) has_group("run") else FALSE
  env_enabled <- if (isTRUE(session_tools)) has_group("env") else FALSE

  list(
    server = server_name,
    session_tools_enabled = if (is.null(session_tools)) NA else session_tools,
    btw_groups = if (groups_known) groups else NA_character_,
    live_code_execution = list(
      enabled = run_enabled,
      tool = "btw_tool_run_r",
      note = paste(
        "Runs R in the bridged live session's global environment. Requires",
        "session_tools = TRUE AND the 'run' btw group. Use for one-off",
        "inspection/plotting against objects you already created there; use the",
        "start_nlme_* tools for heavy fits."
      )
    ),
    environment_inspection = list(
      enabled = env_enabled,
      tools = c("btw_tool_env_describe_environment",
                "btw_tool_env_describe_data_frame"),
      note = paste(
        "Read-only listing/inspection of the bridged session's objects.",
        "Requires session_tools = TRUE AND the 'env' btw group.",
        "btw_tool_env_describe_data_frame takes the object name in the",
        "'data_frame' argument."
      )
    ),
    execution_contexts = .mcp_execution_contexts(),
    connect_live_session = list(
      enabled = if (is.null(session_tools)) NA else isTRUE(session_tools),
      steps = c(
        "1. Start the server with session_tools = TRUE (write_mcp_config(session_tools = TRUE, btw_groups = c('docs','pkg','env','run'))).",
        "2. In the interactive R session you want reached, run btw::btw_mcp_session().",
        "3. Reload the MCP server in the client so it picks up the config.",
        "4. Enumerate sessions with list_r_sessions, then choose one with select_r_session.",
        "5. Inspect with the btw 'env' tools; execute with btw_tool_run_r ('run' group)."
      ),
      enumerate_tool = "list_r_sessions",
      select_tool = "select_r_session"
    ),
    next_steps = .mcp_session_next_steps(session_tools, run_enabled, env_enabled)
  )
}

# Conditional, actionable guidance based on what is actually wired up.
.mcp_session_next_steps <- function(session_tools, run_enabled, env_enabled) {
  if (is.null(session_tools)) {
    return(paste(
      "Launch options unknown (not called from a running server). Inside a",
      "running server this reports whether the live-session bridge and the",
      "env/run btw groups are active."
    ))
  }
  if (!isTRUE(session_tools)) {
    return(paste(
      "Live-session bridge is OFF. To inspect or run code in your interactive",
      "R session, re-run write_mcp_config(session_tools = TRUE, btw_groups =",
      "c('docs','pkg','env','run')), reload the MCP server, and call",
      "btw::btw_mcp_session() in that session. Until then, use the start_nlme_*",
      "tools for any computation."
    ))
  }
  steps <- paste(
    "Live-session bridge is ON: run btw::btw_mcp_session() in your interactive",
    "session, then list_r_sessions / select_r_session to attach."
  )
  if (!isTRUE(env_enabled)) {
    steps <- paste(steps,
      "The 'env' group is not active, so object inspection tools are absent -",
      "add 'env' to btw_groups to enable them.")
  }
  if (!isTRUE(run_enabled)) {
    steps <- paste(steps,
      "The 'run' group is not active, so btw_tool_run_r is absent - add 'run'",
      "to btw_groups to execute code in the session.")
  }
  steps
}
