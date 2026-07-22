# MCP session and execution context

## Certara.R.fn.certara_session_status
type: function_doc
title: certara_session_status() - which R is this acting on, and what can it reach
summary: Reports the MCP server's runtime launch options (live-session bridge on/off, active btw groups), the resulting capabilities (live code execution, environment inspection), the three R execution contexts (server process vs bridged live session vs per-job child), and the exact steps to attach an interactive session - so the agent asks instead of guessing which R holds a model.
keywords: certara_session_status, session, execution context, server process, live session, btw_mcp_session, job child, which R, run code, inspect objects, session_tools, btw groups, list_r_sessions, select_r_session, can't find model
symbols: certara_session_status, certara_mcp_capabilities, launch_certara_mcp, write_mcp_config
applies_to.model_type: pk, pd
related: Certara.RsNLME.workflow.execution_contracts, Certara.RsNLME.guidance.topic.vpc_fit_target, Certara.R.mcp.setup_troubleshooting
provenance.source_file: R/mcp_session_status.R
provenance.symbol: certara_session_status
source.kind: Rd

`certara_session_status()` answers "which R is this acting on, and what can it
reach?" for the calling agent. It removes the recurring confusion between three
distinct R execution contexts:

- **Server process** - the MCP server's own short-lived, stateless R process that
  runs the Certara control-plane tools. It is NOT your interactive workspace and
  cannot see objects you created in RStudio/Positron.
- **Live session** - a separate interactive R session bridged by running
  `btw::btw_mcp_session()` in it; only reachable when the server was started with
  `session_tools = TRUE`. The `btw` `env` tools inspect its objects and
  `btw_tool_run_r` (`run` group) executes code there - this is where your fitted
  models and data frames live.
- **Job child** - a fresh child R process spawned per `start_nlme_*` /
  `start_nlme_job` call for heavy compute, isolated in a per-run sandbox; read
  results back with `get_nlme_job_status()` / `collect_nlme_job()`.

The result reports `server`, `session_tools_enabled`, `btw_groups`,
`live_code_execution` (enabled + the `btw_tool_run_r` tool), `environment_inspection`
(enabled + the `btw` `env` tools), `execution_contexts` (the three descriptions
above), `connect_live_session` (the ordered steps plus `list_r_sessions` /
`select_r_session`), and a conditional `next_steps` string tailored to what is
actually wired. Values are `NA` when called outside a running server (e.g. tests),
because launch options are recorded only at `launch_certara_mcp()` startup.

Use it before assuming an interactive object is reachable (e.g. a VPC handoff -
see `Certara.RsNLME.guidance.topic.vpc_fit_target`) and when fits/VPC "can't find
the model" (see `Certara.R.mcp.setup_troubleshooting`).
`certara_mcp_capabilities()$execution_contexts` is a short pointer back to this
tool, not a duplicate of its three-context map.

```r
s <- certara_session_status()
s$session_tools_enabled   # is the live-session bridge on?
s$live_code_execution$enabled
s$next_steps              # what to do next, given the current wiring
```
