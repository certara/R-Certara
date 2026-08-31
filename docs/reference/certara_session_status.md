# Report the Certara MCP session and execution context

Answers "which R is this acting on, and what can it reach?" when
troubleshooting the MCP server. Returns the server's runtime launch
options (whether the live-session bridge is enabled, which `btw` tool
groups are active), the resulting capabilities (live R code execution,
environment inspection), a description of the three R execution
contexts, and the exact steps to bridge an interactive session.

## Usage

``` r
certara_session_status()
```

## Value

A structured list with `server`, `session_tools_enabled`, `btw_groups`,
`project_dir`, `durability`, `memory` (whether per-user memory is on,
plus `next_action = "Certara.R::enable_memory()"` when it is off),
`live_code_execution`, `environment_inspection`, `execution_contexts`,
`connect_live_session`, and `next_steps`.

## Details

Values are reported as `NA` when called outside a running server (for
example in tests), because the launch options are recorded only at
[`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)
startup.

## See also

[`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md),
[`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md),
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)

## Examples

``` r
st <- certara_session_status()
st$execution_contexts
#> $server_process
#> [1] "The MCP server's own R process. Runs the Certara control-plane tools (KB, validation, job control). Short-lived and stateless between calls; it is NOT your interactive workspace and cannot see objects you created in RStudio/Positron."
#> 
#> $live_session
#> [1] "A separate interactive R session you bridge by running btw::btw_mcp_session() in it. Only reachable when the server was started with session_tools = TRUE. The btw 'env' tools inspect its objects and the btw 'run' tool (btw_tool_run_r) executes code in its global environment - this is where your fitted models and data frames live."
#> 
#> $job_child
#> [1] "A fresh child R process spawned per start_nlme_* / start_nlme_job call for heavy compute. Isolated in a per-run sandbox; read results back with get_nlme_job_status() / collect_nlme_job(), not from the live session."
#> 
```
