# Launch the Certara MCP server

Starts an stdio MCP server exposing curated btw tools, the host's
built-in Certara knowledge / memory / meta tools, and the tools
contributed by every discovered provider package (e.g. Certara.RsNLME).
Intended to be the `command` an MCP client runs (see
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)).
Blocks serving requests.

## Usage

``` r
launch_certara_mcp(
  btw_groups = "docs",
  session_tools = FALSE,
  server_name = "certara-r",
  job_watch_wait_seconds = 45,
  dev_roots = character(0),
  providers = NULL,
  tool_profile = c("full", "core", "authoring", "execution", "diagnostics")
)
```

## Arguments

- btw_groups:

  Which general-purpose `btw` R tool groups to expose alongside the
  Certara tools (default `"docs"`). `"docs"` lets the agent read R
  documentation; add `"pkg"` for package-development actions. For a live
  session bridged with
  [`btw::btw_mcp_session()`](https://posit-dev.github.io/btw/reference/mcp.html)
  (needs `session_tools = TRUE`), `"env"` exposes read-only object
  inspection and `"run"` exposes `btw_tool_run_r`. Other groups:
  `"files"`, `"git"`, `"github"`, `"ide"`, `"cran"`, `"web"`,
  `"sessioninfo"`. Use `character(0)` for none. Call
  [`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
  to see which are active.

- session_tools:

  Whether to expose tools that bridge to a separate, live R session
  registered with
  [`btw::btw_mcp_session()`](https://posit-dev.github.io/btw/reference/mcp.html).
  Default `FALSE` uses only the server's own R process.

- server_name:

  MCP config key this process serves (default `"certara-r"`). Advertised
  in
  [`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md)
  and used to build Cursor `CallMcpTool` routing (`user-<server_name>`).

- job_watch_wait_seconds:

  Per-call server-side watch budget (seconds) for provider long-job
  watch tools. Default `45` keeps a single call under Cursor's ~60s
  timeout;
  [`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
  bakes a larger value (`600`) into the Claude Code and Codex launch
  commands. Clamped to `0..600`.

- dev_roots:

  Optional dev source-tree roots for KB and tool-provider discovery
  (unpublished provider trees).

- providers:

  Optional character vector of provider package names to include
  (default: every discovered provider).

- tool_profile:

  Which curated tool subset to expose, to keep the tool list focused:
  `"full"` (default, every tool), `"core"` (authoring/validation + data
  inspection, no job launches), `"authoring"` (core + model comparison),
  `"execution"` (core + fit/job tools), or `"diagnostics"` (core +
  execution + comparison + interpretation).
  [`find_certara_tools()`](https://github.com/certara/R-Certara/reference/find_certara_tools.md),
  [`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md),
  and
  [`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
  are present in every profile.

## Value

Does not return under normal operation (serves until the client
disconnects). Must be run non-interactively (the `Rscript -e` command an
MCP client launches); calling it from an interactive R session is an
error.

## Examples

``` r
if (FALSE) { # \dontrun{
# You do not call this in an interactive R session. Configure a client, which
# then launches the server as a non-interactive command:
write_mcp_config("cursor")

# The client runs the equivalent of:
#   Rscript -e 'Certara.R::launch_certara_mcp(tool_profile = "core")'
} # }
```
