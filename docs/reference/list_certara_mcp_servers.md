# List running Certara MCP server processes

Reports Certara MCP server processes started with
[`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)
on this machine, discovered from a per-user runtime registry written at
server startup - not by scanning system processes, which cannot reliably
identify a generic `Rscript` process as a Certara MCP server.
Complements
[`list_certara_mcp_configs()`](https://github.com/certara/R-Certara/reference/list_certara_mcp_configs.md),
which reports what is *configured* to launch; this reports what has
actually *started*.

## Usage

``` r
list_certara_mcp_servers(include_stale = FALSE)
```

## Arguments

- include_stale:

  Logical; also include registry entries whose process is no longer
  running (reported with `status = "stale"`). Default `FALSE`.

## Value

A data frame with one row per discovered server: `server_name`, `pid`,
`status` (`"running"` or `"stale"`), `started_at`, `package_version`,
`r_version`, `tool_profile`, `session_tools`, `btw_groups`,
`job_watch_wait_seconds`, and `working_dir`. Zero rows when no server is
running.

## Details

Each registry entry is verified against the live process (PID and
process start time) before being reported as `"running"`; entries whose
process has exited, or whose PID was since reused by an unrelated
process, are treated as stale and omitted unless `include_stale = TRUE`.
Stale entries left by a crashed or forcibly killed server are cleaned up
automatically the next time a Certara MCP server starts.

## See also

[`list_certara_mcp_configs()`](https://github.com/certara/R-Certara/reference/list_certara_mcp_configs.md),
[`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md),
[`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)

## Examples

``` r
list_certara_mcp_servers()
#>   server_name   pid  status               started_at package_version r_version
#> 1   certara-r 14364 running 2026-08-31T16:15:33-0700           2.0.0     4.6.0
#> 3   certara-r 28492 running 2026-08-31T16:15:32-0700           2.0.0     4.6.0
#> 2   certara-r 27808 running 2026-08-31T16:01:58-0700           2.0.0     4.6.0
#> 4   certara-r 40272 running 2026-08-31T16:01:54-0700           2.0.0     4.6.0
#>   tool_profile session_tools btw_groups job_watch_wait_seconds
#> 1         full         FALSE       docs                     45
#> 3         full         FALSE       docs                     45
#> 2         full         FALSE       docs                    600
#> 4         full         FALSE       docs                    600
#>           working_dir
#> 1     C:/Users/jcraig
#> 3     C:/Users/jcraig
#> 2 C:/WINDOWS/system32
#> 4 C:/WINDOWS/system32
```
