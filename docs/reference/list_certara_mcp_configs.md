# Inventory Certara MCP entries across client configuration files

Scans the on-disk MCP client configuration files
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
writes to and reports whether a Certara MCP server is configured in
each, without starting or contacting any process. Complements
[`list_certara_mcp_servers()`](https://github.com/certara/R-Certara/reference/list_certara_mcp_servers.md),
which reports what has actually *started*; this reports what is
*configured* to launch.

## Usage

``` r
list_certara_mcp_configs(
  client = c("cursor", "claude-code", "codex", "claude-desktop"),
  scope = c("project", "user", "local"),
  project_dir = ".",
  server_name = NULL
)
```

## Arguments

- client:

  One or more of `"cursor"`, `"claude-code"`, `"codex"`,
  `"claude-desktop"`. Defaults to all four.

- scope:

  One or more of `"project"`, `"user"`, `"local"`. Defaults to all three
  (ignored by clients that are not scoped, i.e. Claude Desktop and
  Codex, which are always reported at `"user"` scope).

- project_dir:

  Project root to check for project-scope files.

- server_name:

  Optional MCP server key to look for. When `NULL` (default), every
  configured entry is scanned for a
  [`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)
  call instead of assuming the `"certara-r"` convention, so a server
  configured under a different name is still discovered. When given,
  only that key is considered.

## Value

A data frame with one row per client/scope combination checked (or per
matching entry, if more than one server invokes
[`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)
in the same file): `client`, `scope`, `path`, `exists`, `configured`,
`server_key`, `status` (`"configured"`, `"not_configured"`,
`"parse_error"`, or `"unsupported"`), `command`, `server_name`,
`btw_groups`, `session_tools`, `job_watch_wait_seconds`, `tool_profile`,
and `note`.

## Details

A missing or unparsable config file never aborts the scan - each
requested client/scope combination gets its own row and `status`, so one
broken file does not hide the rest. Claude Code's `"user"`/`"local"`
scopes are managed through its CLI (`~/.claude.json`), which this
function does not read or write; those combinations are reported with
`status = "unsupported"`, as is Cursor's nonexistent `"local"` scope.
Claude Desktop and Codex are always user-scoped regardless of `scope`.

## See also

[`list_certara_mcp_servers()`](https://github.com/certara/R-Certara/reference/list_certara_mcp_servers.md),
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md),
[`remove_mcp_config()`](https://github.com/certara/R-Certara/reference/remove_mcp_config.md)

## Examples

``` r
list_certara_mcp_configs(client = "cursor", project_dir = tempdir())
#>   client   scope
#> 1 cursor project
#> 2 cursor    user
#> 3 cursor   local
#>                                                                   path exists
#> 1 C:\\Users\\jcraig\\AppData\\Local\\Temp\\RtmpEjytjC/.cursor/mcp.json  FALSE
#> 2                                   C:\\Users\\jcraig/.cursor/mcp.json   TRUE
#> 3                                                                 <NA>     NA
#>   configured server_key         status
#> 1      FALSE       <NA> not_configured
#> 2       TRUE  certara-r     configured
#> 3      FALSE       <NA>    unsupported
#>                                                command server_name btw_groups
#> 1                                                 <NA>        <NA>       <NA>
#> 2 C:\\Program Files\\R\\R-4.6.0\\bin\\x64\\Rscript.exe   certara-r       docs
#> 3                                                 <NA>        <NA>       <NA>
#>   session_tools job_watch_wait_seconds tool_profile
#> 1            NA                     NA         <NA>
#> 2         FALSE                     45         full
#> 3            NA                     NA         <NA>
#>                           note
#> 1                         <NA>
#> 2                         <NA>
#> 3 Cursor has no 'local' scope.
```
