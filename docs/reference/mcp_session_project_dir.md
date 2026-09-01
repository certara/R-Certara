# Session project root for MCP deliverables

Get or set the session project root. When set, reproducible scripts,
figures, report Rmd files, and saved models are written under
`<dir>/scripts/`, `<dir>/figures/`, `<dir>/reports/`, and
`<dir>/models/` respectively. The pin is also written to
`<dir>/.certara-mcp/session.json` and restored automatically after a
server restart.

## Usage

``` r
mcp_session_project_dir(dir = NULL)
```

## Arguments

- dir:

  Optional new project root (single non-empty path). Setting it
  re-points the repro script and report Rmd and creates the
  subdirectories.

## Value

The active project root, or `NULL` when unset.

## Examples

``` r
mcp_session_project_dir(tempdir())
#> [1] "C:/Users/jcraig/AppData/Local/Temp/RtmpEjytjC"
mcp_session_project_dir()
#> [1] "C:/Users/jcraig/AppData/Local/Temp/RtmpEjytjC"
```
