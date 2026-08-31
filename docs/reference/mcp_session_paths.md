# MCP session output directories

Internal path helpers used by MCP tools and provider packages.

## Usage

``` r
mcp_session_scripts_dir()

mcp_session_figures_dir()

mcp_session_reports_dir()

mcp_session_models_dir()

mcp_session_paths_reset()
```

## Value

The corresponding output directory, or `NULL` when the session project
root is unset. `mcp_session_paths_reset()` returns invisibly.
