# Aggregate tools from all discovered tool providers

Aggregate tools from all discovered tool providers

## Usage

``` r
.mcp_provider_tools(
  dev_roots = character(0),
  providers = NULL,
  provider_groups = NULL
)
```

## Arguments

- dev_roots:

  Optional dev source-tree roots.

- providers:

  Optional character vector of provider package names to include
  (default: all discovered).

- provider_groups:

  Optional launch-profile group allowlist (default `NULL` = all). Either
  a plain character vector applied to every provider, or a named list of
  per-package allowlists with an optional `"*"` fallback.

## Value

A list with `tools` (flat list of ellmer ToolDefs) and `skipped`
(records with a reason, including providers that failed to build).
