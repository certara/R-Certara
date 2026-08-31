# Discover installed-package tool manifests

Discover installed-package tool manifests

## Usage

``` r
.mcp_discover_tool_providers(dev_roots = character(0))
```

## Arguments

- dev_roots:

  Optional package source-tree roots to include.

## Value

A list with `providers` (loadable records) and `skipped` (with a
reason). Each provider record carries `package`, `manifest`, `mode`
(`"builder"`), `tool_count`, and `manifest_path`.
