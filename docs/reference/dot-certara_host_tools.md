# Host-owned Certara MCP tools

The knowledge (KB), memory, and meta tools the host exposes itself, as a
list of
[`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)
objects. Provider-specific tools are added separately by
[`.mcp_provider_tools()`](https://github.com/certara/R-Certara/reference/dot-mcp_provider_tools.md).

## Usage

``` r
.certara_host_tools(groups = c("meta", "knowledge", "memory"))
```

## Arguments

- groups:

  Host tool groups to include: any of `"meta"`, `"knowledge"`,
  `"memory"` (default all). Launch profiles pass a subset.

## Value

A list of ellmer ToolDef objects.
