# Performance report for the Certara MCP server

Times cold-start namespace loads and compares a trivial
capabilities/ping call against KB search (and a btw pkg tool when
available). Slow ping points at startup/loop; slow only on search points
at index/serialization; fast ping but slow session points at agent-side
cost.

## Usage

``` r
certara_mcp_perf_report(query = "structural parameters")
```

## Arguments

- query:

  KB query used for the search timing.

## Value

A list of timings (seconds).

## Examples

``` r
if (FALSE) { # \dontrun{
certara_mcp_perf_report()
} # }
```
