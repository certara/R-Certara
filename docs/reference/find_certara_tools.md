# Discover the tools relevant to a task

Retrieval over the registered tool catalog: returns the few tools whose
name/description best match a free-text task, so the agent picks from a
short ranked list instead of the full set. Call this first when unsure
which tool fits; then call the chosen tool(s) directly.

## Usage

``` r
find_certara_tools(task, limit = 5)
```

## Arguments

- task:

  Free-text description of what you are trying to do.

- limit:

  Maximum tools to return; clamped to 3..10 (default 5).

## Value

A list with `query`, ranked `tools` (`name`, `summary`, `provider`,
`score`), and a `note`. Empty `tools` means broaden the task or call
[`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md).

## Examples

``` r
if (FALSE) { # \dontrun{
find_certara_tools("fit a population model from a spec")
find_certara_tools("wait for a running fit to finish", limit = 3)
} # }
```
