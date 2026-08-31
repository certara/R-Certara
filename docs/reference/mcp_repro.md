# Reproducible-script recorder

A host-owned, provider-agnostic recorder that accumulates the exact
runnable R code executed by MCP tools into a single `.R` script per
server session. Host tools and provider tools both append to it, so
re-running the script reproduces the MCP-driven analysis for QC / audit.

## Usage

``` r
mcp_repro_reset()

mcp_repro_path(path = NULL)

mcp_repro_record(code, libraries = character())

mcp_repro_read()

mcp_repro_info()

mcp_repro_sym(x)

mcp_repro_call(fn, args = list(), var = NULL)
```

## Arguments

- path:

  New script path (`mcp_repro_path`); setting it resets contents.

- code:

  Character vector of runnable R code (`mcp_repro_record`).

- libraries:

  Package names to attach once at the top of the script.

- x:

  Value to mark as a verbatim symbol (`mcp_repro_sym`).

- fn:

  Function name to render (`mcp_repro_call`).

- args:

  Named list of argument values to render. `NULL` values are dropped; an
  unnamed element is rendered positionally.

- var:

  Optional variable name to assign the rendered call to.

## Value

`mcp_repro_path` returns the active path; `mcp_repro_read` the script
text; `mcp_repro_info` a list of `path` + `contents`; `mcp_repro_call` a
code string; others are called for their side effect.

## Examples

``` r
mcp_repro_reset()
mcp_repro_record(mcp_repro_call("lm", list(formula = mcp_repro_sym("y ~ x")),
                                var = "fit"))
cat(mcp_repro_read())
#> # Reproducible R script recorded by Certara MCP tools.
#> # Re-run top to bottom to reproduce the MCP-driven analysis.
#> 
#> fit <- lm(formula = y ~ x)
```
