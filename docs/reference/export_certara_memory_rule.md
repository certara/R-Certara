# Export active memory as a client rule file

Materializes active hard preferences and high-priority (corrective)
lessons into a client-loaded rule file so defaults auto-apply without a
tool call. Only writes when the user invokes it - never an
automatic/surprise write.

## Usage

``` r
export_certara_memory_rule(path = NULL, context = NULL)
```

## Arguments

- path:

  Destination rule file. Defaults to `.cursor/rules/certara-memory.md`.

- context:

  Optional scope tag to include alongside global records.

## Value

Invisibly the written path.

## Examples

``` r
if (FALSE) { # \dontrun{
export_certara_memory_rule()
} # }
```
