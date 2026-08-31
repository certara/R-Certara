# Current KB schema version

Semantic version of the knowledge-base entry/manifest contract
implemented by this build. Providers declare the `schema_version` they
were generated against in their manifest; the server uses
[`.kb_schema_compatible()`](https://github.com/certara/R-Certara/reference/dot-kb_schema_compatible.md)
to decide whether to load them.

## Usage

``` r
kb_schema_version()
```

## Value

A length-one character version string.

## Examples

``` r
kb_schema_version()
#> [1] "1.0.0"
```
