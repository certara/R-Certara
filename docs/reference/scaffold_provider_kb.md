# Scaffold a provider package's KB sources

Creates `inst/mcp/kb/sources/` with a format README and a starter source
file in a provider package source tree, so the provider can author cited
markdown and run
[`generate_certara_kb()`](https://github.com/certara/R-Certara/reference/generate_certara_kb.md)
to ship its own discoverable KB slice.

## Usage

``` r
scaffold_provider_kb(pkg_root, package, overwrite = FALSE)
```

## Arguments

- pkg_root:

  Path to the provider package source root.

- package:

  Provider package name (stamped on entries).

- overwrite:

  Overwrite an existing starter file.

## Value

Invisibly the sources directory path.

## Examples

``` r
if (FALSE) { # \dontrun{
scaffold_provider_kb(pkg_root = ".", package = "tidyvpc")
} # }
```
