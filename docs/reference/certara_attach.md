# Attach the Certara pharmacometrics ecosystem

Attaches the installed member packages returned by
[`certara_core_packages()`](https://github.com/certara/R-Certara/reference/certara_core_packages.md)
so a single [`library(Certara.R)`](https://github.com/certara/R-Certara)
brings the suite onto the search path. Missing packages are skipped
rather than raising an error.

## Usage

``` r
certara_attach()
```

## Value

Invisibly, a named logical vector indicating which packages were
attached.

## Examples

``` r
if (FALSE) { # \dontrun{
certara_attach()
} # }
```
