# Check Certara package versions

Check Certara package versions

## Usage

``` r
check_certara_package_versions(pkgs)
```

## Arguments

- pkgs:

  Character or character vector of package names. If missing, defaults
  to
  [`certara_packages()`](https://github.com/certara/R-Certara/reference/certara_packages.md)

## Value

Named character vector indicating package version, `NA` is returned if
Certara package is not installed

## Examples

``` r
if (FALSE) { # \dontrun{
check_certara_package_versions()
} # }
```
