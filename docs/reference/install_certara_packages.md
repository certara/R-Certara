# Install Certara packages

A user friendly wrapper to easily install R packages and Shiny
applications developed by Certara

## Usage

``` r
install_certara_packages(
  CRAN_repo = "https://cloud.r-project.org",
  clean_install = FALSE,
  quiet = FALSE
)
```

## Arguments

- CRAN_repo:

  Character; The URL of a CRAN mirror which defaults to
  `"https://cloud.r-project.org"`

- clean_install:

  Logical; Set to `TRUE` to remove all existing Certara packages prior
  to installing

- quiet:

  Logical; Set to `TRUE` to suppress installation messages and reduce
  console output

## Value

`TRUE` if all packages successfully install

## Examples

``` r
if (FALSE) { # \dontrun{
install_certara_packages()
} # }
```
