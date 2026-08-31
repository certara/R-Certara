# List discovered KB provider packages

List discovered KB provider packages

## Usage

``` r
list_certara_kb_packages()
```

## Value

A list with `packages` (loaded providers) and `skipped`
(incompatible/invalid manifests with reasons).

## Examples

``` r
list_certara_kb_packages()
#> $packages
#> $packages[[1]]
#> $packages[[1]]$package
#> [1] "Certara.R"
#> 
#> $packages[[1]]$package_version
#> [1] "2.0.0"
#> 
#> $packages[[1]]$schema_version
#> [1] "1.0.0"
#> 
#> $packages[[1]]$entry_count
#> [1] 2
#> 
#> $packages[[1]]$engine
#> NULL
#> 
#> 
#> $packages[[2]]
#> $packages[[2]]$package
#> [1] "Certara.R"
#> 
#> $packages[[2]]$package_version
#> [1] "2.0.0"
#> 
#> $packages[[2]]$schema_version
#> [1] "1.0.0"
#> 
#> $packages[[2]]$entry_count
#> [1] 2
#> 
#> $packages[[2]]$engine
#> NULL
#> 
#> 
#> $packages[[3]]
#> $packages[[3]]$package
#> [1] "Certara.RDarwin"
#> 
#> $packages[[3]]$package_version
#> [1] "1.2.0"
#> 
#> $packages[[3]]$schema_version
#> [1] "1.0.0"
#> 
#> $packages[[3]]$entry_count
#> [1] 13
#> 
#> $packages[[3]]$engine
#> NULL
#> 
#> 
#> $packages[[4]]
#> $packages[[4]]$package
#> [1] "Certara.RsNLME"
#> 
#> $packages[[4]]$package_version
#> [1] "3.2.0"
#> 
#> $packages[[4]]$schema_version
#> [1] "1.0.0"
#> 
#> $packages[[4]]$entry_count
#> [1] 194
#> 
#> $packages[[4]]$engine
#> $packages[[4]]$engine$tdl5_version
#> [1] "source-tree"
#> 
#> $packages[[4]]$engine$grammar_source
#> [1] "PML grammar (TDL5)"
#> 
#> $packages[[4]]$engine$audit_date
#> [1] "2026-08-19"
#> 
#> 
#> 
#> $packages[[5]]
#> $packages[[5]]$package
#> [1] "Certara.Xpose.NLME"
#> 
#> $packages[[5]]$package_version
#> [1] "2.1.0"
#> 
#> $packages[[5]]$schema_version
#> [1] "1.0.0"
#> 
#> $packages[[5]]$entry_count
#> [1] 17
#> 
#> $packages[[5]]$engine
#> NULL
#> 
#> 
#> $packages[[6]]
#> $packages[[6]]$package
#> [1] "tidyvpc"
#> 
#> $packages[[6]]$package_version
#> [1] "1.6.0"
#> 
#> $packages[[6]]$schema_version
#> [1] "1.0.0"
#> 
#> $packages[[6]]$entry_count
#> [1] 18
#> 
#> $packages[[6]]$engine
#> NULL
#> 
#> 
#> 
#> $skipped
#> list()
#> 
```
