# Core ecosystem packages attached by `library(Certara.R)`

The attach-time subset of
[`certara_packages()`](https://github.com/certara/R-Certara/reference/certara_packages.md):
the JFrog/CRAN-installable Certara suite, excluding GitHub-only
packages. Packages that are not installed are skipped gracefully by
[`certara_attach()`](https://github.com/certara/R-Certara/reference/certara_attach.md).

## Usage

``` r
certara_core_packages()
```

## Value

Character vector of package names.

## Examples

``` r
certara_core_packages()
#>  [1] "Certara.RsNLME"               "Certara.Xpose.NLME"          
#>  [3] "Certara.RsNLME.ModelExecutor" "Certara.ModelResults"        
#>  [5] "Certara.VPCResults"           "Certara.RsNLME.ModelBuilder" 
#>  [7] "Certara.RDarwin"              "Certara.DarwinReporter"      
#>  [9] "tidyvpc"                      "ggquickeda"                  
#> [11] "coveffectsplot"              
```
