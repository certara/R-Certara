# List all packages developed by Certara

Returns the membership of the Certara pharmacometrics R ecosystem. This
is the attach manifest consulted by
[`certara_attach()`](https://github.com/certara/R-Certara/reference/certara_attach.md);
it is distinct from the set of MCP providers, which is discovered at
runtime from installed packages that ship an `inst/mcp/` slice.

## Usage

``` r
certara_packages(
  include_self = FALSE,
  include_github = TRUE,
  include_JFROG_deps = FALSE
)
```

## Arguments

- include_self:

  Logical; Set to `TRUE` to include `Certara.R`

- include_github:

  Logical; Set to `TRUE` to include packages only available on GitHub

- include_JFROG_deps:

  Logical; Set to `TRUE` to include Certara package dependencies on
  Jfrog

## Value

Character vector of package names

## Examples

``` r
certara_packages()
#>  [1] "Certara.RsNLME"               "Certara.Xpose.NLME"          
#>  [3] "Certara.RsNLME.ModelExecutor" "Certara.ModelResults"        
#>  [5] "Certara.VPCResults"           "Certara.RsNLME.ModelBuilder" 
#>  [7] "Certara.RDarwin"              "Certara.DarwinReporter"      
#>  [9] "tidyvpc"                      "ggquickeda"                  
#> [11] "coveffectsplot"               "table1c"                     
#> [13] "ggcertara"                   
```
