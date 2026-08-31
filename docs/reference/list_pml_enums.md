# List controlled PML vocabularies (enums)

List controlled PML vocabularies (enums)

## Usage

``` r
list_pml_enums(kind = NULL)
```

## Arguments

- kind:

  Optional enum name (e.g. `"error_model"`). `NULL` returns all.

## Value

A named list of allowed values.

## Examples

``` r
list_pml_enums()
#> $param_style
#> [1] "log-normal" "normal"     "logit"     
#> 
#> $error_model
#> [1] "additive"       "multiplicative" "combined"       "power"         
#> [5] "log-additive"  
#> 
#> $absorption
#> [1] "intravenous"      "first-order"      "gamma"            "weibull"         
#> [5] "inverse-gaussian"
#> 
#> $elimination
#> [1] "linear"                       "michaelis-menten"            
#> [3] "linear-plus-michaelis-menten"
#> 
#> $distribution
#>  [1] "normal"      "lognorm"     "beta"        "betamean"    "binomial"   
#>  [6] "chisq"       "constant"    "exponential" "gamma"       "invgamma"   
#> [11] "logistic"    "negbin"      "poisson"     "studentt"    "uniform"    
#> [16] "weibull"     "MVN"         "MVT"        
#> 
```
