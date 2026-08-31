# Find runnable/illustrative examples in the KB

Find runnable/illustrative examples in the KB

## Usage

``` r
find_certara_examples(query, package = NULL, limit = 5)
```

## Arguments

- query:

  Free-text query.

- package:

  Optional provider package filter.

- limit:

  Maximum results.

## Value

A list of `(id, title, examples)` records; respects each example's
`runnable` flag.

## Examples

``` r
find_certara_examples("residual error model")
#> [[1]]
#> [[1]]$id
#> [1] "Certara.RsNLME.pml.error"
#> 
#> [[1]]$title
#> [1] "Error model (error / observe)"
#> 
#> [[1]]$examples
#> [[1]]$examples[[1]]
#> [[1]]$examples[[1]]$code
#> [1] "error(CEps = 0.1)\nobserve(CObs = C * (1 + CEps))          # multiplicative (proportional)\nobserve(CObs = C + CEps)                # additive\n# combined (additive + proportional) - ONE epsilon, not two:\nobserve(CObs = C + CEps * sqrt(1 + C^2 * (CMultStdev/sigma())^2))\nstparm(CMultStdev = tvCMultStdev)\nfixef(tvCMultStdev = c(, 1, ))"
#> 
#> [[1]]$examples[[1]]$language
#> [1] "pml"
#> 
#> [[1]]$examples[[1]]$runnable
#> [1] FALSE
#> 
#> 
#> [[1]]$examples[[2]]
#> [[1]]$examples[[2]]$code
#> [1] "error(CEps = 0.1)\nobserve(CObs = C * exp(CEps))"
#> 
#> [[1]]$examples[[2]]$language
#> [1] "pml"
#> 
#> [[1]]$examples[[2]]$runnable
#> [1] FALSE
#> 
#> 
#> 
#> 
#> [[2]]
#> [[2]]$id
#> [1] "Certara.RsNLME.antipattern.observe_multi_error"
#> 
#> [[2]]$title
#> [1] "Multiple residual-error epsilons in one observe()"
#> 
#> [[2]]$examples
#> [[2]]$examples[[1]]
#> [[2]]$examples[[1]]$code
#> [1] "# WRONG - two epsilons in one observe(); rejected at TDL5 translation on a\n# current engine, and at model compile on older engines either way\nerror(CEps1 = 1)\nerror(CEps2 = 0.1)\nobserve(CObs = C * CEps1 + CEps2)\n\n# CORRECT - single epsilon + multiplicative-stdev parameter (combined error)\nerror(CEps = 0.1)\nobserve(CObs = C + CEps * sqrt(1 + C^2 * (CMultStdev/sigma())^2))\nstparm(CMultStdev = tvCMultStdev)\nfixef(tvCMultStdev = c(, 1, ))"
#> 
#> [[2]]$examples[[1]]$language
#> [1] "pml"
#> 
#> [[2]]$examples[[1]]$runnable
#> [1] FALSE
#> 
#> 
#> 
#> 
#> [[3]]
#> [[3]]$id
#> [1] "Certara.RsNLME.antipattern.ltbs_lost_on_second_endpoint"
#> 
#> [[3]]$title
#> [1] "Log-additive observe text silently loses LTBS when a second residual error exists"
#> 
#> [[3]]$examples
#> [[3]]$examples[[1]]
#> [[3]]$examples[[1]]$code
#> [1] "# FRAGILE - LTBS while this is the only error()\nerror(CEps = 0.1)\nobserve(CObs = C * exp(CEps))\n\n# After adding a second endpoint, the SAME CObs line is no longer LTBS:\nerror(CEps = 0.1)\nerror(EEps = 0.1)\nobserve(CObs = C * exp(CEps))   # ordinary multiplicative, not LTBS\nobserve(EObs = Resp + EEps)"
#> 
#> [[3]]$examples[[1]]$language
#> [1] "pml"
#> 
#> [[3]]$examples[[1]]$runnable
#> [1] FALSE
#> 
#> 
#> 
#> 
#> [[4]]
#> [[4]]$id
#> [1] "Certara.RsNLME.pml.vcvfixef"
#> 
#> [[4]]$title
#> [1] "Fixed-effect covariance for simulation (vcvfixef)"
#> 
#> [[4]]$examples
#> [[4]]$examples[[1]]
#> [[4]]$examples[[1]]$code
#> [1] "fixef(tvCl = c(, 0.7, ))\nfixef(tvV  = c(, 5, ))\n# correlated (lower triangle):\nvcvfixef(block(tvCl, tvV) = c(0.01, 0.0006, 0.0002))\n# or independent (do not combine with the block form for the same names):\n# vcvfixef(diag(tvCl, tvV) = c(0.01, 0.0002))"
#> 
#> [[4]]$examples[[1]]$language
#> [1] "pml"
#> 
#> [[4]]$examples[[1]]$runnable
#> [1] FALSE
#> 
#> 
#> 
#> 
#> [[5]]
#> [[5]]$id
#> [1] "Certara.Xpose.NLME.fn.get_summaryNlme"
#> 
#> [[5]]$title
#> [1] "get_summaryNlme - parameter summary table"
#> 
#> [[5]]$examples
#> [[5]]$examples[[1]]
#> [[5]]$examples[[1]]$code
#> [1] "get_summaryNlme(xpdb, shrinkage = \"engine\")"
#> 
#> [[5]]$examples[[1]]$language
#> [1] "r"
#> 
#> [[5]]$examples[[1]]$runnable
#> [1] FALSE
#> 
#> 
#> 
#> 
```
