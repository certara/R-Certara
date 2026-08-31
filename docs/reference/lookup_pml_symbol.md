# Exact PML symbol lookup

Maps a PML keyword/token or function name directly to the entries that
document it, via the generated symbol index.

## Usage

``` r
lookup_pml_symbol(symbol)
```

## Arguments

- symbol:

  Exact symbol (e.g. `"stparm"`, `"dosepoint"`).

## Value

A list of compact entry summaries (possibly empty).

## Examples

``` r
lookup_pml_symbol("stparm")
#> $symbol
#> [1] "stparm"
#> 
#> $matches
#> $matches[[1]]
#> $matches[[1]]$id
#> [1] "Certara.RsNLME.antipattern.qrpem_time_varying_covariate"
#> 
#> $matches[[1]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[1]]$type
#> [1] "anti_pattern"
#> 
#> $matches[[1]]$title
#> [1] "Time-varying covariate effect left inside stparm() under QRPEM"
#> 
#> $matches[[1]]$summary
#> [1] "QRPEM rejects covariate effects inside stparm() when the covariate has more than one value for any subject; move the full covariate factor into a body assignment (split stparm), leaving only typical value and random effect in stparm."
#> 
#> $matches[[1]]$symbols
#> [1] "stparm"       "fcovariate"   "engineParams"
#> 
#> 
#> $matches[[2]]
#> $matches[[2]]$id
#> [1] "Certara.RsNLME.nonmem.overview"
#> 
#> $matches[[2]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[2]]$type
#> [1] "pml_topic"
#> 
#> $matches[[2]]$title
#> [1] "NONMEM-to-PML translation overview and scope"
#> 
#> $matches[[2]]$summary
#> [1] "Translation maps NONMEM control-stream sections to PML statements where a clean analog exists; constructs without one are flagged needs_review rather than guessed, and v1 translates the model only (no data row rewriting)."
#> 
#> $matches[[2]]$symbols
#> [1] "stparm"  "observe"
#> 
#> 
#> $matches[[3]]
#> $matches[[3]]$id
#> [1] "Certara.RsNLME.nonmem.covariates"
#> 
#> $matches[[3]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[3]]$type
#> [1] "pml_topic"
#> 
#> $matches[[3]]$title
#> [1] "Hand-coded $PK covariate effects to structured PML"
#> 
#> $matches[[3]]$summary
#> [1] "Common NONMEM covariate idioms map to PML stparm forms - allometric (weight on CL/V via power of 0.75/1.0), linear, exponential (categorical via exp(theta*flag)), and power - by recognizing the algebra around the typical-value parameter."
#> 
#> $matches[[3]]$symbols
#> [1] "stparm"     "covariate"  "fcovariate"
#> 
#> 
#> $matches[[4]]
#> $matches[[4]]$id
#> [1] "Certara.RsNLME.nonmem.estimation"
#> 
#> $matches[[4]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[4]]$type
#> [1] "pml_topic"
#> 
#> $matches[[4]]$title
#> [1] "$ESTIMATION/$COVARIANCE to engineParams() suggestion"
#> 
#> $matches[[4]]$summary
#> [1] "NONMEM estimation methods map to RsNLME engine methods where an analog exists (FOCE+INTERACTION to FOCE-ELS, FO to FO, ITS to IT2S-EM), $COVARIANCE to a standard-error method, and SAEM/BAYES/NUTS/IMP/DIRECT are flagged needs_review."
#> 
#> $matches[[4]]$symbols
#> [1] "stparm"
#> 
#> 
#> $matches[[5]]
#> $matches[[5]]$id
#> [1] "Certara.RsNLME.pml.authoring.skeleton"
#> 
#> $matches[[5]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[5]]$type
#> [1] "pml_topic"
#> 
#> $matches[[5]]$title
#> [1] "Canonical PML model skeleton and statement order"
#> 
#> $matches[[5]]$summary
#> [1] "A from-scratch PML model follows the generatePMLModel() emission order - structural, error/observe, stparm (residual-error extras such as CMultStdev included), covariate/fcovariate, fixef, ranef, then optional secondary() - so hand-written models diff cleanly against builtins and write_mmdl. The parser itself is order-insensitive outside sequence blocks."
#> 
#> $matches[[5]]$symbols
#> [1] "stparm"    "fixef"     "ranef"     "observe"   "secondary" "covariate"
#> [7] "error"    
#> 
#> 
#> $matches[[6]]
#> $matches[[6]]$id
#> [1] "Certara.RsNLME.pml.authoring.naming"
#> 
#> $matches[[6]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[6]]$type
#> [1] "pml_topic"
#> 
#> $matches[[6]]$title
#> [1] "PML naming conventions (A, C, Obs, eps, tv, n)"
#> 
#> $matches[[6]]$summary
#> [1] "Phoenix-generated PML follows conventional prefixes - A1/Aa for amounts, C/C1 for concentration, *Obs for observations, eps* for residual error, tv* for typical values, n*/eta* for random effects, K* for rate constants - which are conventions, not parser rules."
#> 
#> $matches[[6]]$symbols
#> [1] "stparm"  "error"   "observe"
#> 
#> 
#> $matches[[7]]
#> $matches[[7]]$id
#> [1] "Certara.RsNLME.pml.authoring.builtin_first"
#> 
#> $matches[[7]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[7]]$type
#> [1] "pml_topic"
#> 
#> $matches[[7]]$title
#> [1] "Builtin model constructors vs textualmodel (recommended path)"
#> 
#> $matches[[7]]$summary
#> [1] "For standard linear PK/PD, prefer the RsNLME builtin model constructor (pkmodel/pkemaxmodel/...) which is validated and search-ready; fall back to textualmodel() with hand-written PML only for structures the builtin API cannot express."
#> 
#> $matches[[7]]$symbols
#> [1] "stparm"
#> 
#> 
#> $matches[[8]]
#> $matches[[8]]$id
#> [1] "Certara.RsNLME.pml.stparm"
#> 
#> $matches[[8]]$package
#> [1] "Certara.RsNLME"
#> 
#> $matches[[8]]$type
#> [1] "pml_topic"
#> 
#> $matches[[8]]$title
#> [1] "Structural parameters (stparm)"
#> 
#> $matches[[8]]$summary
#> [1] "stparm() defines structural parameters as functions of fixed effects (theta) and random effects (eta), choosing the parameter distribution."
#> 
#> $matches[[8]]$symbols
#> [1] "stparm"
#> 
#> 
#> 
```
