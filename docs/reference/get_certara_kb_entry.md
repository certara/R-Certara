# Retrieve a full KB entry by id

Retrieve a full KB entry by id

## Usage

``` r
get_certara_kb_entry(id)
```

## Arguments

- id:

  Stable entry id.

## Value

On a hit, the full entry list with `found = TRUE` prefixed. On a miss,
an explicit `found = FALSE` record carrying a human-readable `message`
and `suggestions` from a relaxed search - never a bare logical, which
serializes ambiguously through MCP (a lone `not_found = TRUE` rendered
as `TRUE`).

## Examples

``` r
get_certara_kb_entry("Certara.RsNLME.pml.stparm")
#> $found
#> [1] TRUE
#> 
#> $id
#> [1] "Certara.RsNLME.pml.stparm"
#> 
#> $package
#> [1] "Certara.RsNLME"
#> 
#> $package_version
#> [1] "3.2.0"
#> 
#> $type
#> [1] "pml_topic"
#> 
#> $title
#> [1] "Structural parameters (stparm)"
#> 
#> $summary
#> [1] "stparm() defines structural parameters as functions of fixed effects (theta) and random effects (eta), choosing the parameter distribution."
#> 
#> $details_md
#> [1] "`stparm` declares the structural parameters. The functional form chosen sets the\neffective distribution of the parameter:\n\n```pml\nstparm(V  = tvV * exp(nV))         # log-normal (positive parameters)\nstparm(Emax = tvEmax + nEmax)      # normal (can be negative)\nstparm(Frac = ilogit(tvFrac + nFrac)) # logit (bounded 0..1)\n```\n\n`tv*` names are fixed effects defined with `fixef`; `n*`/`eta` names are random\neffects defined with `ranef`."
#> 
#> $keywords
#> $keywords[[1]]
#> [1] "stparm"
#> 
#> $keywords[[2]]
#> [1] "structural parameter"
#> 
#> $keywords[[3]]
#> [1] "declare structural parameters"
#> 
#> $keywords[[4]]
#> [1] "define structural parameters"
#> 
#> $keywords[[5]]
#> [1] "theta"
#> 
#> $keywords[[6]]
#> [1] "eta"
#> 
#> $keywords[[7]]
#> [1] "log-normal"
#> 
#> $keywords[[8]]
#> [1] "normal"
#> 
#> $keywords[[9]]
#> [1] "logit"
#> 
#> 
#> $symbols
#> [1] "stparm"
#> 
#> $applies_to
#> $applies_to$engine
#> list()
#> 
#> $applies_to$route
#> list()
#> 
#> $applies_to$model_type
#> $applies_to$model_type[[1]]
#> [1] "pk"
#> 
#> $applies_to$model_type[[2]]
#> [1] "pd"
#> 
#> 
#> 
#> $related
#> $related[[1]]
#> [1] "Certara.RsNLME.pml.fixef"
#> 
#> $related[[2]]
#> [1] "Certara.RsNLME.pml.ranef"
#> 
#> $related[[3]]
#> [1] "Certara.RsNLME.pml.secondary"
#> 
#> 
#> $examples
#> $examples[[1]]
#> $examples[[1]]$code
#> [1] "stparm(V  = tvV * exp(nV))         # log-normal (positive parameters)\nstparm(Emax = tvEmax + nEmax)      # normal (can be negative)\nstparm(Frac = ilogit(tvFrac + nFrac)) # logit (bounded 0..1)"
#> 
#> $examples[[1]]$language
#> [1] "pml"
#> 
#> $examples[[1]]$runnable
#> [1] FALSE
#> 
#> 
#> 
#> $error_signature
#> NULL
#> 
#> $provenance
#> $provenance$source_file
#> [1] "PML grammar (TDL5)"
#> 
#> $provenance$symbol
#> [1] "stparm( (<id> <assign_op> <expr>)+ )"
#> 
#> $provenance$anchor
#> NULL
#> 
#> 
#> $source
#> $source$kind
#> [1] "grammar"
#> 
#> $source$url
#> NULL
#> 
#> 
```
