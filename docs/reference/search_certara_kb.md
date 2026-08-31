# Search the Certara knowledge base

Deterministic offline BM25 keyword search over titles, summaries,
keywords, symbols, and details, with optional package/type/scope
filters.

## Usage

``` r
search_certara_kb(
  query,
  package = NULL,
  type = NULL,
  model_type = NULL,
  route = NULL,
  engine = NULL,
  axis = NULL,
  limit = 5
)
```

## Arguments

- query:

  Free-text query.

- package:

  Optional provider package filter.

- type:

  Optional entry-type filter.

- model_type, route, engine:

  Optional `applies_to` scope filters.

- axis:

  Optional retrieval-axis filter (`"guidance"`, `"reference"`,
  `"remediation"`). When set, results are ranked with a guidance boost
  so the bible surfaces before raw reference.

- limit:

  Maximum results (default 5).

## Value

A list of compact entry summaries with scores.

## Examples

``` r
search_certara_kb("two compartment oral absorption")
#> [[1]]
#> [[1]]$id
#> [1] "Certara.RsNLME.guidance.topic.worked_example_oral_1cpt"
#> 
#> [[1]]$package
#> [1] "Certara.RsNLME"
#> 
#> [[1]]$type
#> [1] "guidance_topic"
#> 
#> [[1]]$title
#> [1] "Worked example - complete population PK analysis (oral 1-compartment)"
#> 
#> [[1]]$summary
#> [1] "A complete end-to-end population PK example threading every guidance chapter - oral one-compartment model, sparse sampling, BLQ present, body weight on CL/V and renal function on CL - illustrative only, no fabricated numbers."
#> 
#> [[1]]$score
#> [1] 12.0751
#> 
#> 
#> [[2]]
#> [[2]]$id
#> [1] "Certara.RsNLME.fn.pkmodel"
#> 
#> [[2]]$package
#> [1] "Certara.RsNLME"
#> 
#> [[2]]$type
#> [1] "function_doc"
#> 
#> [[2]]$title
#> [1] "pkmodel() - build a PK model"
#> 
#> [[2]]$summary
#> [1] "pkmodel() constructs a built-in PK model object choosing compartments, absorption, elimination, and parameterization."
#> 
#> [[2]]$symbols
#> [1] "pkmodel"
#> 
#> [[2]]$score
#> [1] 11.381
#> 
#> 
#> [[3]]
#> [[3]]$id
#> [1] "Certara.RsNLME.example.warfpk"
#> 
#> [[3]]$package
#> [1] "Certara.RsNLME"
#> 
#> [[3]]$type
#> [1] "example"
#> 
#> [[3]]$title
#> [1] "Worked example - warfarin first-order oral population PK end to end via MCP"
#> 
#> [[3]]$summary
#> [1] "A complete MCP-only warfarin PK workflow - inspect the dataset, validate and fit a 1-compartment first-order oral model, add prespecified covariates, compare on AIC/BIC, and run a VPC - reproducing the AIC-vs-BIC covariate trade-off without leaving the MCP tools."
#> 
#> [[3]]$symbols
#> [1] "inspect_pk_dataset"  "validate_fit_spec"   "start_nlme_fit_spec"
#> [4] "start_nlme_fitmodel" "compare_nlme_jobs"   "start_nlme_vpcmodel"
#> 
#> [[3]]$score
#> [1] 11.0242
#> 
#> 
#> [[4]]
#> [[4]]$id
#> [1] "Certara.RsNLME.pml.authoring.compartment_map"
#> 
#> [[4]]$package
#> [1] "Certara.RsNLME"
#> 
#> [[4]]$type
#> [1] "pml_topic"
#> 
#> [[4]]$title
#> [1] "How PML statements create compartments (no compartment keyword)"
#> 
#> [[4]]$summary
#> [1] "PML has no compartment keyword - compartments are created implicitly by deriv, urinecpt, transit, delayInfCpt, cfMicro, and cfMacro*; a depot is just a compartment that has a dosepoint plus an outflow into the central compartment."
#> 
#> [[4]]$symbols
#> [1] "deriv"     "dosepoint" "cfMicro"   "transit"   "urinecpt" 
#> 
#> [[4]]$score
#> [1] 10.7355
#> 
#> 
#> [[5]]
#> [[5]]$id
#> [1] "Certara.RsNLME.pml.calctmax"
#> 
#> [[5]]$package
#> [1] "Certara.RsNLME"
#> 
#> [[5]]$type
#> [1] "pml_topic"
#> 
#> [[5]]$title
#> [1] "CalcTMax — closed-form time of maximum concentration"
#> 
#> [[5]]$summary
#> [1] "CalcTMax returns the time of peak concentration for multi-exponential PK; accepted arities are 3, 5, 6, and 7, and the legacy 6-argument form takes Ka (not Gamma) as its last argument."
#> 
#> [[5]]$symbols
#> [1] "CalcTMax"
#> 
#> [[5]]$score
#> [1] 10.0057
#> 
#> 
```
