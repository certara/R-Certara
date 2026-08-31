# Certara MCP server capabilities

Early-signal contract for agents: server/schema versions, discovered KB
and tool providers, concurrency policy, gated tool names, the attached
ecosystem, and the merged behavior rules (generic host rules plus
provider fragments).

## Usage

``` r
certara_mcp_capabilities(dev_roots = character(0))
```

## Arguments

- dev_roots:

  Optional dev source-tree roots for provider discovery.

## Value

A structured capabilities list.

## Examples

``` r
cap <- certara_mcp_capabilities()
cap$server
#> [1] "certara-r"
names(cap$rules)
#>  [1] "providers_first"                     "repro_script"                       
#>  [3] "report_rmd"                          "vpc_two_step"                       
#>  [5] "capability_gap"                      "evidence_tiers"                     
#>  [7] "memory_and_sources"                  "nlme_only"                          
#>  [9] "prereqs_first"                       "two_routes_one_contract"            
#> [11] "direct_authoring_is_first_class"     "no_silent_large_search"             
#> [13] "fitness_vs_acceptance"               "handoff_to_rsnlme"                  
#> [15] "candidates_not_nested"               "candidate_recovery_not_fabricated"  
#> [17] "ofv_not_fitness_for_likelihood_work" "opt_in_criteria_never_automatic"    
#> [19] "certara_first"                       "mcp_preflight"                      
#> [21] "guidance_first"                      "model_staging"                      
#> [23] "blq_gating"                          "scm_gating"                         
#> [25] "project_layout"                      "pirana_overlay"                     
#> [27] "sequential_lrt_gating"               "xpose_create_first"                 
#> [29] "xpose_repro_script"                  "xpose_eta_shrinkage"                
#> [31] "yobs_ysim_not_y"                     "plot_not_vpc_plot"                  
#> [33] "stratify_covariates"                 "tidyvpc_repro_script"               
```
