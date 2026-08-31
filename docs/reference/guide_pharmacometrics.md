# Guidance-first pharmacometrics lookup

The primary entry point for a modeling task. Resolves the intended use
first, then returns a resolved guidance tree in one call - the matching
guidance chapter(s), relevant decisions, the ordered execution steps, a
merged checklist split into auto-verifiable vs human-judgment items, the
linked reference entries, and the related anti-patterns. Summary-first:
it returns chapter summaries and step titles, not full bodies - fetch
those on demand with
[`get_certara_kb_entry()`](https://github.com/certara/R-Certara/reference/get_certara_kb_entry.md).

## Usage

``` r
guide_pharmacometrics(task, intended_use = NULL, limit = 2)
```

## Arguments

- task:

  Free-text description of the modeling task or question.

- intended_use:

  Optional fit-for-purpose level: one of `"exploratory"`,
  `"dose_selection"`, `"labeling"`, `"pediatric"`, `"er_input"`.
  Resolved first; scales which checks and validation depth are surfaced.
  When omitted, the result flags that intended use should be resolved
  before final recommendations.

- limit:

  Maximum guidance chapters to resolve (default 2).

## Value

A resolved guidance tree (see Description).

## Examples

``` r
guide_pharmacometrics("covariate selection", intended_use = "labeling")
#> $intended_use
#> [1] "labeling"
#> 
#> $intended_use_note
#> NULL
#> 
#> $guidance_chapters
#> $guidance_chapters[[1]]
#> $guidance_chapters[[1]]$id
#> [1] "Certara.RsNLME.guidance.analysis_plan"
#> 
#> $guidance_chapters[[1]]$title
#> [1] "Prespecified analysis plan"
#> 
#> $guidance_chapters[[1]]$summary
#> [1] "Before building models, prespecify the objective, candidate structures, covariate list and relationships, selection criteria, outlier rules, BLQ method, and missing-data policy."
#> 
#> $guidance_chapters[[1]]$chapter_order
#> [1] 2
#> 
#> $guidance_chapters[[1]]$guidance_refs
#> [1] "FDA Population PK Guidance (2022) section IV"
#> [2] "EMA reporting guideline (analysis plan)"     
#> 
#> $guidance_chapters[[1]]$conflict
#> NULL
#> 
#> 
#> $guidance_chapters[[2]]
#> $guidance_chapters[[2]]$id
#> [1] "Certara.RsNLME.guidance.covariate_model"
#> 
#> $guidance_chapters[[2]]$title
#> [1] "Covariate model development"
#> 
#> $guidance_chapters[[2]]$summary
#> [1] "Build the covariate model from prespecified, plausible parameter-covariate relationships; prefer stepwise covariate search, judge effects on clinical relevance not only statistics, and avoid collinear covariates."
#> 
#> $guidance_chapters[[2]]$chapter_order
#> [1] 5
#> 
#> $guidance_chapters[[2]]$guidance_refs
#> [1] "FDA Population PK Guidance (2022) section V.B (covariates)"
#> [2] "EMA reporting guideline (covariate analysis)"              
#> [3] "covariate selection review (PMC4294083)"                   
#> 
#> $guidance_chapters[[2]]$conflict
#> NULL
#> 
#> 
#> 
#> $decisions
#> $decisions[[1]]
#> $decisions[[1]]$id
#> [1] "Certara.RsNLME.decision.scm_vs_shotgun"
#> 
#> $decisions[[1]]$type
#> [1] "decision"
#> 
#> $decisions[[1]]$title
#> [1] "Stepwise covariate search vs exhaustive shotgun search"
#> 
#> 
#> $decisions[[2]]
#> $decisions[[2]]$id
#> [1] "Certara.RsNLME.decision.sortfit_vs_scm"
#> 
#> $decisions[[2]]$type
#> [1] "decision"
#> 
#> $decisions[[2]]$title
#> [1] "Batched sortfit prescreen vs stepwise covariate modeling (SCM)"
#> 
#> 
#> $decisions[[3]]
#> $decisions[[3]]$id
#> [1] "Certara.RsNLME.decision.when_to_launch_scm"
#> 
#> $decisions[[3]]$type
#> [1] "decision"
#> 
#> $decisions[[3]]$title
#> [1] "When to launch a covariate search (and when not to)"
#> 
#> 
#> 
#> $steps_resolved
#> $steps_resolved[[1]]
#> $steps_resolved[[1]]$id
#> [1] "Certara.RsNLME.workflow.protocol_to_analysis_plan"
#> 
#> $steps_resolved[[1]]$type
#> [1] "playbook"
#> 
#> $steps_resolved[[1]]$title
#> [1] "Protocol review to a versioned PopPK analysis plan"
#> 
#> 
#> $steps_resolved[[2]]
#> $steps_resolved[[2]]$id
#> [1] "Certara.RsNLME.workflow.pkpd_end_to_end"
#> 
#> $steps_resolved[[2]]$type
#> [1] "playbook"
#> 
#> $steps_resolved[[2]]$title
#> [1] "Default end-to-end PK/PD workflow"
#> 
#> 
#> $steps_resolved[[3]]
#> $steps_resolved[[3]]$id
#> [1] "Certara.RsNLME.workflow.initial_estimates"
#> 
#> $steps_resolved[[3]]$type
#> [1] "playbook"
#> 
#> $steps_resolved[[3]]$title
#> [1] "Get good initial estimates (Naive-Pooled first; NCA for PK)"
#> 
#> 
#> $steps_resolved[[4]]
#> $steps_resolved[[4]]$id
#> [1] "Certara.RsNLME.workflow.covariate_search_planning"
#> 
#> $steps_resolved[[4]]$type
#> [1] "playbook"
#> 
#> $steps_resolved[[4]]$title
#> [1] "Plan and gate a covariate search before launching SCM"
#> 
#> 
#> $steps_resolved[[5]]
#> $steps_resolved[[5]]$id
#> [1] "Certara.RsNLME.fn.validate_covariate_search"
#> 
#> $steps_resolved[[5]]$type
#> [1] "function_doc"
#> 
#> $steps_resolved[[5]]$title
#> [1] "validate_covariate_search() - gate and (opt-in) assemble a covariate search"
#> 
#> 
#> $steps_resolved[[6]]
#> $steps_resolved[[6]]$id
#> [1] "Certara.RsNLME.fn.stepwiseSearch"
#> 
#> $steps_resolved[[6]]$type
#> [1] "function_doc"
#> 
#> $steps_resolved[[6]]$title
#> [1] "stepwiseSearch() - stepwise covariate modeling (SCM)"
#> 
#> 
#> $steps_resolved[[7]]
#> $steps_resolved[[7]]$id
#> [1] "Certara.RsNLME.fn.shotgunSearch"
#> 
#> $steps_resolved[[7]]$type
#> [1] "function_doc"
#> 
#> $steps_resolved[[7]]$title
#> [1] "shotgunSearch() - exhaustive covariate search"
#> 
#> 
#> $steps_resolved[[8]]
#> $steps_resolved[[8]]$id
#> [1] "Certara.RsNLME.workflow.choose_run_mode"
#> 
#> $steps_resolved[[8]]$type
#> [1] "workflow_recipe"
#> 
#> $steps_resolved[[8]]$title
#> [1] "Choose the RsNLME run mode (fit, SCM, bootstrap, VPC, simulation)"
#> 
#> 
#> $steps_resolved[[9]]
#> $steps_resolved[[9]]$id
#> [1] "Certara.RsNLME.fn.covariateModel"
#> 
#> $steps_resolved[[9]]$type
#> [1] "function_doc"
#> 
#> $steps_resolved[[9]]$title
#> [1] "covariateModel() / addCovariate() - add covariate effects"
#> 
#> 
#> $steps_resolved[[10]]
#> $steps_resolved[[10]]$id
#> [1] "Certara.RsNLME.fn.copyModel"
#> 
#> $steps_resolved[[10]]$type
#> [1] "function_doc"
#> 
#> $steps_resolved[[10]]$title
#> [1] "copyModel() - branch a model for the next step"
#> 
#> 
#> 
#> $checks
#> $checks$auto_verifiable
#> $checks$auto_verifiable[[1]]
#> $checks$auto_verifiable[[1]]$id
#> [1] "selection_criterion_applied"
#> 
#> $checks$auto_verifiable[[1]]$text
#> [1] "The prespecified selection criterion (forward/backward thresholds or information criterion) was applied"
#> 
#> $checks$auto_verifiable[[1]]$verifiable_by
#> [1] "fit_health"
#> 
#> 
#> $checks$auto_verifiable[[2]]
#> $checks$auto_verifiable[[2]]$id
#> [1] "frem_covariates_once_per_subject"
#> 
#> $checks$auto_verifiable[[2]]$text
#> [1] "Each continuous covariate contributes one observation per subject"
#> 
#> $checks$auto_verifiable[[2]]$verifiable_by
#> [1] "data_summary"
#> 
#> 
#> 
#> $checks$human_judgment
#> $checks$human_judgment[[1]]
#> $checks$human_judgment[[1]]$id
#> [1] "plan_documented"
#> 
#> $checks$human_judgment[[1]]$text
#> [1] "Objective, candidate models, covariate list, criteria, outlier/BLQ/missing policy are written before modeling"
#> 
#> $checks$human_judgment[[1]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[2]]
#> $checks$human_judgment[[2]]$id
#> [1] "criteria_prespecified"
#> 
#> $checks$human_judgment[[2]]$text
#> [1] "Selection criteria (e.g. -2LL/AIC thresholds, forward/backward p-values) are fixed in advance"
#> 
#> $checks$human_judgment[[2]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[3]]
#> $checks$human_judgment[[3]]$id
#> [1] "log_per_run"
#> 
#> $checks$human_judgment[[3]]$text
#> [1] "Each run records id, parent, change, method, key estimates, objective/AIC, convergence, and warnings"
#> 
#> $checks$human_judgment[[3]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[4]]
#> $checks$human_judgment[[4]]$id
#> [1] "decisions_logged"
#> 
#> $checks$human_judgment[[4]]$text
#> [1] "Covariate and structural decisions record the rationale and the criterion used"
#> 
#> $checks$human_judgment[[4]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[5]]
#> $checks$human_judgment[[5]]$id
#> [1] "outlier_rule_prespecified"
#> 
#> $checks$human_judgment[[5]]$text
#> [1] "The outlier-handling rule was prespecified, not decided after seeing fits"
#> 
#> $checks$human_judgment[[5]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[6]]
#> $checks$human_judgment[[6]]$id
#> [1] "exclusion_documented"
#> 
#> $checks$human_judgment[[6]]$text
#> [1] "Any excluded record/subject is documented with a reason"
#> 
#> $checks$human_judgment[[6]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[7]]
#> $checks$human_judgment[[7]]$id
#> [1] "exclusion_sensitivity"
#> 
#> $checks$human_judgment[[7]]$text
#> [1] "The impact of exclusion is assessed (fit with and without)"
#> 
#> $checks$human_judgment[[7]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[8]]
#> $checks$human_judgment[[8]]$id
#> [1] "covariates_prespecified"
#> 
#> $checks$human_judgment[[8]]$text
#> [1] "Tested covariate-parameter relationships were prespecified and are plausible"
#> 
#> $checks$human_judgment[[8]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[9]]
#> $checks$human_judgment[[9]]$id
#> [1] "search_explicitly_approved"
#> 
#> $checks$human_judgment[[9]]$text
#> [1] "The covariate search (candidate pairs, functional forms, criteria, host) was planned and explicitly approved before launch, not started silently"
#> 
#> $checks$human_judgment[[9]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[10]]
#> $checks$human_judgment[[10]]$id
#> [1] "clinical_relevance"
#> 
#> $checks$human_judgment[[10]]$text
#> [1] "Retained covariates are clinically relevant, not only statistically significant"
#> 
#> $checks$human_judgment[[10]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[11]]
#> $checks$human_judgment[[11]]$id
#> [1] "no_collinear_pair"
#> 
#> $checks$human_judgment[[11]]$text
#> [1] "Strongly correlated covariates are not both retained on the same parameter"
#> 
#> $checks$human_judgment[[11]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[12]]
#> $checks$human_judgment[[12]]$id
#> [1] "relationships_prespecified"
#> 
#> $checks$human_judgment[[12]]$text
#> [1] "The parameter-covariate relationships and forms are prespecified and plausible"
#> 
#> $checks$human_judgment[[12]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[13]]
#> $checks$human_judgment[[13]]$id
#> [1] "multiplicity_controlled"
#> 
#> $checks$human_judgment[[13]]$text
#> [1] "The number of tested effects is bounded (no exhaustive search over many covariates)"
#> 
#> $checks$human_judgment[[13]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[14]]
#> $checks$human_judgment[[14]]$id
#> [1] "frem_covariate_sigma_frozen"
#> 
#> $checks$human_judgment[[14]]$text
#> [1] "Covariate residual error terms are frozen near zero unless covariate error is modeled deliberately"
#> 
#> $checks$human_judgment[[14]]$verifiable_by
#> [1] "human"
#> 
#> 
#> $checks$human_judgment[[15]]
#> $checks$human_judgment[[15]]$id
#> [1] "frem_omega_block_present"
#> 
#> $checks$human_judgment[[15]]$text
#> [1] "PK and covariate etas share one omega block so the covariate effects can be recovered"
#> 
#> $checks$human_judgment[[15]]$verifiable_by
#> [1] "human"
#> 
#> 
#> 
#> 
#> $reference
#>  [1] "Certara.RsNLME.workflow.protocol_to_analysis_plan"
#>  [2] "Certara.RsNLME.workflow.pkpd_end_to_end"          
#>  [3] "Certara.RsNLME.workflow.initial_estimates"        
#>  [4] "Certara.RsNLME.workflow.covariate_search_planning"
#>  [5] "Certara.RsNLME.fn.validate_covariate_search"      
#>  [6] "Certara.RsNLME.fn.stepwiseSearch"                 
#>  [7] "Certara.RsNLME.fn.shotgunSearch"                  
#>  [8] "Certara.RsNLME.fn.covariateModel"                 
#>  [9] "Certara.RsNLME.fn.copyModel"                      
#> [10] "Certara.RsNLME.pml.covariate"                     
#> 
#> $anti_patterns
#> [1] "Certara.RsNLME.antipattern.bad_initial_estimates"
#> 
```
