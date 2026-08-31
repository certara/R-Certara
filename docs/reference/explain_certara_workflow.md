# Explain a Certara workflow for a task

Returns the best-matching execution recipes (ordered) - the place to
look for a step-by-step recipe. Matches both `playbook` (the
guidance-linked recipes) and legacy `workflow_recipe` entries. For the
guidance reasoning behind a task (what to verify and why), call
[`guide_pharmacometrics()`](https://github.com/certara/R-Certara/reference/guide_pharmacometrics.md)
instead.

## Usage

``` r
explain_certara_workflow(task, packages = NULL, limit = 3)
```

## Arguments

- task:

  Free-text description of the modeling task.

- packages:

  Optional vector of provider packages to restrict to.

- limit:

  Maximum recipes.

## Value

A list of workflow entries (full details).

## Examples

``` r
explain_certara_workflow("fit a base PK model")
#> [[1]]
#> [[1]]$id
#> [1] "Certara.RsNLME.workflow.pkpd_end_to_end"
#> 
#> [[1]]$title
#> [1] "Default end-to-end PK/PD workflow"
#> 
#> [[1]]$summary
#> [1] "Recommended order - EDA, base structural model selection, covariate search, final fit, GOF and VPC - each in its own run folder."
#> 
#> [[1]]$details_md
#> [1] "A typical RsNLME modeling sequence (PK, PD, or PK/PD - the steps are the same\nfor every model family, only the constructor changes):\n\n1. Exploratory data analysis of the dataset (doses, observations, covariates).\n2. Pick a structural model: choose a built-in constructor that matches\n   (`pkmodel`, `pklinearmodel`, `pkindirectmodel`, `pkemaxmodel`, `emaxmodel`,\n   `linearmodel` - see `list_builtin_model_constructors`) or, when no built-in\n   template fits the requested structure, build custom PML with `textualmodel()`\n   after `validate_pml()`. Map columns inline or with `colMapping()`.\n   Optionally add derived reporting parameters with `addSecondary()` on the\n   builtin model object (e.g. after `pkmodel(...)`: half-lives, micro/macro\n   bridges, `Vss`) before fitting — secondaries are computed at fit time and\n   cannot be added to an existing fit. Prefer fixef-only expressions; see\n   `Certara.RsNLME.pml.secondary` and `Certara.RsNLME.pml.secondary_catalog`.\n3. Fit each candidate with `fitmodel()`; compare on -2LL/AIC. Use sensible\n   initials (see `workflow.initial_estimates`). The result is the **structural\n   base model**: IIV and residual error, no estimated covariate effects (fixed\n   structural scaling/required design effects aside). A reference/literature\n   model with covariates is not the base - see `guidance.topic.model_stages`.\n4. Begin the **covariate model**: branch the settled base with `copyModel()` and\n   add covariate effects via `addCovariate()`/`covariateModel()`.\n5. Run `stepwiseSearch()` (or `shotgunSearch()`) for covariate selection - see\n   `workflow.choose_run_mode` for when each applies.\n6. Final fit; assess GOF with `Certara.Xpose.NLME` - build an xpdb\n   (`xposeNlme(dir = run_dir)`), then GOF plots + `get_summaryNlme()`, see\n   `workflow.gof_xpose` - and run `vpcmodel()` + tidyvpc. Optionally `bootstrap()`\n   for parameter uncertainty, summarized with `get_bootSummaryNlme()` (see\n   `workflow.bootstrap_summary_xpose`).\n\nFor headless/agent use, build the model and fit in one job with\n`start_nlme_fit_spec`, or iterate with `start_nlme_fitmodel`; keep each step in\nits own working directory for reproducibility."
#> 
#> [[1]]$parent
#> [1] "Certara.RsNLME.guidance.toc"
#> 
#> [[1]]$steps
#> NULL
#> 
#> [[1]]$related
#> [1] "Certara.RsNLME.fn.pkmodel"                      
#> [2] "Certara.RsNLME.fn.stepwiseSearch"               
#> [3] "Certara.RsNLME.fn.vpcmodel"                     
#> [4] "Certara.RsNLME.workflow.gof_xpose"              
#> [5] "Certara.RsNLME.workflow.bootstrap_summary_xpose"
#> 
#> 
#> [[2]]
#> [[2]]$id
#> [1] "Certara.RsNLME.mcp.bundled_scripts"
#> 
#> [[2]]$title
#> [1] "Bundled MCP job scripts (compare, SCM, data prep, VPC, bootstrap, simulation)"
#> 
#> [[2]]$summary
#> [1] "The package ships ready-to-run R scripts under inst/mcp/scripts for multi-step workflows; run them with start_nlme_job(file = ...) instead of hand-writing R, passing inputs via environment variables."
#> 
#> [[2]]$details_md
#> [1] "Prefer these bundled scripts over writing R by hand. Run with\n`start_nlme_job(file = \"<script>.R\")`; each script reads its inputs from\nenvironment variables and writes artifacts under `<RUN_DIR>/artifacts`.\n\n| Script | Purpose | Key inputs (env vars) |\n|--------|---------|-----------------------|\n| `examples/prepare_pk_data.R` | Clean/assemble a PK dataset (writes a prepared CSV) | dataset path + mapping vars |\n| `compare_models.R` | Compare two fits on -2LL/AIC/BIC | `FIT_RDS_A`, `FIT_RDS_B` |\n| `scm_stepwise.R` | Stepwise covariate search (forward add + backward eliminate) | base model + covariate scenarios |\n| `scm_shotgun.R` | Exhaustive all-combinations covariate search (grows as 2^n) | base model + covariate scenarios |\n| `covariate_grid.R` | Fit a small, prespecified grid of covariate models and rank them | `GRID_SPEC` (JSON: `base` fit-spec + named `recipes`) |\n| `vpc.R` | Visual predictive check from a fit | `FIT_RDS` |\n| `bootstrap.R` | Bootstrap a final model for parameter uncertainty | `MODEL_RDS` (the fitted model, e.g. `fit$model`), `BOOT_PARAMS_RDS` |\n| `simulation.R` | Simulate from a model | model + simulation spec |\n| `population_fit.R`, `naive_pooled_fit.R` | Single-model fits | model + method |\n\nNotes:\n\n- Prefer the first-class `start_nlme_bootstrap` tool over running `bootstrap.R`\n  directly: it always reroots into its own run sandbox (see artifact-safety\n  guidance) and reads the same `MODEL_RDS` input, so the parent fit's\n  `dmp.txt`/`residuals.csv` are never at risk of being overwritten.\n- `compare_models.R` reports the numbers only; a likelihood-ratio comparison is\n  valid for nested models on the same data, while non-nested candidates are\n  compared on AIC/BIC. The newer `compare_nlme_jobs` tool ranks several jobs at\n  once and is the preferred entry point for multi-model comparison.\n- `covariate_grid.R` fits an explicit, prespecified set of covariate models\n  (e.g. base, weight-only, full) so the warfpk-style comparison runs without\n  writing R; it is NOT a search. For covariate selection across many candidate\n  effects use `scm_stepwise.R`. The fit-spec `covariates` array is for a single\n  prespecified model.\n- `prepare_pk_data.R` is the explicit data-writing step; read-only inspection is\n  `inspect_pk_dataset`, which never writes files."
#> 
#> [[2]]$parent
#> NULL
#> 
#> [[2]]$steps
#> NULL
#> 
#> [[2]]$related
#> [1] "Certara.RsNLME.workflow.choose_run_mode"
#> [2] "Certara.RsNLME.fn.start_nlme_fit_spec"  
#> [3] "Certara.RsNLME.guidance.data_eda"       
#> [4] "Certara.RsNLME.guidance.covariate_model"
#> 
#> 
#> [[3]]
#> [[3]]$id
#> [1] "Certara.RsNLME.workflow.fda_poppk_phased"
#> 
#> [[3]]$title
#> [1] "Phased FDA PopPK model building - one job per phase, SCM as its own checkpoint"
#> 
#> [[3]]$summary
#> [1] "Run a regulatory PopPK build as separate, checkpointed jobs - plan, structural base, IIV/BOV/covariance, then SCM as its own long job, then VPC/QPC for the chosen target - watching each job with wait_for_nlme_job and never bundling phases into one monolithic script or polling stdout.log."
#> 
#> [[3]]$details_md
#> [1] "Build a regulatory population PK model as a sequence of **separate, checkpointed\njobs** - never one bundled script. Each phase is launched, watched to terminal,\nand collected before the next begins, so a long phase (SCM especially) is a\nvisible checkpoint and a failure never discards earlier phases.\n\nThe single monitoring contract for every job in this playbook is\n`start_nlme_*` (launch) -> `wait_for_nlme_job` (until `watch$terminal`) ->\n`collect_nlme_job`. Follow each result's `next_action` directive, and stop and\nask the user when a result sets `requires_user_attention`. Never tail\n`stdout.log`, never start a shell timer/monitor, and never poll\n`get_nlme_job_status` in a loop - watching happens server-side inside\n`wait_for_nlme_job`.\n\n1. **Plan first (gate)** - a versioned plan from `save_analysis_plan()` must\n   exist before fitting unless `intended_use` is `'exploratory'` or\n   `allow_unplanned=TRUE` with a non-empty `unplanned_reason`; otherwise the\n   fit/VPC tools refuse to launch\n   (`decision.when_to_launch_scm`, `workflow.protocol_to_analysis_plan`). Agree\n   one `project_dir` and reuse it for every call\n   (`guidance.topic.project_layout`).\n2. **Structural base - its own job** - fit candidate structural models with\n   `start_nlme_fit_spec` (cold start) or `start_nlme_fitmodel` (iterate a built\n   model). This is the structural base: IIV + residual error, no estimated\n   covariate effects (`guidance.topic.model_stages`). Watch, collect, and judge\n   `fit_health` before moving on.\n3. **Random effects / BOV / covariance - its own job(s)** - add IIV structure,\n   between-occasion variability, and covariance blocks as separate checkpointed\n   fits. Do not fold these into the structural step or into the search.\n4. **SCM - its own long job** - run the prespecified covariate search as a single\n   dedicated job (`scm_stepwise.R` via `start_nlme_job(file = ...)`, planned and\n   gated by `validate_covariate_search`; see\n   `workflow.covariate_search_planning`). SCM over a realistic candidate set runs\n   for **hours** and tests candidates one at a time - that is normal, not a hang.\n   Watch it with `wait_for_nlme_job` like any other job and keep it separate so\n   the forward/backward result is a clean, independently collected checkpoint.\n5. **VPC / QPC - only after the target is chosen** - run `start_nlme_vpcmodel`\n   (pcVPC / QPC) once you know which model you are qualifying: base, final, or\n   both. Do not start VPC before the model target is settled.\n6. **Models and deliverables** - save portable `.mdl` / `.mmdl` models under\n   `<project_dir>/models/`, and let the host capture QC: set\n   `certara_session_project_dir` once so the repro script, figures, and report\n   accumulate automatically (`workflow.assemble_report`). Report the script and\n   report paths at the end.\n\nAnti-pattern this playbook prevents: a single `phases_2_to_7.R` job that bundles\nBSV / BOV / covariance / SCM / VPC. That hides the SCM checkpoint, makes a\nmulti-hour run look like one opaque job, and tempts shell-log polling. Split it -\none job per phase, each watched through MCP."
#> 
#> [[3]]$parent
#> [1] "Certara.RsNLME.guidance.toc"
#> 
#> [[3]]$steps
#> NULL
#> 
#> [[3]]$related
#>  [1] "Certara.RsNLME.workflow.protocol_to_analysis_plan"
#>  [2] "Certara.RsNLME.workflow.covariate_search_planning"
#>  [3] "Certara.RsNLME.workflow.assemble_report"          
#>  [4] "Certara.RsNLME.workflow.choose_run_mode"          
#>  [5] "Certara.RsNLME.mcp.bundled_scripts"               
#>  [6] "Certara.RsNLME.decision.when_to_launch_scm"       
#>  [7] "Certara.RsNLME.guidance.topic.model_stages"       
#>  [8] "Certara.RsNLME.guidance.topic.project_layout"     
#>  [9] "Certara.RsNLME.fn.start_nlme_fit_spec"            
#> [10] "Certara.RsNLME.fn.start_nlme_vpcmodel"            
#> [11] "Certara.RsNLME.fn.save_analysis_plan"             
#> 
#> 
```
