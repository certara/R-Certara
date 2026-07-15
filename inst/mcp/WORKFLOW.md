# Certara MCP: setup and default workflow

The Certara MCP server (hosted by `Certara.R`) exposes the Certara ecosystem's
knowledge-base and tool providers (NLME modeling via `Certara.RsNLME`, and more)
to AI coding agents (Cursor, Claude Code, Codex). It is an **optional** feature:
core modeling does not require it.

## 1. Install

```r
install.packages("Certara.R")   # ecosystem meta-package + MCP host
```

`Certara.R` pulls in the MCP stack (`btw`, `mcptools`, `ellmer`) and the provider
packages (e.g. `Certara.RsNLME`) as dependencies. Version floors:
`btw >= 1.2.0`, `mcptools >= 0.2.0`, `ellmer >= 0.4.0`.

## 2. Write the client configuration

```r
# Cursor (project scope) - writes/merges .cursor/mcp.json
Certara.R::write_mcp_config(client = "cursor", scope = "project")

# Claude Code (project scope) - writes/merges .mcp.json
Certara.R::write_mcp_config(client = "claude-code", scope = "project")

# Codex - writes the managed block to ~/.codex/config.toml directly
# (the equivalent `codex mcp add` command is reported for reference only)
Certara.R::write_mcp_config(client = "codex")

# Claude Desktop / Cowork (user scope) - merges claude_desktop_config.json
# (on Windows, the MSIX-virtualized path Desktop actually reads)
Certara.R::write_mcp_config(client = "claude-desktop")
```

After `claude-desktop`, fully quit and relaunch Claude Desktop (tray -> Quit) so
it re-reads the config. Desktop/Cowork do not auto-load a guidance doc and have
no working config-file tool allowlist, so call `certara_mcp_capabilities` as the
first MCP tool each session and approve tools in the UI when prompted.

The generated command runs `Certara.R::launch_certara_mcp()` with an absolute
`Rscript` path. The default is `btw_groups = "docs"`; pass a `tool_profile`
(e.g. `"core"`) to expose a focused tool subset.

### Tool profiles

| Profile | Host groups | Provider tools included |
|---------|-------------|--------------------------|
| `full` | meta, knowledge, memory | Every group every provider offers. |
| `core` | meta, knowledge | Read/author/validate only - no job launches (no `start_*`, no `start_darwin_search`). |
| `authoring` | meta, knowledge | `core` + model comparison, still no job launches. |
| `execution` | meta, knowledge, memory | `core` + fit/search job launches **and** their results/qualification tools (Darwin `collect_darwin_search`/`explain_darwin_fitness`/`propose_darwin_qualification`; RsNLME's sequential-LRT `qualification` tools) - a launched job can always be followed through to a decision. |
| `diagnostics` | meta, knowledge, memory | `execution` + comparison + interpretation. |

Each provider names its own tool groups (Certara.RDarwin's differ from
Certara.RsNLME's), so a profile maps provider groups **per package** rather
than one flat list assumed to mean the same thing everywhere - otherwise a
provider whose vocabulary does not happen to overlap the default one would be
silently reduced to a partial, unusable tool set under every profile but
`full`.

Project-scope Cursor setup also writes `.cursor/rules/certara-mcp-usage.mdc`
with the correct `CallMcpTool` server id.

### Cursor agent routing

The key in `.cursor/mcp.json` is `certara-r` (or your custom `server_name`),
but Cursor's `CallMcpTool` API uses `user-{server_name}` — e.g.
`user-certara-r`. Using the `mcp.json` key as the server argument fails with
"tool not found".

- Check `client_routing` in `certara_mcp_capabilities()` for the mapping.
- Project setup auto-writes `.cursor/rules/certara-mcp-usage.mdc` as a reminder.

## 3. (Optional) live RStudio session tools

```r
# 'env' = inspect the live session's objects; 'run' = run code there (btw_tool_run_r).
Certara.R::write_mcp_config(client = "cursor",
                            btw_groups = c("docs", "pkg", "env", "run"),
                            session_tools = TRUE)
# then in the RStudio session you want the agent to inspect:
btw::btw_mcp_session()
```

`session_tools = TRUE` exposes the live-session bridge but does **not** by
itself connect a session. After writing the config, reload the MCP server,
run `btw::btw_mcp_session()` in the target session, then have the agent call
`list_r_sessions` / `select_r_session` to attach. `write_mcp_config()` prints
this checklist and warns when `btw_groups` omits `"env"` or `"run"`.

Three R contexts are in play and are easy to confuse: the **server process**
(control-plane tools), the **bridged live session** (`env` inspection + `run`
code execution), and the **per-job child process** from `start_nlme_*`. The
`certara_session_status` tool reports which features are active and how to
attach a session; `certara_mcp_capabilities()$execution_contexts` carries the
same map.

## 4. Reload the agent's MCP servers, then work

The agent should call `certara_mcp_capabilities()` first to learn the
concurrency policy (retry sequentially if parallel calls fail), the
Certara-first rule, and to fetch preferences/lessons (if memory is enabled).

### Knowledge lookup (guidance first, then reference, then btw)

The KB has two layers: a **guidance layer** (the pharmacometric "bible" — what to
do and why, grounded in FDA/EMA/industry expectations) and a **reference layer**
(PML/API/function detail). Route by the kind of question:

1. **Methodology / "what should I do" / regulatory / fit-for-purpose questions** →
   `guide_pharmacometrics(task, intended_use)` **first**. It resolves one
   guidance tree in a single call: the relevant chapter(s), decision points,
   ordered steps, a merged pre-flight checklist (split into auto-verifiable vs
   human-judgment), and linked anti-patterns. Always resolve `intended_use` (one
   of `exploratory`, `dose_selection`, `labeling`, `pediatric`, `er_input`) — it
   scales the required validation depth and the checklist.
2. **Specific API / PML / "how do I call X" questions** →
   `search_certara_kb()` → `get_certara_kb_entry()` for the best hit;
   `lookup_pml_symbol()` for exact PML tokens; `explain_certara_workflow()` for
   step-by-step playbooks.
3. **Fallback** → `btw_tool_docs_help_page()` only when no KB entry covers it or
   the detail is insufficient (installed `.Rd` → markdown over MCP — no browser).

Avoid redundant parallel lookups of the same topic across KB, btw, and browser
tools. Project-scope Cursor setup writes `.cursor/rules/certara-mcp-usage.mdc`
with the same guidance for agents.

### Authoring PML and translating from NONMEM

Two supporting flows feed step 2 of the workflow below.

**Writing PML from scratch (KB-driven).** Retrieve the authoring KB before
hand-writing PML: `Certara.RsNLME.pml.authoring.skeleton` (statement order),
`...authoring.naming` (conventions), `...authoring.compartment_map` (how
statements create compartments), `...authoring.builtin_first` (prefer a builtin
constructor; reserve `textualmodel()` for custom structures), and
`...pml.parser_quirks` (lexical traps). Then `validate_pml()` and
`confirm_pml_structure()` before building.

**Translating a NONMEM control stream.** This is understanding-led, not
one-to-one:

1. `analyze_nonmem_control(control_text=|path=)` — read-only. Returns the
   ADVAN/TRANS structure, parameter counts, an `$INPUT`-derived
   `column_mapping_hint`, the `recommended_path` (builtin vs textual), an
   estimation summary, and explicit `open_questions`/`unsupported` lists.
2. `draft_pml_from_nonmem(...)` — drafts PML for the common-model subset with a
   `translation_report` (per-area confidence, deterministic conversions,
   `needs_review`, column-mapping spec, estimation notes). Deterministic math
   includes `$SIGMA` variance → `error()` SD via `sqrt`; `$THETA` FIX →
   `(freeze)`. It never guesses unmatched constructs — they become
   `needs_review`.
3. `validate_pml(draft$pml)` then `confirm_pml_structure(draft$pml, expected)` —
   the lint flags any untranslated NONMEM left behind (`THETA(1)`, `$PK`,
   `DADT(n)`, uppercase intrinsics).
4. Persist the audit trail with `save_translation_report(draft, dir)` and copy
   the `needs_review` items into the modeling log.
5. Resolve every `needs_review`/`open_questions` item with the user before
   fitting. v1 translates the **model only** — apply the `data_requirements`
   note to the dataset yourself; no rows are rewritten.

Mapping rules live in the KB (`Certara.RsNLME.nonmem.*`): `advan_trans`,
`param_blocks`, `residual_error`, `blq`, `dosing`, `records`, `pk_reserved`,
`covariates`, `estimation`, `column_mapping`.

### Tools by phase (quick reference)

The tools are organized into task-oriented phases so you reason by phase instead
of scanning the full list. This is a presentation map, not a second enablement
axis - the same tool can appear in more than one phase, and the cross-cutting
bands apply throughout. `certara_mcp_capabilities()$workflows` returns the same
map.

| Phase | Tools |
|-------|-------|
| setup | `certara_mcp_capabilities`, `list_certara_kb_packages`, `guide_pharmacometrics`, `get_user_preferences`, `get_lessons` |
| analysis_plan | `guide_pharmacometrics`, `analyze_nonmem_control`, `validate_analysis_plan`, `save_analysis_plan` |
| data_inspection | `inspect_pk_dataset`, `inspect_textual_model` |
| model_creation | `list_builtin_model_constructors`, `validate_pml`, `confirm_pml_structure`, `analyze_nonmem_control`, `draft_pml_from_nonmem`, `scaffold_mmdl_from_pml`, `validate_mmdl`, `validate_nlme_model`, `inspect_nlme_model`, `validate_fit_spec` |
| fit_execution | `start_nlme_fit_spec`, `start_nlme_fitmodel`, `start_nlme_fit`, `start_nlme_job` |
| diagnostics | `get_fit_summary`, `interpret_parameters`, `interpret_run` |
| vpc | `start_nlme_vpcmodel`, `summarize_vpc` |
| covariate_search | `start_nlme_job`, `compare_nlme_jobs` |
| darwin_search (Certara.RDarwin) | `check_darwin_prereqs`, `scaffold_darwin_project`, `validate_darwin_search`, `start_darwin_search`, `wait_for_darwin_job`, `collect_darwin_search`, `explain_darwin_fitness`, `propose_darwin_qualification` |
| qualification (Certara.RsNLME) | `check_ofv_parity`, `validate_sequential_lrt`, `start_sequential_lrt`, `advance_sequential_lrt`, `get_sequential_lrt_status`, `collect_sequential_lrt`, `stop_sequential_lrt`, `list_sequential_lrt_sessions` |
| report_qc | `get_fit_summary`, `compare_nlme_jobs`, `summarize_vpc`, `interpret_parameters`, `list_nlme_artifacts` |
| knowledge (any phase) | `search_certara_kb`, `get_certara_kb_entry`, `find_certara_examples`, `explain_certara_workflow`, `lookup_pml_symbol`, `list_pml_enums` |
| job_management (any phase) | `wait_for_nlme_job`, `get_nlme_job_status`, `collect_nlme_job`, `get_fit_summary`, `list_nlme_artifacts`, `list_mcp_runs`, `cleanup_mcp_runs`, `get_certara_project_status` |
| memory (any phase) | `get_user_preferences`, `get_lessons`, `record_lesson`, `set_preference`, `list_memory_records` |

### Default end-to-end PK/PD workflow

The server is **model-agnostic**: it drives any `NlmePmlModel` (built by
`pkmodel`, `pklinearmodel`, `pkindirectmodel`, `pkemaxmodel`, `emaxmodel`,
`linearmodel`, or `textualmodel`), not just PK or `.mmdl` files. There are two
equally first-class execution paths - choose per the
`Certara.RsNLME.workflow.choose_entry_point` KB entry:

- **RsNLME-direct (preferred for iteration):** build a model in R, fit it as an
  in-memory object - no `.mmdl` required.
- **Metamodel:** package a `.mmdl` and run it with `start_nlme_fit()` (best for
  portable, auditable handoff).

0. **Frame the analysis** - call `guide_pharmacometrics(task, intended_use)` to
   establish the intended use and pull the guidance tree (expectations, decision
   points, ordered steps, pre-flight checks). The intended use sets how much
   rigor the rest of the workflow needs; revisit the relevant chapter at each
   stage below. When the analysis derives from a protocol/SAP, turn it into a
   versioned analysis plan first (`Certara.RsNLME.workflow.protocol_to_analysis_plan`):
   extract the plan elements, label each `protocol_stated` / `prior_knowledge` /
   `agent_inferred`, then `validate_analysis_plan()` and `save_analysis_plan()`
   to persist an auditable `analysis-plan/analysis_plan.v<N>.json`. Agree one
   `project_dir` with the user and record where the data/protocol/reference files
   live in the plan; reuse that same `project_dir` for every tool call so the
   tool-owned paths (`analysis-plan/`, `rsnlme-mcp-runs/`, `covariate-search/`)
   stay correlated (`Certara.RsNLME.guidance.topic.project_layout`).
1. **EDA** of the dataset (doses, observations, covariates) - agent uses its own
   file/terminal tools plus R. For PK, see the data-prep example
   `inst/mcp/scripts/examples/prepare_pk_data.R` (resolves duplicate `(ID, TIME)`
   rows the engine rejects).
2. **Pick a structure** - `list_builtin_model_constructors()` to choose a
   template (`Certara.RsNLME.workflow.choose_model_constructor`); drop to
   `textualmodel()` for custom PML. Use `search_certara_kb()` /
   `explain_certara_workflow()` for recipes; check custom PML with
   `validate_pml()`. Authoring from scratch or porting a NONMEM control stream?
   See "Authoring PML and translating from NONMEM" above
   (`analyze_nonmem_control` / `draft_pml_from_nonmem`).

   **Decision rule - which execution path?**
   - **Builtin/simple model** (one of the seven constructors, a single dose
     column) → `validate_fit_spec()` then `start_nlme_fit_spec()`. A
     `textualmodel` spec may carry a `column_map`; it is applied via
     `colMapping()` after the build (the same mechanism as an `.mmdl ## MAP`
     block), so a flat term→column map works for custom PML too.
   - **Custom PML with ADVAN6/`$DES` ODEs, multiple dosepoints, or MAP/COLDEF
     needs** → `scaffold_mmdl_from_pml(pml, data_path, map, estargs=…)` to
     generate the `.mmdl` (no hand-writing `## MAP`/`## COLDEF`), then
     `validate_mmdl()` (confirm `data_available == TRUE`), then
     `start_nlme_fit()`. This is the recommended route for multi-dose-column
     models such as `A1=AMT1, A2=AMT2, A3=AMT3` with forward-covariates.
3. **Good initial estimates first** - do **not** fit a population model from the
   all-`1` defaults. Run a `Naive-Pooled` fit (or a quick NCA) to stabilize
   initials, then carry them forward (`Certara.RsNLME.workflow.initial_estimates`;
   anti-pattern `bad_initial_estimates`). `start_nlme_fit_spec()` and the
   `naive_pooled_fit.R` script automate this.
4. **Build + validate + fit** - the cold-start one-liner is
   `start_nlme_fit_spec(spec)`: it builds the model, runs `validate_nlme_model()`
   as a preflight, and fits - all in one non-blocking job. To iterate on an
   existing `model.rds`, use `start_nlme_fitmodel()`. For custom PML, scaffold an
   `.mmdl` (`scaffold_mmdl_from_pml()`), call `validate_mmdl(path)` and confirm
   `data_available == TRUE` first (the `## DATA` path resolves relative to the
   `.mmdl` directory; the sandbox holds **outputs only**), then
   `start_nlme_fit()`. In all cases watch the job with
   `wait_for_nlme_job(job_id, project_dir)` - it blocks server-side and returns
   once the job is terminal or the server's watch budget elapses, so the MCP
   server waits internally instead of making the agent spend tokens on repeated
   polling turns. If `watch$terminal` is `FALSE`, call it again to resume; don't
   pass `max_wait_seconds` unless asked. Use `get_nlme_job_status()` for a quick
   one-off snapshot (live `progress` - latest iteration, `-2LL`, theta - plus a
   `stderr_tail`). Once terminal, `collect_nlme_job()`. Each job result carries a
   single `next_action` directive naming the exact next call - follow it rather
   than re-deriving the step, and stop when `requires_user_attention` is set. The
   canonical chain and the no-shell-polling rule live in `certara-mcp-usage.mdc`;
   never `tail stdout.log` or start a shell monitor for a job.
5. **Judge the fit, then compare** - read `fit_health` (`data1_bytes`,
   `nlme_status`, `retcode`, scanned `err1.txt` reasons) and the structured
   `fit_summary` (estimates, OFV, convergence, `next_step` hint) - never
   `state`/`exit_code` alone, since a fit on empty data or bad initials can still
   exit 0. `fit_summary` is available even for `.mmdl`/`run_metamodel` fits with
   no `fit.rds`: estimates, the omega diagonal, and shrinkage are parsed from the
   engine artifacts (`Overall.csv`/`theta.csv`/`omega.csv`), with
   `estimates_source` reporting `fit.rds` vs `artifacts`. Use
   `get_fit_summary(job_id, project_dir)` for just this block (pass the same
   `project_dir` you launched with). Compare models on -2LL/AIC
   (`compare_models.R`).
6. **Covariate model** - `copyModel()`, `addCovariate()`/`covariateModel()`,
   then `stepwiseSearch()` (or `shotgunSearch()`) - run via `scm_stepwise.R` /
   `scm_shotgun.R`. Use `Certara.RsNLME.workflow.choose_run_mode` to decide when
   covariate search vs. bootstrap vs. VPC is warranted.
7. **Final fit + diagnostics** - Set `certara_session_project_dir` at project
   start so `scripts/`, `figures/`, and `reports/` co-locate. VPC: RsNLME
   simulation (`start_nlme_vpcmodel`) then tidyvpc MCP tools
   (`tidyvpc_load_from_dir` -> `tidyvpc_build_vpc` -> `tidyvpc_plot_vpc`);
   numeric quick check: `summarize_vpc`. GOF via Certara.Xpose.NLME (plots
   register in `certara_report_rmd`). Bootstrap via `bootstrap.R`. See
   `Certara.RsNLME.workflow.vpc_tidyvpc` and `workflow.mmdl_fit_vpc`.
8. **Export for handoff** - after a good fit, `write_mmdl()` produces the
   portable, shareable package (`.rds` is local workflow state only;
   `Certara.RsNLME.workflow.execution_contracts`).
9. **Interpretation (optional)** - `interpret_parameters()` against literature
   (cited), `interpret_run()` against your own history (opt-in memory).
10. **Modeling report (on request)** - when the user asks for a modeling (or
    modeling and simulation) report, call `guide_pharmacometrics("modeling
    report", intended_use)` first, then follow the standard section structure
    (`Certara.RsNLME.guidance.topic.modeling_report`) and the
    `Certara.RsNLME.workflow.assemble_report` playbook. The deliverable is a
    `.Rmd` under `reports/` with figures under `figures/` (MCP plot tools and
    `certara_report_rmd`); add narrative with `add_certara_report_note` and knit
    with `render_certara_report` when pandoc is available.

Heavy compute always runs in child processes (the `start_*`/`collect_*` tools or
a vetted script via `start_nlme_job(file=)`), never inside a blocking MCP tool
call. Scripts launched this way must `library(Certara.RsNLME)`, sandbox their
output, and fit with `runInBackground = FALSE`
(`Certara.RsNLME.workflow.mcp_script_runner`; anti-pattern `blocking_rscript`).

### Darwin (pyDarwin) hybrid qualification and sequential LRT

When a search over structure/token space is warranted, `Certara.RDarwin`
(pyDarwin) and `Certara.RsNLME`'s own SCM/FABE covariate search are
complementary, not competing (`Certara.RDarwin.workflow.overview`). The
hybrid path is deliberately **two stages, with two separate approvals** -
never a single automatic pipeline:

1. **Stage A - structural-anchor qualification (Certara.RDarwin).**
   `check_darwin_prereqs` -> author/import a project -> `validate_darwin_search`
   -> `start_darwin_search` -> `wait_for_darwin_job` -> `collect_darwin_search`
   / `explain_darwin_fitness`. The search winner is a **search winner, not an
   automatically qualified final model** - it must clear
   `darwin_default_acceptance_gates()` (convergence, covariance step,
   correlation, condition number) before use. When the winner alone does not
   settle the question, `propose_darwin_qualification()` recovers top-K
   candidates' control text (from `key_models`, then `models.json`, then a
   surviving temp run dir - never fabricated; a candidate with none of the
   three is reported `unrecoverable` with a reason) so the user can pick
   **one** to refit as a structural anchor.
   - **Non-nesting caveat:** Darwin's top-K candidates are almost never
     nested models - each token/search-space gene is an independent
     categorical choice, so two candidates routinely differ on several axes
     at once. **Never** run a likelihood-ratio test across
     `explain_darwin_fitness()`/`collect_darwin_search()`'s candidates; rank
     them only by fitness + acceptance gates
     (`Certara.RDarwin.workflow.hybrid_qualification`).
2. **Stage B - anchored sequential LRT (Certara.RsNLME).** Refit the chosen
   anchor (`start_nlme_fitmodel`/`start_nlme_fit`,
   `model_stage = "structural_anchor"`), then reconcile Darwin's recorded
   `ofv` (never `fitness` - see `ofv_not_fitness_for_likelihood_work`) against
   the RsNLME refit's `-2LL` with `check_ofv_parity()` **before** any nested
   testing. Only then does a genuinely nested, session-approved sequential
   LRT make sense: `validate_sequential_lrt` -> `start_sequential_lrt`
   (**one** `user_confirmed = TRUE` approves the whole envelope - anchor,
   ordered add/remove operations, alpha_add/alpha_remove, distributions,
   `host_resources$max_concurrent_fits`, and `fit_budget`; the session only
   adapts *within* it) -> `advance_sequential_lrt` repeatedly -> once final,
   `collect_sequential_lrt` for the decisions ledger and telemetry.
   `advance_sequential_lrt` pauses with `requires_user_attention = TRUE` on
   OFV-parity failure, a failed reference fit, an altered `plan.json`, an
   off-plan candidate, or exhausted budget - resolve with the user before
   `force_continue = TRUE`. `list_sequential_lrt_sessions` /
   `get_project_workflow_status` surface any paused session so it is not
   silently forgotten.
3. **Downstream, still user-gated.** Once a session is final,
   `collect_sequential_lrt()$downstream_proposals` may suggest VPC, the
   opt-in `qpc_score` (a **secondary**, informational predictive-check
   metric - never an automatic acceptance gate; see
   `tidyvpc.workflow.qpc_scoring`), AIC/BIC re-confirmation against the
   original Darwin candidates, bootstrap, or a constrained Darwin rerun -
   none of these launch automatically. Feed `collect_sequential_lrt()`'s
   `telemetry`/`session_provenance` block to `record_run()`'s `provenance`
   argument to benchmark this adaptive workflow against a fixed pyDarwin run
   under matched fit budgets.

Use `get_certara_project_status()` (this host) instead of a single provider's
own status tool when a project spans more than one provider (a Darwin search
feeding an RsNLME qualification/LRT) - it merges each discovered provider's
own status hook (Darwin's run registry, RsNLME's
`get_project_workflow_status()`, which already includes sequential-LRT
session state) without assuming they share a shape.

## Troubleshooting setup

Setup friction is mostly an environment problem (a `repos = NULL` source install
skips the `Suggests`; the client may need a reload; `INSTALLDIR` may be unset).
The common symptoms and fixes are collected for agents in the KB entry
`Certara.R.mcp.setup_troubleshooting` and for humans in the package
troubleshooting vignette. Start with `certara_mcp_capabilities()` and
`certara_session_status()` to see what is actually wired. There is no automated
"doctor" MCP tool - the checklist plus those two tools is the supported path
from inside a running server.

From the R console (before or outside a running server), two read-only
functions distinguish "configured" from "running":

```r
# What is written to client config files right now?
Certara.R::list_certara_mcp_configs()

# What Certara MCP server processes has this machine actually started?
Certara.R::list_certara_mcp_servers()
```

A server configured but never listed as running usually means the client has
not been reloaded since `write_mcp_config()`; a server listed as running with
options that do not match the config was likely started before the last
`write_mcp_config()` call and needs a client reload to pick up the new launch
command.

## Concurrency note

The MCP server is a single stdio R process. Certara tools are short
control-plane calls; if a client issues parallel calls and they fail, retry
sequentially. This is advertised in `certara_mcp_capabilities()`.

## Keeping the Cursor rule in sync

The agent rule at `.cursor/rules/certara-mcp-usage.mdc` is written once at setup.
After upgrading `Certara.RsNLME`, re-run
`write_mcp_config(client = "cursor", scope = "project")` so the rule (including
the fit/VPC preflight contract) is refreshed; existing projects otherwise keep
the older rule until that is done.
