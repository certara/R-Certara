# Merged cross-provider project status

Calls every discovered provider's optional `status_hook` (declared in
its `inst/mcp/tools/manifest.json`, see
[`.mcp_discover_tool_providers()`](https://github.com/certara/R-Certara/reference/dot-mcp_discover_tool_providers.md))
with `project_dir` and merges the results into one snapshot, so an agent
can see where a project stands across Certara.RDarwin searches,
Certara.RsNLME jobs/VPCs/sequential-LRT sessions, and any other provider
that ships a hook - without this host hardcoding any provider's own
status representation. A provider with no `status_hook` is absent from
`providers` rather than treated as an error. Nothing is written;
provider-local artifacts (Darwin run directories, LRT session/decision
files, repro scripts) remain the source of truth - this only surfaces
the pointers each provider's status already returns (e.g. `run_dir`,
`session_dir`, `plan_path`, `decisions_path`).

## Usage

``` r
get_certara_project_status(project_dir = ".", dev_roots = character(0))
```

## Arguments

- project_dir:

  Project root (default `"."`).

- dev_roots:

  Optional dev source-tree roots for provider discovery (see
  [`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)).

## Value

A list with `project_dir`, `providers` (one entry per provider that
declares a `status_hook`: `package`, `status_hook`, and either `status`
or `error`), `next_gated_phase` (a best-effort hint from whichever
provider's status names one, or `NULL`), `repro_script`, `report_rmd`
(this session's own accumulated artifacts), and `repro_project_mismatch`
(`TRUE` when `repro_script` is not rooted under `project_dir` - the
session recorder was never pointed at this project, so it is not a
faithful replay of work done here).
