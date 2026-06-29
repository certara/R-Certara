# MCP setup troubleshooting

## Certara.R.mcp.setup_troubleshooting
type: workflow_recipe
title: When MCP setup goes wrong - a troubleshooting checklist
summary: Agent-facing checklist for the common Certara MCP setup failures (server not listed, missing Suggests from a source install, fit/VPC can't find the model, engine/compile errors, stale package, tool overload) using the existing tools - write_mcp_config, certara_session_status, certara_mcp_capabilities - without adding a new tool.
keywords: MCP setup, troubleshooting, server not listed, mcp.json, write_mcp_config, ellmer, mcptools, btw, Suggests, INSTALLDIR, certara_session_status, reload, permissions, tool overload, can't find model, stale package, configuration
symbols: write_mcp_config, certara_session_status, certara_mcp_capabilities, list_mcp_runs
applies_to.model_type: pk, pd
related: Certara.R.fn.certara_session_status, Certara.RsNLME.guidance.topic.vpc_fit_target, Certara.RsNLME.workflow.execution_contracts, Certara.RsNLME.mcp.bundled_scripts
provenance.source_file: R/mcp_config.R
provenance.symbol: write_mcp_config
source.kind: Rd

MCP setup friction is largely an **environment** problem, not something another
MCP tool can fix. A release install (not a `repos = NULL` source install) pulls
the Suggests; the rest is configuration and PATH. Use the existing tools -
`write_mcp_config()`, `certara_session_status()`, `certara_mcp_capabilities()` -
and this checklist rather than improvising.

| Symptom | Likely cause | What to try |
|---------|--------------|-------------|
| MCP tools missing / server not listed | `write_mcp_config()` not run, or the client was not reloaded | `write_mcp_config(client = "cursor", scope = "project")`; reload the client; in Cursor, Settings -> Tools & MCPs -> enable the Certara R server |
| `ellmer` / `mcptools` / `btw` not found | A dev install with `repos = NULL` skips Suggests | Install from the release repo, or `install.packages(c("btw","mcptools","ellmer"))`; floors: `btw >= 1.2.0`, `mcptools >= 0.2.0`, `ellmer >= 0.4.0` |
| Fit/VPC "can't find the model" | Confusing the MCP server R process with an interactive session, or acting on the wrong/unconfirmed model | `certara_session_status()`; ask the user which fit and enumerate with `list_mcp_runs` / `list_nlme_artifacts`; then use `fit.rds` from the chosen job or the live-session object (`Certara.RsNLME.guidance.topic.vpc_fit_target`) |
| Engine / compile failures | `INSTALLDIR` / NLME not resolvable (Windows PATH) | Confirm the NLME Engine install; the validators resolve `INSTALLDIR` via `.mcp_resolve_installdir()` - check it is set (see the installation/troubleshooting vignette) |
| Agent behavior feels "old" | The installed package predates the current MCP KB/rules | Load/install the current release; call `certara_mcp_capabilities()` and confirm `rules` includes `project_layout`, `model_staging`, `scm_gating` |
| Too many tools | Normal when `btw` groups are enabled | Use `certara_mcp_capabilities()$workflows` phases to reason by task; for the leanest server set `btw_groups = "docs"` |

This is an agent-facing checklist, not an automated doctor: there is **no**
`rsnlme_mcp_doctor` tool. The same content is in the package's troubleshooting
vignette for human setup.
