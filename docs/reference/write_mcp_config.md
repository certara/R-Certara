# Write MCP client configuration for the Certara server

Write MCP client configuration for the Certara server

## Usage

``` r
write_mcp_config(
  client = c("cursor", "claude-code", "codex", "claude-desktop"),
  scope = c("project", "user", "local"),
  btw_groups = "docs",
  session_tools = FALSE,
  tool_profile = c("full", "core", "authoring", "execution", "diagnostics"),
  server_name = "certara-r",
  project_dir = ".",
  tool_allowlist = TRUE,
  run = FALSE,
  job_watch_wait_seconds = NULL,
  agent = NULL
)
```

## Arguments

- client:

  One or more of `"cursor"`, `"claude-code"`, `"codex"`,
  `"claude-desktop"`. A bare call defaults to `"cursor"` only; pass a
  vector to configure several. `"claude-desktop"` targets Claude Desktop
  and its Cowork local-agent mode (user scope only).

- scope:

  One or more of `"project"`, `"user"`, `"local"`. A bare call defaults
  to `"project"` only. `"local"` is Claude Code-only (a per-project
  private entry in `~/.claude.json`); it is skipped with a warning for
  `"cursor"`, which has no local tier, and is irrelevant to `"codex"`.

- btw_groups:

  Which general-purpose `btw` R tool groups to expose to the agent
  alongside the Certara tools; the chosen groups are baked into the
  launch command. The default `"docs"` provides read-only R
  documentation lookup (help pages, help-topic listings, vignettes,
  release notes). Add `"pkg"` for package-development actions (document,
  check, test, coverage, load-all). Two groups enable working against a
  live session bridged with
  [`btw::btw_mcp_session()`](https://posit-dev.github.io/btw/reference/mcp.html)
  (set `session_tools = TRUE` as well): `"env"` inspects that session's
  objects (loaded data frames, fitted models) and `"run"` exposes
  `btw_tool_run_r`, which executes R code in that session's global
  environment - include `"run"` only when you want the agent to run
  arbitrary code there. Other groups `btw` offers include `"files"`,
  `"git"`, `"github"`, `"ide"`, `"cran"`, `"web"`, and `"sessioninfo"`.
  Use `"docs"` alone for the leanest, lowest-latency server, or
  `character(0)` to expose only the Certara tools.

- session_tools:

  Whether to expose tools that bridge to a separate, live R session you
  register with
  [`btw::btw_mcp_session()`](https://posit-dev.github.io/btw/reference/mcp.html)
  (for example your interactive RStudio session), letting the agent
  inspect that session's objects - loaded data frames, fitted models -
  and run code there. When `FALSE` (the default) the server uses only
  its own R process, which starts faster and avoids the session hop;
  enable it to work against your live workspace. When `TRUE`, pair it
  with `btw_groups` `"env"` (inspect objects) and/or `"run"` (run code
  via `btw_tool_run_r`), then run
  [`btw::btw_mcp_session()`](https://posit-dev.github.io/btw/reference/mcp.html)
  in the session you want reached; the call prints a post-setup
  checklist.
  [`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
  reports the live wiring.

- tool_profile:

  Curated tool subset baked into the launch command, to keep the tool
  list focused: `"full"` (default), `"core"`, `"authoring"`,
  `"execution"`, or `"diagnostics"`. See
  [`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md).

- server_name:

  MCP server key (default `"certara-r"`). Restricted to letters, digits,
  `-`, and `_` (starting with a letter or digit), since it is written
  verbatim as a JSON key, a TOML table name, and inside an R string
  literal.

- project_dir:

  Project root for project-scope files.

- tool_allowlist:

  When `TRUE`, pre-authorize the Certara MCP tools so they run without
  per-tool approval prompts, using each requested client's native
  mechanism for every requested scope:

  - Cursor: merge `server_name:*` into `permissions.json`
    (`<project>/.cursor/permissions.json` or
    `~/.cursor/permissions.json`); takes effect under Cursor Run Mode.

  - Claude Code: merge `mcp__server_name` into `permissions$allow` of
    the scope-appropriate settings file
    (`<project>/.claude/settings.json` for project,
    `<project>/.claude/settings.local.json` for local,
    `~/.claude/settings.json` for user).

  - Codex: write a user-scope managed block into `~/.codex/config.toml`
    with this MCP server and Codex MCP approval settings. Most Certara
    tools are approved, while privileged cleanup and arbitrary-R job
    tools stay prompt-gated.

  Default `TRUE`.

- run:

  For CLI-managed targets (Claude Code user/local scope), actually
  execute the client CLI when it is on `PATH` instead of only printing
  the command. Falls back to printing if the CLI is not found. Default
  `FALSE`. Claude Code's `claude mcp add` refuses when the server name
  already exists, so this path always remove-then-adds (soft-remove if
  absent) so a stale launcher (e.g. an old
  `Certara.RsNLME::launch_certara_mcp` entry) is replaced. Codex is
  configured by writing `~/.codex/config.toml` directly (the only way to
  set per-tool approvals), so `run` does not invoke the Codex CLI; the
  equivalent `codex mcp add` command is reported for reference.

- job_watch_wait_seconds:

  Optional override for the per-call server-side job-watch budget
  (seconds) baked into the launch command, used by
  [`wait_for_nlme_job()`](https://certara.github.io/R-RsNLME/reference/wait_for_nlme_job.html).
  When `NULL` (the default) each client gets a sensible default: `45`
  for Cursor (which caps a single MCP tool call at ~60s and has no
  timeout setting) and `600` for Claude Code and Codex (which allow
  long-running tool calls). Supply a single number to override every
  selected client; clamped to `0..600`.

- agent:

  Alias for `client`, for users who think of these as agent
  configuration targets. Use either `client` or `agent`, not both.

## Value

Invisibly, a list describing actions taken (written paths, snippets).

## Details

For `"claude-code"`, the routing/behavior contract is delivered as a
`CLAUDE.md`-imported guidance doc (`.claude/certara-mcp-usage.md` for
project/local, `~/.claude/certara-mcp-usage.md` for user), since Claude
Code does not read Cursor `.mdc` rules;
[`remove_mcp_config()`](https://github.com/certara/R-Certara/reference/remove_mcp_config.md)
removes it.

For `"claude-desktop"` (Claude Desktop and Cowork), the server is merged
into `claude_desktop_config.json` (user scope only). On Windows the MSIX
virtualized path Desktop actually reads is targeted when present. A
companion `certara-mcp-usage.md` is written beside that file for manual
reference - Desktop/Cowork do not auto-load it the way Claude Code
imports `CLAUDE.md`, so call `certara_mcp_capabilities` at session
start. There is no working config-file tool allowlist for Cowork, so
`tool_allowlist` only prints a note (approve tools in the UI per
session). Fully quit and relaunch Claude Desktop after config changes.

For the leanest, lowest-latency Claude Code or Desktop server, drop the
package-development tools with `btw_groups = "docs"` (or `character(0)`
for Certara tools only).

## Examples

``` r
if (FALSE) { # \dontrun{
# Cursor, project scope (the bare-call default).
write_mcp_config()

# Lean Claude Code server: Certara tools + R docs only, no pkg-dev tools.
write_mcp_config("claude-code", btw_groups = "docs")

# Claude Code at user scope, registering via the claude CLI when present.
write_mcp_config("claude-code", scope = "user", run = TRUE)

# Claude Desktop / Cowork (merges claude_desktop_config.json).
write_mcp_config("claude-desktop")
} # }
```
