# Remove the Certara MCP server from client configuration

Inverse of
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md).
For file-based clients (Cursor, Claude Code project scope) it removes
the named server while preserving every other server. For CLI-managed
targets (Claude Code user/local scope, Codex) it prints the removal
command to run.

## Usage

``` r
remove_mcp_config(
  client = c("cursor", "claude-code", "codex", "claude-desktop"),
  scope = c("project", "user", "local"),
  server_name = "certara-r",
  project_dir = ".",
  run = FALSE
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

- server_name:

  MCP server key (default `"certara-r"`). Restricted to letters, digits,
  `-`, and `_` (starting with a letter or digit), since it is written
  verbatim as a JSON key, a TOML table name, and inside an R string
  literal.

- project_dir:

  Project root for project-scope files.

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

## Value

Invisibly, a list describing actions taken (paths edited, commands).

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove the Cursor project-scope config written by write_mcp_config().
remove_mcp_config("cursor")
} # }
```
