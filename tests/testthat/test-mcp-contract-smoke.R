# Focused contract smoke tests for the provider-agnostic MCP host:
#   * a declarative (tools-array) manifest is skipped, not loaded (builder-mode
#     discovery itself is exercised in test-mcp-tool-providers.R)
#   * certara_mcp_capabilities exposes active_tool_profile and does not require
#     any declarative-mode plumbing to succeed

test_that("declarative tools-array manifest is skipped with a reason", {
  tmp <- tempfile("declarative_smoke_")
  mdir <- file.path(tmp, "inst", "mcp", "tools")
  dir.create(mdir, recursive = TRUE)
  writeLines(
    paste0('{"package":"DeclarativeOnly","schema_version":"1.0.0",',
           '"tools":[{"name":"t","description":"d","handler":"h"}]}'),
    file.path(mdir, "manifest.json")
  )
  disc <- .mcp_discover_tool_providers(dev_roots = tmp)
  # The declarative-only manifest never appears among loadable providers.
  pkgs <- vapply(disc$providers, function(p) p$package %||% "", character(1))
  expect_false("DeclarativeOnly" %in% pkgs)
  # And it is reported in the skipped list with an actionable reason.
  reasons <- vapply(disc$skipped, function(s) s$reason %||% "", character(1))
  expect_true(any(grepl("'tools'|declarative", reasons)))
})

test_that("certara_mcp_capabilities returns active_tool_profile without declarative plumbing", {
  on.exit(.kb_reset_index(), add = TRUE)
  cap <- certara_mcp_capabilities()
  # Field is always advertised; NA when no profile is active (e.g. outside a
  # running launch_certara_mcp() invocation).
  expect_true("active_tool_profile" %in% names(cap))
  expect_true(is.character(cap$active_tool_profile) &&
                length(cap$active_tool_profile) == 1L)
  # Core provider-agnostic fields the host contract advertises regardless of
  # tool-provider mode.
  for (field in c("server", "client_routing", "package_version",
                  "kb_schema_version", "tools_schema_version",
                  "kb_providers", "tool_providers", "gated_tools",
                  "capability_providers", "concurrency", "rules",
                  "workflows", "prerequisites", "session_start",
                  "execution_contexts", "environment_notes")) {
    expect_true(field %in% names(cap), info = paste("missing field:", field))
  }
})
