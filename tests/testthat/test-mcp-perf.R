# Tests for the tool-timing instrumentation. The detailed perf report
# (.mcp_perf_report) calls the capabilities/KB tools and is covered with the
# server/config engine in Phase 1c; here we only assert the timing wrapper is
# transparent and gated by the CERTARA_MCP_PERF env var.

test_that(".mcp_timed is a no-op passthrough when perf logging is disabled", {
  withr::local_envvar(c(CERTARA_MCP_PERF = ""))
  expect_false(.mcp_perf_enabled())
  expect_identical(.mcp_timed("demo", 41 + 1), 42)
})

test_that(".mcp_timed returns the expression value when perf logging is enabled", {
  withr::local_envvar(c(CERTARA_MCP_PERF = "1"))
  expect_true(.mcp_perf_enabled())
  # Emits a [certara-mcp-perf] line to stderr but must still return the value.
  expect_identical(.mcp_timed("demo", 41 + 1), 42)
})
