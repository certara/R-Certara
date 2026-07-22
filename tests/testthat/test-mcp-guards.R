# Light execution-guard + provenance tests (Phase 2.5f).

test_that(".mcp_note_call counts consecutive identical calls and resets on change", {
  on.exit(.kb_reset_index(), add = TRUE)
  .kb_reset_index()
  expect_identical(.mcp_note_call("t", list(x = 1)), 1L)
  expect_identical(.mcp_note_call("t", list(x = 1)), 2L)
  expect_identical(.mcp_note_call("t", list(x = 1)), 3L)
  # different args reset the counter
  expect_identical(.mcp_note_call("t", list(x = 2)), 1L)
  # different tool resets too
  expect_identical(.mcp_note_call("u", list(x = 2)), 1L)
})

test_that("loop guard threshold and toggle read the environment", {
  withr::local_envvar(c(CERTARA_MCP_LOOP_MAX = "3"))
  expect_identical(.mcp_loop_threshold(), 3L)
  withr::local_envvar(c(CERTARA_MCP_LOOP_MAX = ""))
  expect_identical(.mcp_loop_threshold(), 5L)          # default
  withr::local_envvar(c(CERTARA_MCP_LOOP_GUARD = "0"))
  expect_false(.mcp_loop_guard_on())
  withr::local_envvar(c(CERTARA_MCP_LOOP_GUARD = "1"))
  expect_true(.mcp_loop_guard_on())
})

test_that(".mcp_loop_nudge returns a structured advisory", {
  n <- .mcp_loop_nudge("find_certara_tools", 6)
  expect_true(isTRUE(n$loop_detected))
  expect_identical(n$tool, "find_certara_tools")
  expect_identical(n$repeats, 6)
  expect_match(n$message, "identical arguments")
})

test_that("audit is opt-in and never errors", {
  withr::local_envvar(c(CERTARA_MCP_AUDIT = ""))
  expect_false(.mcp_audit_enabled())
  expect_invisible(.mcp_audit("t", list(a = 1), result = list(1, 2)))
  withr::local_envvar(c(CERTARA_MCP_AUDIT = "1"))
  expect_true(.mcp_audit_enabled())
  expect_invisible(suppressWarnings(.mcp_audit("t", list(a = 1), error_class = "simpleError")))
})

test_that(".mcp_invoke passes through normally and short-circuits a loop without executing", {
  on.exit({.kb_reset_index(); options(mcp_calls = NULL)}, add = TRUE)
  .kb_reset_index()
  withr::local_envvar(c(CERTARA_MCP_LOOP_MAX = "3", CERTARA_MCP_LOOP_GUARD = "1"))
  calls <- 0L
  fun <- function(x = 1) { calls <<- calls + 1L; x * 2 }

  expect_identical(.mcp_invoke("doubler", fun, list(x = 3)), 6)   # 1st
  expect_identical(.mcp_invoke("doubler", fun, list(x = 3)), 6)   # 2nd
  expect_identical(calls, 2L)
  # 3rd identical call hits the threshold -> nudge, handler NOT executed again
  res <- .mcp_invoke("doubler", fun, list(x = 3))
  expect_true(isTRUE(res$loop_detected))
  expect_identical(calls, 2L)
  # a different argument resets and executes again
  expect_identical(.mcp_invoke("doubler", fun, list(x = 5)), 10)
  expect_identical(calls, 3L)
})

test_that("tool-provider discovery labels source provenance (installed vs dev_root)", {
  root <- testthat::test_path("fixtures", "tool-provider-builder")
  disc <- .mcp_discover_tool_providers(dev_roots = root)
  src <- vapply(disc$providers, function(p) p$source %||% "", character(1))
  expect_true("dev_root" %in% src)
})
