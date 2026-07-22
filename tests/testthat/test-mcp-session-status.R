# Tests for session / execution-context reporting (certara_session_status) and
# the launch-config plumbing. The capabilities-, tool-, and write_mcp_config-
# coupled checks live with the server/config engine (added in Phase 1c).

test_that("certara_session_status reports unknown wiring outside a running server", {
  on.exit(.kb_reset_index(), add = TRUE)
  .mcp_state$launch_config <- NULL
  st <- certara_session_status()
  expect_true(is.na(st$session_tools_enabled))
  expect_true(is.na(st$btw_groups)[1])
  expect_false(st$live_code_execution$enabled)       # off until proven on
  expect_false(st$environment_inspection$enabled)
  expect_match(st$next_steps, "unknown")
  # The three execution contexts are always described.
  expect_true(all(c("server_process", "live_session", "job_child") %in%
                    names(st$execution_contexts)))
})

test_that("certara_session_status reflects the recorded launch config", {
  on.exit({.mcp_state$launch_config <- NULL; .kb_reset_index()}, add = TRUE)
  .mcp_set_launch_config(btw_groups = c("docs", "pkg", "env", "run"),
                         session_tools = TRUE)
  st <- certara_session_status()
  expect_true(isTRUE(st$session_tools_enabled))
  expect_identical(st$btw_groups, c("docs", "pkg", "env", "run"))
  expect_true(st$live_code_execution$enabled)
  expect_identical(st$live_code_execution$tool, "btw_tool_run_r")
  expect_true(st$environment_inspection$enabled)
  expect_true(isTRUE(st$connect_live_session$enabled))
})

test_that("certara_session_status flags session_tools on but groups missing", {
  on.exit({.mcp_state$launch_config <- NULL; .kb_reset_index()}, add = TRUE)
  .mcp_set_launch_config(btw_groups = c("docs"), session_tools = TRUE)
  st <- certara_session_status()
  expect_true(isTRUE(st$session_tools_enabled))
  expect_false(st$live_code_execution$enabled)       # no 'run' group
  expect_false(st$environment_inspection$enabled)    # no 'env' group
  expect_match(st$next_steps, "'run'")
  expect_match(st$next_steps, "'env'")
})

test_that("certara_session_status guides enabling the bridge when it is off", {
  on.exit({.mcp_state$launch_config <- NULL; .kb_reset_index()}, add = TRUE)
  .mcp_set_launch_config(btw_groups = c("docs", "pkg"), session_tools = FALSE)
  st <- certara_session_status()
  expect_false(isTRUE(st$session_tools_enabled))
  expect_false(st$live_code_execution$enabled)
  expect_match(st$next_steps, "session_tools = TRUE")
})

test_that("capabilities advertise the execution_contexts pointer", {
  on.exit(.kb_reset_index(), add = TRUE)
  cap <- certara_mcp_capabilities()
  expect_false(is.null(cap$execution_contexts))
  # Long three-contexts essay was dropped; certara_session_status is the source
  # of truth for the live wiring.
  expect_identical(cap$execution_contexts$status_tool, "certara_session_status")
  expect_true(nzchar(cap$execution_contexts$note))
})

test_that("certara_session_status is registered among the host meta tools", {
  tool_names <- vapply(.certara_host_tools(), function(t) t@name, character(1))
  expect_true("certara_session_status" %in% tool_names)
})

test_that("write_mcp_config prints a session checklist when session_tools = TRUE", {
  proj <- file.path(tempdir(), paste0("mcpcfg_sess_", as.integer(Sys.time())))
  dir.create(proj, showWarnings = FALSE, recursive = TRUE)
  expect_message(
    write_mcp_config(client = "cursor", scope = "project", project_dir = proj,
                     btw_groups = c("docs", "pkg", "env", "run"),
                     session_tools = TRUE),
    "btw::btw_mcp_session\\(\\)"
  )
  cfg <- jsonlite::read_json(file.path(proj, ".cursor", "mcp.json"))
  expect_match(cfg$mcpServers[["certara-r"]]$args[[2]], "session_tools = TRUE")
})

test_that("session checklist warns when env/run groups are omitted", {
  expect_true(any(grepl("no 'env'",
                        .mcp_session_setup_checklist(c("docs", "pkg")))))
  expect_true(any(grepl("no 'run'",
                        .mcp_session_setup_checklist(c("docs", "pkg")))))
  full <- .mcp_session_setup_checklist(c("docs", "pkg", "env", "run"))
  expect_false(any(grepl("no 'env'", full)))
  expect_false(any(grepl("no 'run'", full)))
})
