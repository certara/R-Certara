# Durable session pin, capability-gap reporter, and durability signals.

test_that("mcp_session_project_dir persists and restores the pin", {
  mcp_session_paths_reset()
  mcp_repro_reset()
  mcp_report_reset()
  root <- file.path(tempdir(), paste0("certara_pin_", as.integer(Sys.time())))
  on.exit({
    unlink(root, recursive = TRUE)
    mcp_session_paths_reset()
    mcp_repro_reset()
    mcp_report_reset()
  }, add = TRUE)

  mcp_session_project_dir(root)
  expect_true(file.exists(file.path(root, ".certara-mcp", "session.json")))
  expect_true(file.exists(Certara.R:::.mcp_session_last_project_path()))

  mcp_session_paths_reset()
  expect_null(.mcp_session_paths_state$project_dir)
  restored <- mcp_session_project_dir()
  expect_equal(gsub("\\\\", "/", restored), gsub("\\\\", "/", root))
})

test_that("mcp_session_durability reports durable=FALSE under Rtmp*", {
  mcp_session_paths_reset()
  on.exit(mcp_session_paths_reset(), add = TRUE)
  mcp_session_project_dir(tempdir())
  dur <- mcp_session_durability()
  expect_false(dur$durable)
  expect_true(!is.null(dur$next_action))
  expect_equal(dur$next_action$tool, "certara_session_project_dir")

  info <- mcp_repro_info()
  expect_false(info$durable)
  expect_true(!is.null(info$next_action))
})

test_that("report_mcp_gap round-trips through list_memory_records", {
  gap_path <- Certara.R:::.memory_gaps_path()
  if (file.exists(gap_path)) {
    old <- readLines(gap_path, warn = FALSE)
    on.exit(writeLines(old, gap_path), add = TRUE)
  } else {
    on.exit(if (file.exists(gap_path)) unlink(gap_path), add = TRUE)
  }

  out <- report_mcp_gap(
    tool = "validate_fit_spec",
    task = "set fastOptimization via fit spec",
    missing_capability = "engine key was dropped",
    attempted_args = '{"engine":{"fastOptimization":"InnerAD"}}',
    workaround = "none"
  )
  expect_true(nzchar(out$id))
  expect_false(isTRUE(out$record$active))

  recs <- list_memory_records()
  expect_true(any(vapply(recs$gaps, function(g) identical(g$id, out$id),
                         logical(1))))
})

test_that("certara_mcp_capabilities exposes session_start_checklist and tool_discovery", {
  skip_if_not_installed("ellmer")
  cap <- certara_mcp_capabilities()
  expect_true(length(cap$session_start_checklist) >= 3L)
  expect_true(!is.null(cap$tool_discovery$host_tool_count))
  expect_true(cap$tool_discovery$host_tool_count >= 1L)
  expect_true("capability_gap" %in% names(cap$rules))
  expect_true(any(vapply(cap$tool_providers, function(p) {
    !is.na(p$tool_count)
  }, logical(1))))
})
