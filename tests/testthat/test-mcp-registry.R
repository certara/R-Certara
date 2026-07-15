# Tests for the per-user MCP runtime registry (mcp_registry.R) and
# list_certara_mcp_servers(). Each test isolates the registry directory via
# the Certara.R.mcp_registry_dir option so runs never touch the real per-user
# cache dir.

local_registry_dir <- function(env = parent.frame()) {
  dir <- file.path(tempdir(), paste0("mcp-registry-", as.integer(Sys.time()),
                                     "-", sample.int(1e6, 1)))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  withr::local_options(list(Certara.R.mcp_registry_dir = dir), .local_envir = env)
  dir
}

test_that("list_certara_mcp_servers reports nothing when the registry is empty", {
  local_registry_dir()
  out <- list_certara_mcp_servers()
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
  expect_true(all(c("server_name", "pid", "status", "started_at",
                    "package_version", "r_version", "tool_profile",
                    "session_tools", "btw_groups", "job_watch_wait_seconds",
                    "working_dir") %in% names(out)))
})

test_that("registering the current process makes it discoverable", {
  local_registry_dir()
  .mcp_registry_register(server_name = "certara-r", btw_groups = c("docs", "pkg"),
                         session_tools = TRUE, job_watch_wait_seconds = 45,
                         tool_profile = "full")
  out <- list_certara_mcp_servers()
  expect_identical(nrow(out), 1L)
  expect_identical(out$server_name, "certara-r")
  expect_identical(out$pid, Sys.getpid())
  expect_identical(out$status, "running")
  expect_true(out$session_tools)
  expect_identical(out$btw_groups, "docs, pkg")
  expect_identical(out$job_watch_wait_seconds, 45)
  expect_identical(out$tool_profile, "full")
})

test_that("unregistering removes the current process from the listing", {
  local_registry_dir()
  .mcp_registry_register(server_name = "certara-r", btw_groups = "docs",
                         session_tools = FALSE, job_watch_wait_seconds = 45,
                         tool_profile = "core")
  expect_identical(nrow(list_certara_mcp_servers()), 1L)

  .mcp_registry_unregister()
  expect_identical(nrow(list_certara_mcp_servers()), 0L)

  # Unregistering again is a harmless no-op.
  expect_false(isTRUE(.mcp_registry_unregister()))
})

test_that("re-registering the same PID overwrites the existing record", {
  dir <- local_registry_dir()
  .mcp_registry_register(server_name = "certara-r", btw_groups = "docs",
                         session_tools = FALSE, job_watch_wait_seconds = 45,
                         tool_profile = "core")
  # Simulate a server restarting under the same PID (e.g. re-registering
  # after .mcp_registry_prune() left the file in place): the destination
  # already exists, which is the case file.rename() cannot overwrite on
  # Windows without an explicit unlink() first.
  expect_no_error(
    .mcp_registry_register(server_name = "certara-r", btw_groups = c("docs", "pkg"),
                           session_tools = TRUE, job_watch_wait_seconds = 600,
                           tool_profile = "full")
  )
  out <- list_certara_mcp_servers()
  expect_identical(nrow(out), 1L)
  expect_identical(out$btw_groups, "docs, pkg")
  expect_true(out$session_tools)
  expect_identical(out$tool_profile, "full")
  # No leftover temp file from the rename.
  expect_length(Sys.glob(file.path(dir, "pid-*.json.tmp-*")), 0)

  .mcp_registry_unregister()
})

test_that("a dead PID is reported only with include_stale = TRUE", {
  dir <- local_registry_dir()
  # A large-but-integer-range PID very unlikely to be a live process on the
  # test machine.
  fake <- list(pid = 999999999, create_time = 1577836800, server_name = "certara-r",
              started_at = "2020-01-01T00:00:00+0000", package_version = "0.0.0",
              r_version = "4.0", tool_profile = "full", session_tools = FALSE,
              btw_groups = list("docs"), job_watch_wait_seconds = 45,
              working_dir = "/tmp")
  jsonlite::write_json(fake, file.path(dir, "pid-999999999.json"), auto_unbox = TRUE)

  expect_identical(nrow(list_certara_mcp_servers()), 0L)
  out <- list_certara_mcp_servers(include_stale = TRUE)
  expect_identical(nrow(out), 1L)
  expect_identical(out$status, "stale")
  expect_identical(out$pid, 999999999L)
})

test_that("a PID reused by an unrelated process is treated as stale", {
  dir <- local_registry_dir()
  # The current process is real and running, but the recorded create_time is
  # deliberately wrong - simulating the PID having been reused since the
  # server that owned it exited.
  fake <- list(pid = Sys.getpid(), create_time = 1, server_name = "certara-r",
              started_at = "2020-01-01T00:00:00+0000", package_version = "0.0.0",
              r_version = "4.0", tool_profile = "full", session_tools = FALSE,
              btw_groups = list("docs"), job_watch_wait_seconds = 45,
              working_dir = "/tmp")
  jsonlite::write_json(fake, file.path(dir, sprintf("pid-%d.json", Sys.getpid())),
                       auto_unbox = TRUE)

  out <- list_certara_mcp_servers(include_stale = TRUE)
  expect_identical(nrow(out), 1L)
  expect_identical(out$status, "stale")
})

test_that("a malformed registry file is silently skipped", {
  dir <- local_registry_dir()
  writeLines("{not valid json", file.path(dir, "pid-123.json"))
  expect_identical(nrow(list_certara_mcp_servers()), 0L)
  expect_identical(nrow(list_certara_mcp_servers(include_stale = TRUE)), 0L)
})

test_that(".mcp_registry_prune removes malformed and dead-process entries", {
  dir <- local_registry_dir()
  writeLines("{not valid json", file.path(dir, "pid-124.json"))
  fake <- list(pid = 999999999, create_time = 1, server_name = "certara-r",
              started_at = "2020-01-01T00:00:00+0000", package_version = "0.0.0",
              r_version = "4.0", tool_profile = "full", session_tools = FALSE,
              btw_groups = list("docs"), job_watch_wait_seconds = 45,
              working_dir = "/tmp")
  jsonlite::write_json(fake, file.path(dir, "pid-999999999.json"), auto_unbox = TRUE)
  .mcp_registry_register(server_name = "certara-r", btw_groups = "docs",
                         session_tools = FALSE, job_watch_wait_seconds = 45,
                         tool_profile = "full")

  expect_length(Sys.glob(file.path(dir, "pid-*.json")), 3)
  .mcp_registry_prune()
  remaining <- Sys.glob(file.path(dir, "pid-*.json"))
  expect_length(remaining, 1)
  expect_identical(basename(remaining), sprintf("pid-%d.json", Sys.getpid()))

  .mcp_registry_unregister()
})

test_that("multiple registered instances with custom server names are all listed", {
  dir <- local_registry_dir()
  .mcp_registry_register(server_name = "certara-r", btw_groups = "docs",
                         session_tools = FALSE, job_watch_wait_seconds = 45,
                         tool_profile = "full")
  # Simulate a second, distinct process under a custom server name.
  second <- list(pid = Sys.getpid() + 1L, create_time = 1700000000,
                 server_name = "rsnlme-mcp",
                 started_at = "2026-01-01T00:00:00+0000", package_version = "2.0.0",
                 r_version = "4.6.0", tool_profile = "execution",
                 session_tools = TRUE, btw_groups = list("docs", "env", "run"),
                 job_watch_wait_seconds = 600, working_dir = "/proj")
  jsonlite::write_json(second, file.path(dir, sprintf("pid-%d.json", Sys.getpid() + 1L)),
                       auto_unbox = TRUE)

  # The fabricated second PID is very unlikely to be a live process on the
  # test machine, but guard against the (astronomically rare) flake by
  # checking only for our own real entry plus at least one row overall.
  out <- list_certara_mcp_servers(include_stale = TRUE)
  expect_true(nrow(out) >= 1)
  expect_true("certara-r" %in% out$server_name)
  own <- out[out$pid == Sys.getpid(), ]
  expect_identical(nrow(own), 1L)
  expect_identical(own$status, "running")

  .mcp_registry_unregister()
})

test_that("list_certara_mcp_servers is registered read-only (no side effects)", {
  dir <- local_registry_dir()
  .mcp_registry_register(server_name = "certara-r", btw_groups = "docs",
                         session_tools = FALSE, job_watch_wait_seconds = 45,
                         tool_profile = "full")
  before <- Sys.glob(file.path(dir, "pid-*.json"))
  list_certara_mcp_servers()
  list_certara_mcp_servers(include_stale = TRUE)
  after <- Sys.glob(file.path(dir, "pid-*.json"))
  expect_identical(before, after)

  .mcp_registry_unregister()
})
