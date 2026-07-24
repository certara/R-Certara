# get_certara_project_status() merges each discovered provider's status_hook.
# .mcp_discover_tool_providers()/getExportedValue() are mocked so this
# exercises only the host-side merge/error-handling logic, never a real
# provider package or a live engine.

test_that(".mcp_call_status_hook calls an exported status_hook and returns its result", {
  provider <- list(package = "base", manifest = list(status_hook = "nchar"))
  out <- .mcp_call_status_hook(provider, "abc")
  expect_equal(out$package, "base")
  expect_equal(out$status_hook, "nchar")
  expect_equal(out$status, 3L)
})

test_that(".mcp_call_status_hook returns NULL when no status_hook is declared", {
  provider <- list(package = "base", manifest = list())
  expect_null(.mcp_call_status_hook(provider, "."))
})

test_that(".mcp_call_status_hook reports (not throws) an unresolvable status_hook", {
  provider <- list(package = "base", manifest = list(status_hook = "definitely_not_a_real_fn"))
  out <- .mcp_call_status_hook(provider, ".")
  expect_match(out$error, "not an exported function")
})

test_that(".mcp_call_status_hook reports (not throws) a status_hook that errors", {
  boom <- function(project_dir) stop("kaboom")
  local_mocked_bindings(getExportedValue = function(ns, name) boom, .package = "base")
  provider <- list(package = "base", manifest = list(status_hook = "boom"))
  out <- .mcp_call_status_hook(provider, ".")
  expect_match(out$error, "failed or returned NULL")
})

test_that("get_certara_project_status merges providers and skips those with no status_hook", {
  local_mocked_bindings(
    .mcp_discover_tool_providers = function(dev_roots = character(0)) {
      list(
        providers = list(
          list(package = "base", manifest = list(status_hook = "nchar")),
          list(package = "base", manifest = list())  # no status_hook -> absent from output
        ),
        skipped = list()
      )
    }
  )
  out <- get_certara_project_status(project_dir = "abcd")
  expect_length(out$providers, 1)
  expect_equal(out$providers[[1]]$package, "base")
  expect_equal(out$providers[[1]]$status, 4L)
})

test_that("get_certara_project_status surfaces a next_gated_phase hint from a provider's status", {
  local_mocked_bindings(
    .mcp_discover_tool_providers = function(dev_roots = character(0)) {
      list(providers = list(
        list(package = "Certara.RsNLME", manifest = list(status_hook = "fake_status"))
      ), skipped = list())
    }
  )
  local_mocked_bindings(
    getExportedValue = function(ns, name) function(project_dir) list(recommended_next_phase = "vpc"),
    .package = "base"
  )
  out <- get_certara_project_status(project_dir = ".")
  expect_equal(out$next_gated_phase$package, "Certara.RsNLME")
  expect_equal(out$next_gated_phase$phase, "vpc")
})

test_that("get_certara_project_status returns no next_gated_phase when no provider offers one", {
  local_mocked_bindings(
    .mcp_discover_tool_providers = function(dev_roots = character(0)) {
      list(providers = list(), skipped = list())
    }
  )
  out <- get_certara_project_status(project_dir = ".")
  expect_length(out$providers, 0)
  expect_null(out$next_gated_phase)
})

test_that(".mcp_repro_project_mismatch is FALSE when the repro path is unset", {
  mcp_repro_reset()
  on.exit(mcp_repro_reset(), add = TRUE)
  # No path ever set: mcp_repro_path() lazily resolves under tempdir(), which
  # is never "under" an arbitrary project dir, but an unset recorder simply
  # has not started yet -- that is not the same failure as a stale one.
  expect_false(.mcp_repro_project_mismatch(withr::local_tempdir()))
})

test_that(".mcp_repro_project_mismatch is FALSE when the repro script is rooted under project_dir", {
  mcp_repro_reset()
  on.exit(mcp_repro_reset(), add = TRUE)
  proj <- withr::local_tempdir()
  mcp_repro_path(file.path(proj, "scripts", "certara_mcp_repro.R"))
  expect_false(.mcp_repro_project_mismatch(proj))
})

test_that(".mcp_repro_project_mismatch is TRUE when the repro script lives elsewhere", {
  mcp_repro_reset()
  on.exit(mcp_repro_reset(), add = TRUE)
  other <- withr::local_tempdir()
  mcp_repro_path(file.path(other, "certara_mcp_repro.R"))
  proj <- withr::local_tempdir()
  expect_true(.mcp_repro_project_mismatch(proj))
})

test_that("get_certara_project_status surfaces repro_project_mismatch and warns in note", {
  local_mocked_bindings(
    .mcp_discover_tool_providers = function(dev_roots = character(0)) {
      list(providers = list(), skipped = list())
    }
  )
  mcp_repro_reset()
  on.exit(mcp_repro_reset(), add = TRUE)
  other <- withr::local_tempdir()
  mcp_repro_path(file.path(other, "certara_mcp_repro.R"))
  proj <- withr::local_tempdir()

  out <- get_certara_project_status(project_dir = proj)
  expect_true(out$repro_project_mismatch)
  expect_match(out$note, "WARNING", fixed = TRUE)
})
