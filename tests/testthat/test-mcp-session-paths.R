# Session-root path contract (mcp_session_paths.R).

test_that("mcp_session_project_dir creates subdirs and returns root", {
  mcp_session_paths_reset()
  root <- file.path(tempdir(), "mcp_sess_test")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_equal(mcp_session_project_dir(root), gsub("\\\\", "/", root))
  expect_true(dir.exists(file.path(root, "scripts")))
  expect_true(dir.exists(file.path(root, "figures")))
  expect_true(dir.exists(file.path(root, "reports")))
  expect_true(dir.exists(file.path(root, "models")))
})

test_that("dir helpers return NULL without project root", {
  mcp_session_paths_reset()
  expect_null(mcp_session_scripts_dir())
  expect_null(mcp_session_figures_dir())
  expect_null(mcp_session_reports_dir())
  expect_null(mcp_session_models_dir())
})

test_that("mcp_repro_path defaults under scripts/ when project root is set", {
  mcp_repro_reset()
  mcp_session_paths_reset()
  root <- file.path(tempdir(), "mcp_repro_root")
  on.exit({
    unlink(root, recursive = TRUE)
    mcp_repro_reset()
    mcp_session_paths_reset()
  }, add = TRUE)
  mcp_session_project_dir(root)
  path <- mcp_repro_path()
  expect_equal(gsub("\\\\", "/", path),
               gsub("\\\\", "/", file.path(root, "scripts", "certara_mcp_repro.R")))
})

test_that("setting project dir validates input", {
  expect_error(mcp_session_project_dir(""), "single non-empty")
})
