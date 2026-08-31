# Fixture-based agent-facing contract regression suite (W6).
# Asserts the response shape agents rely on rather than internals.

test_that("profile coverage: diagnostics keeps tidyvpc build/plot lifecycle", {
  # Would have caught B4 (tidyvpc starved to loaders-only under diagnostics).
  skip_if_not_installed("tidyvpc")
  skip_if_not_installed("ellmer")
  diag <- .mcp_resolve_profile("diagnostics")$provider_groups
  tv_req <- .mcp_resolve_provider_group_request(diag, "tidyvpc")
  expect_true(all(c("data", "build", "plot", "meta") %in% tv_req))
  expect_false("stats" %in% tv_req)

  offered <- eval(formals(tidyvpc::tidyvpc_mcp_tools)[["groups"]])
  use <- intersect(tv_req, offered)
  tools <- tidyvpc::tidyvpc_mcp_tools(groups = use)
  nms <- vapply(tools, function(t) t@name, character(1))
  expect_true(any(grepl("load", nms)))
  expect_true(any(grepl("build", nms)))
  expect_true(any(grepl("plot", nms)))
  expect_false(any(grepl("qpc", nms)))
})

test_that("unknown engine key contract: valid/error_class/suggestion", {
  skip_if_not_installed("Certara.RsNLME")
  csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(ID = 1, time = 0, amt = 1, dv = 1), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  res <- Certara.RsNLME::validate_fit_spec(list(
    constructor = "pkmodel",
    data = csv,
    column_map = list(ID = "ID", Time = "time", A1 = "amt", CObs = "dv"),
    engine = list(stderr = "Sandwich")
  ))
  expect_false(res$valid)
  errs <- unlist(res$errors)
  expect_true(any(grepl("engine_unknown_key", errs)))
  expect_true(any(grepl("Did you mean 'stdErr'", errs)))
})

test_that("constructor mode conflict contract exposes error_class", {
  skip_if_not_installed("Certara.RsNLME")
  csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(ID = 1, time = 0, amt = 1, dv = 1, wt = 70),
            csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  res <- Certara.RsNLME::validate_fit_spec(list(
    constructor = "pklinearmodel",
    data = csv,
    constructor_args = list(isSequential = TRUE, isPkFrozen = TRUE),
    column_map = list(ID = "ID", Time = "time", A1 = "amt", CObs = "dv",
                      EObs = "wt")
  ))
  expect_false(res$valid)
  expect_true(any(grepl("constructor_mode_conflict", unlist(res$errors))))
})

test_that("Darwin all-crash contract: state/error_class/next_action/no winner", {
  skip_if_not_installed("Certara.RDarwin")
  project_dir <- tempfile("darwin-contract-")
  run_dir <- file.path(project_dir, "darwin-mcp-runs", "job-1")
  output_dir <- file.path(run_dir, "wd", "output")
  dir.create(output_dir, recursive = TRUE)
  on.exit(unlink(project_dir, recursive = TRUE), add = TRUE)

  Certara.RDarwin:::.darwin_write_json(
    list(state = "succeeded", pid = NA, exit_code = 0L),
    file.path(run_dir, "status.json"))
  Certara.RDarwin:::.darwin_write_json(
    list(algorithm = "GA", crash_value = 99999999),
    file.path(run_dir, "options.json"))
  Certara.RDarwin:::.darwin_write_json(
    list(job_kind = "darwin_search", project_dir = project_dir,
         algorithm = "GA", engine_adapter = "nlme", output_dir = output_dir),
    file.path(run_dir, "run_context.json"))
  utils::write.csv(
    data.frame(iteration = 1, `model number` = 1, `run directory` = "r",
               status = "done", ntheta = 1, nomega = 1, nsigm = 1, model = "1",
               fitness = 99999999, ofv = 99999999, `r penalty` = 0,
               `python penalty` = 0, `condition num` = 1, success = "False",
               covariance = "False", correlation = "False",
               `translation messages` = "", `runtime errors` = "",
               check.names = FALSE),
    file.path(output_dir, "results.csv"), row.names = FALSE)
  root <- Certara.RDarwin:::.darwin_run_root(project_dir)
  Certara.RDarwin:::.darwin_register_job(root, list(
    job_id = "job-1", run_dir = run_dir, label = "GA",
    created = "2024-01-01T00:00:00+0000", pid = NA, create_time = NA,
    algorithm = "GA", engine_adapter = "nlme", project_dir = project_dir))

  status <- Certara.RDarwin::get_darwin_job_status("job-1", project_dir)
  if (!identical(status$state, "completed_no_viable_models")) {
    skip(paste(
      "Installed Certara.RDarwin does not reclassify all-crash runs;",
      "reinstall from the R-Darwin working tree (landing order: Darwin before host)."))
  }
  expect_equal(status$state, "completed_no_viable_models")
  expect_equal(status$failure_reason, "all_crash")

  collected <- Certara.RDarwin::collect_darwin_search("job-1", project_dir)
  expect_null(collected$fitness)
  expect_equal(collected$error_class, "all_crash")
  expect_true(!is.null(collected$next_action))

  proj <- Certara.RDarwin::darwin_project_status(project_dir)
  expect_equal(proj$by_state$completed_no_viable_models, 1L)
  expect_true(any(vapply(proj$needs_attention, function(r) {
    identical(r$state, "completed_no_viable_models")
  }, logical(1))))
})

test_that("repro path under tempdir reports durable=FALSE contract", {
  mcp_session_paths_reset()
  mcp_repro_reset()
  on.exit({
    mcp_session_paths_reset()
    mcp_repro_reset()
  }, add = TRUE)
  mcp_session_project_dir(tempdir())
  info <- mcp_repro_info()
  expect_false(isTRUE(info$durable))
  expect_true(!is.null(info$next_action$tool))
})

test_that("workflow phase table is rendered from capability fragments", {
  caps <- certara_mcp_capabilities()
  lines <- .mcp_render_workflow_phase_table(caps[["workflows"]])
  expect_true(any(grepl("Certara\\.RsNLME|Certara\\.RDarwin|tidyvpc", lines)))
  expect_true(any(grepl("^\\| Phase \\| Tools \\|$", lines)))
  # Placeholder in the shipped rule template must remain substitutable.
  tmpl <- system.file("mcp", "certara-mcp-usage.mdc", package = "Certara.R")
  if (!nzchar(tmpl)) {
    tmpl <- file.path(testthat::test_path(), "../../inst/mcp/certara-mcp-usage.mdc")
  }
  skip_if_not(file.exists(tmpl))
  body <- paste(readLines(tmpl, warn = FALSE), collapse = "\n")
  expect_true(grepl("__WORKFLOW_PHASE_TABLE__", body, fixed = TRUE))
})
