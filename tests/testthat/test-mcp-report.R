# Report Rmd accumulator (mcp_report.R).

test_that("mcp_report_init creates scaffolded Rmd", {
  mcp_report_reset()
  mcp_session_paths_reset()
  root <- file.path(tempdir(), "mcp_report_test")
  on.exit({
    unlink(root, recursive = TRUE)
    mcp_report_reset()
    mcp_session_paths_reset()
  }, add = TRUE)
  mcp_session_project_dir(root)
  path <- mcp_report_init("Test report", stem = "test")
  expect_true(file.exists(path))
  txt <- mcp_report_read()
  expect_match(txt, "title: \"Test report\"")
  expect_match(txt, "# Overview")
  expect_match(txt, "## Goodness-of-fit diagnostics")
})

test_that("mcp_report_figure auto-inits and stores relative path", {
  mcp_report_reset()
  mcp_session_paths_reset()
  root <- file.path(tempdir(), "mcp_fig_test")
  on.exit({
    unlink(root, recursive = TRUE)
    mcp_report_reset()
    mcp_session_paths_reset()
  }, add = TRUE)
  mcp_session_project_dir(root)
  fig <- file.path(root, "figures", "gof_dv_vs_pred.png")
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  writeLines("png", fig)
  mcp_report_figure(fig, "DV vs PRED", section = "diagnostics.gof", key = "gof_dv")
  txt <- mcp_report_read()
  expect_match(txt, "include_graphics", fixed = FALSE)
  expect_match(txt, "```\\{r figure-gof_dv", fixed = FALSE)
  expect_match(txt, "```", fixed = TRUE)
  expect_match(txt, "gof_dv_vs_pred.png")
  expect_match(txt, "fig.cap = 'DV vs PRED'")
})

test_that("re-running figure with same key does not duplicate", {
  mcp_report_reset()
  root <- file.path(tempdir(), "mcp_fig_dup")
  on.exit({
    unlink(root, recursive = TRUE)
    mcp_report_reset()
    mcp_session_paths_reset()
  }, add = TRUE)
  mcp_session_project_dir(root)
  fig <- file.path(root, "figures", "vpc.png")
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  writeLines("png", fig)
  mcp_report_figure(fig, "VPC", section = "diagnostics.vpc", key = "vpc1")
  mcp_report_figure(fig, "VPC updated", section = "diagnostics.vpc", key = "vpc1")
  txt <- mcp_report_read()
  expect_equal(lengths(gregexpr("include_graphics", txt))[[1]], 1L)
  expect_match(txt, "VPC updated")
})

test_that("sections render in registry order regardless of insertion order", {
  mcp_report_reset()
  mcp_report_init("Order test")
  mcp_report_figure(tempfile(fileext = ".png"), "VPC", section = "diagnostics.vpc",
                    key = "v")
  mcp_report_figure(tempfile(fileext = ".png"), "GOF", section = "diagnostics.gof",
                    key = "g")
  txt <- mcp_report_read()
  expect_lt(regexpr("Goodness-of-fit", txt), regexpr("Visual predictive", txt))
})

test_that("mcp_report_text replaces by key", {
  mcp_report_reset()
  mcp_report_init("Text test")
  mcp_report_text("First", section = "methods", key = "m1")
  mcp_report_text("Second", section = "methods", key = "m1")
  txt <- mcp_report_read()
  expect_match(txt, "Second")
  expect_no_match(txt, "First")
})

test_that("mcp_report_chunk emits fenced runnable chunks", {
  mcp_report_reset()
  mcp_report_init("Chunk test")
  mcp_report_chunk("x <- 1\nx", section = "results.parameters", key = "x")
  txt <- mcp_report_read()
  expect_match(txt, "```\\{r chunk-x", fixed = FALSE)
  expect_match(txt, "x <- 1", fixed = TRUE)
  expect_match(txt, "```", fixed = TRUE)
})

test_that("report_rmd and vpc_two_step are host capability rules", {
  cap <- certara_mcp_capabilities()
  expect_true("report_rmd" %in% names(cap$rules))
  expect_true("vpc_two_step" %in% names(cap$rules))
})

test_that("render_certara_report warns without rmarkdown", {
  skip_if(requireNamespace("rmarkdown", quietly = TRUE))
  mcp_report_reset()
  mcp_report_init("No render")
  expect_warning(render_certara_report(), "rmarkdown")
})
