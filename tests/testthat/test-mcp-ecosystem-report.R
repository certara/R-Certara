# Ecosystem report + VPC integration smoke test.

test_that("guide_pharmacometrics resolves a PPP&D query to the PPP&D playbook", {
  # Regression for the MCP UX review: before the PPP&D playbook/routing were
  # added, guide_pharmacometrics("sequential PK then PD", ...) resolved only
  # to covariate-model/VPC guidance, with no path to the isSequential/
  # isPkFrozen staging recipe.
  skip_if_not_installed("Certara.RsNLME")
  Certara.R:::.kb_build_index(refresh = TRUE)
  # Guard on KB-entry presence so this host test does not fail against a
  # released Certara.RsNLME that predates the PPP&D playbook (skip, not fail).
  entry <- tryCatch(
    Certara.R::get_certara_kb_entry("Certara.RsNLME.workflow.pppd_sequential"),
    error = function(e) NULL
  )
  skip_if(is.null(entry), "PPP&D playbook not in installed Certara.RsNLME KB")
  expect_equal(entry$id, "Certara.RsNLME.workflow.pppd_sequential")

  res <- Certara.R::guide_pharmacometrics("sequential PK then PD PPP&D frozen PK")
  chapter_ids <- vapply(res$guidance_chapters, function(c) c$id, character(1))
  expect_true("Certara.RsNLME.guidance.structural_model" %in% chapter_ids)

  step_ids <- vapply(res$steps_resolved, function(s) s$id %||% s, character(1))
  expect_true("Certara.RsNLME.workflow.pppd_sequential" %in% step_ids)
  expect_true(
    "Certara.RsNLME.antipattern.pppd_stage_flag_conflict" %in% res$anti_patterns)
})

test_that("tidyvpc bridge related ids resolve when provider KB is indexed", {
  skip_if_not_installed("tidyvpc")
  idx <- Certara.R:::.kb_build_index(refresh = TRUE)
  entry <- Certara.R::get_certara_kb_entry("Certara.RsNLME.workflow.vpc_tidyvpc")
  expect_equal(entry$id, "Certara.RsNLME.workflow.vpc_tidyvpc")
  expect_true("tidyvpc.workflow.standard_vpc" %in% unlist(entry$related))
  tidy <- Certara.R::get_certara_kb_entry("tidyvpc.guidance.toc")
  expect_equal(tidy$package, "tidyvpc")
})

test_that("GOF and VPC figures co-locate in one report Rmd", {
  skip_if_not_installed("Certara.Xpose.NLME")
  skip_if_not_installed("tidyvpc")
  mcp_repro_reset()
  mcp_report_reset()
  mcp_session_paths_reset()
  root <- file.path(tempdir(), "ecosystem_report_e2e")
  mcp_session_project_dir(root)
  on.exit({
    unlink(root, recursive = TRUE)
    mcp_repro_reset()
    mcp_report_reset()
    mcp_session_paths_reset()
  }, add = TRUE)
  # VPC figure
  obs <- tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ]
  sim <- tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ]
  tidyvpc:::.tidyvpc_state_reset()
  h <- tidyvpc:::.tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL))
  tidyvpc:::.tv_build_vpc(h, x_col = "TIME", yobs_col = "DV", ysim_col = "DV",
                          bin_col = "NTIME", nbins = 6L)
  vpc_res <- tidyvpc:::.tv_plot_vpc(h)
  expect_true(file.exists(vpc_res$plot_path))
  # GOF figure via payload helper
  gof_fig <- file.path(mcp_session_figures_dir(), "gof_dv_vs_pred_xpdb1.png")
  writeLines("png", gof_fig)
  Certara.Xpose.NLME:::.xpose_plot_payload(
    gof_fig, "GOF: dv_vs_pred", section = "diagnostics.gof", key = "gof_dv"
  )
  txt <- mcp_report_read()
  expect_match(txt, "Goodness-of-fit")
  expect_match(txt, "Visual predictive")
  expect_equal(count_matches("include_graphics", txt), 2L)
  mcp_report_text("Methods narrative.", section = "methods", key = "m1")
  expect_match(mcp_report_read(), "Methods narrative")
  repro <- mcp_repro_read()
  expect_true(nzchar(repro))
  expect_silent(parse(text = repro))
})
