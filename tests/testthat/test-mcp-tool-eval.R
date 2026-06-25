# Distractor-heavy tool-selection eval (Phase 2.5 #2), MCPAgentBench-style.
# Runs against the REAL full catalog (host + the installed Certara.RsNLME
# provider), so the overlapping clusters (start_nlme_* x5, validators x4,
# KB-query x4, the two job tools) are all present as distractors. Skipped when
# the NLME provider is not installed so a bare-host CI stays green.

build_full_index <- function() {
  tools <- c(.certara_host_tools(), .mcp_provider_tools()$tools)
  .mcp_set_tool_index(.mcp_build_tool_index(tools))
  vapply(tools, function(t) t@name, character(1))
}

rank_of <- function(query, tool, limit = 7) {
  res <- find_certara_tools(query, limit = limit)
  r <- match(tool, vapply(res$tools, function(t) t$name, character(1)))
  if (is.na(r)) Inf else r   # not retrieved -> rank infinity
}

test_that("find_certara_tools selects the right tool amid real distractor clusters (hit@5)", {
  skip_if_not_installed("Certara.RsNLME")
  catalog <- build_full_index()
  on.exit(.kb_reset_index(), add = TRUE)
  # Only meaningful when the builder provider actually contributed its tools.
  skip_if_not("start_nlme_fit_spec" %in% catalog,
              "Certara.RsNLME tool provider not discovered")

  cases <- list(
    list(q = "fit a model from a declarative JSON spec, nothing built yet", e = "start_nlme_fit_spec"),
    list(q = "re-fit an already-built NlmePmlModel saved as an rds file",   e = "start_nlme_fitmodel"),
    list(q = "run a visual predictive check from a completed fit",          e = "start_nlme_vpcmodel"),
    list(q = "fit from a portable mmdl metamodel file",                     e = "start_nlme_fit"),
    list(q = "run a covariate search or bootstrap R script",               e = "start_nlme_job"),
    list(q = "validate PML code before fitting",                            e = "validate_pml"),
    list(q = "validate a mmdl metamodel file",                              e = "validate_mmdl"),
    list(q = "preflight a built NlmePmlModel rds before fitting",           e = "validate_nlme_model"),
    list(q = "validate a fit spec JSON without launching a job",            e = "validate_fit_spec"),
    list(q = "search the knowledge base for stparm PML syntax",             e = "search_certara_kb"),
    list(q = "find runnable example code",                                  e = "find_certara_examples"),
    list(q = "wait for a running fit job to finish",                        e = "wait_for_nlme_job"),
    list(q = "check the status of a job once",                              e = "get_nlme_job_status"),
    list(q = "compare candidate models by AIC BIC",                         e = "compare_nlme_jobs"),
    list(q = "inspect a PK dataset for EDA",                                e = "inspect_pk_dataset")
  )

  ranks <- vapply(cases, function(cc) rank_of(cc$q, cc$e), numeric(1))
  hit5 <- ranks <= 5
  # Report any miss precisely.
  for (i in which(!hit5)) {
    fail(sprintf("expected '%s' in top-5 for query '%s' (rank %s)",
                 cases[[i]]$e, cases[[i]]$q, ifelse(is.infinite(ranks[i]), ">7", ranks[i])))
  }
  expect_true(all(hit5))
  # hit@3 is a quality signal, not a hard gate.
  expect_gte(mean(ranks <= 3), 0.8)
})

test_that("the two job tools disambiguate by intent (wait vs single snapshot)", {
  skip_if_not_installed("Certara.RsNLME")
  catalog <- build_full_index()
  on.exit(.kb_reset_index(), add = TRUE)
  skip_if_not("wait_for_nlme_job" %in% catalog,
              "Certara.RsNLME tool provider not discovered")

  # "wait/block" should prefer the blocking watch; "once/snapshot" the single shot.
  expect_lt(rank_of("wait for a running fit to finish", "wait_for_nlme_job"),
            rank_of("wait for a running fit to finish", "get_nlme_job_status"))
  expect_lt(rank_of("check job status once as a single snapshot", "get_nlme_job_status"),
            rank_of("check job status once as a single snapshot", "wait_for_nlme_job"))
})
