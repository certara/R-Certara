# Launch-profile tests (Phase 2.5b): host-group filtering and the builder
# group-intersection decision. Declarative tool providers are no longer
# supported, so there is no separate declarative group-filtering path.

host_names <- function(...) vapply(.certara_host_tools(...), function(t) t@name, character(1))

test_that(".mcp_resolve_profile returns specs and rejects unknown profiles", {
  expect_named(.mcp_tool_profiles(),
               c("full", "core", "authoring", "execution", "diagnostics"),
               ignore.order = TRUE)
  expect_null(.mcp_resolve_profile("full")$provider_groups)        # full = no filter
  expect_true("data" %in% .mcp_resolve_profile("core")$provider_groups[["*"]])
  expect_false("memory" %in% .mcp_resolve_profile("core")$host)    # core drops memory
  expect_error(.mcp_resolve_profile("nope"))
})

test_that("provider_groups gives Certara.RDarwin, Certara.RsNLME, and tidyvpc their own vocabulary", {
  exec_groups <- .mcp_resolve_profile("execution")$provider_groups
  expect_true("results" %in% exec_groups[["Certara.RDarwin"]])
  expect_false("results" %in% exec_groups[["*"]])
  expect_true("qualification" %in% exec_groups[["Certara.RsNLME"]])
  expect_false("qualification" %in% exec_groups[["*"]])
  # tidyvpc's build/plot/meta groups are not in the host "*" vocabulary; an
  # explicit entry keeps the VPC lifecycle reachable under execution/
  # diagnostics (stats/qpc_score stays full-only).
  expect_true(all(c("data", "build", "plot", "meta") %in% exec_groups[["tidyvpc"]]))
  expect_false("stats" %in% exec_groups[["tidyvpc"]])
  expect_identical(
    .mcp_resolve_provider_group_request(exec_groups, "tidyvpc"),
    exec_groups[["tidyvpc"]]
  )
  diag_groups <- .mcp_resolve_profile("diagnostics")$provider_groups
  expect_identical(
    .mcp_resolve_provider_group_request(diag_groups, "tidyvpc"),
    c("data", "build", "plot", "meta")
  )
})

test_that("diagnostics profile keeps each provider's build/plot lifecycle non-empty", {
  skip_if_not_installed("tidyvpc")
  skip_if_not_installed("Certara.Xpose.NLME")
  diag <- .mcp_resolve_profile("diagnostics")$provider_groups
  # tidyvpc: load + build + plot must all be selected.
  tv_req <- .mcp_resolve_provider_group_request(diag, "tidyvpc")
  tv_offered <- eval(formals(tidyvpc::tidyvpc_mcp_tools)[["groups"]])
  tv_use <- intersect(tv_req, tv_offered)
  expect_true(all(c("data", "build", "plot") %in% tv_use))
  tv_tools <- tidyvpc::tidyvpc_mcp_tools(groups = tv_use)
  tv_names <- vapply(tv_tools, function(t) t@name, character(1))
  expect_true(any(grepl("load", tv_names)))
  expect_true(any(grepl("build", tv_names)))
  expect_true(any(grepl("plot", tv_names)))
  expect_false(any(grepl("qpc", tv_names)))

  # Xpose: interpretation + comparison under diagnostics via "*".
  xp_req <- .mcp_resolve_provider_group_request(diag, "Certara.Xpose.NLME")
  xp_offered <- eval(formals(Certara.Xpose.NLME::xpose_mcp_tools)[["groups"]])
  xp_use <- intersect(xp_req, xp_offered)
  expect_true(length(xp_use) >= 1L)
  expect_true(length(Certara.Xpose.NLME::xpose_mcp_tools(groups = xp_use)) >= 1L)
})

test_that(".mcp_resolve_provider_group_request supports all three provider_groups shapes", {
  expect_null(.mcp_resolve_provider_group_request(NULL, "Certara.RsNLME"))
  expect_identical(.mcp_resolve_provider_group_request(c("data", "execution"), "Certara.RsNLME"),
                   c("data", "execution"))
  named <- list("*" = c("data"), "Certara.RDarwin" = c("results"))
  expect_identical(.mcp_resolve_provider_group_request(named, "Certara.RDarwin"), "results")
  expect_identical(.mcp_resolve_provider_group_request(named, "tidyvpc"), "data")
  no_fallback <- list("Certara.RDarwin" = c("results"))
  expect_null(.mcp_resolve_provider_group_request(no_fallback, "tidyvpc"))
})

test_that("host tool groups filter correctly and always keep meta tools", {
  meta_only <- host_names(groups = "meta")
  expect_true("find_certara_tools" %in% meta_only)
  expect_true("certara_mcp_capabilities" %in% meta_only)
  expect_false("search_certara_kb" %in% meta_only)   # knowledge
  expect_false("record_lesson" %in% meta_only)       # memory

  core <- host_names(groups = c("meta", "knowledge"))
  expect_true("search_certara_kb" %in% core)
  expect_false("record_lesson" %in% core)

  expect_true("record_lesson" %in% host_names())     # default = all groups
})

test_that(".mcp_builder_call_groups intersects requested groups with what the builder offers", {
  b_plain <- function() list()
  b_grouped <- function(groups = c("knowledge", "data", "execution")) list()

  expect_identical(.mcp_builder_call_groups(b_plain, c("data"))$mode, "all")     # can't filter
  expect_identical(.mcp_builder_call_groups(b_grouped, NULL)$mode, "all")        # no request
  hit <- .mcp_builder_call_groups(b_grouped, c("data", "comparison"))
  expect_identical(hit$mode, "groups")
  expect_identical(hit$groups, "data")                                           # intersection only
  expect_identical(.mcp_builder_call_groups(b_grouped, c("comparison"))$mode, "none")  # disjoint -> exclude
})
