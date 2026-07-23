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

test_that("provider_groups gives Certara.RDarwin and Certara.RsNLME their own vocabulary", {
  exec_groups <- .mcp_resolve_profile("execution")$provider_groups
  expect_true("results" %in% exec_groups[["Certara.RDarwin"]])
  expect_false("results" %in% exec_groups[["*"]])
  expect_true("qualification" %in% exec_groups[["Certara.RsNLME"]])
  expect_false("qualification" %in% exec_groups[["*"]])
  # A provider with no explicit entry falls back to "*".
  expect_identical(
    .mcp_resolve_provider_group_request(exec_groups, "tidyvpc"),
    exec_groups[["*"]]
  )
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
