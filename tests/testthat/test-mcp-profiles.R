# Launch-profile tests (Phase 2.5b): host-group filtering, the builder
# group-intersection decision, and declarative group filtering.

host_names <- function(...) vapply(.certara_host_tools(...), function(t) t@name, character(1))

test_that(".mcp_resolve_profile returns specs and rejects unknown profiles", {
  expect_named(.mcp_tool_profiles(),
               c("full", "core", "authoring", "execution", "diagnostics"),
               ignore.order = TRUE)
  expect_null(.mcp_resolve_profile("full")$provider_groups)        # full = no filter
  expect_true("data" %in% .mcp_resolve_profile("core")$provider_groups)
  expect_false("memory" %in% .mcp_resolve_profile("core")$host)    # core drops memory
  expect_error(.mcp_resolve_profile("nope"))
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

test_that("declarative provider tools are filtered by group", {
  # A fake declarative provider whose handlers resolve to a real exported fn.
  prov <- list(
    mode = "declarative", package = "Certara.R",
    manifest = list(tools = list(
      list(name = "t_data", description = "d", handler = "list_certara_kb_packages", group = "data"),
      list(name = "t_exec", description = "d", handler = "list_certara_kb_packages", group = "execution")
    ))
  )
  keep <- .mcp_build_provider_tools(prov, provider_groups = "data")
  expect_length(keep, 1)
  expect_identical(keep[[1]]@name, "t_data")
  expect_length(.mcp_build_provider_tools(prov, provider_groups = NULL), 2)  # full
})
