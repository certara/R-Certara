# Tests for tool-provider discovery and materialization. Fixtures under
# fixtures/tool-provider* use exported host functions as stand-in handlers so the
# declarative and builder paths can be exercised without a separate package.

tp_dec <- testthat::test_path("fixtures", "tool-provider")
tp_bld <- testthat::test_path("fixtures", "tool-provider-builder")
tp_bad <- testthat::test_path("fixtures", "tool-provider-bad")

test_that("tools-manifest schema compatibility follows the shared semver rule", {
  expect_true(.mcp_tools_schema_compatible("1.0.0"))
  expect_true(.mcp_tools_schema_compatible("1.0.9"))
  expect_false(.mcp_tools_schema_compatible("9.9.9"))
  expect_false(.mcp_tools_schema_compatible("2.0.0"))
})

test_that("a declarative tool provider is discovered and classified", {
  disc <- .mcp_discover_tool_providers(dev_roots = tp_dec)
  p <- Filter(function(x) identical(x$package, "Certara.R"), disc$providers)
  expect_length(p, 1)
  expect_identical(p[[1]]$mode, "declarative")
  expect_identical(p[[1]]$tool_count, 2L)
})

test_that("an incompatible tool manifest is skipped and reported", {
  disc <- .mcp_discover_tool_providers(dev_roots = tp_bad)
  reasons <- vapply(disc$skipped, function(s) s$reason, character(1))
  expect_true(any(grepl("incompatible schema", reasons)))
})

test_that("declarative tools build into named ellmer ToolDefs", {
  res <- .mcp_provider_tools(dev_roots = tp_dec)
  expect_length(res$skipped, 0)
  nms <- vapply(res$tools, function(t) t@name, character(1))
  expect_true(all(c("fixture_list_memory", "fixture_kb_packages") %in% nms))
})

test_that("a builder-mode provider returns the builder's tools", {
  res <- .mcp_provider_tools(dev_roots = tp_bld)
  expect_gt(length(res$tools), 0)
  nms <- vapply(res$tools, function(t) t@name, character(1))
  expect_true("certara_mcp_capabilities" %in% nms)
})

test_that("gated tool names come from declarative gated:true entries only", {
  expect_identical(.mcp_gated_tool_names(dev_roots = tp_dec), "fixture_kb_packages")
})

test_that(".mcp_arg_type maps known types and rejects unknown ones", {
  expect_false(is.null(.mcp_arg_type(list(type = "string", required = TRUE))))
  expect_false(is.null(.mcp_arg_type(list(type = "integer"))))
  expect_false(is.null(.mcp_arg_type(list(type = "array", items = "string"))))
  expect_error(.mcp_arg_type(list(type = "weird")), "unsupported")
  expect_error(.mcp_arg_type(list(type = 1)), "unsupported argument type")
  expect_error(.mcp_arg_type(list()), "unsupported argument type")
})

test_that(".mcp_validate_tools_manifest rejects non-scalar-string builder values", {
  base <- list(package = "P", schema_version = "1.0.0")
  prob_arr <- .mcp_validate_tools_manifest(c(base, list(builder = list("fn"))))
  prob_obj <- .mcp_validate_tools_manifest(c(base, list(builder = list(fn = "x"))))
  expect_true(any(grepl("either 'builder' or 'tools'", prob_arr)))
  expect_true(any(grepl("either 'builder' or 'tools'", prob_obj)))
})

test_that("discovery skips manifests whose builder is not a scalar string", {
  tmp <- tempfile("tp_badbuilder_")
  mdir <- file.path(tmp, "inst", "mcp", "tools")
  dir.create(mdir, recursive = TRUE)
  writeLines(
    '{"package":"BadBuilder","schema_version":"1.0.0","builder":["not","scalar"]}',
    file.path(mdir, "manifest.json")
  )
  disc <- .mcp_discover_tool_providers(dev_roots = tmp)
  reasons <- vapply(disc$skipped, function(s) s$reason, character(1))
  expect_true(any(grepl("either 'builder' or 'tools'", reasons)))
})
