# Tests for tool-provider discovery and materialization. Only builder-mode
# providers are supported; the tool-provider-builder fixture uses an exported
# host function as the stand-in builder so the path can be exercised without a
# separate package. tool-provider-bad advertises an incompatible schema_version.
# The declarative-manifest-is-skipped integration scenario lives in
# test-mcp-contract-smoke.R, not duplicated here.

tp_bld <- testthat::test_path("fixtures", "tool-provider-builder")
tp_bad <- testthat::test_path("fixtures", "tool-provider-bad")

test_that("tools-manifest schema compatibility follows the shared semver rule", {
  expect_true(.mcp_tools_schema_compatible("1.0.0"))
  expect_true(.mcp_tools_schema_compatible("1.0.9"))
  expect_false(.mcp_tools_schema_compatible("9.9.9"))
  expect_false(.mcp_tools_schema_compatible("2.0.0"))
})

test_that("an incompatible tool manifest is skipped and reported", {
  disc <- .mcp_discover_tool_providers(dev_roots = tp_bad)
  reasons <- vapply(disc$skipped, function(s) s$reason, character(1))
  expect_true(any(grepl("incompatible schema", reasons)))
})

test_that("a builder-mode provider is discovered and returns its builder's tools", {
  disc <- .mcp_discover_tool_providers(dev_roots = tp_bld)
  p <- Filter(function(x) identical(x$package, "Certara.R"), disc$providers)
  expect_length(p, 1)
  expect_identical(p[[1]]$mode, "builder")

  res <- .mcp_provider_tools(dev_roots = tp_bld)
  expect_length(res$skipped, 0)
  nms <- vapply(res$tools, function(t) t@name, character(1))
  expect_true("certara_mcp_capabilities" %in% nms)
})

test_that(".mcp_validate_tools_manifest requires a scalar 'builder'", {
  base <- list(package = "P", schema_version = "1.0.0")
  # Missing builder entirely.
  prob_missing <- .mcp_validate_tools_manifest(base)
  expect_true(any(grepl("'builder'", prob_missing)))
  # Non-scalar builder.
  prob_arr <- .mcp_validate_tools_manifest(c(base, list(builder = list("fn"))))
  expect_true(any(grepl("'builder'", prob_arr)))
  # Object as builder.
  prob_obj <- .mcp_validate_tools_manifest(c(base, list(builder = list(fn = "x"))))
  expect_true(any(grepl("'builder'", prob_obj)))
})

test_that(".mcp_validate_tools_manifest rejects tools-only and tools+builder manifests", {
  base <- list(package = "P", schema_version = "1.0.0")
  tools <- list(list(name = "t", description = "d", handler = "h"))
  # tools-only (no builder)
  prob_tools_only <- .mcp_validate_tools_manifest(c(base, list(tools = tools)))
  expect_true(any(grepl("'tools'|declarative", prob_tools_only)))
  # tools + builder together
  prob_both <- .mcp_validate_tools_manifest(
    c(base, list(builder = "fn", tools = tools)))
  expect_true(any(grepl("'tools'|declarative", prob_both)))
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
  expect_true(any(grepl("'builder'", reasons)))
})

test_that(".mcp_gated_tool_names has no gating source for builder-only manifests", {
  # Builder manifests carry no per-tool metadata, so there is currently no way
  # for a provider to mark a tool gated; this always returns character(0).
  expect_identical(.mcp_gated_tool_names(dev_roots = tp_bld), character(0))
})

test_that("the shipped manifest JSON schema stays in sync with the builder-only runtime contract", {
  schema_path <- system.file("mcp", "tools", "tools-manifest.schema.json", package = "Certara.R")
  expect_true(nzchar(schema_path) && file.exists(schema_path))
  schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  # Matches .mcp_validate_tools_manifest(): 'builder' is required, and there
  # is no declarative 'tools' property left for provider authors to (mis)use.
  expect_true("builder" %in% unlist(schema$required))
  expect_null(schema$properties$tools)
  expect_null(schema$oneOf)
})
