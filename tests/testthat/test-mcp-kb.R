# Tests for the MCP knowledge-base schema, generation, discovery, and search.
# These exercise the generic engine against a small fixture KB slice under
# fixtures/mcp-kb so they do not depend on any provider package being installed.

kb_fixture_root <- testthat::test_path("fixtures", "mcp-kb")

test_that("schema version compatibility rule", {
  expect_true(.kb_schema_compatible("1.0.0", "1.0.0"))
  expect_true(.kb_schema_compatible("1.0.5", "1.2.0"))  # older minor ok
  expect_false(.kb_schema_compatible("1.3.0", "1.2.0")) # newer minor refused
  expect_false(.kb_schema_compatible("2.0.0", "1.2.0")) # different major
  expect_false(.kb_schema_compatible("garbage"))
})

test_that("entry validation flags missing fields and dangling refs", {
  good <- list(id = "x.y", package = "P", type = "pml_topic",
               title = "T", summary = "S",
               provenance = list(source_file = "f"))
  expect_length(.validate_kb_entry(good, known_ids = "x.y"), 0)

  bad <- list(id = "x.y", package = "P", type = "pml_topic", title = "T",
              summary = "S", provenance = list(source_file = "f"),
              related = list("P.does.not.exist"))
  expect_true(any(grepl("dangling", .validate_kb_entry(bad, known_ids = "x.y"))))

  # Cross-package related refs (a different package prefix) are NOT flagged by
  # the single-package gate; they resolve when the host merges KB slices.
  xpkg <- list(id = "P.a", package = "P", type = "pml_topic", title = "T",
               summary = "S", provenance = list(source_file = "f"),
               related = list("Certara.RsNLME.guidance.toc"))
  expect_length(.validate_kb_entry(xpkg, known_ids = "P.a"), 0)

  no_prov <- list(id = "z", package = "P", type = "function_doc",
                  title = "T", summary = "S")
  expect_true(any(grepl("provenance", .validate_kb_entry(no_prov))))

  anti <- list(id = "a", package = "P", type = "anti_pattern",
               title = "T", summary = "S")
  expect_true(any(grepl("error_signature", .validate_kb_entry(anti))))
})

test_that("markdown source parser extracts metadata, body, and examples", {
  md <- c(
    "# header",
    "",
    "## Pkg.topic.one",
    "type: pml_topic",
    "title: A topic",
    "summary: A summary.",
    "keywords: foo, bar",
    "symbols: stparm",
    "provenance.source_file: bnf.txt",
    "",
    "Body text here.",
    "",
    "```pml",
    "stparm(V = tvV)",
    "```"
  )
  f <- tempfile(fileext = ".md")
  writeLines(md, f)
  entries <- .parse_kb_source_file(f, package = "Pkg")
  expect_length(entries, 1)
  e <- entries[[1]]
  expect_identical(e$id, "Pkg.topic.one")
  expect_identical(e$title, "A topic")
  expect_identical(e$keywords, c("foo", "bar"))
  expect_identical(e$symbols, "stparm")
  expect_length(e$examples, 1)
  expect_match(e$examples[[1]]$code, "stparm")
})

test_that("discovery + index load the fixture KB via dev_roots", {
  idx <- .kb_build_index(dev_roots = kb_fixture_root, refresh = TRUE)
  on.exit(.kb_reset_index(), add = TRUE)
  # >= the four fixture entries (installed providers, if any, only add more).
  expect_gte(idx$n_docs, 4)
  expect_true("Certara.KBFixture.example.alpha" %in% names(idx$by_id))
  # The fixture provider loads without being skipped.
  skipped_pkgs <- vapply(idx$skipped, function(s) {
    if (is.null(s$package)) "" else s$package
  }, character(1))
  expect_false("Certara.KBFixture" %in% skipped_pkgs)
})

test_that("search_certara_kb ranks a matching fixture entry first", {
  .kb_build_index(dev_roots = kb_fixture_root, refresh = TRUE)
  on.exit(.kb_reset_index(), add = TRUE)
  res <- search_certara_kb("absorption clearance", package = "Certara.KBFixture")
  ids <- vapply(res, function(r) r$id, character(1))
  expect_true("Certara.KBFixture.example.alpha" %in% ids)
})

test_that("search_certara_kb honors limit (axis is the 7th arg, limit the 8th)", {
  # Regression: the host search tool wrapper must pass limit by NAME, not
  # positionally into `axis`. With limit misrouted to axis, results collapse to 0.
  .kb_build_index(dev_roots = kb_fixture_root, refresh = TRUE)
  on.exit(.kb_reset_index(), add = TRUE)
  res <- search_certara_kb("fixture", package = "Certara.KBFixture", limit = 2)
  expect_length(res, 2)
})

test_that("get_certara_kb_entry returns an explicit found contract", {
  .kb_build_index(dev_roots = kb_fixture_root, refresh = TRUE)
  on.exit(.kb_reset_index(), add = TRUE)

  hit <- get_certara_kb_entry("Certara.KBFixture.example.alpha")
  expect_true(isTRUE(hit$found))
  expect_identical(hit$id, "Certara.KBFixture.example.alpha")
  expect_true(is.character(hit$title) && nzchar(hit$title))

  miss <- get_certara_kb_entry("Certara.KBFixture.fn.does_not_exist")
  expect_false(isTRUE(miss$found))
  expect_null(miss$not_found)    # never the old bare-logical shape
  expect_true(is.character(miss$message) && nzchar(miss$message))
  expect_true(is.list(miss$suggestions))
})

test_that("incompatible manifest is skipped and reported", {
  tmp <- file.path(tempdir(), "badkb", "inst", "mcp", "kb")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(package = "BadPkg", schema_version = "9.9.9",
         files = list(list(path = "BadPkg.jsonl"))),
    file.path(tmp, "manifest.json"), auto_unbox = TRUE
  )
  writeLines("{}", file.path(tmp, "BadPkg.jsonl"))
  disc <- .kb_discover(dev_roots = file.path(tempdir(), "badkb"))
  reasons <- vapply(disc$skipped, function(s) s$reason, character(1))
  expect_true(any(grepl("incompatible schema", reasons)))
})
