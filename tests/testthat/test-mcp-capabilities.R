# Tests for the provider-agnostic capabilities host and capability-fragment merge.

cap_root <- testthat::test_path("fixtures", "cap-provider")

test_that("a capability fragment is discovered and validated", {
  disc <- .mcp_discover_capabilities(dev_roots = cap_root)
  pkgs <- vapply(disc$fragments, function(f) f$package, character(1))
  expect_true("Certara.Fixture" %in% pkgs)
  expect_length(disc$skipped, 0)
})

test_that("the pure fragment merge implements the documented contract", {
  # Deterministic: controlled fragments, independent of what is installed.
  frags <- list(
    list(package = "A",
         rules = list(list(id = "providers_first", text = "OVR"),
                      list(id = "a_rule", text = "x")),
         primary_guidance_entry = "A.guidance.toc",
         session_start = list("s1"), prerequisites = list("p1"),
         workflows = list(phases = list())),
    list(package = "B",
         rules = list(list(id = "b_rule", text = "y")),
         primary_guidance_entry = "B.guidance.toc",
         session_start = list("s1", "s2"), prerequisites = list("p2"))
  )
  m <- .mcp_merge_capability_fragments(frags)
  expect_identical(m$primary_guidance_entry, "A.guidance.toc")        # first wins
  expect_identical(m$rules[["providers_first"]], "OVR")               # override by id
  expect_true(all(c("a_rule", "b_rule") %in% names(m$rules)))         # union of rules
  expect_setequal(m$session_start, c("s1", "s2"))                     # concat + dedupe
  expect_setequal(m$prerequisites, c("p1", "p2"))
  expect_false(is.null(m$workflows[["A"]]))                           # per-package
  expect_setequal(m$providers, c("A", "B"))
})

test_that("merge collects the fixture provider's rules, session_start, prerequisites, workflows", {
  # Discovery-based: robust to other installed providers (e.g. Certara.RsNLME)
  # co-existing - assert the fixture's content is present, not that it is sole.
  m <- .mcp_merge_capabilities(dev_roots = cap_root)
  expect_true("fixture_domain" %in% names(m$rules))
  expect_false(is.null(m$primary_guidance_entry))
  expect_true("Fixture engine license." %in% m$prerequisites)
  expect_true("Load the fixture domain context." %in% m$session_start)
  expect_false(is.null(m$workflows[["Certara.Fixture"]]))
})

test_that("certara_mcp_capabilities merges fragments; provider wins on rule id", {
  on.exit(.kb_reset_index(), add = TRUE)
  cap <- certara_mcp_capabilities(dev_roots = cap_root)
  # Provider override of a host rule id wins.
  expect_identical(cap$rules[["providers_first"]],
                   "OVERRIDDEN host rule from the fixture provider.")
  # Provider-only rule is present, host rule without a provider override remains.
  expect_true("fixture_domain" %in% names(cap$rules))
  expect_true("memory_and_sources" %in% names(cap$rules))
  expect_false(is.null(cap$primary_guidance_entry))
  expect_true("Certara.Fixture" %in% cap$capability_providers)
})

test_that("certara_mcp_capabilities reports server identity, routing, and versions", {
  on.exit(.kb_reset_index(), add = TRUE)
  cap <- certara_mcp_capabilities()
  expect_identical(cap$server, "certara-r")
  expect_identical(cap$client_routing$cursor$call_mcp_server, "user-certara-r")
  expect_identical(cap$kb_schema_version, "1.0.0")
  expect_identical(cap$tools_schema_version, "1.0.0")
  expect_identical(cap$package_version,
                   as.character(utils::packageVersion("Certara.R")))
  # Generic host rules are always present even with no providers.
  expect_true(all(c("providers_first", "memory_and_sources") %in% names(cap$rules)))
  expect_identical(cap$execution_contexts$status_tool, "certara_session_status")
})

test_that(".mcp_validate_capability_fragment rejects non-scalar rule id/text", {
  frag <- list(
    package = "P",
    rules = list(list(id = c("a", "b"), text = "ok"))
  )
  prob <- .mcp_validate_capability_fragment(frag)
  expect_true(any(grepl("rules\\[1\\] needs id \\+ text", prob)))
})

test_that("discovery skips capability fragments with invalid rule shapes", {
  tmp <- tempfile("cap_badrule_")
  cdir <- file.path(tmp, "inst", "mcp")
  dir.create(cdir, recursive = TRUE)
  writeLines(
    '{"package":"BadCap","rules":[{"id":["not","scalar"],"text":"x"}]}',
    file.path(cdir, "capabilities.json")
  )
  disc <- .mcp_discover_capabilities(dev_roots = tmp)
  reasons <- vapply(disc$skipped, function(s) s$reason, character(1))
  expect_true(any(grepl("rules\\[1\\] needs id \\+ text", reasons)))
  # Valid fixture provider still loads alongside the bad one.
  good <- .mcp_discover_capabilities(dev_roots = cap_root)
  expect_true("Certara.Fixture" %in% vapply(good$fragments, function(f) f$package, character(1)))
})
