# Tool-retrieval (Phase 2.5) tests + a distractor-style selection eval.
# Scoped to the host tools, which already contain a real overlapping cluster
# (the KB-query family: search_certara_kb / guide_pharmacometrics /
# find_certara_examples / explain_certara_workflow) so selection is tested
# against genuine distractors without needing an installed provider.

local_host_tool_index <- function(env = parent.frame()) {
  .mcp_set_tool_index(.mcp_build_tool_index(.certara_host_tools()))
  withr::defer(.kb_reset_index(), envir = env)
}

top_names <- function(task, limit = 5) {
  res <- find_certara_tools(task, limit = limit)
  vapply(res$tools, function(t) t$name, character(1))
}

test_that("find_certara_tools retrieves the right tool from an overlapping cluster", {
  local_host_tool_index()
  # exact-term queries should rank their tool first
  expect_identical(top_names("search the knowledge base for stparm PML syntax")[1],
                   "search_certara_kb")
  expect_identical(top_names("record a corrective lesson for next time")[1],
                   "record_lesson")
  # discriminating within the KB-query distractor cluster
  expect_true("find_certara_examples" %in%
                top_names("find runnable PML code examples", limit = 3))
  expect_true("guide_pharmacometrics" %in%
                top_names("guidance before covariate modeling decisions", limit = 3))
  expect_true("certara_mcp_capabilities" %in%
                top_names("what server capabilities and tool providers are available",
                          limit = 4))
})

test_that("find_certara_tools never recommends itself and returns scored hits", {
  local_host_tool_index()
  res <- find_certara_tools("which tool should I use to discover tools")
  nms <- vapply(res$tools, function(t) t$name, character(1))
  expect_false("find_certara_tools" %in% nms)
  if (length(res$tools)) {
    expect_true(all(vapply(res$tools, function(t) is.numeric(t$score), logical(1))))
    # scores are non-increasing
    sc <- vapply(res$tools, function(t) t$score, numeric(1))
    expect_false(is.unsorted(rev(sc)))
  }
})

test_that("limit is clamped to 3..10", {
  local_host_tool_index()
  expect_lte(length(find_certara_tools("certara", limit = 50)$tools), 10)
  # a broad query that matches many; limit = 1 floored to 3
  res <- find_certara_tools("certara knowledge model fit memory", limit = 1)
  expect_gte(length(res$tools), min(3, length(.certara_host_tools()) - 1))
})

test_that("find_certara_tools is registered as a host tool", {
  nms <- vapply(.certara_host_tools(), function(t) t@name, character(1))
  expect_true("find_certara_tools" %in% nms)
})
