# Shared test helpers for Certara.R.

# Absolute path to a test fixture under tests/testthat/fixtures/.
# Populated in Phase 1b with a tiny MCP knowledge-base slice so engine KB
# tests do not depend on any provider package being installed.
fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

# Count non-overlapping occurrences of `pattern` in a single string `text`.
# `lengths(gregexpr(...))` is misleading for zero matches: gregexpr() returns
# the sentinel -1 (a length-1 vector) when nothing matches, so lengths()
# reports 1 instead of 0. Count real match positions from gregexpr()[[1]]
# instead.
count_matches <- function(pattern, text) {
  positions <- gregexpr(pattern, text)[[1]]
  sum(positions != -1L)
}
