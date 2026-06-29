# Shared test helpers for Certara.R.

# Absolute path to a test fixture under tests/testthat/fixtures/.
# Populated in Phase 1b with a tiny MCP knowledge-base slice so engine KB
# tests do not depend on any provider package being installed.
fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}
