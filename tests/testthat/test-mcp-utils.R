test_that(".mcp_parse_json_arg parses a JSON object/array passed as text", {
  expect_equal(.mcp_parse_json_arg('{"a": 1, "b": "x"}'), list(a = 1, b = "x"))
  expect_equal(.mcp_parse_json_arg("[1, 2, 3]"), list(1, 2, 3))
})

test_that(".mcp_parse_json_arg treats NULL/empty as NULL", {
  expect_null(.mcp_parse_json_arg(NULL))
  expect_null(.mcp_parse_json_arg(""))
  expect_null(.mcp_parse_json_arg("   "))
})

test_that(".mcp_parse_json_arg passes an already-a-list value through unchanged", {
  x <- list(job_id = "j1")
  expect_identical(.mcp_parse_json_arg(x), x)
})

test_that(".mcp_parse_json_arg keeps unparsable text as a best-effort raw object", {
  out <- .mcp_parse_json_arg("not json at all {")
  expect_equal(out, list(raw = "not json at all {"))
})
