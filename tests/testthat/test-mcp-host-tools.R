# Tests for the host-owned tool registrations (knowledge / memory / meta).

test_that(".certara_host_tools registers the meta, knowledge, and memory tools", {
  tools <- .certara_host_tools()
  nms <- vapply(tools, function(t) t@name, character(1))
  expect_true(all(c(
    "certara_mcp_capabilities", "list_certara_kb_packages", "certara_session_status",
    "search_certara_kb", "get_certara_kb_entry", "find_certara_examples",
    "guide_pharmacometrics", "lookup_pml_symbol", "list_pml_enums",
    "get_user_preferences", "get_lessons", "record_lesson", "set_preference",
    "list_memory_records"
  ) %in% nms))
})

test_that("the host registers no NLME provider tools itself", {
  nms <- vapply(.certara_host_tools(), function(t) t@name, character(1))
  expect_false(any(c("start_nlme_fit", "validate_mmdl", "inspect_pk_dataset",
                     "interpret_run", "compare_nlme_jobs",
                     "list_builtin_model_constructors") %in% nms))
})

test_that("search_certara_kb MCP tool exposes axis", {
  tools <- .certara_host_tools("knowledge")
  search <- Filter(function(t) identical(t@name, "search_certara_kb"), tools)[[1]]
  expect_true("axis" %in% names(formals(search)))
})

test_that("add_certara_report_note replaces in place when the same key is reused", {
  mcp_report_reset()
  mcp_report_init("Note test")
  on.exit(mcp_report_reset(), add = TRUE)
  note <- Filter(function(t) identical(t@name, "add_certara_report_note"),
                 .certara_host_tools("meta"))[[1]]
  expect_true("key" %in% names(formals(note)))
  note(markdown = "First", section = "methods", key = "m1")
  note(markdown = "Second", section = "methods", key = "m1")
  txt <- mcp_report_read()
  expect_match(txt, "Second")
  expect_no_match(txt, "First")
})
