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
