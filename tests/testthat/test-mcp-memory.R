# Tests for the per-user memory store (opt-in, lessons, preferences, lifecycle).
# Redirect tools::R_user_dir() to a unique temp dir so tests never share a
# store. R_user_dir() consults R_USER_DATA_DIR, then XDG_DATA_HOME, then on
# Windows APPDATA. A Sys.time()+sample() path can collide under R CMD check's
# fixed seed when tests run in the same second, so use tempfile().

local_memory <- function(env = parent.frame()) {
  dir <- tempfile("mcpmem_")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  withr::local_envvar(
    c(
      R_USER_DATA_DIR = dir,
      XDG_DATA_HOME = dir,
      APPDATA = dir,
      LOCALAPPDATA = dir
    ),
    .local_envir = env
  )
  dir
}

test_that("memory is opt-in: disabled by default and errors on write", {
  local_memory()
  expect_false(.memory_enabled())
  expect_error(record_lesson("x"), "disabled")
})

test_that("lessons record and read back, corrective first", {
  local_memory()
  enable_memory()
  expect_true(.memory_enabled())
  record_lesson("avoid c() around error SD", category = "corrective",
                trigger = "self_detected", scope = "global")
  record_lesson("prefer FOCE-ELS for rich data", category = "best_practice")
  ls <- get_lessons()
  expect_length(ls, 2)
  expect_identical(ls[[1]]$category, "corrective")
})

test_that("preferences supersede rather than overwrite", {
  local_memory()
  enable_memory()
  set_preference("error_model", "additive")
  set_preference("error_model", "combined")
  active <- get_user_preferences()
  vals <- vapply(active, function(p) p$value, character(1))
  expect_identical(vals, "combined")
  # Superseded record is retained in the raw store.
  all_recs <- list_memory_records()$preferences
  expect_length(all_recs, 2)
})

test_that("lifecycle: list, delete, clear, disable", {
  local_memory()
  enable_memory()
  id <- record_lesson("temp lesson")$id
  expect_true(delete_memory_record(id))
  expect_length(get_lessons(), 0)
  set_preference("x", "1")
  clear_memory()
  expect_length(get_user_preferences(), 0)
  disable_memory()
  expect_false(.memory_enabled())
})

test_that("redaction scrubs configured patterns", {
  local_memory()
  enable_memory(redact = "CompoundX")
  record_lesson("CompoundX showed high shrinkage")
  txt <- get_lessons()[[1]]$text
  expect_false(grepl("CompoundX", txt))
  expect_match(txt, "redacted")
})

test_that("invalid redact regex is rejected at enable time", {
  local_memory()
  expect_error(enable_memory(redact = "("), "Invalid redact regex")
})

test_that("corrupt JSONL lines are skipped with a warning", {
  local_memory()
  enable_memory()
  record_lesson("good lesson")
  cat("{not valid json}\n", file = .memory_lessons_path(), append = TRUE)
  expect_warning(ls <- get_lessons(), "Skipping unparsable memory record")
  expect_length(ls, 1)
  expect_match(ls[[1]]$text, "good lesson")
})

test_that("deactivate_lesson hides from default get_lessons", {
  local_memory()
  enable_memory()
  id <- record_lesson("obsolete note")$id
  expect_length(get_lessons(), 1)
  res <- deactivate_lesson(id)
  expect_true(isTRUE(res$deactivated))
  expect_length(get_lessons(), 0)
  all <- get_lessons(include_superseded = TRUE)
  expect_length(all, 1)
  expect_false(isTRUE(all[[1]]$active))
})

test_that("record_run round-trips through list_memory_records", {
  local_memory()
  enable_memory()
  id <- record_run("FOCE-ELS fit converged; OFV 1234.5")$id
  runs <- list_memory_records()$run_memory
  expect_length(runs, 1)
  expect_identical(runs[[1]]$id, id)
  expect_match(runs[[1]]$summary, "FOCE-ELS")
  expect_identical(runs[[1]]$kind, "run")
})

test_that("record_lesson MCP wrapper returns next_action when memory is off", {
  local_memory()
  rec <- Filter(function(t) identical(t@name, "record_lesson"),
                .certara_host_tools("memory"))[[1]]
  out <- rec(lesson = "do not freeze every residual")
  expect_false(isTRUE(out$recorded))
  expect_match(out$reason, "disabled")
  expect_identical(out$next_action, "Certara.R::enable_memory()")
  err <- tryCatch(record_lesson("x"), error = identity)
  expect_s3_class(err, "certara_memory_disabled")
})

test_that("record_lesson MCP wrapper still raises non-disable errors", {
  local_memory()
  enable_memory()
  rec <- Filter(function(t) identical(t@name, "record_lesson"),
                .certara_host_tools("memory"))[[1]]
  expect_error(rec(lesson = "x", category = "not_a_category"), "arg")
})

test_that("list_memory_records MCP wrapper hints enable_memory when off", {
  local_memory()
  lst <- Filter(function(t) identical(t@name, "list_memory_records"),
                .certara_host_tools("memory"))[[1]]
  out <- lst()
  expect_false(isTRUE(out$enabled))
  expect_identical(out$next_action, "Certara.R::enable_memory()")
})

test_that("certara_session_status points at enable_memory when memory is off", {
  local_memory()
  st <- certara_session_status()
  expect_false(isTRUE(st$memory$enabled))
  expect_identical(st$memory$next_action, "Certara.R::enable_memory()")
})
