# Tests for the per-user memory store (opt-in, lessons, preferences, lifecycle).
# R_USER_DATA_DIR is redirected to a tempdir so tests never touch real memory.

local_memory <- function(env = parent.frame()) {
  dir <- file.path(tempdir(), paste0("mcpmem_", as.integer(Sys.time()),
                                     sample(1000, 1)))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  withr::local_envvar(c(R_USER_DATA_DIR = dir), .local_envir = env)
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
