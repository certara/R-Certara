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
