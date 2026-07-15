# Per-user memory. Local, opt-in, append-only, never pooled across users and
# never phoned home, because run history and lessons are private to the analyst.
# Two kinds: quantitative run fingerprints and qualitative lessons/preferences,
# stored under the per-user app dir. OFF by default until enable_memory().

.memory_dir <- function() {
  dir <- file.path(tools::R_user_dir("Certara.R", "data"), "mcp-memory")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

.memory_config_path <- function() file.path(.memory_dir(), "config.json")
.memory_runs_path <- function() file.path(.memory_dir(), "run_memory.jsonl")
.memory_lessons_path <- function() file.path(.memory_dir(), "lessons.jsonl")
.memory_prefs_path <- function() file.path(.memory_dir(), "preferences.jsonl")

.memory_config <- function() {
  p <- .memory_config_path()
  if (!file.exists(p)) {
    return(list(enabled = FALSE, redact = list()))
  }
  tryCatch(jsonlite::read_json(p, simplifyVector = TRUE),
           error = function(e) list(enabled = FALSE, redact = list()))
}

#' Is per-user memory enabled?
#' @return Logical scalar.
#' @keywords internal
#' @export
.memory_enabled <- function() {
  isTRUE(.memory_config()$enabled)
}

#' Enable per-user memory
#' @param redact Optional character vector of regex patterns to scrub from
#'   stored free text (e.g. compound or project identifiers).
#' @return Invisibly the new config.
#' @examples
#' \dontrun{
#' enable_memory()
#' }
#' @export
enable_memory <- function(redact = NULL) {
  cfg <- .memory_config()
  cfg$enabled <- TRUE
  if (!is.null(redact)) {
    .memory_validate_redact(redact)
    cfg$redact <- as.list(redact)
  }
  jsonlite::write_json(cfg, .memory_config_path(), auto_unbox = TRUE)
  invisible(cfg)
}

#' Disable per-user memory (does not delete existing records)
#' @return Invisibly the new config.
#' @examples
#' \dontrun{
#' disable_memory()
#' }
#' @export
disable_memory <- function() {
  cfg <- .memory_config()
  cfg$enabled <- FALSE
  jsonlite::write_json(cfg, .memory_config_path(), auto_unbox = TRUE)
  invisible(cfg)
}

.memory_redact <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    return(x)
  }
  for (pat in unlist(.memory_config()$redact, use.names = FALSE)) {
    x <- gsub(pat, "[redacted]", x, perl = TRUE)
  }
  x
}

.memory_validate_redact <- function(patterns) {
  for (pat in patterns) {
    if (!is.character(pat) || length(pat) != 1 || !nzchar(pat)) {
      stop("Each redact pattern must be a non-empty character string.",
           call. = FALSE)
    }
    tryCatch(
      suppressWarnings(grepl(pat, "", perl = TRUE)),
      error = function(e) stop(
        sprintf("Invalid redact regex pattern: '%s' (%s)",
                pat, conditionMessage(e)),
        call. = FALSE
      )
    )
  }
  invisible(NULL)
}

.memory_require_enabled <- function() {
  if (!.memory_enabled()) {
    stop(
      "Per-user memory is disabled. Enable with enable_memory() (opt-in).",
      call. = FALSE
    )
  }
}

.memory_append <- function(path, rec) {
  rec$id <- rec$id %||% .memory_new_id()
  rec$recorded <- .mcp_now()
  line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null")
  cat(line, "\n", sep = "", file = path, append = TRUE)
  invisible(rec$id)
}

.memory_read <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  recs <- lapply(lines, function(l) {
    tryCatch(
      jsonlite::fromJSON(l, simplifyVector = TRUE),
      error = function(e) {
        warning(
          sprintf("Skipping unparsable memory record in '%s': %s",
                  path, conditionMessage(e)),
          call. = FALSE
        )
        NULL
      }
    )
  })
  Filter(Negate(is.null), recs)
}

.memory_new_id <- function() {
  paste0("mem-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
         paste(sample(c(0:9, letters), 4, replace = TRUE), collapse = ""))
}

# ---- qualitative lessons & preferences --------------------------------------

#' Record a corrective lesson or endorsed best practice
#'
#' @param lesson What went wrong / the corrected approach (free text).
#' @param category One of "corrective", "best_practice".
#' @param trigger For corrective lessons: "self_detected" or "user_feedback".
#' @param scope "global" or a context tag (model type / task / data shape).
#' @param level "hard" or "soft" (corrective defaults to hard).
#' @param provenance Optional list (e.g. job_id, timestamp).
#' @return A list with the stored `id`.
#' @examples
#' \dontrun{
#' enable_memory()
#' record_lesson("prefer FOCE-ELS for rich data", category = "best_practice")
#' }
#' @keywords internal
#' @export
record_lesson <- function(lesson,
                          category = c("corrective", "best_practice"),
                          trigger = NULL,
                          scope = "global",
                          level = NULL,
                          provenance = NULL) {
  .memory_require_enabled()
  category <- match.arg(category)
  if (is.null(level)) {
    level <- if (category == "corrective") "hard" else "soft"
  }
  rec <- list(
    kind = "lesson",
    category = category,
    text = .memory_redact(lesson),
    trigger = trigger %||% NA_character_,
    scope = scope,
    level = level,
    active = TRUE,
    provenance = provenance %||% NULL
  )
  list(recorded = TRUE, id = .memory_append(.memory_lessons_path(), rec))
}

#' Record a quantitative run fingerprint
#'
#' @param summary Short free-text summary of the run (model, outcome, key metrics).
#' @param scope `"global"` or a context tag (model type / task / data shape).
#' @param provenance Optional list (e.g. job_id, package, tool).
#' @return A list with the stored `id`.
#' @examples
#' \dontrun{
#' enable_memory()
#' record_run("FOCE-ELS fit converged; OFV 1234.5")
#' }
#' @keywords internal
#' @export
record_run <- function(summary, scope = "global", provenance = NULL) {
  .memory_require_enabled()
  rec <- list(
    kind = "run",
    summary = .memory_redact(summary),
    scope = scope,
    provenance = provenance %||% NULL
  )
  list(recorded = TRUE, id = .memory_append(.memory_runs_path(), rec))
}

#' Deactivate a lesson without deleting it
#'
#' Marks a lesson inactive so [get_lessons()] omits it unless
#' `include_superseded = TRUE`. History is preserved (unlike
#' [delete_memory_record()]).
#'
#' @param id Lesson record id returned by [record_lesson()].
#' @return A list with `deactivated` (logical) and `id`.
#' @examples
#' \dontrun{
#' enable_memory()
#' id <- record_lesson("obsolete note")$id
#' deactivate_lesson(id)
#' }
#' @keywords internal
#' @export
deactivate_lesson <- function(id) {
  .memory_require_enabled()
  recs <- .memory_read(.memory_lessons_path())
  hit <- which(vapply(recs, function(r) identical(r$id, id), logical(1)))
  if (!length(hit)) {
    return(list(deactivated = FALSE, id = id))
  }
  recs[[hit[1]]]$active <- FALSE
  .memory_rewrite(.memory_lessons_path(), recs)
  list(deactivated = TRUE, id = id)
}

#' Set a default user preference
#'
#' Append-only with supersede: setting a key marks prior records for that key
#' (same scope) as superseded rather than overwriting them.
#'
#' @param key Preference key (e.g. "error_model", "plot_style").
#' @param value Preference value.
#' @param scope "global" or a context tag.
#' @param level "hard" (always apply) or "soft" (consider).
#' @return A list with the stored `id`.
#' @examples
#' \dontrun{
#' enable_memory()
#' set_preference("error_model", "additive")
#' }
#' @keywords internal
#' @export
set_preference <- function(key, value, scope = "global",
                           level = c("hard", "soft")) {
  .memory_require_enabled()
  level <- match.arg(level)
  new_id <- .memory_new_id()
  recs <- .memory_read(.memory_prefs_path())
  for (i in seq_along(recs)) {
    if (identical(recs[[i]]$key, key) && identical(recs[[i]]$scope, scope) &&
        isTRUE(recs[[i]]$active)) {
      recs[[i]]$active <- FALSE
      recs[[i]]$superseded_by <- new_id
    }
  }
  recs[[length(recs) + 1]] <- list(
    id = new_id, kind = "preference", key = key,
    value = .memory_redact(as.character(value)), scope = scope, level = level,
    active = TRUE, recorded = .mcp_now()
  )
  .memory_rewrite(.memory_prefs_path(), recs)
  list(recorded = TRUE, id = new_id)
}

#' Get context-relevant lessons
#'
#' @param context Optional scope tag; returns global plus matching lessons.
#' @param include_superseded Include inactive lessons (default `FALSE`).
#' @return A list of lesson records (corrective first, then best practices).
#' @examples
#' \dontrun{
#' get_lessons()
#' }
#' @keywords internal
#' @export
get_lessons <- function(context = NULL, include_superseded = FALSE) {
  recs <- .memory_read(.memory_lessons_path())
  recs <- Filter(function(r) include_superseded || isTRUE(r$active), recs)
  if (!is.null(context)) {
    recs <- Filter(
      function(r) r$scope %in% c("global", context), recs
    )
  }
  ord <- order(vapply(recs, function(r) {
    if (identical(r$category, "corrective")) 0L else 1L
  }, integer(1)))
  recs[ord]
}

#' Get active user preferences
#'
#' @param context Optional scope tag; returns global plus matching preferences.
#' @return A list of active preference records.
#' @examples
#' \dontrun{
#' get_user_preferences()
#' }
#' @keywords internal
#' @export
get_user_preferences <- function(context = NULL) {
  recs <- .memory_read(.memory_prefs_path())
  recs <- Filter(function(r) isTRUE(r$active), recs)
  if (!is.null(context)) {
    recs <- Filter(function(r) r$scope %in% c("global", context), recs)
  }
  recs
}

# ---- lifecycle controls -----------------------------------------------------

#' List all per-user memory records
#' @return A list with `run_memory`, `lessons`, `preferences`, and `enabled`.
#' @examples
#' \dontrun{
#' list_memory_records()
#' }
#' @export
list_memory_records <- function() {
  list(
    enabled = .memory_enabled(),
    run_memory = .memory_read(.memory_runs_path()),
    lessons = .memory_read(.memory_lessons_path()),
    preferences = .memory_read(.memory_prefs_path())
  )
}

#' Export all per-user memory records to a JSON file
#' @param path Destination file path.
#' @return Invisibly `path`.
#' @examples
#' \dontrun{
#' export_memory(tempfile(fileext = ".json"))
#' }
#' @export
export_memory <- function(path) {
  jsonlite::write_json(list_memory_records(), path, auto_unbox = TRUE,
                       pretty = TRUE, null = "null")
  invisible(path)
}

#' Delete a single per-user memory record by id
#' @param id Record id.
#' @return Logical: whether a record was removed.
#' @examples
#' \dontrun{
#' delete_memory_record("mem-20260101120000-abcd")
#' }
#' @export
delete_memory_record <- function(id) {
  removed <- FALSE
  for (path in c(.memory_runs_path(), .memory_lessons_path(),
                 .memory_prefs_path())) {
    recs <- .memory_read(path)
    keep <- Filter(function(r) !identical(r$id, id), recs)
    if (length(keep) != length(recs)) {
      .memory_rewrite(path, keep)
      removed <- TRUE
    }
  }
  removed
}

#' Clear all per-user memory records
#' @return Invisibly `TRUE`.
#' @examples
#' \dontrun{
#' clear_memory()
#' }
#' @export
clear_memory <- function() {
  for (path in c(.memory_runs_path(), .memory_lessons_path(),
                 .memory_prefs_path())) {
    if (file.exists(path)) file.remove(path)
  }
  invisible(TRUE)
}

.memory_rewrite <- function(path, recs) {
  if (length(recs) == 0) {
    if (file.exists(path)) file.remove(path)
    return(invisible())
  }
  lines <- vapply(recs, function(r) {
    jsonlite::toJSON(r, auto_unbox = TRUE, null = "null")
  }, character(1))
  writeLines(lines, path, useBytes = TRUE)
}
