# Knowledge-base schema: versioned contract shared by every Certara package that
# ships an `inst/mcp/kb/`. Validators here are hand-written (no JSON Schema
# validator dependency) and operate on lists produced by jsonlite.

#' Current KB schema version
#'
#' Semantic version of the knowledge-base entry/manifest contract implemented by
#' this build. Providers declare the `schema_version` they were generated
#' against in their manifest; the server uses [.kb_schema_compatible()] to decide
#' whether to load them.
#'
#' @return A length-one character version string.
#' @examples
#' kb_schema_version()
#' @keywords internal
#' @export
kb_schema_version <- function() {
  "1.0.0"
}

# Allowed entry types. The guidance layer (`guidance_chapter`, `guidance_topic`,
# `decision`) is the primary axis agents navigate; `playbook` maps a guidance
# step to a concrete RsNLME recipe; the reference layer (`function_doc`,
# `pml_topic`, `format_topic`, ...) is supporting detail; `anti_pattern` and
# `troubleshooting` carry remediation content.
.kb_entry_types <- function() {
  c(
    "guidance_chapter",
    "guidance_topic",
    "playbook",
    "decision",
    "function_doc",
    "vignette_section",
    "pml_topic",
    "format_topic",
    "workflow_recipe",
    "example",
    "troubleshooting",
    "anti_pattern"
  )
}

# Entry types that assert a fact about the engine/API/product and therefore must
# carry provenance back to a source of record (provenance$source_file).
.kb_factual_types <- function() {
  c("function_doc", "vignette_section", "pml_topic", "format_topic",
    "workflow_recipe", "playbook")
}

# Guidance-axis types. These assert a regulatory/industry best-practice claim
# and so must point at an external authority via `guidance_refs` (the URL lives
# in `source$url`; the section labels live in `guidance_refs`).
.kb_guidance_types <- function() {
  c("guidance_chapter", "guidance_topic", "decision")
}

# Allowed values for a check's `verifiable_by` tag: either a known execution-tool
# output field the agent can confirm automatically, or `human` for a judgment
# call the agent must surface to the user.
.kb_check_verifiers <- function() {
  c("fit_health", "vpc", "bootstrap", "cond_number", "data_summary", "human")
}

# Allowed retrieval axes.
.kb_axes <- function() {
  c("guidance", "reference", "remediation")
}

# Allowed intended-use levels (fit-for-purpose). Resolved first by
# guide_pharmacometrics(); scales which checks and validation depth are shown.
.kb_intended_use_levels <- function() {
  c("exploratory", "dose_selection", "labeling", "pediatric", "er_input")
}

#' Schema-version compatibility rule
#'
#' A provider KB is loadable when it shares the server's MAJOR version and its
#' MINOR version is not newer than the server's. PATCH differences are always
#' compatible. This lets older providers keep working after additive (MINOR)
#' schema growth while refusing forward-incompatible content.
#'
#' @param provided Character schema version from a provider manifest.
#' @param supported Character schema version supported by this build.
#' @return `TRUE` when the provided schema can be loaded, otherwise `FALSE`.
#' @keywords internal
#' @export
.kb_schema_compatible <- function(provided, supported = kb_schema_version()) {
  if (!is.character(provided) || length(provided) != 1 || is.na(provided)) {
    return(FALSE)
  }
  p <- .kb_parse_semver(provided)
  s <- .kb_parse_semver(supported)
  if (is.null(p) || is.null(s)) {
    return(FALSE)
  }
  p[1] == s[1] && p[2] <= s[2]
}

.kb_parse_semver <- function(x) {
  m <- regmatches(x, regexec("^(\\d+)\\.(\\d+)\\.(\\d+)", x))[[1]]
  if (length(m) != 4) {
    return(NULL)
  }
  as.integer(m[2:4])
}

# ---- entry validation -------------------------------------------------------

# A related/parent/step id whose prefix differs from this entry's own package is
# a CROSS-PACKAGE reference into another provider's KB slice (federation). Those
# resolve when the host merges all discovered slices, not at the single-package
# coverage gate, so the per-package validator only requires SAME-package ids to
# resolve in known_ids. The trailing dot keeps "Certara.R." from matching
# "Certara.RsNLME." Returns the subset of ids that belong to this package.
.kb_local_ref_ids <- function(ids, package) {
  if (is.null(package) || !is.character(package) || length(package) != 1 ||
      !nzchar(package)) {
    return(ids)
  }
  ids[startsWith(ids, paste0(package, "."))]
}

#' Validate a single KB entry
#'
#' @param entry A named list (one parsed JSONL record).
#' @param known_ids Optional character vector of all entry ids, used to detect
#'   dangling `related` cross-references.
#' @return A character vector of problems; empty when the entry is valid.
#' @keywords internal
#' @export
.validate_kb_entry <- function(entry, known_ids = NULL) {
  problems <- character(0)
  req_chr <- c("id", "package", "type", "title", "summary")
  for (f in req_chr) {
    if (is.null(entry[[f]]) || !is.character(entry[[f]]) ||
        length(entry[[f]]) != 1 || !nzchar(entry[[f]])) {
      problems <- c(problems, sprintf("missing/invalid required field '%s'", f))
    }
  }

  if (!is.null(entry$type) && length(entry$type) == 1 &&
      !entry$type %in% .kb_entry_types()) {
    problems <- c(problems, sprintf("unknown type '%s'", entry$type))
  }

  for (f in c("keywords", "symbols", "related")) {
    v <- entry[[f]]
    if (!is.null(v) && !(is.character(v) || (is.list(v) &&
        all(vapply(v, is.character, logical(1)))))) {
      problems <- c(problems, sprintf("field '%s' must be an array of strings", f))
    }
  }

  if (!is.null(entry$examples)) {
    ex <- entry$examples
    if (!is.list(ex)) {
      problems <- c(problems, "field 'examples' must be an array")
    } else {
      for (i in seq_along(ex)) {
        if (is.null(ex[[i]]$code) || !is.character(ex[[i]]$code)) {
          problems <- c(problems, sprintf("examples[%d].code missing", i))
        }
      }
    }
  }

  # Factual entries must be traceable to a source of record.
  if (!is.null(entry$type) && length(entry$type) == 1 &&
      entry$type %in% .kb_factual_types()) {
    sf <- entry$provenance$source_file
    if (is.null(sf) || !is.character(sf) || !nzchar(sf)) {
      problems <- c(
        problems,
        "factual entry requires provenance$source_file"
      )
    }
  }

  # Guidance-axis entries must cite at least one external authority so a shipped
  # best-practice claim is never presented without a regulatory/industry source.
  if (!is.null(entry$type) && length(entry$type) == 1 &&
      entry$type %in% .kb_guidance_types()) {
    refs <- unlist(entry$guidance_refs, use.names = FALSE)
    if (length(refs) == 0 || !any(nzchar(refs))) {
      problems <- c(problems, "guidance entry requires >=1 guidance_refs")
    }
  }

  problems <- c(problems, .validate_kb_guidance_fields(entry, known_ids))

  # Anti-patterns must carry the engine error signature they remediate.
  if (!is.null(entry$type) && length(entry$type) == 1 &&
      entry$type == "anti_pattern") {
    if (is.null(entry$error_signature) ||
        !is.character(entry$error_signature) ||
        !nzchar(entry$error_signature)) {
      problems <- c(problems, "anti_pattern entry requires error_signature")
    }
  }

  if (!is.null(known_ids) && !is.null(entry$related)) {
    rel <- .kb_local_ref_ids(unlist(entry$related, use.names = FALSE),
                             entry$package)
    dangling <- setdiff(rel, known_ids)
    if (length(dangling)) {
      problems <- c(
        problems,
        sprintf("dangling related id(s): %s", paste(dangling, collapse = ", "))
      )
    }
  }

  problems
}

# Validate the guidance-axis metadata fields (chapter_order, parent, steps,
# axis, intended_use_scope, structured checks, conflict block). Returns a
# character vector of problems. Cross-reference fields (parent/steps) are only
# checked for danglers when `known_ids` is supplied.
.validate_kb_guidance_fields <- function(entry, known_ids = NULL) {
  problems <- character(0)

  if (!is.null(entry$chapter_order)) {
    n <- suppressWarnings(as.integer(entry$chapter_order))
    if (length(n) != 1 || is.na(n) || n < 0) {
      problems <- c(problems, "chapter_order must be a non-negative integer")
    }
  }

  if (!is.null(entry$axis) && length(entry$axis) == 1 &&
      !entry$axis %in% .kb_axes()) {
    problems <- c(problems, sprintf("unknown axis '%s'", entry$axis))
  }

  if (!is.null(entry$intended_use_scope)) {
    bad <- setdiff(unlist(entry$intended_use_scope, use.names = FALSE),
                   .kb_intended_use_levels())
    if (length(bad)) {
      problems <- c(problems,
                    sprintf("unknown intended_use_scope: %s",
                            paste(bad, collapse = ", ")))
    }
  }

  if (!is.null(known_ids)) {
    if (!is.null(entry$parent)) {
      par <- .kb_local_ref_ids(unlist(entry$parent, use.names = FALSE),
                               entry$package)
      dangling <- setdiff(par, known_ids)
      if (length(dangling)) {
        problems <- c(problems, sprintf("dangling parent id(s): %s",
                                        paste(dangling, collapse = ", ")))
      }
    }
    if (!is.null(entry$steps)) {
      st <- .kb_local_ref_ids(unlist(entry$steps, use.names = FALSE),
                              entry$package)
      dangling <- setdiff(st, known_ids)
      if (length(dangling)) {
        problems <- c(problems, sprintf("dangling step id(s): %s",
                                        paste(dangling, collapse = ", ")))
      }
    }
  }

  if (!is.null(entry$checks)) {
    if (!is.list(entry$checks)) {
      problems <- c(problems, "field 'checks' must be an array")
    } else {
      for (i in seq_along(entry$checks)) {
        ck <- entry$checks[[i]]
        if (is.null(ck$id) || !nzchar(ck$id %||% "")) {
          problems <- c(problems, sprintf("checks[%d] missing id", i))
        }
        if (is.null(ck$text) || !nzchar(ck$text %||% "")) {
          problems <- c(problems, sprintf("checks[%d] missing text", i))
        }
        vb <- ck$verifiable_by
        if (is.null(vb) || !vb %in% .kb_check_verifiers()) {
          problems <- c(problems, sprintf(
            "checks[%d] verifiable_by must be one of: %s", i,
            paste(.kb_check_verifiers(), collapse = ", ")))
        }
      }
    }
  }

  if (!is.null(entry$conflict)) {
    keys <- c("guidance_expectation", "supported_path",
              "advanced_or_manual", "not_exposed")
    if (!is.list(entry$conflict) ||
        !any(vapply(keys, function(k) {
          v <- entry$conflict[[k]]
          !is.null(v) && nzchar(v %||% "")
        }, logical(1)))) {
      problems <- c(problems, paste(
        "conflict must set at least one of:",
        paste(keys, collapse = ", ")))
    }
  }

  problems
}

#' Validate a KB manifest
#'
#' @param manifest A named list parsed from `manifest.json`.
#' @return A character vector of problems; empty when valid.
#' @keywords internal
#' @export
.validate_kb_manifest <- function(manifest) {
  problems <- character(0)
  for (f in c("package", "schema_version")) {
    if (is.null(manifest[[f]]) || !is.character(manifest[[f]]) ||
        length(manifest[[f]]) != 1 || !nzchar(manifest[[f]])) {
      problems <- c(problems, sprintf("missing/invalid manifest field '%s'", f))
    }
  }
  if (!is.null(manifest$files) && !is.list(manifest$files)) {
    problems <- c(problems, "manifest field 'files' must be an array")
  }
  problems
}
