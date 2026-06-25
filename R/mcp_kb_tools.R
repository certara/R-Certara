# Knowledge tools. Each returns a small, JSON-serializable list so tool results
# stay token-cheap for the agent. The ellmer wrapping lives in
# certara_mcp_tools().

# Compact projection of an entry for list/search results.
.kb_entry_summary <- function(e, score = NULL) {
  out <- list(
    id = e$id,
    package = e$package,
    type = e$type,
    title = e$title,
    summary = e$summary
  )
  if (length(e$symbols)) out$symbols <- unlist(e$symbols, use.names = FALSE)
  if (!is.null(score)) out$score <- round(score, 4)
  out
}

#' Search the Certara knowledge base
#'
#' Deterministic offline BM25 keyword search over titles, summaries, keywords,
#' symbols, and details, with optional package/type/scope filters.
#'
#' @param query Free-text query.
#' @param package Optional provider package filter.
#' @param type Optional entry-type filter.
#' @param model_type,route,engine Optional `applies_to` scope filters.
#' @param axis Optional retrieval-axis filter (`"guidance"`, `"reference"`,
#'   `"remediation"`). When set, results are ranked with a guidance boost so the
#'   bible surfaces before raw reference.
#' @param limit Maximum results (default 5).
#' @return A list of compact entry summaries with scores.
#' @examples
#' search_certara_kb("two compartment oral absorption")
#' @keywords internal
#' @export
search_certara_kb <- function(query,
                              package = NULL,
                              type = NULL,
                              model_type = NULL,
                              route = NULL,
                              engine = NULL,
                              axis = NULL,
                              limit = 5) {
  index <- .kb_index()
  if (index$n_docs == 0) {
    return(list())
  }
  scores <- .kb_score_bm25(index, query)
  # An axis filter is a guidance-first request, so boost guidance types; plain
  # searches keep the unboosted BM25 ordering.
  if (!is.null(axis)) {
    boost <- vapply(index$entries, function(e) .kb_type_boost(e$type), numeric(1))
    scores <- scores * boost
  }
  ord <- order(scores, decreasing = TRUE)

  results <- list()
  for (i in ord) {
    if (scores[i] <= 0) break
    e <- index$entries[[i]]
    if (!is.null(package) && !identical(e$package, package)) next
    if (!is.null(type) && !identical(e$type, type)) next
    if (!is.null(axis) && !identical(.kb_entry_axis(e), axis)) next
    if (!.kb_scope_match(e, "model_type", model_type)) next
    if (!.kb_scope_match(e, "route", route)) next
    if (!.kb_scope_match(e, "engine", engine)) next
    results[[length(results) + 1]] <- .kb_entry_summary(e, scores[i])
    if (length(results) >= limit) break
  }
  results
}

#' Retrieve a full KB entry by id
#'
#' @param id Stable entry id.
#' @return On a hit, the full entry list with `found = TRUE` prefixed. On a miss,
#'   an explicit `found = FALSE` record carrying a human-readable `message` and
#'   `suggestions` from a relaxed search - never a bare logical, which serializes
#'   ambiguously through MCP (a lone `not_found = TRUE` rendered as `TRUE`).
#' @examples
#' get_certara_kb_entry("Certara.RsNLME.pml.stparm")
#' @keywords internal
#' @export
get_certara_kb_entry <- function(id) {
  index <- .kb_index()
  e <- index$by_id[[id]]
  if (is.null(e)) {
    return(list(
      found = FALSE,
      id = id,
      message = sprintf("No KB entry with id '%s'.", id),
      suggestions = search_certara_kb(gsub("[._-]", " ", id), limit = 3)
    ))
  }
  c(list(found = TRUE), e)
}

#' Find runnable/illustrative examples in the KB
#'
#' @param query Free-text query.
#' @param package Optional provider package filter.
#' @param limit Maximum results.
#' @return A list of `(id, title, examples)` records; respects each example's
#'   `runnable` flag.
#' @examples
#' find_certara_examples("residual error model")
#' @keywords internal
#' @export
find_certara_examples <- function(query, package = NULL, limit = 5) {
  index <- .kb_index()
  scores <- .kb_score_bm25(index, query)
  ord <- order(scores, decreasing = TRUE)
  out <- list()
  for (i in ord) {
    if (scores[i] <= 0) break
    e <- index$entries[[i]]
    if (!is.null(package) && !identical(e$package, package)) next
    if (is.null(e$examples) || length(e$examples) == 0) next
    out[[length(out) + 1]] <- list(
      id = e$id,
      title = e$title,
      examples = e$examples
    )
    if (length(out) >= limit) break
  }
  out
}

#' Explain a Certara workflow for a task
#'
#' Returns the best-matching execution recipes (ordered) - the place to look for
#' a step-by-step recipe. Matches both `playbook` (the guidance-linked recipes)
#' and legacy `workflow_recipe` entries. For the guidance reasoning behind a
#' task (what to verify and why), call [guide_pharmacometrics()] instead.
#'
#' @param task Free-text description of the modeling task.
#' @param packages Optional vector of provider packages to restrict to.
#' @param limit Maximum recipes.
#' @return A list of workflow entries (full details).
#' @examples
#' explain_certara_workflow("fit a base PK model")
#' @keywords internal
#' @export
explain_certara_workflow <- function(task, packages = NULL, limit = 3) {
  index <- .kb_index()
  scores <- .kb_score_bm25(index, task)
  ord <- order(scores, decreasing = TRUE)
  out <- list()
  for (i in ord) {
    if (scores[i] <= 0) break
    e <- index$entries[[i]]
    if (!(e$type %in% c("playbook", "workflow_recipe"))) next
    if (!is.null(packages) && !(e$package %in% packages)) next
    out[[length(out) + 1]] <- list(
      id = e$id,
      title = e$title,
      summary = e$summary,
      details_md = e$details_md,
      parent = e$parent %||% NULL,
      steps = unlist(e$steps, use.names = FALSE),
      related = unlist(e$related, use.names = FALSE)
    )
    if (length(out) >= limit) break
  }
  out
}

#' Guidance-first pharmacometrics lookup
#'
#' The primary entry point for a modeling task. Resolves the intended use first,
#' then returns a resolved guidance tree in one call - the matching guidance
#' chapter(s), relevant decisions, the ordered execution steps, a merged
#' checklist split into auto-verifiable vs human-judgment items, the linked
#' reference entries, and the related anti-patterns. Summary-first: it returns
#' chapter summaries and step titles, not full bodies - fetch those on demand
#' with [get_certara_kb_entry()].
#'
#' @param task Free-text description of the modeling task or question.
#' @param intended_use Optional fit-for-purpose level: one of `"exploratory"`,
#'   `"dose_selection"`, `"labeling"`, `"pediatric"`, `"er_input"`. Resolved
#'   first; scales which checks and validation depth are surfaced. When omitted,
#'   the result flags that intended use should be resolved before final
#'   recommendations.
#' @param limit Maximum guidance chapters to resolve (default 2).
#' @return A resolved guidance tree (see Description).
#' @examples
#' guide_pharmacometrics("covariate selection", intended_use = "labeling")
#' @keywords internal
#' @export
guide_pharmacometrics <- function(task, intended_use = NULL, limit = 2) {
  index <- .kb_index()
  levels <- .kb_intended_use_levels()

  iu <- NULL
  iu_note <- NULL
  if (!is.null(intended_use) && nzchar(intended_use)) {
    if (!intended_use %in% levels) {
      return(list(
        error = sprintf("unknown intended_use '%s'", intended_use),
        allowed_intended_use = levels
      ))
    }
    iu <- intended_use
  } else {
    iu_note <- paste(
      "intended_use not set. Resolve it (one of:",
      paste(levels, collapse = ", "),
      ") before final recommendations - it scales the required checks and",
      "validation depth (fit-for-purpose)."
    )
  }

  if (index$n_docs == 0) {
    return(list(intended_use = iu, intended_use_note = iu_note,
                guidance_chapters = list()))
  }

  # Rank with the guidance boost and pick the entry chapter(s); the graph walk
  # (not repeated BM25) does the rest of the resolution.
  scores <- .kb_score_bm25(index, task)
  boost <- vapply(index$entries, function(e) .kb_type_boost(e$type), numeric(1))
  scores <- scores * boost
  ord <- order(scores, decreasing = TRUE)

  graph <- .kb_build_guidance_graph(index$entries)
  graph_by_id <- list()
  for (g in graph) graph_by_id[[g$id]] <- g

  # The table of contents (chapter_order 0) is the front door, not a topical
  # chapter; resolve to the substantive chapters for a specific task.
  chosen <- list()
  for (i in ord) {
    if (scores[i] <= 0) break
    e <- index$entries[[i]]
    if (!identical(e$type, "guidance_chapter")) next
    if (isTRUE((e$chapter_order %||% NA) == 0)) next
    chosen[[length(chosen) + 1]] <- e
    if (length(chosen) >= limit) break
  }
  if (length(chosen) == 0) {
    return(list(
      intended_use = iu, intended_use_note = iu_note,
      guidance_chapters = list(),
      note = paste("No guidance chapter matched; fall back to",
                   "search_certara_kb() / explain_certara_workflow().")
    ))
  }

  chapters_out <- list()
  all_decisions <- character(0)
  all_steps <- character(0)
  all_refs <- character(0)
  all_anti <- character(0)
  all_checks <- list()

  for (ch in chosen) {
    g <- graph_by_id[[ch$id]] %||% list()
    chapters_out[[length(chapters_out) + 1]] <- list(
      id = ch$id, title = ch$title, summary = ch$summary,
      chapter_order = ch$chapter_order %||% NA_integer_,
      guidance_refs = unlist(ch$guidance_refs, use.names = FALSE),
      conflict = ch$conflict %||% NULL
    )
    all_decisions <- c(all_decisions, g$decisions %||% character(0))
    all_steps <- c(all_steps, g$steps %||% character(0))
    all_refs <- c(all_refs, g$reference %||% character(0))
    all_anti <- c(all_anti, g$anti_patterns %||% character(0))
    # Checks live on the chapter and on its child topics.
    all_checks <- c(all_checks, .kb_collect_checks(index, ch, g$topics %||% character(0)))
  }

  checks_split <- .kb_split_checks(all_checks, iu)

  list(
    intended_use = iu,
    intended_use_note = iu_note,
    guidance_chapters = chapters_out,
    decisions = .kb_resolve_titles(index, unique(all_decisions)),
    steps_resolved = .kb_resolve_titles(index, unique(all_steps)),
    checks = checks_split,
    reference = unique(all_refs),
    anti_patterns = unique(all_anti)
  )
}

# Gather structured checks from a chapter and its child topics, de-duplicated by
# check id (first occurrence wins).
.kb_collect_checks <- function(index, chapter, topic_ids) {
  out <- list()
  seen <- character(0)
  add <- function(checks) {
    for (ck in checks %||% list()) {
      id <- ck$id %||% ""
      if (nzchar(id) && id %in% seen) next
      seen <<- c(seen, id)
      out[[length(out) + 1]] <<- ck
    }
  }
  add(chapter$checks)
  for (tid in topic_ids) {
    t <- index$by_id[[tid]]
    if (!is.null(t)) add(t$checks)
  }
  out
}

# Split checks into auto-verifiable vs human-judgment, honoring intended-use
# scoping (a check tagged for specific levels is dropped when the resolved
# intended use is not among them).
.kb_split_checks <- function(checks, intended_use) {
  auto <- list()
  human <- list()
  for (ck in checks) {
    scope <- unlist(ck$intended_use_scope, use.names = FALSE)
    if (!is.null(intended_use) && length(scope) &&
        !(intended_use %in% scope)) {
      next
    }
    item <- list(id = ck$id, text = ck$text, verifiable_by = ck$verifiable_by)
    if (identical(ck$verifiable_by, "human")) {
      human[[length(human) + 1]] <- item
    } else {
      auto[[length(auto) + 1]] <- item
    }
  }
  list(auto_verifiable = auto, human_judgment = human)
}

# Resolve a vector of entry ids to compact {id, type, title} records, preserving
# order and skipping ids that do not resolve.
.kb_resolve_titles <- function(index, ids) {
  out <- list()
  for (id in ids) {
    e <- index$by_id[[id]]
    if (is.null(e)) next
    out[[length(out) + 1]] <- list(id = e$id, type = e$type, title = e$title)
  }
  out
}

#' Exact PML symbol lookup
#'
#' Maps a PML keyword/token or function name directly to the entries that
#' document it, via the generated symbol index.
#'
#' @param symbol Exact symbol (e.g. `"stparm"`, `"dosepoint"`).
#' @return A list of compact entry summaries (possibly empty).
#' @examples
#' lookup_pml_symbol("stparm")
#' @keywords internal
#' @export
lookup_pml_symbol <- function(symbol) {
  index <- .kb_index()
  ids <- index$symbol_index[[symbol]]
  if (is.null(ids)) {
    return(list(symbol = symbol, matches = list()))
  }
  matches <- lapply(ids, function(id) .kb_entry_summary(index$by_id[[id]]))
  list(symbol = symbol, matches = matches)
}

#' List controlled PML vocabularies (enums)
#'
#' @param kind Optional enum name (e.g. `"error_model"`). `NULL` returns all.
#' @return A named list of allowed values.
#' @examples
#' list_pml_enums()
#' @keywords internal
#' @export
list_pml_enums <- function(kind = NULL) {
  enums <- .kb_pml_enums()
  if (is.null(kind)) {
    return(enums)
  }
  if (!kind %in% names(enums)) {
    return(list(unknown_kind = kind, available = names(enums)))
  }
  enums[kind]
}

#' List discovered KB provider packages
#'
#' @return A list with `packages` (loaded providers) and `skipped`
#'   (incompatible/invalid manifests with reasons).
#' @examples
#' list_certara_kb_packages()
#' @keywords internal
#' @export
list_certara_kb_packages <- function() {
  index <- .kb_index()
  list(packages = index$providers, skipped = index$skipped)
}
