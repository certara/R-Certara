# Deterministic KB generator: parse curated markdown sources into normalized
# JSONL entries + a symbol index + controlled enums + a manifest. No AI, no
# network; pure transformation so CI can re-run and diff the committed output.

#' Controlled vocabularies surfaced by list_pml_enums()
#'
#' These are the closed sets the agent should pick from instead of guessing.
#' Generated into `index/enums.json` and also served at runtime.
#'
#' @return A named list of character vectors.
#' @keywords internal
#' @export
.kb_pml_enums <- function() {
  list(
    param_style = c("log-normal", "normal", "logit"),
    error_model = c("additive", "multiplicative", "combined", "power", "log-additive"),
    absorption = c("intravenous", "first-order", "gamma", "weibull", "inverse-gaussian"),
    elimination = c("linear", "michaelis-menten", "linear-plus-michaelis-menten"),
    distribution = c(
      "normal", "lognorm", "beta", "betamean", "binomial", "chisq", "constant",
      "exponential", "gamma", "invgamma", "logistic", "negbin", "poisson",
      "studentt", "uniform", "weibull", "MVN", "MVT"
    )
  )
}

# Parse a single curated markdown source file into a list of entry lists.
.parse_kb_source_file <- function(path, package, package_version = NULL) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  heading_idx <- grep("^##\\s+", lines)
  if (length(heading_idx) == 0) {
    return(list())
  }
  bounds <- c(heading_idx, length(lines) + 1L)
  entries <- vector("list", length(heading_idx))

  for (i in seq_along(heading_idx)) {
    start <- heading_idx[i]
    stop <- bounds[i + 1] - 1L
    block <- lines[start:stop]
    entries[[i]] <- .parse_kb_entry_block(
      block, path, package, package_version
    )
  }
  Filter(Negate(is.null), entries)
}

.parse_kb_entry_block <- function(block, path, package, package_version) {
  id <- sub("^##\\s+", "", block[1])
  id <- trimws(id)
  if (!grepl("^[A-Za-z0-9._-]+$", id)) {
    stop(sprintf("Invalid entry id '%s' in %s", id, path), call. = FALSE)
  }

  # Metadata: contiguous key: value lines after the heading until a blank line.
  meta <- list()
  body_start <- 2L
  j <- 2L
  while (j <= length(block)) {
    ln <- block[j]
    if (!nzchar(trimws(ln))) {
      body_start <- j + 1L
      break
    }
    m <- regmatches(ln, regexec("^([A-Za-z_.]+):\\s*(.*)$", ln))[[1]]
    if (length(m) != 3) {
      # Not a metadata line: body starts here.
      body_start <- j
      break
    }
    meta[[m[2]]] <- trimws(m[3])
    j <- j + 1L
    body_start <- j
  }

  body <- if (body_start <= length(block)) {
    paste(block[body_start:length(block)], collapse = "\n")
  } else {
    ""
  }
  body <- trimws(body)

  split_list <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      return(character(0))
    }
    out <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    out[nzchar(out)]
  }

  type <- if (!is.null(meta$type)) meta$type else "pml_topic"
  title <- if (!is.null(meta$title)) meta$title else id
  summary <- meta$summary
  if (is.null(summary) || !nzchar(summary)) {
    summary <- .kb_first_paragraph(body)
  }

  examples <- .kb_extract_examples(
    body,
    runnable = identical(tolower(meta$runnable %||% ""), "true")
  )

  provenance <- list(
    source_file = meta[["provenance.source_file"]] %||% NA_character_,
    symbol = meta[["provenance.symbol"]] %||% NA_character_,
    anchor = meta[["provenance.anchor"]] %||% NA_character_
  )
  if (!is.null(meta[["provenance.accessed"]])) {
    provenance$accessed <- meta[["provenance.accessed"]]
  }
  if (!is.null(meta[["provenance.guidance_revision"]])) {
    provenance$guidance_revision <- meta[["provenance.guidance_revision"]]
  }

  entry <- list(
    id = id,
    package = package,
    package_version = package_version %||% NA_character_,
    type = type,
    title = title,
    summary = summary,
    details_md = body,
    keywords = split_list(meta$keywords),
    symbols = split_list(meta$symbols),
    applies_to = list(
      engine = split_list(meta[["applies_to.engine"]]),
      route = split_list(meta[["applies_to.route"]]),
      model_type = split_list(meta[["applies_to.model_type"]])
    ),
    related = split_list(meta$related),
    examples = examples,
    error_signature = meta$error_signature %||% NULL,
    provenance = provenance,
    source = list(
      kind = meta[["source.kind"]] %||% NA_character_,
      url = meta[["source.url"]] %||% NA_character_
    )
  )

  # Guidance-axis metadata. Only attach fields that are actually present so the
  # generated JSONL stays minimal and deterministic.
  if (!is.null(meta$axis)) entry$axis <- meta$axis
  if (!is.null(meta$chapter_order)) {
    entry$chapter_order <- as.integer(meta$chapter_order)
  }
  if (!is.null(meta$parent)) entry$parent <- meta$parent
  refs <- .kb_split_semis(meta$guidance_refs)
  if (length(refs)) entry$guidance_refs <- refs
  steps <- split_list(meta$steps)
  if (length(steps)) entry$steps <- steps
  ius <- split_list(meta$intended_use_scope)
  if (length(ius)) entry$intended_use_scope <- ius
  checks <- .kb_parse_checks(meta$checks)
  if (length(checks)) entry$checks <- checks
  conflict <- .kb_collect_conflict(meta)
  if (length(conflict)) entry$conflict <- conflict

  if (!is.null(meta$symptom)) entry$symptom <- meta$symptom
  if (!is.null(meta$fix)) entry$fix <- meta$fix
  entry
}

# Split a semicolon-delimited metadata value (used for guidance_refs, whose
# labels may contain commas). Returns a trimmed character vector.
.kb_split_semis <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(character(0))
  }
  out <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  out[nzchar(out)]
}

# Parse the single-line structured `checks` encoding into a list of
# {id, text, verifiable_by[, intended_use_scope]}. Checks are separated by ';;'
# and the fields by '|'. The optional 4th field is a comma-separated
# intended-use scope, e.g.
#   checks: shrinkage_ok|Eta shrinkage < ~30%|fit_health ;;
#           bootstrap|Bootstrap CIs on final model|bootstrap|labeling,dose_selection
.kb_parse_checks <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(list())
  }
  items <- trimws(strsplit(x, ";;", fixed = TRUE)[[1]])
  items <- items[nzchar(items)]
  lapply(items, function(it) {
    parts <- trimws(strsplit(it, "|", fixed = TRUE)[[1]])
    out <- list(
      id = parts[1] %||% "",
      text = if (length(parts) >= 2) parts[2] else "",
      verifiable_by = if (length(parts) >= 3 && nzchar(parts[3])) parts[3] else "human"
    )
    if (length(parts) >= 4 && nzchar(parts[4])) {
      scope <- trimws(strsplit(parts[4], ",", fixed = TRUE)[[1]])
      out$intended_use_scope <- scope[nzchar(scope)]
    }
    out
  })
}

# Assemble a conflict block from dotted `conflict.<key>` metadata lines.
.kb_collect_conflict <- function(meta) {
  keys <- c("guidance_expectation", "supported_path",
            "advanced_or_manual", "not_exposed")
  out <- list()
  for (k in keys) {
    v <- meta[[paste0("conflict.", k)]]
    if (!is.null(v) && nzchar(v)) out[[k]] <- v
  }
  out
}

.kb_first_paragraph <- function(body) {
  if (!nzchar(body)) {
    return("")
  }
  paras <- strsplit(body, "\n[ \t]*\n")[[1]]
  para <- paras[nzchar(trimws(paras))][1]
  para <- gsub("\\s+", " ", trimws(para %||% ""))
  para
}

.kb_extract_examples <- function(body, runnable = FALSE) {
  lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
  fences <- grep("^```", lines)
  if (length(fences) < 2) {
    return(list())
  }
  examples <- list()
  k <- 1L
  while (k + 1 <= length(fences)) {
    open <- fences[k]
    close <- fences[k + 1]
    lang <- sub("^```\\s*", "", lines[open])
    lang <- trimws(lang)
    code <- if (close - open > 1) {
      paste(lines[(open + 1):(close - 1)], collapse = "\n")
    } else {
      ""
    }
    examples[[length(examples) + 1]] <- list(
      code = code,
      language = if (nzchar(lang)) lang else "text",
      runnable = isTRUE(runnable) && lang %in% c("r", "R")
    )
    k <- k + 2L
  }
  examples
}

# Build the symbol index: token -> entry ids.
.kb_build_symbol_index <- function(entries) {
  idx <- list()
  for (e in entries) {
    for (s in e$symbols) {
      idx[[s]] <- unique(c(idx[[s]], e$id))
    }
  }
  idx
}

# Coerce chapter_order to integer; non-numeric or NA values sort last (999L).
.kb_chapter_order <- function(chapter_order) {
  if (is.null(chapter_order) || !length(chapter_order)) {
    return(999L)
  }
  if (length(chapter_order) != 1L) {
    warning(
      "chapter_order is unparseable or ambiguous; sorting chapter last (999).",
      call. = FALSE
    )
    return(999L)
  }
  co <- suppressWarnings(as.integer(chapter_order[[1L]]))
  if (is.na(co)) {
    warning(
      "chapter_order is unparseable or ambiguous; sorting chapter last (999).",
      call. = FALSE
    )
    return(999L)
  }
  co
}

# Ordered guidance chapter list + parent/child graph. Powers the guidance-first
# table of contents and lets guide_pharmacometrics() resolve a chapter's topics.
.kb_build_guidance_index <- function(entries) {
  chapters <- Filter(function(e) identical(e$type, "guidance_chapter"), entries)
  ord <- order(vapply(chapters, function(e) .kb_chapter_order(e$chapter_order),
                      integer(1)))
  chapters <- chapters[ord]
  lapply(chapters, function(ch) {
    kids <- Filter(function(e) {
      identical(e$type, "guidance_topic") && identical(e$parent %||% "", ch$id)
    }, entries)
    list(
      id = ch$id,
      title = ch$title,
      chapter_order = ch$chapter_order %||% NA_integer_,
      summary = ch$summary,
      steps = unlist(ch$steps, use.names = FALSE) %||% character(0),
      topics = vapply(kids, function(k) k$id, character(1))
    )
  })
}

# Map task phrases (each keyword + the title) to playbook ids, so an agent can
# jump from a task description to the right recipe.
.kb_build_playbooks_index <- function(entries) {
  pbs <- Filter(function(e) identical(e$type, "playbook"), entries)
  lapply(pbs, function(pb) {
    list(
      id = pb$id,
      title = pb$title,
      summary = pb$summary,
      parent = pb$parent %||% NA_character_,
      keywords = unlist(pb$keywords, use.names = FALSE) %||% character(0),
      steps = unlist(pb$steps, use.names = FALSE) %||% character(0)
    )
  })
}

# Traversal backbone for guide_pharmacometrics(): per chapter, the decisions,
# ordered steps, reference leaves, child topics, and linked anti-patterns.
.kb_build_guidance_graph <- function(entries) {
  by_id <- list()
  for (e in entries) by_id[[e$id]] <- e
  chapters <- Filter(function(e) identical(e$type, "guidance_chapter"), entries)
  ord <- order(vapply(chapters, function(e) e$chapter_order %||% 999L, integer(1)))
  chapters <- chapters[ord]
  lapply(chapters, function(ch) {
    rel <- unlist(ch$related, use.names = FALSE)
    steps <- unlist(ch$steps, use.names = FALSE)
    type_of <- function(id) (by_id[[id]] %||% list())$type %||% NA_character_
    decisions <- Filter(function(id) identical(type_of(id), "decision"), rel)
    anti <- Filter(function(id) identical(type_of(id), "anti_pattern"), rel)
    refs <- Filter(function(id) {
      type_of(id) %in% c("function_doc", "pml_topic", "format_topic",
                         "vignette_section", "playbook")
    }, c(steps, rel))
    kids <- Filter(function(e) {
      identical(e$type, "guidance_topic") && identical(e$parent %||% "", ch$id)
    }, entries)
    list(
      id = ch$id,
      decisions = unique(decisions),
      steps = steps,
      reference = unique(refs),
      anti_patterns = unique(anti),
      topics = vapply(kids, function(k) k$id, character(1))
    )
  })
}

#' Generate the committed KB artifacts for a package source tree
#'
#' Reads `inst/mcp/kb/sources/*.md`, validates entries, and writes
#' `inst/mcp/kb/<package>.jsonl`, `inst/mcp/kb/index/symbols.json` + `enums.json`, and
#' `inst/mcp/kb/manifest.json`. Re-running on unchanged sources reproduces
#' byte-identical JSONL and `index/*.json` artifacts; `manifest.json` also
#' carries a generation timestamp (`generated`).
#'
#' @param pkg_root Path to the package root (containing `inst/mcp/kb`).
#' @param package Package name to stamp on entries/manifest.
#' @param package_version Optional package version string.
#' @param engine Optional named list with `tdl5_version`, `grammar_source`,
#'   `audit_date` for the manifest's `engine` block.
#' @param generator_version Version string recorded in the manifest.
#' @param quiet Suppress the summary message.
#' @return Invisibly, the list of generated entries.
#' @examples
#' \dontrun{
#' generate_certara_kb(pkg_root = ".", package = "Certara.RsNLME")
#' }
#' @keywords internal
#' @export
generate_certara_kb <- function(pkg_root,
                                package,
                                package_version = NULL,
                                engine = NULL,
                                generator_version = "1.0.0",
                                quiet = FALSE) {
  kb_dir <- file.path(pkg_root, "inst", "mcp", "kb")
  src_dir <- file.path(kb_dir, "sources")
  if (!dir.exists(src_dir)) {
    stop("No sources directory at ", src_dir, call. = FALSE)
  }
  # Recurse so the guidance/, playbooks/, reference/, and remediation/ subtrees
  # are all discovered. README.md is documentation and files prefixed with "_"
  # (e.g. authoring templates) are scaffolding, not entries.
  src_files <- sort(list.files(
    src_dir, pattern = "\\.md$", full.names = TRUE, recursive = TRUE
  ))
  bn <- basename(src_files)
  src_files <- src_files[bn != "README.md" & !startsWith(bn, "_")]

  entries <- list()
  for (f in src_files) {
    entries <- c(entries, .parse_kb_source_file(f, package, package_version))
  }
  if (length(entries) == 0) {
    stop("No KB entries parsed from ", src_dir, call. = FALSE)
  }

  ids <- vapply(entries, function(e) e$id, character(1))
  dup <- ids[duplicated(ids)]
  if (length(dup)) {
    stop("Duplicate entry ids: ", paste(unique(dup), collapse = ", "),
         call. = FALSE)
  }

  problems <- character(0)
  for (e in entries) {
    p <- .validate_kb_entry(e, known_ids = ids)
    if (length(p)) {
      problems <- c(problems, sprintf("[%s] %s", e$id, p))
    }
  }
  if (length(problems)) {
    stop("KB validation failed:\n", paste(problems, collapse = "\n"),
         call. = FALSE)
  }

  index_dir <- file.path(kb_dir, "index")
  dir.create(index_dir, showWarnings = FALSE, recursive = TRUE)

  jsonl_path <- file.path(kb_dir, paste0(package, ".jsonl"))
  jsonl_lines <- vapply(
    entries,
    function(e) jsonlite::toJSON(e, auto_unbox = TRUE, null = "null"),
    character(1)
  )
  writeLines(jsonl_lines, jsonl_path, useBytes = TRUE)

  symbols <- .kb_build_symbol_index(entries)
  symbols_path <- file.path(index_dir, "symbols.json")
  writeLines(
    jsonlite::toJSON(symbols, auto_unbox = FALSE, pretty = TRUE),
    symbols_path, useBytes = TRUE
  )

  enums_path <- file.path(index_dir, "enums.json")
  writeLines(
    jsonlite::toJSON(.kb_pml_enums(), auto_unbox = FALSE, pretty = TRUE),
    enums_path, useBytes = TRUE
  )

  guidance_path <- file.path(index_dir, "guidance.json")
  writeLines(
    jsonlite::toJSON(.kb_build_guidance_index(entries),
                     auto_unbox = TRUE, pretty = TRUE, null = "null"),
    guidance_path, useBytes = TRUE
  )

  playbooks_path <- file.path(index_dir, "playbooks.json")
  writeLines(
    jsonlite::toJSON(.kb_build_playbooks_index(entries),
                     auto_unbox = TRUE, pretty = TRUE, null = "null"),
    playbooks_path, useBytes = TRUE
  )

  graph_path <- file.path(index_dir, "guidance_graph.json")
  writeLines(
    jsonlite::toJSON(.kb_build_guidance_graph(entries),
                     auto_unbox = TRUE, pretty = TRUE, null = "null"),
    graph_path, useBytes = TRUE
  )

  guidance_count <- sum(vapply(
    entries, function(e) e$type %in% .kb_guidance_types(), logical(1)
  ))
  toc_id <- paste0(package, ".guidance.toc")
  has_toc <- any(vapply(entries, function(e) identical(e$id, toc_id), logical(1)))

  manifest <- list(
    package = package,
    package_version = package_version %||% NA_character_,
    schema_version = kb_schema_version(),
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    generator_version = generator_version,
    entry_count = length(entries),
    guidance_entry_count = guidance_count,
    primary_entry = if (has_toc) toc_id else NA_character_,
    kinds = sort(unique(vapply(entries, function(e) e$type, character(1)))),
    files = list(list(
      path = basename(jsonl_path),
      sha256 = .kb_file_sha(jsonl_path),
      entry_count = length(entries)
    )),
    indexes = list(
      symbols = "index/symbols.json", enums = "index/enums.json",
      guidance = "index/guidance.json", playbooks = "index/playbooks.json",
      guidance_graph = "index/guidance_graph.json"
    ),
    hashes = list(
      sources_sha256 = .kb_dir_sha(src_files),
      jsonl_sha256 = .kb_file_sha(jsonl_path),
      symbols_sha256 = .kb_file_sha(symbols_path),
      enums_sha256 = .kb_file_sha(enums_path),
      guidance_sha256 = .kb_file_sha(guidance_path),
      playbooks_sha256 = .kb_file_sha(playbooks_path),
      guidance_graph_sha256 = .kb_file_sha(graph_path)
    )
  )
  if (!is.null(engine)) {
    manifest$engine <- engine
  }
  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    file.path(kb_dir, "manifest.json"), useBytes = TRUE
  )

  if (!quiet) {
    message(sprintf(
      "Generated %d KB entries for %s -> %s",
      length(entries), package, jsonl_path
    ))
  }
  invisible(entries)
}

.kb_file_sha <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(file = path, algo = "sha256"))
  }
  # Fallback: stable non-cryptographic fingerprint of file bytes.
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  format(sum(as.integer(bytes) * seq_along(bytes)) %% .Machine$integer.max)
}

.kb_dir_sha <- function(paths) {
  parts <- vapply(sort(paths), .kb_file_sha, character(1))
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(paste(parts, collapse = ""), algo = "sha256"))
  }
  paste(parts, collapse = "-")
}
