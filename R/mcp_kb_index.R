# KB discovery + in-process search index. Built once per server process and
# cached. Discovery scans every installed package for inst/mcp/kb/manifest.json
# (no Certara.* prefix assumption) so any provider package is discoverable.

# Package-local mutable state (index cache, discovery report). Created at load.
.mcp_state <- new.env(parent = emptyenv())

# Reset the cached index. Mainly for tests and dev reloads.
.kb_reset_index <- function() {
  rm(list = ls(.mcp_state), envir = .mcp_state)
  invisible(TRUE)
}

# MCP config key from write_mcp_config(); set at launch_certara_mcp() startup.
.mcp_set_server_name <- function(name) {
  .mcp_state$server_name <- name
  invisible(name)
}

.mcp_server_name <- function() {
  nm <- .mcp_state$server_name
  if (is.null(nm) || !nzchar(nm)) "certara-r" else nm
}

#' Discover installed-package KB manifests
#'
#' Scans installed packages (and any `dev_roots` source trees) for
#' `inst/mcp/kb/manifest.json`. Validates manifest fields and schema-version
#' compatibility; incompatible/invalid manifests are recorded but skipped.
#'
#' @param dev_roots Optional character vector of package source-tree roots to
#'   include (development override). Off by default.
#' @return A list with `packages` (loadable provider records) and `skipped`
#'   (records with a reason).
#' @keywords internal
#' @export
.kb_discover <- function(dev_roots = character(0)) {
  records <- list()

  installed <- tryCatch(
    rownames(utils::installed.packages()),
    error = function(e) character(0)
  )
  for (pkg in installed) {
    mpath <- system.file("mcp/kb/manifest.json", package = pkg)
    if (nzchar(mpath) && file.exists(mpath)) {
      records[[length(records) + 1]] <- list(
        package = pkg,
        manifest_path = mpath,
        kb_dir = dirname(mpath)
      )
    }
  }

  for (root in dev_roots) {
    mpath <- file.path(root, "inst", "mcp", "kb", "manifest.json")
    if (file.exists(mpath)) {
      records[[length(records) + 1]] <- list(
        package = NA_character_,
        manifest_path = mpath,
        kb_dir = dirname(mpath)
      )
    }
  }

  packages <- list()
  skipped <- list()
  for (rec in records) {
    res <- .kb_load_manifest(rec)
    if (is.null(res$error)) {
      packages[[length(packages) + 1]] <- res
    } else {
      skipped[[length(skipped) + 1]] <- list(
        package = res$package %||% rec$package,
        manifest_path = rec$manifest_path,
        reason = res$error
      )
    }
  }

  list(packages = packages, skipped = skipped)
}

.kb_load_manifest <- function(rec) {
  manifest <- tryCatch(
    jsonlite::fromJSON(rec$manifest_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(manifest)) {
    return(list(error = "manifest is not valid JSON", package = rec$package))
  }
  problems <- .validate_kb_manifest(manifest)
  if (length(problems)) {
    return(list(
      error = paste(problems, collapse = "; "),
      package = manifest$package %||% rec$package
    ))
  }
  if (!.kb_schema_compatible(manifest$schema_version)) {
    return(list(
      error = sprintf(
        "incompatible schema_version %s (server supports %s)",
        manifest$schema_version, kb_schema_version()
      ),
      package = manifest$package
    ))
  }
  list(
    error = NULL,
    package = manifest$package,
    manifest = manifest,
    kb_dir = rec$kb_dir
  )
}

# Load all JSONL entries declared by a validated provider record.
.kb_load_entries <- function(provider) {
  files <- provider$manifest$files
  entries <- list()
  for (f in files) {
    path <- file.path(provider$kb_dir, f$path)
    if (!file.exists(path)) next
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
    for (ln in lines) {
      e <- tryCatch(
        jsonlite::fromJSON(ln, simplifyVector = FALSE),
        error = function(err) NULL
      )
      if (!is.null(e)) entries[[length(entries) + 1]] <- e
    }
  }
  entries
}

#' Build (and cache) the KB search index for this process
#'
#' @param dev_roots Optional dev source-tree roots (passed to [.kb_discover()]).
#' @param refresh Force a rebuild even if cached.
#' @return The cached index list.
#' @keywords internal
#' @export
.kb_build_index <- function(dev_roots = character(0), refresh = FALSE) {
  if (!refresh && !is.null(.mcp_state$index)) {
    return(.mcp_state$index)
  }
  disc <- .kb_discover(dev_roots = dev_roots)

  entries <- list()
  providers <- list()
  for (p in disc$packages) {
    pkg_entries <- .kb_load_entries(p)
    entries <- c(entries, pkg_entries)
    providers[[length(providers) + 1]] <- list(
      package = p$package,
      package_version = p$manifest$package_version %||% NA_character_,
      schema_version = p$manifest$schema_version,
      entry_count = length(pkg_entries),
      engine = p$manifest$engine %||% NULL
    )
  }

  # The same package KB can be discovered twice (e.g. an installed copy plus a
  # dev source tree under load_all). Keep the first occurrence per id so
  # duplicates do not consume top-k slots or skew BM25 document frequencies.
  seen_ids <- vapply(entries, function(e) e$id %||% NA_character_, character(1))
  keep <- !duplicated(seen_ids) | is.na(seen_ids)
  entries <- entries[keep]

  by_id <- list()
  symbol_index <- list()
  doc_tokens <- vector("list", length(entries))
  for (i in seq_along(entries)) {
    e <- entries[[i]]
    by_id[[e$id]] <- e
    for (s in unlist(e$symbols, use.names = FALSE)) {
      symbol_index[[s]] <- unique(c(symbol_index[[s]], e$id))
    }
    doc_tokens[[i]] <- .kb_tokenize(.kb_entry_text(e))
  }

  index <- list(
    entries = entries,
    by_id = by_id,
    symbol_index = symbol_index,
    doc_tokens = doc_tokens,
    df = .kb_doc_frequencies(doc_tokens),
    n_docs = length(entries),
    providers = providers,
    skipped = disc$skipped,
    built_at = Sys.time()
  )
  .mcp_state$index <- index
  index
}

# Accessor that lazily builds the index on first use.
.kb_index <- function() {
  if (is.null(.mcp_state$index)) {
    .kb_build_index()
  }
  .mcp_state$index
}

# ---- text / tokenization / scoring ------------------------------------------

.kb_entry_text <- function(e) {
  paste(
    e$title %||% "",
    e$summary %||% "",
    paste(unlist(e$keywords, use.names = FALSE), collapse = " "),
    paste(unlist(e$symbols, use.names = FALSE), collapse = " "),
    e$details_md %||% "",
    collapse = " "
  )
}

.kb_tokenize <- function(text) {
  text <- tolower(text)
  toks <- unlist(strsplit(text, "[^a-z0-9_]+"))
  toks[nzchar(toks) & nchar(toks) > 1]
}

.kb_doc_frequencies <- function(doc_tokens) {
  df <- list()
  for (toks in doc_tokens) {
    for (tk in unique(toks)) {
      df[[tk]] <- (df[[tk]] %||% 0) + 1
    }
  }
  df
}

# BM25 scoring of a query against the cached index.
.kb_score_bm25 <- function(index, query, k1 = 1.2, b = 0.75) {
  q_tokens <- unique(.kb_tokenize(query))
  n <- index$n_docs
  if (n == 0 || length(q_tokens) == 0) {
    return(numeric(0))
  }
  doc_len <- vapply(index$doc_tokens, length, integer(1))
  avg_len <- mean(doc_len[doc_len > 0])
  if (!is.finite(avg_len) || avg_len == 0) avg_len <- 1

  scores <- numeric(n)
  for (i in seq_len(n)) {
    toks <- index$doc_tokens[[i]]
    if (length(toks) == 0) next
    tab <- table(toks)
    s <- 0
    for (tk in q_tokens) {
      tf <- as.integer(tab[tk])
      if (is.na(tf) || tf == 0) next
      df <- index$df[[tk]] %||% 0
      idf <- log(1 + (n - df + 0.5) / (df + 0.5))
      denom <- tf + k1 * (1 - b + b * doc_len[i] / avg_len)
      s <- s + idf * (tf * (k1 + 1)) / denom
    }
    scores[i] <- s
  }
  scores
}

# Filter helper: does entry match an applies_to scope value (or has no scope)?
.kb_scope_match <- function(entry, dim, value) {
  if (is.null(value) || !nzchar(value)) {
    return(TRUE)
  }
  vals <- tolower(unlist(entry$applies_to[[dim]], use.names = FALSE))
  length(vals) == 0 || tolower(value) %in% vals
}

# Retrieval axis of an entry: explicit `axis` wins, otherwise inferred from type.
# Guidance is the primary navigable axis; reference is supporting detail;
# remediation is fix-it content.
.kb_entry_axis <- function(e) {
  ax <- e$axis %||% ""
  if (nzchar(ax)) {
    return(ax)
  }
  t <- e$type %||% ""
  if (t %in% c("guidance_chapter", "guidance_topic", "decision", "playbook")) {
    return("guidance")
  }
  if (t %in% c("anti_pattern", "troubleshooting")) {
    return("remediation")
  }
  "reference"
}

# Guidance-axis ranking multiplier. Used only by guide_pharmacometrics() and by
# axis-filtered search so the bible surfaces before raw reference; default
# search ordering is left unboosted.
.kb_type_boost <- function(type) {
  switch(type %||% "",
    guidance_chapter = 3,
    guidance_topic = 2.5,
    playbook = 2.5,
    decision = 2,
    1
  )
}
