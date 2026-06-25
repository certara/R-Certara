# Tool retrieval. A BM25 index over the registered tools' name + description so
# an agent can discover the relevant handful via find_certara_tools() instead of
# scanning the whole catalog (large flat tool lists degrade tool-selection
# accuracy). Reuses the KB tokenizer / document-frequency / BM25 scorer from
# mcp_kb_index.R, so there is one scoring implementation. mcptools cannot change
# the advertised tool set after the server starts, so this is a selection aid
# over the full set, not a dynamic-listing mechanism.

# First sentence of a description, for a compact one-line summary.
.mcp_first_sentence <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return("")
  m <- regexpr("^.*?[.!?](\\s|$)", x)
  if (m > 0) trimws(regmatches(x, m)) else x
}

# Coarse provider label from a tool name (cosmetic; the BM25 text is name+desc).
.mcp_tool_provider_label <- function(name) {
  if (startsWith(name, "btw_")) "btw" else "certara"
}

# Build records {name, description, provider, text} from a list of ellmer tools.
# The indexed `text` is the name + the FIRST sentence (the purpose) only, NOT
# the full description: later "for X use Y" disambiguation cues mention sibling
# tool names, and indexing those would pollute BM25 with other tools' tokens.
# The agent still reads the full description; retrieval scores on clean purpose.
.mcp_tool_records <- function(tools) {
  lapply(tools, function(t) {
    nm <- t@name
    desc <- t@description %||% ""
    list(name = nm, description = desc,
         provider = .mcp_tool_provider_label(nm),
         text = paste(nm, .mcp_first_sentence(desc)))
  })
}

# Build (uncached) a BM25 index over a list of ellmer ToolDefs.
.mcp_build_tool_index <- function(tools) {
  records <- .mcp_tool_records(tools)
  doc_tokens <- lapply(records, function(r) .kb_tokenize(r$text))
  list(
    records = records,
    doc_tokens = doc_tokens,
    df = .kb_doc_frequencies(doc_tokens),
    n_docs = length(records)
  )
}

# Cache setter, called at launch_certara_mcp() once tools are assembled.
.mcp_set_tool_index <- function(index) {
  .mcp_state$tool_index <- index
  invisible(index)
}

# Lazy accessor: outside a running server (tests, direct calls) build from the
# host tools plus any discovered/dev_roots providers.
.mcp_tool_index <- function(dev_roots = character(0)) {
  if (!is.null(.mcp_state$tool_index)) {
    return(.mcp_state$tool_index)
  }
  prov <- tryCatch(.mcp_provider_tools(dev_roots = dev_roots)$tools,
                   error = function(e) list())
  idx <- .mcp_build_tool_index(c(.certara_host_tools(), prov))
  .mcp_set_tool_index(idx)
  idx
}

#' Discover the tools relevant to a task
#'
#' Retrieval over the registered tool catalog: returns the few tools whose
#' name/description best match a free-text task, so the agent picks from a short
#' ranked list instead of the full set. Call this first when unsure which tool
#' fits; then call the chosen tool(s) directly.
#'
#' @param task Free-text description of what you are trying to do.
#' @param limit Maximum tools to return; clamped to 3..10 (default 5).
#' @return A list with `query`, ranked `tools` (`name`, `summary`, `provider`,
#'   `score`), and a `note`. Empty `tools` means broaden the task or call
#'   [certara_mcp_capabilities()].
#' @examples
#' \dontrun{
#' find_certara_tools("fit a population model from a spec")
#' find_certara_tools("wait for a running fit to finish", limit = 3)
#' }
#' @keywords internal
#' @export
find_certara_tools <- function(task, limit = 5) {
  limit <- suppressWarnings(as.integer(limit))
  if (length(limit) != 1L || is.na(limit)) limit <- 5L
  limit <- max(3L, min(limit, 10L))

  idx <- .mcp_tool_index()
  if (idx$n_docs == 0) {
    return(list(query = task, tools = list(),
                note = "No tools are indexed."))
  }
  scores <- .kb_score_bm25(idx, task)
  ord <- order(scores, decreasing = TRUE)

  out <- list()
  for (i in ord) {
    if (scores[i] <= 0) break
    r <- idx$records[[i]]
    if (identical(r$name, "find_certara_tools")) next  # never recommend itself
    out[[length(out) + 1]] <- list(
      name = r$name,
      summary = .mcp_first_sentence(r$description),
      provider = r$provider,
      score = round(scores[i], 4)
    )
    if (length(out) >= limit) break
  }
  list(
    query = task,
    tools = out,
    note = paste(
      "Ranked by relevance. Call the chosen tool(s) directly. If none fit,",
      "broaden the task or call certara_mcp_capabilities() for the full map."
    )
  )
}
