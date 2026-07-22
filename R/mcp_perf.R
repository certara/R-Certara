# Performance instrumentation. Measure-first: per-call stderr timing and a
# cold-start/ping/search/btw comparison report, so optimization targets the real
# bottleneck (often agent-side, not the R server). Enabled via the
# CERTARA_MCP_PERF env var to keep normal runs quiet.

.mcp_perf_enabled <- function() {
  tolower(Sys.getenv("CERTARA_MCP_PERF", "")) %in% c("1", "true", "yes")
}

# Wrap a handler call with timestamped stderr timing (dispatch -> handler-end).
.mcp_timed <- function(name, expr) {
  if (!.mcp_perf_enabled()) {
    return(force(expr))
  }
  t0 <- Sys.time()
  .mcp_perf_log(name, "handler-start", 0)
  on.exit({
    dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    .mcp_perf_log(name, "handler-end", dt)
  }, add = TRUE)
  force(expr)
}

.mcp_perf_log <- function(name, stage, dt) {
  cat(
    sprintf("[certara-mcp-perf] %s %-12s %s %.4fs\n",
            format(Sys.time(), "%H:%M:%S"), stage, name, dt),
    file = stderr()
  )
}

# ---- light execution guards + audit -----------------------------------------
# Proportionate to a first-party, single-user, stdio server: a loop/repeat guard
# (mitigates the "overthinking loop" token-amplification failure) and an opt-in
# call audit. Applied at the host .ctool chokepoint, so they cover only the
# host's own tools; builder-mode provider tools (e.g. Certara.RsNLME) are
# timed/guarded by their own package, and the canonical job-polling loop is
# already designed out via wait_for_nlme_job's server-side blocking watch.

.mcp_loop_guard_on <- function() {
  !(tolower(Sys.getenv("CERTARA_MCP_LOOP_GUARD", "1")) %in% c("0", "false", "no"))
}

.mcp_loop_threshold <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("CERTARA_MCP_LOOP_MAX", "5")))
  if (length(n) != 1L || is.na(n) || n < 2L) 5L else n
}

.mcp_arg_hash <- function(args) {
  tryCatch(digest::digest(args), error = function(e) paste0("n", length(args)))
}

# Record a call and return how many times THIS (tool + args) has been invoked
# consecutively. A different signature resets the counter, so interleaved calls
# never trip the guard.
.mcp_note_call <- function(name, args) {
  sig <- paste0(name, ":", .mcp_arg_hash(args))
  prev <- .mcp_state$last_call
  st <- if (!is.null(prev) && identical(prev$sig, sig)) {
    list(sig = sig, count = prev$count + 1L)
  } else {
    list(sig = sig, count = 1L)
  }
  .mcp_state$last_call <- st
  st$count
}

# Advisory result returned in place of executing an identical call for the Nth+
# consecutive time, so the agent breaks the cycle instead of amplifying tokens.
.mcp_loop_nudge <- function(name, count) {
  list(
    loop_detected = TRUE,
    tool = name,
    repeats = count,
    message = sprintf(
      paste("'%s' was called with identical arguments %d times in a row and was",
            "not re-run. Change the arguments, choose a different tool",
            "(find_certara_tools can help), or proceed - the previous result",
            "still stands."),
      name, count
    )
  )
}

.mcp_audit_enabled <- function() {
  tolower(Sys.getenv("CERTARA_MCP_AUDIT", "")) %in% c("1", "true", "yes")
}

# Single invocation chokepoint used by .ctool: loop guard -> timing -> audit ->
# call. On the Nth+ identical consecutive call it returns the nudge WITHOUT
# executing the handler (so a stuck agent cannot keep re-running it).
.mcp_invoke <- function(name, fun, supplied) {
  if (.mcp_loop_guard_on() &&
      .mcp_note_call(name, supplied) >= .mcp_loop_threshold()) {
    return(.mcp_loop_nudge(name, .mcp_state$last_call$count))
  }
  .mcp_timed(name, {
    res <- tryCatch(
      do.call(fun, supplied),
      error = function(e) {
        .mcp_audit(name, supplied, error_class = class(e)[1])
        stop(e)
      }
    )
    .mcp_audit(name, supplied, result = res)
    res
  })
}

# Opt-in structured audit line (no values, only sizes/classes) for observability.
.mcp_audit <- function(name, args, result = NULL, error_class = NULL) {
  if (!.mcp_audit_enabled()) return(invisible())
  outcome <- if (!is.null(error_class)) paste0("error=", error_class) else {
    sz <- tryCatch(length(unlist(result, use.names = FALSE)), error = function(e) NA)
    paste0("ok result_size=", sz)
  }
  cat(
    sprintf("[certara-mcp-audit] %s %s args=%s %s\n",
            format(Sys.time(), "%H:%M:%S"), name, .mcp_arg_hash(args), outcome),
    file = stderr()
  )
  invisible()
}

#' Performance report for the Certara MCP server
#'
#' Times cold-start namespace loads and compares a trivial capabilities/ping
#' call against KB search (and a btw pkg tool when available). Slow ping points
#' at startup/loop; slow only on search points at index/serialization; fast ping
#' but slow session points at agent-side cost.
#'
#' @param query KB query used for the search timing.
#' @return A list of timings (seconds).
#' @examples
#' \dontrun{
#' certara_mcp_perf_report()
#' }
#' @keywords internal
#' @export
certara_mcp_perf_report <- function(query = "structural parameters") {
  time_it <- function(expr) {
    as.numeric(system.time(force(expr))[["elapsed"]])
  }

  cold <- list(
    certara_rsnlme = tryCatch(
      time_it(loadNamespace("Certara.RsNLME")), error = function(e) NA_real_
    ),
    jsonlite = tryCatch(
      time_it(loadNamespace("jsonlite")), error = function(e) NA_real_
    ),
    btw = tryCatch(
      time_it(loadNamespace("btw")), error = function(e) NA_real_
    )
  )

  # Ensure index is warm so we time steady-state, not first build.
  invisible(tryCatch(.kb_index(), error = function(e) NULL))

  per_call <- list(
    ping = time_it(certara_mcp_capabilities()),
    kb_search = time_it(search_certara_kb(query, limit = 5)),
    kb_get = time_it(get_certara_kb_entry("Certara.RsNLME.pml.stparm"))
  )

  list(
    cold_start_seconds = cold,
    per_call_seconds = per_call,
    note = paste(
      "Cold start is one-time per process. If ping is fast but a full agent",
      "turn feels slow, the dominant cost is agent-side (model latency +",
      "serialized tool calls + large results), not this server."
    )
  )
}
