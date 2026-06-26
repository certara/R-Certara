# Host-owned MCP tools as ellmer ToolDefs: the generic knowledge (KB), memory,
# and meta tools the host ships itself. Provider packages contribute their own
# tools via inst/mcp/tools/manifest.json (see mcp_tool_providers.R). State is
# explicit: context is passed as arguments or returned as handles, never inferred
# from a hidden session.

# Convenience constructors over ellmer types.
.ts <- function(desc, required = FALSE) ellmer::type_string(desc, required = required)
.ti <- function(desc, required = FALSE) ellmer::type_integer(desc, required = required)
.tb <- function(desc, required = FALSE) ellmer::type_boolean(desc, required = required)

# Wrap a handler so each invocation is timed when perf logging is on. ellmer
# requires the tool function's formals to match the declared `arguments`, so the
# wrapper clones the handler's formals and forwards the supplied args.
.ctool <- function(fun, name, description, arguments = list()) {
  force(fun)
  force(name)
  timed <- function() {
    supplied <- as.list(match.call())[-1]
    supplied <- lapply(supplied, eval, envir = parent.frame())
    # Loop guard + timing + audit applied at a single chokepoint.
    .mcp_invoke(name, fun, supplied)
  }
  formals(timed) <- formals(fun)
  ellmer::tool(timed, description = description, arguments = arguments,
               name = name)
}

.ctools_host_meta <- function() {
  list(
    .ctool(
      function() certara_mcp_capabilities(),
      "certara_mcp_capabilities",
      paste(
        "Server capabilities: versions, discovered KB and tool providers,",
        "concurrency policy (retry sequentially if parallel calls fail),",
        "client_routing (Cursor CallMcpTool server is user-{config_key}, not the",
        "mcp.json key), gated tools, the merged behavior rules, and a reminder to",
        "fetch preferences/lessons at session start. Call this first."
      )
    ),
    .ctool(
      function() list_certara_kb_packages(),
      "list_certara_kb_packages",
      "List discovered KB provider packages and any skipped/incompatible ones."
    ),
    .ctool(
      function(task, limit = 5) find_certara_tools(task, limit),
      "find_certara_tools",
      paste(
        "Discover which tools fit a task: returns a short ranked list of the",
        "most relevant tools (name, one-line summary, score) from the full",
        "catalog. Call this FIRST when unsure which tool to use, then call the",
        "chosen tool(s) directly - avoids scanning every registered tool."
      ),
      arguments = list(
        task = .ts("Free-text description of what you are trying to do.",
                   required = TRUE),
        limit = .ti("Max tools to return; clamped to 3..10 (default 5).")
      )
    ),
    .ctool(
      function() certara_session_status(),
      "certara_session_status",
      paste(
        "Report which R this server can act on and what it can reach: whether",
        "the live-session bridge (session_tools) is enabled, which btw groups",
        "are active (env inspection, run = btw_tool_run_r code execution), the",
        "three execution contexts (server process vs bridged",
        "btw::btw_mcp_session() session vs provider job child), and the steps",
        "to attach a live session. Call this when unsure where objects live or",
        "why btw_tool_run_r / env tools are missing."
      )
    )
  )
}

.ctools_host_knowledge <- function() {
  list(
    .ctool(
      function(query, package = NULL, type = NULL, model_type = NULL,
               route = NULL, engine = NULL, limit = 5) {
        search_certara_kb(query, package = package, type = type,
                          model_type = model_type, route = route,
                          engine = engine, limit = limit)
      },
      "search_certara_kb",
      paste(
        "Search the Certara knowledge base by keyword for facts and PML/R",
        "syntax (grammar, R-API, anti-patterns).",
        "Returns compact summaries; fetch full text with get_certara_kb_entry.",
        "Use for 'what is X'; for a task's decisions use guide_pharmacometrics,",
        "for ordered steps explain_certara_workflow, for code find_certara_examples."
      ),
      arguments = list(
        query = .ts("Free-text search query.", required = TRUE),
        package = .ts("Optional provider package filter."),
        type = .ts("Optional type filter (pml_topic, function_doc, workflow_recipe, anti_pattern, ...)."),
        model_type = .ts("Optional scope filter (pk, pd)."),
        route = .ts("Optional route scope filter."),
        engine = .ts("Optional engine scope filter."),
        limit = .ti("Max results (default 5).")
      )
    ),
    .ctool(
      function(id) get_certara_kb_entry(id),
      "get_certara_kb_entry",
      "Retrieve a full KB entry (details, examples, provenance) by its id.",
      arguments = list(id = .ts("Stable entry id.", required = TRUE))
    ),
    .ctool(
      function(query, package = NULL, limit = 5) {
        find_certara_examples(query, package, limit)
      },
      "find_certara_examples",
      paste(
        "Find illustrative/runnable PML or R example code for a query. Use when",
        "you want copyable code; for concepts/syntax use search_certara_kb."
      ),
      arguments = list(
        query = .ts("Free-text query.", required = TRUE),
        package = .ts("Optional provider package filter."),
        limit = .ti("Max results (default 5).")
      )
    ),
    .ctool(
      function(task, intended_use = NULL, limit = 2) {
        guide_pharmacometrics(task, intended_use = intended_use, limit = limit)
      },
      "guide_pharmacometrics",
      paste(
        "Guidance-first entry point for a modeling task: resolves intended use,",
        "then returns the regulatory guidance, decisions, ordered checks, and",
        "anti-patterns for what to do and why (summary-first; fetch bodies with",
        "get_certara_kb_entry).",
        "Call BEFORE structural/covariate/validation work; for ordered how-to",
        "steps use explain_certara_workflow, for raw syntax search_certara_kb."
      ),
      arguments = list(
        task = .ts("Description of the modeling task or question.",
                   required = TRUE),
        intended_use = .ts(paste(
          "Fit-for-purpose level: exploratory, dose_selection, labeling,",
          "pediatric, or er_input. Scales required checks/validation depth."
        )),
        limit = .ti("Max guidance chapters to resolve (default 2).")
      )
    ),
    .ctool(
      function(task, packages = NULL, limit = 3) {
        explain_certara_workflow(task, packages, limit)
      },
      "explain_certara_workflow",
      paste(
        "Return ordered step-by-step Certara workflow recipes (how-to) for a",
        "task. Use for the sequence of steps; for the guidance/decisions behind",
        "them use guide_pharmacometrics, for raw syntax use search_certara_kb."
      ),
      arguments = list(
        task = .ts("Description of the modeling task.", required = TRUE),
        packages = ellmer::type_array(items = ellmer::type_string(),
                                      description = "Optional provider filter.",
                                      required = FALSE),
        limit = .ti("Max recipes (default 3).")
      )
    ),
    .ctool(
      function(symbol) lookup_pml_symbol(symbol),
      "lookup_pml_symbol",
      "Exact PML token/function lookup (e.g. stparm, dosepoint) via the symbol index.",
      arguments = list(symbol = .ts("Exact PML symbol.", required = TRUE))
    ),
    .ctool(
      function(kind = NULL) list_pml_enums(kind),
      "list_pml_enums",
      "List controlled PML vocabularies (param_style, error_model, absorption, elimination, distribution).",
      arguments = list(kind = .ts("Optional enum name."))
    )
  )
}

.ctools_host_memory <- function() {
  list(
    .ctool(
      function(context = NULL) get_user_preferences(context),
      "get_user_preferences",
      "Get active default preferences (call at session start when memory is on).",
      arguments = list(context = .ts("Optional scope/context tag."))
    ),
    .ctool(
      function(context = NULL) get_lessons(context),
      "get_lessons",
      "Get corrective lessons + best practices (call at session start when memory is on).",
      arguments = list(context = .ts("Optional scope/context tag."))
    ),
    .ctool(
      function(lesson, category = "corrective", trigger = NULL,
               scope = "global", level = NULL) {
        record_lesson(lesson, category, trigger, scope, level)
      },
      "record_lesson",
      "Record a corrective lesson or endorsed best practice for future sessions.",
      arguments = list(
        lesson = .ts("What went wrong / the corrected approach.", required = TRUE),
        category = .ts("'corrective' or 'best_practice'."),
        trigger = .ts("'self_detected' or 'user_feedback'."),
        scope = .ts("'global' or a context tag."),
        level = .ts("'hard' or 'soft'.")
      )
    ),
    .ctool(
      function(id) deactivate_lesson(id),
      "deactivate_lesson",
      "Deactivate a lesson without deleting it (history preserved).",
      arguments = list(id = .ts("Lesson record id.", required = TRUE))
    ),
    .ctool(
      function(summary, scope = "global") record_run(summary, scope),
      "record_run",
      "Record a quantitative run fingerprint for future sessions.",
      arguments = list(
        summary = .ts("Short summary of the run.", required = TRUE),
        scope = .ts("'global' or a context tag.")
      )
    ),
    .ctool(
      function(key, value, scope = "global", level = "hard") {
        set_preference(key, value, scope, level)
      },
      "set_preference",
      "Set a default user preference (append-only with supersede).",
      arguments = list(
        key = .ts("Preference key.", required = TRUE),
        value = .ts("Preference value.", required = TRUE),
        scope = .ts("'global' or a context tag."),
        level = .ts("'hard' or 'soft'.")
      )
    ),
    .ctool(
      function() list_memory_records(),
      "list_memory_records",
      "Inspect all stored memory records (run memory, lessons, preferences)."
    )
  )
}

#' Host-owned Certara MCP tools
#'
#' The knowledge (KB), memory, and meta tools the host exposes itself, as a list
#' of `ellmer::tool()` objects. Provider-specific tools are added separately by
#' [.mcp_provider_tools()].
#'
#' @param groups Host tool groups to include: any of `"meta"`, `"knowledge"`,
#'   `"memory"` (default all). Launch profiles pass a subset.
#' @return A list of ellmer ToolDef objects.
#' @keywords internal
#' @export
.certara_host_tools <- function(groups = c("meta", "knowledge", "memory")) {
  groups <- match.arg(groups, several.ok = TRUE)
  tools <- list()
  if ("meta" %in% groups) tools <- c(tools, .ctools_host_meta())
  if ("knowledge" %in% groups) tools <- c(tools, .ctools_host_knowledge())
  if ("memory" %in% groups) tools <- c(tools, .ctools_host_memory())
  tools
}
