# Host capabilities: generic and provider-agnostic. The host advertises its own
# routing / concurrency / memory rules and MERGES capability fragments that
# provider packages ship at inst/mcp/capabilities.json. Domain rules (NLME
# fit/VPC preflight, BLQ, covariate-search gating, per-domain workflow phase
# overlays, job-watch advertisement) live in the providers' fragments, not here.

# Generic "use the supported provider tools and KB first" rule. Domain-specific
# elaborations come from provider fragments.
.rule_providers_first <- function() {
  paste(
    "Provider-first: for a domain task, prefer the supported Certara provider",
    "tools and KB-documented workflows over ad hoc code. Discover what is",
    "available with certara_mcp_capabilities() (tool/KB providers, merged rules,",
    "gated tools) and search the KB before inventing syntax or workflows.",
    "Generic R remains valid when it is the right tool - state why you fall back."
  )
}

# Reproducible-script behavior contract (generic; a core host capability).
.rule_repro_script <- function() {
  paste(
    "Reproducible script: the host records the exact R code run by MCP tools",
    "into a single runnable .R script this session. Tools that create objects,",
    "tables, or plots append their code; re-running the script reproduces the",
    "analysis by hand. Set certara_session_project_dir at project start so the",
    "script lands under scripts/. Report the script path (certara_repro_script)",
    "so the user can audit and QC the work; fetch its contents with",
    "get_certara_repro_script."
  )
}

.rule_report_rmd <- function() {
  paste(
    "Report Rmd: plot and table MCP tools register figures and numeric output",
    "into a single modeling report Rmd (certara_report_rmd). Set",
    "certara_session_project_dir first so figures/ and reports/ co-locate.",
    "Add narrative with add_certara_report_note; render with",
    "render_certara_report when rmarkdown and pandoc are available."
  )
}

.rule_vpc_two_step <- function() {
  paste(
    "VPC two-step: run VPC simulation with Certara.RsNLME (start_nlme_vpcmodel,",
    "collect_nlme_job), then build and plot with tidyvpc MCP tools",
    "(tidyvpc_load_from_dir, tidyvpc_build_vpc, tidyvpc_plot_vpc). Use",
    "summarize_vpc for a fast numeric check without plotting. Figures register",
    "in the report Rmd and code appends to the reproducible script."
  )
}

# Memory-and-sources behavior contract (generic; applies to every provider).
.rule_memory_and_sources <- function() {
  paste(
    "Memory and sources: (1) Provenance mandatory - every KB fact, memory",
    "record, and literature citation carries source + timestamp + version.",
    "(2) Freshness discipline - run memory is append-only with timestamps; KB",
    "ships an engine-version stamp; treat a stale stamp as a refresh signal.",
    "(3) External-source precedence - uncited claims may never be presented as",
    "established literature values; engine/source ground truth wins over drafts."
  )
}

# Per-client CallMcpTool / namespace ids (config key != Cursor runtime id).
.mcp_client_routing <- function(server_name) {
  list(
    config_key = server_name,
    cursor = list(
      call_mcp_server = paste0("user-", server_name),
      note = "Cursor prefixes user MCP servers with 'user-'; use call_mcp_server."
    ),
    claude_code = list(
      call_mcp_server = server_name,
      note = paste("Claude Code namespaces tools as mcp__{key}__{tool};",
                   "write_mcp_config() installs a CLAUDE.md-imported guidance doc.")
    ),
    claude_desktop = list(
      call_mcp_server = server_name,
      guidance_file = tryCatch(
        file.path(.claude_desktop_config_dir(), "certara-mcp-usage.md"),
        error = function(e) NA_character_
      ),
      note = paste("Claude Desktop / Cowork namespaces tools as mcp__{key}__{tool};",
                   "guidance_file is manual reference (no CLAUDE.md auto-import).")
    ),
    codex = list(
      call_mcp_server = server_name,
      note = "Codex uses the TOML mcp_servers key as the tool namespace."
    )
  )
}

# Member packages currently attached (search path), for the capabilities report.
.certara_attached <- function() {
  pkgs <- tryCatch(certara_core_packages(), error = function(e) character(0))
  pkgs[paste0("package:", pkgs) %in% search()]
}

# ---- capability fragment discovery + merge ----------------------------------

.mcp_validate_capability_fragment <- function(frag) {
  problems <- character(0)
  if (is.null(frag$package) || !is.character(frag$package) ||
      length(frag$package) != 1 || !nzchar(frag$package)) {
    problems <- c(problems, "missing/invalid field 'package'")
  }
  if (!is.null(frag$rules)) {
    if (!is.list(frag$rules)) {
      problems <- c(problems, "field 'rules' must be an array")
    } else {
      for (i in seq_along(frag$rules)) {
        r <- frag$rules[[i]]
        if (!.is_scalar_string(r$id) || !.is_scalar_string(r$text)) {
          problems <- c(problems, sprintf("rules[%d] needs id + text", i))
        }
      }
    }
  }
  problems
}

#' Discover provider capability fragments
#'
#' @param dev_roots Optional package source-tree roots to include.
#' @return A list with `fragments` (valid) and `skipped` (with a reason).
#' @keywords internal
#' @export
.mcp_discover_capabilities <- function(dev_roots = character(0)) {
  records <- list()
  installed <- tryCatch(rownames(utils::installed.packages()),
                        error = function(e) character(0))
  for (pkg in installed) {
    cpath <- system.file("mcp/capabilities.json", package = pkg)
    if (nzchar(cpath) && file.exists(cpath)) {
      records[[length(records) + 1]] <- list(package = pkg, path = cpath)
    }
  }
  for (root in dev_roots) {
    cpath <- file.path(root, "inst", "mcp", "capabilities.json")
    if (file.exists(cpath)) {
      records[[length(records) + 1]] <- list(package = NA_character_, path = cpath)
    }
  }

  fragments <- list()
  skipped <- list()
  seen <- character(0)
  for (rec in records) {
    frag <- tryCatch(jsonlite::fromJSON(rec$path, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.null(frag)) {
      skipped[[length(skipped) + 1]] <- list(
        package = rec$package, reason = "capabilities.json is not valid JSON")
      next
    }
    problems <- .mcp_validate_capability_fragment(frag)
    if (length(problems)) {
      skipped[[length(skipped) + 1]] <- list(
        package = frag$package %||% rec$package,
        reason = paste(problems, collapse = "; "))
      next
    }
    if (frag$package %in% seen) next
    seen <- c(seen, frag$package)
    fragments[[length(fragments) + 1]] <- frag
  }
  list(fragments = fragments, skipped = skipped)
}

# Pure merge over an ordered list of capability fragments (no discovery), so the
# contract is unit-testable independent of what is installed: provider rules
# override host rules of the same id; session_start/prerequisites concatenate +
# dedupe; the first fragment's primary_guidance_entry wins; workflows per package.
.mcp_merge_capability_fragments <- function(fragments) {
  rules <- list()
  session_start <- character(0)
  prerequisites <- character(0)
  workflows <- list()
  primary <- NULL
  providers <- character(0)
  for (frag in fragments) {
    providers <- c(providers, frag$package)
    for (r in frag$rules %||% list()) rules[[r$id]] <- r$text
    session_start <- c(session_start, unlist(frag$session_start, use.names = FALSE))
    prerequisites <- c(prerequisites, unlist(frag$prerequisites, use.names = FALSE))
    if (!is.null(frag$workflows)) workflows[[frag$package]] <- frag$workflows
    if (is.null(primary) && !is.null(frag$primary_guidance_entry)) {
      primary <- frag$primary_guidance_entry
    }
  }
  list(rules = rules,
       session_start = unique(session_start),
       prerequisites = unique(prerequisites),
       workflows = workflows,
       primary_guidance_entry = primary,
       providers = providers)
}

.mcp_merge_capabilities <- function(dev_roots = character(0)) {
  disc <- .mcp_discover_capabilities(dev_roots)
  out <- .mcp_merge_capability_fragments(disc$fragments)
  out$skipped <- disc$skipped
  out
}

#' Certara MCP server capabilities
#'
#' Early-signal contract for agents: server/schema versions, discovered KB and
#' tool providers, concurrency policy, gated tool names, the attached ecosystem,
#' and the merged behavior rules (generic host rules plus provider fragments).
#'
#' @param dev_roots Optional dev source-tree roots for provider discovery.
#' @return A structured capabilities list.
#' @examples
#' cap <- certara_mcp_capabilities()
#' cap$server
#' names(cap$rules)
#' @keywords internal
#' @export
certara_mcp_capabilities <- function(dev_roots = character(0)) {
  index <- tryCatch(.kb_index(), error = function(e) NULL)
  kb_providers <- if (is.null(index)) list() else index$providers
  kb_skipped <- if (is.null(index)) list() else index$skipped
  server_name <- .mcp_server_name()

  tool_disc <- tryCatch(.mcp_discover_tool_providers(dev_roots),
                        error = function(e) list(providers = list(), skipped = list()))
  frag <- .mcp_merge_capabilities(dev_roots)

  host_rules <- list(
    providers_first = .rule_providers_first(),
    repro_script = .rule_repro_script(),
    report_rmd = .rule_report_rmd(),
    vpc_two_step = .rule_vpc_two_step(),
    memory_and_sources = .rule_memory_and_sources()
  )
  # Provider rules override host rules of the same id (more specific wins).
  rules <- utils::modifyList(host_rules, frag$rules)

  list(
    server = server_name,
    client_routing = .mcp_client_routing(server_name),
    active_tool_profile = .mcp_launch_config()$tool_profile %||% NA_character_,
    package_version = as.character(utils::packageVersion("Certara.R")),
    kb_schema_version = kb_schema_version(),
    tools_schema_version = mcp_tools_schema_version(),
    primary_guidance_entry = frag$primary_guidance_entry,
    kb_providers = lapply(kb_providers, function(p) {
      list(package = p$package, entry_count = p$entry_count,
           schema_version = p$schema_version)
    }),
    skipped_kb_providers = lapply(kb_skipped, function(s) {
      list(package = s$package, reason = s$reason)
    }),
    tool_providers = lapply(tool_disc$providers, function(p) {
      list(package = p$package, mode = p$mode, tool_count = p$tool_count,
           source = p$source %||% "installed")
    }),
    skipped_tool_providers = lapply(tool_disc$skipped, function(s) {
      list(package = s$package, reason = s$reason)
    }),
    gated_tools = .mcp_gated_tool_names(dev_roots),
    capability_providers = frag$providers,
    attached_ecosystem = .certara_attached(),
    concurrency = list(
      parallel_tools_call = "best_effort",
      recommended_retry = "sequential",
      note = "Short control-plane calls; retry sequentially on parallel failures."
    ),
    rules = rules,
    workflows = frag$workflows,
    prerequisites = frag$prerequisites,
    session_start = list(
      memory_enabled = .memory_enabled(),
      instruction = paste(
        "If memory is enabled, call get_user_preferences() and get_lessons()",
        "before proposing a workflow. Diagnose setup with certara_session_status()."
      ),
      provider_notes = .mcp_unique_session_start(frag$session_start)
    ),
    execution_contexts = list(
      status_tool = "certara_session_status",
      note = paste("Three R contexts (server process, bridged live session,",
                   "per-job child); call certara_session_status for wiring.")
    ),
    environment_notes = list(
      help_docs_pandoc = paste("btw_tool_docs_help_page renders Rd via pandoc;",
                                "prefer lookup_pml_symbol / get_certara_kb_entry",
                                "for Certara/PML APIs.")
    )
  )
}

# Drop only the clause(s) that repeat the host's memory-bootstrap pointer
# (mentions get_user_preferences OR get_lessons) from each fragment
# session_start item, keeping any other clause sharing that item. Providers
# often combine a memory-bootstrap reminder with unique guidance in the same
# item (e.g. Certara.RsNLME pairs it with a setup_troubleshooting pointer) -
# dropping the whole item would silently lose that guidance. Split on
# sentence/clause boundaries (., ;, !, ? followed by whitespace) without
# requiring a capitalized word afterward - a lowercase continuation like
# "get_lessons(); then consult setup_troubleshooting." must still split.
.mcp_unique_session_start <- function(items) {
  items <- unique(unlist(items, use.names = FALSE))
  if (!length(items)) return(character(0))
  out <- character(0)
  for (item in items) {
    clauses <- strsplit(item, "(?<=[.;!?])\\s+", perl = TRUE)[[1]]
    keep <- clauses[!grepl("get_user_preferences|get_lessons", clauses, ignore.case = TRUE)]
    if (length(keep)) out <- c(out, paste(keep, collapse = " "))
  }
  unique(out)
}

#' Export active memory as a client rule file
#'
#' Materializes active hard preferences and high-priority (corrective) lessons
#' into a client-loaded rule file so defaults auto-apply without a tool call.
#' Only writes when the user invokes it - never an automatic/surprise write.
#'
#' @param path Destination rule file. Defaults to `.cursor/rules/certara-memory.md`.
#' @param context Optional scope tag to include alongside global records.
#' @return Invisibly the written path.
#' @examples
#' \dontrun{
#' export_certara_memory_rule()
#' }
#' @keywords internal
#' @export
export_certara_memory_rule <- function(path = NULL, context = NULL) {
  if (!.memory_enabled()) {
    stop("Memory is disabled; nothing to export. Enable with enable_memory().",
         call. = FALSE)
  }
  path <- path %||% file.path(".cursor", "rules", "certara-memory.md")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  prefs <- Filter(function(p) identical(p$level, "hard"),
                  get_user_preferences(context))
  lessons <- Filter(function(l) identical(l$category, "corrective"),
                    get_lessons(context))

  lines <- c(
    "# Certara memory (auto-generated; do not edit by hand)",
    "",
    "Exported active hard preferences and corrective lessons.",
    ""
  )
  if (length(prefs)) {
    lines <- c(lines, "## Default preferences", "")
    for (p in prefs) {
      lines <- c(lines, sprintf("- %s = %s (scope: %s)", p$key, p$value, p$scope))
    }
    lines <- c(lines, "")
  }
  if (length(lessons)) {
    lines <- c(lines, "## Corrective lessons", "")
    for (l in lessons) {
      lines <- c(lines, sprintf("- %s (scope: %s)", l$text, l$scope))
    }
    lines <- c(lines, "")
  }
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}
