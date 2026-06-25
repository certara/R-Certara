# Tool-provider discovery. Mirrors the KB federation in mcp_kb_index.R: scan
# installed packages (and any dev_roots) for inst/mcp/tools/manifest.json, then
# materialize each provider's tools as ellmer ToolDefs. Two manifest modes:
#   - declarative: the manifest lists tools + handler names + argument types;
#     the host binds each handler and maps argument types to ellmer::type_*.
#   - builder: the manifest names an exported function returning ellmer tools
#     (used by Certara.RsNLME, whose tool set is built programmatically).
# A broken or schema-incompatible manifest is skipped and reported, never fatal.

#' Tool-manifest schema version implemented by this build.
#' @examples
#' mcp_tools_schema_version()
#' @keywords internal
#' @export
mcp_tools_schema_version <- function() {
  "1.0.0"
}

# Same MAJOR / MINOR-not-newer rule as the KB schema.
.mcp_tools_schema_compatible <- function(provided) {
  .kb_schema_compatible(provided, mcp_tools_schema_version())
}

.mcp_validate_tools_manifest <- function(manifest) {
  problems <- character(0)
  for (f in c("package", "schema_version")) {
    if (is.null(manifest[[f]]) || !is.character(manifest[[f]]) ||
        length(manifest[[f]]) != 1 || !nzchar(manifest[[f]])) {
      problems <- c(problems, sprintf("missing/invalid manifest field '%s'", f))
    }
  }
  has_builder <- !is.null(manifest$builder) && nzchar(manifest$builder %||% "")
  has_tools <- !is.null(manifest$tools) && length(manifest$tools) > 0
  if (has_builder && has_tools) {
    problems <- c(problems, "manifest sets both 'builder' and 'tools' (use one)")
  }
  if (!has_builder && !has_tools) {
    problems <- c(problems, "manifest must set either 'builder' or 'tools'")
  }
  problems
}

#' Discover installed-package tool manifests
#'
#' @param dev_roots Optional package source-tree roots to include.
#' @return A list with `providers` (loadable records) and `skipped` (with a
#'   reason). Each provider record carries `package`, `manifest`, `mode`
#'   ("builder"/"declarative"), `tool_count`, and `manifest_path`.
#' @keywords internal
#' @export
.mcp_discover_tool_providers <- function(dev_roots = character(0)) {
  records <- list()
  installed <- tryCatch(rownames(utils::installed.packages()),
                        error = function(e) character(0))
  for (pkg in installed) {
    mpath <- system.file("mcp/tools/manifest.json", package = pkg)
    if (nzchar(mpath) && file.exists(mpath)) {
      records[[length(records) + 1]] <- list(package = pkg, manifest_path = mpath,
                                              source = "installed")
    }
  }
  for (root in dev_roots) {
    mpath <- file.path(root, "inst", "mcp", "tools", "manifest.json")
    if (file.exists(mpath)) {
      records[[length(records) + 1]] <- list(package = NA_character_,
                                              manifest_path = mpath,
                                              source = "dev_root")
    }
  }

  providers <- list()
  skipped <- list()
  seen_pkg <- character(0)
  for (rec in records) {
    manifest <- tryCatch(
      jsonlite::fromJSON(rec$manifest_path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(manifest)) {
      skipped[[length(skipped) + 1]] <- list(
        package = rec$package, manifest_path = rec$manifest_path,
        reason = "manifest is not valid JSON")
      next
    }
    problems <- .mcp_validate_tools_manifest(manifest)
    if (length(problems)) {
      skipped[[length(skipped) + 1]] <- list(
        package = manifest$package %||% rec$package,
        manifest_path = rec$manifest_path,
        reason = paste(problems, collapse = "; "))
      next
    }
    if (!.mcp_tools_schema_compatible(manifest$schema_version)) {
      skipped[[length(skipped) + 1]] <- list(
        package = manifest$package, manifest_path = rec$manifest_path,
        reason = sprintf("incompatible schema_version %s (host supports %s)",
                         manifest$schema_version, mcp_tools_schema_version()))
      next
    }
    # The same package can be discovered twice (installed + dev_root); keep one.
    if (manifest$package %in% seen_pkg) next
    seen_pkg <- c(seen_pkg, manifest$package)
    mode <- if (nzchar(manifest$builder %||% "")) "builder" else "declarative"
    providers[[length(providers) + 1]] <- list(
      package = manifest$package,
      manifest = manifest,
      mode = mode,
      tool_count = if (mode == "builder") NA_integer_ else length(manifest$tools),
      manifest_path = rec$manifest_path,
      source = rec$source %||% "installed")
  }
  list(providers = providers, skipped = skipped)
}

# Map a declarative argument type string to an ellmer type object.
.mcp_arg_type <- function(spec) {
  required <- isTRUE(spec$required)
  desc <- spec$description %||% ""
  switch(spec$type,
    string = ellmer::type_string(desc, required = required),
    integer = ellmer::type_integer(desc, required = required),
    number = ellmer::type_number(desc, required = required),
    boolean = ellmer::type_boolean(desc, required = required),
    array = ellmer::type_array(
      items = switch(spec$items %||% "string",
        integer = ellmer::type_integer(),
        number = ellmer::type_number(),
        boolean = ellmer::type_boolean(),
        ellmer::type_string()),
      description = desc, required = required),
    stop(sprintf("unsupported argument type '%s'", spec$type %||% "<NA>"))
  )
}

# Decide which groups to pass to a builder for a requested profile group set.
# Returns: list(mode = "all") to call the builder with no groups (no filtering
# possible or none requested), list(mode = "groups", groups = <chr>) to call
# builder(groups = ...), or list(mode = "none") to exclude the provider (it
# offers groups but none intersect the request).
.mcp_builder_call_groups <- function(builder, requested) {
  if (is.null(requested)) return(list(mode = "all"))
  if (!("groups" %in% names(formals(builder)))) return(list(mode = "all"))
  offered <- tryCatch(eval(formals(builder)$groups), error = function(e) NULL)
  use <- if (is.null(offered)) requested else intersect(requested, offered)
  if (length(use) == 0) list(mode = "none") else list(mode = "groups", groups = use)
}

# Materialize one provider's tools as a list of ellmer ToolDefs. `provider_groups`
# (a profile's group allowlist, or NULL for all) filters declarative tools by
# their `group` and is passed to builders that accept a `groups` argument. Errors
# here are caught by the caller and turn into a skipped-provider record.
.mcp_build_provider_tools <- function(provider, provider_groups = NULL) {
  pkg <- provider$package
  if (provider$mode == "builder") {
    builder <- getExportedValue(pkg, provider$manifest$builder)
    call_spec <- .mcp_builder_call_groups(builder, provider_groups)
    tools <- switch(call_spec$mode,
      all = builder(),
      groups = do.call(builder, list(groups = call_spec$groups)),
      none = list()
    )
    if (!is.list(tools)) {
      stop("builder did not return a list of tools")
    }
    return(tools)
  }
  ts <- provider$manifest$tools
  if (!is.null(provider_groups)) {
    # Declarative tools are kept only when their declared group is requested;
    # ungrouped tools appear only in the unfiltered (full) profile.
    ts <- Filter(function(t) !is.null(t$group) && t$group %in% provider_groups, ts)
  }
  lapply(ts, function(t) {
    fun <- getExportedValue(pkg, t$handler)
    args <- list()
    for (a in t$arguments %||% list()) {
      args[[a$name]] <- .mcp_arg_type(a)
    }
    .ctool(fun, t$name, t$description %||% t$name, args)
  })
}

#' Aggregate tools from all discovered tool providers
#'
#' @param dev_roots Optional dev source-tree roots.
#' @param providers Optional character vector of provider package names to
#'   include (default: all discovered).
#' @param provider_groups Optional launch-profile group allowlist (default
#'   `NULL` = all): filters declarative tools by their `group` and is passed to
#'   builders that accept a `groups` argument.
#' @return A list with `tools` (flat list of ellmer ToolDefs) and `skipped`
#'   (records with a reason, including providers that failed to build).
#' @keywords internal
#' @export
.mcp_provider_tools <- function(dev_roots = character(0), providers = NULL,
                                provider_groups = NULL) {
  disc <- .mcp_discover_tool_providers(dev_roots)
  tools <- list()
  skipped <- disc$skipped
  for (p in disc$providers) {
    if (!is.null(providers) && !(p$package %in% providers)) next
    built <- tryCatch(.mcp_build_provider_tools(p, provider_groups), error = function(e) e)
    if (inherits(built, "error")) {
      skipped[[length(skipped) + 1]] <- list(
        package = p$package, manifest_path = p$manifest_path,
        reason = paste("tool build failed:", conditionMessage(built)))
      next
    }
    tools <- c(tools, built)
  }
  list(tools = tools, skipped = skipped)
}

# Names of tools a provider marks gated: true (declarative manifests only).
# Surfacing only - the host uses these to set client approval prompts; the
# gate itself is enforced inside the provider handler.
.mcp_gated_tool_names <- function(dev_roots = character(0)) {
  disc <- .mcp_discover_tool_providers(dev_roots)
  out <- character(0)
  for (p in disc$providers) {
    if (p$mode != "declarative") next
    for (t in p$manifest$tools) {
      if (isTRUE(t$gated)) out <- c(out, t$name)
    }
  }
  unique(out)
}
