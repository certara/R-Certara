# Tool-provider discovery. Mirrors the KB federation in mcp_kb_index.R: scan
# installed packages (and any dev_roots) for inst/mcp/tools/manifest.json, then
# materialize each provider's tools as ellmer ToolDefs.
#
# Only builder manifests are supported: the manifest names an exported function
# that returns ellmer tools (Certara.RsNLME, Certara.RDarwin, tidyvpc,
# Certara.Xpose.NLME). A broken or schema-incompatible manifest is skipped and
# reported, never fatal.
#
# A manifest may also declare an optional `status_hook`: the name of an
# exported `function(project_dir)` returning that provider's own project
# status. It is unrelated to `builder` and is validated only when present (see
# .mcp_call_status_hook() in mcp_project_status.R).

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
  if (!.is_scalar_string(manifest$builder)) {
    problems <- c(problems, "manifest must set a scalar 'builder' export name")
  }
  # Reject the key whenever present (including empty arrays): leftover
  # declarative fields must fail loudly under the builder-only contract.
  if (!is.null(manifest$tools)) {
    problems <- c(problems, paste(
      "manifest sets a 'tools' array; declarative tool manifests are no longer",
      "supported - expose tools via 'builder' only"))
  }
  problems
}

#' Discover installed-package tool manifests
#'
#' @param dev_roots Optional package source-tree roots to include.
#' @return A list with `providers` (loadable records) and `skipped` (with a
#'   reason). Each provider record carries `package`, `manifest`, `mode`
#'   (`"builder"`), `tool_count`, and `manifest_path`.
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
    providers[[length(providers) + 1]] <- list(
      package = manifest$package,
      manifest = manifest,
      mode = "builder",
      tool_count = NA_integer_,
      manifest_path = rec$manifest_path,
      source = rec$source %||% "installed")
  }
  list(providers = providers, skipped = skipped)
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
# (a profile's group allowlist, or NULL for all) is passed to builders that accept
# a `groups` argument. Errors here are caught by the caller and turn into a
# skipped-provider record.
.mcp_build_provider_tools <- function(provider, provider_groups = NULL) {
  pkg <- provider$package
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
  tools
}

# Resolve one provider's group allowlist from a profile's `provider_groups`.
# Three shapes, kept distinct so existing callers are unaffected:
#   - NULL: no filtering
#   - a plain character vector: the same allowlist for every provider
#   - a named list: per-package allowlists with optional `"*"` fallback
.mcp_resolve_provider_group_request <- function(provider_groups, package) {
  if (is.null(provider_groups) || !is.list(provider_groups)) {
    return(provider_groups)
  }
  if (package %in% names(provider_groups)) {
    return(provider_groups[[package]])
  }
  provider_groups[["*"]] %||% NULL
}

#' Aggregate tools from all discovered tool providers
#'
#' @param dev_roots Optional dev source-tree roots.
#' @param providers Optional character vector of provider package names to
#'   include (default: all discovered).
#' @param provider_groups Optional launch-profile group allowlist (default
#'   `NULL` = all). Either a plain character vector applied to every provider,
#'   or a named list of per-package allowlists with an optional `"*"` fallback.
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
    pkg_groups <- .mcp_resolve_provider_group_request(provider_groups, p$package)
    built <- tryCatch(.mcp_build_provider_tools(p, pkg_groups), error = function(e) e)
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

# Names of tools a provider marks gated. Builder manifests carry no per-tool
# metadata (the builder function alone decides what it returns), so there is
# currently no source of gated tool names; this always returns character(0).
# Kept as a function (rather than inlining character(0) at call sites) so a
# future gating source can be wired in here without touching callers in
# mcp_capabilities.R / mcp_config.R.
.mcp_gated_tool_names <- function(dev_roots = character(0)) {
  character(0)
}
