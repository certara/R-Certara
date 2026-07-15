# Read-only inventory of Certara MCP entries across supported MCP client
# config files. Complements write_mcp_config()/remove_mcp_config() (which
# mutate) by answering "what is configured to launch?" without touching disk.
# Missing or malformed files never abort the scan - each client/scope target
# gets its own status so one broken config does not hide the rest.

# ---- soft readers (never throw) --------------------------------------------

# Unlike .read_json_or_stop() (used by the writer, which must refuse to
# proceed on corrupt input), a listing needs to keep going and report the
# problem per-target instead.
.mcp_read_json_soft <- function(path) {
  if (!file.exists(path)) {
    return(list(exists = FALSE, servers = list(), parse_error = NA_character_))
  }
  parsed <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                     error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(exists = TRUE, servers = list(),
                parse_error = conditionMessage(parsed)))
  }
  list(exists = TRUE, servers = parsed$mcpServers %||% list(),
       parse_error = NA_character_)
}

# Extract every double-quoted TOML string token from a line fragment,
# unescaping \" and \\. Used for both `command = "..."` and
# `args = ["-e", "..."]` - the same escaping rules apply to both
# (.toml_string_array() in mcp_config.R writes them identically).
.mcp_toml_string_tokens <- function(text) {
  m <- gregexpr('"((?:[^"\\\\]|\\\\.)*)"', text, perl = TRUE)
  matches <- regmatches(text, m)[[1]]
  if (!length(matches)) {
    return(character(0))
  }
  inner <- substring(matches, 2, nchar(matches) - 1)
  inner <- gsub('\\\\"', '"', inner)
  gsub("\\\\\\\\", "\\\\", inner)
}

# Line-based extraction of [mcp_servers.<name>] tables from a Codex
# config.toml, mirroring the no-TOML-library approach the writer uses
# (.strip_codex_mcp_tables()). Handles the single-line `command`/`args` form
# that .write_codex_mcp_config() always produces; a hand-written multi-line
# array is reported with empty args rather than mis-parsed.
.mcp_read_codex_servers <- function(path) {
  if (!file.exists(path)) {
    return(list(exists = FALSE, servers = list(), parse_error = NA_character_))
  }
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) e)
  if (inherits(lines, "error")) {
    return(list(exists = TRUE, servers = list(),
                parse_error = conditionMessage(lines)))
  }
  header_re <- "^\\s*\\[\\s*mcp_servers\\.([A-Za-z0-9_.-]+)\\s*\\]\\s*$"
  any_header_re <- "^\\s*\\["
  servers <- list()
  current <- NULL
  cur_lines <- character(0)
  flush <- function() {
    if (!is.null(current)) {
      servers[[current]] <<- .mcp_parse_codex_table(cur_lines)
    }
  }
  for (line in lines) {
    m <- regmatches(line, regexec(header_re, line))[[1]]
    if (length(m) >= 2 && !is.na(m[1])) {
      flush()
      current <- m[2]
      cur_lines <- character(0)
      next
    }
    if (grepl(any_header_re, line)) {
      # A different table (a nested mcp_servers.<name>.tools.<t> approval
      # table, or an unrelated section) ends the current one.
      flush()
      current <- NULL
      cur_lines <- character(0)
      next
    }
    if (!is.null(current)) {
      cur_lines <- c(cur_lines, line)
    }
  }
  flush()
  list(exists = TRUE, servers = servers, parse_error = NA_character_)
}

.mcp_parse_codex_table <- function(lines) {
  get_field_line <- function(key) {
    hit <- grep(sprintf("^\\s*%s\\s*=", key), lines, value = TRUE)
    if (!length(hit)) NA_character_ else hit[1]
  }
  command_line <- get_field_line("command")
  command <- if (!is.na(command_line)) {
    toks <- .mcp_toml_string_tokens(command_line)
    if (length(toks)) toks[1] else NA_character_
  } else {
    NA_character_
  }
  args_line <- get_field_line("args")
  args <- if (!is.na(args_line)) as.list(.mcp_toml_string_tokens(args_line)) else list()
  list(command = command, args = args)
}

# ---- launch-expression parsing (inverse of .mcp_launch_expr()) ------------

.mcp_regex_capture <- function(text, pattern) {
  m <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(m) < 2 || is.na(m[1])) NA_character_ else m[2]
}

# Recover the launch options .mcp_launch_expr() baked into the -e string, so a
# config entry can be compared against a running server's actual options
# (list_certara_mcp_servers()) without invoking any client CLI.
.mcp_parse_launch_expr <- function(expr) {
  empty <- list(server_name = NA_character_, btw_groups = NA_character_,
                session_tools = NA, job_watch_wait_seconds = NA_real_,
                tool_profile = NA_character_)
  if (is.null(expr) || !.is_scalar_string(expr)) {
    return(empty)
  }
  groups_raw <- .mcp_regex_capture(
    expr, "btw_groups\\s*=\\s*(character\\(0\\)|c\\([^)]*\\))"
  )
  btw_groups <- if (is.na(groups_raw) || identical(groups_raw, "character(0)")) {
    character(0)
  } else {
    inside <- sub("^c\\((.*)\\)$", "\\1", groups_raw)
    trimws(gsub("'", "", strsplit(inside, ",")[[1]]))
  }
  session_raw <- .mcp_regex_capture(expr, "session_tools\\s*=\\s*(TRUE|FALSE)")
  list(
    server_name = .mcp_regex_capture(expr, "server_name\\s*=\\s*'([^']*)'"),
    btw_groups = if (length(btw_groups)) paste(btw_groups, collapse = ", ") else NA_character_,
    session_tools = if (is.na(session_raw)) NA else identical(session_raw, "TRUE"),
    job_watch_wait_seconds = suppressWarnings(as.numeric(
      .mcp_regex_capture(expr, "job_watch_wait_seconds\\s*=\\s*([0-9.]+)")
    )),
    tool_profile = .mcp_regex_capture(expr, "tool_profile\\s*=\\s*'([^']*)'")
  )
}

# ---- finding Certara entries among a client's configured servers ----------

# `servers` is a named list (one element per configured server key), each with
# $command (scalar) and $args (list of scalars) - the shape both
# .mcp_read_json_soft() and .mcp_read_codex_servers() produce. When
# `server_name` is given, only that key is considered (present or not);
# otherwise every entry is scanned for a launch_certara_mcp() call, so a
# renamed or additional Certara server is still discovered.
.mcp_config_find_certara_entries <- function(servers, server_name = NULL) {
  if (!length(servers)) {
    return(list())
  }
  if (!is.null(server_name)) {
    if (is.null(servers[[server_name]])) {
      return(list())
    }
    return(servers[server_name])
  }
  is_launcher <- function(block) {
    args <- unlist(block$args, use.names = FALSE)
    any(grepl("launch_certara_mcp\\(", args))
  }
  keys <- names(servers)
  hit <- keys[vapply(keys, function(k) is_launcher(servers[[k]]), logical(1))]
  servers[hit]
}

# ---- row assembly -----------------------------------------------------------

.mcp_config_row <- function(client, scope, path = NA_character_, exists = NA,
                            configured = FALSE, server_key = NA_character_,
                            status = "not_configured", command = NA_character_,
                            server_name = NA_character_,
                            btw_groups = NA_character_, session_tools = NA,
                            job_watch_wait_seconds = NA_real_,
                            tool_profile = NA_character_, note = NA_character_) {
  data.frame(
    client = client, scope = scope, path = path, exists = exists,
    configured = configured, server_key = server_key, status = status,
    command = command, server_name = server_name, btw_groups = btw_groups,
    session_tools = session_tools,
    job_watch_wait_seconds = job_watch_wait_seconds,
    tool_profile = tool_profile, note = note,
    stringsAsFactors = FALSE
  )
}

.mcp_config_unsupported_row <- function(client, scope, note) {
  .mcp_config_row(client, scope, status = "unsupported", note = note)
}

.mcp_config_row_from_block <- function(client, scope, path, key, block) {
  args <- unlist(block$args, use.names = FALSE)
  launch_hit <- args[grepl("launch_certara_mcp\\(", args)]
  launch_expr <- if (length(launch_hit)) launch_hit[1] else NA_character_
  opts <- .mcp_parse_launch_expr(launch_expr)
  note <- if (is.na(launch_expr)) {
    paste("Entry present but its args do not contain a",
          "Certara.R::launch_certara_mcp() call.")
  } else {
    NA_character_
  }
  .mcp_config_row(
    client, scope, path = path, exists = TRUE, configured = TRUE,
    server_key = key, status = "configured",
    command = block$command %||% NA_character_,
    server_name = opts$server_name, btw_groups = opts$btw_groups,
    session_tools = opts$session_tools,
    job_watch_wait_seconds = opts$job_watch_wait_seconds,
    tool_profile = opts$tool_profile, note = note
  )
}

# Scan one JSON config file (Cursor, Claude Code project, Claude Desktop) and
# return a list of rows - possibly more than one if multiple entries invoke
# launch_certara_mcp() (e.g. the server was renamed at some point and the old
# entry was never removed), or none-but-one when the file is missing, corrupt,
# or has no matching entry.
.mcp_config_scan_json <- function(client, scope, path, server_name) {
  soft <- .mcp_read_json_soft(path)
  .mcp_config_scan_servers(client, scope, path, soft, server_name)
}

.mcp_config_scan_codex <- function(client, scope, path, server_name) {
  soft <- .mcp_read_codex_servers(path)
  .mcp_config_scan_servers(client, scope, path, soft, server_name)
}

.mcp_config_scan_servers <- function(client, scope, path, soft, server_name) {
  if (!soft$exists) {
    return(list(.mcp_config_row(client, scope, path = path, exists = FALSE,
                                status = "not_configured")))
  }
  if (!is.na(soft$parse_error)) {
    return(list(.mcp_config_row(client, scope, path = path, exists = TRUE,
                                status = "parse_error", note = soft$parse_error)))
  }
  hits <- .mcp_config_find_certara_entries(soft$servers, server_name)
  if (!length(hits)) {
    return(list(.mcp_config_row(client, scope, path = path, exists = TRUE,
                                status = "not_configured")))
  }
  lapply(names(hits), function(key) {
    .mcp_config_row_from_block(client, scope, path, key, hits[[key]])
  })
}

.mcp_config_empty_frame <- function() {
  data.frame(
    client = character(0), scope = character(0), path = character(0),
    exists = logical(0), configured = logical(0), server_key = character(0),
    status = character(0), command = character(0), server_name = character(0),
    btw_groups = character(0), session_tools = logical(0),
    job_watch_wait_seconds = numeric(0), tool_profile = character(0),
    note = character(0), stringsAsFactors = FALSE
  )
}

#' Inventory Certara MCP entries across client configuration files
#'
#' Scans the on-disk MCP client configuration files [write_mcp_config()]
#' writes to and reports whether a Certara MCP server is configured in each,
#' without starting or contacting any process. Complements
#' [list_certara_mcp_servers()], which reports what has actually *started*;
#' this reports what is *configured* to launch.
#'
#' A missing or unparsable config file never aborts the scan - each requested
#' client/scope combination gets its own row and `status`, so one broken file
#' does not hide the rest. Claude Code's `"user"`/`"local"` scopes are managed
#' through its CLI (`~/.claude.json`), which this function does not read or
#' write; those combinations are reported with `status = "unsupported"`, as is
#' Cursor's nonexistent `"local"` scope. Claude Desktop and Codex are always
#' user-scoped regardless of `scope`.
#'
#' @param client One or more of `"cursor"`, `"claude-code"`, `"codex"`,
#'   `"claude-desktop"`. Defaults to all four.
#' @param scope One or more of `"project"`, `"user"`, `"local"`. Defaults to
#'   all three (ignored by clients that are not scoped, i.e. Claude Desktop
#'   and Codex, which are always reported at `"user"` scope).
#' @param project_dir Project root to check for project-scope files.
#' @param server_name Optional MCP server key to look for. When `NULL`
#'   (default), every configured entry is scanned for a
#'   [launch_certara_mcp()] call instead of assuming the `"certara-r"`
#'   convention, so a server configured under a different name is still
#'   discovered. When given, only that key is considered.
#' @return A data frame with one row per client/scope combination checked (or
#'   per matching entry, if more than one server invokes
#'   [launch_certara_mcp()] in the same file): `client`, `scope`, `path`,
#'   `exists`, `configured`, `server_key`, `status` (`"configured"`,
#'   `"not_configured"`, `"parse_error"`, or `"unsupported"`), `command`,
#'   `server_name`, `btw_groups`, `session_tools`, `job_watch_wait_seconds`,
#'   `tool_profile`, and `note`.
#' @seealso [list_certara_mcp_servers()], [write_mcp_config()],
#'   [remove_mcp_config()]
#' @examples
#' list_certara_mcp_configs(client = "cursor", project_dir = tempdir())
#' @export
list_certara_mcp_configs <- function(client = c("cursor", "claude-code",
                                                "codex", "claude-desktop"),
                                     scope = c("project", "user", "local"),
                                     project_dir = ".",
                                     server_name = NULL) {
  choices_client <- c("cursor", "claude-code", "codex", "claude-desktop")
  choices_scope <- c("project", "user", "local")
  # Unlike write_mcp_config()/remove_mcp_config(), a bare call to a listing
  # function should inventory everything, not just one default target.
  if (missing(client)) {
    client <- choices_client
  } else {
    client <- match.arg(client, choices_client, several.ok = TRUE)
  }
  if (missing(scope)) {
    scope <- choices_scope
  } else {
    scope <- match.arg(scope, choices_scope, several.ok = TRUE)
  }
  if (!is.null(server_name) && !.is_scalar_string(server_name)) {
    stop("`server_name` must be a single non-empty string.", call. = FALSE)
  }

  rows <- list()

  if ("cursor" %in% client) {
    for (sc in intersect(scope, c("project", "user"))) {
      path <- .cursor_config_path(sc, project_dir)
      rows <- c(rows, .mcp_config_scan_json("cursor", sc, path, server_name))
    }
    if ("local" %in% scope) {
      rows <- c(rows, list(.mcp_config_unsupported_row(
        "cursor", "local", "Cursor has no 'local' scope."
      )))
    }
  }

  if ("claude-code" %in% client) {
    if ("project" %in% scope) {
      path <- file.path(project_dir, ".mcp.json")
      rows <- c(rows, .mcp_config_scan_json("claude-code", "project", path,
                                            server_name))
    }
    for (sc in intersect(scope, c("user", "local"))) {
      rows <- c(rows, list(.mcp_config_unsupported_row(
        "claude-code", sc,
        paste0("Claude Code '", sc, "' scope is managed via its CLI ",
               "(~/.claude.json), which this function does not read; run ",
               "`claude mcp list` to inspect it.")
      )))
    }
  }

  if ("claude-desktop" %in% client) {
    path <- .claude_desktop_config_path()
    rows <- c(rows, .mcp_config_scan_json("claude-desktop", "user", path,
                                          server_name))
  }

  if ("codex" %in% client) {
    path <- .codex_config_path()
    rows <- c(rows, .mcp_config_scan_codex("codex", "user", path, server_name))
  }

  if (!length(rows)) {
    return(.mcp_config_empty_frame())
  }
  do.call(rbind, rows)
}
