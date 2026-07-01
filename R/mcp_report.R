# Report-ready Rmd accumulator: host-owned structured sections that plot
# providers register figures into. Mirrors mcp_repro.R (session env, flush on
# write, provider-agnostic SDK).

.mcp_report_state <- new.env(parent = emptyenv())

# Canonical section order (single source of truth for providers).
.mcp_report_section_order <- c(
  "overview",
  "methods",
  "results.parameters",
  "diagnostics.gof",
  "diagnostics.covariates",
  "diagnostics.vpc",
  "appendix"
)

.mcp_report_section_titles <- c(
  overview = "Overview",
  methods = "Methods",
  "results.parameters" = "Parameter estimates",
  "diagnostics.gof" = "Goodness-of-fit diagnostics",
  "diagnostics.covariates" = "Covariate diagnostics",
  "diagnostics.vpc" = "Visual predictive checks",
  appendix = "Appendix"
)

.mcp_report_ensure <- function() {
  if (is.null(.mcp_report_state$sections)) mcp_report_reset()
  invisible(NULL)
}

#' Report Rmd accumulator
#'
#' Host-owned accumulator for a modeling report `.Rmd` per MCP server session.
#' Plot and table tools register content into structured sections; the document
#' is rendered in a fixed section order regardless of tool-call order.
#'
#' @keywords internal
#' @name mcp_report
NULL

#' @rdname mcp_report
#' @export
mcp_report_reset <- function() {
  .mcp_report_state$path <- NULL
  .mcp_report_state$title <- NULL
  .mcp_report_state$initialized <- FALSE
  .mcp_report_state$sections <- list()
  invisible(NULL)
}

#' @rdname mcp_report
#' @param path For `mcp_report_path()`, an optional new report path (resets
#'   contents); for `mcp_report_figure()`, the absolute or relative path to
#'   the saved figure PNG.
#' @export
mcp_report_path <- function(path = NULL) {
  .mcp_report_ensure()
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
      stop("`path` must be a single non-empty string.", call. = FALSE)
    }
    .mcp_report_state$path <- path
    .mcp_report_state$initialized <- FALSE
    .mcp_report_state$sections <- list()
  } else if (is.null(.mcp_report_state$path)) {
    reports_dir <- mcp_session_reports_dir()
    if (!is.null(reports_dir)) {
      .mcp_report_state$path <- file.path(reports_dir, "modeling_report.Rmd")
    } else {
      .mcp_report_state$path <- file.path(tempdir(), "certara_mcp_report.Rmd")
    }
  }
  .mcp_report_state$path
}

.mcp_report_auto_init <- function() {
  if (isTRUE(.mcp_report_state$initialized)) return(invisible(NULL))
  title <- .mcp_report_state$title %||% "Modeling report"
  mcp_report_init(title)
  invisible(NULL)
}

#' @rdname mcp_report
#' @param title For `mcp_report_init()`, the report title for the YAML
#'   header; for `mcp_report_section()`, the section heading text.
#' @param project_dir Ignored when session project root is set; kept for API
#'   compatibility.
#' @param stem Filename stem (default `modeling`).
#' @export
mcp_report_init <- function(title, project_dir = NULL, stem = "modeling") {
  .mcp_report_ensure()
  if (!is.null(project_dir) && nzchar(project_dir) &&
      is.null(mcp_session_project_dir())) {
    mcp_session_project_dir(project_dir)
  }
  path <- if (!is.null(mcp_session_reports_dir())) {
    file.path(mcp_session_reports_dir(), paste0(stem, "_report.Rmd"))
  } else {
    file.path(tempdir(), paste0(stem, "_report.Rmd"))
  }
  .mcp_report_state$path <- path
  .mcp_report_state$title <- title
  .mcp_report_state$initialized <- TRUE
  .mcp_report_state$sections <- list()
  for (id in .mcp_report_section_order) {
    .mcp_report_state$sections[[id]] <- list(
      title = .mcp_report_section_titles[[id]],
      level = if (id == "overview") 1L else 2L,
      items = list()
    )
  }
  .mcp_report_flush()
  invisible(path)
}

.mcp_report_section_ensure <- function(section) {
  .mcp_report_auto_init()
  if (is.null(.mcp_report_state$sections[[section]])) {
    .mcp_report_state$sections[[section]] <- list(
      title = section,
      level = 2L,
      items = list()
    )
  }
  invisible(NULL)
}

#' @rdname mcp_report
#' @param id Section identifier.
#' @param level Heading level (1 or 2).
#' @export
mcp_report_section <- function(id, title, level = 2L) {
  .mcp_report_auto_init()
  .mcp_report_section_ensure(id)
  .mcp_report_state$sections[[id]]$title <- title
  .mcp_report_state$sections[[id]]$level <- level
  .mcp_report_flush()
  invisible(mcp_report_path())
}

.mcp_report_item_key <- function(items, key) {
  if (is.null(key) || !nzchar(key)) return(NA_integer_)
  keys <- vapply(items, function(it) it$key %||% "", character(1))
  which(keys == key)
}

.mcp_report_set_item <- function(section, item) {
  .mcp_report_section_ensure(section)
  sec <- .mcp_report_state$sections[[section]]
  idx <- .mcp_report_item_key(sec$items, item$key)
  if (length(idx) == 1L && !is.na(idx)) {
    sec$items[[idx]] <- item
  } else {
    sec$items[[length(sec$items) + 1L]] <- item
  }
  .mcp_report_state$sections[[section]] <- sec
  invisible(NULL)
}

#' @rdname mcp_report
#' @param markdown Markdown text to insert.
#' @param section Target section id.
#' @param key Optional key for replace-in-place on re-call; for
#'   `mcp_report_figure()` it defaults to the figure file's stem when
#'   omitted, and for `mcp_report_text()`/`mcp_report_chunk()` omitting it
#'   always appends a new item instead of replacing one.
#' @export
mcp_report_text <- function(markdown, section, key = NULL) {
  .mcp_report_auto_init()
  item <- list(type = "text", key = key, body = markdown)
  .mcp_report_set_item(section, item)
  .mcp_report_flush()
  invisible(mcp_report_path())
}

.mcp_report_rel_path <- function(path) {
  report_dir <- normalizePath(dirname(mcp_report_path()), winslash = "/",
                              mustWork = FALSE)
  abs_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  a_parts <- strsplit(report_dir, "/", fixed = TRUE)[[1]]
  b_parts <- strsplit(abs_path, "/", fixed = TRUE)[[1]]
  is_drive <- function(part) grepl("^[A-Za-z]:$", part %||% "")
  if (is_drive(a_parts[1]) && is_drive(b_parts[1]) &&
      !identical(a_parts[1], b_parts[1])) {
    # Different Windows drive letters: no "../" sequence can cross volumes,
    # so fall back to an absolute path that knitr can still resolve.
    return(abs_path)
  }
  n <- min(length(a_parts), length(b_parts))
  i <- 1L
  while (i <= n && identical(a_parts[i], b_parts[i])) i <- i + 1L
  ups <- rep("..", max(0L, length(a_parts) - i + 1L))
  rest <- if (i <= length(b_parts)) b_parts[i:length(b_parts)] else character(0)
  rel_parts <- c(ups, rest)
  if (!length(rel_parts)) basename(path) else paste(rel_parts, collapse = "/")
}

#' @rdname mcp_report
#' @param caption Figure caption.
#' @param width Optional width in inches for include_graphics.
#' @export
mcp_report_figure <- function(path, caption, section, width = NULL,
                              key = NULL) {
  .mcp_report_auto_init()
  if (is.null(key) || !nzchar(key)) {
    key <- tools::file_path_sans_ext(basename(path))
  }
  rel <- .mcp_report_rel_path(path)
  item <- list(type = "figure", key = key, path = rel, caption = caption,
               width = width)
  .mcp_report_set_item(section, item)
  .mcp_report_flush()
  invisible(mcp_report_path())
}

#' @rdname mcp_report
#' @param code R code string for a knitr chunk (e.g. table output).
#' @export
mcp_report_chunk <- function(code, section, key = NULL) {
  .mcp_report_auto_init()
  item <- list(type = "chunk", key = key, body = code)
  .mcp_report_set_item(section, item)
  .mcp_report_flush()
  invisible(mcp_report_path())
}

.mcp_report_render_item <- function(item) {
  switch(item$type,
    text = c("", item$body, ""),
    figure = {
      label <- .mcp_report_chunk_label("figure", item$key)
      cap <- if (nzchar(item$caption %||% "")) {
        sprintf("fig.cap = '%s'", gsub("'", "\\\\'", item$caption))
      } else {
        ""
      }
      w <- if (!is.null(item$width)) {
        sprintf(", fig.width = %s", item$width)
      } else {
        ""
      }
      opts <- if (nzchar(cap) || nzchar(w)) {
        paste0("```{r ", label, ", echo=FALSE, fig.align='center'",
               if (nzchar(cap)) paste0(", ", cap) else "",
               w, "}")
      } else {
        paste0("```{r ", label, ", echo=FALSE, fig.align='center'}")
      }
      c("", opts, paste0("knitr::include_graphics('", item$path, "')"),
        "```", "")
    },
    chunk = {
      label <- .mcp_report_chunk_label("chunk", item$key %||% "chunk")
      c("", paste0("```{r ", label, ", echo=TRUE, message=FALSE, warning=FALSE}"),
        item$body, "```", "")
    },
    character(0)
  )
}

.mcp_report_chunk_label <- function(prefix, key) {
  safe <- gsub("[^A-Za-z0-9_-]+", "-", as.character(key %||% prefix))
  safe <- gsub("^-+|-+$", "", safe)
  if (!nzchar(safe)) safe <- prefix
  paste(prefix, safe, sep = "-")
}

.mcp_report_ordered_section_ids <- function() {
  known <- .mcp_report_section_order
  present <- names(.mcp_report_state$sections)
  unknown <- setdiff(present, known)
  unknown <- unknown[unknown != "appendix"]
  c(setdiff(known, "appendix"), unknown, "appendix")
}

.mcp_report_render <- function() {
  title <- .mcp_report_state$title %||% "Modeling report"
  yaml <- c(
    "---",
    paste0("title: \"", gsub("\"", "\\\\\"", title), "\""),
    "output: html_document",
    "---",
    ""
  )
  body <- character(0)
  for (id in .mcp_report_ordered_section_ids()) {
    sec <- .mcp_report_state$sections[[id]]
    if (is.null(sec)) next
    is_known <- id %in% .mcp_report_section_order
    has_items <- length(sec$items) > 0L
    show_empty <- is_known && isTRUE(.mcp_report_state$initialized)
    if (!has_items && !show_empty) next
    hashes <- paste(rep("#", sec$level %||% 2L), collapse = "")
    body <- c(body, "", paste(hashes, sec$title), "")
    for (item in sec$items) {
      body <- c(body, .mcp_report_render_item(item))
    }
  }
  c(yaml, body)
}

.mcp_report_flush <- function() {
  path <- mcp_report_path()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(.mcp_report_render(), path, useBytes = TRUE)
  invisible(path)
}

#' @rdname mcp_report
#' @export
mcp_report_read <- function() {
  .mcp_report_ensure()
  if (!isTRUE(.mcp_report_state$initialized) &&
      !length(unlist(lapply(.mcp_report_state$sections, function(s) s$items)))) {
    return("")
  }
  paste(.mcp_report_render(), collapse = "\n")
}

#' @rdname mcp_report
#' @export
mcp_report_info <- function() {
  .mcp_report_ensure()
  sections <- vapply(.mcp_report_ordered_section_ids(), function(id) {
    sec <- .mcp_report_state$sections[[id]]
    if (is.null(sec)) return(0L)
    length(sec$items)
  }, integer(1))
  names(sections) <- .mcp_report_ordered_section_ids()
  list(
    path = mcp_report_path(),
    contents = mcp_report_read(),
    section_item_counts = sections[sections > 0L]
  )
}

#' Render the accumulated report Rmd
#'
#' @param output Output format passed to `rmarkdown::render()` (default `html`).
#' @return Invisibly the rendered output path, or `NULL` when rmarkdown/pandoc
#'   is unavailable.
#' @keywords internal
#' @export
render_certara_report <- function(output = "html") {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("Package 'rmarkdown' is required to render the report.",
            call. = FALSE)
    return(invisible(NULL))
  }
  if (!rmarkdown::pandoc_available()) {
    warning("pandoc is not available; cannot render the report.", call. = FALSE)
    return(invisible(NULL))
  }
  path <- mcp_report_path()
  if (!file.exists(path) || !nzchar(mcp_report_read())) {
    warning("No report content to render.", call. = FALSE)
    return(invisible(NULL))
  }
  output_format <- if (identical(output, "html")) "html_document" else output
  out <- rmarkdown::render(path, output_format = output_format, quiet = TRUE)
  invisible(out)
}
