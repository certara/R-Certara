# Report Rmd accumulator

Host-owned accumulator for a modeling report `.Rmd` per MCP server
session. Plot and table tools register content into structured sections;
the document is rendered in a fixed section order regardless of
tool-call order.

## Usage

``` r
mcp_report_reset()

mcp_report_path(path = NULL)

mcp_report_init(title, project_dir = NULL, stem = "modeling")

mcp_report_section(id, title, level = 2L)

mcp_report_text(markdown, section, key = NULL)

mcp_report_figure(path, caption, section, width = NULL, key = NULL)

mcp_report_chunk(code, section, key = NULL)

mcp_report_read()

mcp_report_info()
```

## Arguments

- path:

  For `mcp_report_path()`, an optional new report path (resets
  contents); for `mcp_report_figure()`, the absolute or relative path to
  the saved figure PNG.

- title:

  For `mcp_report_init()`, the report title for the YAML header; for
  `mcp_report_section()`, the section heading text.

- project_dir:

  Ignored when session project root is set; kept for API compatibility.

- stem:

  Filename stem (default `modeling`).

- id:

  Section identifier.

- level:

  Heading level (1 or 2).

- markdown:

  Markdown text to insert.

- section:

  Target section id.

- key:

  Optional key for replace-in-place on re-call; for
  `mcp_report_figure()` it defaults to the figure file's stem when
  omitted, and for `mcp_report_text()`/`mcp_report_chunk()` omitting it
  always appends a new item instead of replacing one.

- caption:

  Figure caption.

- width:

  Optional width in inches for include_graphics.

- code:

  R code string for a knitr chunk (e.g. table output).
