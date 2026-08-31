# Generate the committed KB artifacts for a package source tree

Reads `inst/mcp/kb/sources/*.md`, validates entries, and writes
`inst/mcp/kb/<package>.jsonl`, `inst/mcp/kb/index/symbols.json` +
`enums.json`, and `inst/mcp/kb/manifest.json`. Re-running on unchanged
sources reproduces byte-identical JSONL and `index/*.json` artifacts;
`manifest.json` also carries a generation timestamp (`generated`).

## Usage

``` r
generate_certara_kb(
  pkg_root,
  package,
  package_version = NULL,
  engine = NULL,
  generator_version = "1.0.0",
  quiet = FALSE
)
```

## Arguments

- pkg_root:

  Path to the package root (containing `inst/mcp/kb`).

- package:

  Package name to stamp on entries/manifest.

- package_version:

  Optional package version string.

- engine:

  Optional named list with `tdl5_version`, `grammar_source`,
  `audit_date` for the manifest's `engine` block.

- generator_version:

  Version string recorded in the manifest.

- quiet:

  Suppress the summary message.

## Value

Invisibly, the list of generated entries.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_certara_kb(pkg_root = ".", package = "Certara.RsNLME")
} # }
```
