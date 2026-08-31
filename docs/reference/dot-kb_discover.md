# Discover installed-package KB manifests

Scans installed packages (and any `dev_roots` source trees) for
`inst/mcp/kb/manifest.json`. Validates manifest fields and
schema-version compatibility; incompatible/invalid manifests are
recorded but skipped.

## Usage

``` r
.kb_discover(dev_roots = character(0))
```

## Arguments

- dev_roots:

  Optional character vector of package source-tree roots to include
  (development override). Off by default.

## Value

A list with `packages` (loadable provider records) and `skipped`
(records with a reason).
