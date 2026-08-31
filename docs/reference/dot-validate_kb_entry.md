# Validate a single KB entry

Validate a single KB entry

## Usage

``` r
.validate_kb_entry(entry, known_ids = NULL)
```

## Arguments

- entry:

  A named list (one parsed JSONL record).

- known_ids:

  Optional character vector of all entry ids, used to detect dangling
  `related` cross-references.

## Value

A character vector of problems; empty when the entry is valid.
