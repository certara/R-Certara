# Set a default user preference

Append-only with supersede: setting a key marks prior records for that
key (same scope) as superseded rather than overwriting them.

## Usage

``` r
set_preference(key, value, scope = "global", level = c("hard", "soft"))
```

## Arguments

- key:

  Preference key (e.g. "error_model", "plot_style").

- value:

  Preference value.

- scope:

  "global" or a context tag.

- level:

  "hard" (always apply) or "soft" (consider).

## Value

A list with the stored `id`.

## Examples

``` r
if (FALSE) { # \dontrun{
enable_memory()
set_preference("error_model", "additive")
} # }
```
