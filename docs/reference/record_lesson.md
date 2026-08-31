# Record a corrective lesson or endorsed best practice

Record a corrective lesson or endorsed best practice

## Usage

``` r
record_lesson(
  lesson,
  category = c("corrective", "best_practice"),
  trigger = NULL,
  scope = "global",
  level = NULL,
  provenance = NULL
)
```

## Arguments

- lesson:

  What went wrong / the corrected approach (free text).

- category:

  One of "corrective", "best_practice".

- trigger:

  For corrective lessons: "self_detected" or "user_feedback".

- scope:

  "global" or a context tag (model type / task / data shape).

- level:

  "hard" or "soft" (corrective defaults to hard).

- provenance:

  Optional list (e.g. job_id, timestamp).

## Value

A list with the stored `id`.

## Examples

``` r
if (FALSE) { # \dontrun{
enable_memory()
record_lesson("prefer FOCE-ELS for rich data", category = "best_practice")
} # }
```
