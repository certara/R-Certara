# Get context-relevant lessons

Get context-relevant lessons

## Usage

``` r
get_lessons(context = NULL, include_superseded = FALSE)
```

## Arguments

- context:

  Optional scope tag; returns global plus matching lessons.

- include_superseded:

  Include inactive lessons (default `FALSE`).

## Value

A list of lesson records (corrective first, then best practices).

## Examples

``` r
if (FALSE) { # \dontrun{
get_lessons()
} # }
```
