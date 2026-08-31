# Deactivate a lesson without deleting it

Marks a lesson inactive so
[`get_lessons()`](https://github.com/certara/R-Certara/reference/get_lessons.md)
omits it unless `include_superseded = TRUE`. History is preserved
(unlike
[`delete_memory_record()`](https://github.com/certara/R-Certara/reference/delete_memory_record.md)).

## Usage

``` r
deactivate_lesson(id)
```

## Arguments

- id:

  Lesson record id returned by
  [`record_lesson()`](https://github.com/certara/R-Certara/reference/record_lesson.md).

## Value

A list with `deactivated` (logical) and `id`.

## Examples

``` r
if (FALSE) { # \dontrun{
enable_memory()
id <- record_lesson("obsolete note")$id
deactivate_lesson(id)
} # }
```
