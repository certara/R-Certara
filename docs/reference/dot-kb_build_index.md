# Build (and cache) the KB search index for this process

Build (and cache) the KB search index for this process

## Usage

``` r
.kb_build_index(dev_roots = character(0), refresh = FALSE)
```

## Arguments

- dev_roots:

  Optional dev source-tree roots (passed to
  [`.kb_discover()`](https://github.com/certara/R-Certara/reference/dot-kb_discover.md)).

- refresh:

  Force a rebuild even if cached.

## Value

The cached index list.
