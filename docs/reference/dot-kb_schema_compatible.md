# Schema-version compatibility rule

A provider KB is loadable when it shares the server's MAJOR version and
its MINOR version is not newer than the server's. PATCH differences are
always compatible. This lets older providers keep working after additive
(MINOR) schema growth while refusing forward-incompatible content.

## Usage

``` r
.kb_schema_compatible(provided, supported = kb_schema_version())
```

## Arguments

- provided:

  Character schema version from a provider manifest.

- supported:

  Character schema version supported by this build.

## Value

`TRUE` when the provided schema can be loaded, otherwise `FALSE`.
