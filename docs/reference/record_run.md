# Record a quantitative run fingerprint

Record a quantitative run fingerprint

## Usage

``` r
record_run(summary, scope = "global", provenance = NULL)
```

## Arguments

- summary:

  Short free-text summary of the run (model, outcome, key metrics).

- scope:

  `"global"` or a context tag (model type / task / data shape).

- provenance:

  Optional list (e.g. job_id, package, tool).

## Value

A list with the stored `id`.

## Examples

``` r
if (FALSE) { # \dontrun{
enable_memory()
record_run("FOCE-ELS fit converged; OFV 1234.5")
} # }
```
