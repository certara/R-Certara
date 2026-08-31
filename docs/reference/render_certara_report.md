# Render the accumulated report Rmd

Render the accumulated report Rmd

## Usage

``` r
render_certara_report(output = "html")
```

## Arguments

- output:

  Output format passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  (default `html`).

## Value

Invisibly the rendered output path, or `NULL` when rmarkdown/pandoc is
unavailable.
