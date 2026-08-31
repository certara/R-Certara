# Certara R Packages

## R Packages for Pharmacometrics

`Certara.R` provides a collection of R packages and Shiny applications
designed for Pharmacometric workflows in R. All Shiny applications
provide the ability to generate R code given point-and-click operations,
enabling a reproducible and extensible workflow from Shiny GUI to
RStudio.

### RsNLME ![RsNLME package logo.](img/RsNLME.png)

[`Certara.RsNLME`](https://certara.github.io/R-RsNLME/index.html) uses
[tidyverse](https://www.tidyverse.org/) syntax to build
Non-Linear-Mixed-Effects (NLME) models in R. Create and execute NLME
models using built-in R functions.

------------------------------------------------------------------------

  
  
  
  

### RsNLME.ModelBuilder ![RsNLME.ModelBuilder package logo.](img/ModelBuilder.png)

[`Certara.RsNLME.ModelBuilder`](https://certara.github.io/R-RsNLME-model-builder/index.html)
is an R package and Shiny application used to build an RsNLME model.

Use the GUI to select from various model building options and observe
the PML update in real time. Additionally, users may generate the
corresponding RsNLME code to learn reproduce the model object from R.

------------------------------------------------------------------------

  
  
  
  

### RsNLME.ModelExecutor ![RsNLME.ModelExecutor package logo.](img/ModelExecutor.png)

[`Certara.RsNLME.ModelExecutor`](https://certara.github.io/R-RsNLME-model-executor/index.html)
is an R package and Shiny application used to execute an RsNLME model.

Use the GUI to add additional output tables, specify engine parameters,
select various run types, and more!

------------------------------------------------------------------------

  
  
  
  

### ModelResults ![ModelResults package logo.](img/ModelResults.png)

[`Certara.ModelResults`](https://certara.github.io/R-model-results/index.html)
is an R package and Shiny GUI used to generate, customize, and report
model diagnostic plots and tables from NLME or NONMEM runs.

Users are not limited by the GUI however, Certara.ModelResults will
generate the underlying `flextable` and `xpose`/`ggplot2` code (`.R`
and/or `.Rmd`) for you inside the Shiny application, which you can then
use to recreate your plot and table objects in R, ensuring
reproducibility and trace-ability of model diagnostics for reporting
output.

------------------------------------------------------------------------

  
  
  
  

### XposeNLME ![XposeNLME package logo.](img/XposeNLME.png)

[`Certara.Xpose.NLME`](https://certara.github.io/R-Xpose-NLME/index.html)
is an R package used to creates `xpose` databases (`xpose_data`) for
PML/NLME results. Additionally,
[`Certara.Xpose.NLME`](https://certara.github.io/R-Xpose-NLME/index.html)
offers various covariate model diagnostic functions, not available in
the `xpose` package.

------------------------------------------------------------------------

  
  
  
  

### VPCResults ![VPCResults package logo.](img/VPCResults.png)

[`Certara.VPCResults`](https://certara.github.io/R-VPCResults/index.html)
is an R package and Shiny application used to parameterize and plot a
Visual Predictive Check (VPC).

Use the GUI to select from various binning or binless methods and
specify options such as censoring, stratification, and
prediction-corrected.

Users are not limited by the GUI however,
[`Certara.VPCResults`](https://certara.github.io/R-VPCResults/index.html)
will generate the underlying `tidyvpc` and `ggplot2` code (`.R` and/or
`.Rmd`) for you inside the Shiny application, which you can then use to
recreate your plot and table objects in R, ensuring reproducibility of
VPC’s for reporting output.

------------------------------------------------------------------------

  
  
  
  

### tidyvpc ![tidyvpc package logo.](img/tidyvpc.png)

The [`tidyvpc`](https://certara.github.io/tidyvpc/index.html) package is
used to perform a Visual Predictive Check (VPC), while accounting for
stratification, censoring, and prediction correction.

Using piping from ‘magrittr’, the intuitive syntax gives users a
flexible and powerful method to generate VPCs using both traditional
binning and a new binless approach Jamsen et al. (2018)
[doi:10.1002/psp4.12319](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6202468/)
with Additive Quantile Regression (AQR) and Locally Estimated
Scatterplot Smoothing (LOESS) prediction correction.

------------------------------------------------------------------------
