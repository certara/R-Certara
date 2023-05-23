# 1: Import Required Libraries ----
library(haven) #useful for import of SAS files in data prep
library(dplyr) #popular package for wrangling data in R
library(tidyr) #useful data manipulation and reshaping functions that work with dplyr
library(lubridate) #useful for working with date-times
library(gtsummary) #useful for quick data summary tables
library(flextable) # useful table package supporting critical file output types
library(ggplot2) #useful generic plotting package
library(Certara.RsNLME) #core modeling package
library(Certara.Xpose.NLME) #used to initialize xpose_data from NLME run
library(xpose) #model diagnostics package for gof plots

# 2: Source Analysis Scripts ----
source("./scripts/dataset_preparation_01.R")
source("./scripts/exploratory_data_analysis_02.R")
source("./scripts/modeling_03.R")
source("./scripts/model_diagnostics_04.R")

# 3: Generate Report ----
generate_report <- TRUE # Set to TRUE to render report output

if (generate_report) {
  rmarkdown::render("./report.Rmd")
}

