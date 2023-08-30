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

# 2: Define any helper functions used in the code
save_files <- function(x, name, type = "table") {
  file_rds <- paste0(name, ".rds")
  file_png <- paste0(name, ".png")

  if (type == "table") {
    flextable::save_as_image(x, path = file.path("./tables", file_png))
    saveRDS(x, file = file.path("./tables", file_rds))
  } else if (type == "plot") {
    ggplot2::ggsave(filename = file.path("./plots", file_png),
                    plot = x)
    saveRDS(x, file = file.path("./plots", file_rds))
  }
}

# 3: Source Analysis Scripts ----
source("./scripts/dataset_preparation_01.R")
source("./scripts/exploratory_data_analysis_02.R")
source("./scripts/modeling_03.R")
source("./scripts/model_diagnostics_04.R")

# 4: Generate Report ----
generate_report <- TRUE # Set to TRUE to render report output

if (generate_report) {
  rmarkdown::render("./ModelSummary.Rmd", output_format = "html_document", params = list(interactive = TRUE))
}

