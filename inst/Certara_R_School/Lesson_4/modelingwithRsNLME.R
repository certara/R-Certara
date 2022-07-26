# Overview ----
# This is code for Certara.R School Lesson 4, Modeling with RsNLME
# In this lesson we will create and run a RsNLME model using the dataset
# we created in Lesson 2, and generate some basic model diagnostics

# 0: Load libraries

library(Certara.RsNLME) #RsNLME package
library(Certara.RsNLME.ModelBuilder) #R Shiny app for creating models
library(ggplot2) #useful plotting package
library(dplyr)
library(gt)
library(gtsummary)
library(flextable)
library(tidyr)

# 1: Import Data ----
# * 1.1 Read in xpt datasets ----
finaldat <- readRDS("finaldat.RDS")

# 2: Explore/Summarize Data ----
# * 2.1. Descriptive Statistics Tables ----

# * * 2.1.1. Demographic summary table using gt ----
# the distinct function provides all unique rows of specified variables - in this case 1 row per individual.
# Note that a dataset with time varying covariates would require a different approach
# to isolate the unique rows to use for a demography summary table
dm_table <- finaldat %>%
  distinct(ID,AGE,SEX,RACE,WT) %>%
  select(AGE,WT,SEX,RACE) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({min} - {max})")

# save dm_table to RDS so we can pull into our report
saveRDS(dm_table,file="dmtable.RDS")



# *  * 2.1.2. Concentration summary by time and dose group using flextable ----
# gtsummary allows only one grouping column, we'll use dplyr for more
# complex data summary operations, then convert summary data.frame to a flextable

mean_conc_table <- finaldat %>%
  select(TIME, CONC, DOSEGRP) %>%
  #filter(TIME > 0) %>%  #Generally would include in table if PK collected pre-dose
  group_by(DOSEGRP, TIME) %>%
  summarise(CONC_mean = signif(mean(CONC), 3),
            CONC_min = signif(min(CONC), 3),
            CONC_max = signif(max(CONC), 3)) %>%
  mutate(CONC_combined_stat = paste0(CONC_mean, " (", CONC_min, "-", CONC_max, ")")) %>%
  select(DOSEGRP, TIME, CONC_combined_stat) %>%
  pivot_wider(names_from = "DOSEGRP", values_from = "CONC_combined_stat") %>%
  flextable(cwidth = 1.5) %>%
  set_header_labels(`TIME` = "Time (hr)",
                    `5000` = paste0("5000  ","\U03BC","g"),
                    `10000` = paste0("10000 ","\U03BC","g"),
                    `20000` = paste0("20000 ","\U03BC","g")) %>%
  add_header_row(colwidths = c(1,3), values = c("", "Dose Group")) %>%
  set_caption("Mean Concentration by Dose Group and Time") %>%
  align(align = "center", part = "all") %>%
  bold(part="header")

saveRDS(mean_conc_table,file="mean_conc_table.RDS")


# * 2.2 Exploratory Data Analysis ----

# * * 2.2.1. Time-Concentration by Subject Linear ----
pkplot <- finaldat %>%
  mutate(ID = as.factor(ID)) %>%
  ggplot(aes(TIME,CONC,group=ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~DOSEGRP)

saveRDS(pkplot,file="pkplot.RDS")

# * * 2.2.2. Time-Concentration by Subject Log ----
pkplotlog <- pkplot +
  scale_y_log10()

saveRDS(pkplotlog,file="pkplotlog.RDS")

# * * 2.2.3. Time-Concentration by Gender & Dose Group ----
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun = fun,  geom=geom,  ...)
}

pkplot_dosesex <- pkplot + facet_grid(DOSEGRP~SEX) +
      stat_sum_single(mean, geom = "line", aes(group = NULL),
                      col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_dosesex,file="pkplot_dosesex.RDS")

# * * 2.2.4. Time-Concentration by Subject Faceted by Dose Group & Race ----
pkplot_doserace <- pkplot + facet_grid(DOSEGRP~RACE) +
  stat_sum_single(mean, geom = "line", aes(group = NULL),
                  col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_doserace,file="pkplot_doserace.RDS")

# * * 2.2.5. Time-Concentration by Subject Faceted by Equal WT Intervals ----
pkplot_dosewt <- pkplot + facet_wrap(DOSEGRP~cut(WT,3)) + #group into 3 equal "cuts" of WT
  stat_sum_single(mean, geom = "line", aes(group = NULL),
                  col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_dosewt,file="pkplot_dosewt.RDS")

# * * 2.2.6. Time-Concentration by Subject Faceted by Age Quantiles ----
pkplot_doseage <- pkplot + facet_wrap(~cut(AGE,quantile(AGE),include.lowest=TRUE)) + #group into quartile "cuts"
  stat_sum_single(mean, geom = "line", aes(group = NULL),
                  col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_doseage,file="pkplot_doseage.RDS")

# 3: Specify PK Model ----
# * 3.1. Using Command Line ----

# * * 3.1.1. Create Initial Model Object ----

?pkmodel

basemod <- pkmodel(numCompartments = 1,
                   absorption = "FirstOrder",
                   data = finaldat,
                   columnMap = FALSE,           #map columns after model creation
                   modelName = "basemod")

print(basemod)

#Hint:  for any model creation function (pkmodel, pkemaxmodel, pkindirectmodel, pklinearmodel, and linearmodel)
#you can use e.g., print(pkemaxmodel(columnMap=FALSE)) to see default model settings


#Code to explore some pkmodel arguments
basemod <- pkmodel(numCompartments = 1,
                   absorption = "FirstOrder",
                   #isClosedForm = FALSE,
                   #isSaturating = TRUE,
                   #infusionAllowed = TRUE,
                   #isDuration = TRUE,
                   #isTlag = TRUE,
                   data = finaldat,
                   columnMap = FALSE,      #map columns after model creation
                   modelName = "basemod")

print(basemod)

# * * 3.1.2. Map Dataset Variables to Model ----
basemod <- basemod %>%
  colMapping(c(id = "ID", time = "TIME", Aa = "AMT", CObs = "CONC"))

print(basemod)

# * * 3.1.3. Set Initial Estimates and Refine Model Structure ----
basemod <- basemod %>%

  #Set initial parameter estimates ?fixedEffect
  fixedEffect(effect = c("tvKa", "tvV", "tvCl"), value = c(1.5, 80, 9)) %>%

  #Set initial ETA estimates ?randomEffect
  randomEffect(effect = c("nKa", "nV", "nCl"), value = c(0.1, 0.1, 0.1)) %>%

  #Switch to additive error model and set residual error estimate ?residualError
  residualError(predName = "C", errorType = "Additive", SD = 5)

  #Add Covariates to the model (AGE, WT, SEX, RACE) ?addCovariate
  #For the base model, we add the covariate to the model, but we do not
  #assign any parameter relationships.  This allows base model diagnostics
  #on possible covariate effects

  basemod <- basemod %>%

  addCovariate(covariate = c("AGE"),
               type = "Continuous",
               center = "Value",
               centerValue = 45,
               effect = c("V", "Cl")) %>%  #Would use this to assign age to Cl and V

  addCovariate(covariate = c("WT"),
               type = "Continuous",
               center = "Value",
               centerValue = 70,
               effect=NULL) %>%

  addCovariate(covariate = c("SEX"),
               type = "Categorical",
               levels = c(0,1),
               labels = c("M","F"),
               effect=NULL) %>%

  addCovariate(covariate = c("RACE"),
               type = "Categorical",
               levels = c(0,1,2,3),
               labels = c("WHITE","BLACK","ASIAN","OTHER"),
               effect=NULL) %>%

  addCovariate(covariate = c("DOSEGRP"),
               type = "Continuous",
               effect=NULL)

print(basemod)

#Use structuralParameter to add/remove eta terms, change form of error model ?structuralParameter

#Change style of structural parameter
basemod <- basemod %>%
  structuralParameter(paramName = 'Cl',
                      style = "LogNormal1")

print(basemod)

#Remove eta term from Ka
basemod <- basemod %>%
  structuralParameter(paramName = "Ka",
                      hasRandomEffect = FALSE)

print(basemod)

#Put eta term back on Ka
basemod <- basemod %>%
  structuralParameter(paramName = "Ka",
                      hasRandomEffect = TRUE)

print(basemod)

#Remove Covariate effects

basemod <- basemod %>%

  removeCovariate(covariate = c("AGE"),
                  paramName = c("Cl")) %>%

  removeCovariate(covariate = c("AGE"),
                  paramName = c("V"))

print(basemod)

# Save as an RDS file so that we can open in RMarkdown and access results
saveRDS(basemod, file = "basemod.RDS")

# 4: Fit PK Model ----
basemodfit <- fitmodel(basemod)

print(basemodfit)

# Save as an RDS file so that we can open in RMarkdown and access results
saveRDS(basemodfit, file = "basemodfit.RDS")

#Can extract elements from model fit object
names(basemodfit)
basemodfit$Overall
basemodfit$theta

# * 4.1. Optional Host Definitions
# host setup: run locally without MPI
# local <- NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
#                               installationDirectory = Sys.getenv("INSTALLDIR"),
#                               parallelMethod = NlmeParallelMethod("None"),
#                               hostName = "Local",
#                               numCores = 1)
#
# # host setup: run locally with MPI enabled
# localMPI <- NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
#                                  installationDirectory = Sys.getenv("INSTALLDIR"),
#                                  parallelMethod = NlmeParallelMethod("LOCAL_MPI"),
#                                  hostName = "Local_MPI",
#                                  numCores = 4)
#
# # host setup: run locally with multicore enabled
# localMultiCore <- NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
#                                        installationDirectory = Sys.getenv("INSTALLDIR"),
#                                        parallelMethod = NlmeParallelMethod("multicore"),
#                                        hostName = "Local_MultiCore",
#                                        numCores = 4)
#
#usage basemodfit <- fitmodel(basemod, hostPlatform = localMPI )



# 5: Editing Models Directly ----

basemodNew <- editModel(basemod)

# 6: Shiny Apps ----
# * 6.1. Model Builder  ----
basemodui <- modelBuilderUI(finaldat, modelName = "basemodelui")

print(basemodui)

# * 6.2. Initial Estimates
basemodui <- estimatesUI(basemodui)

print(basemodui)

#fit the model
basemoduifit <- fitmodel(basemodui)


# 5: Evaluate Model Fit Diagnostics (next lecture)
library(Certara.ModelResults)
resultsUI(basemod)

