#Reference: Lesson 4 & 6
# 5. Model Compartment Scenario Fits ----
numCpt <-  c("OneCpt" = 1,
             "TwoCpt" = 2,
             "ThreeCpt" = 3)

models <- list()
fit_output <- list()

#review double bracket notation

for (cmpt in numCpt) {
  cmptName <- names(numCpt)[cmpt]

  # * 5.1 Create Compartment Models ----
  models[[cmptName]] <- pkmodel(
    numCompartments = cmpt, #this is the only argument we are varying
    absorption = "FirstOrder",
    data = finaldat,
    columnMap = FALSE,
    modelName = paste0("basemod_", cmptName)
  ) %>%
    #Set initial parameter estimates ?fixedEffect
    fixedEffect(effect = c("tvKa", "tvV", "tvCl"),
                value = c(1.5, 80, 9)) %>%
    #Set initial ETA estimates ?randomEffect
    randomEffect(effect = c("nKa", "nV", "nCl"),
                 value = c(0.1, 0.1, 0.1)) %>%
    colMapping(
      id = "ID",
      time = "TIME",
      Aa = "AMT",
      CObs = "CONC"
    )

  message("Fitting ", cmptName)

  # * 5.2 Fit Compartment Models ----
  # Save returned values from fitmodel in a list
  fit_output[[cmptName]] <- fitmodel(models[[cmptName]])

  # Add a 'Model' column in the #Overall table to identify compartment
  fit_output[[cmptName]]$Overall <- fit_output[[cmptName]]$Overall %>%
    mutate(Model = cmptName)
}

# * 5.3 Create Model Comparison Table ----
# Use the rbind() function to combine Overall tables from fitmodel output
# into a single table
overall_table_cmpt_scenarios <-
  rbind(fit_output$OneCpt$Overall,
        fit_output$TwoCpt$Overall,
        fit_output$ThreeCpt$Overall) %>%
  select(Model, `-2LL`, AIC, BIC, nParm, nObs, nSub, EpsShrinkage) %>%
  flextable()

save_as_image(overall_table_cmpt_scenarios, path = "./tables/overall_table_cmpt_scenarios.png")

# -2LL is only good for comparing "nested" models.
# We should additionally use AIC/BIC to consider number of parameters vs how good the fit improves


