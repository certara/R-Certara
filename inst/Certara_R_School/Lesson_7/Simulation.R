# Overview ----
# This is code for Certara.R School Lesson 7, Simulation
# In this lesson we will run a stepwise covariate search
# and use the resultant model to simulate exposure in a
# new patient population



# 0: Load libraries

library(Certara.RsNLME) #RsNLME package
library(Certara.RsNLME.ModelBuilder) #R Shiny app for creating models
library(Certara.ModelResults)
library(ggplot2) #useful plotting package
library(dplyr)
library(gt)
library(gtsummary)
library(flextable)
library(tidyr)
library(egg)



# Stepwise Covariate Model Run ----
# 1: Import Data ----
# * 1.1 Read in final dataset ----
finaldat <- readRDS("../Lesson_2/finaldat.RDS")


# 2: Specify PK Model ----
# * 2.1. Using Command Line ----

# * * 2.1.1. Create Initial Model Object ----

basemod <- pkmodel(numCompartments = 1,
                   absorption = "FirstOrder",
                   data = finaldat,
                   columnMap = FALSE,           #map columns after model creation
                   modelName = "basemod")

print(basemod)

# * * 2.1.2. Map Dataset Variables to Model ----
basemod <- basemod %>%
  colMapping(c(id = "ID", time = "TIME", Aa = "AMT", CObs = "CONC"))

print(basemod)

# * * 2.1.3. Set Initial Estimates and Refine Model Structure ----
basemod <- basemod %>%

  #Set initial parameter estimates ?fixedEffect
  fixedEffect(effect = c("tvKa", "tvV", "tvCl"), value = c(1.5, 80, 9)) %>%

  #Set initial ETA estimates ?randomEffect
  randomEffect(effect = c("nKa", "nV", "nCl"), value = c(0.1, 0.1, 0.1)) %>%

  #Switch to additive error model and set residual error estimate ?residualError
  residualError(predName = "C", errorType = "Multiplicative", SD = .2)

  #Add Covariates to the model (AGE, WT, SEX, RACE) ?addCovariate
  #For the base model, we add the covariate to the model, but we do not
  #assign any parameter relationships.  This allows base model diagnostics
  #on possible covariate effects

  basemod <- basemod %>%

  addCovariate(covariate = c("AGE"),
               type = "Continuous",
               center = "Value",
               centerValue = 45,
               effect=NULL) %>%

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

basemodfit <- fitmodel(basemod)

# 3: Prepare and Execute a Stepwise Covariate Model Run ----
## Make a copy of the basemodel

covsearchmod <- copyModel(basemod, modelName="covsearchmod")

## Add covariate effects to the model
covsearchmod <- covsearchmod %>%
  addCovariate(covariate = "WT", effect = c("Ka","V","Cl"), center = "Value", centerValue = 70) %>%
  addCovariate(covariate = "AGE", effect = c("Ka","V","Cl"), center = "Value", centerValue = 50) %>%
  addCovariate(covariate = "SEX", effect = c("Ka","V","Cl"),
               type = "Categorical", levels = c(0,1), labels=c("M","F")) %>%
  addCovariate(covariate = "RACE", effect = c("Ka","V","Cl"),
               type = "Categorical", levels = c(0,1,2,3), labels=c("WHITE","BLACK","ASIAN","OTHER"))

## View the model
print(covsearchmod)

## Execute the stepwise search
## For stepwise search it will improve speeds if we use parallelization
## Define a local multicore host - The "multicore" parallelMethod will send separate jobs
## to open cores, running models in parallel
localMultiCoreHost <- NlmeParallelHost(installationDirectory = Sys.getenv("INSTALLDIR"),
                                       parallelMethod = NlmeParallelMethod("multicore"),
                                       hostName = "Local_MultiCore",
                                       numCores = 4)

## Run the search - see ?stepwiseSearch
covsearchmodfit <- stepwiseSearch(covsearchmod, hostPlatform = localMultiCoreHost)

## The results stored in the "stepwise.txt" file tell us that the stepwise
## model approach identified a model with only WT on Cl as the best model
covsearchresults <- read.csv("./covsearchmod/stepwise.txt", header=FALSE)

covsearchresults %>%
  rename(Model.Run = V1, Chosen.OBJ = V2) %>%
  flextable() %>%
  autofit()

# 4: Create Final Model and Fit ----
finalmod <- copyModel(basemod, acceptAllEffects = TRUE, modelName="finalmod")

## Add covariate effects to the model
finalmod <- finalmod %>%
  addCovariate(covariate = "WT", effect = c("Cl"), center = "Value", centerValue = 70)

print(finalmod)

## Fit final model
finalmodfit <- fitmodel(finalmod)

# 5: Simulation ----

# Can directly simulate from any model
# For example, this runs a simulation from our base model
# using default settings for outputs and simulation parameters
# (100 replicates, dataset times for output): See ?simmodel

# Make a copy of basemod with acceptAllEffects = TRUE
# Why?:  otherwise the initial parameter estimates would be used for simulation
basemod <- copyModel(basemod,acceptAllEffects = TRUE, modelName = "basemod")

print(basemod)

# Run simulation
basesim <- simmodel(basemod)

# Post-process output
basesimout <- basesim$predout %>%
  mutate(id=as.numeric(paste0(REPLICATE,ID5)))

ggplot(basesimout, aes(x=IVAR, y=DV, group=factor(id))) +
  geom_line(alpha=.03)

basesimout %>%
  select(IVAR,DV) %>%
  tbl_summary(by=IVAR)

# Usually we want to simulate for some modified conditions (dose regimen, patient population, etc.)

# * 5.1 Create an Input Dataset that will be used for simulation ----
simdat <- finaldat %>%    #make a copy of original dataset
  filter(AMT!=0) %>%    #keep only rows where AMT!=0 (dose rows, in this case 1 row per ID)
  mutate(ADDL=7,II=6)   #add ADDL (additional doses) and II (interdose interval) variables

# * 5.2 Prepare the Simulation Model ----

# ** 5.2.1 Make a copy of our final model ----
simmod <- copyModel(finalmod, acceptAllEffects = TRUE, modelName = "simmod")

# ** 5.2.2 Replace the Mapped Dataset with the Simulation Dataset ----

#View the dataset that the model is currently mapped to
simmod@inputData  #shows original dataset

#replace this dataset with simdat using the dataMapping function
simmod <- simmod %>%
  dataMapping(simdat)

# ** 5.2.3 Check Mappings and Map Variables as Necessary ----
#Check mappings.  Note the presence of '?' for unmapped variables
print(simmod)

#Map AMT and DV to A1 and CObs
simmod <- simmod %>%
  colMapping(c(Aa="AMT", CObs="CONC"))

#Re-check mappings.  Note that Aa and DV are now mapped
print(simmod)

#If original dataset did not use variables like ADDL, SS, etc., we need to add them
# ** 5.2.4 Use addADDL to map multiple dose variables ----
simmod <- simmod %>%
  addADDL(ADDL="ADDL", II="II")

#Re-check mappings.  Note that ADDL and II are now mapped
print(simmod)

#Use addSteadyState to map a SS dose event
# simmod <- simmod %>%
#   addSteadyState(SS="SS", II="II")

#Use addInfusion to specify an infusion dose event

# ## Using infusion Rate column in dataset:
# simmod <- simmod %>%
#   addInfusion(doseCptName = "A1", colName = "RATE")
#
# ## Using infusion Duration column in dataset:
# simmod <- simmod %>%
#   addInfusion(doseCptName = "A1", isDuration = TRUE, colName = "DUR")


# * 5.3 Specify Simulation Outputs, and Prepare Simulation Settings ----

#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.1),
                            variablesList = c("C", "CObs","WT","SEX","DOSEGRP"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

## * 5.4 Run the simulation ----
simmodfit <- simmodel(simmod, SimSetup)

## * 5.5 Post-Process, Plot simulation results ----
## Simulated drug concentration at the central compartment
## Read in the simulation output dataset specified in the call to NlmeSimTableDef

## can read the file in from the simulation directory
## simdat<-read.csv("./simmod/SimTable.csv")

## Or, can extract it from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

#Make a plot with some WT cuts

SimTableout$wtgrp <- cut_interval(SimTableout$WT,3)

ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  facet_grid(DOSEGRP~wtgrp) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time by Dose and WT Group") +
  theme_certara()


# 6: A note on discretizing (cutting, grouping) continuous variables for plotting ----

# It's often useful to cut continuous variables into groups for plotting
# Many ways to do this

#Let's look at the range of our WT data
summary(simdat$WT)  #WT Range in data is 55-94 kg
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#55.00   64.00   71.00   70.94   77.00   94.00


# Some different cut examples
# base R cut function
SimTableout$wtgrp <- cut(SimTableout$WT,breaks = c(50,70,100))
SimTableout %>%
  select(C,wtgrp,WT) %>%
  tbl_summary(by=wtgrp,
              statistic = list(all_continuous() ~ "{mean} ({min}-{max})"))

# ggplot2 cut_interval
SimTableout$wtgrp <- cut_interval(SimTableout$WT,3)
SimTableout %>%
  select(C,wtgrp,WT) %>%
  tbl_summary(by=wtgrp,
              statistic = list(all_continuous() ~ "{mean} ({min}-{max})"))

# ggplot2 cut_number
SimTableout$wtgrp <- cut_number(SimTableout$WT,3)
SimTableout %>%
  select(C,wtgrp,WT) %>%
  tbl_summary(by=wtgrp,
              statistic = list(all_continuous() ~ "{mean} ({min}-{max})"))

# ggplot2 cut_width
SimTableout$wtgrp <- cut_width(SimTableout$WT,10)
SimTableout %>%
  select(C,wtgrp,WT) %>%
  tbl_summary(by=wtgrp,
              statistic = list(all_continuous() ~ "{mean} ({min}-{max})"))

SimTableout$wtgrp <- cut(SimTableout$WT,
                         breaks = quantile(SimTableout$WT,c(0,.25,.5,.75,1)),
                         include.lowest = TRUE,
                         labels=c("0-25%",">25-50%",">50-75%",">75-100%"))
SimTableout %>%
  select(C,wtgrp,WT) %>%
  tbl_summary(by=wtgrp,
              statistic = list(all_continuous() ~ "{mean} ({min}-{max})"))




# It's possible to simulate covariate distributions for a simulation run within the PML model
# file (as opposed to setting up in the dataset)

# 7: Simulation of Covariates within a PML model ----

# * 7.1 Make a copy of our first simulation model with the new multiple dose regimen----

simmod2 <- copyModel(simmod, modelName = "simmod2")

print(simmod2)


# * 7.2 Edit the model to simulate covariate values on the fly ----
simmod2 <- editModel(simmod2)

# test(){
#  double(WTdist)  #declare a variable we name WTdist
#  sequence{
#    WTdist = 100 + 10 * norm()  #sample WTdist from specified distribution - mean 100, sd 10.
#  }
#  cfMicro(A1,Cl/V, first = (Aa = Ka))
#  dosepoint(Aa)
#  C = A1 / V
#  WT = WTdist    #assign WT to WTdist
#  error(CEps=0.206785987348122)
#  observe(CObs=C * ( 1 + CEps))
#  stparm(Ka = tvKa * exp(nKa))
#  stparm(V = tvV * exp(nV))
#  stparm(Cl = tvCl * ((WT/70)^dCldWT)   * exp(nCl))
#  fcovariate(AGE)
#  #fcovariate(WT)   #comment out WT covariate (we are not using WT from datset)
#  fcovariate(SEX())
#  fcovariate(RACE())
#  fcovariate(DOSEGRP)
#  fixef( tvKa = c(,1.17810280534081,))
#  fixef( tvV = c(,81.5657539364354,))
#  fixef( tvCl = c(,7.76447828738055,))
#  fixef( dCldWT(enable=c(0)) = c(,2.76217598762253,))
#  ranef(diag(nKa,nV,nCl) = c(0.0771743182767522,0.179062993434557,0.259470580853362))
#
# }

## Example with categorical covariate
# test(){
#
#   double(udist, SEXdist)
#   sequence{
#     udist = unif()
#     if (udist < 0.7){          #This will create 70% SEX = 0, 30% SEX = 1
#       SEXdist = 0
#     } else {
#       SEXdist = 1
#     }
#   }
#
#   cfMicro(A1,Cl/V, first = (Aa = Ka))
#   dosepoint(Aa)
#   C = A1 / V
#   SEX = SEXdist    #assign SEX to SEXdist
#   error(CEps=0.206785987348122)
#   observe(CObs=C * ( 1 + CEps))
#   stparm(Ka = tvKa * exp(nKa))
#   stparm(V = tvV * exp(nV))
#   stparm(Cl = tvCl * exp(dClSEX1*(SEX==1))   * exp(nCl))  #SEX==1 is logical check, returns 1 if true, 0 if false
#   fcovariate(AGE)
#   fcovariate(WT)
#   #fcovariate(SEX()) #comment out SEX covariate (we are not using SEX from datset)
#   fcovariate(RACE())
#   fcovariate(DOSEGRP)
#   fixef( tvKa = c(,1.17810280534081,))
#   fixef( tvV = c(,81.5657539364354,))
#   fixef( tvCl = c(,7.76447828738055,))
#   fixef( dClSEX1(enable=c(0)) = c(,0,))
#   ranef(diag(nKa,nV,nCl) = c(0.0771743182767522,0.179062993434557,0.259470580853362))
#
# }

# * 7.3 Specify Simulation Outputs, and Prepare Simulation Settings ----

#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.1),
                            variablesList = c("C", "CObs","WT","SEX","DOSEGRP"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))


## * 7.4 Run the simulation ----
simmodfit2 <- simmodel(simmod2, SimSetup)

## *7.5 Post-Process, Make a plot with some WT cuts ----
SimTableout2 <- simmodfit2$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

options(scipen = 999) #suppress scientific notation in y axis label
ggplot(SimTableout2, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  facet_wrap(~DOSEGRP, nrow=3) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time by Dose Group") +
  theme_certara()


# 9. Markdown!  ----
# Below we simply create RDS objects from our simulation for
# import into Rmarkdown



#Let's cut by quartile
SimTableout$wtgrp <- cut(SimTableout$WT,
                         breaks = quantile(SimTableout$WT,c(0,.25,.5,.75,1)),
                         include.lowest = TRUE,
                         labels=c("0-25%",">25-50%",">50-75%",">75-100%"))

finalsimq6hbywtgrp <- ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  facet_grid(DOSEGRP~wtgrp) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time by Dose and WT Group") +
  theme_certara()

saveRDS(finalsimq6hbywtgrp,file="finalsimq6hbywtgrp.RDS")
