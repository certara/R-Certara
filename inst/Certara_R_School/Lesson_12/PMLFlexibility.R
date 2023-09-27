# Overview ----
# This is code for Certara.R School Lesson 12, History and Advantages of the PML Language
# In this lesson we will run a few examples demonstrating the flexibility of the PML Language

# 0: Load libraries

library(Certara.RsNLME) #RsNLME package
library(Certara.RsNLME.ModelBuilder) #R Shiny app for creating models
library(Certara.ModelResults)
library(ggplot2) #useful plotting package
library(dplyr)
library(Certara.Xpose.NLME)
library(xpose)
library(gridExtra)

# 1: Import Data ----
# * 1.1 Read in final dataset from lesson 2 ----
finaldat <- readRDS("../Lesson_2/finaldat.RDS")


# 2: Specify and Fit PK Model ----
finalmod <- pkmodel(numCompartments = 1,
                   absorption = "FirstOrder",
                   data = finaldat,
                   columnMap = FALSE,
                   modelName = "finalmod") %>%
  colMapping(c(id = "ID", time = "TIME", Aa = "AMT", CObs = "CONC")) %>%
  fixedEffect(effect = c("tvKa", "tvV", "tvCl"), value = c(1.5, 80, 9)) %>%
  randomEffect(effect = c("nKa", "nV", "nCl"), value = c(0.1, 0.1, 0.1)) %>%
  addCovariate(covariate = "WT", effect = c("Cl"), center = "Value", centerValue = 70)

  print(finalmod)

  ## Fit final model
  finalmodfit <- fitmodel(finalmod)

  ## GOF Plots
  xpdb <- xposeNlme("finalmod", dir="finalmod")

  xpdb %>% dv_vs_pred(type="ps")
  xpdb %>% dv_vs_ipred(type="ps")
  #xpdb %>% ind_plots(page = 1, ncol = 3, nrow = 3)

# 3: Simulate a Reference Single Dose, then a SS Regimen ----
# * 3.1 Add a Single SS column to dataset with value = 1 for AMT records ----
simdat <- finaldat %>%
    mutate(SS = if_else(AMT>0,1,0)) %>%
    filter(ID %in% 1:10)   #cut dataset to 10 subjects for speed

# * 3.2 Set up simulation as in Lesson 7 ----

# ** 3.2.1 Make a copy of our final model ----
simmod <- copyModel(finalmod, acceptAllEffects = TRUE, modelName = "simmod")

# ** 3.2.2 Replace the Mapped Dataset with the Simulation Dataset ----

#View the dataset that the model is currently mapped to
simmod@inputData  #shows original dataset

#Swap dataset to new simdat
simmod@inputData <- simdat

# ** 3.2.3 Check Mappings and Map Variables as Necessary ----
#Check mappings.  All look OK
print(simmod)

#Cut BSV to low number so we can easily see dose/schedule change impact for demo
simmod <- simmod %>%
  randomEffect(effect = c("nKa", "nV", "nCl"), value = c(0.01, 0.01, 0.01))

# * 3.3 Single Dose Simulation for Reference ----

# ** 3.3.1 Specify Simulation Outputs, and Prepare Simulation Settings ----

#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.5),
                            variablesList = c("C", "CObs","WT","SEX","DOSEGRP"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

# ** 3.3.2 Run the simulation ----

#This function checks for existing simulation directory and deletes if present
#by default RsNLME will not overwrite sim output
simclean <- function(file_name) {
  if (file.exists(file_name)) {
  unlink(file_name,recursive = T)
  print("File is deleted..")
} else{
  print("File not exists..")
}
}

simclean("simmod")
simmodfit <- simmodel(simmod, SimSetup)

# * 3.3.3 Post-Process, Plot simulation results ----
# extract output from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

#Make a plot

p1<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose") +
  #geom_hline(yintercept=mean(SimTableout$C[SimTableout$time==24]), color='blue') +
  scale_y_continuous(limits=c(0,120)) +
  theme_certara()
p1


# * 3.4 Steady State Simulation ----
# ** 3.4.1 Use addDoseCycle to map SS dose variables ----
simmodSS <- copyModel(simmod, modelName = "simmodSS")


simmodSS <- simmodSS %>%
addDoseCycle(type = "SteadyState", amount="AMT", name = "Aa", colName = "SS", II = 12)

#Re-check mappings.  Note that SteadyState is now mapped
print(simmodSS)

#Run Simulation
simclean("simmodSS")
simmodSSfit <- simmodel(simmodSS, SimSetup)

# * 3.4.2 Post-Process, Plot simulation results ----
# extract output from the simulation fit object
SimTableSSout <- simmodSSfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

#Make a plot

p2<-ggplot(SimTableSSout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Steady State q12h") +
  #geom_hline(yintercept=mean(SimTableSSout$C[SimTableSSout$time==24]), color='blue') +
  scale_y_continuous(limits=c(0,120)) +
  theme_certara()
p2
gridExtra::grid.arrange(p1,p2,ncol=2)


# * 3.5 Multiple Dose Simulation ----
# Make a copy of model
simmodMD <- copyModel(simmod, modelName = "simmodMD")

print(simmodMD)

simmodMD@inputData

# We need to add an addl column to the dataset
simdatMD <- simdat %>%
  mutate(ADDL = if_else(AMT>0,10,0))

# Now we map the new simdatMD to the model
simmodMD <- simmodMD %>%
  dataMapping(simdatMD)

#Check mappings.  Note the presence of '?' for unmapped variables
print(simmodMD)

#Map AMT and DV to Aa and CObs
simmodMD <- simmodMD %>%
  colMapping(c(Aa="AMT", CObs="CONC"))

#Re-check mappings.  Note that Aa and DV are now mapped
print(simmodMD)

# ** 3.5.1 Use addDoseCycle to map multiple dose variables ----
simmodMD <- simmodMD %>%
  addDoseCycle(type = "ADDL", amount="AMT", name = "Aa", colName = "ADDL", II = 12)

#Re-check mappings.  Note that SteadyState is now mapped
print(simmodMD)

#Extend Sample times for Simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,168,.5),
                            variablesList = c("C", "CObs","WT","SEX","DOSEGRP"))

SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))
#Run Simulation
simclean("simmodMD")
simmodMDfit <- simmodel(simmodMD, SimSetup)

# * 3.4.2 Post-Process, Plot simulation results ----
# extract output from the simulation fit object
SimTableMDout <- simmodMDfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

#Make a plot

p3<-ggplot(SimTableMDout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Multiple Dose q12h") +
  #geom_hline(yintercept=10, color='blue') +
  scale_y_continuous(limits=c(0,120)) +
  theme_certara()
p3


#Superimpose our SS curve in blue
p3 +
  #geom_line(data=SimTableSSout,alpha=.05) +
  stat_summary(data=SimTableSSout,aes(group=NULL),fun=median,geom='line',
               colour = "blue", size = 1.2, alpha=.6) +
  stat_summary(data=SimTableSSout,aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "blue", size = 1.2, lty=2, alpha=.6) +
  stat_summary(data=SimTableSSout,aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "blue", size = 1.2, lty=2, alpha=.6)


