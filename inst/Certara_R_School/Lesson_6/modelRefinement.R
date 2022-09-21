# Overview ----
# This is code for Certara.R School Lesson 6, Model Refinement
# In this lesson we will create and run a the base RsNLME model
# we created in Lesson 2, review model diagnostics,
# and then copy and modify our base model to change the residual
# error model and add a few covariate effects
# Finally, we will compare diagnostics between models


# 0: Load libraries

library(Certara.RsNLME) #RsNLME package
library(Certara.RsNLME.ModelBuilder) #R Shiny app for creating models
library(ggplot2) #useful plotting package
library(dplyr)
library(gt)
library(gtsummary)
library(flextable)
library(tidyr)
library(egg)

# 1: Import Data ----
# * 1.1 Read in xpt datasets ----
finaldat <- readRDS("finaldat.RDS")


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

# 3: Fit PK Model ----
basemodfit <- fitmodel(basemod)

print(basemodfit)

# 4: Create Diagnostic Plots using xpose ----
# (Diagnostics covered in detail in Lecture_5)

library(Certara.Xpose.NLME)
library(xpose)

## xposeNlme imports results of an NLME run into xpose database to create commonly used diagnostic plots
xpdb <- xposeNlme(dir = basemod@modelInfo@workingDir, modelName = "basemod")

xpdb %>% dv_vs_pred(type="ps")
xpdb %>% dv_vs_ipred(type="ps")
xpdb %>% res_vs_pred(type="ps")  #residual vs predicted ?res_vs_pred
xpdb %>% absval_res_vs_pred(type="ps") #absolute residual
xpdb %>% res_vs_idv(type="ps")  #res vs time
xpdb %>% res_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)
xpdb %>% prm_vs_cov(covariate="WT",type='ps')

xpdb %>% eta_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)

### POLL #1 ### Next Step to Improve Model?


# 5: Edit and re-fit Base Model ----
# * 5.1 Copy Base Model to New Model ----
basemodNew <- copyModel(basemod, acceptAllEffects = TRUE, modelName = "basemodNew")

# * 5.2 Switch to multiplicative error model ----
basemodNew <- basemodNew %>%
  residualError(predName = "C", errorType = "Multiplicative", SD = 0.1)

print(basemodNew)

# * 5.3 Fit PK Model ----
basemodNewfit <- fitmodel(basemodNew)

# * 5.4 Create Diagnostic Plots using xpose ----
xpdbNew <- xposeNlme(dir = basemodNew@modelInfo@workingDir, modelName = "basemodNew")

xpdbNew %>% dv_vs_pred(type="ps")
xpdbNew %>% dv_vs_ipred(type="ps")
xpdbNew %>% res_vs_pred(type="ps")  #residual vs predicted ?res_vs_pred
xpdbNew %>% res_vs_idv(type="ps")  #res vs time
xpdbNew %>% res_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)
xpdbNew %>% prm_vs_cov(covariate="WT",type='ps')

xpdbNew %>% eta_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)


#Some Useful xpose.nlme functions
xpdbNew %>% nlme.cov.splom()
xpdbNew %>% nlme.par.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)
xpdbNew %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)
xpdbNew %>% nlme.var.vs.cov(covColNames=c("AGE","WT","SEX","RACE"), yVar = "nCl",nrow=2)



### POLL #2 ### Next Step to Improve Model?



# 6: Edit New Base Model using addCovariate ----
# * 6.1 Copy Base Model to New Model ----
run1WTonCLmod <- copyModel(basemodNew, acceptAllEffects = TRUE, modelName = "run1WTonCLmod")

# * 6.2 Add Covariate Effect WT on CL ----
run1WTonCLmod <- run1WTonCLmod %>%
  addCovariate(covariate = c("WT"),
               type = "Continuous",
               center = "Value",
               centerValue = 70,
               effect=c('Cl'))

print(run1WTonCLmod)


#A note on the structuralParameter function  ?structuralParameter
#style = "LogNormal" is default: The structural parameter is defined as Product * exp(Eta)
run1WTonCLmod@statements[9]

structParmDemo1 <- run1WTonCLmod %>%
  structuralParameter(paramName = "Cl", style = "Normal") #The structural parameter is defined as Sum + Eta
structParmDemo1@statements[9]

structParmDemo2 <- run1WTonCLmod %>%
  structuralParameter(paramName = "Cl", style = "LogNormal1") #The structural parameter is defined as Sum * exp(Eta)
structParmDemo2@statements[9]

structParmDemo3 <- run1WTonCLmod %>%
  structuralParameter(paramName = "Cl", style = "LogNormal2") #The structural parameter is defined as exp(Sum + Eta).
structParmDemo3@statements[9]

# * 6.3 Fit PK Model ----
run1WTonCLfit <- fitmodel(run1WTonCLmod)

# * 6.4 Create Diagnostic Plots using xpose ----
xpdbrun1 <- xposeNlme(dir = run1WTonCLmod@modelInfo@workingDir, modelName = "run1WTonCLmod")

xpdbrun1 %>% dv_vs_pred(type="ps")
xpdbrun1 %>% dv_vs_ipred(type="ps")
xpdbrun1 %>% res_vs_pred(type="ps")  #residual vs predicted ?res_vs_pred
xpdbrun1 %>% res_vs_idv(type="ps")  #res vs time
xpdbrun1 %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)

# 7: Edit New Base Model using editModel Function ----
# * 7.1 Copy Base Model to New Model ----
run2SEXonCLmod <- copyModel(basemodNew, acceptAllEffects = TRUE, modelName = "run2SEXonCLmod")

# * 7.2 Edit PML Directly with editModel command ----
run2SEXonCLmod <- editModel(run2SEXonCLmod)

## Here is the syntax for 0,1 categorical covariate
# test(){
#   cfMicro(A1,Cl/V, first = (Aa = Ka))
#   dosepoint(Aa)
#   C = A1 / V
#   error(CEps=0.206648343877209)
#   observe(CObs=C * ( 1 + CEps))
#   stparm(Ka = tvKa * exp(nKa))
#   stparm(V = tvV * exp(nV))
#   stparm(Cl = tvCl * exp(dCldSEX1*(SEX==1)) * exp(nCl))
#   fcovariate(AGE)
#   fcovariate(WT)
#   fcovariate(SEX())
#   fcovariate(RACE())
#   fcovariate(DOSEGRP)
#   fixef( tvKa = c(,1.17549291309572,))
#   fixef( tvV = c(,81.3292974414819,))
#   fixef( tvCl = c(,7.93441043312941,))
#   fixef( dCldSEX1 = c(,0,))
#   ranef(diag(nKa,nV,nCl) = c(0.0795401258514883,0.1782003264944,0.370029725552631))
#
# }

#If you need syntax help, launch modelBuilderUI:
#modelBuilderUI(finaldat,modelName="junk")

#can use base R to understand covariate equations:
# sextheta <- seq(-1,1,.01)
# sexeffect<-exp(sextheta)
# plot(sextheta,sexeffect,type='l')

print(run2SEXonCLmod)

# * 7.3 Fit PK Model ----
run2SEXonCLfit <- fitmodel(run2SEXonCLmod)

# * 7.4 Create Diagnostic Plots using xpose ----
xpdbrun2 <- xposeNlme(dir = run2SEXonCLmod@modelInfo@workingDir, modelName = "run2SEXonCLmod")

xpdbrun2 %>% dv_vs_pred(type="ps")
xpdbrun2 %>% dv_vs_ipred(type="ps")
xpdbrun2 %>% res_vs_pred(type="ps")  #residual vs predicted ?res_vs_pred
xpdbrun2 %>% res_vs_idv(type="ps")  #res vs time
xpdbrun2 %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)

# 8. Edit New Base Model in any text editor and read into RsNLME ----
# * 8.1 Create model from text file using textualmodel function ----
run3WTonVmod <- textualmodel(modelName = "run3WTonVmod", data = finaldat, mdl = "WTonVmodel.txt")

# * 8.2 Check Model and Map Missing Variables ----
print(run3WTonVmod)

run3WTonVmod <- run3WTonVmod %>%
  colMapping(c(Aa = "AMT", CObs = "CONC")) %>%
  addLabel("SEX", c(0,1), c("M","F")) %>%
  addLabel("RACE", c(0,1,2,3), c("WHITE","BLACK","ASIAN","OTHER"))

print(run3WTonVmod)

# * 8.3 Fit PK Model ----
run3WTonVfit <- fitmodel(run3WTonVmod)

# * 8.4 Create Diagnostic Plots using xpose ----
xpdbrun3 <- xposeNlme(dir = run3WTonVmod@modelInfo@workingDir, modelName = "run3WTonVmod")

xpdbrun3 %>% dv_vs_pred(type="ps")
xpdbrun3 %>% dv_vs_ipred(type="ps")
xpdbrun3 %>% res_vs_pred(type="ps")  #residual vs predicted ?res_vs_pred
xpdbrun3 %>% res_vs_idv(type="ps")  #res vs time
xpdbrun3 %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)


# 9. Markdown!  ----
# Below we simply create RDS objects from our diagnostic plots for
# import into Rmarkdown

## BASE model additive error model
## xposeNlme imports results of an NLME run into xpose database to create commonly used diagnostic plots
xpdb <- xposeNlme(dir = basemod@modelInfo@workingDir, modelName = "basemod")

basedvpred <- xpdb %>% dv_vs_pred(type="ps")
saveRDS(basedvpred,file="basedvpred.RDS")

basedvipred <- xpdb %>% dv_vs_ipred(type="ps")
saveRDS(basedvipred,file="basedvipred.RDS")

baserespred <- xpdb %>% res_vs_pred(type="ps")  #residual vs predicted ?res_vs_pred
saveRDS(baserespred,file="baserespred.RDS")

basearespred <- xpdb %>% absval_res_vs_pred(type="ps")
saveRDS(basearespred,file="basearespred.RDS")

baseresidv <- xpdb %>% res_vs_idv(type="ps")  #res vs time
saveRDS(baseresidv,file="baseresidv.RDS")

baseprmcovwt <- xpdb %>% prm_vs_cov(covariate="WT",type='ps')
saveRDS(baseprmcovwt,file="baseprmcovwt.RDS")

baseetacovwt <- xpdb %>% eta_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)
saveRDS(baseetacovwt,file="baseetacovwt.RDS")

## BASENEW model multiplicative additive error model
saveRDS(basemodNewfit, file = "basemodNewfit.RDS")

## xposeNlme imports results of an NLME run into xpose database to create commonly used diagnostic plots
xpdbnew <- xposeNlme(dir = basemodNew@modelInfo@workingDir, modelName = "basemodNew")

basenewdvpred <- xpdbnew %>% dv_vs_pred(type="ps")
saveRDS(basenewdvpred,file="basenewdvpred.RDS")

basenewdvipred <- xpdbnew %>% dv_vs_ipred(type="ps")
saveRDS(basenewdvipred,file="basenewdvipred.RDS")

basenewrespred <- xpdbnew %>% res_vs_pred(type="ps")  #residual vs predicted ?res_vs_pred
saveRDS(basenewrespred,file="basenewrespred.RDS")

basenewarespred <- xpdbnew %>% absval_res_vs_pred(type="ps")
saveRDS(basenewarespred,file="basenewarespred.RDS")

basenewresidv <- xpdbnew %>% res_vs_idv(type="ps")  #res vs time
saveRDS(basenewresidv,file="basenewresidv.RDS")

basenewprmcovwt <- xpdbnew %>% prm_vs_cov(covariate="WT",type='ps')
saveRDS(basenewprmcovwt,file="basenewprmcovwt.RDS")

basenewetacovwt <- xpdbnew %>% eta_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)
saveRDS(basenewetacovwt,file="basenewetacovwt.RDS")

basenewranparcov <- xpdbnew %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)
saveRDS(basenewranparcov,file="basenewranparcov.RDS")

#WT on CL Plots

#model fit summary
saveRDS(run1WTonCLfit,file="run1WTonCLfit.RDS")

run1dvpred <- xpdbrun1 %>% dv_vs_pred(type="ps")
saveRDS(run1dvpred,file="run1dvpred.RDS")

run1dvipred <- xpdbrun1 %>% dv_vs_ipred(type="ps")
saveRDS(run1dvipred,file="run1dvipred.RDS")

run1respred <- xpdbrun1 %>% res_vs_pred(type="ps")
saveRDS(run1respred,file="run1respred.RDS")

run1residv <- xpdbrun1 %>% res_vs_idv(type="ps")
saveRDS(run1residv,file="run1residv.RDS")

run1ranparcov <- xpdbrun1 %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)
saveRDS(run1ranparcov,file="run1ranparcov.RDS")

#SEX on CL Plots

#model fit summary
saveRDS(run2SEXonCLfit,file="run2SEXonCLfit.RDS")

run2dvpred <- xpdbrun2 %>% dv_vs_pred(type="ps")
saveRDS(run2dvpred,file="run2dvpred.RDS")

run2dvipred <- xpdbrun2 %>% dv_vs_ipred(type="ps")
saveRDS(run2dvipred,file="run2dvipred.RDS")

run2respred <- xpdbrun2 %>% res_vs_pred(type="ps")
saveRDS(run2respred,file="run2respred.RDS")

run2residv <- xpdbrun2 %>% res_vs_idv(type="ps")
saveRDS(run2residv,file="run2residv.RDS")

run2ranparcov <- xpdbrun2 %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)
saveRDS(run2ranparcov,file="run2ranparcov.RDS")

#WT on V Plots

#model fit summary
saveRDS(run3WTonVfit,file="run3WTonVfit.RDS")

run3dvpred <- xpdbrun3 %>% dv_vs_pred(type="ps")
saveRDS(run3dvpred,file="run3dvpred.RDS")

run3dvipred <- xpdbrun3 %>% dv_vs_ipred(type="ps")
saveRDS(run3dvipred,file="run3dvipred.RDS")

run3respred <- xpdbrun3 %>% res_vs_pred(type="ps")
saveRDS(run3respred,file="run3respred.RDS")

run3residv <- xpdbrun3 %>% res_vs_idv(type="ps")
saveRDS(run3residv,file="run3residv.RDS")

run3ranparcov <- xpdbrun3 %>% nlme.ranpar.vs.cov(covColNames=c("AGE","WT","SEX","RACE"),nrow = 4)
saveRDS(run3ranparcov,file="run3ranparcov.RDS")
