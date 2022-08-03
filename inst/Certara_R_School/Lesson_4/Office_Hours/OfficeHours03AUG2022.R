## Set Working Directory and Set up RsNLME R session
library(Certara.RsNLME)
library(Certara.RsNLME.ModelBuilder)
library(Certara.RsNLME.ModelExecutor)
library(Certara.Xpose.NLME)
library(Certara.ModelResults)
library(Certara.VPCResults)
library(tidyvpc)
library(xpose)
library(dplyr)
library(ggplot2)
library(ggquickeda)
library(egg)
##==========================================================================================================
## Read the datafile into R and make some plots
##==========================================================================================================
dt_InputDataSet <- read.csv("pkdata.csv") 

#display the first few rows
head(dt_InputDataSet)

## Graph drug concentration vs. time
## This plot suggests that either a 1 or 2-compartment model
## with IV bolus is a good starting point

ggplot(subset(dt_InputDataSet,AMT==0), aes(x = TIME, y = DV, group = ID)) +  
  geom_point() +
  geom_line() + 
  scale_y_log10() +
  xlim(0,24) + #limit plot to single dose rich sampling
  facet_grid(~SEX) 

# ggquickeda is a Rshiny app interface to the ggplot2 package
# run_ggquickeda(dt_InputDataSet)  
  
##==========================================================================================================
## Define Your PK Model
##==========================================================================================================
ModelName<-"TwoCptIV"

model <- pkmodel(numCompartments = 2,
                  data = dt_InputDataSet, ID = "ID", Time = "TIME", A1 = "AMT", CObs = "DV",
                  modelName = ModelName)

## Use ?pkmodel to see options, also emaxmodel, pkemaxmodel, pkindirect model, linearmodel 

## View the model 
print(model)

## Set Initial Estimates and remove Random Effect terms from Cl2 and V2
model <- model %>%
  structuralParameter(paramName = "Cl2", hasRandomEffect = FALSE) %>%
  structuralParameter(paramName = "V2", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvV", "tvCl", "tvV2", "tvCl2"), value = c(80, 6, 100, 9)) %>%
  randomEffect(effect = c("nV", "nCl"), value = c(0.1, 0.1)) %>%
  residualError(predName = "C", SD = 0.2)

## View the updated model 
print(model)

## Add covariates to your model, but don't assign to any parameters yet -- use for base diagnostics

model <- model %>%
  addCovariate(covariate = "WT") %>%
  addCovariate(covariate = "AGE") %>%
  addCovariate(covariate = "CRCL") %>%
  addCovariate(covariate = "SEX", type = "Categorical", levels = c(0, 1), 
               labels = c("male", "female"))

## View the updated model 
print(model)

## If user needs help with code, launch Model Builder shiny app
## modelui<-modelBuilderUI(dt_InputDataSet,modelName="TwoCptIVui")

##print(modelui)

## Model can also be edited directly with modelTextualUI
## modelnew<-modelTextualUI(model)

##==========================================================================================================
## Fit Your PK Model
##==========================================================================================================

## Fit the model
TwoCptIVfit <- fitmodel(model)

## If user needs help with engine settings/options, launch Model Executor Shiny app
## modelExecutorUI(model)

## View a summary of estimation results
print(TwoCptIVfit)

##==========================================================================================================
## Create Diagnostic Plots using xpose
##==========================================================================================================

## xposeNlme imports results of an NLME run into xpose database to create commonly used diagnostic plots
xp <- xposeNlme(dir = model@modelInfo@workingDir, modelName = ModelName)

##  DV vs PRED/IPRED, Residuals, -- Many more diagnostics available in xpose package
p1<-dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")  #use "template titles" to inform plots ?template_titles
p2<-dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv")
p3<-res_vs_idv(xp, type = "ps")
p4<-res_vs_pred(xp, type = "ps")

ggarrange(print(p1),print(p2),print(p3),print(p4),nrow=1,ncol=4)

#xpose is ggplot2 based -- easy to customize/enhance default diagnostics
dv_vs_pred(xp) 
dv_vs_pred(xp, facet='SEX') 
dv_vs_pred(xp, facet=SEX~cut(WT,3)) 
dv_vs_pred(xp, facet='SEX',type='ps') 
dv_vs_pred(xp, facet='SEX',type='ps') + theme_certara()

#filter example
xp %>% filter(WT>100) %>% dv_vs_pred(title='WT>100') 


#highlight outliers example (the U{2264} is unicode for <=)

xp  %>%  mutate(CWRESflag = factor(abs(CWRES) > 2.5, labels=c("|CWRES| \U{2264} 2.5", "|CWRES| > 2.5"))) %>% 
  res_vs_pred(type="ps",facets='SEX') +
  geom_point(aes(color = CWRESflag)) +
  scale_color_manual(values = c('black', 'red')) +
  scale_y_continuous(breaks = c(-2.5,0,2.5)) 

#annotations
xp %>% dv_vs_ipred(type='ps') + 
  scale_x_continuous(limits =c(0,2)) +
  scale_y_continuous(limits =c(0,2)) +
  geom_hline(yintercept=0.2, linetype="dashed") +
  geom_text(data=data.frame(x=Inf, y=0.2), aes(x=x, y=y),
            label="LLOQ = 0.2 ng/mL", hjust=1.1, vjust=-0.5, size=3) +
  theme_certara()

mygofs <- function(xp){
  p1 <- dv_vs_pred(xp) 
  p2 <- dv_vs_pred(xp, facet='SEX') 
  p3 <- dv_vs_pred(xp, facet='SEX',type='ps') 
  p4 <- dv_vs_pred(xp, facet='SEX',type='ps') + theme_certara()
  p5 <- dv_vs_pred(xp, facet='SEX',type='ps') + theme_certara() + xlim(0,2) + ylim(0,2)
  
  #filter example
  p6 <- xp %>% filter(WT>100) %>% dv_vs_pred(title='WT>100') 
  
  
  #highlight outliers example
  p7 <- xp  %>%  mutate(CWRESflag = factor(abs(CWRES) > 2.5, labels=c("|CWRES| \U{2264} 2.5", "|CWRES| > 2"))) %>% 
    res_vs_pred(type="ps",facets='SEX') +
    geom_point(aes(color = CWRESflag)) +
    scale_color_manual(values = c('black', 'red')) +
    scale_y_continuous(breaks = c(-2.5,0,2.5)) 
  
  #annotations
  p8 <- xp %>% dv_vs_ipred(type='ps') + 
    scale_x_continuous(limits =c(0,2)) +
    scale_y_continuous(limits =c(0,2)) +
    geom_hline(yintercept=0.2, linetype="dashed") +
    geom_text(data=data.frame(x=Inf, y=0.2), aes(x=x, y=y),
              label="LLOQ = 0.2 ng/mL", hjust=1.1, vjust=-0.5, size=3) +
    theme_certara()

  print(list(p1,p2,p3,p4,p5,p6,p7,p8))
  
  }

mygofs(xp)


#copy base model, add gender covariate to CL and re-run
eta_vs_cov(xp,covariate="SEX",subtitle = paste0("@nind individuals ","OFV = @ofv"))

#Use copyModel to make a copy
modelsexcl <- copyModel(model,acceptAllEffects = TRUE, modelName = "TwoCptIVsexCl")

#add gender to Cl
modelsexcl <- modelsexcl %>% 
  addCovariate(covariate = "SEX", effect = "Cl", type = "Categorical", levels = c(0, 1), 
               labels = c("male", "female"))
#check model
print(modelsexcl)

#fit model
TwoCptIVsexClfit <- fitmodel(modelsexcl)

#create xp object and run our custom diagnostics function
xp2 <- xposeNlme(dir = modelsexcl@modelInfo@workingDir, modelName = "TwoCptIVsexCl")
mygofs(xp2)

eta_vs_cov(xp2,covariate="SEX", subtitle = paste0("@nind individuals ","OFV = @ofv"))


## Results UI DEMO
## This tool is open source like all RsNLME packages and works with NLME and NONMEM output
#resultsUI(xpdb=xpdb_NLME) -- package includes built-in datasets for exploration NONMEM and NLME
#resultsUI(xpdb=xpdb_NONMEM)

#resultsUI(model=c(model,modelsexcl))

#resultsUI(model)

# Run a VPC
## Copy the final selected covariate model into a new object, and accept final parameter estimates from fitting run 
## as initial estimates for VPC simulation
TwoCptIVfinalVPC <- copyModel(modelsexcl, acceptAllEffects = TRUE, modelName = "TwoCptIVsexcl_VPC")

## View the model
print(TwoCptIVfinalVPC)

## Run VPC using the default host, default values for the relevant NLME engine arguments

TwoCptIVfinalVPCfit <- vpcmodel(model = TwoCptIVfinalVPC)

## Simulation obs output dataset predout.csv
VPCObsData <- TwoCptIVfinalVPCfit$predcheck0

## Simulation output dataset predout.csv
VPCSimData <- TwoCptIVfinalVPCfit$predout

## launch vpcResultsUI
vpcResultsUI(observed = VPCObsData, simulated = VPCSimData)


## code from vcpResultsUI
vpc <- observed(VPCObsData, x = IVAR, y = DV) %>%
  simulated(VPCSimData, y = DV) %>%
  binless(optimize = TRUE, interval = c(0L, 7L)) %>%
  vpcstats(qpred = c(0.05, 0.5, 0.95), conf.level = 0.95, quantile.type = 6)

vpcPlot <- ggplot(vpc$stats, aes(x = x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = qname, col = qname, group = qname), alpha = 0.1, col = NA) +
  geom_line(aes(y = md, col = qname, group = qname)) +
  geom_line(aes(y = y, linetype = qname), size = 1) +
  geom_point(data = vpc$obs[!(blq | alq)], aes(x = x, y = y), color = "#757D8F", size = 3L, shape = 16, alpha = 0.5, show.legend = FALSE) +
  scale_colour_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63660", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_fill_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63660", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_linetype_manual(name = "Observed Percentiles", breaks = c("q0.05", "q0.5", "q0.95"), values = c("dashed", "solid", "dashed"), labels = c("5%", "50%", "95%")) +
  guides(fill = guide_legend(order = 2), colour = guide_legend(order = 2), linetype = guide_legend(order = 1)) +
  ylab(sprintf("Observed/Simulated percentiles and associated %s%% CI", 100 * vpc$conf.level)) +
  xlab("\nTIME") +
  theme_certara() +
  theme(legend.position = "top")

vpcPlot

# Render the markdown report:
 library(rmarkdown)
 render("Report.Rmd", output_format = "word_document")
