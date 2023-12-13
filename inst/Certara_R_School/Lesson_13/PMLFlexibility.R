# Overview ----
# This is code for Certara.R School Lesson 13, Hands-on Application of PML in R
# In this lesson we will look a little deeper at the PML language

# 0: Load libraries

library(Certara.RsNLME) #RsNLME package
library(Certara.RsNLME.ModelBuilder)
library(Certara.ModelResults)
library(ggplot2) #useful plotting package
library(dplyr)
library(gridExtra)

# 1: Create a Simple Dataset ----

dat<-data.frame(ID=1,Time=0,Dose=1000,Conc=0,WT=75,SEX="F",ADDL=6)

# 2: Create a simple model ----
simmod <- pkmodel(numCompartments = 1,
                   isClosedForm = FALSE,
                   absorption = "FirstOrder",
                   data = dat,
                   columnMap = FALSE,
                   modelName = "simmod") %>%
  colMapping(c(id = "ID", time = "Time", Aa = "Dose", CObs = "Conc")) %>%
  fixedEffect(effect = c("tvKa", "tvV", "tvCl"), value = c(1.5, 50, 2)) %>%
  randomEffect(effect = c("nKa", "nV", "nCl"), value = c(0.04, 0.04, 0.04))

  print(simmod)

# 3: Create and Run a Simulation ----
#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.5),
                            variablesList = c("C", "CObs"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

# * 3.1 Run the simulation ----

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

# * 3.2 Post-Process, Make a Plot ----
# extract output from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))


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
  ggtitle("C vs Time Single Dose")
  #scale_y_continuous(limits=c(0,50))

p1

#Map Sex and WT to model
simmod<-simmod %>%
  addCovariate(covariate = "WT", effect = NULL, type = "Continuous", direction = "Forward", center = "Value", centerValue = 70) %>%
  addCovariate(covariate = "SEX", effect = NULL, type = "Categorical", direction = "Forward",
               levels = c(0, 1), labels = c("M", "F"))

print(simmod)

simmod@columnMapping
simmod@covariateList

simclean("simmod")
simmodfit <- simmodel(simmod, SimSetup)


#Map ADDL dose cycle to model
simmod<-simmod %>%
  addDoseCycle(type="ADDL", colName="ADDL", name="Aa", amount="Dose",II=12)

simclean("simmod")

## Sim Table (extend to 96h)
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,96,.5),
                            variablesList = c("C", "CObs"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

simmodfit <- simmodel(simmod, SimSetup)

# * 3.2 Post-Process, Make a Plot ----
# extract output from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))


p2<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
#scale_y_continuous(limits=c(0,50))

p2

# 4: dosepoint statement options ----

# Let's Edit Our Model manually
# Recreate original single dose model
simmod <- pkmodel(numCompartments = 1,
                  isClosedForm = FALSE,
                  absorption = "FirstOrder",
                  data = dat,
                  columnMap = FALSE,
                  modelName = "simmod") %>%
  colMapping(c(id = "ID", time = "Time", Aa = "Dose", CObs = "Conc")) %>%
  fixedEffect(effect = c("tvKa", "tvV", "tvCl"), value = c(1.5, 50, 2)) %>%
  randomEffect(effect = c("nKa", "nV", "nCl"), value = c(0.04, 0.04, 0.04))

print(simmod)


simmod_lag <- editModel(simmod)
simmod_lag <- modelTextualUI(simmod)

##Edit1 - Fixed Tlag
#   dosepoint(Aa,tlag=6)

SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.5),
                            variablesList = c("C", "CObs"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

# * 4.1 Run the simulation
simclean("simmod_lag")
simmodfit <- simmodel(simmod_lag, SimSetup)

# * 4.2 Post-Process, Make a Plot
# extract output from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

p3<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
#scale_y_continuous(limits=c(0,50))

p3


#Lets include tlag as parameter
simmod_lag2 <- editModel(simmod_lag)

##Edit2 - Tlag with BSV
# test(){
#   dosepoint(Aa,tlag=Tlag)
#   C = A1 / V
#   deriv(Aa =  -  Ka * Aa)
#   deriv(A1 =  Ka * Aa  -  Cl * C)
#   error(CEps=0.1)
#   observe(CObs=C * ( 1 + CEps))
#   stparm(Ka = tvKa * exp(nKa))
#   stparm(V = tvV * exp(nV))
#   stparm(Cl = tvCl * exp(nCl))
#   stparm(Tlag = tvTlag * exp(nTlag))
#   fixef( tvKa = c(,1.5,))
#   fixef( tvV = c(,50,))
#   fixef( tvCl = c(,2,))
#   fixef( tvTlag = c(,6,))
#   ranef(diag(nKa,nV,nCl,nTlag) = c(0.04,0.04,0.04,.1))
# }


# Run the simulation
simclean("simmod_lag2")
simmodfit <- simmodel(simmod_lag2, SimSetup)

#Post-Process, Make a Plot
# extract output from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

p4<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
#scale_y_continuous(limits=c(0,50))

p4


# 5: Built in Advanced Absorption Models ----
# * 5.1 Transit Model ----

#Lets edit our model to a transit compartment model

#Lets include tlag as parameter
simmod_transit <- editModel(simmod)

##Edit3 - Transit model
# test(){
#   dosepoint(Aa)
#   C = A1 / V
#   transit(Aa, mtt, ntr, max=50, out =  -Ka * Aa)
#   deriv(A1 =  Ka * Aa  -  Cl * C)
#   error(CEps=0.1)
#   observe(CObs=C * ( 1 + CEps))
#   stparm(Ka = tvKa * exp(nKa))
#   stparm(V = tvV * exp(nV))
#   stparm(Cl = tvCl * exp(nCl))
#   stparm(mtt = tvmtt * exp(nmtt))
#   stparm(ntr = tvntr)
#   fixef( tvKa = c(,1.5,))
#   fixef( tvV = c(,50,))
#   fixef( tvCl = c(,2,))
#   fixef( tvmtt = c(,5,))
#   fixef( tvntr = c(,34,))
#   ranef(diag(nKa,nV,nCl,nmtt) = c(0.04,0.04,0.04,.04))
# }

# Run the simulation
simclean("simmod_transit")
simmodfit <- simmodel(simmod_transit, SimSetup)

#Post-Process, Make a Plot
# extract output from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

p5<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
#scale_y_continuous(limits=c(0,50))

p5


#* 5.2 Delay Absorption Models ----
simmod_delay <- pkmodel(numCompartments = 1,
        absorption = "Gamma",
        data = dat,
        columnMap = FALSE,
        modelName = "Gamma") %>%
  colMapping(c(id="ID",time = "Time", A1 = "Dose", CObs = "Conc")) %>%
  fixedEffect(effect = c("tvMeanDelayTime", "tvShapeParamMinusOne","tvV", "tvCl"),
              value = c(8, 8, 50, 2)) %>%
  randomEffect(effect = c("nMeanDelayTime","nShapeParamMinusOne", "nV", "nCl"),
               value = c(0.04, 0.04, 0.04, 0.04))

print(simmod_delay)

# Run the simulation
simclean("simmod_delay")
simmodfit <- simmodel(simmod_delay, SimSetup)

#Post-Process, Make a Plot
# extract output from the simulation fit object
SimTableout <- simmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

p6<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
#scale_y_continuous(limits=c(0,50))

p6

#6. Urine Model doafter observation statement ----

urinedat <- read.csv("urinedat.csv")

urinemod <- textualmodel(modelName="urinemod", data=urinedat, mdl="urinemod.txt")

urinemod@isPopulation <- FALSE
print(urinemod)

urinemod <- urinemod %>%
  colMapping(id=ID,time=Time,A1=Dose,CObs=Conc,A0Obs=UrineAmtCum)

urinemodfit <- fitmodel(urinemod, method="Naive-Pooled")

p7 <- urinemodfit$residuals %>%
  ggplot(aes(x=IVAR, y=IPRED)) +
            geom_line() +
            geom_point(aes(y=DV)) +
            facet_wrap(~ObsName, scales="free") +
            theme_bw() +
            xlab("Time (hr)") +
            ylab("IPRED (mg/L)") +
            ggtitle("Cumulative Urine Amt and Conc vs Time Single Dose")
p7

urinemodfit$theta

#Now lets fit aliquot data, UrineAmt column, with doafter statement

urinemod2 <- textualmodel(modelName="urinemod2", data=urinedat, mdl="urinemod2.txt")
urinemod2@isPopulation <- FALSE
print(urinemod2)

urinemod2 <- urinemod2 %>%
  colMapping(id=ID,time=Time,A1=Dose,CObs=Conc,A0Obs=UrineAmt)

urinemod2fit <- fitmodel(urinemod2, method="Naive-Pooled")

p8 <- urinemod2fit$residuals %>%
  ggplot(aes(x=IVAR, y=IPRED)) +
  geom_line() +
  geom_point(aes(y=DV)) +
  facet_wrap(~ObsName, scales="free") +
  theme_bw() +
  xlab("Time (hr)") +
  ylab("IPRED (mg/L)") +
  ggtitle("Urine Amt by Aliquot and Conc vs Time Single Dose")
p8

urinemodfit$theta
urinemod2fit$theta


#7: Sequence Statement Examples ----
# * 7.1 Initialize state variables ----

seqmod1 <-editModel(simmod)
#Add to model
# sequence{
#   A1=500
# }

print(seqmod1)


#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.5),
                            variablesList = c("C", "CObs"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))


simclean("seqmod1")
seqmod1fit <- simmodel(seqmod1, SimSetup)

# Post-Process, Make a Plot
# extract output from the simulation fit object
SimTableout <- seqmod1fit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))


p10<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
#scale_y_continuous(limits=c(0,50))


gridExtra::grid.arrange(p1 + scale_y_continuous(limits=c(0,40)),p10, nrow=1)


# * 7.2 create Covariates on the fly for simulation ----
covgenmod <- textualmodel(modelName="covgenmod",data=dat,mdl="covgenmod.txt")
print(covgenmod)

covgenmod <- covgenmod %>%
  colMapping(c(id = "ID", time = "Time", A1 = "Dose", CObs = "Conc"))

#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.5),
                            variablesList = c("C", "CObs","SEX","WT","CRCL","AGE"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))
simclean("covgenmod")
covgenmodfit <- simmodel(covgenmod, SimSetup)

# Post-Process, Make a Plot
# extract output from the simulation fit object
SimTableout <- covgenmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))


p11<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  facet_wrap(~SEX) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
p11

# * 7.3 Sequence with Sleep statement ----
seqmod2 <-editModel(simmod)
#Add to model
# sequence{
#   sleep(6)
#   A1=A1+500
# }

print(seqmod2)


#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.5),
                            variablesList = c("C", "CObs"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))


simclean("seqmod2")
seqmod2fit <- simmodel(seqmod2, SimSetup)

# Post-Process, Make a Plot
# extract output from the simulation fit object
SimTableout <- seqmod2fit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))


p12<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose")
p12


# * 7.4 Enterohepatic Recycling Example ----

# Read in final dataset from lesson 2 ----
finaldat <- readRDS("../Lesson_2/finaldat.RDS")
#cut down to dose records for 10 subjects
simdat <- finaldat %>%
  mutate(SS = if_else(AMT>0,1,0)) %>%
  filter(ID %in% 1:10)

EHRmod <- textualmodel(modelName = "EHRmod", data=simdat, mdl="EHRmodel.txt")
EHRmod <- EHRmod %>%
            colMapping(c(id = "ID", time = "TIME", Aa = "AMT", CObs = "CONC"))

print(EHRmod)

#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = seq(0,48,.5),
                            variablesList = c("C", "CObs","Aa", "GB"))
## Simulation setup
SimSetup <- NlmeSimulationParams(numReplicates = 10,
                                 seed = 1234,
                                 simulationTables = c(SimTable))

# Run the simulation

simclean("EHRmod")
EHRmodfit <- simmodel(EHRmod, SimSetup)

# Post-Process, Plot simulation results
# extract output from the simulation fit object
SimTableout <- EHRmodfit$SimTable %>%
  rename("Replicate"="# repl") %>%   #rename repl column to Replicate
  mutate(id=as.numeric(paste0(Replicate,id5)))

#Make a plot

p13<-ggplot(SimTableout, aes(x=time, y=C,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("C (mg/L)") +
  ggtitle("C vs Time Single Dose EHR Model") +
  #geom_hline(yintercept=mean(SimTableout$C[SimTableout$time==24]), color='blue') +
  #scale_y_continuous(limits=c(0,120)) +
  scale_x_continuous(breaks=seq(0,50,2)) +
  theme_certara()
p13

p14<-ggplot(SimTableout, aes(x=time, y=Aa,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("Aa (mg)") +
  ggtitle("Aa Amount vs Time Single Dose EHR Model") +
  #geom_hline(yintercept=mean(SimTableout$C[SimTableout$time==24]), color='blue') +
  #scale_y_continuous(limits=c(0,120)) +
  scale_x_continuous(breaks=seq(0,50,2)) +
theme_certara()
p14

p15<-ggplot(SimTableout, aes(x=time, y=GB,group=factor(id))) +
  geom_line(alpha=.05) +
  #scale_y_log10() +
  stat_summary(aes(group=NULL),fun=median,geom='line', colour = "red", size = 1.2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.05)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  stat_summary(aes(group=NULL),fun = "quantile", fun.args = list(probs = c(0.95)),
               geom = "line", colour = "red", size = 1.2, lty=2, alpha=.6) +
  xlab("Time (hr)") +
  ylab("GB Amount (mg)") +
  ggtitle("GB vs Time Single Dose EHR Model") +
  #geom_hline(yintercept=mean(SimTableout$C[SimTableout$time==24]), color='blue') +
  #scale_y_continuous(limits=c(0,120)) +
  scale_x_continuous(breaks=seq(0,50,2)) +
theme_certara()
p15

gridExtra::grid.arrange(p13, p14, p15)
