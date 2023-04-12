# setting working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

.runninglocal <- TRUE # set flag if running local or vdi
source("comined_run_sims(test).r")
# make sure you have 
#[1] "Certara.NLME8"
#[1] ‘1.2.1’


.Nsubjects <- c(seq(5,20,by =5))   #number of subject in each formulation group
.formula = c(1,2) # Number of formulation ( 0 = Frel 1,    1 = Frel 1.05,   2= Frel 1.10,   3= Frel 1.15, 4= Frel 1.20   , 5= Frel 1.25)
.sampling = c("sparse","moderate","rich")

.numCores = ifelse(.runninglocal,6,27)
.NBOOT = ifelse(.runninglocal,3,500)#number of bootstrap datasets
.model= "nca"  #  nca   ,  truemodel   , 2CMTLINNLIN  ,   2CMTNLINOnly

FREL <- case_when(
  .formula == 0 ~ 1,
  .formula == 1 ~ 1.05,
  .formula == 2 ~ 1.1,
  .formula == 3 ~ 1.15,
  .formula == 4 ~ 1.2,
  .formula == 5 ~ 1.25,
  TRUE ~ -1
)

.numIterations = 1000 #number of iterations
.RSNLMEENGINE <- "FOCE-ELS"
.maxStepsODE = 1000


for(i in .Nsubjects){
  for(j in .formula){
    for(k in .sampling){
      sortedmodeltest <- run_sims(Nsubjects = i,
                                  formula = j ,
                                  model= .model,
                                  sampling=k,
                                  runninglocal = .runninglocal,
                                  numCores = .numCores,
                                  numIterations = .numIterations, 
                                  RSNLMEENGINE = .RSNLMEENGINE,
                                  NBOOT = .NBOOT)
    }}}
