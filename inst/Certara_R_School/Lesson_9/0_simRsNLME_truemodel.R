library(Certara.RsNLME)
library(tidyverse)
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# number of subjects per each formulation
NIDs <- 50
#Dataset Prep
#RsNLME only requires dose (input) events -
#sim times are specified in call to simmodel
#6000 unique IDs, form 0 to 5, Frel by Form
simdat <-
  crossing(formulation = c(0, 1, 2, 3, 4, 5), id = 1:NIDs) %>%    ###NOTE reduced from 1K to 50
  mutate(id = id + NIDs * formulation) %>%
  mutate(
    amtorig = 420000,
    dur = 1.5,
    time = 0,
    CObs = 0
  ) %>% # amount original highest dose with nonlinear pk
  mutate(
    Frel = case_when(
      formulation  == 0 ~ 1,
      formulation  == 1 ~ 1.05,
      formulation  == 2 ~ 1.10,
      formulation  == 3 ~ 1.15,
      formulation  == 4 ~ 1.20,
      formulation  == 5 ~ 1.25,
      TRUE ~ -1
    ),
    amt = amtorig * Frel
  ) %>%  # amount after multiplying by Frel
  select(id, time, amt, dur, CObs, formulation, Frel, amtorig)

# build textual model using shiny
#  Certara.RsNLME.ModelBuilder::modelBuilderUI(simdat)

## to delete the previous simulated data and
## permit to simulate a new one with the same name

file_name <- "truemodelSimOut"
# code to make simmodel work
if (file.exists(file_name)) {
  unlink(file_name, recursive = T)
  print("File is deleted..")
} else{
  print("File not exists..")
}
# Create model Using truemodel text modelfile
truemodel <-
  textualmodel(modelName = "truemodelSimOut",
               data = simdat,
               mdl = "truemodel.txt")

print(truemodel)

## Mapping the model variable to dataset variables
truemodel <- truemodel %>%
  colMapping(A1 = amt, FORM = formulation)

print(truemodel)

#Now we set up simulation

#Define table we want as output from the simulation
SimTable <- NlmeSimTableDef(name="SimTable.csv",
                            timesList = c(0,1.5,seq(24,3480,24)),
                            variablesList = c("Ke","Km","V1","V2","V3",
                                              "C","C2","C3","CUMAUC","CObs",
                                              "A1","A2","A3","FORM"))
## Simulation setup
SimSetup <- NlmeSimulationParams(
  numReplicates = 1,
  seed = 1234,
  simulationTables = c(SimTable)
)
## Local Host Configurations: 

localMPIHost <- NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
                                 installationDirectory = Sys.getenv("INSTALLDIR"),
                                 parallelMethod = NlmeParallelMethod("multicore"),
                                 hostName = "Local_MultiCore",
                                 numCores = 6)
## Run the model


tictoc::tic() ## for calculation of the time needed for execution
truemodelsimfit <-
  simmodel(truemodel, SimSetup,hostPlatform = localMPIHost)
tictoc::toc() ## for calculation of the time needed for execution

## Postprocess output
SimTableout <- truemodelsimfit$SimTable %>%
  rename("Replicate" = "# repl") %>% #rename repl column to Replicate
  rename("id" = "id5")

#merge with original dataset, create variables to match your rxode output
simdat_amt <- simdat %>%
  select(id, amt, time, formulation, Frel) %>%
  mutate(evid = 1, cmt = 1)

SimTableout <- SimTableout %>%
  left_join(simdat_amt,by=c('id','time')) %>%
  select(id,evid,cmt,amt,time,Ke,Km,V1,V2,V3,
         C,C2,C3,CUMAUC,CObs,
         A1,A2,A3,FORM) %>% 
  rename(formulation= FORM) %>% 
  mutate(
    Frel = case_when(
      formulation  == 0 ~ 1,
      formulation  == 1 ~ 1.05,
      formulation  == 2 ~ 1.10,
      formulation  == 3 ~ 1.15,
      formulation  == 4 ~ 1.20,
      formulation  == 5 ~ 1.25,
      TRUE ~ -1
    ))
  

# plot the simulated data to check
ggplot(SimTableout, aes(x=time, y=C, group=factor(formulation),color=factor(formulation))) +
  geom_line(aes(group=id),alpha=.05) +
  stat_summary(aes(group = formulation),fun=median,geom='line', size = .5, alpha=.6) +
  facet_grid(~formulation) +
  xlab("Time (hr)") +
  ylab("C (unit)") +
  labs(color = "formulation") +
  ggtitle("C vs Time by formulation") +
  scale_y_log10()

#simtable to be saved to use for resmpling/fitting
fwrite(SimTableout,"Allsims.df.csv",row.names = FALSE)

