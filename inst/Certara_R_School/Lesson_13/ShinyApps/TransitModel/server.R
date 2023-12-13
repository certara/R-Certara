# Define server logic

library(shiny)
library(Certara.RsNLME) #RsNLME package
library(ggplot2) #useful plotting package
library(dplyr)

shinyServer(function(input, output) {

output$conctimePlot <- renderPlot({

dat<-data.frame(ID=1,Time=0,Dose=input$dose,Conc=0)

  # 2: Create a simple model ----

write(paste(
"test(){
  dosepoint(Aa)
  C = A1 / V
  transit(Aa, mtt, ntr, max=50, out =  -Ka * Aa)
  deriv(A1 =  Ka * Aa  -  Cl * C)
  error(CEps=0.1)
  observe(CObs=C * ( 1 + CEps))
  stparm(Ka = tvKa * exp(nKa))
  stparm(V = tvV * exp(nV))
  stparm(Cl = tvCl * exp(nCl))
  stparm(mtt = tvmtt * exp(nmtt))
  stparm(ntr = tvntr)
  fixef( tvKa =", input$Ka_value, ")
  fixef( tvV =" , input$V_value, ")
  fixef( tvCl =" , input$Cl_value, ")
  fixef( tvmtt =" , input$mtt_value, ")
  fixef( tvntr =" , input$ntr_value, ")
  ranef(diag(nKa,nV,nCl,nmtt) = c(0.04,0.04,0.04,.04))
}"),
file="transit.mdl")

simmod <- textualmodel(mdl="transit.mdl",
                       modelName= "simmod",
                       data=dat) %>%

  colMapping(c(id = "ID", time = "Time", Aa = "Dose", CObs = "Conc"))

  # 3: Create and Run a Simulation ----
  #Define table we want as output from the simulation
  SimTable <- NlmeSimTableDef(name="SimTable.csv",
                              timesList = seq(0,48,.5),
                              variablesList = c("C", "CObs"))
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
    scale_y_continuous(limits=c(0,50))
  p1

  })


})
