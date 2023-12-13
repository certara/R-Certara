# Define server logic

library(shiny)
library(Certara.RsNLME)
library(ggplot2)
library(magrittr)
library(dplyr)

shinyServer(function(input, output) {

output$conctimePlot <- renderPlot({

  dat<-data.frame(ID=1,Time=0,Dose=input$dose,Conc=0)

  # 2: Create a simple model ----
  weibullmodel <- pkmodel(numCompartments = 1,
                    absorption = "Weibull",
                    data = dat,
                    columnMap = FALSE,
                    modelName = "Weibull") %>%
  colMapping(c(id="ID",time = "Time", A1 = "Dose", CObs = "Conc")) %>%
  fixedEffect(effect = c("tvMeanDelayTime", "tvShapeParamMinusOne","tvV", "tvCl"),
  value = c(input$MeanDelayTime, input$ShapeParamMinusOne, 50, 2)) %>%
  randomEffect(effect = c("nMeanDelayTime","nShapeParamMinusOne", "nV", "nCl"),
               value = c(0.04, 0.04, 0.04, 0.04))

  gammamodel <- pkmodel(numCompartments = 1,
               absorption = "Gamma",
               data = dat,
               columnMap = FALSE,
               modelName = "Gamma") %>%
    colMapping(c(id="ID",time = "Time", A1 = "Dose", CObs = "Conc")) %>%
    fixedEffect(effect = c("tvMeanDelayTime", "tvShapeParamMinusOne","tvV", "tvCl"),
                value = c(input$MeanDelayTime, input$ShapeParamMinusOne, 50, 2)) %>%
    randomEffect(effect = c("nMeanDelayTime","nShapeParamMinusOne", "nV", "nCl"),
                 value = c(0.04, 0.04, 0.04, 0.04))

  igmodel <- pkmodel(numCompartments = 1,
                     absorption = "InverseGaussian",
                     data = dat,
                     columnMap = FALSE,
                     modelName = "InverseGaussian") %>%
    colMapping(c(id="ID",time = "Time", A1 = "Dose", CObs = "Conc")) %>%
    fixedEffect(effect = c("tvMeanDelayTime", "tvShapeParam","tvV", "tvCl"),
                value = c(input$MeanDelayTime, input$ShapeParamMinusOne+1, 50, 2)) %>%
    randomEffect(effect = c("nMeanDelayTime","nShapeParam", "nV", "nCl"),
                 value = c(0.04, 0.04, 0.04, 0.04))
    ###########  Run Simulation ###############

  SimTableCObs <- NlmeSimTableDef(name = "SimTableCObs.csv",
                                  # timesList = seq(0,input$xmax,0.1),  #sequence 0 to 24 hours by .5 increment
                                  timesList = seq(0,48,.5),
                                  variablesList = c("C", "CObs"))

  SimSetup <- NlmeSimulationParams(numReplicates = 1,
                                   seed = 1234,
                                   simulationTables = c(SimTableCObs))

  if (file.exists("Weibull")) {
    unlink("Weibull",recursive = T)
    print("File is deleted..")
  } else{
    print("File not exists..")
  }

  if (file.exists("Gamma")) {
    unlink("Gamma",recursive = T)
    print("File is deleted..")
  } else{
    print("File not exists..")
  }

  if (file.exists("InverseGaussian")) {
    unlink("InverseGaussian",recursive = T)
    print("File is deleted..")
  } else{
    print("File not exists..")
  }


  weibullmodelfit <- simmodel(weibullmodel, SimSetup)
  gammamodelfit <- simmodel(gammamodel, SimSetup)
  igmodelfit <- simmodel(igmodel, SimSetup)

  weibullsimdat <-  weibullmodelfit$SimTableCObs %>%
    as.data.frame()
  gammasimdat <-  gammamodelfit$SimTableCObs %>%
    as.data.frame()
  igsimdat <-  igmodelfit$SimTableCObs %>%
    as.data.frame()

  #######  allow scale change on shiny app
  # txmax<-as.numeric(input$txmax)

par(mfrow=c(2,2))

plot(weibullsimdat[,'time'],weibullsimdat[,'C'],
       log=input$yaxlog,
       xlab="Time (h)",
       #xlim=c(0,txmax),
       #xlim=c(0,24),
       #ylim=c(0,3),
       #xaxp=c(0,240,20),
       ylab="Plasma Concentration (mg/L)",
       type='l',
       main="Weibull")

plot(gammasimdat[,'time'],gammasimdat[,'C'],
     log=input$yaxlog,
     xlab="Time (h)",
     #xlim=c(0,txmax),
     #xlim=c(0,24),
     #ylim=c(0,3),
     #xaxp=c(0,240,20),
     ylab="Plasma Concentration (mg/L)",
     type='l',
     main="Gamma")

plot(igsimdat[,'time'],igsimdat[,'C'],
     log=input$yaxlog,
     xlab="Time (h)",
     #xlim=c(0,txmax),
     #xlim=c(0,24),
     #ylim=c(0,3),
     #xaxp=c(0,240,20),
     ylab="Plasma Concentration (mg/L)",
     type='l',
     main="InverseGaussian")


  })


})
