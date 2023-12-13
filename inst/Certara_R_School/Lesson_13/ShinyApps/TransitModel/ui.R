# ui.R
library(shiny)
library(Certara.RsNLME) #RsNLME package
library(ggplot2) #useful plotting package
library(dplyr)


# Define UI for application
shinyUI(fluidPage(

  # Application title

  br(), img(src="CertaraLogo.png", height = 100, width = 200),
  titlePanel("Drug XYZ"),


  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(


       numericInput("dose",
                   label = h5("Enter Dose (mg)"),
                   value = 1000),

       sliderInput("Ka_value", label = h5("Enter Ka"),
                   min = 0, max = 10, step=0.1, value = 1.5),

       sliderInput("Cl_value", label = h5("Enter Cl"),
                   min = 0, max = 10, step=0.1, value = 2),

       sliderInput("V_value", label = h5("Enter V"),
                   min = 0, max = 100, value = 50),

       sliderInput("ntr_value", label = h5("Enter ntr"),
                   min = 0, max = 50, step=1, value = 5),

       sliderInput("mtt_value", label = h5("Enter mtt"),
                   min = 0, max = 48, step=0.5, value = 11)

  ),


    # Show a plot of resulting conc time profile
    mainPanel(
      plotOutput("conctimePlot")

    )
  )
))
