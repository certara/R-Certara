# ui.R
library(shiny)
library(deSolve)


# Define UI for application
shinyUI(fluidPage(

  # Application title

  br(), img(src="Certara.png", height = 100, width = 175),
  titlePanel("PML Delayed Absorption Distributions"),


  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(


      # radioButtons("delaytype", h5("Select Delay Distribution"),
      #              c("Weibull" = "Weibull",
      #                "Gamma" = "Gamma",
      #                "InverseGaussian" = "InverseGaussian")),

      # radioButtons("overlay", h5("Overlay?"),
      #              c("Yes" = "yes",
      #                "No" = "no")),

      numericInput("dose",
                   label = h5("Enter Dose (mg)"),
                   value = 1000),

      numericInput("Cl_value", label = h5("Enter Cl"),
                  value = 2),

      numericInput("V_value", label = h5("Enter V"),
                 value = 50),

      numericInput("MeanDelayTime",
                    label = h5("Enter MeanDelayTime"),
                    value = 8),

       numericInput("ShapeParamMinusOne",
                    label = h5("Enter ShapeParam"),
                    value = 8),

       # sliderInput("xmax", label = h5("Select Simulation Duration (h)"),
       #             min = 0, max = 240, value = 24,step=6,width="100%"),
       #
       # sliderInput("txmax", label = h5("Select Time Axis Max (h)"),
       #             min = 0, max = 240, value = 24,step=6,width="100%"),

       radioButtons("yaxlog", h5("Semi-Log?"),
                    c("No" = "",
                      "Yes" = "y"))

      ),


    # Show a plot of resulting conc time profile
    mainPanel(
      plotOutput("conctimePlot")

    )
  )
))
