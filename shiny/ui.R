library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Plače po letih"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Izberi leto in vrsto"),
      
      sliderInput(inputId ="izberi", label="Izberi leto:",
                  value=2007, min=2007, max=2015, step=1, sep=""),
      
      selectInput(inputId="izb", label ="Izberi kategorijo:",
                  choices = list ("neto", "bruto"), 
                  selected = "neto")),
    mainPanel(plotOutput(outputId ="Graf"))
  )))
