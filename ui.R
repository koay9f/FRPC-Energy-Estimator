
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)

shinyUI(fluidPage(

  # Application title
  titlePanel("CFRP Energy Use Estimation Tool"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    img(scr = "CFRP Scope.png", height = 150),
    #WHY ISN'T THE PICTURE APPEARING?
    p("This tool is being developed by ORNL to provide CFRP researchers and manufacturers the ability to quickly estimate the embodied energy use of their CFRP manufactuing process and compare it to other processes")
    
      ),
  

  mainPanel(
    
    selectizeInput("moldingInput", label = ("Molding Technology Options"),
                   choices = NULL,
                   selected = "",
                   multiple = FALSE
    ),
    h3("Embodied energy of molding technology:"),
    h3(textOutput("EnergyNum"),"MJ/kg")
  ))))
