
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

  selectizeInput("moldingInput", label = ("Molding Technology Options"),
                 choices = NULL,
                 selected = "",
                 multiple = FALSE
                 ),
  mainPanel(
    textOutput("EnergyNum")
  )
  )
))
