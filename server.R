
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)

Data_Mold = read_csv("data/Data_Mold.csv")
moldnames = Data_Mold$Name_Mold
moldenergy = Data_Mold$Energy_Mold

shinyServer(function(input, output, session) {
  
  
  updateSelectizeInput(session, 'moldingInput1',
                    choices = moldnames,
                    selected = "",
                    server = TRUE)
  moldenergyvalue <- eventReactive(input$moldingInput1, {
    moldenergy[moldnames %in% input$moldingInput1]
  })
  output$EnergyNum <- renderText(moldenergyvalue())
})
