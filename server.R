
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)
source("math.R")

Data_Mold = read_csv("data/Data_Mold.csv")
moldnames = Data_Mold$Name_Mold
moldenergy = Data_Mold$Energy_Mold
moldfrac = Data_Mold$Frac_Fiber
moldyield = Data_Mold$Yield_Mold

shinyServer(function(input, output, session) {
  
  output$partname1 <- renderText({paste("Part Name:",input$name1)})
  output$partweight1 <- renderText({paste("Part Weight:",input$finalweight1, "kg")})
  output$partname2 <- renderText({paste("Part Name:",input$name2)})
  output$partweight2 <- renderText({paste("Part Weight:",input$finalweight2, "kg")})
  
  # Mold1 ----
  # Make List for Box
  updateSelectizeInput(session, 'moldingInput1',
                    choices = moldnames,
                    selected = "",
                    server = TRUE)
 # Associate Name value with mold type name
  moldnamevalue1 <- eventReactive(input$moldingInput1, {
    moldnames[moldnames %in% input$moldingInput1]
  })
  output$moldname1 <- renderText(moldnamevalue1())
  
   # Associate Energy value with mold type name
  moldenergyvalue1 <- eventReactive(input$moldingInput1, {
    moldenergy[moldnames %in% input$moldingInput1]
  })
  output$EnergyNum1 <- renderText(moldenergyvalue1())

  # Associate Fiber Frac value with mold type name
   moldfracvalue1 <- eventReactive(input$moldingInput1,{
    moldfrac[moldnames %in% input$moldingInput1]
  })
  output$moldfracNum1 <- renderText(moldfracvalue1())
  
    # Associate Layup yield with mold type name
  moldyieldvalue1 <- eventReactive(input$moldingInput1,{
    moldyield[moldnames %in% input$moldingInput1]
  })
  output$moldyieldNum1 <- renderText(moldyieldvalue1())
  
  # Mold2 ----
  updateSelectizeInput(session, 'moldingInput2',
                       choices = moldnames,
                       selected = "",
                       server = TRUE)
  
  # Associate Name value with mold type name
  moldnamevalue2 <- eventReactive(input$moldingInput2, {
    moldnames[moldnames %in% input$moldingInput2]
  })
  output$moldname2 <- renderText(moldnamevalue2())
  
  moldenergyvalue2 <- eventReactive(input$moldingInput2, {
    moldenergy[moldnames %in% input$moldingInput2]
  })
  output$EnergyNum2 <- renderText(moldenergyvalue2())
  # Associate Fiber Frac value with mold type name
  moldfracvalue2 <- eventReactive(input$moldingInput2,{
    moldfrac[moldnames %in% input$moldingInput2]
  })
  output$moldfracNum2 <- renderText(moldfracvalue2())
  
  # Associate Layup yield with mold type name
  moldyieldvalue2 <- eventReactive(input$moldingInput2,{
    moldyield[moldnames %in% input$moldingInput2]
  })
  output$moldyieldNum2 <- renderText(moldyieldvalue2())
 

  
  
  
  
  
  
  })
