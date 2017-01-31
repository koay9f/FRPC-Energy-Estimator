
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
Data_Int = read_csv("data/Data_Int.csv")
intnames = Data_Int$Name_Int
intenergy = Data_Int$Energy_Int
intscrap = Data_Int$Scrap_Int


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
  moldnamefetch1 <- eventReactive(input$moldingInput1, {
    moldnames[moldnames %in% input$moldingInput1]
  })
  output$moldname1 <- renderText(moldnamefetch1())
  
   # Associate Energy value with mold type name
  moldenergyfetch1 <- eventReactive(input$moldingInput1, {
    moldenergy[moldnames %in% input$moldingInput1]
  })
  output$EnergyNum1 <- renderText(moldenergyfetch1())

  # Associate Fiber Frac value with mold type name
   moldfracfetch1 <- eventReactive(input$moldingInput1,{
    moldfrac[moldnames %in% input$moldingInput1]
  })
  output$moldfracNum1 <- renderText(moldfracfetch1())
  
    # Associate Molding yield with mold type name
  moldyieldfetch1 <- eventReactive(input$moldingInput1,{
    moldyield[moldnames %in% input$moldingInput1]
  })
  output$moldyieldNum1 <- renderText(moldyieldfetch1())
  
  # Mold2 ----
  updateSelectizeInput(session, 'moldingInput2',
                       choices = moldnames,
                       selected = "",
                       server = TRUE)
  
  # Associate Name value with mold type name
  moldnamefetch2 <- eventReactive(input$moldingInput2, {
    moldnames[moldnames %in% input$moldingInput2]
  })
  output$moldname2 <- renderText(moldnamefetch2())
  
  moldenergyfetch2 <- eventReactive(input$moldingInput2, {
    moldenergy[moldnames %in% input$moldingInput2]
  })
  output$EnergyNum2 <- renderText(moldenergyfetch2())
  # Associate Fiber Frac value with mold type name
  moldfracfetch2 <- eventReactive(input$moldingInput2,{
    moldfrac[moldnames %in% input$moldingInput2]
  })
  output$moldfracNum2 <- renderText(moldfracfetch2())
  
  # Associate Layup yield with mold type name
  moldyieldfetch2 <- eventReactive(input$moldingInput2,{
    moldyield[moldnames %in% input$moldingInput2]
  })
  output$moldyieldNum2 <- renderText(moldyieldfetch2())
 

  
  
  
  
  
  
  
  # Int1 ----
  
  #Make List for box
  updateSelectizeInput(session, 'IntInput1',
                       choices = intnames,
                       selected = "",
                       server = TRUE)
  # Associate Name with Int type name
  intnamefetch1 <- eventReactive(input$intInput1, {
    intnames[intnames %in% input$intInput1]
  })
  output$intname1 <- renderText(intnamefetch1())
  
  # Associate Energy value with int type name
  intenergyfetch1 <- eventReactive(input$intInput1, {
    intenergy[moldnames %in% input$intInput1]
  })
  output$intEnergyNum1 <- renderText(intenergyfetch1())
  
  # Associate layup scrap with int type name
  intscrapfetch1 <- eventReactive(input$intInput1,{
    intscrap[intnames %in% input$intInput1]
  })
  output$intscrapNum1 <- renderText(intscrapfetch1())
  
  # Int2 ----
  
  #Make List for box
  updateSelectizeInput(session, 'IntInput2',
                       choices = intnames,
                       selected = "",
                       server = TRUE)
  # Associate Name with Int type name
  intnamefetch2 <- eventReactive(input$intInput2, {
    intnames[intnames %in% input$intInput2]
  })
  output$intname2 <- renderText(intnamefetch2())
  
  # Associate Energy value with int type name
  intenergyfetch2 <- eventReactive(input$intInput2, {
    intenergy[moldnames %in% input$intInput2]
  })
  output$intEnergyNum2 <- renderText(intenergyfetch2())
  
  # Associate layup scrap with int type name
  intscrapfetch2 <- eventReactive(input$intInput2,{
    intscrap[intnames %in% input$intInput2]
  })
  output$intscrapNum2 <- renderText(intscrapfetch2())
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  })
