
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)
source("math.R")

#Name Data_Mold columns
Data_Mold = read_csv("data/Data_Mold.csv")
moldnames = Data_Mold$Name_Mold
moldenergy = Data_Mold$Energy_Mold
moldfrac = Data_Mold$Frac_Fiber
moldyield = Data_Mold$Yield_Mold

#Name Data_Int columns
Data_Int = read_csv("data/Data_Int.csv")
intnames = Data_Int$Name_Int
intenergy = Data_Int$Energy_Int
intscrap = Data_Int$Scrap_Int

#Name Data_Fiber columns
Data_Fiber = read.csv("data/Data_Fiber.csv")
fibernames = Data_Fiber$Name_Fiber
fiberenergy = Data_Fiber$Energy_Fiber

#Name Data_MatrixM columns
Data_MatrixM = read.csv("data/Data_Matrix.csv")
matrixnames = Data_MatrixM$Name_Matrix
matrixenergy = Data_MatrixM$Energy_Matrix
matrixtype = Data_MatrixM$Type_Matrix
Data_primatrix <- subset(Data_MatrixM, Type_Matrix == "Matrix")
primatrixnames = Data_primatrix$Name_Matrix
Data_othermatrix <- subset(Data_MatrixM, Type_Matrix != "Insert")
othermatrixnames = Data_othermatrix$Name_Matrix
Data_inserts <- subset(Data_MatrixM, Type_Matrix == "Insert")
insertsnames = Data_inserts$Name_Matrix

shinyServer(function(input, output, session) {
  
  # General Definations ----
  # (none) = first use a = int b = matrix c = mold d= summary e = results z = calcs
  output$partname1e <- output$partname1d <- output$partname1c <- output$partname1b <- output$partname1a <- output$partname1 <- renderText({paste("Part Name:",input$name1)})
  output$partweight1z <- output$partweight1e <- output$partweight1d <- output$partweight1c <- output$partweight1b <- output$partweight1a <- output$partweight1 <- renderText({paste("Part Weight:",input$finalweight1, "kg")})
  output$partname2e <-output$partname2d <-output$partname2c <-output$partname2b <-output$partname2a <- output$partname2 <- renderText({paste("Part Name:",input$name2)})
  output$partweight2z <-  output$partweight2e <- output$partweight2d <- output$partweight2c <- output$partweight2b <- output$partweight2a <- output$partweight2 <- renderText({paste("Part Weight:",input$finalweight2, "kg")})
  
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
output$moldname1e <- output$moldname1d <- output$moldname1c <- output$moldname1b <- output$moldname1a <- output$moldname1 <- renderText(moldnamefetch1())
  
   # Associate Energy value with mold type name
  moldenergyfetch1 <- eventReactive(input$moldingInput1, {
    moldenergy[moldnames %in% input$moldingInput1]
  })
  output$EnergyNum1z <-output$EnergyNum1 <- renderText(moldenergyfetch1())

  # Associate Fiber Frac value with mold type name
   moldfracfetch1 <- eventReactive(input$moldingInput1,{
    moldfrac[moldnames %in% input$moldingInput1]
  })
   output$moldfracNum1z <-output$moldfracNum1 <- renderText(moldfracfetch1())
  
    # Associate Molding yield with mold type name
  moldyieldfetch1 <- eventReactive(input$moldingInput1,{
    moldyield[moldnames %in% input$moldingInput1]
  })
  output$moldyieldNum1z <-output$moldyieldNum1 <- renderText(moldyieldfetch1())
  
  # Mold2 ----
  updateSelectizeInput(session, 'moldingInput2',
                       choices = moldnames,
                       selected = "",
                       server = TRUE)
  
  # Associate Name value with mold type name
  moldnamefetch2 <- eventReactive(input$moldingInput2, {
    moldnames[moldnames %in% input$moldingInput2]
  })
  output$moldname2z <-output$moldname2a <- output$moldname2 <- renderText(moldnamefetch2())
  
  moldenergyfetch2 <- eventReactive(input$moldingInput2, {
    moldenergy[moldnames %in% input$moldingInput2]
  })
  output$EnergyNum2z <- output$EnergyNum2 <- renderText(moldenergyfetch2())
  # Associate Fiber Frac value with mold type name
  moldfracfetch2 <- eventReactive(input$moldingInput2,{
    moldfrac[moldnames %in% input$moldingInput2]
  })
  output$moldfracNum2z <-output$moldfracNum2 <- renderText(moldfracfetch2())
  
  # Associate Layup yield with mold type name
  moldyieldfetch2 <- eventReactive(input$moldingInput2,{
    moldyield[moldnames %in% input$moldingInput2]
  })
  output$moldyieldNum2z <-output$moldyieldNum2 <- renderText(moldyieldfetch2())

  # Fiber1 ----
  # Make List for Box
  updateSelectizeInput(session, 'fiberInput1',
                       choices = fibernames,
                       selected = "",
                       server = TRUE)
  # Associate Name value with fiber type name
  fibernamefetch1 <- eventReactive(input$fiberInput1, {
    fibernames[fibernames %in% input$fiberInput1]
  })
  output$fibername1 <- renderText(fibernamefetch1())
  
  # Associate Energy value with fiber type name
  fiberenergyfetch1 <- eventReactive(input$fiberInput1, {
    fiberenergy[fibernames %in% input$fiberInput1]
  })
  output$fiberEnergyNum1z <-output$fiberEnergyNum1 <- renderText(fiberenergyfetch1())
  # Fiber2 ----
  # Make List for Box
  updateSelectizeInput(session, 'fiberInput2',
                       choices = fibernames,
                       selected = "",
                       server = TRUE)
  # Associate Name value with fiber type name
  fibernamefetch2 <- eventReactive(input$fiberInput2, {
    fibernames[fibernames %in% input$fiberInput2]
  })
  output$fibername2 <- renderText(fibernamefetch2())
  
  # Associate Energy value with fiber type name
  fiberenergyfetch2 <- eventReactive(input$fiberInput2, {
    fiberenergy[fibernames %in% input$fiberInput2]
  })
  output$fiberEnergyNum2z <-output$fiberEnergyNum2 <- renderText(fiberenergyfetch2())
  
  # Int1 ----
  
  #Make List for box
  updateSelectizeInput(session, 'intInput1',
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
    intenergy[intnames %in% input$intInput1]
  })
  output$intEnergyNum1z <-output$intEnergyNum1 <- renderText(intenergyfetch1())
  
  # Associate layup scrap with int type name
  intscrapfetch1 <- eventReactive(input$intInput1,{
    intscrap[intnames %in% input$intInput1]
  })
  output$intscrapNum1z <-output$intscrapNum1 <- renderText(intscrapfetch1())
  
  # Int2 ----
  
  #Make List for box
  updateSelectizeInput(session, 'intInput2',
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
    intenergy[intnames %in% input$intInput2]
  })
  output$intEnergyNum2z <-output$intEnergyNum2 <- renderText(intenergyfetch2())
  
  # Associate layup scrap with int type name
  intscrapfetch2 <- eventReactive(input$intInput2,{
    intscrap[intnames %in% input$intInput2]
  })
  output$intscrapNum2z <-output$intscrapNum2 <- renderText(intscrapfetch2())
  
  # Matrix1 ----
  
    updateSelectizeInput(session, 'PriMatrixInput1',
                         choices = primatrixnames,
                         selected = "",
                         server = TRUE)
  # Associate Name with Matrix type name
  primatrixnamefetch1 <- eventReactive(input$PriMatrixInput1, {
    primatrixnames[primatrixnames %in% input$PriMatrixInput1]
  })
  output$primatrixname1 <- renderText(primatrixnamefetch1())
  
  # Associate Energy value with Matrix type name
  primatrixenergyfetch1 <- eventReactive(input$PriMatrixInput1, {
    matrixenergy[matrixnames %in% input$PriMatrixInput1]
  })
  output$primatrixEnergyNum1z <-output$primatrixEnergyNum1 <- renderText(primatrixenergyfetch1())
  
  # OtherMat1 ----
  
  ###A
  updateSelectizeInput(session, 'OtherMatrixAInput1',
                       choices = othermatrixnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  othermatrixAnamefetch1 <- eventReactive(input$OtherMatrixAInput1, {
    othermatrixnames[othermatrixnames %in% input$OtherMatrixAInput1]
  })
  output$othermatrixAname1 <- renderText(othermatrixAnamefetch1())
  
  # Associate Energy value with int type name
  othermatrixAenergyfetch1 <- eventReactive(input$OtherMatrixAInput1, {
    matrixenergy[matrixnames %in% input$OtherMatrixAInput1]
  })
  output$othermatrixAEnergyNum1z <-output$othermatrixAEnergyNum1 <- renderText(othermatrixAenergyfetch1())
  
  ####B
  
  updateSelectizeInput(session, 'OtherMatrixBInput1',
                       choices = othermatrixnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  othermatrixBnamefetch1 <- eventReactive(input$OtherMatrixBInput1, {
    othermatrixnames[othermatrixnames %in% input$OtherMatrixBInput1]
  })
  output$othermatrixBname1 <- renderText(othermatrixBnamefetch1())
  
  # Associate Energy value with int type name
  othermatrixBenergyfetch1 <- eventReactive(input$OtherMatrixBInput1, {
    matrixenergy[matrixnames %in% input$OtherMatrixBInput1]
  })
  output$othermatrixBEnergyNum1z <-output$othermatrixBEnergyNum1 <- renderText(othermatrixBenergyfetch1())
  
  ####C
  
  updateSelectizeInput(session, 'OtherMatrixCInput1',
                       choices = othermatrixnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  othermatrixCnamefetch1 <- eventReactive(input$OtherMatrixCInput1, {
    othermatrixnames[othermatrixnames %in% input$OtherMatrixCInput1]
  })
  output$othermatrixCname1 <- renderText(othermatrixCnamefetch1())
  
  # Associate Energy value with int type name
  othermatrixCenergyfetch1 <- eventReactive(input$OtherMatrixCInput1, {
    matrixenergy[matrixnames %in% input$OtherMatrixCInput1]
  })
  output$othermatrixCEnergyNum1z <-output$othermatrixCEnergyNum1 <- renderText(othermatrixCenergyfetch1())
  
  ###InsertsA
  updateSelectizeInput(session, 'InsertsAInput1',
                       choices = insertsnames,
                       selected = "",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsAnamefetch1 <- eventReactive(input$InsertsAInput1, {
    insertsnames[insertsnames %in% input$InsertsInput1]
  })
  output$insertsAname1 <- renderText(insertsAnamefetch1())
  
  # Associate Energy value with int type name
  insertsAenergyfetch1 <- eventReactive(input$InsertsAInput1, {
    matrixenergy[matrixnames %in% input$InsertsAInput1]
  })
  output$insertsAEnergyNum1z <-output$insertsAEnergyNum1 <- renderText(insertsAenergyfetch1())
  
  ####InsertsB
  
  updateSelectizeInput(session, 'InsertsBInput1',
                       choices = insertsnames,
                       selected = "",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsBnamefetch1 <- eventReactive(input$InsertsBInput1, {
    insertsnames[insertsnames %in% input$InsertsBInput1]
  })
  output$insertsBname1 <- renderText(insertsBnamefetch1())
  
  # Associate Energy value with int type name
  insertsBenergyfetch1 <- eventReactive(input$InsertsBInput1, {
    matrixenergy[matrixnames %in% input$InsertsBInput1]
  })
  output$othermatrixBEnergyNum1z <-output$insertsBEnergyNum1 <- renderText(insertsBenergyfetch1())
  
  # Matrix2 ----
  
  updateSelectizeInput(session, 'PriMatrixInput2',
                       choices = primatrixnames,
                       selected = "",
                       server = TRUE)
  # Associate Name with Matrix type name
  primatrixnamefetch2 <- eventReactive(input$PriMatrixInput2, {
    primatrixnames[primatrixnames %in% input$PriMatrixInput2]
  })
  output$primatrixname2 <- renderText(primatrixnamefetch2())
  
  # Associate Energy value with Matrix type name
  primatrixenergyfetch2 <- eventReactive(input$PriMatrixInput2, {
    matrixenergy[matrixnames %in% input$PriMatrixInput2]
  })
  output$primatrixEnergyNum2z <-output$primatrixEnergyNum2 <- renderText(primatrixenergyfetch2())
  
  # OtherMat2 ----
  
  ###A
  updateSelectizeInput(session, 'OtherMatrixAInput2',
                       choices = othermatrixnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  othermatrixAnamefetch2 <- eventReactive(input$OtherMatrixAInput2, {
    othermatrixnames[othermatrixnames %in% input$OtherMatrixAInput2]
  })
  output$othermatrixAname2 <- renderText(othermatrixAnamefetch2())
  
  # Associate Energy value with int type name
  othermatrixAenergyfetch2 <- eventReactive(input$OtherMatrixAInput2, {
    matrixenergy[matrixnames %in% input$OtherMatrixAInput2]
  })
  output$othermatrixAEnergyNum2z <-output$othermatrixAEnergyNum2 <- renderText(othermatrixAenergyfetch2())
  
  ####B
  
  updateSelectizeInput(session, 'OtherMatrixBInput2',
                       choices = othermatrixnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  othermatrixBnamefetch2 <- eventReactive(input$OtherMatrixBInput2, {
    othermatrixnames[othermatrixnames %in% input$OtherMatrixBInput2]
  })
  output$othermatrixBname2 <- renderText(othermatrixBnamefetch2())
  
  # Associate Energy value with int type name
  othermatrixBenergyfetch2 <- eventReactive(input$OtherMatrixBInput2, {
    matrixenergy[matrixnames %in% input$OtherMatrixBInput2]
  })
  output$othermatrixBEnergyNum2z <-output$othermatrixBEnergyNum2 <- renderText(othermatrixBenergyfetch2())
  
  ####C
  
  updateSelectizeInput(session, 'OtherMatrixCInput2',
                       choices = othermatrixnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  othermatrixCnamefetch2 <- eventReactive(input$OtherMatrixCInput2, {
    othermatrixnames[othermatrixnames %in% input$OtherMatrixCInput2]
  })
  output$othermatrixCname2 <- renderText(othermatrixCnamefetch2())
  
  # Associate Energy value with int type name
  othermatrixCenergyfetch2 <- eventReactive(input$OtherMatrixCInput2, {
    matrixenergy[matrixnames %in% input$OtherMatrixCInput2]
  })
  output$othermatrixCEnergyNum2z <-output$othermatrixCEnergyNum2 <- renderText(othermatrixCenergyfetch2())
  
  ###InsertsA
  updateSelectizeInput(session, 'InsertsAInput2',
                       choices = insertsnames,
                       selected = "",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsAnamefetch2 <- eventReactive(input$InsertsAInput2, {
    insertsnames[insertsnames %in% input$InsertsInput2]
  })
  output$insertsAname2 <- renderText(insertsAnamefetch2())
  
  # Associate Energy value with int type name
  insertsAenergyfetch2 <- eventReactive(input$InsertsAInput2, {
    matrixenergy[matrixnames %in% input$InsertsAInput2]
  })
  output$insertsAEnergyNum2z <-output$insertsAEnergyNum2 <- renderText(insertsAenergyfetch2())
  
  ####InsertsB
  
  updateSelectizeInput(session, 'InsertsBInput2',
                       choices = insertsnames,
                       selected = "",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsBnamefetch2 <- eventReactive(input$InsertsBInput2, {
    insertsnames[insertsnames %in% input$InsertsBInput2]
  })
  output$insertsBname2 <- renderText(insertsBnamefetch2())
  
  # Associate Energy value with int type name
  insertsBenergyfetch2 <- eventReactive(input$InsertsBInput2, {
    matrixenergy[matrixnames %in% input$InsertsBInput2]
  })
  output$othermatrixBEnergyNum2z <-output$insertsBEnergyNum2 <- renderText(insertsBenergyfetch2())
  
   })
