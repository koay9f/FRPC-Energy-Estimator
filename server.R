
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)
source("math.R")
library(tidyverse)

#Naming variables & data sheets ---- 
#Name Data_Mold columns
Data_Mold = read_csv("data/Data_Mold.csv")
moldnames = Data_Mold$Name_Mold
moldshort = Data_Mold$ShortName_Mold
moldenergy = Data_Mold$Energy_Mold
moldfrac = Data_Mold$Frac_Fiber
moldyield = Data_Mold$Yield_Mold

#Name Data_Int columns
Data_Int = read_csv("data/Data_Int.csv")
intnames = Data_Int$Name_Int
intenergy = Data_Int$Energy_Int
intscrap = Data_Int$Scrap_Int
intprepreg = Data_Int$Prepreg_Int

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

#Name Data_Cure columns
Data_Cure = read.csv("data/Data_Cure.csv")
curenames = Data_Cure$Name_Cure
cureenergy = Data_Cure$Energy_Cure

#Name Data_Finishing columns
Data_Finish = read.csv("data/Data_Finishing.csv")
finishnames = Data_Finish$Name_Finish
finishenergy = Data_Finish$Energy_Finish



#Talk to server ----
shinyServer(function(input, output, session) {
  
  # General Definations ----
  # (none) = first use a = int b = matrix c = mold d= summary e = results z = calcs
  output$partname1e <- output$partname1d <- output$partname1c <- output$partname1b <- output$partname1a <- output$partname1 <- renderText({paste("Part Name:",input$name1)})
  output$partweight1e <- output$partweight1d <- output$partweight1c <- output$partweight1b <- output$partweight1a <- output$partweight1 <- renderText({paste("Part Weight:",input$finalweight1, "kg")})
  
  
  output$partname2e <-output$partname2d <-output$partname2c <-output$partname2b <-output$partname2a <- output$partname2 <- renderText({paste("Part Name:",input$name2)})
  output$partweight2e <- output$partweight2d <- output$partweight2c <- output$partweight2b <- output$partweight2a <- output$partweight2 <- renderText({paste("Part Weight:",input$finalweight2, "kg")})
  
  
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


moldshortfetch1 <- eventReactive(input$moldingInput1, {
  moldshort[moldnames %in% input$moldingInput1]
})
output$moldshort1e <- output$moldshort1d <- output$moldshort1c <- output$moldshort1b <- output$moldshort1a <- output$moldshort1 <- renderText(moldshortfetch1())


   # Associate Energy value with mold type name
  moldenergyfetch1 <- eventReactive(input$moldingInput1, {
    moldenergy[moldnames %in% input$moldingInput1]
  })
  output$EnergyNum1z <-output$EnergyNum1 <- renderText(moldenergyfetch1())

  # Associate Fiber Frac value with mold type name
  moldfracfetch1 <- eventReactive(input$moldingInput1,{
    moldfrac[moldnames %in% input$moldingInput1]
  })
   moldfracNum1z  <-output$moldfracNum1 <- renderText(moldfracfetch1())
   moldfracNum1y  <- reactive(moldfracfetch1())

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
  output$moldname2e <-output$moldname2d <-output$moldname2c <- output$moldname2b <- output$moldname2a <- output$moldname2 <- renderText(moldnamefetch2())
  
  moldshortfetch2 <- eventReactive(input$moldingInput2, {
    moldshort[moldnames %in% input$moldingInput2]
  })
  output$moldshort2e <- output$moldshort2d <- output$moldshort2c <- output$moldshort2b <- output$moldshort2a <- output$moldshort2 <- renderText(moldshortfetch2())
  
  
  # Associate energy with mold type name
  moldenergyfetch2 <- eventReactive(input$moldingInput2, {
    moldenergy[moldnames %in% input$moldingInput2]
  })
  output$EnergyNum2z <- output$EnergyNum2 <- renderText(moldenergyfetch2())
  # Associate Fiber Frac value with mold type name
  moldfracfetch2 <- eventReactive(input$moldingInput2,{
    moldfrac[moldnames %in% input$moldingInput2]
  })
  output$moldfracNum2z <- output$moldfracNum2 <- renderText(moldfracfetch2())
  output$moldfracNum2y  <- renderText(moldfracfetch2())
  
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
  output$fibername1e <- output$fibername1d  <- output$fibername1 <- renderText(fibernamefetch1())
  
  # Associate Energy value with fiber type name
  fiberenergyfetch1 <- eventReactive(input$fiberInput1, {
    fiberenergy[fibernames %in% input$fiberInput1]
  })
  output$fiberEnergyNum1z <-output$fiberEnergyNum1e <-output$fiberEnergyNum1d <-output$fiberEnergyNum1 <- renderText(fiberenergyfetch1())
  

  output$fiberfrac1z <-output$fiberfrac1e <-output$fiberfrac1d <-output$fiberfrac1b <- renderText(input$moldfracUSERNum1)
  
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
  output$fiberEnergyNum2z <-output$fiberEnergyNum2e <-output$fiberEnergyNum2d <-output$fiberEnergyNum2 <- renderText(fiberenergyfetch2())
  
  output$fiberfrac2z <-output$fiberfrac2e <-output$fiberfrac2d <-output$fiberfrac2b <- renderText(input$moldfracUSERNum2)
  
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
  
  intprepregfetch1 <- eventReactive(input$intInput1, {
    intprepreg[intnames %in% input$intInput1]
  })
  output$intprepregYN1z <-renderText(intenergyfetch1())
  
  
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
  
  intprepregfetch2 <- eventReactive(input$intInput2, {
    intprepreg[intnames %in% input$intInput2]
  })
  output$intprepregYN2z <-renderText(intenergyfetch2())
  
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
  
  # Cure1 ----
  # Make List for Box
  updateSelectizeInput(session, 'cureInput1',
                       choices = curenames,
                       selected = "",
                       server = TRUE)
  
  # Associate Name value with cure type name
  curenamefetch1 <- eventReactive(input$cureInput1, {
    curenames[curenames %in% input$cureInput1]
  })
  output$curename1e <- output$curename1 <- renderText(curenamefetch1())
  
  # Associate Energy value with cure type name
  cureenergyfetch1 <- eventReactive(input$cureInput1, {
    cureenergy[curenames %in% input$cureInput1]
  })
  output$cureEnergyNum1z <-output$cureEnergyNum1 <- renderText(cureenergyfetch1())
  
  # Cure2 ----
  # Make List for Box
  updateSelectizeInput(session, 'cureInput2',
                       choices = curenames,
                       selected = "",
                       server = TRUE)
  # Associate Name value with cure type name
  curenamefetch2 <- eventReactive(input$cureInput2, {
    curenames[curenames %in% input$cureInput2]
  })
  output$curename2e <- output$curename2 <- renderText(curenamefetch2())
  
  # Associate Energy value with cure type name
  cureenergyfetch2 <- eventReactive(input$cureInput2, {
    cureenergy[curenames %in% input$cureInput2]
  })
  output$cureEnergyNum2z <-output$cureEnergyNum2 <- renderText(cureenergyfetch2())  
  
  # Finish1 ----
  # Make List for Box
  updateSelectizeInput(session, 'finishInput1',
                       choices = finishnames,
                       selected = "",
                       server = TRUE)
  # Associate Name value with finish type name
  finishnamefetch1 <- eventReactive(input$finishInput1, {
    finishnames[finishnames %in% input$finishInput1]
  })
  output$finishname1e <- output$finishname1 <- renderText(finishnamefetch1())
  
  # Associate Energy value with finish type name
  finishenergyfetch1 <- eventReactive(input$finishInput1, {
    finishenergy[finishnames %in% input$finishInput1]
  })
  output$finishEnergyNum1z <-output$finishEnergyNum1 <- renderText(finishenergyfetch1())  
  
  
  
  # Finish2 ----
  # Make List for Box
  updateSelectizeInput(session, 'finishInput2',
                       choices = finishnames,
                       selected = "",
                       server = TRUE)
  # Associate Name value with finish type name
  finishnamefetch2 <- eventReactive(input$finishInput2, {
    finishnames[finishnames %in% input$finishInput2]
  })
  output$finishname2e <- output$finishname2 <- renderText(finishnamefetch2())
  
  # Associate Energy value with finish type name
  finishenergyfetch2 <- eventReactive(input$finishInput2, {
    finishenergy[finishnames %in% input$finishInput2]
  })
  output$finishEnergyNum2z <-output$finishEnergyNum2 <- renderText(finishenergyfetch2())  
  
  
  
  
  
  
  
  
  
 
  # User Input --> variables ----
  #Yield
  

  layup_yield_val1 <- reactive(yield_layup(input$intscrapUSERNum1,input$intscraprecycle1))
  layup_yield_val2 <- reactive(yield_layup(input$intscrapUSERNum2,input$intscraprecycle2))
  mold_yield_val1 <- reactive(yield_mold(input$moldyieldUSERNum1, input$moldyieldrecycle1))
  mold_yield_val2 <- reactive(yield_mold(input$moldyieldUSERNum2, input$moldyieldrecycle2))
  finish_yield_val1 <- reactive(yield_finish(input$finishscrap1, input$finishscraprecycle1))
  finish_yield_val2 <- reactive(yield_finish(input$finishscrap2, input$finishscraprecycle2))
  
  
  
  #mass fracs
  finalweight1 <- reactive({input$finalweight1})
  
  raw.f.f1 <- reactive({input$moldfracUSERNum1})
  raw.f.pm1 <- reactive({input$primatrixfrac1})
  raw.f.ma1 <- reactive({input$othermatrixAfrac1})
  raw.f.mb1 <- reactive({input$othermatrixBfrac1})
  raw.f.mc1 <- reactive({input$othermatrixCfrac1})
  m.ia1 <- reactive({input$insertsAfrac1})
  m.ib1 <- reactive({input$insertsBfrac1})
  
  finalweight2 <- reactive({input$finalweight2})
  raw.f.f2 <- reactive({input$moldfracUSERNum2})
  raw.f.pm2 <- reactive({input$primatrixfrac2})
  raw.f.ma2 <- reactive({input$othermatrixAfrac2})
  raw.f.mb2 <- reactive({input$othermatrixBfrac2})
  raw.f.mc2 <- reactive({input$othermatrixCfrac2})
  m.ia2 <- reactive({input$insertsAfrac2})
  m.ib2 <- reactive({input$insertsBfrac2})
  
  #fraccheck1 <- sum(raw.f.f1,raw.f.pm1, raw.f.ma1, raw.f.mb1, raw.f.mc1)
  #fraccheck1 <- sum(raw.f.f2,raw.f.pm2, raw.f.ma2, raw.f.mb2, raw.f.mc2)
  
  f.f1 <- reactive(newmassfrac_fxn(raw.f.f1, finalweight1, m.ia1, m.iab))
  output$testff <- renderText(m.ia1())
  
  
  #f.f1 <- raw.f.f1*(partweight1z - m.ia1 - m.ib1) / partweight1z
  #f.f2 <- raw.f.f2*(partweight2z - m.ia2 - m.ib2) / partweight2z
  
  #f.pm1 <- raw.f.pm1*(partweight1z - m.ia1 - m.ib1) / partweight1z
  #f.ma1 <- raw.fma1*(partweight1z - m.ia1 - m.ib1) / partweight1z
  #f.mb1 <- raw.fmb1*(partweight1z - m.ia1 - m.ib1) / partweight1z
  #f.mc1 <- raw.fmc1*(partweight1z - m.ia1 - m.ib1) / partweight1z
  
  #f.pm1 <- raw.f.pm2*(partweight2z - m.ia2 - m.ib2) / partweight2z
  #f.ma2 <- raw.fma2*(partweight2z - m.ia2 - m.ib2) / partweight2z
  #f.mb2 <- raw.fmb2*(partweight2z - m.ia2 - m.ib2) / partweight2z
  #f.mc2 <- raw.fmc2*(partweight2z - m.ia2 - m.ib2) / partweight2z
  
  #f.ia1 <- m.ia1 / partweight1z
  #f.ib1 <- m.ib1 / partweight1z
  #f.ia2 <- m.ia2 / partweight2z
  #f.ib2 <- m.ib2 / partweight2z
  
  
  
  
  
  
  
  
  
  # Change user values to default ----
  observe({
    ff1 <- moldfracfetch1()
    ff2 <- moldfracfetch2()
    ints1 <-  intscrapfetch1()
    ints2 <-  intscrapfetch2()
    moldy1 <-  moldyieldfetch1()
    moldy2 <-  moldyieldfetch2()
    
    
    updateNumericInput(session, "moldfracUSERNum1", value = ff1)
    updateNumericInput(session, "moldfracUSERNum2", value = ff2)
    updateNumericInput(session, "intscrapUSERNum1", value = ints1)
    updateNumericInput(session, "intscrapUSERNum2", value = ints2)
    updateNumericInput(session, "moldyieldUSERNum1", value = moldy1)
    updateNumericInput(session, "moldyieldUSERNum2", value = moldy2)
    
  })
 
  
  
  
  # End ----
   })
