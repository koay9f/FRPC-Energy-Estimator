
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

#Name Data_Inserts columns
Data_inserts =  read.csv("data/Data_Inserts.csv")
insertsnames = Data_inserts$Name_Inserts
insertsenergy = Data_inserts$Energy_Inserts


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
  output$partname1d <- output$partname1c <- output$partname1b <- output$partname1a <- output$partname1 <- renderText({paste("Part Name:",input$name1)})
  partname1e<- reactive(input$name1)
  
  output$partweight1e <- output$partweight1d <- output$partweight1c <- output$partweight1b <- output$partweight1a <- output$partweight1 <- renderText({paste("Part Weight:",input$finalweight1, "kg")})
  
  
  output$partname2d <-output$partname2c <-output$partname2b <-output$partname2a <- output$partname2 <- renderText({paste("Part Name:",input$name2)})
  partname2e<- reactive(input$name2)
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
output$moldname1d <- output$moldname1c <- output$moldname1b <- output$moldname1a <- output$moldname1 <- renderText(moldnamefetch1())


moldshortfetch1 <- eventReactive(input$moldingInput1, {
  moldshort[moldnames %in% input$moldingInput1]
})
 output$moldshort1d <- output$moldshort1c <- output$moldshort1b <- output$moldshort1a <- output$moldshort1 <- renderText(moldshortfetch1())

   # Associate Energy value with mold type name
  moldenergyfetch1 <- eventReactive(input$moldingInput1, {
    moldenergy[moldnames %in% input$moldingInput1]
  })
  output$EnergyNum1 <- renderText(moldenergyfetch1())

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
   output$moldshort2d <- output$moldshort2c <- output$moldshort2b <- output$moldshort2a <- output$moldshort2 <- renderText(moldshortfetch2())
  
  # Associate energy with mold type name
  moldenergyfetch2 <- eventReactive(input$moldingInput2, {
    moldenergy[moldnames %in% input$moldingInput2]
  })
  output$EnergyNum2 <- renderText(moldenergyfetch2())
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
   output$fibername1d  <- output$fibername1 <- renderText(fibernamefetch1())
  # Associate Energy value with fiber type name
  fiberenergyfetch1 <- eventReactive(input$fiberInput1, {
    fiberenergy[fibernames %in% input$fiberInput1]
  })
  output$fiberEnergyNum1e <-output$fiberEnergyNum1d <-output$fiberEnergyNum1 <- renderText(fiberenergyfetch1())
  

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
  output$fiberEnergyNum2e <-output$fiberEnergyNum2d <-output$fiberEnergyNum2 <- renderText(fiberenergyfetch2())
  
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
  output$intEnergyNum1 <- renderText(intenergyfetch1())
  
  # Associate layup scrap with int type name
  intscrapfetch1 <- eventReactive(input$intInput1,{
    intscrap[intnames %in% input$intInput1]
  })
  output$intscrapNum1z <-output$intscrapNum1 <- renderText(intscrapfetch1())
  
  intprepregfetch1 <- eventReactive(input$intInput1, {
    intprepreg[intnames %in% input$intInput1]
  })
  int.prepregYN1 <-renderText(intprepregfetch1())
  
  
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
  output$intEnergyNum2 <- renderText(intenergyfetch2())
  
  # Associate layup scrap with int type name
  intscrapfetch2 <- eventReactive(input$intInput2,{
    intscrap[intnames %in% input$intInput2]
  })
  output$intscrapNum2z <-output$intscrapNum2 <- renderText(intscrapfetch2())
  
  intprepregfetch2 <- eventReactive(input$intInput2, {
    intprepreg[intnames %in% input$intInput2]
  })
  int.prepregYN2 <-renderText(intprepregfetch2())
  
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
  output$primatrixEnergyNum1 <- renderText(primatrixenergyfetch1())
  
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
  output$othermatrixAEnergyNum1 <- renderText(othermatrixAenergyfetch1())
  
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
  output$othermatrixBEnergyNum1 <- renderText(othermatrixBenergyfetch1())
  
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
  output$othermatrixCEnergyNum1 <- renderText(othermatrixCenergyfetch1())
  
  # Inserts1 ----
  # Insert A 1
  updateSelectizeInput(session, 'InsertsAInput1',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsAnamefetch1 <- eventReactive(input$InsertsAInput1, {
    insertsnames[insertsnames %in% input$InsertsInput1]
  })
   output$insertsAname1 <- renderText(insertsAnamefetch1())
  # Associate Energy value with int type name
  insertsAenergyfetch1 <- eventReactive(input$InsertsAInput1, {
    insertsenergy[insertsnames %in% input$InsertsAInput1]
  })
  output$insertsAEnergyNum1 <- renderText(insertsAenergyfetch1())
  
  ####InsertsB
  
  updateSelectizeInput(session, 'InsertsBInput1',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsBnamefetch1 <- eventReactive(input$InsertsBInput1, {
    insertsnames[insertsnames %in% input$InsertsBInput1]
  })
  output$insertsBname1 <- renderText(insertsBnamefetch1())
  # Associate Energy value with int type name
  insertsBenergyfetch1 <- eventReactive(input$InsertsBInput1, {
    insertsenergy[insertsnames %in% input$InsertsBInput1]
  })
  output$insertsBEnergyNum1 <- renderText(insertsBenergyfetch1())
  
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
 output$primatrixEnergyNum2 <- renderText(primatrixenergyfetch2())
  
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
  output$othermatrixAEnergyNum2 <- renderText(othermatrixAenergyfetch2())
  
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
  output$othermatrixBEnergyNum2 <- renderText(othermatrixBenergyfetch2())
  
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
  output$othermatrixCEnergyNum2 <- renderText(othermatrixCenergyfetch2())
  
  # Inserts 2 ---- 
  
  ###InsertsA
  updateSelectizeInput(session, 'InsertsAInput2',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsAnamefetch2 <- eventReactive(input$InsertsAInput2, {
    insertsnames[insertsnames %in% input$InsertsInput2]
  })
  output$insertsAname2 <- renderText(insertsAnamefetch2())
  # Associate Energy value with int type name
  insertsAenergyfetch2 <- eventReactive(input$InsertsAInput2, {
    insertsenergy[insertsnames %in% input$InsertsAInput2]
  })
  output$insertsAEnergyNum2 <- renderText(insertsAenergyfetch2())
  
  ####InsertsB
  
  updateSelectizeInput(session, 'InsertsBInput2',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsBnamefetch2 <- eventReactive(input$InsertsBInput2, {
    insertsnames[insertsnames %in% input$InsertsBInput2]
  })
  output$insertsBname2 <- renderText(insertsBnamefetch2())
  # Associate Energy value with int type name
  insertsBenergyfetch2 <- eventReactive(input$InsertsBInput2, {
    insertsenergy[insertsnames %in% input$InsertsBInput2]
  })
  output$insertsBEnergyNum2 <- renderText(insertsBenergyfetch2())
  
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
   output$curename1 <- renderText(curenamefetch1())
  # Associate Energy value with cure type name
  cureenergyfetch1 <- eventReactive(input$cureInput1, {
    cureenergy[curenames %in% input$cureInput1]
  })
  output$cureEnergyNum1 <- renderText(cureenergyfetch1())
  
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
  output$curename2 <- renderText(curenamefetch2())
  # Associate Energy value with cure type name
  cureenergyfetch2 <- eventReactive(input$cureInput2, {
    cureenergy[curenames %in% input$cureInput2]
  })
  output$cureEnergyNum2 <- renderText(cureenergyfetch2())  
  
  # Finish1 ----
  # Make List for Box
  updateSelectizeInput(session, 'finishInput1',
                       choices = finishnames,
                       selected = "None",
                       server = TRUE)
  # Associate Name value with finish type name
  finishnamefetch1 <- eventReactive(input$finishInput1, {
    finishnames[finishnames %in% input$finishInput1]
  })
   output$finishname1 <- renderText(finishnamefetch1())
  # Associate Energy value with finish type name
  finishenergyfetch1 <- eventReactive(input$finishInput1, {
    finishenergy[finishnames %in% input$finishInput1]
  })
  output$finishEnergyNum1 <- renderText(finishenergyfetch1())  
  
  
  
  # Finish2 ----
  # Make List for Box
  updateSelectizeInput(session, 'finishInput2',
                       choices = finishnames,
                       selected = "None",
                       server = TRUE)
  # Associate Name value with finish type name
  finishnamefetch2 <- eventReactive(input$finishInput2, {
    finishnames[finishnames %in% input$finishInput2]
  })
  output$finishname2 <- renderText(finishnamefetch2())
  # Associate Energy value with finish type name
  finishenergyfetch2 <- eventReactive(input$finishInput2, {
    finishenergy[finishnames %in% input$finishInput2]
  })
 output$finishEnergyNum2 <- renderText(finishenergyfetch2())  
  
  
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
  # User Input --> variables ----
    #Yield
    
    
    layup.yield1 <- reactive(yield_layup(input$intscrapUSERNum1,input$intscraprecycle1))
    layup.yield2 <- reactive(yield_layup(input$intscrapUSERNum2,input$intscraprecycle2))
    mold.yield1 <- reactive(yield_mold(input$moldyieldUSERNum1, input$moldyieldrecycle1))
    mold.yield2 <- reactive(yield_mold(input$moldyieldUSERNum2, input$moldyieldrecycle2))
    finish.yield1 <- reactive(yield_finish(input$finishscrap1, input$finishscraprecycle1))
    finish.yield2 <- reactive(yield_finish(input$finishscrap2, input$finishscraprecycle2))
    
   
    
    
    #mass fracs
    finalweight1 <- reactive({input$finalweight1})
    
    raw.f.f1 <- reactive({input$moldfracUSERNum1})
    raw.f.pm1 <- reactive({input$primatrixfrac1})
    raw.f.ma1 <- reactive({input$othermatrixAfrac1})
    raw.f.ma1 <- reactive({input$othermatrixBfrac1})
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
    
    observe({
      rff1 <- input$moldfracUSERNum1
      rff2 <- input$moldfracUSERNum2
    updateNumericInput(session, "primatrixfrac1", value = 1-rff1)
    updateNumericInput(session, "primatrixfrac2", value = 1-rff2)
    })
    
  # Mass Fractions converion ----  
    raw.to.actual.fracs1 <- reactive(Data_mass_fxn(finalweight1(), raw.f.f1(), raw.f.pm1(), raw.f.ma1(), raw.f.ma1(), raw.f.mc1(), m.ia1(), m.ib1()))
    raw.to.actual.fracs2 <- reactive(Data_mass_fxn(finalweight2(), raw.f.f2(), raw.f.pm2(), raw.f.ma2(), raw.f.ma2(), raw.f.mc2(), m.ia2(), m.ib2()))
    
   
    
    f.f1  <- reactive(raw.to.actual.fracs1()$mass.frac[1])
    f.pm1 <- reactive(raw.to.actual.fracs1()$mass.frac[2])
    f.ma1 <- reactive(raw.to.actual.fracs1()$mass.frac[3])
    f.mb1 <- reactive(raw.to.actual.fracs1()$mass.frac[4])
    f.mc1 <- reactive(raw.to.actual.fracs1()$mass.frac[5])
    f.ia1 <- reactive(raw.to.actual.fracs1()$mass.frac[6])
    f.ib1 <- reactive(raw.to.actual.fracs1()$mass.frac[7])
    f.f2  <- reactive(raw.to.actual.fracs2()$mass.frac[1])
    f.pm2 <- reactive(raw.to.actual.fracs2()$mass.frac[2])
    f.ma2 <- reactive(raw.to.actual.fracs2()$mass.frac[3])
    f.mb2 <- reactive(raw.to.actual.fracs2()$mass.frac[4])
    f.mc2 <- reactive(raw.to.actual.fracs2()$mass.frac[5])
    f.ia2 <- reactive(raw.to.actual.fracs2()$mass.frac[6])
    f.ib2 <- reactive(raw.to.actual.fracs2()$mass.frac[7])

    #output$table1 <- renderTable(raw.to.actual.fracs1())
    # output$testff <- renderText(int.prepregYN1())
    # output$testclass <- renderText(class(int.prepregYN1()))
    # output$testyield <- renderText(layup.yield1())
    # 
  
  # True Yield ----  

    yield_data.df <- reactive(BIGFUNCTION1(
      finish.yield1(), mold.yield1(), layup.yield1(),
      finish.yield2(), mold.yield2(), layup.yield2(),
      f.f1(), f.pm1(), f.ma1(), f.mb1(), f.mc1(), f.ia1(), f.ib1(),
      f.f2(), f.pm2(), f.ma2(), f.mb2(), f.mc2(), f.ia2(), f.ib2(),
      int.prepregYN1(), int.prepregYN2(), 
      finalweight1(), finalweight2()
      ))
    
  # Energy Calcs ----
    
    E.fib1 <- reactive(fiberenergyfetch1())
    E.int1 <- reactive(intenergyfetch1())
    E.pm1  <- reactive(primatrixenergyfetch1())
    E.ma1  <- reactive(othermatrixAenergyfetch1())
    E.mb1  <- reactive(othermatrixBenergyfetch1())
    E.mc1  <- reactive(othermatrixCenergyfetch1())
    E.ia1  <- reactive(insertsAenergyfetch1())
    E.ib1  <- reactive(insertsBenergyfetch1())
    E.mold1 <- reactive(moldenergyfetch1())
    E.cure1 <- reactive(cureenergyfetch1())
    E.fin1  <- reactive(finishenergyfetch1())  
    
    
    E.fib2 <- reactive(fiberenergyfetch2())
    E.int2 <- reactive(intenergyfetch2())
    E.pm2  <- reactive(primatrixenergyfetch2())
    E.ma2  <- reactive(othermatrixAenergyfetch2())
    E.mb2  <- reactive(othermatrixBenergyfetch2())
    E.mc2  <- reactive(othermatrixCenergyfetch2())
    E.ia2  <- reactive(insertsAenergyfetch2())
    E.ib2  <- reactive(insertsBenergyfetch2())
    E.mold2 <- reactive(moldenergyfetch2())
    E.cure2 <- reactive(cureenergyfetch2())  
    E.fin2  <- reactive(finishenergyfetch2())  
    
   output$fiber.m.i1 <- reactive(yield_data.df()$mass_initial[3])
   output$fiber.m.i2 <- reactive(yield_data.df()$mass_initial[12])
   output$pm.m.i1 <- reactive(yield_data.df()$mass_initial[6]* f.pm1()/sum(f.pm1(),f.ma1(),f.mb1(),f.mc1()))
   output$pm.m.i2 <- reactive(yield_data.df()$mass_initial[15]* f.pm2()/sum(f.pm2(),f.ma1(),f.mb2(),f.mc2())) 
 
   output$ma.m.i1 <- reactive(yield_data.df()$mass_initial[6]* f.ma1()/sum(f.pm1(),f.ma1(),f.mb1(),f.mc1()))
   output$ma.m.i2 <- reactive(yield_data.df()$mass_initial[15]* f.ma2()/sum(f.pm2(),f.ma1(),f.mb2(),f.mc2())) 
   output$mb.m.i1 <- reactive(yield_data.df()$mass_initial[6]* f.mb1()/sum(f.pm1(),f.ma1(),f.mb1(),f.mc1()))
   output$mb.m.i2 <- reactive(yield_data.df()$mass_initial[15]* f.mb2()/sum(f.pm2(),f.ma1(),f.mb2(),f.mc2())) 
   output$mc.m.i1 <- reactive(yield_data.df()$mass_initial[6]* f.mc1()/sum(f.pm1(),f.ma1(),f.mb1(),f.mc1()))
   output$mc.m.i2 <- reactive(yield_data.df()$mass_initial[15]* f.mc2()/sum(f.pm2(),f.ma1(),f.mb2(),f.mc2())) 
   
   output$ia.m.i1 <- reactive(yield_data.df()$mass_initial[9]* f.ia1()/sum(f.ia1(),f.ib1()))
   output$ia.m.i2 <- reactive(yield_data.df()$mass_initial[18]* f.ia2()/sum(f.ia1(),f.ib2())) 
   output$ib.m.i1 <- reactive(yield_data.df()$mass_initial[9]* f.ib1()/sum(f.ia1(),f.ib1()))
   output$ib.m.i2 <- reactive(yield_data.df()$mass_initial[18]* f.ib2()/sum(f.ia1(),f.ib2()))
   
   
   
   
    
    energy_data.df <- reactive(BIGFUNCTION2(yield_data.df(),
             f.pm1(), f.ma1(), f.mb1(), f.mc1(), f.ia1(), f.ib1(),
             f.pm2(), f.ma2(), f.mb2(), f.mc2(), f.ia2(), f.ib2(),
             E.fib1(), E.int1(), E.pm1(), E.ma1(), E.mb1(), E.mc1(),
             E.ia1(), E.ib1(), E.mold1(), E.cure1(), E.fin1(),
             E.fib2(), E.int2(), E.pm2(), E.ma2(), E.mb2(), E.mc2(),
             E.ia2(), E.ib2(), E.mold2(), E.cure2(), E.fin2()
    )) 
    
    
      # 
      # output$table1 <- renderTable(yield_data.df())
      # 
      # output$table2 <- renderTable(energy_data.df()[-3, -4])
      # 
# Prep final tables ----
    
    n.mold1 <- reactive(moldshortfetch1())
    n.mold2 <- reactive(moldshortfetch2())
    n.int1 <- reactive(intnamefetch1())
    n.int2 <- reactive(intnamefetch2())
    n.cure1 <-reactive(curenamefetch1())
    n.cure2 <- reactive(curenamefetch2())
    n.fin1 <- reactive(finishnamefetch1())
    n.fin2 <- renderText(finishnamefetch2())
    
    
    
    n.f1 <- reactive(fibernamefetch1())
    n.pm1 <-reactive(primatrixnamefetch1())
    n.ma1 <- reactive(othermatrixAnamefetch1())
    n.mb1 <- reactive(othermatrixBnamefetch1())
    n.mc1 <- reactive(othermatrixCnamefetch1())
    n.ia1 <-reactive(insertsAnamefetch1())
    n.ib1 <- reactive(insertsBnamefetch1())
    
    fibername2e <- reactive(fibernamefetch2())
    primatrixname2e <- reactive(primatrixnamefetch2())
    othermatrixAname2e <- reactive(othermatrixAnamefetch2())
    othermatrixBname2e <- reactive(othermatrixBnamefetch2())
    othermatrixCname2e <- reactive(othermatrixCnamefetch2())
    insertsAname2e <- reactive(insertsAnamefetch2())
    insertsBname2e <- reactive(insertsBnamefetch2())
    
    
    
    

    E.f.fib1 <- reactive(energy_data.df()$finalenergy[1])
    E.f.int1 <- reactive(energy_data.df()$finalenergy[2])
    E.f.pm1  <- reactive(energy_data.df()$finalenergy[3])
    E.f.ma1  <- reactive(energy_data.df()$finalenergy[4])
    E.f.mb1  <- reactive(energy_data.df()$finalenergy[5])
    E.f.mc1  <- reactive(energy_data.df()$finalenergy[6])
    E.f.ia1  <- reactive(energy_data.df()$finalenergy[7])
    E.f.ib1  <- reactive(energy_data.df()$finalenergy[8])
    E.f.mold1 <- reactive(energy_data.df()$finalenergy[9])
    E.f.cure1 <- reactive(energy_data.df()$finalenergy[10])
    E.f.fin1  <- reactive(energy_data.df()$finalenergy[11])

    E.f.fib2 <- reactive(energy_data.df()$finalenergy[12])
    E.f.int2 <- reactive(energy_data.df()$finalenergy[13])
    E.f.pm2  <- reactive(energy_data.df()$finalenergy[14])
    E.f.ma2  <- reactive(energy_data.df()$finalenergy[15])
    E.f.mb2  <- reactive(energy_data.df()$finalenergy[16])
    E.f.mc2  <- reactive(energy_data.df()$finalenergy[17])
    E.f.ia2  <- reactive(energy_data.df()$finalenergy[18])
    E.f.ib2  <- reactive(energy_data.df()$finalenergy[19])
    E.f.mold2 <- reactive(energy_data.df()$finalenergy[20])
    E.f.cure2 <- reactive(energy_data.df()$finalenergy[21])
    E.f.fin2 <- reactive(energy_data.df()$finalenergy[22])
    
    # Final Tables ----
    Mat1.E.df <- reactive(data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert"),
      #Choice = c(n.f1(), n.pm1(), n.ma1(), n.mb1(), n.mc1(), n.ia1(), n.ib1()),
      "Embodied Energy (MJ/part)" = c(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1())
    ))
    
    Process1.E.df <- reactive(data_frame(
      Process = c("Intermediate", "Molding", "Curing", "Finishing"),
      #Choice = list(n.int1(), n.mold1(), n.cure1(), n.fin1()),
      "Embodied Energy (MJ/part)" = c(E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1())
    ))
    
    Mat2.E.df <- reactive(data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert"),
      
      "Embodied Energy (MJ/part)" = c(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2())
    ))
    
    Process2.E.df <- reactive(data_frame(
      Process = c("Intermediate", "Molding", "Curing", "Finishing"),
     # Choice = list(n.int2(), n.mold2(), n.cure2(), n.fin2()),
      "Embodied Energy (MJ/part)" = c(E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2())
    ))
    
    output$Table.mat.1 <- renderTable(Mat1.E.df())
    output$Table.pro.1 <- renderTable(Process1.E.df())
    output$Table.mat.2 <- renderTable(Mat2.E.df())
    output$Table.pro.2 <- renderTable(Process2.E.df())
  
    
   # Final Graph ----

   energy_plot.df <- reactive(data_frame(
     techset = c(rep(partname1e(), 7), rep(partname2e(), 7)),
     process_segment = c(rep(c("Fiber", "Primary Matrix Material", "Other Materials", "Intermediate", "Molding", "Curing", "Finishing"),2)),
     process_energy = c(E.fib1(), E.f.pm1(), sum(E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()), E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(),
                                 E.fib2(), E.f.pm2(), sum(E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()), E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2())
     , order = rep(c(1, 2, 3, 4, 5, 6, 7), 2)
       ))
   
  
   fill <- c("Fiber" = "#2960A8", "Intermediate"  = "#74A138", "Primary Matrix Material" = "#C46827",
             "Other Materials"= "#24A7C1", "Molding" = "#8D2A1E", "Curing" = "#E2B500", "Finishing" = "#8A9FCF")
   
   

   output$plot1 <- renderPlotly({
    ggplot(energy_plot.df(), aes(x = factor(techset), y = process_energy, fill = reorder(process_segment, order))) +
       geom_bar(stat = "identity") +
       theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) +
       labs(y = "Embodied Energy (MJ/part)", x = "") +
       scale_fill_manual(values = fill, name = "")+
       coord_flip()
      
   })
   
  
  # End ----
   })
