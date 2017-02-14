
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
  output$partname1d <- output$partname1c <- output$partname1b <- output$partname1a <- output$partname1 <- renderText(input$name1)
  partname1e<- reactive(input$name1)
  
  output$partweight1e <- output$partweight1d <- output$partweight1c <- output$partweight1b <- output$partweight1a <- output$partweight1 <- renderText({paste(input$finalweight1, "kg")})
  
  
  output$partname2d <-output$partname2c <-output$partname2b <-output$partname2a <- output$partname2 <- renderText(input$name2)
  partname2e<- reactive(input$name2)
  output$partweight2e <- output$partweight2d <- output$partweight2c <- output$partweight2b <- output$partweight2a <- output$partweight2 <- renderText({paste(input$finalweight2, "kg")})
  
  
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
  output$EnergyNum1 <- renderText({paste(moldenergyfetch1(), "MJ/kg")})

  # Associate Fiber Frac value with mold type name
  moldfracfetch1 <- eventReactive(input$moldingInput1,{
    moldfrac[moldnames %in% input$moldingInput1]
  })
   output$moldfracNum1 <- renderText({paste(moldfracfetch1() *100, "%" )})

    # Associate Molding yield with mold type name
  moldyieldfetch1 <- eventReactive(input$moldingInput1,{
    moldyield[moldnames %in% input$moldingInput1]
  })
  output$moldyieldNum1 <- renderText({paste(moldyieldfetch1() *100, "%" )})
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
  output$EnergyNum2 <- renderText({paste(moldenergyfetch2(), "MJ/kg")})
  # Associate Fiber Frac value with mold type name
  moldfracfetch2 <- eventReactive(input$moldingInput2,{
    moldfrac[moldnames %in% input$moldingInput2]
  })
  output$moldfracNum2 <- renderText({paste(moldfracfetch2() *100, "%" )})

  # Associate Layup yield with mold type name
  moldyieldfetch2 <- eventReactive(input$moldingInput2,{
    moldyield[moldnames %in% input$moldingInput2]
  })
  output$moldyieldNum2 <- renderText({paste(moldyieldfetch2() *100, "%" )})

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
  output$fiberEnergyNum1 <- renderText({paste(fiberenergyfetch1(), "MJ/kg")})
  output$fiberfrac1b <- renderText({paste(input$moldfracUSERNum1, "%")})

  output$fiberfrac1z <-output$fiberfrac1e <-output$fiberfrac1d <- renderText(input$moldfracUSERNum1)
  
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
  output$fiberEnergyNum2 <- renderText({paste(fiberenergyfetch2(), "MJ/kg")})
  output$fiberfrac2b <- renderText({paste(input$moldfracUSERNum2, "%")})
  output$fiberfrac2z <-output$fiberfrac2e <-output$fiberfrac2d  <- renderText(input$moldfracUSERNum2)
  
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
  output$intEnergyNum1 <- renderText({paste(intenergyfetch1(), "MJ/kg")})
  # Associate layup scrap with int type name
  intscrapfetch1 <- eventReactive(input$intInput1,{
    intscrap[intnames %in% input$intInput1]
  })
  output$intscrapNum1 <- renderText({paste(intscrapfetch1()*100, "%")})
  
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
  output$intEnergyNum2 <- renderText({paste(intenergyfetch2(), "MJ/kg")})
 
  # Associate layup scrap with int type name
  intscrapfetch2 <- eventReactive(input$intInput2,{
    intscrap[intnames %in% input$intInput2]
  })
  output$intscrapNum2 <- renderText({paste(intscrapfetch2()*100, "%")})
  
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
  output$primatrixEnergyNum1 <- renderText({paste(primatrixenergyfetch1(), "MJ/kg")})
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
  output$othermatrixAEnergyNum1 <- renderText({paste(othermatrixAenergyfetch1(), "MJ/kg")})

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
  output$othermatrixBEnergyNum1 <- renderText({paste(othermatrixBenergyfetch1(), "MJ/kg")})
  
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
  output$othermatrixCEnergyNum1 <- renderText({paste(othermatrixCenergyfetch1(), "MJ/kg")})
  
  # Inserts1 ----
  # Insert A 1
  updateSelectizeInput(session, 'InsertsAInput1',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsAnamefetch1 <- eventReactive(input$InsertsAInput1, {
    insertsnames[insertsnames %in% input$InsertsAInput1]
  })
   output$insertsAname1 <- renderText(insertsAnamefetch1())
  # Associate Energy value with int type name
  insertsAenergyfetch1 <- eventReactive(input$InsertsAInput1, {
    insertsenergy[insertsnames %in% input$InsertsAInput1]
  })
  output$insertsAEnergyNum1 <- renderText({paste(insertsAenergyfetch1(), "MJ/kg")})
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
  output$insertsBEnergyNum1 <- renderText({paste(insertsBenergyfetch1(), "MJ/kg")})

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
 output$primatrixEnergyNum2 <- renderText({paste(primatrixenergyfetch2(), "MJ/kg")})
 
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
  output$othermatrixAEnergyNum2 <-  renderText({paste(othermatrixAenergyfetch2(), "MJ/kg")})

  
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
  output$othermatrixBEnergyNum2 <- renderText({paste(othermatrixBenergyfetch2(), "MJ/kg")})
  
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
  output$othermatrixCEnergyNum2 <- renderText({paste(othermatrixCenergyfetch2(), "MJ/kg")})
  
  # Inserts 2 ---- 
  
  ###InsertsA
  updateSelectizeInput(session, 'InsertsAInput2',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  # Associate Name with Matrix type name
  insertsAnamefetch2 <- eventReactive(input$InsertsAInput2, {
    insertsnames[insertsnames %in% input$InsertsAInput2]
  })
  output$insertsAname2 <- renderText(insertsAnamefetch2())
  # Associate Energy value with int type name
  insertsAenergyfetch2 <- eventReactive(input$InsertsAInput2, {
    insertsenergy[insertsnames %in% input$InsertsAInput2]
  })
  output$insertsAEnergyNum2 <- renderText({paste(insertsAenergyfetch2(), "MJ/kg")})
  
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
  output$insertsBEnergyNum2 <- renderText({paste(insertsBenergyfetch2(), "MJ/kg")})
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
  output$cureEnergyNum1 <- renderText({paste(cureenergyfetch1(), "MJ/kg")})
  
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
  output$cureEnergyNum2 <- renderText({paste(cureenergyfetch2(), "MJ/kg")})  
  
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
  output$finishEnergyNum1 <- renderText({paste(finishenergyfetch1(), "MJ/kg")})  

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
 output$finishEnergyNum2 <- renderText({paste(finishenergyfetch2(), "MJ/kg")})  

  # Change user values to default ----
  observe({
    ff1 <- moldfracfetch1()
    ff2 <- moldfracfetch2()
    ints1 <-  intscrapfetch1()
    ints2 <-  intscrapfetch2()
    moldy1 <-  moldyieldfetch1()
    moldy2 <-  moldyieldfetch2()
    
    
    updateNumericInput(session, "moldfracUSERNum1", value = (ff1*100)) 
    updateNumericInput(session, "moldfracUSERNum2", value = (ff2*100)) 
    updateNumericInput(session, "intscrapUSERNum1", value = (ints1*100)) 
    updateNumericInput(session, "intscrapUSERNum2", value = (ints2*100)) 
    updateNumericInput(session, "moldyieldUSERNum1", value = (moldy1*100)) 
    updateNumericInput(session, "moldyieldUSERNum2", value = (moldy2*100) )

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
    
    raw.f.f1 <- reactive({input$moldfracUSERNum1/100})
    raw.f.pm1 <- reactive({input$primatrixfrac1/100})
    raw.f.ma1 <- reactive({input$othermatrixAfrac1/100})
    raw.f.ma1 <- reactive({input$othermatrixBfrac1/100})
    raw.f.mc1 <- reactive({input$othermatrixCfrac1/100})
    m.ia1 <- reactive({input$insertsAfrac1})
    m.ib1 <- reactive({input$insertsBfrac1})
    
    finalweight2 <- reactive({input$finalweight2})
    raw.f.f2 <- reactive({input$moldfracUSERNum2/100})
    raw.f.pm2 <- reactive({input$primatrixfrac2/100})
    raw.f.ma2 <- reactive({input$othermatrixAfrac2/100})
    raw.f.mb2 <- reactive({input$othermatrixBfrac2/100})
    raw.f.mc2 <- reactive({input$othermatrixCfrac2/100})
    m.ia2 <- reactive({input$insertsAfrac2})
    m.ib2 <- reactive({input$insertsBfrac2})
    
    observe({
      rff1 <- input$moldfracUSERNum1
      rff2 <- input$moldfracUSERNum2
    updateNumericInput(session, "primatrixfrac1", value = 100-rff1)
    updateNumericInput(session, "primatrixfrac2", value = 100-rff2)
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

    #output$table4 <- renderTable(raw.to.actual.fracs1())
  
    # True Yield ----  

    yield_data1.df <- reactive(BIGFUNCTION1(
      finish.yield1(), mold.yield1(), layup.yield1(),
      f.f1(), f.pm1(), f.ma1(), f.mb1(), f.mc1(), f.ia1(), f.ib1(),
      int.prepregYN1(), finalweight1()
      ))
    
    yield_data2.df <- reactive(BIGFUNCTION1(
      finish.yield2(), mold.yield2(), layup.yield2(),
      f.f2(), f.pm2(), f.ma2(), f.mb2(), f.mc2(), f.ia2(), f.ib2(),
      int.prepregYN2(), finalweight2()
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
    
  
    energy_data1.df <- reactive(BIGFUNCTION2(yield_data1.df(),
             f.pm1(), f.ma1(), f.mb1(), f.mc1(), f.ia1(), f.ib1(),
             E.fib1(), E.int1(), E.pm1(), E.ma1(), E.mb1(), E.mc1(),
             E.ia1(), E.ib1(), E.mold1(), E.cure1(), E.fin1()
    )) 
    
    energy_data2.df <- reactive(BIGFUNCTION2(yield_data2.df(),
                                            f.pm2(), f.ma2(), f.mb2(), f.mc2(), f.ia2(), f.ib2(),
                                            E.fib2(), E.int2(), E.pm2(), E.ma2(), E.mb2(), E.mc2(),
                                            E.ia2(), E.ib2(), E.mold2(), E.cure2(), E.fin2()
    )) 
    
    
       output$table1a <- renderTable(yield_data1.df(), signif = 3)
       output$table1b <- renderTable(yield_data2.df(), signif = 3)
       
       
       output$table2a <- renderTable(energy_data1.df(), signif = 3)
       output$table2b <- renderTable(energy_data2.df(), signif = 3)
       
       
    # Reactive Names ----
    n.mold1 <- reactive(as.character(moldshortfetch1()))
    n.mold2 <- reactive(as.character(moldshortfetch2()))
    n.int1 <- reactive(as.character(intnamefetch1()))
    n.int2 <- reactive(as.character(intnamefetch2()))
    n.cure1 <-reactive(as.character(curenamefetch1()))
    n.cure2 <- reactive(as.character(curenamefetch2()))
    n.fin1 <- reactive(as.character(finishnamefetch1()))
    n.fin2 <- renderText(as.character(finishnamefetch2()))
    
    n.f1 <- reactive(as.character(fibernamefetch1()))
    n.pm1 <-reactive(as.character(primatrixnamefetch1()))
    n.ma1 <- reactive(as.character(othermatrixAnamefetch1()))
    n.mb1 <- reactive(as.character(othermatrixBnamefetch1()))
    n.mc1 <- reactive(as.character(othermatrixCnamefetch1()))
    n.ia1 <-reactive(as.character(insertsAnamefetch1()))
    n.ib1 <- reactive(as.character(insertsBnamefetch1()))
    
    n.f2 <- reactive(as.character(fibernamefetch2()))
    n.pm2 <-reactive(as.character(primatrixnamefetch2()))
    n.ma2 <- reactive(as.character(othermatrixAnamefetch2()))
    n.mb2 <- reactive(as.character(othermatrixBnamefetch2()))
    n.mc2 <- reactive(as.character(othermatrixCnamefetch2()))
    n.ia2 <-reactive(as.character(insertsAnamefetch2()))
    n.ib2 <- reactive(as.character(insertsBnamefetch2()))
    
    
    # Reactive Energies ---- 

    E.f.fib1 <- reactive(energy_data1.df()$finalenergy[1])
    E.f.int1 <- reactive(energy_data1.df()$finalenergy[2])
    E.f.pm1  <- reactive(energy_data1.df()$finalenergy[3])
    E.f.ma1  <- reactive(energy_data1.df()$finalenergy[4])
    E.f.mb1  <- reactive(energy_data1.df()$finalenergy[5])
    E.f.mc1  <- reactive(energy_data1.df()$finalenergy[6])
    E.f.ia1  <- reactive(energy_data1.df()$finalenergy[7])
    E.f.ib1  <- reactive(energy_data1.df()$finalenergy[8])
    E.f.mold1 <- reactive(energy_data1.df()$finalenergy[9])
    E.f.cure1 <- reactive(energy_data1.df()$finalenergy[10])
    E.f.fin1  <- reactive(energy_data1.df()$finalenergy[11])

    E.f.fib2 <- reactive(energy_data2.df()$finalenergy[1])
    E.f.int2 <- reactive(energy_data2.df()$finalenergy[2])
    E.f.pm2  <- reactive(energy_data2.df()$finalenergy[3])
    E.f.ma2  <- reactive(energy_data2.df()$finalenergy[4])
    E.f.mb2  <- reactive(energy_data2.df()$finalenergy[5])
    E.f.mc2  <- reactive(energy_data2.df()$finalenergy[6])
    E.f.ia2  <- reactive(energy_data2.df()$finalenergy[7])
    E.f.ib2  <- reactive(energy_data2.df()$finalenergy[8])
    E.f.mold2 <- reactive(energy_data2.df()$finalenergy[9])
    E.f.cure2 <- reactive(energy_data2.df()$finalenergy[10])
    E.f.fin2 <- reactive(energy_data2.df()$finalenergy[11])
    
    # Final Tables ----
    Mat1.E.df <- reactive(data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total"),
      Choice = c(n.f1(), n.pm1(), n.ma1(), n.mb1(), n.mc1(), n.ia1(), n.ib1(), " "),
      "Effecive Mass Fraction" = c(f.f1()*100  , f.pm1()*100 , f.ma1()*100 , f.mb1()*100 , f.mc1()*100 , f.ia1()*100 , f.ib1()*100 , sum(f.f1()*100  , f.pm1()*100 , f.ma1()*100 , f.mb1()*100 , f.mc1()*100 , f.ia1()*100 , f.ib1()*100 )),
      "Embodied Energy (MJ/part)" = c(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1(), sum(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()))
    ))
    
    Process1.E.df <- reactive(data_frame(
      Process = c("Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
      Choice =c(n.int1(), n.mold1(), n.cure1(), n.fin1(), " "),
      Yield = c(layup.yield1()*100, mold.yield1()*100, " -- " , finish.yield1()*100, (layup.yield1()* mold.yield1()* finish.yield1()*100)),
      "Embodied Energy (MJ/part)" = c(E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(), sum(E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1()))
    ))
    
    Mat2.E.df <- reactive(data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total"),
      Choice = c(n.f2(), n.pm2(), n.ma2(), n.mb2(), n.mc2(), n.ia2(), n.ib2(), " "),
      "Effective Mass Fraction" = c(f.f2()*100  , f.pm2()*100 , f.ma2()*100 , f.mb2()*100 , f.mc2()*100 , f.ia2()*100 , f.ib2()*100 , sum(f.f2()*100  , f.pm2()*100 , f.ma2()*100 , f.mb2()*100 , f.mc2()*100 , f.ia2()*100 , f.ib2()*100)),
      "Embodied Energy (MJ/part)" = c(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2(), sum(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()))
    ))
    
    Process2.E.df <- reactive(data_frame(
      Process = c("Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
      Choice = list(n.int2(), n.mold2(), n.cure2(), n.fin2(), " "),
      Yield = c(layup.yield2()*100, mold.yield2()*100, " -- " , finish.yield2()*100, (layup.yield2()*mold.yield2()* finish.yield2()*100)),
      "Embodied Energy (MJ/part)" = c(E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2(), sum(E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2()))
    ))
    
    output$Table.mat.1 <- renderTable(Mat1.E.df(), signif = 2)
    output$Table.pro.1 <- renderTable(Process1.E.df(), signif = 2)
    output$Table.mat.2 <- renderTable(Mat2.E.df(), signif = 2)
    output$Table.pro.2 <- renderTable(Process2.E.df(), signif = 2)
  
    
    # Final Graph ----

   energy_plot.df <- reactive(data_frame(
     techset = c(rep(partname1e(), 7), rep(partname2e(), 7)),
     process_segment = c(rep(c("Fiber", "Primary Matrix Material", "Other Materials", "Intermediate", "Molding", "Curing", "Finishing"),2)),
     process_energy = c(E.f.fib1(), E.f.pm1(), sum(E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()), E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(),
                        E.f.fib2(), E.f.pm2(), sum(E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()), E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2())
     , order = rep(c(1, 2, 3, 4, 5, 6, 7), 2)
       ))
   
    output$table3 <- renderTable(energy_plot.df(), signif = 3)
    
   fill <- c("Fiber" = "#2960A8", "Intermediate"  = "#74A138", "Primary Matrix Material" = "#C46827",
             "Other Materials"= "#24A7C1", "Molding" = "#8D2A1E", "Curing" = "#E2B500", "Finishing" = "#8A9FCF")
   
   

   output$plot1 <- renderPlotly({
    ggplot(energy_plot.df(), aes(x = factor(techset), y = process_energy, fill = reorder(process_segment, order) )) +
       geom_bar(stat = "identity") +
       coord_flip() +
       labs(y = "Embodied Energy (MJ/part)", x = "") +
       scale_fill_manual(values = fill, name = "") +
     
       theme_bw() + theme(  legend.title = element_blank(), legend.text = element_text(size = 12), legend.background = element_rect(color = "black")
                        , axis.text = element_text(size = 14), axis.title.x = element_text(size = 18)
                           ) 
      
      
   })
   
  
  # End ----
   })
