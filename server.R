
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

#Name Data_Int columns
Data_Int = read_csv("data/Data_Int.csv")
intnames = Data_Int$Name_Int

#Name Data_Fiber columns
Data_Fiber = read.csv("data/Data_Fiber.csv")
fibernames = Data_Fiber$Name_Fiber

#Name Data_MatrixM columns
Data_MatrixM = read.csv("data/Data_Matrix.csv")
matrixnames = Data_MatrixM$Name_Matrix
Data_Primatrix <- subset(Data_MatrixM, Type_Matrix == "Matrix")
Data_Additive <- subset(Data_MatrixM, Type_Matrix == "Additive")
Data_Filler <- subset(Data_MatrixM, Type_Matrix == "Filler")
Data_Other <- subset(Data_MatrixM, Type_Matrix == "Other")
primatrixnames = Data_Primatrix$Name_Matrix

#Name Data_Inserts columns
Data_Insert =  read.csv("data/Data_Inserts.csv")
insertsnames = Data_Insert$Name_Insert

#Name Data_Cure columns
Data_Cure = read.csv("data/Data_Cure.csv")
curenames = Data_Cure$Name_Cure
allcure <- c("Cures in Mold","Autoclave Curing","Oven Curing","Oven Curing with Vacuum","QuickStep",
             "Microwave Curing","Microwave Curing with Vacuum","Infrared Curing", "Direct Induction Curing","E Beam Curing")
onlymoldcure <- c("Cures in Mold")
wetcure <- c("Cures in Mold","Oven Curing","QuickStep","Microwave Curing","Infrared Curing","Direct Induction Curing","E Beam Curing")
vbcure <- c("Autoclave Curing","QuickStep","Microwave Curing with Vacuum","Infrared Curing","Direct Induction Curing","E Beam Curing")

#Name Data_Finishing columns
Data_Finish = read.csv("data/Data_Finishing.csv")
finishnames = Data_Finish$Name_Finishing

#Name Molding Properties Tables
Props = read.csv("data/Properties_Mold.csv")


#Talk to server ----
shinyServer(function(input, output, session) {
  
  # General Definations ----
  # (none) = first use; a = int; b = matrix; c = mold; d= summary;  e or z = calcs
  output$partname1d <- output$partname1c <- output$partname1b <- output$partname1a <- output$partname1 <- renderText(input$name1)
  partname1e<- reactive(input$name1)
  
  output$partweight1d <- output$partweight1c <- output$partweight1b <- output$partweight1a <- output$partweight1 <- renderText({paste(input$finalweight1, "kg")})
  
  
  output$partname2d <-output$partname2c <-output$partname2b <-output$partname2a <- output$partname2 <- renderText(input$name2)
  partname2e<- reactive(input$name2)
  output$partweight2d <- output$partweight2c <- output$partweight2b <- output$partweight2a <- output$partweight2 <- renderText({paste(input$finalweight2, "kg")})
  
  
  propnames <- names(Props)
  updateSelectizeInput(session, 'show_vars',
                           choices = propnames,
                           selected = c("Process"))
  
  
  output$props <- renderDataTable({
    Props[,input$show_vars]
    }, options = list(lengthMenu = c(4, 8, 12, 16), pageLength = 16))
  
  
  # Molding ----
  # Make List for Select Box
  updateSelectizeInput(session, 'moldingInput1',
                    choices = moldnames,
                    selected = "",
                    server = TRUE)
  updateSelectizeInput(session, 'moldingInput2',
                       choices = moldnames,
                       selected = "",
                       server = TRUE)
  
  # Build new dataframe if add new molding process
  Data_Mold_new  <- reactiveValues()
  
  Data_Mold_new  <- eventReactive(input$gomold, {
    mold.name <- as.character(input$mold_add)
    mold.E.calc <- calcenergy(input$mold_add_E_m, input$mold_add_E_P_M, input$mold_add_E_per_M, input$mold_add_E_t_M, input$mold_add_E_P_p, input$mold_add_E_per_p, input$mold_add_E_t_p,
                              input$mold_add_E_P_c, input$mold_add_E_per_c, input$mold_add_E_t_c,input$mold_add_E_P_h, input$mold_add_E_per_h, input$mold_add_E_t_h,
                              input$mold_add_E_P_o, input$mold_add_E_per_o, input$mold_add_E_t_o)
    mold.Ener <- whichenergy(input$mold_add_EYN, mold.E.calc, input$mold_add_E_Y)
  
    df <- isolate(Data_Mold) %>%
      add_row(Name_Mold = mold.name,
              ShortName_Mold = mold.name,
              Energy_Mold = mold.Ener,
              Frac_Fiber = 0.5,
              Yield_Mold = 1)
  })
    
  calcmoldE <- reactive(calcenergy(input$mold_add_E_m, input$mold_add_E_P_M, input$mold_add_E_per_M, input$mold_add_E_t_M, input$mold_add_E_P_p, input$mold_add_E_per_p, input$mold_add_E_t_p,
                                   input$mold_add_E_P_c, input$mold_add_E_per_c, input$mold_add_E_t_c,input$mold_add_E_P_h, input$mold_add_E_per_h, input$mold_add_E_t_h,
                                   input$mold_add_E_P_o, input$mold_add_E_per_o, input$mold_add_E_t_o))
  output$calcedmoldE <- renderText({paste(calcmoldE(), "MJ/kg")})
  
  # Change Select box if add new molding process
  observeEvent(input$gomold, { 
    updateSelectizeInput(session, 'moldingInput1',
                         choices = Data_Mold_new()$Name_Mold,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'moldingInput2',
                         choices = Data_Mold_new()$Name_Mold,
                         selected = "",
                         server = TRUE)
  })

  #Match Names to Table
  moldnamefetch1 <- eventReactive(input$moldingInput1,{
    mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
    mold_name[mold_name %in% input$moldingInput1]  })

  moldnamefetch2 <- eventReactive(input$moldingInput2,{
    mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
    mold_name[mold_name %in% input$moldingInput2]  })

  #Match Energy to Name
  moldenergyfetch1 <- eventReactive(input$moldingInput1,{
    mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
    mold_energy <- whichone(input$gomold, Data_Mold_new()$Energy_Mold, Data_Mold$Energy_Mold)
    mold_energy[mold_name %in% input$moldingInput1]  })

  moldenergyfetch2 <- eventReactive(input$moldingInput2,{
    mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
    mold_energy <- whichone(input$gomold, Data_Mold_new()$Energy_Mold, Data_Mold$Energy_Mold)
    mold_energy[mold_name %in% input$moldingInput2]  })

 #Match ShortName to Name

moldshortfetch1 <-  eventReactive(input$moldingInput1,{
  mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
  mold_short <- whichone(input$gomold, Data_Mold_new()$ShortName_Mold, Data_Mold$ShortName_Mold)
  mold_short[mold_name  %in% input$moldingInput1]  })

moldshortfetch2 <- eventReactive(input$moldingInput2, {
  mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
  mold_short <- whichone(input$gomold, Data_Mold_new()$ShortName_Mold, Data_Mold$ShortName_Mold)
  mold_short[mold_name %in% input$moldingInput2]})
   
 # Match Fraction to Name
  moldfracfetch1 <- eventReactive(input$moldingInput1,{
    mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
    mold_frac <- whichone(input$gomold, Data_Mold_new()$Frac_Fiber, Data_Mold$Frac_Fiber)
    mold_frac[mold_name %in% input$moldingInput1]  })
  
  moldfracfetch2 <- eventReactive(input$moldingInput2,{ 
    mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
  mold_frac <- whichone(input$gomold, Data_Mold_new()$Frac_Fiber, Data_Mold$Frac_Fiber)
  mold_frac[mold_name %in% input$moldingInput2]  })

  # Match Yield to Name
  moldyieldfetch1 <- eventReactive(input$moldingInput1,{ 
  mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
  mold_yield <- whichone(input$gomold, Data_Mold_new()$Yield_Mold, Data_Mold$Yield_Mold)
  mold_yield[mold_name %in% input$moldingInput1]  })
  

  moldyieldfetch2 <- eventReactive(input$moldingInput2,{
    mold_name <- whichone(input$gomold, Data_Mold_new()$Name_Mold, Data_Mold$Name_Mold)
    mold_yield <- whichone(input$gomold, Data_Mold_new()$Yield_Mold, Data_Mold$Yield_Mold)
    mold_yield[mold_name %in% input$moldingInput2]  })
  
  # Generate outputs
  output$moldname1d <- output$moldname1c <- output$moldname1b <- output$moldname1a <- output$moldname1 <- renderText(moldnamefetch1())
  output$moldshort1d <- output$moldshort1c <- output$moldshort1b <- output$moldshort1a <- output$moldshort1 <- renderText(moldshortfetch1())
  output$EnergyNum1 <- renderText({paste(moldenergyfetch1(), "MJ/kg")})
  output$moldfracNum1 <- renderText({paste(moldfracfetch1() *100, "%" )})
  output$moldyieldNum1 <- renderText({paste(moldyieldfetch1() *100, "%" )})
  
  output$moldname2e <-output$moldname2d <-output$moldname2c <- output$moldname2b <- output$moldname2a <- output$moldname2 <- renderText(moldnamefetch2())
  output$moldshort2d <- output$moldshort2c <- output$moldshort2b <- output$moldshort2a <- output$moldshort2 <- renderText(moldshortfetch2())
  output$EnergyNum2 <- renderText({paste(moldenergyfetch2(), "MJ/kg")})
  output$moldfracNum2 <- renderText({paste(moldfracfetch2() *100, "%" )})
  output$moldyieldNum2 <- renderText({paste(moldyieldfetch2() *100, "%" )})

  # Fiber ----
    # Make List for Box if no custom data
  updateSelectizeInput(session, 'fiberInput1',
                       choices = fibernames,
                       selected = "",
                       server = TRUE)
  updateSelectizeInput(session, 'fiberInput2',
                       choices = fibernames,
                       selected = "",
                       server = TRUE)
  
  # Make dataframe for Box if  custom data
  Data_Fiber_new  <- reactiveValues()
  Data_Fiber_new  <- eventReactive(input$gofiber, {
    fiber.name <- input$fiber_add
    fiber.Ener <- as.double(input$fiber_add_E)
    
    df <- isolate(Data_Fiber) %>%
      add_row(Name_Fiber = fiber.name,
              Energy_Fiber = fiber.Ener)
  })
  
  # Make List for Box if  custom data
  observeEvent(input$gofiber, { 
    updateSelectizeInput(session, 'fiberInput1',
                         choices = Data_Fiber_new()$Name_Fiber,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'fiberInput2',
                         choices = Data_Fiber_new()$Name_Fiber,
                         selected = "",
                         server = TRUE)
  })
  
  #Match Names to Table
  fibernamefetch1 <- eventReactive(input$fiberInput1,{
    f_name <- whichone(input$gofiber, Data_Fiber_new()$Name_Fiber, Data_Fiber$Name_Fiber)
    f_name[f_name %in% input$fiberInput1]  })
  
  fibernamefetch2 <- eventReactive(input$fiberInput2,{
    f_name <- whichone(input$gofiber, Data_Fiber_new()$Name_Fiber, Data_Fiber$Name_Fiber)
    f_name[f_name %in% input$fiberInput2]  })
  
  #Match Energy to Name
  fiberenergyfetch1 <- eventReactive(input$fiberInput1,{
    f_name <- whichone(input$gofiber, Data_Fiber_new()$Name_Fiber, Data_Fiber$Name_Fiber)
    f_energy <- whichone(input$gofiber, Data_Fiber_new()$Energy_Fiber, Data_Fiber$Energy_Fiber)
    f_energy[f_name %in% input$fiberInput1]  })
  
  fiberenergyfetch2 <- eventReactive(input$fiberInput2,{
    f_name <- whichone(input$gofiber, Data_Fiber_new()$Name_Fiber, Data_Fiber$Name_Fiber)
    f_energy <- whichone(input$gofiber, Data_Fiber_new()$Energy_Fiber, Data_Fiber$Energy_Fiber)
    f_energy[f_name %in% input$fiberInput2]  })
  
 # Generate Outputs
  output$fibername1d  <- output$fibername1 <- renderText(fibernamefetch1())
  output$fibername2d  <- output$fibername2 <- renderText(fibernamefetch2())
  output$fiberEnergyNum1 <- renderText({paste(fiberenergyfetch1(), "MJ/kg")})
  output$fiberEnergyNum2 <- renderText({paste(fiberenergyfetch2(), "MJ/kg")})
  
  # Pull used fiber fractions for use in matrix tab and calculations
  output$fiberfrac1b <- renderText({paste(input$moldfracUSERNum1, "%")})
  output$fiberfrac1z <-output$fiberfrac1e <-output$fiberfrac1d <- renderText(input$moldfracUSERNum1)
  output$fiberfrac2b <- renderText({paste(input$moldfracUSERNum2, "%")})
  output$fiberfrac2z <-output$fiberfrac2e <-output$fiberfrac2d  <- renderText(input$moldfracUSERNum2)
  
  # Intermediate ----
    #Make List for select box based on molding choice
    observeEvent({
    input$intYN1
    input$moldingInput1} ,{
      intlist1 <- intlistfxn(input$moldingInput1)
      if (input$intYN1 == TRUE) {
        updateSelectizeInput(session, 'intInput1',
                             choices = intnames,
                             selected = "",
                             server = TRUE)
      }
      else 
        updateSelectizeInput(session, 'intInput1',
                             choices = intlist1,
                             selected = "",
                             server = TRUE)
    })
  
  observeEvent({
    input$intYN2
    input$moldingInput2} ,{
      intlist2 <- intlistfxn(input$moldingInput2)
      if (input$intYN2 == TRUE) {
        updateSelectizeInput(session, 'intInput2',
                             choices = intnames,
                             selected = "",
                             server = TRUE)
      }
      else 
        updateSelectizeInput(session, 'intInput2',
                             choices = intlist2,
                             selected = "",
                             server = TRUE)
    })
  
  # Make dataframe Box if  custom data
  Data_Int_new  <- reactiveValues()
  Data_Int_new  <- eventReactive(input$goint, {
    int.name <- input$int_add
    int.Ener <- as.double(input$int_add_E)
    int.Scrap <- 0
    int.PP <- input$int_add_PP
    
    df <- isolate(Data_Int) %>%
      add_row(Name_Int = int.name,
              Energy_Int = int.Ener,
              Scrap_Int = int.Scrap,
              Prepreg_Int = int.PP
              )
  })
  
  # Make list Box if  custom data
  observeEvent(input$goint, { 
    updateSelectizeInput(session, 'intInput1',
                         choices = Data_Int_new()$Name_Int,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'intInput2',
                         choices = Data_Int_new()$Name_Int,
                         selected = "",
                         server = TRUE)
  })
  
  #Match Names to Table
  intnamefetch1 <- eventReactive(input$intInput1,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_name[int_name %in% input$intInput1]  })
  
  intnamefetch2 <- eventReactive(input$intInput2,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_name[int_name %in% input$intInput2]  })
  
  #Match Energy to Name
  intenergyfetch1 <- eventReactive(input$intInput1,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_energy <- whichone(input$goint, Data_Int_new()$Energy_Int, Data_Int$Energy_Int)
    int_energy[int_name %in% input$intInput1]  })
  
  intenergyfetch2 <- eventReactive(input$intInput2,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_energy <- whichone(input$goint, Data_Int_new()$Energy_Int, Data_Int$Energy_Int)
    int_energy[int_name %in% input$intInput2]  })
  
  #Match Scrap to Name
  intscrapfetch1 <- eventReactive(input$intInput1,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_scrap <- whichone(input$goint, Data_Int_new()$Scrap_Int, Data_Int$Scrap_Int)
    int_scrap[int_name %in% input$intInput1]  })
  
  intscrapfetch2 <- eventReactive(input$intInput2,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_scrap <- whichone(input$goint, Data_Int_new()$Scrap_Int, Data_Int$Scrap_Int)
    int_scrap[int_name %in% input$intInput2]  })
  
  #Match PrepregYN to Name (for determining if matrix material needs to be included in intermediate scrap)
  intprepregfetch1 <- eventReactive(input$intInput1,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_preg <- whichone(input$goint, Data_Int_new()$Prepreg_Int, Data_Int$Prepreg_Int)
    int_preg[int_name %in% input$intInput1]  })
  
  intprepregfetch2 <- eventReactive(input$intInput2,{
    int_name <- whichone(input$goint, Data_Int_new()$Name_Int, Data_Int$Name_Int)
    int_preg <- whichone(input$goint, Data_Int_new()$Prepreg_Int, Data_Int$Prepreg_Int)
    int_preg[int_name %in% input$intInput2]  })
  
  #Generate Outputs
  output$intname1d  <- output$intname1 <- renderText(intnamefetch1())
  output$intname2d  <- output$intname2 <- renderText(intnamefetch2())
  output$intEnergyNum1 <- renderText({paste(intenergyfetch1(), "MJ/kg")})
  output$intEnergyNum2 <- renderText({paste(intenergyfetch2(), "MJ/kg")})
  output$intscrapNum1 <- renderText({paste(intscrapfetch1()*100, "%")})
  output$intscrapNum2 <- renderText({paste(intscrapfetch2()*100, "%")})
  int.prepregYN1 <-renderText(intprepregfetch1())
  int.prepregYN2<-renderText(intprepregfetch2())
  
  # Matrix ----
  # Make List for Box if no custom data
  updateSelectizeInput(session, 'PriMatrixInput1',
                         choices = primatrixnames,
                         selected = "",
                         server = TRUE)
  updateSelectizeInput(session, 'PriMatrixInput2',
                       choices = primatrixnames,
                       selected = "",
                       server = TRUE)
  # Make dataframe for Box if  custom data
Data_Primatrix_new  <- reactiveValues()
  Data_Primatrix_new  <- eventReactive(input$gomatrix, {
    matrix.name <- input$matrix_add
    matrix.Ener <- as.double(input$matrix_add_E)
    matrix.type <- "Matrix"
    
    df <- isolate(Data_Primatrix) %>%
      add_row(Name_Matrix = matrix.name,
              Energy_Matrix = matrix.Ener,
              Type_Matrix = matrix.type)
  })
  
  # Make List for Box if  custom data
  observeEvent(input$gomatrix, { 
    updateSelectizeInput(session, 'PriMatrixInput1',
                         choices = Data_Primatrix_new()$Name_Matrix,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'PriMatrixInput2',
                         choices = Data_Primatrix_new()$Name_Matrix,
                         selected = "",
                         server = TRUE)
  })
  
  #Match Names to Table
  primatrixnamefetch1 <- eventReactive(input$PriMatrixInput1,{
    matrixnames_new()[matrixnames_new() %in% input$PriMatrixInput1]  })
  
  primatrixnamefetch2 <- eventReactive(input$PriMatrixInput2,{
    matrixnames_new()[matrixnames_new()  %in% input$PriMatrixInput2]  })
  
  #Match Energy to Name
  primatrixenergyfetch1 <- eventReactive(input$PriMatrixInput1,{
    matrixenergy_new()[matrixnames_new()  %in% input$PriMatrixInput1]  })
  
  primatrixenergyfetch2 <- eventReactive(input$PriMatrixInput2,{
    matrixenergy_new()[matrixnames_new()  %in% input$PriMatrixInput2]  })
  
  # Generate Outputs
  output$primatrixname1 <- renderText(primatrixnamefetch1())
  output$primatrixname2 <- renderText(primatrixnamefetch2())
  output$primatrixEnergyNum1 <- renderText({paste(primatrixenergyfetch1(), "MJ/kg")})
  output$primatrixEnergyNum2 <- renderText({paste(primatrixenergyfetch2(), "MJ/kg")})
  
  # OtherMat ----
  # Build temp and new data tables for when adding custom data
  Data_Primatrix_temp  <- reactiveValues()
  Data_Primatrix_temp  <- eventReactive(input$gomatrix, {
    matrix.name <- input$matrix_add
    matrix.Ener <- as.double(input$matrix_add_E)
    matrix.type <- "Matrix"
    
    df <- isolate(Data_Primatrix) %>%
      add_row(Name_Matrix = matrix.name,
              Energy_Matrix = matrix.Ener,
              Type_Matrix = matrix.type)
  })
  
  Data_Additive_temp  <- reactiveValues()
  Data_Additive_temp  <- eventReactive(input$goadditive, {
    add.name <- input$additive_add
    add.Ener <- as.double(input$additive_add_E)
    add.type <- "Additive"
    
    df <- isolate(Data_Additive) %>%
      add_row(Name_Matrix = add.name,
              Energy_Matrix = add.Ener,
              Type_Matrix = add.type)
  })
  
  Data_Filler_temp  <- reactiveValues()
  Data_Filler_temp  <- eventReactive(input$gofiller, {
    filler.name <- input$filler_add
    filler.Ener <- as.double(input$filler_add_E)
    filler.type <- "Filler"
    
    df <- isolate(Data_Filler) %>%
      add_row(Name_Matrix = filler.name,
              Energy_Matrix = filler.Ener,
              Type_Matrix = filler.type)
  })
  
 Data_MatrixM_new <- reactive(BuildnewMatrix.df(input$gomatrix, input$goadditive, input$gofiller, Data_Primatrix_temp() , Data_Primatrix, Data_Additive_temp(), Data_Additive, Data_Filler_temp(), Data_Filler, Data_Other))
  
 Data_Primatrix_new <- reactive(subset(Data_MatrixM_new(), Type_Matrix == "Matrix"))
 Data_Additive_new <- reactive(subset(Data_MatrixM_new(), Type_Matrix == "Additive"))
 Data_Filler_new <- reactive(subset(Data_MatrixM_new(), Type_Matrix == "Filler"))
 
 matrixnames_new <- reactive(Data_MatrixM_new()[,1])
 matrixenergy_new <- reactive(Data_MatrixM_new()[,2])

 # Build selection boxes
  observeEvent(input$types1a, {
   list1a <- othermatfxn(input$types1a, Data_Primatrix_new()[,1], Data_Additive_new()[,1], Data_Filler_new()[,1])
   updateSelectizeInput(session, 'OtherMatrixAInput1',
                        choices = list1a,
                        selected = "Not Used",
                        server = TRUE)})

   observeEvent(input$types1b, {
    list1b <-  othermatfxn(input$types1b, Data_Primatrix_new()[,1], Data_Additive_new()[,1], Data_Filler_new()[,1])
    updateSelectizeInput(session, 'OtherMatrixBInput1',
                         choices = list1b,
                         selected = "Not Used",
                         server = TRUE)})
  
  observeEvent(input$types1c, {
    list1c <-  othermatfxn(input$types1c, Data_Primatrix_new()[,1], Data_Additive_new()[,1], Data_Filler_new()[,1])
    updateSelectizeInput(session, 'OtherMatrixCInput1',
                         choices = list1c,
                         selected = "Not Used",
                         server = TRUE)})
  
  observeEvent(input$types2a, {
    list2a <-  othermatfxn(input$types2a, Data_Primatrix_new()[,1], Data_Additive_new()[,1], Data_Filler_new()[,1])
    updateSelectizeInput(session, 'OtherMatrixAInput2',
                         choices = list2a,
                         selected = "Not Used",
                         server = TRUE) })
  
  observeEvent(input$types2b, {
    list2b <-  othermatfxn(input$types2b, Data_Primatrix_new()[,1], Data_Additive_new()[,1], Data_Filler_new()[,1])
    updateSelectizeInput(session, 'OtherMatrixBInput2',
                         choices = list2b,
                         selected = "Not Used",
                         server = TRUE)})
  
  observeEvent(input$types2c, {
    list2c <-  othermatfxn(input$types2c, Data_Primatrix_new()[,1], Data_Additive_new()[,1], Data_Filler_new()[,1])
    updateSelectizeInput(session, 'OtherMatrixCInput2',
                         choices = list2c,
                         selected = "Not Used",
                         server = TRUE)})
  
  # Associate Name with Table
  othermatrixAnamefetch1 <- eventReactive(input$OtherMatrixAInput1, {
    matrixnames_new()[matrixnames_new() %in% input$OtherMatrixAInput1]  })

  othermatrixBnamefetch1 <- eventReactive(input$OtherMatrixBInput1, {
    matrixnames_new()[matrixnames_new() %in% input$OtherMatrixBInput1]  })
  
  othermatrixCnamefetch1 <- eventReactive(input$OtherMatrixCInput1, {
    matrixnames_new()[matrixnames_new() %in% input$OtherMatrixCInput1]  })
  
  othermatrixAnamefetch2 <- eventReactive(input$OtherMatrixAInput2, {
    matrixnames_new()[matrixnames_new() %in% input$OtherMatrixAInput2]  })
  
  othermatrixBnamefetch2 <- eventReactive(input$OtherMatrixBInput2, {
    matrixnames_new()[matrixnames_new() %in% input$OtherMatrixBInput2]  })
  
  othermatrixCnamefetch2 <- eventReactive(input$OtherMatrixCInput2, {
    matrixnames_new()[matrixnames_new() %in% input$OtherMatrixCInput2]  })
  
 # Associate Energy with Name
  othermatrixAenergyfetch1 <- eventReactive(input$OtherMatrixAInput1, {
    matrixenergy_new()[matrixnames_new() %in% input$OtherMatrixAInput1]  })
  
  othermatrixBenergyfetch1 <- eventReactive(input$OtherMatrixBInput1, {
    matrixenergy_new()[matrixnames_new() %in% input$OtherMatrixBInput1]  })
  
  othermatrixCenergyfetch1 <- eventReactive(input$OtherMatrixCInput1, {
    matrixenergy_new()[matrixnames_new() %in% input$OtherMatrixCInput1]  })
  
  othermatrixAenergyfetch2 <- eventReactive(input$OtherMatrixAInput2, {
    matrixenergy_new()[matrixnames_new() %in% input$OtherMatrixAInput2]  })
  
  othermatrixBenergyfetch2 <- eventReactive(input$OtherMatrixBInput2, {
    matrixenergy_new()[matrixnames_new() %in% input$OtherMatrixBInput2]  })
  
  othermatrixCenergyfetch2 <- eventReactive(input$OtherMatrixCInput2, {
    matrixenergy_new()[matrixnames_new() %in% input$OtherMatrixCInput2]  })
  
  # Generate  Outputs
  output$othermatrixAname1 <- renderText(othermatrixAnamefetch1())
  output$othermatrixBname1 <- renderText(othermatrixBnamefetch1())
  output$othermatrixCname1 <- renderText(othermatrixCnamefetch1())
  output$othermatrixAname2 <- renderText(othermatrixAnamefetch2())
  output$othermatrixBname2 <- renderText(othermatrixBnamefetch2())
  output$othermatrixCname2 <- renderText(othermatrixCnamefetch2())
  
  output$othermatrixAEnergyNum1 <- renderText({paste(othermatrixAenergyfetch1(), "MJ/kg")})
  output$othermatrixBEnergyNum1 <- renderText({paste(othermatrixBenergyfetch1(), "MJ/kg")})
  output$othermatrixCEnergyNum1 <- renderText({paste(othermatrixCenergyfetch1(), "MJ/kg")})
  output$othermatrixAEnergyNum2 <-  renderText({paste(othermatrixAenergyfetch2(), "MJ/kg")})
  output$othermatrixBEnergyNum2 <- renderText({paste(othermatrixBenergyfetch2(), "MJ/kg")})
  output$othermatrixCEnergyNum2 <- renderText({paste(othermatrixCenergyfetch2(), "MJ/kg")})
  
  
  # Inserts ----
    # Make Insert List if no custom data
  updateSelectizeInput(session, 'InsertsAInput1',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  updateSelectizeInput(session, 'InsertsBInput1',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  updateSelectizeInput(session, 'InsertsAInput2',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  updateSelectizeInput(session, 'InsertsBInput2',
                       choices = insertsnames,
                       selected = "Not Used",
                       server = TRUE)
  
  # Make Inserts dataframe if custom data
  Data_Insert_new  <- eventReactive(input$goinsert, {
    insert.name <- input$insert_add
    insert.Ener <- as.double(input$insert_add_E)
    
    df <- isolate(Data_Insert) %>%
      add_row(Name_Inserts = insert.name,
              Energy_Inserts = insert.Ener)
  })
  
  # Make Inserts list for box if custom data
  observeEvent(input$goinsert, {
    updateSelectizeInput(session, 'InsertsAInput1',
                         choices = Data_Insert_new()$Name_Inserts,
                         selected = "Not Used",
                         server = TRUE)
    updateSelectizeInput(session, 'InsertsBInput1',
                         choices = Data_Insert_new()$Name_Inserts,
                         selected = "Not Used",
                         server = TRUE)
    updateSelectizeInput(session, 'InsertsAInput2',
                         choices = Data_Insert_new()$Name_Inserts,
                         selected = "Not Used",
                         server = TRUE)
    updateSelectizeInput(session, 'InsertsBInput2',
                         choices = Data_Insert_new()$Name_Inserts,
                         selected = "Not Used",
                         server = TRUE)
  })
  
  # Associate Names To Table
  insertsAnamefetch1  <- eventReactive(input$InsertsAInput1,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_name[ins_name %in% input$InsertsAInput1]  })
  output$insertsAname1 <- renderText(insertsAnamefetch1())
  
  insertsBnamefetch1  <- eventReactive(input$InsertsBInput1,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_name[ins_name %in% input$InsertsBInput1]  })
  output$insertsBname1 <- renderText(insertsBnamefetch1())
  
  insertsAnamefetch2  <- eventReactive(input$InsertsAInput2,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_name[ins_name %in% input$InsertsAInput2]  })
  output$insertsAname2 <- renderText(insertsAnamefetch2())
  
  insertsBnamefetch2  <- eventReactive(input$InsertsBInput2,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_name[ins_name %in% input$InsertsBInput2]  })
  output$insertsBname2 <- renderText(insertsBnamefetch2())
  
  #Associate Energy to Table
  insertsAenergyfetch1 <- eventReactive(input$InsertsAInput1,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_energy <- whichone(input$goinsert, Data_Insert_new()$Energy_Inserts, Data_Insert$Energy_Inserts)
    ins_energy[ins_name %in% input$InsertsAInput1]  })
  output$insertsAEnergyNum1 <- renderText({paste(insertsAenergyfetch1(), "MJ/kg")})
  
  insertsBenergyfetch1 <- eventReactive(input$InsertsBInput1,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_energy <- whichone(input$goinsert, Data_Insert_new()$Energy_Inserts, Data_Insert$Energy_Inserts)
    ins_energy[ins_name %in% input$InsertsBInput1]  })
  output$insertsBEnergyNum1 <- renderText({paste(insertsBenergyfetch1(), "MJ/kg")})
  
  
  insertsAenergyfetch2 <- eventReactive(input$InsertsAInput2,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_energy <- whichone(input$goinsert, Data_Insert_new()$Energy_Inserts, Data_Insert$Energy_Inserts)
    ins_energy[ins_name %in% input$InsertsAInput2]  })
  output$insertsAEnergyNum2 <- renderText({paste(insertsAenergyfetch2(), "MJ/kg")})
  
  insertsBenergyfetch2 <- eventReactive(input$InsertsBInput2,{
    ins_name <- whichone(input$goinsert, Data_Insert_new()$Name_Inserts, Data_Insert$Name_Inserts)
    ins_energy <- whichone(input$goinsert, Data_Insert_new()$Energy_Inserts, Data_Insert$Energy_Inserts)
    ins_energy[ins_name %in% input$InsertsBInput2]  })
  output$insertsBEnergyNum2 <- renderText({paste(insertsBenergyfetch2(), "MJ/kg")})

  # Cure ----
  # Make List for Box dependent on molding process choice
   observeEvent({
     input$cureYN1
     input$moldingInput1} ,{
     curelist1 <- curelistfxn(input$moldingInput1, allcure, onlymoldcure, wetcure, vbcure)
     if (input$cureYN1 == TRUE) {
     updateSelectizeInput(session, 'cureInput1',
                          choices = allcure,
                          selected = "",
                          server = TRUE)
   }
   else 
     updateSelectizeInput(session, 'cureInput1',
                          choices = curelist1,
                          selected = "",
                          server = TRUE)
  })
  
  observeEvent({
    input$cureYN2
    input$moldingInput2} ,{
      curelist2 <- curelistfxn(input$moldingInput2, allcure, onlymoldcure, wetcure, vbcure)
      if (input$cureYN2 == TRUE) {
        updateSelectizeInput(session, 'cureInput2',
                             choices = allcure,
                             selected = "",
                             server = TRUE)
      }
      else 
        updateSelectizeInput(session, 'cureInput2',
                             choices = curelist2,
                             selected = "",
                             server = TRUE)
    })
  
  # Make dataframe for box if custom values used
  Data_Cure_new  <- reactiveValues()
  Data_Cure_new  <- eventReactive(input$gocure, {
    cure.name <- as.character(input$cure_add)
    cure.E.calc <- calcenergy(input$cure_add_E_m, input$cure_add_E_P_M, input$cure_add_E_per_M, input$cure_add_E_t_M, input$cure_add_E_P_p, input$cure_add_E_per_p, input$cure_add_E_t_p,
                            input$cure_add_E_P_c, input$cure_add_E_per_c, input$cure_add_E_t_c,input$cure_add_E_P_h, input$cure_add_E_per_h, input$cure_add_E_t_h,
                            input$cure_add_E_P_o, input$cure_add_E_per_o, input$cure_add_E_t_o)
    cure.Ener <- whichenergy(input$cure_add_EYN, cure.E.calc, input$cure_add_E_Y)
    
    df <- isolate(Data_Cure) %>%
      add_row(Name_Cure = cure.name,
              Energy_Cure = cure.Ener)
  })
  calccureE <- reactive(calcenergy(input$cure_add_E_m, input$cure_add_E_P_M, input$cure_add_E_per_M, input$cure_add_E_t_M, input$cure_add_E_P_p, input$cure_add_E_per_p, input$cure_add_E_t_p,
                          input$cure_add_E_P_c, input$cure_add_E_per_c, input$cure_add_E_t_c,input$cure_add_E_P_h, input$cure_add_E_per_h, input$cure_add_E_t_h,
                          input$cure_add_E_P_o, input$cure_add_E_per_o, input$cure_add_E_t_o))
  output$calcedcureE <- renderText({paste(calccureE(), "MJ/kg")})
  
  # Make list for box if custom values used
  observeEvent(input$gocure, { 
    updateSelectizeInput(session, 'cureInput1',
                         choices = Data_Cure_new()$Name_Cure,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'cureInput2',
                         choices = Data_Cure_new()$Name_Cure,
                         selected = "",
                         server = TRUE)
  })
  
  #Match Names to Table
  curenamefetch1 <- eventReactive(input$cureInput1,{
    c_name <- whichone(input$gocure, Data_Cure_new()$Name_Cure, Data_Cure$Name_Cure)
    c_name[c_name %in% input$cureInput1]  })
  
  curenamefetch2 <- eventReactive(input$cureInput2,{
    c_name <- whichone(input$gocure, Data_Cure_new()$Name_Cure, Data_Cure$Name_Cure)
    c_name[c_name %in% input$cureInput2]  })
  
  #Match Energy to Table
  cureenergyfetch1 <- eventReactive(input$cureInput1,{
    c_name <- whichone(input$gocure, Data_Cure_new()$Name_Cure, Data_Cure$Name_Cure)
    c_energy <- whichone(input$gocure, Data_Cure_new()$Energy_Cure, Data_Cure$Energy_Cure)
    c_energy[c_name %in% input$cureInput1]  })
  
  cureenergyfetch2 <- eventReactive(input$cureInput2,{
    c_name <- whichone(input$gocure, Data_Cure_new()$Name_Cure, Data_Cure$Name_Cure)
    c_energy <- whichone(input$gocure, Data_Cure_new()$Energy_Cure, Data_Cure$Energy_Cure)
    c_energy[c_name %in% input$cureInput2]  })
  
 # Generate Outputs
  output$curename1d  <- output$curename1 <- renderText(curenamefetch1())
  output$curename2d  <- output$curename2 <- renderText(curenamefetch2())
  output$cureEnergyNum1 <- renderText({paste(cureenergyfetch1(), "MJ/kg")})
  output$cureEnergyNum2 <- renderText({paste(cureenergyfetch2(), "MJ/kg")})
  
  # Finish ----
  # Make List for Box
  updateSelectizeInput(session, 'finishInput1',
                       choices = finishnames,
                       selected = "None",
                       server = TRUE)
  
  updateSelectizeInput(session, 'finishInput2',
                       choices = finishnames,
                       selected = "None",
                       server = TRUE)
  
  #Make Dataframe for box if custom balues used
  Data_Finish_new  <- reactiveValues()
  Data_Finish_new  <- eventReactive(input$gofinish, {
    finish.name <- as.character(input$finish_add)
    finish.Ener <- as.double(input$finish_add_E)
    
    df <- isolate(Data_Finish) %>%
      add_row(Name_Finishing = finish.name,
              Energy_Finishing = finish.Ener)
  })
  
  #Make list for box if custom balues used
  observeEvent(input$gofinish, { 
    updateSelectizeInput(session, 'finishInput1',
                         choices = Data_Finish_new()$Name_Finishing,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'finishInput2',
                         choices = Data_Finish_new()$Name_Finishing,
                         selected = "",
                         server = TRUE)
  })
  
  #Match Names to Table
  finishnamefetch1 <- eventReactive(input$finishInput1,{
    fn_name <- whichone(input$gofinish, Data_Finish_new()$Name_Finishing, Data_Finish$Name_Finishing)
    fn_name[fn_name %in% input$finishInput1]  })
  
  finishnamefetch2 <- eventReactive(input$finishInput2,{
    fn_name <- whichone(input$gofinish, Data_Finish_new()$Name_Finishing, Data_Finish$Name_Finishing)
    fn_name[fn_name %in% input$finishInput2]  })
  
  #Match Energy to Table
  finishenergyfetch1 <- eventReactive(input$finishInput1,{
    fn_name <- whichone(input$gofinish, Data_Finish_new()$Name_Finishing, Data_Finish$Name_Finishing)
    fn_energy <- whichone(input$gofinish, Data_Finish_new()$Energy_Finishing, Data_Finish$Energy_Finishing)
    fn_energy[fn_name %in% input$finishInput1]  })
  
  finishenergyfetch2 <- eventReactive(input$finishInput2,{
    fn_name <- whichone(input$gofinish, Data_Finish_new()$Name_Finishing, Data_Finish$Name_Finishing)
    fn_energy <- whichone(input$gofinish, Data_Finish_new()$Energy_Finishing, Data_Finish$Energy_Finishing)
    fn_energy[fn_name %in% input$finishInput2]  })
 
  # Generate Outputs 
  output$finishname1d  <- output$finishname1 <- renderText(finishnamefetch1())
  output$finishname2d  <- output$finishname2 <- renderText(finishnamefetch2())
  output$finishEnergyNum1 <- renderText({paste(finishenergyfetch1(), "MJ/kg")})
  output$finishEnergyNum2 <- renderText({paste(finishenergyfetch2(), "MJ/kg")})
  
  # Change Fiber fraction, yield and scrap values to default based on molding process and intermediate type ----

  
    observeEvent(input$moldingInput1,{
    ff1 <- moldfracfetch1()
    moldy1 <-  moldyieldfetch1()
    updateNumericInput(session, "moldfracUSERNum1", value = (ff1*100)) 
    updateNumericInput(session, "moldyieldUSERNum1", value = (moldy1*100)) 
  })
 
 observeEvent(input$intInput1,{
   ints1 <-  intscrapfetch1()
       updateNumericInput(session, "intscrapUSERNum1", value = (ints1*100)) 
 })
 
 observeEvent(input$moldingInput2,{
   ff2 <- moldfracfetch2()
   moldy2 <-  moldyieldfetch2()
   updateNumericInput(session, "moldfracUSERNum2", value = (ff2*100)) 
   updateNumericInput(session, "moldyieldUSERNum2", value = (moldy2*100) )
 })

  observeEvent(input$intInput2,{
   ints2 <-  intscrapfetch2()
        updateNumericInput(session, "intscrapUSERNum2", value = (ints2*100)) 
 })
 
 #If adding custom molding process
 
 observeEvent(input$gomold, {
   updateCheckboxInput(session, "intYN1", value = 1)
   updateCheckboxInput(session, "intYN2", value = 1)
   updateCheckboxInput(session, "cureYN1", value = 1)
   updateCheckboxInput(session, "cureYN2", value = 1)
 })
 
  # Return Mass/Massfracs to zero if "Use ..." checkboxes are false ----
 #Set Inserts mass to zero
 observeEvent(input$insertsAUSERYN1, {
   if (!input$insertsAUSERYN1){
     updateNumericInput(session, "insertsAfrac1", value = (0)) 
   updateNumericInput(session, "insertsBfrac1", value = (0)) }
                 })
 
 observeEvent(input$insertsBUSERYN1, {
   if (!input$insertsBUSERYN1){
     updateNumericInput(session, "insertsBfrac1", value = (0)) }
    })
 
 observeEvent(input$insertsAUSERYN2, {
   if (!input$insertsAUSERYN2){
     updateNumericInput(session, "insertsAfrac2", value = (0)) 
     updateNumericInput(session, "insertsBfrac2", value = (0)) }
  })
 
 observeEvent(input$insertsBUSERYN2, {
   if (!input$insertsBUSERYN2){
     updateNumericInput(session, "insertsBfrac2", value = (0)) }
  })
 
 # set Additional Matrix fractions to zero, choices to "Not Used" and other "Use Additional Matrix" to false
 observeEvent(input$othermatrixAUSERYN1, {
   if (!input$othermatrixAUSERYN1){
   updateNumericInput(session, "othermatrixAfrac1", value = (0)) 
        updateSelectizeInput(session, 'types1a',
                        choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = "Not Used",
                        server = TRUE)
     updateSelectizeInput(session, 'OtherMatrixAInput1',
                        choices = "Not Used",
                        selected = "Not Used",
                        server = TRUE)
     updateCheckboxInput(session, "othermatrixBUSERYN1", value = FALSE)
     updateCheckboxInput(session, "othermatrixCUSERYN1", value = FALSE)
 }})
 
 observeEvent(input$othermatrixBUSERYN1, {
   if (!input$othermatrixBUSERYN1){
     updateNumericInput(session, "othermatrixBfrac1", value = (0)) 
   updateSelectizeInput(session, 'types1b',
                        choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = "Not Used",
                        server = TRUE)
   updateSelectizeInput(session, 'OtherMatrixbInput1',
                        choices = "Not Used",
                        selected = "Not Used",
                        server = TRUE)
   updateCheckboxInput(session, "othermatrixCUSERYN1", value = FALSE)
 }})
 
 observeEvent(input$othermatrixCUSERYN1, {
   if (!input$othermatrixCUSERYN1){
     updateNumericInput(session, "othermatrixCfrac1", value = (0)) 
   updateSelectizeInput(session, 'types1c',
                        choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = "Not Used",
                        server = TRUE)
   updateSelectizeInput(session, 'OtherMatrixcInput1',
                        choices = "Not Used",
                        selected = "Not Used",
                        server = TRUE)
 }})
 
 observeEvent(input$othermatrixAUSERYN2, {
   if (!input$othermatrixAUSERYN2){
     updateNumericInput(session, "othermatrixAfrac2", value = (0)) 
   updateSelectizeInput(session, 'types2a',
                        choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = "Not Used",
                        server = TRUE)
   updateSelectizeInput(session, 'OtherMatrixAInput2',
                        choices = "Not Used",
                        selected = "Not Used",
                        server = TRUE)
   updateCheckboxInput(session, "othermatrixBUSERYN2", value = FALSE)
   updateCheckboxInput(session, "othermatrixCUSERYN2", value = FALSE)
   }})
 
 observeEvent(input$othermatrixBUSERYN2, {
   if (!input$othermatrixBUSERYN2){
     updateNumericInput(session, "othermatrixBfrac2", value = (0)) 
   updateSelectizeInput(session, 'types2b',
                        choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = "Not Used",
                        server = TRUE)
   updateSelectizeInput(session, 'OtherMatrixBInput2',
                        choices = "Not Used",
                        selected = "Not Used",
                        server = TRUE)
   updateCheckboxInput(session, "othermatrixCUSERYN2", value = FALSE)
 }})
 
 observeEvent(input$othermatrixCUSERYN2, {
   if (!input$othermatrixCUSERYN2){
     updateNumericInput(session, "othermatrixCfrac2", value = (0)) 
   updateSelectizeInput(session, 'types2c',
                        choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = "Not Used",
                        server = TRUE)
   updateSelectizeInput(session, 'OtherMatrixCInput2',
                        choices = "Not Used",
                        selected = "Not Used",
                        server = TRUE)
   }})
 
  # Change User Input --> Variables ----
    # Yield
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
    raw.f.mb1 <- reactive({input$othermatrixBfrac1/100})
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
    
  # Set primary matrix mass fraction to 1 - the used fiber fraction (raw) ----
    observe({
      rff1 <- input$moldfracUSERNum1
      rff2 <- input$moldfracUSERNum2
    updateNumericInput(session, "primatrixfrac1", value = 100-rff1)
    updateNumericInput(session, "primatrixfrac2", value = 100-rff2)
    })
    
  # Calculations ----
    # Mass Fractions converion (calculations treats inserts as part of sum(mass fractions) = 1 )----  
    raw.to.actual.fracs1 <- reactive(Data_mass_fxn(finalweight1(), raw.f.f1(), raw.f.pm1(), raw.f.ma1(), raw.f.mb1(), raw.f.mc1(), m.ia1(), m.ib1()))
    raw.to.actual.fracs2 <- reactive(Data_mass_fxn(finalweight2(), raw.f.f2(), raw.f.pm2(), raw.f.ma2(), raw.f.mb2(), raw.f.mc2(), m.ia2(), m.ib2()))
    
     f.raw.sum1 <- reactive(sum(raw.f.f1(), raw.f.pm1(), raw.f.ma1(), raw.f.mb1(), raw.f.mc1()))
     f.raw.sum2 <- reactive(sum(raw.f.f2(), raw.f.pm2(), raw.f.ma2(), raw.f.mb2(), raw.f.mc2()))
     m.inserts1 <- reactive(sum(m.ia1(), m.ib1()))
     m.inserts2 <- reactive(sum(m.ia2(), m.ib2()))
      
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

    # Build Cumulative Yield dataframe ----  
    # yield_data1.df <- reactiveValues()
    # yield_data2.df <- reactiveValues()
    
    yield_data1.df <- reactive(BIGFUNCTION1(partname1e(),
      finish.yield1(), mold.yield1(), layup.yield1(),
      f.f1(), f.pm1(), f.ma1(), f.mb1(), f.mc1(), f.ia1(), f.ib1(),
      int.prepregYN1(), finalweight1()
      ))
    
    yield_data2.df <- reactive(BIGFUNCTION1(partname2e(),
      finish.yield2(), mold.yield2(), layup.yield2(),
      f.f2(), f.pm2(), f.ma2(), f.mb2(), f.mc2(), f.ia2(), f.ib2(),
      int.prepregYN2(), finalweight2()
    ))   

    # Energy Variables ----
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
    
    # Build Energy use dataframe ----
    # energy_data1.df <- reactiveValues()
    # energy_data2.df <- reactiveValues()
    
    energy_data1.df <- reactive(BIGFUNCTION2(yield_data1.df(),partname1e(),
             f.pm1(), f.ma1(), f.mb1(), f.mc1(), f.ia1(), f.ib1(),
             E.fib1(), E.int1(), E.pm1(), E.ma1(), E.mb1(), E.mc1(),
             E.ia1(), E.ib1(), E.mold1(), E.cure1(), E.fin1()
    )) 
    
    energy_data2.df <- reactive(BIGFUNCTION2(yield_data2.df(),partname2e(),
            f.pm2(), f.ma2(), f.mb2(), f.mc2(), f.ia2(), f.ib2(),
            E.fib2(), E.int2(), E.pm2(), E.ma2(), E.mb2(), E.mc2(),
            E.ia2(), E.ib2(), E.mold2(), E.cure2(), E.fin2()
    )) 
    
    #code to test that calculations are preformed correctly
    # output$table1a <- renderTable(yield_data1.df())
    # output$table1b <- renderTable(energy_data1.df())
    # 
    # output$table2a <- renderTable(yield_data2.df())
    # output$table2b <- renderTable(energy_data2.df())
    
    
  # Final Display ----
    # Reactive Names for display table ----
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
    Mat1.E.df <- reactive({  
      validate(
          need(f.raw.sum1() == 1, "Sum of Mass Fractions should add to 100%")
        , need(m.inserts1() < finalweight1(), "Inserts should not weigh more than the final part")
        , need(raw.f.f1() >= 0, "Mass Fractions must not be negative")
        , need(raw.f.pm1() >= 0, "Mass Fractions must not be negative")
        , need(raw.f.ma1() >= 0, "Mass Fractions must not be negative")
        , need(raw.f.mb1() >= 0, "Mass Fractions must not be negative")
        , need(raw.f.mc1() >= 0, "Mass Fractions must not be negative")
        , need(E.fib1(), "Please chose a fiber type")  
        , need(E.pm1(), "Please chose a primary matrix material")   
        , need(E.int1(), 'Please chose a fiber intermediate ("Not Used" is an option)')    
        , need(E.mold1(), "Please chose a molding option") 
        , need(E.cure1(), 'Please chose a curing option ("Cures in mold" is an option)')
           )
     
     data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total"),
      Choice = c(n.f1(), n.pm1(), n.ma1(), n.mb1(), n.mc1(), n.ia1(), n.ib1(), " "),
      "Effecive Mass Fraction" = c(f.f1()*100  , f.pm1()*100 , f.ma1()*100 , f.mb1()*100 , f.mc1()*100 , f.ia1()*100 , f.ib1()*100 , sum(f.f1()*100  , f.pm1()*100 , f.ma1()*100 , f.mb1()*100 , f.mc1()*100 , f.ia1()*100 , f.ib1()*100 )),
      "Embodied Energy (MJ/part)" = c(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1(), sum(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()))
    )})
    
    Process1.E.df <- reactive({
      validate(
          need(f.raw.sum1() == 1, "")
        , need(m.inserts1() < finalweight1(), "")
        , need(raw.f.f1() >= 0, "")
        , need(raw.f.pm1() >= 0, "")
        , need(raw.f.ma1() >= 0, "")
        , need(raw.f.mb1() >= 0, "")
        , need(raw.f.mc1() >= 0, "")
        , need(E.fib1(), "")  
        , need(E.pm1(), "")   
        , need(E.int1(), '')    
        , need(E.mold1(), "") 
        , need(E.cure1(), '')
          )
      
     data_frame(
      Process = c("Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
      Choice =c(n.int1(), n.mold1(), n.cure1(), n.fin1(), " "),
      Yield = c(layup.yield1()*100, mold.yield1()*100, " -- " , finish.yield1()*100, (layup.yield1()* mold.yield1()* finish.yield1()*100)),
      "Embodied Energy (MJ/part)" = c(E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(), sum(E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1()))
    )})
    
    Mat2.E.df <- reactive({
        validate(
            need(f.raw.sum2() == 1, "Sum of Mass Fractions should add to 100%")
          , need(m.inserts2() < finalweight2(), "Inserts should not weigh more than the final part")
          , need(raw.f.f2() >= 0, "Mass Fractions must not be negative")
          , need(raw.f.pm2() >= 0, "Mass Fractions must not be negative")
          , need(raw.f.ma2() >= 0, "Mass Fractions must not be negative")
          , need(raw.f.mb2() >= 0, "Mass Fractions must not be negative")
          , need(raw.f.mc2() >= 0, "Mass Fractions must not be negative")
          , need(E.fib2(), "Please chose a fiber type")  
          , need(E.pm2(), "Please chose a primary matrix material")   
          , need(E.int2(), 'Please chose a fiber intermediate ("Not Used" is an option)')    
          , need(E.mold2(), "Please chose a molding option") 
          , need(E.cure2(), 'Please chose a curing option ("Cures in mold" is an option)')
                )
        
     data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total"),
      Choice = c(n.f2(), n.pm2(), n.ma2(), n.mb2(), n.mc2(), n.ia2(), n.ib2(), " "),
      "Effective Mass Fraction" = c(f.f2()*100  , f.pm2()*100 , f.ma2()*100 , f.mb2()*100 , f.mc2()*100 , f.ia2()*100 , f.ib2()*100 , sum(f.f2()*100  , f.pm2()*100 , f.ma2()*100 , f.mb2()*100 , f.mc2()*100 , f.ia2()*100 , f.ib2()*100)),
      "Embodied Energy (MJ/part)" = c(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2(), sum(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()))
    )})
    
    Process2.E.df <- reactive({
          validate(
              need(f.raw.sum2() == 1, "")
            , need(m.inserts2() < finalweight2(), "")
            , need(raw.f.f2() >= 0, "")
            , need(raw.f.pm2() >= 0, "")
            , need(raw.f.ma2() >= 0, "")
            , need(raw.f.mb2() >= 0, "")
            , need(raw.f.mc2() >= 0, "")
            , need(E.fib2(), "")  
            , need(E.pm2(), "")   
            , need(E.int2(), '')    
            , need(E.mold2(), "") 
            , need(E.cure2(), '')
      )
      
      data_frame(
      Process = c("Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
      Choice = list(n.int2(), n.mold2(), n.cure2(), n.fin2(), " "),
      Yield = c(layup.yield2()*100, mold.yield2()*100, " -- " , finish.yield2()*100, (layup.yield2()*mold.yield2()* finish.yield2()*100)),
      "Embodied Energy (MJ/part)" = c(E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2(), sum(E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2()))
    )})
    
    #Final Tables
    output$Table.mat.1 <- renderTable (Mat1.E.df(), signif = 2)
    output$Table.pro.1 <- renderTable(Process1.E.df(), signif = 2)
    output$Table.mat.2 <- renderTable(Mat2.E.df(), signif = 2)
    output$Table.pro.2 <- renderTable(Process2.E.df(), signif = 2)
  
    
    # Final Graph ----
   energy_plot.df <- reactive({
     validate(
         need(f.raw.sum1() == 1, "Sum of Mass Fractions (1) should add to 100%")
       , need(m.inserts1() < finalweight1(), "Inserts (1) should not weigh more than the final part")
       , need(raw.f.f1() >= 0, "Mass Fractions (Fiber-1) must not be negative")
       , need(raw.f.pm1() >= 0, "Mass Fractions (Primary Matrix -1) must not be negative")
       , need(raw.f.ma1() >= 0, "Mass Fractions (Matrix 1A) must not be negative")
       , need(raw.f.mb1() >= 0, "Mass Fractions (Matrix 1B) must not be negative")
       , need(raw.f.mc1() >= 0, "Mass Fractions (Matrix 1C) must not be negative")
       , need(E.fib1(), "Please chose a fiber type (1)")  
       , need(E.pm1(), "Please chose a primary matrix material(1)")   
       , need(E.int1(), 'Please chose a fiber intermediate (1) ("Not Used" is an option)')    
       , need(E.mold1(), "Please chose a molding option (1)") 
       , need(E.cure1(), 'Please chose a curing option (1) ("Cures in mold" is an option)')
       , need(f.raw.sum2() == 1, "Sum of Mass Fractions (2) should add to 100%")
       , need(m.inserts2() < finalweight2(), "Inserts (2) should not weigh more than the final part")
       , need(raw.f.f2() >= 0, "Mass Fractions (Fiber-2) must not be negative")
       , need(raw.f.pm2() >= 0, "Mass Fractions (Primary Matrix -2) must not be negative")
       , need(raw.f.ma2() >= 0, "Mass Fractions (Matrix 2A) must not be negative")
       , need(raw.f.mb2() >= 0, "Mass Fractions (Matrix 2B) must not be negative")
       , need(raw.f.mc2() >= 0, "Mass Fractions (Matrix 2C) must not be negative")
       , need(E.fib2(), "Please chose a fiber type(2)")  
       , need(E.pm2(), "Please chose a primary matrix material(2)")   
       , need(E.int2(), 'Please chose a fiber intermediate (2) ("Not Used" is an option)')    
       , need(E.mold2(), "Please chose a molding option (2)") 
       , need(E.cure2(), 'Please chose a curing option (2) ("Cures in mold" is an option)')
       )
     
     data_frame(
     techset = c(rep(partname1e(), 7), rep(partname2e(), 7)),
     process_segment = c(rep(c("Fiber", "Primary Matrix Material", "Other Materials", "Intermediate", "Molding", "Curing", "Finishing"),2)),
     Energy = c(E.f.fib1(), E.f.pm1(), sum(E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()), E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(),
                        E.f.fib2(), E.f.pm2(), sum(E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()), E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2())
     , order = rep(c(1, 2, 3, 4, 5, 6, 7), 2)
       )})

   fill <- c("Fiber" = "#2960A8", "Intermediate"  = "#74A138", "Primary Matrix Material" = "#C46827",
             "Other Materials"= "#24A7C1", "Molding" = "#8D2A1E", "Curing" = "#E2B500", "Finishing" = "#8A9FCF")
   
   energy_plot.df.p <- reactive({
     energy_plot.df() %>%
       mutate(Part = as.factor(techset),
              Process_Segment = reorder(process_segment, order))
   })

   output$plot1 <- renderPlotly({
    ggplot(energy_plot.df.p(), aes(x = Part, y = Energy, fill = Process_Segment )) +
       geom_bar(stat = "identity") +
       coord_flip() +
       labs(y = "Embodied Energy (MJ/part)", x = "") +
       scale_fill_manual(values = fill, name = "") +
     
       theme_bw() + theme(  legend.title = element_blank(), legend.text = element_text(size = 12), legend.background = element_rect(color = "black")
                        , axis.text = element_text(size = 14), axis.title.x = element_text(size = 18)
                           ) 
         })
   
  
   
  # Download Results ----
   #Build tables for download
  RESULTSTABLE1 <-  reactive(data_frame(
    Stage = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total",
                         "Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
    Choice = c(n.f1(), n.pm1(), n.ma1(), n.mb1(), n.mc1(), n.ia1(), n.ib1(), " ",
               n.int1(), n.mold1(), n.cure1(), n.fin1(), " "),
    "Frac/Yield" = c(f.f1(), f.pm1(), f.ma1(), f.mb1() , f.mc1(), f.ia1(), f.ib1() , sum(f.f1()  , f.pm1() , f.ma1() , f.mb1() , f.mc1() , f.ia1() , f.ib1() ),
                   layup.yield1(), mold.yield1(), " -- " , finish.yield1(), (layup.yield1()* mold.yield1()* finish.yield1())),
    "Embodied Energy (MJ/part)" = c(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1(), sum(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()),
                                    E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(), sum(E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1()))

  ))
  
  RESULTSTABLE2 <-  reactive(data_frame(
    Stage = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total",
                         "Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
    Choice = c(n.f2(), n.pm2(), n.ma2(), n.mb2(), n.mc2(), n.ia2(), n.ib2(), " ",
               n.int2(), n.mold2(), n.cure2(), n.fin2(), " "),
    "Frac/Yield" = c(f.f2(), f.pm2(), f.ma2(), f.mb2() , f.mc2(), f.ia2(), f.ib2() , sum(f.f2()  , f.pm2() , f.ma2() , f.mb2() , f.mc2() , f.ia2() , f.ib2() ),
                   layup.yield2(), mold.yield2(), " -- " , finish.yield2(), (layup.yield2()* mold.yield2()* finish.yield2())),
    "Embodied Energy (MJ/part)" = c(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2(), sum(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()),
                                    E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2(), sum(E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2()))
    
  ))
  
  # Attach tables to download buttons
     output$DL_results1 <- downloadHandler(
     filename = function() {paste(input$results1, ".csv") },
     content = function(file) {
       write.csv(RESULTSTABLE1(), file)
     }   )
     output$DL_results2 <- downloadHandler(
       filename = function() {paste(input$results2, ".csv") },
       content = function(file) {
         write.csv(RESULTSTABLE2(), file)
       }   )
     

     
     #maybe write the .csv out here then just call it like above
   
blank.df <- c("No Data")
     

     output$zipcalcs <- downloadHandler(
         filename = 'CFRP_Tool_Calcualtion_Data.zip',
         content = function(fname){
           Sys.setenv(R_CMDZIP = 'C:/Rtools/bin/zip')
    tmpdir <- tempdir()
    setwd(tempdir())
    print(tempdir())

   filestosave <- c("yield_table1.csv","energy_table1.csv","yield_table2.csv","energy_table2.csv")
    
   if(exists("yield_data1.df()")){
      write.csv(yield_data1.df(), file = "yield_table1.csv")
    } else{
          write.csv(blank.df, file = "yield_table1.csv")
        }        
   
   if(exists("yield_data2.df()")){
     write.csv(yield_data2.df(), file = "yield_table2.csv")
   } else{
     write.csv(blank.df, file = "yield_table2.csv")
   }
   
   if(exists("energy_data1.df()")){
        write.csv(energy_data1.df(), file = "energy_table1.csv")
   } else{
     write.csv(blank.df, file = "energy_table1.csv")
    }
   
   if(exists("energy_data2.df()")){
       write.csv(energy_data2.df(), file = "energy_table2.csv")   
     } else{
     write.csv(blank.df, file = "energy_table2.csv")
   }
   
    zip(zipfile = fname, files = filestosave)
    if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}      },
    
contentType = "application/zip"

         )
     
     
     output$info <- downloadHandler(
       filename = 'CFRP_Tool_Background_Info.zip',
       content = function(file){
         file.copy("data/CFRP_Tool_Background_Info.zip", file)     },
       
       contentType = "application/zip"
       
     )
     
     
#  End ----
   })
