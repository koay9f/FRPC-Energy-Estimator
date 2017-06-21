
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)
library(tidyverse)
library(DT)
library(shinyjs)

#Naming variables & data sheets ---- 
Data_All = read_csv("data/Data_All.csv")

Data_Mold <- subset(Data_All, Table_All == "Mold")
Data_Fiber <- subset(Data_All, Table_All == "Fiber")
Data_Primatrix <- subset(Data_All, Table_All == "Matrix")
Data_Additive <- subset(Data_All, Table_All == "Additive")
Data_Filler <- subset(Data_All, Table_All == "Filler")
Data_Other <- subset(Data_All, Table_All == "Other")
Data_Insert <- subset(Data_All, Table_All == "Insert")
Data_Int <- subset(Data_All, Table_All == "Int")
Data_Cure <- subset(Data_All, Table_All == "Cure")
Data_Finish <- subset(Data_All, Table_All == "Finish")

allcure <- c("Cures in Mold","Autoclave Curing","Oven Curing","Oven Curing with Vacuum","QuickStep",
             "Microwave Curing","Microwave Curing with Vacuum","Infrared Curing", "Direct Induction Curing","E Beam Curing")
onlymoldcure <- c("Cures in Mold")
wetcure <- c("Cures in Mold","Oven Curing","QuickStep","Microwave Curing","Infrared Curing","Direct Induction Curing","E Beam Curing")
vbcure <- c("Autoclave Curing","QuickStep","Microwave Curing with Vacuum","Infrared Curing","Direct Induction Curing","E Beam Curing")

Props = read.csv("data/Properties_Mold.csv") # causes error if read_csv due to use of row names
Data_Cite = read_csv("data/Data_Citations.csv")

Input_List1 = read_csv("data/Defaults.csv")
Input_List2 = read_csv("data/Defaults.csv")
Ex_List1 = read_csv("data/Example.csv")
source("math.R", local = TRUE) 

#Talk to server ----
shinyServer(function(input, output, session) {

  # Molding Process Properties----
  Props.df <- Props[,-1]
  rownames(Props.df) <- Props[,1]
  propmolds <- rownames(Props.df)
  new_col <- c("Surface Finish","Part Size", "Part Shape-Complexity", "Dimensional Control", "Mold Sides", "Capital Costs", "Part Cost", 
               "Cycle Time", "Production Volume", "Max Fiber Mass Frac.", "Strength", "Fiber Placement Control", "Molding Yield", 
               "Additional Curing?", "Heat Cure", "Pressure Cure", "Vacuum Cure", "Prepreg?", "Preform?", "Easily Incorp. Inserts")
  names(Props.df)<- new_col
  
  propnames <- names(Props.df)
  selected_mold <- reactive(checkboxprops(input$show_allmolds, propmolds, NULL))
  selected_vars <- reactive(checkboxprops(input$show_allvars, new_col, c("Surface Finish", "Part Size")))
  
  observeEvent(input$show_allmolds,{
    updateSelectizeInput(session, 'show_molds',
                         choices = propmolds,
                         selected =  selected_mold())
  })
  
  observeEvent(input$show_allvars,{
    updateSelectizeInput(session, 'show_vars',
                         choices = new_col,
                         selected = selected_vars())
  })
  
  output$props <- DT::renderDataTable({
    Props.df[input$show_molds , input$show_vars]
  },  options = list(lengthMenu = c(2, 4, 8, 12, 16), pageLength = 16, scrollX = TRUE), style = 'bootstrap')
  
  # Custom Data ----
  {
  # Complete icons for Custom Data ----
    onclick ("gofiber", {show("fibi")    })
    onclick("gomatrix", {show("mati")    })
    onclick("gofiller", {show("fili")    })
    onclick("goadditive", {show("addi")    })
    onclick("goinsert", {show("insi")    })
    onclick("goint", {show("inti")    })
    onclick("gomold", {show("moldi")    })
    onclick("gocure", {show("curei")    })
    onclick("gofinish", {show("fini")    })
    
    onclick("fiber_add", {hide("fibi")    })
    onclick("matrix_add", {hide("mati")    })
    onclick("filler_add", {hide("fili")    })
    onclick("additive_add", {hide("addi")    })
    onclick("insert_add", {hide("insi")    })
    onclick("int_add", {hide("inti")    })
    onclick("mold_add", {hide("moldi")    })
    onclick("cure_add", {hide("curei")    })
    onclick("finish_add", {hide("fini")    })
  
  # Set Process heating values to zero if boxes un-checked ----
  observeEvent(input$mold_add_row1, {
    if (!input$mold_add_row1){
      updateNumericInput(session, "mold_add_E_P_h2", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h2", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h2", value = (0)) 
      updateCheckboxInput(session, "mold_add_row2", value = FALSE)  }
  })
  observeEvent(input$mold_add_row2, {
    if (!input$mold_add_row2){
      updateNumericInput(session, "mold_add_E_P_h3", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h3", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h3", value = (0)) 
      updateCheckboxInput(session, "mold_add_row3", value = FALSE)  }
  })
  observeEvent(input$mold_add_row3, {
    if (!input$mold_add_row3){
      updateNumericInput(session, "mold_add_E_P_h4", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h4", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h4", value = (0)) 
      updateCheckboxInput(session, "mold_add_row4", value = FALSE)  }
  })
  observeEvent(input$mold_add_row4, {
    if (!input$mold_add_row4){
      updateNumericInput(session, "mold_add_E_P_h5", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h5", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h5", value = (0)) 
      updateCheckboxInput(session, "mold_add_row5", value = FALSE)  }
  })
  observeEvent(input$mold_add_row5, {
    if (!input$mold_add_row5){
      updateNumericInput(session, "mold_add_E_P_h6", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h6", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h6", value = (0)) 
      updateCheckboxInput(session, "mold_add_row6", value = FALSE)  }
  })
  observeEvent(input$mold_add_row6, {
    if (!input$mold_add_row6){
      updateNumericInput(session, "mold_add_E_P_h7", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h7", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h7", value = (0)) 
      updateCheckboxInput(session, "mold_add_row7", value = FALSE)  }
  })
  observeEvent(input$mold_add_row7, {
    if (!input$mold_add_row7){
      updateNumericInput(session, "mold_add_E_P_h7", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h7", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h7", value = (0)) 
      updateCheckboxInput(session, "mold_add_row7", value = FALSE)  }
  })
  observeEvent(input$mold_add_row7, {
    if (!input$mold_add_row7){
      updateNumericInput(session, "mold_add_E_P_h8", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h8", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h8", value = (0)) 
      updateCheckboxInput(session, "mold_add_row8", value = FALSE)  }
  })
  observeEvent(input$mold_add_row8, {
    if (!input$mold_add_row8){
      updateNumericInput(session, "mold_add_E_P_h9", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h9", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h9", value = (0)) 
      updateCheckboxInput(session, "mold_add_row9", value = FALSE)  }
  })
  observeEvent(input$mold_add_row9, {
    if (!input$mold_add_row9){
      updateNumericInput(session, "mold_add_E_P_h0", value = (0)) 
      updateNumericInput(session, "mold_add_E_per_h0", value = (100)) 
      updateNumericInput(session, "mold_add_E_t_h0", value = (0))  }
  })
  
  observeEvent(input$cure_add_row1, {
    if (!input$cure_add_row1){
      updateNumericInput(session, "cure_add_E_P_h2", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h2", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h2", value = (0)) 
      updateCheckboxInput(session, "cure_add_row2", value = FALSE)  }
  })
  observeEvent(input$cure_add_row2, {
    if (!input$cure_add_row2){
      updateNumericInput(session, "cure_add_E_P_h3", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h3", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h3", value = (0)) 
      updateCheckboxInput(session, "cure_add_row3", value = FALSE)  }
  })
  observeEvent(input$cure_add_row3, {
    if (!input$cure_add_row3){
      updateNumericInput(session, "cure_add_E_P_h4", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h4", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h4", value = (0)) 
      updateCheckboxInput(session, "cure_add_row4", value = FALSE)  }
  })
  observeEvent(input$cure_add_row4, {
    if (!input$cure_add_row4){
      updateNumericInput(session, "cure_add_E_P_h5", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h5", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h5", value = (0)) 
      updateCheckboxInput(session, "cure_add_row5", value = FALSE)  }
  })
  observeEvent(input$cure_add_row5, {
    if (!input$cure_add_row5){
      updateNumericInput(session, "cure_add_E_P_h6", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h6", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h6", value = (0)) 
      updateCheckboxInput(session, "cure_add_row6", value = FALSE)  }
  })
  observeEvent(input$cure_add_row6, {
    if (!input$cure_add_row6){
      updateNumericInput(session, "cure_add_E_P_h7", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h7", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h7", value = (0)) 
      updateCheckboxInput(session, "cure_add_row7", value = FALSE)  }
  })
  observeEvent(input$cure_add_row7, {
    if (!input$cure_add_row7){
      updateNumericInput(session, "cure_add_E_P_h7", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h7", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h7", value = (0)) 
      updateCheckboxInput(session, "cure_add_row7", value = FALSE)  }
  })
  observeEvent(input$cure_add_row7, {
    if (!input$cure_add_row7){
      updateNumericInput(session, "cure_add_E_P_h8", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h8", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h8", value = (0)) 
      updateCheckboxInput(session, "cure_add_row8", value = FALSE)  }
  })
  observeEvent(input$cure_add_row8, {
    if (!input$cure_add_row8){
      updateNumericInput(session, "cure_add_E_P_h9", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h9", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h9", value = (0)) 
      updateCheckboxInput(session, "cure_add_row9", value = FALSE)  }
  })
  observeEvent(input$cure_add_row9, {
    if (!input$cure_add_row9){
      updateNumericInput(session, "cure_add_E_P_h0", value = (0)) 
      updateNumericInput(session, "cure_add_E_per_h0", value = (100)) 
      updateNumericInput(session, "cure_add_E_t_h0", value = (0))  }
  })
  
  # Build Data_All_Custom ----
  values  <- reactiveValues(dn = data.frame(Name_All = NA,
                                            Energy_All = NA,
                                            ShortName_All = NA,
                                            Frac_Fiber = NA,
                                            Yield_Mold = NA,
                                            Scrap_Int = NA,
                                            Prepreg_Int = NA,
                                            Table_All = NA,
                                            User_YN_All = TRUE))
  
  observeEvent( input$gofiber, {
    newLinefib <- data.frame(Name_All = as.character(input$fiber_add),
                             Energy_All = as.double(input$fiber_add_E),
                             ShortName_All = NA,
                             Frac_Fiber = NA,
                             Yield_Mold = NA,
                             Scrap_Int = NA,
                             Prepreg_Int = FALSE,
                             Table_All = "Fiber",
                             User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLinefib))
  })
  
  observeEvent( input$gomatrix, {
    newLinemat <- data.frame(Name_All = as.character(input$matrix_add),
                             Energy_All = as.double(input$matrix_add_E),
                             ShortName_All = NA,
                             Frac_Fiber = NA,
                             Yield_Mold = NA,
                             Scrap_Int = NA,
                             Prepreg_Int = FALSE,
                             Table_All = "Matrix",
                             User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLinemat))
  })
  
  observeEvent( input$goadditive, {
    newLineadd <- data.frame(Name_All = as.character(input$additive_add),
                             Energy_All = as.double(input$additive_add_E),
                             ShortName_All = NA,
                             Frac_Fiber = NA,
                             Yield_Mold = NA,
                             Scrap_Int = NA,
                             Prepreg_Int = FALSE,
                             Table_All = "Additive",
                             User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLineadd))
  })
  
  observeEvent( input$gofiller, {
    newLinefil <- data.frame(Name_All = as.character(input$filler_add),
                             Energy_All = as.double(input$filler_add_E),
                             ShortName_All = NA,
                             Frac_Fiber = NA,
                             Yield_Mold = NA,
                             Scrap_Int = NA,
                             Prepreg_Int = FALSE,
                             Table_All = "Filler",
                             User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLinefil))
  })
  
  observeEvent( input$goinsert, {
    newLineins <- data.frame(Name_All = as.character(input$insert_add),
                             Energy_All = as.double(input$insert_add_E),
                             ShortName_All = NA,
                             Frac_Fiber = NA,
                             Yield_Mold = NA,
                             Scrap_Int = NA,
                             Prepreg_Int = FALSE,
                             Table_All = "Insert",
                             User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLineins))
  })
  
  observeEvent( input$goint, {
    newLineins <- data.frame(Name_All = as.character(input$int_add),
                             Energy_All = as.double(input$int_add_E),
                             ShortName_All = NA,
                             Frac_Fiber = NA,
                             Yield_Mold = NA,
                             Scrap_Int = 0,
                             Prepreg_Int = input$int_add_PP,
                             Table_All = "Int",
                             User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLineins))
  })

  observeEvent( input$gomold, {
    mold.E.h.calc <- calc.ph(input$mold_add_E_P_h1, input$mold_add_E_u_h1, input$mold_add_E_per_h1, input$mold_add_E_t_h1,
                             input$mold_add_E_P_h2, input$mold_add_E_u_h2, input$mold_add_E_per_h2, input$mold_add_E_t_h2,
                             input$mold_add_E_P_h3, input$mold_add_E_u_h3, input$mold_add_E_per_h3, input$mold_add_E_t_h3,
                             input$mold_add_E_P_h4, input$mold_add_E_u_h4, input$mold_add_E_per_h4, input$mold_add_E_t_h4,
                             input$mold_add_E_P_h5, input$mold_add_E_u_h5, input$mold_add_E_per_h5, input$mold_add_E_t_h5,
                             input$mold_add_E_P_h6, input$mold_add_E_u_h6, input$mold_add_E_per_h6, input$mold_add_E_t_h6,
                             input$mold_add_E_P_h7, input$mold_add_E_u_h7, input$mold_add_E_per_h7, input$mold_add_E_t_h7,
                             input$mold_add_E_P_h8, input$mold_add_E_u_h8, input$mold_add_E_per_h8, input$mold_add_E_t_h8,
                             input$mold_add_E_P_h9, input$mold_add_E_u_h9, input$mold_add_E_per_h9, input$mold_add_E_t_h9,
                             input$mold_add_E_P_h0, input$mold_add_E_u_h0, input$mold_add_E_per_h0, input$mold_add_E_t_h0)
    mold.E.calc <- calcenergy(input$mold_add_E_m, 
                              input$mold_add_E_P_M, input$mold_add_E_per_M, input$mold_add_E_t_M, 
                              input$mold_add_E_P_p, input$mold_add_E_per_p, input$mold_add_E_t_p,
                              input$mold_add_E_P_c, input$mold_add_E_per_c, input$mold_add_E_t_c,
                              mold.E.h.calc,
                              input$mold_add_E_P_o, input$mold_add_E_u_o, input$mold_add_E_per_o, input$mold_add_E_t_o)
    mold.Ener <- whichenergy(input$mold_add_EYN, mold.E.calc, input$mold_add_E_Y)
    newLinemold <- data.frame(Name_All = as.character(input$mold_add),
                              Energy_All = mold.Ener,
                              ShortName_All = as.character(input$mold_add),
                              Frac_Fiber = 0.5,
                              Yield_Mold = 1,
                              Scrap_Int = NA,
                              Prepreg_Int = FALSE,
                              Table_All = "Mold",
                              User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLinemold))
  })
  
  observeEvent( input$gocure, {
    cure.E.h.calc <- calc.ph(input$cure_add_E_P_h1, input$cure_add_E_u_h1, input$cure_add_E_per_h1, input$cure_add_E_t_h1,
                             input$cure_add_E_P_h2, input$cure_add_E_u_h2, input$cure_add_E_per_h2, input$cure_add_E_t_h2,
                             input$cure_add_E_P_h3, input$cure_add_E_u_h3, input$cure_add_E_per_h3, input$cure_add_E_t_h3,
                             input$cure_add_E_P_h4, input$cure_add_E_u_h4, input$cure_add_E_per_h4, input$cure_add_E_t_h4,
                             input$cure_add_E_P_h5, input$cure_add_E_u_h5, input$cure_add_E_per_h5, input$cure_add_E_t_h5,
                             input$cure_add_E_P_h6, input$cure_add_E_u_h6, input$cure_add_E_per_h6, input$cure_add_E_t_h6,
                             input$cure_add_E_P_h7, input$cure_add_E_u_h7, input$cure_add_E_per_h7, input$cure_add_E_t_h7,
                             input$cure_add_E_P_h8, input$cure_add_E_u_h8, input$cure_add_E_per_h8, input$cure_add_E_t_h8,
                             input$cure_add_E_P_h9, input$cure_add_E_u_h9, input$cure_add_E_per_h9, input$cure_add_E_t_h9,
                             input$cure_add_E_P_h0, input$cure_add_E_u_h0, input$cure_add_E_per_h0, input$cure_add_E_t_h0)
    cure.E.calc <- calcenergy(input$cure_add_E_m, 
                              input$cure_add_E_P_M, input$cure_add_E_per_M, input$cure_add_E_t_M, 
                              input$cure_add_E_P_p, input$cure_add_E_per_p, input$cure_add_E_t_p,
                              input$cure_add_E_P_c, input$cure_add_E_per_c, input$cure_add_E_t_c,
                              cure.E.h.calc,
                              input$cure_add_E_P_o, input$cure_add_E_u_o, input$cure_add_E_per_o, input$cure_add_E_t_o)
    cure.Ener <- whichenergy(input$cure_add_EYN, cure.E.calc, input$cure_add_E_Y)
    newLinecure <- data.frame(Name_All = as.character(input$cure_add),
                              Energy_All = cure.Ener,
                              ShortName_All = NA,
                              Frac_Fiber = NA,
                              Yield_Mold = NA,
                              Scrap_Int = NA,
                              Prepreg_Int = FALSE,
                              Table_All = "Cure",
                              User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLinecure))
  })
  
  observeEvent( input$gofinish, {
    newLinefin <- data.frame(Name_All = as.character(input$finish_add),
                             Energy_All = as.double(input$finish_add_E),
                             ShortName_All = NA,
                             Frac_Fiber = NA,
                             Yield_Mold = NA,
                             Scrap_Int = NA,
                             Prepreg_Int = FALSE,
                             Table_All = "Finish",
                             User_YN_All = TRUE)
    isolate(values$dn <- rbind(values$dn, newLinefin))
  })
  
  observeEvent( input$Re_Custom, {
    isolate(values$dn <- unique(rbind(values$dn, Re_custom.df())))
  })
  
  # Build Custom Data Frame ----
  
  Data_All_Custom <- eventReactive( c(input$goadditive, input$gocure, input$gofiber, input$gofiller, input$gofinish, input$goinsert,
                                      input$goint, input$gomatrix, input$gomold, input$Re_Custom), {
                                        unique(values$dn)})
  Data_All_df <- eventReactive( c(input$goadditive, input$gocure, input$gofiber, input$gofiller, input$gofinish, input$goinsert,
                                    input$goint, input$gomatrix, input$gomold, input$Re_Custom), {
                                      unique(rbind(Data_All, Data_All_Custom()))})
  # output$testtable <- renderTable(Data_All_df())
  
  
  }
  # Upload Example ----
  observeEvent(input$goexample, {
    # Choices
    updateSelectizeInput(session, 'moldingInput1', selected = exselect(Ex_List1,"moldingInput"))
    updateSelectizeInput(session, 'fiberInput1', selected = exselect(Ex_List1,"fiberInput"))
    updateSelectizeInput(session, 'PriMatrixInput1', selected = exselect(Ex_List1,"PriMatrixInput"))
    updateSelectizeInput(session, 'intInput1', selected = exselect(Ex_List1,"intInput"))
    updateSelectizeInput(session, 'cureInput1', selected = exselect(Ex_List1,"cureInput"))
    updateSelectizeInput(session, 'finishInput1', selected = exselect(Ex_List1,"finishInput"))
    # Manual
    updateTextInput(session, "name1", value = exselect(Ex_List1, "name"))
    updateTextInput(session, "finalweight1", value = exselect(Ex_List1, "finalweight"))
  })
  # Upload Previous Run ----
  Re_input1.df <- reactiveValues()
  Re_input1.df <- reactive({
    if (is.null(input$re_input1)) 
      return(NULL)
    data <- read_csv(input$re_input1$datapath)
  })
  
  Re_input2.df <- reactiveValues()
  Re_input2.df <- reactive({
    if (is.null(input$re_input2)) 
      return(NULL)
    data <- read_csv(input$re_input2$datapath)
  })
  
  Re_custom.df <- reactive({
    if (is.null(input$Re_Custom)) 
      return(NULL)
    data <- read_csv(input$Re_Custom$datapath, col_types = cols(
      Name_All = col_character(),
      Energy_All = col_double(),
      ShortName_All = col_character(),
      Frac_Fiber = col_double(),
      Yield_Mold = col_double(),
      Scrap_Int = col_double(),
      Prepreg_Int = col_logical(),
      Table_All = col_character(),
      User_YN_All = col_logical()
    ))
  })
  
  # Initial Page (not molding) ----
  # (none) = first use; a = int; b = matrix; c = mold; d= summary;  e or z = calcs
  observeEvent(input$re_input1, {
    updateTextInput(session, "name1", value = whichselect(Re_input1.df(), Input_List1, "name"))
    updateTextInput(session, "finalweight1", value = whichselect(Re_input1.df(), Input_List1, "finalweight"))
    updateCheckboxInput(session, "insertsAUSERYN1", value = as.logical(YNcheck(Re_input1.df(), "insertsAUSERYN")))
    updateCheckboxInput(session, "insertsBUSERYN1", value = as.logical(YNcheck(Re_input1.df(), "insertsBUSERYN")))
    updateNumericInput(session, "insertsAfrac1", value = whichselect(Re_input1.df(), Input_List1, "insertsAfrac"))
    updateNumericInput(session, "insertsBfrac1", value = whichselect(Re_input1.df(), Input_List1, "insertsBfrac"))
            })
  observeEvent(input$re_input2, {
    updateTextInput(session, "name2", value = whichselect(Re_input2.df(), Input_List2, "name"))
    updateTextInput(session, "finalweight2", value = whichselect(Re_input2.df(), Input_List2, "finalweight"))
    updateCheckboxInput(session, "insertsAUSERYN2", value = as.logical(YNcheck(Re_input2.df(), "insertsAUSERYN")))
    updateCheckboxInput(session, "insertsBUSERYN2", value = as.logical(YNcheck(Re_input2.df(), "insertsBUSERYN")))
    updateNumericInput(session, "insertsAfrac2", value = whichselect(Re_input2.df(), Input_List2, "insertsAfrac"))
    updateNumericInput(session, "insertsBfrac2", value = whichselect(Re_input2.df(), Input_List2, "insertsBfrac"))
    })
  
  output$partname1d <- output$partname1c <- output$partname1b <- output$partname1a <- output$partname1 <- renderText(input$name1)
  partname1e<- reactive(input$name1)
  output$partweight1d <- output$partweight1c <- output$partweight1b <- output$partweight1a <- output$partweight1 <- renderText({paste(input$finalweight1, "kg")})
  
  output$partname2d <-output$partname2c <-output$partname2b <-output$partname2a <- output$partname2 <- renderText(input$name2)
  partname2e<- reactive(input$name2)
  output$partweight2d <- output$partweight2c <- output$partweight2b <- output$partweight2a <- output$partweight2 <- renderText({paste(input$finalweight2, "kg")})
  
  
  # Molding ----
  {
   # Build dataframe for Box----
    calcmoldph <- reactive(calc.ph(
                             input$mold_add_E_P_h1, input$mold_add_E_u_h1, input$mold_add_E_per_h1, input$mold_add_E_t_h1,
                             input$mold_add_E_P_h2, input$mold_add_E_u_h2, input$mold_add_E_per_h2, input$mold_add_E_t_h2,
                             input$mold_add_E_P_h3, input$mold_add_E_u_h3, input$mold_add_E_per_h3, input$mold_add_E_t_h3,
                             input$mold_add_E_P_h4, input$mold_add_E_u_h4, input$mold_add_E_per_h4, input$mold_add_E_t_h4,
                             input$mold_add_E_P_h5, input$mold_add_E_u_h5, input$mold_add_E_per_h5, input$mold_add_E_t_h5,
                             input$mold_add_E_P_h6, input$mold_add_E_u_h6, input$mold_add_E_per_h6, input$mold_add_E_t_h6,
                             input$mold_add_E_P_h7, input$mold_add_E_u_h7, input$mold_add_E_per_h7, input$mold_add_E_t_h7,
                             input$mold_add_E_P_h8, input$mold_add_E_u_h8, input$mold_add_E_per_h8, input$mold_add_E_t_h8,
                             input$mold_add_E_P_h9, input$mold_add_E_u_h9, input$mold_add_E_per_h9, input$mold_add_E_t_h9,
                             input$mold_add_E_P_h0, input$mold_add_E_u_h0, input$mold_add_E_per_h0, input$mold_add_E_t_h0))
    calcmoldE <- reactive(calcenergy(input$mold_add_E_m, 
                              input$mold_add_E_P_M, input$mold_add_E_per_M, input$mold_add_E_t_M, 
                              input$mold_add_E_P_p, input$mold_add_E_per_p, input$mold_add_E_t_p,
                              input$mold_add_E_P_c, input$mold_add_E_per_c, input$mold_add_E_t_c,
                              calcmoldph(),
                              input$mold_add_E_P_o, input$mold_add_E_u_o, input$mold_add_E_per_o, input$mold_add_E_t_o))
    
   mold.Ener <- reactiveValues()
   mold.Ener <- reactive(whichenergy(input$mold_add_EYN, calcmoldE(), input$mold_add_E_Y))

  output$calcedmoldE <- renderText({paste(signif(calcmoldE(), digits = 3), "MJ/kg")})
  
  Data_Mold_new  <- reactiveValues()
  Data_Mold_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Mold
    } else {  subset(Data_All_df(), Table_All == "Mold")})
  mold1_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"moldingInput"))
  mold2_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"moldingInput"))

   # Make List for Box----
  observe( { 
    updateSelectizeInput(session, 'moldingInput1',
                         choices = Data_Mold_new()$Name_All,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'moldingInput2',
                         choices = Data_Mold_new()$Name_All,
                         selected = "",
                         server = TRUE)
  })
  
  observeEvent(input$re_input1, {updateSelectizeInput(session, 'moldingInput1', selected = mold1_selected())  })
  observeEvent(input$re_input2, {updateSelectizeInput(session, 'moldingInput2', selected = mold2_selected())  })
  
   # Match Names to Table----
  moldnamefetch1 <- eventReactive(input$moldingInput1,{
    Data_Mold_new() %>%     filter(Name_All == input$moldingInput1) %>% 
      select(Name_All) %>%  unlist() %>%   last() })
  
  moldnamefetch2 <- eventReactive(input$moldingInput2,{
    Data_Mold_new() %>%     filter(Name_All == input$moldingInput2) %>% 
      select(Name_All) %>%   unlist() %>%  last() })
  
   # Match Names to YN is it User or Default ----
    moldnamecheck1<- eventReactive(input$moldingInput1,{
    Data_Mold_new() %>%     filter(Name_All == input$moldingInput1) %>% 
      select(User_YN_All) %>%   unlist() %>%  last() })
    
    moldnamecheck2<- eventReactive(input$moldingInput2,{
      Data_Mold_new() %>%     filter(Name_All == input$moldingInput2) %>% 
        select(User_YN_All) %>%   unlist() %>%  last() })  
    
   # Match Energy to Name----
  moldenergyfetch1 <- eventReactive(input$moldingInput1,{
  Data_Mold_new() %>%     filter(Name_All == input$moldingInput1) %>% 
    select(Energy_All) %>%  unlist() %>%    last() })
  
  moldenergyfetch2 <- eventReactive(input$moldingInput2,{
    Data_Mold_new() %>%       filter(Name_All == input$moldingInput2) %>% 
      select(Energy_All) %>%   unlist() %>%    last()    })
  
   # Match ShortName to Name----
  moldshortfetch1 <-  eventReactive(input$moldingInput1,{
  Data_Mold_new() %>%       filter(Name_All == input$moldingInput1) %>% 
    select(ShortName_All) %>%    unlist() %>%   last()    })
  
  moldshortfetch2 <- eventReactive(input$moldingInput2, {
  Data_Mold_new() %>%       filter(Name_All == input$moldingInput2) %>% 
    select(ShortName_All) %>%   unlist() %>%    last()    })
  
   # Match Fraction to Name----
  moldfracfetch1 <- eventReactive(input$moldingInput1,{
  Data_Mold_new() %>%       filter(Name_All == input$moldingInput1) %>% 
    select(Frac_Fiber) %>%   unlist() %>%    last()    })
  moldfracfetch2 <- eventReactive(input$moldingInput2,{ 
    Data_Mold_new() %>%       filter(Name_All == input$moldingInput2) %>% 
      select(Frac_Fiber) %>%    unlist() %>%   last()    })
  
   # Match Yield to Name----
  moldyieldfetch1 <- eventReactive(input$moldingInput1,{ 
  Data_Mold_new() %>%       filter(Name_All == input$moldingInput1) %>% 
    select(Yield_Mold) %>%   unlist() %>%    last()    })
  moldyieldfetch2 <- eventReactive(input$moldingInput2,{
    Data_Mold_new() %>%       filter(Name_All == input$moldingInput2) %>% 
      select(Yield_Mold) %>%    unlist() %>%   last()    })
  
   # Match Citations to Name----
  moldcitefetch1 <- eventReactive(input$moldingInput1,{
    Data_Cite %>%       filter(Name == input$moldingInput1) %>% 
    select(Source) %>%     unlist() %>%  last()    })
  
  moldcitefetch2 <- eventReactive(input$moldingInput2,{
    Data_Cite %>%       filter(Name == input$moldingInput2) %>% 
      select(Source) %>%    unlist() %>%   last()    })
  
   # Generate Outputs----
  moldname1 <- output$moldname1d <- output$moldname1c <- output$moldname1b <- output$moldname1a <- output$moldname1 <- renderText(moldnamefetch1())
  output$moldshort1d <- output$moldshort1c <- output$moldshort1b <- output$moldshort1a <- output$moldshort1 <- renderText(moldshortfetch1())
  output$moldfracNum1 <- renderText({paste(moldfracfetch1() *100, "%" )})
  output$moldyieldNum1 <- renderText({paste(moldyieldfetch1() *100, "%" )}) 
  
  moldname2 <-output$moldname2e <-output$moldname2d <-output$moldname2c <- output$moldname2b <- output$moldname2a <- output$moldname2 <- renderText(moldnamefetch2())
  output$moldshort2d <- output$moldshort2c <- output$moldshort2b <- output$moldshort2a <- output$moldshort2 <- renderText(moldshortfetch2())
  output$moldfracNum2 <- renderText({paste(moldfracfetch2() *100, "%" )})
  output$moldyieldNum2 <- renderText({paste(moldyieldfetch2() *100, "%" )})
  
   # Generate error or energy number----
  output$EnergyNum1 <-  renderText({
    validate(
      need(moldnamefetch1(), "Choose Molding Technology"),   
      need(input$finalweight1 > 0, "Part Weight cannot be negative")     )  
    ({paste(moldenergyfetch1(), "MJ/kg")}                   )})
  
  output$EnergyNum2 <- renderText({
    validate(
      need(moldnamefetch2(), "Choose Molding Technology"),   
      need(input$finalweight2 > 0, "Part Weight cannot be negative")       ) 
    ({paste(moldenergyfetch2(), "MJ/kg")}                   )})
  
   # Citations----
  moldcite1 <- renderText(as.character(moldcitefetch1()))
  observeEvent(input$moldingInput1, {
    validate(need(moldname1() != 'NA', "")) 
    showNotification(paste("Citation for ", moldname1(), ": ",moldcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  moldcite2 <- renderText(as.character(moldcitefetch2()))
  observeEvent(input$moldingInput2, {
    validate(need(moldname2() != 'NA', "")) 
    showNotification(paste("Citation for ", moldname2(), ": ",moldcite2()), duration = 5, closeButton = TRUE, type = "message")})
  
   # Update Fiber fractions & mold yield based on mold process choice----

  observeEvent(input$moldingInput1,{
    ff1 <- moldfracfetch1() * 100
    moldy1 <-  moldyieldfetch1() *100
    
    updateNumericInput(session, "moldfracUSERNum1", value = (ff1)) 
    updateNumericInput(session, "moldyieldUSERNum1", value = (moldy1))    
    
    observeEvent(input$re_input1, {

      Re_ff1 <- unname(unlist(dplyr::filter (Re_input1.df(), Variable_Name == "moldfracUSERNum") %>% select(4)))
      updateNumericInput(session, "moldfracUSERNum1", value = (Re_ff1)) 

      Re_y1 <- unname(unlist(dplyr::filter (Re_input1.df(), Variable_Name == "moldyieldUSERNum") %>% select(4)))
      updateNumericInput(session, "moldyieldUSERNum1", value = (Re_y1))

      })
       })
  
  observeEvent(input$moldingInput2,{
    ff2 <- moldfracfetch2() * 100
    moldy2 <-  moldyieldfetch2() *100
    
    updateNumericInput(session, "moldfracUSERNum2", value = (ff2)) 
    updateNumericInput(session, "moldyieldUSERNum2", value = (moldy2))    
    
    observeEvent(input$re_input2, {
      
      Re_ff2 <- unname(unlist(dplyr::filter (Re_input2.df(), Variable_Name == "moldfracUSERNum") %>% select(4)))
      updateNumericInput(session, "moldfracUSERNum2", value = (Re_ff2)) 
      
      Re_y2 <- unname(unlist(dplyr::filter (Re_input2.df(), Variable_Name == "moldyieldUSERNum") %>% select(4)))
      updateNumericInput(session, "moldyieldUSERNum2", value = (Re_y2))
    })
  })
  
  # THIS IS HOW I DID IT PREVIOUSLY BUT IT WOULD NOT UPDATE BASED ON UPLOAD, ONLY CHANGE IN MOLDING.  NOW WILL UPDATE BASED ON CHANGEING MOLDING *UNLESS*
  # SOMETHING HAS BE UPLOADED, THEN WILL ONLY PULL BASED ON WHAT IS IN THE UPLOAD FILE
  # #update 2 based on molding
  # observeEvent(input$moldingInput2,{
  #   ff2 <- moldfracfetch2()
  #   moldy2 <-  moldyieldfetch2()
  #   updateNumericInput(session, "moldfracUSERNum2", value = (ff2*100)) 
  #   updateNumericInput(session, "moldyieldUSERNum2", value = (moldy2*100)) 
  # })
  # 
  # #update 2 based on upload
  # observeEvent({input$moldingInput2
  #   input$re_input2}, 
  #              {
  #   updateNumericInput(session, "moldfracUSERNum2", value = whichselect(Re_input2.df(), Input_List2, "moldfracUSERNum"))
  #   updateNumericInput(session, "moldyieldUSERNum2", value = whichselect(Re_input2.df(), Input_List2, "moldyieldUSERNum"))
  # })
  #THIS SHOULD ALSO WORK WITHIN THE OBSERVEEVENT BLOCK: 
 # onclick("moldingInput1", {updateNumericInput(session,"moldfracUSERNum1", value = moldfracfetch1() * 100)})
  
  
}
  # Fiber ----
  {
   # Build dataframe for Box----
  Data_Fiber_new  <- reactiveValues()
  Data_Fiber_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Fiber
      } else {  subset(Data_All_df(), Table_All == "Fiber")})

  fiber1_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"fiberInput"))
  fiber2_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"fiberInput"))
  
   # Make List for Box----
  observe({ 
    updateSelectizeInput(session, 'fiberInput1',
                         choices = Data_Fiber_new()$Name_All,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'fiberInput2',
                         choices = Data_Fiber_new()$Name_All,
                         selected = "",
                         server = TRUE)
  })
  
  observeEvent(input$re_input1, {updateSelectizeInput(session, 'fiberInput1', selected = fiber1_selected())})
  observeEvent(input$re_input2, {updateSelectizeInput(session, 'fiberInput2', selected = fiber2_selected())})
  
   # Match Names to Table----
  fibernamefetch1 <- eventReactive(input$fiberInput1,{
  Data_Fiber_new() %>%     filter(Name_All == input$fiberInput1) %>% 
      select(Name_All) %>%  unlist() %>%    last() })
  
  fibernamefetch2 <- eventReactive(input$fiberInput2,{
    Data_Fiber_new() %>%     filter(Name_All == input$fiberInput2) %>% 
      select(Name_All) %>%  unlist() %>%    last() })  
  
   # Match Energy to Name----
  fiberenergyfetch1 <- eventReactive(input$fiberInput1,{
    Data_Fiber_new() %>%     filter(Name_All == input$fiberInput1) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })  
  
  fiberenergyfetch2 <- eventReactive(input$fiberInput2,{
    Data_Fiber_new() %>%     filter(Name_All == input$fiberInput2) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })  
  
   # Generate Outputs----
  fibername1 <- output$fibername1d  <-  output$fibername1c <- output$fibername1b <- output$fibername1a <- output$fibername1 <- renderText(as.character(fibernamefetch1()))
  fibername2 <- output$fibername2d  <- output$fibername2c <-output$fibername2b <-output$fibername2a <-output$fibername2 <- renderText(as.character(fibernamefetch2()))
  
   # Pull used fiber fractions for use in matrix tab and calculations----
  output$fiberfrac1c <-output$fiberfrac1a <-output$fiberfrac1b <- renderText({paste(input$moldfracUSERNum1, "%")})
  output$fiberfrac1z <-output$fiberfrac1e <-output$fiberfrac1d <- renderText(input$moldfracUSERNum1)
  output$fiberfrac2c <-output$fiberfrac2a <-output$fiberfrac2b <- renderText({paste(input$moldfracUSERNum2, "%")})
  output$fiberfrac2z <-output$fiberfrac2e <-output$fiberfrac2d  <- renderText(input$moldfracUSERNum2)
  
   # Generate error or energy number----
  output$fiberEnergyNum1 <- renderText({
    validate(
      need(fibernamefetch1(), "Choose Fiber Technology"),
      need(input$moldfracUSERNum1 >0, "Mass Fraction cannot be negative"),
      need(input$moldfracUSERNum1 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({paste(fiberenergyfetch1(), "MJ/kg")}                   )})
  
  output$fiberEnergyNum2 <- renderText({
    validate(
      need(fibernamefetch2(), "Choose Fiber Technology"),
      need(input$moldfracUSERNum2 >0, "Mass Fraction cannot be negative"),
      need(input$moldfracUSERNum2 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({paste(fiberenergyfetch2(), "MJ/kg")}                   )})
  
   # Citations----
  fibercitefetch1 <- eventReactive(input$fiberInput1,{
    Data_Cite %>%       filter(Name == input$fiberInput1) %>% 
      select(Source) %>%     unlist() %>%  last()    })  
  fibercitefetch2 <- eventReactive(input$fiberInput2,{
    Data_Cite %>%       filter(Name == input$fiberInput2) %>% 
      select(Source) %>%     unlist() %>%  last()    })  
  
  fibercite1 <- renderText(as.character(fibercitefetch1()))
  observeEvent(input$fiberInput1, {
    validate(need(fibername1() != 'NA', "")) 
    showNotification(paste("Citation for ", fibername1(), ": ",fibercite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  fibercite2 <- renderText(as.character(fibercitefetch2()))
  observeEvent(input$fiberInput2, {
    validate(need(fibername2() != 'NA', "")) 
    showNotification(paste("Citation for ", fibername2(), ": ",fibercite2()), duration = 5, closeButton = TRUE, type = "message")})
  }
  # Matrix ----
  {
   # Build dataframe for Box----
 Data_Primatrix_new  <- reactiveValues()
  Data_Primatrix_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Primatrix
    } else {  subset(Data_All_df(), Table_All == "Matrix")})
  
  PM1_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"PriMatrixInput"))
  PM2_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"PriMatrixInput"))
  
  Data_Primatrix <- subset(Data_All, Table_All == "Matrix")
  Data_Additive <- subset(Data_All, Table_All == "Additive")
  Data_Filler <- subset(Data_All, Table_All == "Filler")
  Data_Other <- subset(Data_All, Table_All == "Other")
  
  Data_Additive_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Additive
    } else {  subset(Data_All_df(), Table_All == "Additive")})
  Data_Filler_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Filler
    } else {  subset(Data_All_df(), Table_All == "Filler")})
  
  Data_MatrixM_new  <- reactiveValues()
  Data_MatrixM_new  <- reactive(
    if (is.null(Data_All_df())) {rbind(Data_Primatrix, Data_Additive, Data_Filler, Data_Other)
    } else {  rbind(Data_Primatrix_new(), Data_Additive_new(), Data_Filler_new(), Data_Other)     })

   # Make List for Box ----
  observe({ 
    updateSelectizeInput(session, 'PriMatrixInput1',
                         choices = Data_Primatrix_new()$Name_All,
                         selected = "",
                         server = TRUE)
    updateSelectizeInput(session, 'PriMatrixInput2',
                         choices = Data_Primatrix_new()$Name_All,
                         selected = "",
                         server = TRUE)
  })
  
  observeEvent(input$re_input1, {updateSelectizeInput(session, 'PriMatrixInput1', selected = PM1_selected())})
  observeEvent(input$re_input2, {updateSelectizeInput(session, 'PriMatrixInput2', selected = PM2_selected())})

   # Matrix mass fraction = 1 - fiber or from upload----
  observe({
      rff1 <- input$moldfracUSERNum1
      updateNumericInput(session, "primatrixfrac1", value = 100-rff1)
  })
  
  observeEvent(input$re_input1, {
    updateNumericInput(session, "primatrixfrac1", value = whichselect(Re_input1.df(), Input_List1, "primatrixfrac"))
  })
  
  observe({
    rff2 <- input$moldfracUSERNum2
    updateNumericInput(session, "primatrixfrac2", value = 100-rff2)
  })
  
  observeEvent(input$re_input2, {
    updateNumericInput(session, "primatrixfrac2", value = whichselect(Re_input2.df(), Input_List2, "primatrixfrac"))
  })
    
   # Match Names to Table----
  matrixnames_new <- reactive(Data_MatrixM_new()$Name_All)
  matrixenergy_new <- reactive(Data_MatrixM_new()$Energy_All)

  primatrixnamefetch1 <- eventReactive(input$PriMatrixInput1,{
  Data_MatrixM_new() %>%     filter(Name_All == input$PriMatrixInput1) %>% 
    select(Name_All) %>%  unlist() %>%    last() })  
  
  primatrixnamefetch2 <- eventReactive(input$PriMatrixInput2,{
    Data_MatrixM_new() %>%     filter(Name_All == input$PriMatrixInput2) %>% 
      select(Name_All) %>%  unlist() %>%    last() })    
  
   # Match Energy to Name----
  primatrixenergyfetch1 <- eventReactive(input$PriMatrixInput1,{
    Data_MatrixM_new() %>%     filter(Name_All == input$PriMatrixInput1) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })  
  
  primatrixenergyfetch2 <- eventReactive(input$PriMatrixInput2,{
    Data_MatrixM_new() %>%     filter(Name_All == input$PriMatrixInput2) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })  
  
   # Generate Outputs----
  primatrixname1 <- output$primatrixname1c <-output$primatrixname1a <- renderText(as.character(primatrixnamefetch1()))
  primatrixname2 <- output$primatrixname2c <-output$primatrixname2a <- renderText(as.character(primatrixnamefetch2()))
  output$primatrixfrac1c <-output$primatrixfrac1a <- renderText({paste(input$primatrixfrac1, "%")})
  output$primatrixfrac2c <-output$primatrixfrac2a <- renderText({paste(input$primatrixfrac2, "%")})
  
   # Generate error or energy number----
    output$primatrixEnergyNum1 <-renderText({
    validate(
      need(primatrixnamefetch1(), "Choose Primary Matrix"),
      need(sum(input$moldfracUSERNum1, input$primatrixfrac1, input$othermatrixAfrac1, input$othermatrixBfrac1, input$othermatrixCfrac1) == 100,      "Sum of Mass Fractions should add to 100%"),
      need(input$primatrixfrac1 >=0, "Mass Fraction cannot be negative"),
      need(input$primatrixfrac1 <100, "Mass Fraction cannot be greater than 100%")      ) 
      ({paste(primatrixenergyfetch1(), "MJ/kg")}                )})
  
    output$primatrixEnergyNum2 <- renderText({    
      validate(
       need(primatrixnamefetch2(), "Choose Primary Matrix"),
       need(sum(input$moldfracUSERNum2, input$primatrixfrac2, input$othermatrixAfrac2, input$othermatrixBfrac2, input$othermatrixCfrac2) == 100,      "Sum of Mass Fractions should add to 100%"),
       need(input$primatrixfrac2 >=0, "Mass Fraction cannot be negative"),
        need(input$primatrixfrac2 <100, "Mass Fraction cannot be greater than 100%")      ) 
     ({ paste(primatrixenergyfetch2(), "MJ/kg")}               )})
    
  
   # Citations----
  primatrixcitefetch1 <- eventReactive(input$PriMatrixInput1,{
    Data_Cite %>%       filter(Name == input$PriMatrixInput1) %>% 
      select(Source) %>%     unlist() %>%  last()    })  
    primatrixcitefetch2 <- eventReactive(input$PriMatrixInput2,{
      Data_Cite %>%       filter(Name == input$PriMatrixInput2) %>% 
        select(Source) %>%     unlist() %>%  last()    })    
  
  primatrixcite1 <- renderText(as.character(primatrixcitefetch1()))
  observeEvent(input$PriMatrixInput1, {
    validate(need(primatrixname1() != 'NA', "")) 
    showNotification(paste("Citation for ", primatrixname1(), ": ",primatrixcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  primatrixcite2 <- renderText(as.character(primatrixcitefetch2()))
  observeEvent(input$PriMatrixInput2, {
    validate(need(primatrixname2() != 'NA', "")) 
    showNotification(paste("Citation for ", primatrixname2(), ": ",primatrixcite2()), duration = 5, closeButton = TRUE, type = "message")})
  }
  # OtherMat ----
  {
   # Build additive and filler dataframes for Box ----
  # Above w/ Matrix

   # Recall from upload ----
  observeEvent(input$re_input1, {
    types1a_selected <- whichselect(Re_input1.df(), Input_List1,"typesa")
    types1b_selected <- whichselect(Re_input1.df(), Input_List1,"typesb")
    types1c_selected <- whichselect(Re_input1.df(), Input_List1,"typesc")
        
   updateCheckboxInput(session, "othermatrixAUSERYN1", value = as.logical(YNcheck(Re_input1.df(), "othermatrixAUSERYN")))
   updateCheckboxInput(session, "othermatrixBUSERYN1", value = as.logical(YNcheck(Re_input1.df(), "othermatrixBUSERYN")))
   updateCheckboxInput(session, "othermatrixCUSERYN1", value = as.logical(YNcheck(Re_input1.df(), "othermatrixCUSERYN")))
   updateNumericInput(session, "othermatrixAfrac1", value = whichselect(Re_input1.df(), Input_List1, "othermatrixAfrac"))
   updateNumericInput(session, "othermatrixBfrac1", value = whichselect(Re_input1.df(), Input_List1, "othermatrixBfrac"))
   updateNumericInput(session, "othermatrixCfrac1", value = whichselect(Re_input1.df(), Input_List1, "othermatrixCfrac"))
   updateSelectizeInput(session, "types1a", choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = types1a_selected, server = TRUE)
   updateSelectizeInput(session, "types1b", choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = types1b_selected, server = TRUE)
   updateSelectizeInput(session, "types1c", choices = c("Matrix", "Additive", "Filler", "Not Used"),
                        selected = types1c_selected, server = TRUE)
   })

  observeEvent(input$re_input2, {
    types2a_selected <- whichselect(Re_input2.df(), Input_List2,"typesa")
    types2b_selected <- whichselect(Re_input2.df(), Input_List2,"typesb")
    types2c_selected <- whichselect(Re_input2.df(), Input_List2,"typesc")
    
    updateCheckboxInput(session, "othermatrixAUSERYN2", value = as.logical(YNcheck(Re_input2.df(), "othermatrixAUSERYN")))
    updateCheckboxInput(session, "othermatrixBUSERYN2", value = as.logical(YNcheck(Re_input2.df(), "othermatrixBUSERYN")))
    updateCheckboxInput(session, "othermatrixCUSERYN2", value = as.logical(YNcheck(Re_input2.df(), "othermatrixCUSERYN")))
    updateNumericInput(session, "othermatrixAfrac2", value = whichselect(Re_input2.df(), Input_List2, "othermatrixAfrac"))
    updateNumericInput(session, "othermatrixBfrac2", value = whichselect(Re_input2.df(), Input_List2, "othermatrixBfrac"))
    updateNumericInput(session, "othermatrixCfrac2", value = whichselect(Re_input2.df(), Input_List2, "othermatrixCfrac"))
    updateSelectizeInput(session, "types2a", choices = c("Matrix", "Additive", "Filler", "Not Used"),
                         selected = types2a_selected, server = TRUE)
    updateSelectizeInput(session, "types2b", choices = c("Matrix", "Additive", "Filler", "Not Used"),
                         selected = types2b_selected, server = TRUE)
    updateSelectizeInput(session, "types2c", choices = c("Matrix", "Additive", "Filler", "Not Used"),
                         selected = types2c_selected, server = TRUE)
  })

   # Make Lists for boxes----

  
  other1a_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"OtherMatrixAInput"))
  other1b_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"OtherMatrixBInput"))
  other1c_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"OtherMatrixCInput"))
  
  other2a_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"OtherMatrixAInput"))
  other2b_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"OtherMatrixBInput"))
  other2c_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"OtherMatrixCInput"))
  
  observeEvent(input$types1a, {
   list1a <- othermatfxn(input$types1a, Data_Primatrix_new()$Name_All, Data_Additive_new()$Name_All, Data_Filler_new()$Name_All)
   updateSelectizeInput(session, 'OtherMatrixAInput1',
                        choices = list1a,
                        selected = other1a_selected(), 
                        server = TRUE)})

   observeEvent(input$types1b, {
    list1b <-  othermatfxn(input$types1b, Data_Primatrix_new()$Name_All, Data_Additive_new()$Name_All, Data_Filler_new()$Name_All)
    updateSelectizeInput(session, 'OtherMatrixBInput1',
                         choices = list1b,
                         selected = other1b_selected(),
                         server = TRUE)})
  
  observeEvent(input$types1c, {
    list1c <-  othermatfxn(input$types1c, Data_Primatrix_new()$Name_All, Data_Additive_new()$Name_All, Data_Filler_new()$Name_All)
    updateSelectizeInput(session, 'OtherMatrixCInput1',
                         choices = list1c,
                         selected = other1c_selected(),
                         server = TRUE)})
  
  observeEvent(input$types2a, {
    list2a <- othermatfxn(input$types2a, Data_Primatrix_new()$Name_All, Data_Additive_new()$Name_All, Data_Filler_new()$Name_All)
    updateSelectizeInput(session, 'OtherMatrixAInput2',
                         choices = list2a,
                         selected = other2a_selected(), 
                         server = TRUE)})
  
  observeEvent(input$types2b, {
    list2b <-  othermatfxn(input$types2b, Data_Primatrix_new()$Name_All, Data_Additive_new()$Name_All, Data_Filler_new()$Name_All)
    updateSelectizeInput(session, 'OtherMatrixBInput2',
                         choices = list2b,
                         selected = other2b_selected(),
                         server = TRUE)})
  
  observeEvent(input$types2c, {
    list2c <-  othermatfxn(input$types2c, Data_Primatrix_new()$Name_All, Data_Additive_new()$Name_All, Data_Filler_new()$Name_All)
    updateSelectizeInput(session, 'OtherMatrixCInput2',
                         choices = list2c,
                         selected = other2c_selected(),
                         server = TRUE)})
  
   # Match Name with Table----
  othermatrixAnamefetch1 <- eventReactive(input$OtherMatrixAInput1, {
                                            Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixAInput1) %>% 
                                              select(Name_All) %>%  unlist() %>%    last() })  
  othermatrixBnamefetch1 <- eventReactive(input$OtherMatrixBInput1, {
                                          Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixBInput1) %>% 
                                            select(Name_All) %>%  unlist() %>%    last() })  
  othermatrixCnamefetch1 <- eventReactive(input$OtherMatrixCInput1,{
                                          Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixCInput1) %>% 
                                            select(Name_All) %>%  unlist() %>%    last() })  
  othermatrixAnamefetch2 <- eventReactive(input$OtherMatrixAInput2, {
                                          Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixAInput2) %>% 
                                            select(Name_All) %>%  unlist() %>%    last() })  
  othermatrixBnamefetch2 <- eventReactive(input$OtherMatrixBInput2, {
                                          Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixBInput2) %>% 
                                            select(Name_All) %>%  unlist() %>%    last() }) 
  othermatrixCnamefetch2 <- eventReactive(input$OtherMatrixCInput2, {
                                          Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixCInput2) %>% 
                                            select(Name_All) %>%  unlist() %>%    last() }) 
  
   # Match Energy with Name----
  othermatrixAenergyfetch1 <- eventReactive(input$OtherMatrixAInput1, {
                                            Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixAInput1) %>% 
                                              select(Energy_All) %>%  unlist() %>%    last() })  
  othermatrixBenergyfetch1 <- eventReactive(input$OtherMatrixBInput1, { 
                                            Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixBInput1) %>% 
                                              select(Energy_All) %>%  unlist() %>%    last() })  
  othermatrixCenergyfetch1 <- eventReactive(input$OtherMatrixCInput1, { 
                                            Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixCInput1) %>% 
                                              select(Energy_All) %>%  unlist() %>%    last() })  
  othermatrixAenergyfetch2 <- eventReactive(input$OtherMatrixAInput2, { 
                                            Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixAInput2) %>% 
                                              select(Energy_All) %>%  unlist() %>%    last() })  
  othermatrixBenergyfetch2 <- eventReactive(input$OtherMatrixBInput2, { 
                                              Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixBInput2) %>% 
                                                select(Energy_All) %>%  unlist() %>%    last() })  
  othermatrixCenergyfetch2 <- eventReactive(input$OtherMatrixCInput2, { 
                                                Data_MatrixM_new() %>%     filter(Name_All == input$OtherMatrixCInput2) %>% 
                                                  select(Energy_All) %>%  unlist() %>%    last() })  
  
   # Generate  Outputs----
  othermatrixAname1 <- renderText(as.character(othermatrixAnamefetch1()))
  othermatrixBname1 <- renderText(as.character(othermatrixBnamefetch1()))
  othermatrixCname1 <- renderText(as.character(othermatrixCnamefetch1()))
  othermatrixAname2 <- renderText(as.character(othermatrixAnamefetch2()))
  othermatrixBname2 <- renderText(as.character(othermatrixBnamefetch2()))
  othermatrixCname2 <- renderText(as.character(othermatrixCnamefetch2()))
  
   # Generate Error message if no choice ----
  output$othermatrixAEnergyNum1 <-renderText({
    validate(
      need(input$othermatrixAfrac1 >=0, "Mass Fraction cannot be negative"),
      need(input$othermatrixAfrac1 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({paste(othermatrixAenergyfetch1(), "MJ/kg")}                )})
  
  output$othermatrixAEnergyNum2 <- renderText({    
    validate(
      need(input$othermatrixAfrac2 >=0, "Mass Fraction cannot be negative"),
      need(input$othermatrixAfrac2 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({ paste(othermatrixAenergyfetch2(), "MJ/kg")}               )})
  
  output$othermatrixBEnergyNum1 <-renderText({
    validate(
      need(input$othermatrixBfrac1 >=0, "Mass Fraction cannot be negative"),
      need(input$othermatrixBfrac1 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({paste(othermatrixBenergyfetch1(), "MJ/kg")}                )})
  
  output$othermatrixBEnergyNum2 <- renderText({    
    validate(
      need(input$othermatrixBfrac2 >=0, "Mass Fraction cannot be negative"),
      need(input$othermatrixBfrac2 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({ paste(othermatrixBenergyfetch2(), "MJ/kg")}               )})
  
  output$othermatrixCEnergyNum1 <-renderText({
    validate(
      need(input$othermatrixCfrac1 >=0, "Mass Fraction cannot be negative"),
      need(input$othermatrixCfrac1 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({paste(othermatrixCenergyfetch1(), "MJ/kg")}                )})
  
  output$othermatrixCEnergyNum2 <- renderText({    
    validate(
      need(input$othermatrixCfrac2 >=0, "Mass Fraction cannot be negative"),
      need(input$othermatrixCfrac2 <100, "Mass Fraction cannot be greater than 100%")      ) 
    ({ paste(othermatrixCenergyfetch2(), "MJ/kg")}               )})
  
   # Citations ----
  othermatrixAcitefetch1 <- eventReactive(input$OtherMatrixAInput1,{
    Data_Cite %>%       filter(Name == input$OtherMatrixAInput1) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  othermatrixAcitefetch2 <- eventReactive(input$OtherMatrixAInput2,{
    Data_Cite %>%       filter(Name == input$OtherMatrixAInput2) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  
  othermatrixAcite1 <- renderText(as.character(othermatrixAcitefetch1()))
  observeEvent(input$OtherMatrixAInput1, {
    validate(need(othermatrixAname1() != "Not Used", ""), need(othermatrixAname1()!= 'NA', "")) 
    showNotification(paste("Citation for ", othermatrixAname1(), ": ",othermatrixAcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  othermatrixAcite2 <- renderText(as.character(othermatrixAcitefetch2()))
  observeEvent(input$OtherMatrixAInput2, {
    validate(need(othermatrixAname2()!= "Not Used", ""), need(othermatrixAname2()!= 'NA', "")) 
    showNotification(paste("Citation for ", othermatrixAname2(), ": ",othermatrixAcite2()), duration = 5, closeButton = TRUE, type = "message")})
 
  othermatrixBcitefetch1 <- eventReactive(input$OtherMatrixBInput1,{
    Data_Cite %>%       filter(Name == input$OtherMatrixBInput1) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  othermatrixBcitefetch2 <- eventReactive(input$OtherMatrixBInput2,{
    Data_Cite %>%       filter(Name == input$OtherMatrixBInput2) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  
  othermatrixBcite1 <- renderText(as.character(othermatrixBcitefetch1()))
  observeEvent(input$OtherMatrixBInput1, {
    validate(need(othermatrixBname1() != "Not Used", ""), need(othermatrixBname1()!= 'NA', "")) 
    showNotification(paste("Citation for ", othermatrixBname1(), ": ",othermatrixBcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  othermatrixBcite2 <- renderText(as.character(othermatrixBcitefetch2()))
  observeEvent(input$OtherMatrixBInput2, {
    validate(need(othermatrixBname2() != "Not Used", ""), need(othermatrixBname2()!= 'NA', "")) 
    showNotification(paste("Citation for ", othermatrixBname2(), ": ",othermatrixBcite2()), duration = 5, closeButton = TRUE, type = "message")})
  
  othermatrixCcitefetch1 <- eventReactive(input$OtherMatrixCInput1,{
    Data_Cite %>%       filter(Name == input$OtherMatrixCInput1) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  othermatrixCcitefetch2 <- eventReactive(input$OtherMatrixAInput2,{
    Data_Cite %>%       filter(Name == input$OtherMatrixCInput2) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  
  othermatrixCcite1 <- renderText(as.character(othermatrixCcitefetch1()))
  observeEvent(input$OtherMatrixCInput1, {
    validate(need(othermatrixCname1() != "Not Used", ""), need(othermatrixCname1()!= 'NA', "")) 
    showNotification(paste("Citation for ", othermatrixCname1(), ": ",othermatrixCcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  othermatrixCcite2 <- renderText(as.character(othermatrixCcitefetch2()))
  observeEvent(input$OtherMatrixCInput2, {
    validate(need(othermatrixCname2() != "Not Used", ""), need(othermatrixCname2()!= 'NA', "")) 
    showNotification(paste("Citation for ", othermatrixCname2(), ": ",othermatrixCcite2()), duration = 5, closeButton = TRUE, type = "message")})
  }
  # Inserts ----
  {
   # Build Dataframe for Box----
  Data_Insert_new <- reactiveValues()
  Data_Insert_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Insert
    } else {  subset(Data_All_df(), Table_All == "Insert")})
  
   # Make List for Box ----
  ins1a_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"InsertsAInput"))
  ins2a_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"InsertsAInput"))
  
  ins1b_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"InsertsBInput"))
  ins2b_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"InsertsBInput"))
  
  observe({
    updateSelectizeInput(session, 'InsertsAInput1',
                         choices = Data_Insert_new()$Name_All,
                         selected = "Not Used",
                         server = TRUE)
    updateSelectizeInput(session, 'InsertsBInput1',
                         choices = Data_Insert_new()$Name_All,
                         selected ="Not Used",
                         server = TRUE)
    updateSelectizeInput(session, 'InsertsAInput2',
                         choices = Data_Insert_new()$Name_All,
                         selected = "Not Used",
                         server = TRUE)
    updateSelectizeInput(session, 'InsertsBInput2',
                         choices = Data_Insert_new()$Name_All,
                         selected = "Not Used",
                         server = TRUE)
  })
  
  observeEvent(input$re_input1, {updateSelectizeInput(session, 'InsertsAInput1', selected = ins1a_selected())})
  observeEvent(input$re_input1, {updateSelectizeInput(session, 'InsertsBInput1', selected = ins1b_selected())})
  
  observeEvent(input$re_input2, {updateSelectizeInput(session, 'InsertsAInput2', selected = ins2a_selected())})
  observeEvent(input$re_input2, {updateSelectizeInput(session, 'InsertsBInput2', selected = ins2b_selected())})
  
   # Match Names To Table----
  insertsAnamefetch1  <- eventReactive(input$InsertsAInput1,{
  Data_Insert_new() %>%     filter(Name_All == input$InsertsAInput1) %>% 
    select(Name_All) %>%  unlist() %>%    last() })  
  insertsBnamefetch1  <- eventReactive(input$InsertsBInput1,{
    Data_Insert_new() %>%     filter(Name_All == input$InsertsBInput1) %>% 
      select(Name_All) %>%  unlist() %>%    last() })   
  insertsAnamefetch2  <- eventReactive(input$InsertsAInput2,{
    Data_Insert_new() %>%     filter(Name_All == input$InsertsAInput2) %>% 
      select(Name_All) %>%  unlist() %>%    last() })   
  insertsBnamefetch2  <- eventReactive(input$InsertsBInput2,{
    Data_Insert_new() %>%     filter(Name_All == input$InsertsBInput2) %>% 
      select(Name_All) %>%  unlist() %>%    last() })   
   # Match Energy to Table----
  insertsAenergyfetch1 <- eventReactive(input$InsertsAInput1,{
    Data_Insert_new() %>%     filter(Name_All == input$InsertsAInput1) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })     
  insertsBenergyfetch1 <- eventReactive(input$InsertsBInput1,{
    Data_Insert_new() %>%     filter(Name_All == input$InsertsBInput1) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })  
  insertsAenergyfetch2 <- eventReactive(input$InsertsAInput2,{
    Data_Insert_new() %>%     filter(Name_All == input$InsertsAInput2) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })    
  insertsBenergyfetch2 <- eventReactive(input$InsertsBInput2,{
    Data_Insert_new() %>%     filter(Name_All == input$InsertsBInput2) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })  
  
   # Generate Outputs----
  insertsAname1 <- renderText(as.character(insertsAnamefetch1()))
  insertsBname1 <- renderText(as.character(insertsBnamefetch1()))
  insertsAname2 <- renderText(as.character(insertsAnamefetch2()))
  insertsBname2 <- renderText(as.character(insertsBnamefetch2()))
  
   # Generate error or energy number ----
  output$insertsAEnergyNum1 <-renderText({
    validate(
      need(input$insertsAfrac1 >= 0, "Mass of inserts cannot be negative (Go to Initial Page)"),
      need(sum(input$insertsAfrac1,input$insertsBfrac1) < finalweight1(), "Mass of inserts cannot be greater than part mass (Go to Initial Page)")      ) 
    ({paste(insertsAenergyfetch1(), "MJ/kg")}                )})
  
  output$insertsAEnergyNum2 <- renderText({    
    validate(
      need(input$insertsAfrac2 >= 0, "Mass of inserts cannot be negative (Go to Initial Page)"),
      need(sum(input$insertsAfrac2,input$insertsBfrac2) < finalweight2(), "Mass of inserts cannot be greater than part mass (Go to Initial Page)")      ) 
    ({ paste(insertsAenergyfetch2(), "MJ/kg")}               )})
  
  output$insertsBEnergyNum1 <-renderText({
    validate(
      need(input$insertsBfrac1 >= 0, "Mass of inserts cannot be negative (Go to Initial Page)"),
      need(sum(input$insertsAfrac1,input$insertsBfrac1) < finalweight1(), "Mass of inserts cannot be greater than part mass (Go to Initial Page)")      ) 
    ({paste(insertsBenergyfetch1(), "MJ/kg")}                )})
  
  output$insertsBEnergyNum2 <- renderText({    
    validate(
      need(input$insertsBfrac2 >= 0, "Mass of inserts cannot be negative (Go to Initial Page)"),
      need(sum(input$insertsAfrac2,input$insertsBfrac2) < finalweight2(), "Mass of inserts cannot be greater than part mass (Go to Initial Page)")      ) 
    ({ paste(insertsBenergyfetch2(), "MJ/kg")}               )})
  
   # Citations----
 insertAcitefetch1 <- eventReactive(input$InsertsAInput1,{
    Data_Cite %>%       filter(Name == input$InsertsAInput1) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  insertAcitefetch2 <- eventReactive(input$InsertsAInput2,{
    Data_Cite %>%       filter(Name == input$InsertsAInput2) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  
  insertAcite1 <- renderText(as.character(insertAcitefetch1()))
  observeEvent(input$InsertsAInput1, {
    validate(need(insertsAname1() != 'NA', ""), need(insertsAname1() != "Not Used", "")) 
    showNotification(paste("Citation for ", insertsAname1(), ": ",insertAcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  insertAcite2 <- renderText(as.character(insertAcitefetch2()))
  observeEvent(input$InsertsAInput2, {
    validate(need(insertsAname2() != 'NA', ""), need(insertsAname2() != "Not Used", "")) 
    showNotification(paste("Citation for ", insertsAname2(), ": ",insertAcite2()), duration = 5, closeButton = TRUE, type = "message")})
  
  insertBcitefetch1 <- eventReactive(input$InsertsBInput1,{
    Data_Cite %>%       filter(Name == input$InsertsBInput1) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  insertBcitefetch2 <- eventReactive(input$InsertsAInput2,{
    Data_Cite %>%       filter(Name == input$InsertsBInput2) %>% 
      select(Source) %>%     unlist() %>%  last()    })
  
  insertBcite1 <- renderText(as.character(insertBcitefetch1()))
  observeEvent(input$InsertsBInput1, {
    validate(need(insertsBname1() != 'NA', ""), need(insertsBname1() != "Not Used", "")) 
    showNotification(paste("Citation for ", insertsBname1(), ": ",insertBcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  insertBcite2 <- renderText(as.character(insertBcitefetch2()))
  observeEvent(input$InsertsBInput2, {
    validate(need(insertsBname2() != 'NA', ""), need(insertsBname2() != "Not Used", "")) 
    showNotification(paste("Citation for ", insertsBname2(), ": ",insertBcite2()), duration = 5, closeButton = TRUE, type = "message")})
  }
  # Intermediate ----
  {
   # Recall from upload (update scrap is lower) ----
  observeEvent(input$re_input1, {
    updateNumericInput(session, "intscraprecycle1", value = whichselect(Re_input1.df(), Input_List1, "intscraprecycle"))
  })
  observeEvent(input$re_input2, {
    updateNumericInput(session, "intscraprecycle2", value = whichselect(Re_input2.df(), Input_List2, "intscraprecycle"))
  })
  
   # Build dataframe for Box ----
  qintyn1 <- reactive(as.logical(checkallYN(moldnamecheck1(), Re_input1.df(), input$goint, input$Re_Custom, "intYN")))
  qintyn2 <- reactive(as.logical(checkallYN(moldnamecheck2(), Re_input2.df(), input$goint, input$Re_Custom, "intYN")))

    observe(updateCheckboxInput(session, "intYN1", value = qintyn1()))
  observe(updateCheckboxInput(session, "intYN2", value = qintyn2()))
  
  Data_Int_new <- reactiveValues()
  Data_Int_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Int
    } else {  subset(Data_All_df(), Table_All == "Int")})
  
   # Make List for Box: choose if check box is selected----
  int1_selected <- reactive(whichselect2(Re_input1.df(), Input_List1, input$goexample, Ex_List1, "intInput"))
  int2_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"intInput"))
  
  observeEvent(c(input$intYN1, input$goint, input$moldingInput1, input$Re_Custom) , {
      intlist1 <- intlistfxn(input$moldingInput1)
     if (input$intYN1 == 0) {
       updateSelectizeInput(session, 'intInput1',
                            choices = intlist1,
                            selected = int1_selected(),
                            server = TRUE)
     } else {
       updateSelectizeInput(session, 'intInput1',
                            choices = Data_Int_new()$Name_All,
                            selected = int1_selected(),
                            server = TRUE)
     }      })
  
  observeEvent(c(input$intYN2, input$goint, input$moldingInput2, input$Re_Custom) , {
    intlist2 <- intlistfxn(input$moldingInput2)
      if (input$intYN2 == 0) {
        updateSelectizeInput(session, 'intInput2',
                             choices = intlist2,
                             selected = int2_selected(),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, 'intInput2',
                             choices = Data_Int_new()$Name_All,
                             selected = int2_selected(),
                             server = TRUE)
      }    })

   # Match Names to Table----
  intnamefetch1 <- eventReactive(input$intInput1,{
    Data_Int_new() %>%     filter(Name_All == input$intInput1) %>% 
    select(Name_All) %>%  unlist() %>%    last() })  
  intnamefetch2 <- eventReactive(input$intInput2,{
    Data_Int_new() %>%     filter(Name_All == input$intInput2) %>% 
      select(Name_All) %>%  unlist() %>%    last() })  
  
   # Match Energy to Name----
  intenergyfetch1 <- eventReactive(input$intInput1,{
    Data_Int_new() %>%     filter(Name_All == input$intInput1) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })  
  intenergyfetch2 <- eventReactive(input$intInput2,{
    Data_Int_new() %>%     filter(Name_All == input$intInput2) %>% 
      select(Energy_All) %>%  unlist() %>%    last() }) 
  
   # Match Scrap to Name----
  intscrapfetch1 <- eventReactive(input$intInput1,{
    Data_Int_new() %>%     filter(Name_All == input$intInput1) %>% 
      select(Scrap_Int) %>%  unlist() %>%    last() }) 
  intscrapfetch2 <- eventReactive(input$intInput2,{
    Data_Int_new() %>%     filter(Name_All == input$intInput2) %>% 
      select(Scrap_Int) %>%  unlist() %>%    last() }) 
  
   # Match PrepregYN to Name (for determining if matrix material needs to be included in intermediate scrap)----
  intprepregfetch1 <- eventReactive(input$intInput1,{
    Data_Int_new() %>%     filter(Name_All == input$intInput1) %>% 
      select(Prepreg_Int) %>%  unlist() %>%    last() }) 

  intprepregfetch2 <- eventReactive(input$intInput2,{
    Data_Int_new() %>%     filter(Name_All == input$intInput2) %>% 
      select(Prepreg_Int) %>%  unlist() %>%    last() }) 
  
   # Generate Outputs----
  intname1 <- output$intname1d  <- output$intname1c <-output$intname1 <- renderText(as.character(intnamefetch1()))
  intname2 <- output$intname2d  <- output$intname2c <-output$intname2 <- renderText(as.character(intnamefetch2()))
  output$intscrapNum1c <-output$intscrapNum1 <- renderText({paste(intscrapfetch1()*100, "%")})
  output$intscrapNum2c <-output$intscrapNum2 <- renderText({paste(intscrapfetch2()*100, "%")})
  int.prepregYN1 <-renderText(intprepregfetch1())
  int.prepregYN2<-renderText(intprepregfetch2())
  
   # Update scrap to match int choice----
  observeEvent(input$intInput1,{
    if (is.null(input$re_input1)) {
      ints1 <-  intscrapfetch1()
      updateNumericInput(session, "intscrapUSERNum1", value = (ints1*100)) 
    } else {
      updateNumericInput(session, "intscrapUSERNum1", value = whichselect(Re_input1.df(), Input_List1, "intscrapUSERNum"))
    } })
  
  observeEvent(input$intInput2,{
    if (is.null(input$re_input2)) {
      ints2 <-  intscrapfetch2()
      updateNumericInput(session, "intscrapUSERNum2", value = (ints2*100)) 
    } else {
      updateNumericInput(session, "intscrapUSERNum2", value = whichselect(Re_input2.df(), Input_List2, "intscrapUSERNum"))
    } })
  
   # Generate error or energy number----
  output$intEnergyNum1 <-renderText({
    validate(
      need(intnamefetch1(), "Choose Intermediate Technology"),
      need(input$intscrapUSERNum1 >=0, "Scrap cannot be negative"),
      need(input$intscrapUSERNum1 <=100, "Scrap cannot be greater than 100%")      ) 
    ({paste(intenergyfetch1(), "MJ/kg")}                   )})
  
  output$intEnergyNum2 <- renderText({
    validate(
      need(intnamefetch2(), "Choose Intermediate Technology"),
      need(input$intscrapUSERNum2 >=0, "Scrap cannot be negative"),
      need(input$intscrapUSERNum2 <=100, "Scrap cannot be greater than 100%")      ) 
    ({paste(intenergyfetch2(), "MJ/kg")}                   )})
  
   # Citations----
  intcitefetch1 <- eventReactive(input$intInput1,{
    Data_Cite %>%       filter(Name == input$intInput1) %>% 
      select(Source) %>%    unlist() %>%   last()    })
  intcitefetch2 <- eventReactive(input$intInput2,{
    Data_Cite %>%       filter(Name == input$intInput2) %>% 
      select(Source) %>%    unlist() %>%   last()    })
  
  intcite1 <- renderText(as.character(intcitefetch1()))
  observeEvent(input$intInput1, {
    validate(need(intname1() != 'NA', "")) 
    showNotification(paste("Citation for ", intname1(), ": ",intcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  intcite2 <- renderText(as.character(intcitefetch2()))
  observeEvent(input$intInput2, {
    validate(need(intname2() != 'NA', "")) 
    showNotification(paste("Citation for ", intname2(), ": ",intcite2()), duration = 5, closeButton = TRUE, type = "message")})
  }
  # Cure ----
  {
   # Recall from Upload-----
  observeEvent(input$re_input1, {
    updateNumericInput(session, "moldyieldUSERNum1", value = whichselect(Re_input1.df(), Input_List1, "moldyieldUSERNum"))
    updateNumericInput(session, "moldyieldrecycle1", value = whichselect(Re_input1.df(), Input_List1, "moldyieldrecycle"))
  })
  observeEvent(input$re_input2, {
    updateNumericInput(session, "moldyieldUSERNum2", value = whichselect(Re_input2.df(), Input_List2, "moldyieldUSERNum"))
    updateNumericInput(session, "moldyieldrecycle2", value = whichselect(Re_input2.df(), Input_List2, "moldyieldrecycle"))
  })

  qcureyn1 <- reactive(as.logical(checkallYN(moldnamecheck1(), Re_input1.df(), input$gocure, input$Re_Custom, "cureYN")))
  qcureyn2 <- reactive(as.logical(checkallYN(moldnamecheck2(), Re_input2.df(), input$gocure, input$Re_Custom, "cureYN")))

  observe(updateCheckboxInput(session, "cureYN1", value = qcureyn1()))
  observe(updateCheckboxInput(session, "cureYN2", value = qcureyn2()))
  
   # Make dataframe for box if custom values used----
    calccureph <- reactive(calc.ph(
    input$cure_add_E_P_h1, input$cure_add_E_u_h1, input$cure_add_E_per_h1, input$cure_add_E_t_h1,
    input$cure_add_E_P_h2, input$cure_add_E_u_h2, input$cure_add_E_per_h2, input$cure_add_E_t_h2,
    input$cure_add_E_P_h3, input$cure_add_E_u_h3, input$cure_add_E_per_h3, input$cure_add_E_t_h3,
    input$cure_add_E_P_h4, input$cure_add_E_u_h4, input$cure_add_E_per_h4, input$cure_add_E_t_h4,
    input$cure_add_E_P_h5, input$cure_add_E_u_h5, input$cure_add_E_per_h5, input$cure_add_E_t_h5,
    input$cure_add_E_P_h6, input$cure_add_E_u_h6, input$cure_add_E_per_h6, input$cure_add_E_t_h6,
    input$cure_add_E_P_h7, input$cure_add_E_u_h7, input$cure_add_E_per_h7, input$cure_add_E_t_h7,
    input$cure_add_E_P_h8, input$cure_add_E_u_h8, input$cure_add_E_per_h8, input$cure_add_E_t_h8,
    input$cure_add_E_P_h9, input$cure_add_E_u_h9, input$cure_add_E_per_h9, input$cure_add_E_t_h9,
    input$cure_add_E_P_h0, input$cure_add_E_u_h0, input$cure_add_E_per_h0, input$cure_add_E_t_h0))
  calccureE <- reactive(calcenergy(input$cure_add_E_m, 
                                   input$cure_add_E_P_M, input$cure_add_E_per_M, input$cure_add_E_t_M, 
                                   input$cure_add_E_P_p, input$cure_add_E_per_p, input$cure_add_E_t_p,
                                   input$cure_add_E_P_c, input$cure_add_E_per_c, input$cure_add_E_t_c,
                                   calccureph(),
                                   input$cure_add_E_P_o, input$cure_add_E_u_o, input$cure_add_E_per_o, input$cure_add_E_t_o))
  
  output$calcedcureE <- renderText({paste(signif(calccureE(), digits = 3), "MJ/kg")})
  
  cure.Ener <- reactiveValues()
  cure.Ener <- reactive(whichenergy(input$cure_add_EYN, calccureE(), input$cure_add_E_Y))
  
  Data_Cure_new  <- reactiveValues()
  Data_Cure_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Cure
    } else {  subset(Data_All_df(), Table_All == "Cure")})
  
   # Make list for box ----
  cure1_selected <- reactive(whichselect2(Re_input1.df(), Input_List1, input$goexample, Ex_List1, "cureInput"))
  cure2_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"cureInput"))
  
  observeEvent(c(input$cureYN1, input$gocure, input$moldingInput1 , input$Re_Custom) , {
    curelist1 <- curelistfxn(input$moldingInput1, allcure, onlymoldcure, wetcure, vbcure)
      if (input$cureYN1 == 0) {
        updateSelectizeInput(session, 'cureInput1',
                             choices = curelist1,
                             selected = cure1_selected(),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, 'cureInput1',
                             choices = Data_Cure_new()$Name_All,
                             selected = cure1_selected(),
                             server = TRUE)
      } })
  
  observeEvent(c(input$cureYN2, input$gocure, input$moldingInput2, input$Re_Custom) , {
    curelist2 <- curelistfxn(input$moldingInput2, allcure, onlymoldcure, wetcure, vbcure)
      if (input$cureYN2 == 0) {
        updateSelectizeInput(session, 'cureInput2',
                             choices = curelist2,
                             selected = cure2_selected(),
                             server = TRUE)
      } else {
        updateSelectizeInput(session, 'cureInput2',
                             choices = Data_Cure_new()$Name_All,
                             selected = cure2_selected(),
                             server = TRUE)
      } })

   # Match Names to Table----
  curenamefetch1 <- eventReactive(input$cureInput1,{
  Data_Cure_new() %>%     filter(Name_All == input$cureInput1) %>% 
    select(Name_All) %>%  unlist() %>%    last() }) 
  
  curenamefetch2 <- eventReactive(input$cureInput2,{
    Data_Cure_new() %>%     filter(Name_All == input$cureInput2) %>% 
      select(Name_All) %>%  unlist() %>%    last() }) 
  
   # Match Energy to Table----
  cureenergyfetch1 <- eventReactive(input$cureInput1,{
    Data_Cure_new() %>%     filter(Name_All == input$cureInput1) %>% 
      select(Energy_All) %>%  unlist() %>%    last() }) 
  
  cureenergyfetch2 <- eventReactive(input$cureInput2,{
    Data_Cure_new() %>%     filter(Name_All == input$cureInput2) %>% 
      select(Energy_All) %>%  unlist() %>%    last() }) 
  
   # Generate Outputs----
  output$curename1d  <- curename1 <- renderText(as.character(curenamefetch1()))
  output$curename2d  <- curename2 <- renderText(as.character(curenamefetch2()))

   # Generate error or energy number----
  output$cureEnergyNum1 <-renderText({
    validate(
      need(curenamefetch1(), "Choose Curing Technology"),
      need(input$moldyieldUSERNum1 >=0, "Yield cannot be negative"),
      need(input$moldyieldUSERNum1 <=100, "Yield cannot be greater than 100%")      ) 
    ({paste(cureenergyfetch1(), "MJ/kg")}                   )})
  
  output$cureEnergyNum2 <- renderText({
    validate(
      need(curenamefetch2(), "Choose Curing Technology"),
      need(input$moldyieldUSERNum2 >=0, "Yield cannot be negative"),
      need(input$moldyieldUSERNum2 <=100, "Yield cannot be greater than 100%")      ) 
    ({paste(cureenergyfetch2(), "MJ/kg")}                   )})
  
   # Citations----
  curecitefetch1 <- eventReactive(input$cureInput1,{
    Data_Cite %>%       filter(Name == input$cureInput1) %>% 
      select(Source) %>%    unlist() %>%   last()    })
  curecitefetch2 <- eventReactive(input$cureInput2,{
    Data_Cite %>%       filter(Name == input$cureInput2) %>% 
      select(Source) %>%    unlist() %>%   last()    })
  
  curecite1 <- renderText(as.character(curecitefetch1()))
  observeEvent(input$cureInput1, {
    validate(need(curename1() != 'NA', "")) 
    showNotification(paste("Citation for ", curename1(), ": ",curecite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  curecite2 <- renderText(as.character(curecitefetch2()))
  observeEvent(input$cureInput2, {
    validate(need(curename2() != 'NA', "")) 
    showNotification(paste("Citation for ", curename2(), ": ",curecite2()), duration = 5, closeButton = TRUE, type = "message")})
  }
  # Finish ----
  {
   # Build Dataframe for Box ----
  Data_Finish_new  <- reactiveValues()
  Data_Finish_new  <- reactive(
    if (is.null(Data_All_df())) {Data_Finish
    } else {  subset(Data_All_df(), Table_All == "Finish")})
  
  observeEvent(input$re_input1, {
    updateNumericInput(session, "finishscrap1", value = whichselect(Re_input1.df(), Input_List1, "finishscrap"))
    updateNumericInput(session, "finishscraprecycle1", value = whichselect(Re_input1.df(), Input_List1, "finishscraprecycle"))
  })
  observeEvent(input$re_input2, {
    updateNumericInput(session, "finishscrap2", value = whichselect(Re_input2.df(), Input_List2, "finishscrap"))
    updateNumericInput(session, "finishscraprecycle2", value = whichselect(Re_input2.df(), Input_List2, "finishscraprecycle"))
  })
  
   # Make List for Box ----
  fin1_selected <- reactive(whichselect(Re_input1.df(), Input_List1,"finishInput"))
  fin2_selected <- reactive(whichselect(Re_input2.df(), Input_List2,"finishInput"))
  observe( { 
    updateSelectizeInput(session, 'finishInput1',
                         choices = Data_Finish_new()$Name_All,
                         selected =  "None",
                         server = TRUE)
    updateSelectizeInput(session, 'finishInput2',
                         choices = Data_Finish_new()$Name_All,
                         selected =  "None",
                         server = TRUE)
  })
  
  observeEvent(input$re_input1, {updateSelectizeInput(session, 'finishInput1', selected = fin1_selected())})
  observeEvent(input$re_input2, {updateSelectizeInput(session, 'finishInput2', selected = fin2_selected())})
  
   # Match Names to Table----
  finishnamefetch1 <- eventReactive(input$finishInput1,{
  Data_Finish_new() %>%     filter(Name_All == input$finishInput1) %>% 
    select(Name_All) %>%  unlist() %>%    last() }) 
    finishnamefetch2 <- eventReactive(input$finishInput2,{
    Data_Finish_new() %>%     filter(Name_All == input$finishInput2) %>% 
      select(Name_All) %>%  unlist() %>%    last() })   
  
   # Match Energy to Table-----
  finishenergyfetch1 <- eventReactive(input$finishInput1,{
    Data_Finish_new() %>%     filter(Name_All == input$finishInput1) %>% 
      select(Energy_All) %>%  unlist() %>%    last() })   
  finishenergyfetch2 <- eventReactive(input$finishInput2,{
    Data_Finish_new() %>%     filter(Name_All == input$finishInput2) %>% 
      select(Energy_All) %>%  unlist() %>%    last() }) 
  
   # Generate Outputs ----
  output$finishname1d  <- finishname1 <- renderText(as.character(finishnamefetch1()))
  output$finishname2d  <- finishname2 <- renderText(as.character(finishnamefetch2()))

  output$finishEnergyNum1 <-renderText({
    validate(
      need(input$finishscrap1 >=0, "Scrap cannot be negative"),
      need(input$finishscrap1 <=100, "Scrap cannot be greater than 100%")      ) 
    ({paste(finishenergyfetch1(), "MJ/kg")}                   )})
  
  output$finishEnergyNum2 <- renderText({
    validate(
      need(input$finishscrap2 >=0, "Scrap cannot be negative"),
      need(input$finishscrap2 <=100, "Scrap cannot be greater than 100%")      ) 
    ({paste(finishenergyfetch2(), "MJ/kg")}                   )})
  
   # Citations----
  finishcitefetch1 <- eventReactive(input$finishInput1,{
    Data_Cite %>%       filter(Name == input$finishInput1) %>% 
      select(Source) %>%    unlist() %>%   last()    })
  finishcitefetch2 <- eventReactive(input$finishInput2,{
    Data_Cite %>%       filter(Name == input$finishInput2) %>% 
      select(Source) %>%    unlist() %>%   last()    })
  
  finishcite1 <- renderText(as.character(finishcitefetch1()))
  observeEvent(input$finishInput1, {
    validate(need(finishname1() != 'NA', ""), need(finishname1() != "None", "")) 
    showNotification(paste("Citation for ", finishname1(), ": ",finishcite1()), duration = 5, closeButton = TRUE, type = "message")})
  
  finishcite2 <- renderText(as.character(finishcitefetch2()))
  observeEvent(input$finishInput2, {
    validate(need(finishname2() != 'NA', ""), need(finishname2() != "None", "")) 
    showNotification(paste("Citation for ", finishname2(), ": ",finishcite2()), duration = 5, closeButton = TRUE, type = "message")})
  }
  # Citation Page ----
  observeEvent(input$cite_type, {
    citelist <- citefxn(input$cite_type)
    updateSelectizeInput(session, 'cite_specific',
                         choices = citelist,
                         selected = NULL,
                         server = TRUE)
  })
  
  citationfetch1 <- eventReactive(input$cite_specific,{
    cite_full1[cite_name %in% input$cite_specific]  })
  citationfetch2 <- eventReactive(input$cite_specific,{
    cite_full2[cite_name %in% input$cite_specific]  })
  citationfetch3 <- eventReactive(input$cite_specific,{
    cite_full3[cite_name %in% input$cite_specific]  })
  citationfetch4 <- eventReactive(input$cite_specific,{
    cite_full4[cite_name %in% input$cite_specific]  })
  citationfetch5 <- eventReactive(input$cite_specific,{
    cite_full5[cite_name %in% input$cite_specific]  })
  output$citation1 <- renderText(as.character(citationfetch1()))
  output$citation2 <- renderText(as.character(citationfetch2()))
  output$citation3 <- renderText(as.character(citationfetch3()))
  output$citation4 <- renderText(as.character(citationfetch4()))
  output$citation5 <- renderText(as.character(citationfetch5()))
  
  # Return Mass/Massfracs to zero if "Use ..." checkboxes are false ----
  {
 # Set Inserts mass to zero----
 observeEvent(input$insertsAUSERYN1, {
   if (!input$insertsAUSERYN1){
     updateNumericInput(session, "insertsAfrac1", value = (0)) 
   updateNumericInput(session, "insertsBfrac1", value = (0)) 
   updateCheckboxInput(session, "insertsBUSERYN1", value = FALSE)  }
                 })
 
 observeEvent(input$insertsBUSERYN1, {
   if (!input$insertsBUSERYN1){
     updateNumericInput(session, "insertsBfrac1", value = (0)) }
    })
 
 observeEvent(input$insertsAUSERYN2, {
   if (!input$insertsAUSERYN2){
     updateNumericInput(session, "insertsAfrac2", value = (0)) 
     updateNumericInput(session, "insertsBfrac2", value = (0)) 
     updateCheckboxInput(session, "insertsBUSERYN2", value = FALSE)   }
  })
 
 observeEvent(input$insertsBUSERYN2, {
   if (!input$insertsBUSERYN2){
     updateNumericInput(session, "insertsBfrac2", value = (0)) }
  })
 
 # set Additional Matrix fractions to zero, choices to "Not Used" and other "Use Additional Matrix" to false ----
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
  }
  # Calculations ----
    {
    # Change User Input --> Variables (Yield & Mass Fracs) ----
      {
       # Yield ----
      layup.yield1 <- reactive(yield_layup(input$intscrapUSERNum1,input$intscraprecycle1))
      layup.yield2 <- reactive(yield_layup(input$intscrapUSERNum2,input$intscraprecycle2))
      mold.yield1 <- reactive(yield_mold(input$moldyieldUSERNum1, input$moldyieldrecycle1))
      mold.yield2 <- reactive(yield_mold(input$moldyieldUSERNum2, input$moldyieldrecycle2))
      finish.yield1 <- reactive(yield_finish(input$finishscrap1, input$finishscraprecycle1))
      finish.yield2 <- reactive(yield_finish(input$finishscrap2, input$finishscraprecycle2))
      
      # mass fracs ----
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
      
    }
    # Mass Fractions converion (calculations treats inserts as part of sum(mass fractions) = 1 )----  
    raw.to.actual.fracs1 <- reactive(Data_mass_fxn(finalweight1(), raw.f.f1(), raw.f.pm1(), raw.f.ma1(), raw.f.mb1(), raw.f.mc1(), m.ia1(), m.ib1()))
    raw.to.actual.fracs2 <- reactive(Data_mass_fxn(finalweight2(), raw.f.f2(), raw.f.pm2(), raw.f.ma2(), raw.f.mb2(), raw.f.mc2(), m.ia2(), m.ib2()))
    
     f.raw.sum1 <- reactive(sum(raw.f.f1(), raw.f.pm1(), raw.f.ma1(), raw.f.mb1(), raw.f.mc1()))
     
     output$massfracsum1 <- renderText({ paste(f.raw.sum1() * 100, "%")})
     
     f.raw.sum2 <- reactive(sum(raw.f.f2(), raw.f.pm2(), raw.f.ma2(), raw.f.mb2(), raw.f.mc2()))
     output$massfracsum2 <- renderText( { paste(f.raw.sum2() * 100, "%")}  )
    
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
     yield_data1.df<- reactiveValues()
    yield_data2.df<- reactiveValues()
    
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
    energy_data1.df <- reactiveValues()
    energy_data2.df <- reactiveValues()
   
    
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
     output$yieldtab1 <- renderTable(yield_data1.df())
     output$energytab1 <- renderTable(energy_data1.df())
     output$yieldtab2 <- renderTable(yield_data2.df())
     output$energytab2 <- renderTable(energy_data2.df())

    
    }
  # Final Display ----
    {
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
    
    # Reactive Mass for table ----
    m.f.fib1 <- reactive(energy_data1.df()$mass_materials[1])
    m.f.int1 <- reactive(energy_data1.df()$mass_materials[2])
    m.f.pm1  <- reactive(energy_data1.df()$mass_materials[3])
    m.f.ma1  <- reactive(energy_data1.df()$mass_materials[4])
    m.f.mb1  <- reactive(energy_data1.df()$mass_materials[5])
    m.f.mc1  <- reactive(energy_data1.df()$mass_materials[6])
    m.f.ia1  <- reactive(energy_data1.df()$mass_materials[7])
    m.f.ib1  <- reactive(energy_data1.df()$mass_materials[8])
    m.f.mold1 <- reactive(energy_data1.df()$mass_materials[9])
    m.f.cure1 <- reactive(energy_data1.df()$mass_materials[10])
    m.f.fin1  <- reactive(energy_data1.df()$mass_materials[11])
    
    m.f.fib2 <- reactive(energy_data2.df()$mass_materials[1])
    m.f.int2 <- reactive(energy_data2.df()$mass_materials[2])
    m.f.pm2  <- reactive(energy_data2.df()$mass_materials[3])
    m.f.ma2  <- reactive(energy_data2.df()$mass_materials[4])
    m.f.mb2  <- reactive(energy_data2.df()$mass_materials[5])
    m.f.mc2  <- reactive(energy_data2.df()$mass_materials[6])
    m.f.ia2  <- reactive(energy_data2.df()$mass_materials[7])
    m.f.ib2  <- reactive(energy_data2.df()$mass_materials[8])
    m.f.mold2 <- reactive(energy_data2.df()$mass_materials[9])
    m.f.cure2 <- reactive(energy_data2.df()$mass_materials[10])
    m.f.fin2 <- reactive(energy_data2.df()$mass_materials[11])
    
    # Reactive Energies for table ---- 
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
    
    # Summary Tables ----
    Mat1.E.df <- reactive({  
      validate(
        need(sum(input$moldfracUSERNum1, input$primatrixfrac1, input$othermatrixAfrac1, 
                 input$othermatrixBfrac1, input$othermatrixCfrac1) == 100,      "Sum of Mass Fractions should add to 100% (Go to Matrix Page)")
        , need(m.inserts1() < finalweight1(), "Inserts should not weigh more than the final part (Go to Initial Page)")
        , need(raw.f.f1() >= 0,               "Mass Fractions must not be negative (Go to Fiber Page)")
        , need(raw.f.f1() <= 1,               "Mass Fractions must not be greater than 100% (Go to Fiber Page)")
        , need(raw.f.pm1() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
        , need(raw.f.ma1() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
        , need(raw.f.mb1() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
        , need(raw.f.mc1() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
        , need(E.fib1(),                      "Please chose a fiber type (Go to Fiber Page)")  
        , need(E.pm1(),                       "Please chose a primary matrix material (Go to Matrix Page)")   
        , need(E.int1(),                      'Please chose a fiber intermediate ("Not Used" is an option) (Go to Intermediate Page)')    
        , need(E.mold1(),                     "Please chose a molding option (Go to Initial Page)") 
        , need(E.cure1(),                     'Please chose a curing option ("Cures in mold" is an option) (Go to Molding Page)')
           )
     
     data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total"),
      Choice = c(n.f1(), n.pm1(), n.ma1(), n.mb1(), n.mc1(), n.ia1(), n.ib1(), " "),
      "Effecive Mass Fraction" = c(f.f1()*100  , f.pm1()*100 , f.ma1()*100 , f.mb1()*100 , f.mc1()*100 , f.ia1()*100 , f.ib1()*100 , sum(f.f1()*100  , f.pm1()*100 , f.ma1()*100 , f.mb1()*100 , f.mc1()*100 , f.ia1()*100 , f.ib1()*100 )),
      "Embodied Energy (MJ/part)" = c(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1(), sum(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()))
    )})
    
    Process1.E.df <- reactive({
      validate(
        need(sum(input$moldfracUSERNum1, input$primatrixfrac1, input$othermatrixAfrac1, 
                 input$othermatrixBfrac1, input$othermatrixCfrac1) == 100,      "")
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
          need(sum(input$moldfracUSERNum2, input$primatrixfrac2, input$othermatrixAfrac2, 
                   input$othermatrixBfrac2, input$othermatrixCfrac2) == 100,      "Sum of Mass Fractions should add to 100% (Go to Matrix Page)")
          , need(m.inserts2() < finalweight2(), "Inserts should not weigh more than the final part (Go to Initial Page)")
          , need(raw.f.f2() >= 0,               "Mass Fractions must not be negative (Go to Fiber Page)")
          , need(raw.f.f2() <= 1,               "Mass Fractions must not be greater than 100% (Go to Fiber Page)")
          , need(raw.f.pm2() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
          , need(raw.f.ma2() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
          , need(raw.f.mb2() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
          , need(raw.f.mc2() >= 0,              "Mass Fractions must not be negative (Go to Matrix Page)")
          , need(E.fib2(),                      "Please chose a fiber type (Go to Fiber Page)")  
          , need(E.pm2(),                       "Please chose a primary matrix material (Go to Matrix Page)")   
          , need(E.int2(),                      'Please chose a fiber intermediate ("Not Used" is an option) (Go to Intermediate Page)')    
          , need(E.mold2(),                     "Please chose a molding option (Go to Initial Page)") 
          , need(E.cure2(),                     'Please chose a curing option ("Cures in mold" is an option) (Go to Molding Page)')
                )
        
     data_frame(
      Material = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total"),
      Choice = c(n.f2(), n.pm2(), n.ma2(), n.mb2(), n.mc2(), n.ia2(), n.ib2(), " "),
      "Effective Mass Fraction" = c(f.f2()*100  , f.pm2()*100 , f.ma2()*100 , f.mb2()*100 , f.mc2()*100 , f.ia2()*100 , f.ib2()*100 , sum(f.f2()*100  , f.pm2()*100 , f.ma2()*100 , f.mb2()*100 , f.mc2()*100 , f.ia2()*100 , f.ib2()*100)),
      "Embodied Energy (MJ/part)" = c(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2(), sum(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()))
    )})
    
    Process2.E.df <- reactive({
          validate(
            need(sum(input$moldfracUSERNum1, input$primatrixfrac1, input$othermatrixAfrac1, 
                     input$othermatrixBfrac1, input$othermatrixCfrac1) == 100,      "")
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
  
    # Results Graph ----
    {
      # DF for charts 1 ----
    energy_plot.df1 <- reactive({
     validate(
       need(sum(input$moldfracUSERNum1, input$primatrixfrac1, input$othermatrixAfrac1, 
                input$othermatrixBfrac1, input$othermatrixCfrac1) == 100,      "Sum of Mass Fractions should add to 100% (1)")
       , need(m.inserts1() < finalweight1(), "Inserts (1) should not weigh more than the final part")
       , need(raw.f.f1() >= 0,               "Mass Fractions (Fiber - 1) must not be negative")
       , need(raw.f.pm1() >= 0,              "Mass Fractions (Primary Matrix - 1) must not be negative")
       , need(raw.f.ma1() >= 0,              "Mass Fractions (Matrix 1A) must not be negative")
       , need(raw.f.mb1() >= 0,              "Mass Fractions (Matrix 1B) must not be negative")
       , need(raw.f.mc1() >= 0,              "Mass Fractions (Matrix 1C) must not be negative")
       , need(E.fib1(),                      "Please chose a fiber type (1)")  
       , need(E.pm1(),                       "Please chose a primary matrix material(1)")   
       , need(E.int1(),                      'Please chose a fiber intermediate (1) ("Not Used" is an option)')    
       , need(E.mold1(),                     "Please chose a molding option (1)") 
       , need(E.cure1(),                     'Please chose a curing option (1) ("Cures in mold" is an option)')
            )
     
      data_frame(
        techset = c(rep(partname1e(), 7) ),
        process_segment = c("Fiber", "Primary Matrix Material", "Other Materials", "Intermediate", "Molding", "Curing", "Finishing"),
        Energy = c(E.f.fib1(), E.f.pm1(), sum(E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()), E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1())
        , order = c(1, 2, 3, 4, 5, 6, 7)
      )})  
    
      # DF for Charts 2 ----
    energy_plot.df2 <- reactive({
      validate(
        need(sum(input$moldfracUSERNum2, input$primatrixfrac2, input$othermatrixAfrac2, 
                   input$othermatrixBfrac2, input$othermatrixCfrac2) == 100,      "Sum of Mass Fractions should add to 100% (2)")       
        , need(m.inserts2() < finalweight2(), "Inserts (2) should not weigh more than the final part")
        , need(raw.f.f2() >= 0,               "Mass Fractions (Fiber - 2) must not be negative")
        , need(raw.f.pm2() >= 0,              "Mass Fractions (Primary Matrix - 2) must not be negative")
        , need(raw.f.ma2() >= 0,              "Mass Fractions (Matrix 2A) must not be negative")
        , need(raw.f.mb2() >= 0,              "Mass Fractions (Matrix 2B) must not be negative")
        , need(raw.f.mc2() >= 0,              "Mass Fractions (Matrix 2C) must not be negative")
        , need(E.fib2(),                      "Please chose a fiber type (2)")  
        , need(E.pm2(),                       "Please chose a primary matrix material (2)")   
        , need(E.int2(),                      'Please chose a fiber intermediate (2) ("Not Used" is an option)')    
        , need(E.mold2(),                     "Please chose a molding option (2)") 
        , need(E.cure2(),                     'Please chose a curing option (2) ("Cures in mold" is an option)')
      )
      
      data_frame(
        techset = c(rep(partname2e(), 7) ),
        process_segment = c("Fiber", "Primary Matrix Material", "Other Materials", "Intermediate", "Molding", "Curing", "Finishing"),
        Energy = c(E.f.fib2(), E.f.pm2(), sum(E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()), E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2())
        , order = c(1, 2, 3, 4, 5, 6, 7)
      )})  
    
    
    energy_plot.df <- reactive(bind_rows(energy_plot.df1(), energy_plot.df2()))
    
    energy_plot.df.p1 <- reactive({
      energy_plot.df1() %>%
        mutate(Process_Segment = reorder(process_segment, order)) })
    energy_plot.df.p2 <- reactive({
      energy_plot.df2() %>%
        mutate(Process_Segment = reorder(process_segment, order)) })
    
    # output$testtable1 <- renderTable(energy_plot.df.p1())
    # output$testtable2 <- renderTable(energy_plot.df.p2())
    
      # DF for formatting ---- 

   fillcolor <- c("Fiber" = "#2960A8", "Intermediate"  = "#74A138", "Primary Matrix Material" = "#C46827",
             "Other Materials"= "#24A7C1", "Molding" = "#8D2A1E", "Curing" = "#E2B500", "Finishing" = "#8A9FCF")
   
   energy_plot.df.p <- reactive({
     energy_plot.df() %>%
       mutate(Part = factor(techset, levels = c(partname2e(), partname1e())),
              Process_Segment = reorder(process_segment, order))
   })

   marg <- list(
     l = 50,
     r = 40,
     b = 50,
     t = 50,
     pad = 0
   )
   
      # Bar Chart ----
   output$plot1 <- renderPlotly({
    ggplot(energy_plot.df.p(), aes(x = Part, y = Energy, fill = Process_Segment )) +
       geom_bar(stat = "identity") +
       coord_flip() +
       labs(y = "Embodied Energy (MJ/part)", x = "") +
       scale_fill_manual(values = fillcolor, name = "") +

       theme_bw() + theme(  legend.title = element_blank(), legend.text = element_text(size = 12), legend.background = element_rect(color = "black")
                        , axis.text = element_text(size = 14), axis.title.x = element_text(size = 18), plot.margin = margin(t = 0, r = 0, b = 0, l = 30, unit = "pt") )
         })
   
      # Pie Charts ----

   output$plot2a <- renderPlotly(
     plot_ly(energy_plot.df.p1(), labels = ~Process_Segment, values = ~Energy, type = 'pie',
             textposition = 'auto',
             textinfo = 'percent',
             insidetextfont = list(color = '#FFFFFF', family = "sans-serif", size = 18),
             outsidetextfont = list(color = '#000000', family = "sans-serif", size = 14),
             hoverinfo = 'text',
             text = ~paste(round(Energy, 0), ' MJ'),
             textfont = list(family = "sans-serif"),
             sort = FALSE,
             marker = list(colors = fillcolor,
                           line = list(color = '#FFFFFF', width = 1)),
             showlegend = TRUE) %>%
       layout(title = partname1e(), font = list(family = "sans-serif", size = 16),
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              margin = marg,
              legend = list(bordercolor  = "#000000", borderwidth = 2, bgcolor = "rgba(255, 255, 255, 0.5)") )
   )
   
   output$plot2b <- renderPlotly(
     plot_ly(energy_plot.df.p2(), labels = ~Process_Segment, values = ~Energy, type = 'pie',
                 textposition = 'auto',
                 textinfo = 'percent',
                 insidetextfont = list(color = '#FFFFFF', family = "sans-serif", size = 18),
                 outsidetextfont = list(color = '#000000', family = "sans-serif", size = 14),
                 hoverinfo = 'text',
                 text = ~paste(round(Energy, 0), ' MJ'),
                 textfont = list(family = "sans-serif"),
                 sort = FALSE,
                 marker = list(colors = fillcolor,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = TRUE) %>%
     layout(title = partname2e(), font = list(family = "sans-serif", size = 16),
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            margin = marg,
            legend = list(bordercolor  = "#000000", borderwidth = 2, bgcolor = "rgba(255, 255, 255, 0.5)") ) 
   )
    }
    # Results Display Table ----
   Resultsdf <- reactive({
     validate(
         need(sum(input$moldfracUSERNum2, input$primatrixfrac2, input$othermatrixAfrac2, input$othermatrixBfrac2, input$othermatrixCfrac2) == 100,      "")       
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
       , need(sum(input$moldfracUSERNum2, input$primatrixfrac2, input$othermatrixAfrac2, input$othermatrixBfrac2, input$othermatrixCfrac2) == 100,      "")       
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
     
     y.f1 <- reactive(as.double(yield_data1.df()$yield_cumulative[3]))
     y.m1 <- reactive(as.double(yield_data1.df()$yield_cumulative[6]))
     y.f2 <- reactive(as.double(yield_data2.df()$yield_cumulative[3]))
     y.m2 <- reactive(as.double(yield_data2.df()$yield_cumulative[6]))
     
     
     finaldf(partname1e(), partname2e(), finalweight1(), finalweight2(),
                                 E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1(), E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(), y.f1(), y.m1(), y.f1(),
                                 E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2(), E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2(), y.f2(), y.m2(), y.f2())
     })
   
   output$ResultsTable <- renderTable(Resultsdf(), digits = 0, include.rownames = TRUE, na = "0")
      }
  # Downloads ----
    {
    # Download Results ----
   #Build tables for download
  RESULTSTABLE1 <-  reactive(data_frame(
    Stage = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total",
                         "Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
    Choice = c(n.f1(), n.pm1(), n.ma1(), n.mb1(), n.mc1(), n.ia1(), n.ib1(), " ",
               n.int1(), n.mold1(), n.cure1(), n.fin1(), " "),
    "Frac/Yield" = c(f.f1(), f.pm1(), f.ma1(), f.mb1() , f.mc1(), f.ia1(), f.ib1() , sum(f.f1()  , f.pm1() , f.ma1() , f.mb1() , f.mc1() , f.ia1() , f.ib1() ),
                   layup.yield1(), mold.yield1(), " -- " , finish.yield1(), (layup.yield1()* mold.yield1()* finish.yield1())),
    "Evaluated Mass (kg)"= round(c(m.f.fib1(), m.f.pm1(), m.f.ma1(), m.f.mb1(), m.f.mc1(), m.f.ia1(), m.f.ib1(), sum(m.f.fib1(), m.f.pm1(), m.f.ma1(), m.f.mb1(), m.f.mc1(), m.f.ia1(), m.f.ib1()),
                             m.f.int1(), m.f.mold1(), m.f.cure1(), m.f.fin1(), finalweight1()),2),
    "Specific Energy (MJ/kg)" = c(E.fib1(),  E.pm1(), E.ma1(), E.mb1(), E.mc1(),E.ia1(), E.ib1(), "----" , E.int1(), E.mold1(), E.cure1(), E.fin1(), "----"), 
    "Embodied Energy (MJ/part)" = round(c(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1(), sum(E.f.fib1(), E.f.pm1(), E.f.ma1(), E.f.mb1(), E.f.mc1(), E.f.ia1(), E.f.ib1()),
                                    E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1(), sum(E.f.int1(), E.f.mold1(), E.f.cure1(), E.f.fin1())), 2)
  ))
  
  RESULTSTABLE2 <-  reactive(data_frame(
    Stage = c("Fiber", "Primary Matrix", "Additional Matrix Material", "Additional Matrix Material","Additional Matrix Material", "Insert", "Insert", "Materials Total",
                         "Intermediate", "Molding", "Curing", "Finishing", "Processes Total"),
    Choice = c(n.f2(), n.pm2(), n.ma2(), n.mb2(), n.mc2(), n.ia2(), n.ib2(), " ",
               n.int2(), n.mold2(), n.cure2(), n.fin2(), " "),
    "Frac/Yield" = c(f.f2(), f.pm2(), f.ma2(), f.mb2() , f.mc2(), f.ia2(), f.ib2() , sum(f.f2()  , f.pm2() , f.ma2() , f.mb2() , f.mc2() , f.ia2() , f.ib2() ),
                   layup.yield2(), mold.yield2(), " -- " , finish.yield2(), (layup.yield2()* mold.yield2()* finish.yield2())),
    "Evaluated Mass (kg)" = round(c(m.f.fib2(), m.f.pm2(), m.f.ma2(), m.f.mb2(), m.f.mc2(), m.f.ia2(), m.f.ib2(), sum(m.f.fib2(), m.f.pm2(), m.f.ma2(), m.f.mb2(), m.f.mc2(), m.f.ia2(), m.f.ib2()),
                                    m.f.int2(), m.f.mold2(), m.f.cure2(), m.f.fin2(), finalweight2()),2),
    "Specific Energy (MJ/kg)" = c(E.fib2(),  E.pm2(), E.ma2(), E.mb2(), E.mc2(),E.ia2(), E.ib2(), "----" , E.int2(), E.mold2(), E.cure2(), E.fin2(), "----"),
    "Embodied Energy (MJ/part)" = round(c(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2(), sum(E.f.fib2(), E.f.pm2(), E.f.ma2(), E.f.mb2(), E.f.mc2(), E.f.ia2(), E.f.ib2()),
                                    E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2(), sum(E.f.int2(), E.f.mold2(), E.f.cure2(), E.f.fin2())),2)
      ))
  
   # output$table1c <- renderTable(RESULTSTABLE1())
   # output$table2c <- renderTable(RESULTSTABLE2())

  # Attach tables to download buttons
     output$DL_results1 <- downloadHandler(
     filename = function() {paste(input$DLR1_name, ".csv")},
     content = function(file) {
       write.csv(RESULTSTABLE1(), file)
     }   )
     output$DL_results2 <- downloadHandler(
       filename = function() {paste(input$DLR2_name, ".csv")},
       content = function(file) {
         write.csv(RESULTSTABLE2(), file)
       }   )
     
# Download Calcs     
y1table <- isolate(reactive(yield_data1.df()))
y2table <- isolate(reactive(yield_data2.df()))
e1table <- isolate(reactive(energy_data1.df()))
e2table <- isolate(reactive(energy_data2.df()))

     output$zipcalcs <- downloadHandler(
       filename = function() {paste(input$DLC_name, ".zip")},
       content = function(fname){
           Sys.setenv(R_CMDZIP = 'C:/Rtools/bin/zip')
    tmpdir <- tempdir()
    setwd(tempdir())
    print(tempdir())

   filestosave <- c("yield_table1.csv","energy_table1.csv","yield_table2.csv","energy_table2.csv")

      write.csv(y1table(), file = "yield_table1.csv")
     write.csv(y2table(), file = "yield_table2.csv")
        write.csv(e1table(), file = "energy_table1.csv")
       write.csv(e2table(), file = "energy_table2.csv")

    zip(zipfile = fname, files = filestosave)
    if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}      },

contentType = "application/zip"
         )

  # Download bkgd   
     output$info <- downloadHandler(
       filename = 'CFRP_Tool_Background_Info.zip',
       content = function(file){
         file.copy("www/CFRP_Tool_Background_Info.zip", file)     },
       contentType = "application/zip"
     )

   # Download Input File ----
     List_Values1 <- reactiveValues()
     List_Values2 <- reactiveValues()
     List_Values1 <- reactive(c(input$finalweight1, input$name1, input$moldingInput1, 
                input$insertsAUSERYN1, input$insertsAfrac1, input$insertsBUSERYN1, input$insertsBfrac1,
                input$moldfracUSERNum1, input$fiberInput1,
                input$PriMatrixInput1,input$primatrixfrac1,
                       input$othermatrixAUSERYN1,input$types1a,input$OtherMatrixAInput1,input$othermatrixAfrac1,
                       input$othermatrixBUSERYN1,input$types1b,input$OtherMatrixBInput1,input$othermatrixBfrac1,
                       input$othermatrixCUSERYN1,input$types1c,input$OtherMatrixCInput1,input$othermatrixCfrac1,
                       input$InsertsAInput1,input$InsertsBInput1,
                input$intYN1, input$intInput1, input$intscrapUSERNum1, input$intscraprecycle1,
                input$moldyieldUSERNum1,input$moldyieldrecycle1,input$cureYN1,input$cureInput1,
                     input$finishInput1,input$finishscrap1,input$finishscraprecycle1))
     
     List_Values2 <-reactive(c(input$finalweight2, input$name2, input$moldingInput2, 
                        input$insertsAUSERYN2, input$insertsAfrac2, input$insertsBUSERYN2, input$insertsBfrac2,
                input$moldfracUSERNum2, input$fiberInput2,
                input$PriMatrixInput2,input$primatrixfrac2,
                       input$othermatrixAUSERYN2,input$types2a,input$OtherMatrixAInput2,input$othermatrixAfrac2,
                       input$othermatrixBUSERYN2,input$types2b,input$OtherMatrixBInput2,input$othermatrixBfrac2,
                       input$othermatrixCUSERYN2,input$types2c,input$OtherMatrixCInput2,input$othermatrixCfrac2,
                       input$InsertsAInput2,input$InsertsBInput2,
                input$intYN2, input$intInput2, input$intscrapUSERNum2, input$intscraprecycle2,
                input$moldyieldUSERNum2,input$moldyieldrecycle2,input$cureYN2,input$cureInput2,
                     input$finishInput2,input$finishscrap2,input$finishscraprecycle2) )  
  
     
     #add column to data frame
     Final_Input1 <- reactive(inputsdf(Input_List1, List_Values1()))
     Final_Input2 <- reactive(inputsdf(Input_List2, List_Values2()))
     
      output$table1d <- renderTable(Final_Input1())
      output$table2d <- renderTable(Final_Input2())
     
      #Download     Attach tables to download buttons
     output$DL_inputs1 <- downloadHandler(
       filename = function() {paste(input$DLI1_name, ".csv")},
       content = function(file) {
         write.csv(Final_Input1(), file, row.names = FALSE)
       }   )
     output$DL_inputs2 <- downloadHandler(
       filename = function() {paste(input$DLI2_name, ".csv")},
       content = function(file) {
         write.csv(Final_Input2(), file, row.names = FALSE)
       }   )
      #Download custom data ----
      output$DL_custom <- downloadHandler(
        filename = function() {paste(input$DLCustom_name, ".csv")},
        content = function(file) {
         write.csv(Data_All_Custom(), file, row.names = FALSE)
       }   )
     
     
    }
  #  End ----
     })
