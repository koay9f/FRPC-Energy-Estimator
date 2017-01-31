
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)

shinyUI(navbarPage(theme = "bootstrap.css",
                  
                  # Application title
                  titlePanel("CFRP Energy Use Estimation Tool"),
                        # GuideTab ----
                        tabPanel("Guide",h1("Tool Guide"),img(src = "CFRPScope.png", height = 300)
                              ,p("This tool is being developed by ORNL to provide CFRP researchers and manufacturers the ability 
                              to quickly estimate the embodied energy use of their CFRP manufactuing process 
                              and compare it to other processes")
                              ),
                        
                        # InitialTab ----   
                        tabPanel("Initial", h1("Initial Inputs")
                           , p("Enter information on modeled part:  part name, part weight, and molding technology to be used")
                           , fluidRow(
                                column(6
                                  , h2("Technology Set 1")
                                  , textInput("name1", "Part Name","Part 1")
                                  , numericInput("finalweight1","Final Part Weight (kg)", 1,  min = 0,NA,NA)
                                   )
                              , column(6
                                  , h2("Technology Set 2")
                                  , textInput("name2", "Part Name","Part 2")
                                  , numericInput("finalweight2","Final Part Weight (kg)", 1, min = 0,NA,NA)
                                   )
                                 )
                           , fluidRow(h3("Molding Technology Options", align = "center"))
                       # MOLDING SELECT BOXES
                           , fluidRow(
                                column(6
                                , selectizeInput("moldingInput1", label = "",
                                       choices = NULL, selected = "", multiple = FALSE,
                                       options = list(placeholder = 'Choose Molding Technology')))
                             , column(6
                                , selectizeInput("moldingInput2", label = "",
                                       choices = NULL, selected = "", multiple = FALSE,
                                     options = list(placeholder = 'Choose Molding Technology')))
                                       )
                         , h4("Embodied Energy", align = "center")
                         , fluidRow(
                              column (3,textOutput("EnergyNum1"), align = "right"), column(3, "MJ/kg")                        
                            , column (3,textOutput("EnergyNum2"), align = "right"), column(3, "MJ/kg")
                                 )
                         #EndTab
                         ),
                        
                        # FiberTab ----
                        tabPanel("Fiber", h1("Fiber & Fiber Manufacturing") 
                                 , p("Enter information on modeled part: fiber type, tow, and fiber mass fraction")
                                 
                                 #Display Part Name & Weight & Molding Process & Fiber Fraction
                                 , fluidRow( 
                                   column(6,
                                          h2("Technology Set 1")
                                          , h5(textOutput("partname1"), textOutput("partweight1"))
                                          , h5(span("Molding Techology: ", textOutput("moldname1")))
                                   )    
                                   , column(6,
                                            h2("Technology Set 2")
                                            , h5(textOutput("partname2"), textOutput("partweight2"))
                                            , h5(span("Molding Techology: ", textOutput("moldname2")))
                                   ))
                                 #Place to enter Mass fraction?
                                 , h4("Fiber Fraction: ", align = "center")   
                                 , fluidRow(
                                   column(3, h5("Default: ")), column(3, textOutput("moldfracNum1"))
                                   , column(3, h5("Default: ")), column(3, textOutput("moldfracNum2"))
                                 )
                                 # Use user mass fraction
                                 , fluidRow(
                                   column(6,checkboxInput("moldfracUSERYN1", "Use Default?",TRUE))
                                   , column(6,checkboxInput("moldfracUSERYN2", "Use Default?",TRUE))
                                 ) 
                                 , fluidRow(
                                   column(6,  
                                      conditionalPanel(
                                        condition = "input.moldfracUSERYN1 == false",
                                        numericInput("moldfracUSERNum1","User defined Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)))
                                   , column(6,
                                      conditionalPanel(      
                                        condition = "input.moldfracUSERYN2 == false",    
                                        numericInput("moldfracUSERNum2","User defined Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)))
                                 )         
                                 # FIBER SELECT BOXES & display energy
                                 , fluidRow(
                                   column(6
                                     , selectizeInput("fiberInput1", label = "",
                                           choices = NULL, selected = "", multiple = FALSE,
                                           options = list(placeholder = 'Choose Fiber Type/Tow')))
                                   , column(6
                                     , selectizeInput("fiberInput2", label = "",
                                            choices = NULL, selected = "", multiple = FALSE,
                                            options = list(placeholder = 'Choose Fiber Type/Tow')))
                                 )
                                 , h4("Embodied Energy", align = "center")
                                 , fluidRow(
                                   column (3,textOutput("fiberEnergyNum1"), align = "right"), column(3, "MJ/kg")                        
                                   , column (3,textOutput("fiberEnergyNum2"), align = "right"), column(3, "MJ/kg")
                                 )
                                 #EndFiber   
                        ),
                        # IntTab ----
                        tabPanel("Intermediate", h1("Fiber Intermediate Manufacturing & Layup") 
                                 , p("Enter information on modeled part: intermediate type layup scrap rate")
                                 
                                 #CANNOT REDISPLAY MOLDING INFO
                                 #Display Part Name & Weight & Molding Process & Fiber Fraction
                                 , fluidRow( 
                                   column(6,
                                          h2("Technology Set 1")
                                          , h5(textOutput("partname1a"), textOutput("partweight1a"))
                                          , h5(span("Molding Techology: ", textOutput("moldname1a")))
                                   )    
                                   , column(6,
                                            h2("Technology Set 2")
                                            , h5(textOutput("partname2a"), textOutput("partweight2a"))
                                            , h5(span("Molding Techology: ", textOutput("moldname2a")))
                                   ))
                                 
                                 # INTERMEDIATE SELECT BOXES & display energy
                                 ,  fluidRow(h3("Intermediate Fiber Technology Options", align = "center")),
                                 fluidRow(
                                   column(6
                                      , selectizeInput("intInput1", label = "",
                                             choices = NULL,  selected = "",  multiple = FALSE,
                                             options = list(placeholder = 'Choose Intermediate Technology'))
                                   )
                                   , column(6
                                      , selectizeInput("intInput2", label = "",
                                             choices = NULL,  selected = "",  multiple = FALSE,
                                             options = list(placeholder = 'Choose Intermediate Technology'))
                                   ))
                                 , h4("Embodied Energy", align = "center")
                                 , fluidRow(column (3,textOutput("intEnergyNum1"), align = "right"), column(3, "MJ/kg")
                                          , column (3,textOutput("intEnergyNum2"), align = "right"), column(3, "MJ/kg"))
                                 
                                 #Display default scrap rate
                                 , br()
                                 , h5("Layup Scrap Rate", align = "center")
                                 , fluidRow(
                                   column(3, h5("Default: ")), column(3, textOutput("intscrapNum1"))
                                   , column(3, h5("Default: ")), column(3, textOutput("intscrapNum2"))
                                 )
                               
                                 # Use user scrap
                                 , fluidRow(
                                   column(6,checkboxInput("intscrapUSERYN1", "Use Default?",TRUE))
                                   , column(6,checkboxInput("intscrapUSERYN2", "Use Default?",TRUE))
                                 ) 
                                 , fluidRow(
                                   column(6,  
                                          conditionalPanel(
                                            condition = "input.intscrapUSERYN1 == false",
                                            numericInput("intscrapUSERNum1","User defined layup scrap", 0.0,  min = 0.0, max = 1.0,0.1)))
                                   , column(6,
                                            conditionalPanel(      
                                              condition = "input.intscrapUSERYN2 == false",    
                                              numericInput("intscrapUSERNum2","User defined layup scrap", 0.0,  min = 0.0, max = 1.0,0.1)))
                                 ) 
                                 
                                 #enter recycle
                                 , fluidRow(
                                   column(6,numericInput("intscraprecycle1","Layup Recycle Rate", 0.0,  min = 0.0, max = 1.0,0.1))
                                   , column(6,  numericInput("intscraprecycle2","Layup Recycle Rate", 0.0,  min = 0.0, max = 1.0,0.1))
                                 )
                                 #END TAB
                        ),
                        
                        
                        # MatrixTab ----
                        tabPanel("Matrix", h1("Matrix Materials Manufacturing"), 
                                 p("This is where users will choose the primary matrix material and other materials 
                                   (additional matrix, additives, fillers and inserts) and the mass fraction of each")),
                        
                        # MoldingTab ----  
                        tabPanel("Molding", h1("Molding, Curing & Finishing"),
                                 p("This is where users will choose curing technology, amount of finishing, 
                                   finishing scrap rate and confirm the molding technology and molding yield")
                                 ),
                        # SummaryTab ----
                        tabPanel("Summary", h1("Summary"),
                                 p("This is where users will be able to review all of the previous choices")),
                        # ResultsTab ---- 
                        tabPanel("Results", h1("Results"),
                                 p("This is where users will be able to graphically compare the two technology pathways"))
                        
                        # End ----
                        ))
