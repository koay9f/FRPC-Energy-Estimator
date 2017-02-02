
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
                                
                             #TechSet1
                                column(6
                                  , h2("Technology Set 1")
                                  , textInput("name1", "Part Name","Part 1")
                                  , numericInput("finalweight1","Final Part Weight (kg)", 1,  min = 0,NA,NA)
                                  , selectizeInput("moldingInput1", label = "Molding Technology Options",
                                                   choices = NULL, selected = "", multiple = FALSE,
                                                   options = list(placeholder = 'Choose Molding Technology    '))
                                  , column (4,h5("Embodied Energy:")),column(1,textOutput("EnergyNum1"), align = "right"), column(1, "MJ/kg")
                                   )
                             #TechSet2
                                , column(6
                                  , h2("Technology Set 2")
                                  , textInput("name2", "Part Name","Part 2")
                                  , numericInput("finalweight2","Final Part Weight (kg)", 1, min = 0,NA,NA)
                                  , selectizeInput("moldingInput2", label = "Molding Technology Options",
                                                   choices = NULL, selected = "", multiple = FALSE,
                                                   options = list(placeholder = 'Choose Molding Technology    '))
                                  , column (4,h5("Embodied Energy:")),column(1,textOutput("EnergyNum2"), align = "right"), column(1, "MJ/kg")
                                   )
                                 )
                         #EndTab
                         ),
                        
                        # FiberTab ----
                        tabPanel("Fiber", h1("Fiber & Fiber Manufacturing") 
                                 , p("Enter information on modeled part: fiber type, tow, and fiber mass fraction")
                                 
                                 #Display Part Name & Weight & Molding Process & Fiber Fraction
                                 , fluidRow(
                                   #TechSet1
                                   column(6,
                                          h2("Technology Set 1")
                                          , h5(textOutput("partname1"), textOutput("partweight1"))
                                          , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname1")))
                                          , fluidRow(column(4, h5("Default Fiber Fraction: ")), column(1, textOutput("moldfracNum1")), column(1, checkboxInput("moldfracUSERYN1", "Use Default?",TRUE)))
                                          , conditionalPanel(
                                                  condition = "input.moldfracUSERYN1 == false",
                                                  numericInput("moldfracUSERNum1","User defined Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1))   
                                          , selectizeInput("fiberInput1", label = "",
                                                  choices = NULL, selected = "", multiple = FALSE,
                                                  options = list(placeholder = 'Choose Fiber Type/Tow     '))
                                          , column (4,h5("Embodied Energy:")),column (1,textOutput("fiberEnergyNum1"), align = "right"), column(1, "MJ/kg")   
                                   )  
                                   #TechSet2
                                   , column(6,
                                            h2("Technology Set 2")
                                            , h5(textOutput("partname2"), textOutput("partweight2"))
                                            , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname2")))
                                            , fluidRow(column(4, h5("Default Fiber Fraction: ")), column(1, textOutput("moldfracNum2")), column(1, checkboxInput("moldfracUSERYN2", "Use Default?",TRUE)))
                                            , conditionalPanel(      
                                                   condition = "input.moldfracUSERYN2 == false",    
                                                   numericInput("moldfracUSERNum2","User defined Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1))
                                            , selectizeInput("fiberInput2", label = "",
                                                      choices = NULL, selected = "", multiple = FALSE,
                                                      options = list(placeholder = 'Choose Fiber Type/Tow     '))
                                            , column (4,h5("Embodied Energy:")),column (1,textOutput("fiberEnergyNum2"), align = "right"), column(1, "MJ/kg")
                                            
                                   )
                               )
                                 #EndFiber   
                        ),
                        # IntTab ----
                        tabPanel("Intermediate", h1("Fiber Intermediate Manufacturing & Layup") 
                                 , p("Enter information on modeled part: intermediate type layup scrap rate")
                                 
                                 #CANNOT REDISPLAY MOLDING INFO
                                 #Display Part Name & Weight & Molding Process & Fiber Fraction
                                 , fluidRow( 
                                   #Techset1
                                   column(6
                                          , h2("Technology Set 1")
                                          , h5(textOutput("partname1a"), textOutput("partweight1a"))
                                          , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname1a")))
                                          , selectizeInput("intInput1", label = "Choose Fiber Intermediate",
                                                           choices = NULL,  selected = "",  multiple = FALSE,
                                                           options = list(placeholder = 'Choose Fiber Intermediate     '))
                                          , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("intEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                                          , fluidRow(column(3, h5("Default Layup Scrap Rate: ")), column(1, textOutput("intscrapNum1")), column(2,checkboxInput("intscrapUSERYN1", "Use Default?",TRUE)))
                                          , conditionalPanel(
                                                condition = "input.intscrapUSERYN1 == false",
                                                numericInput("intscrapUSERNum1","User defined layup scrap", 0.0,  min = 0.0, max = 1.0,0.1))
                                          ,numericInput("intscraprecycle1",p("Layup Recycle Rate", span( h6("(% scrap that is recycled)"))), 0.0,  min = 0.0, max = 1.0,0.1)
                                            )    
                                   #TechSet2
                                   , column(6
                                            , h2("Technology Set 2")
                                            , h5(textOutput("partname2a"), textOutput("partweight2a"))
                                            , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname2a")))
                                            , selectizeInput("intInput2", label = "Choose Fiber Intermediate",
                                                             choices = NULL,  selected = "",  multiple = FALSE,
                                                             options = list(placeholder = 'Choose Fiber Intermediate     '))
                                            , fluidRow(column(4, h5("Embodied Energy:")), column (1,textOutput("intEnergyNum2"), align = "right"), column(1, "MJ/kg") )
                                            , fluidRow(column(4, h5("Default Layup Scrap Rate: ")), column(1, textOutput("intscrapNum2")), column(1,checkboxInput("intscrapUSERYN2", "Use Default?",TRUE)))
                                            , conditionalPanel(      
                                                     condition = "input.intscrapUSERYN2 == false",    
                                                      numericInput("intscrapUSERNum2","User defined layup scrap", 0.0,  min = 0.0, max = 1.0,0.1))
                                            ,  numericInput("intscraprecycle2",p("Layup Recycle Rate", span( h6("(% scrap that is recycled)"))), 0.0,  min = 0.0, max = 1.0,0.1)
                                            ))
                                  #END TAB
                        ),
                        
                        
                        # MatrixTab ----
                        tabPanel("Matrix", h1("Matrix Materials Manufacturing"), 
                                 p("Enter information on modeled part: primary matrix material and other materials 
                                   (additional matrix, additives, fillers and inserts) and the mass fraction of each")
                  , fluidRow( 
                    #Techset1
                    column(6
                           , h2("Technology Set 1")
                           , h5(textOutput("partname1b"), textOutput("partweight1b"))
                           , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname1b")))
                           , fluidRow(column(4, h5("Fiber Mass Fraction:")), column(2, textOutput("fiberfrac1")))
                           #Matrix Stuff
                           #Choose Primary Matrix
                           , selectizeInput("PriMatrixInput1", label = "Choose Primary Matrix Material",
                                           choices = NULL,  selected = "",  multiple = FALSE,
                                           options = list(placeholder = 'Choose Matrix  '))
                           , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("primatrixEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                           # mass frac 
                           , numericInput("primatrixfrac1","Primary Matrix Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                    
                          # Y/N use additional matrix materials
                          , checkboxInput("othermatrixAUSERYN1", "Use Additional Matrix Materials?",FALSE)
                          # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXA TO THAT TYPE
                          , conditionalPanel(
                             condition = "input.othermatrixAUSERYN1 == true"
                             , selectizeInput("OtherMatrixAInput1", label = "Choose Other Matrix Material",
                                           choices = NULL,  selected = "Not Used",  multiple = FALSE)
                              , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("othermatrixAEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                          # mass frac 
                              , numericInput("othermatrixAfrac1","Other Material A Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                          
                          # Y/N use additional matrix materials
                             , checkboxInput("othermatrixBUSERYN1", "Use Additional Matrix Materials?",FALSE)
                          # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXB TO THAT TYPE
                              , conditionalPanel(
                                   condition = "input.othermatrixBUSERYN1 == true"
                                 , selectizeInput("OtherMatrixBInput1", label = "Choose Other Matrix Material",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("othermatrixBEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                            # mass frac 
                                , numericInput("othermatrixBfrac1","Other Material B Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                         
                          # Y/N use additional matrix materials
                             , checkboxInput("othermatrixCUSERYN1", "Use Additional Matrix Materials?",FALSE)
                          # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXC TO THAT TYPE
                              , conditionalPanel(
                            condition = "input.othermatrixCUSERYN1 == true"
                                  , selectizeInput("OtherMatrixCInput1", label = "Choose Other Matrix Material",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                    , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("othermatrixCEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                            # mass frac 
                                 , numericInput("othermatrixCfrac1","Other Material C Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                          )))
                          
                          # Y/N use inserts
                          # Y/N use additional matrix materials
                          , checkboxInput("insertsAUSERYN1", "Use Inserts?",FALSE)
                          , conditionalPanel(
                            condition = "input.insertsAUSERYN1 == true"
                            , selectizeInput("InsertsAInput1", label = "Choose Insert Type",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                             options = list(placeholder = 'Choose Insert     '))
                            , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("insertsAEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                            # mass frac 
                            , numericInput("insertsAfrac1","Insert Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                          
                          # Y/N use additional matrix materials
                          , checkboxInput("insertsBUSERYN1", "Use Additional Insert?",FALSE)
                          # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXC TO THAT TYPE
                          , conditionalPanel(
                            condition = "input.insertsBUSERYN1 == true"
                            , selectizeInput("InsertsBInput1", label = "Choose Additional Insert Type",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                             options = list(placeholder = 'Choose Insert     '))
                            , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("insertsBEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                            # mass frac 
                            , numericInput("insertsBfrac1","Additional Insert Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                          ))
                            # choose specific & display energy
                            # make them know the mass fraction
                          # ERROR IF MASS FRACS + FIBER MASS FRAC =/= 1
                    
                    )    
                    #TechSet2
                    , column(6
                             , h2("Technology Set 2")
                             , h5(textOutput("partname2b"), textOutput("partweight2b"))
                             , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname2b")))
                             , fluidRow(column(4, h5("Fiber Mass Fraction:")), column(2, "OUTPUT"))
                             
                             #Matrix Stuff
                             #Matrix Stuff
                             #Choose Primary Matrix
                             , selectizeInput("PriMatrixInput2", label = "Choose Primary Matrix Material",
                                              choices = NULL,  selected = "",  multiple = FALSE,
                                              options = list(placeholder = 'Choose Matrix  '))
                             , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("primatrixEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                             # mass frac 
                             , numericInput("primatrixfrac2","Primary Matrix Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                             
                             # Y/N use additional matrix materials
                             , checkboxInput("othermatrixAUSERYN2", "Use Additional Matrix Materials?",FALSE)
                             # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXA TO THAT TYPE
                                 , conditionalPanel(
                                     condition = "input.othermatrixAUSERYN2"
                                    , selectizeInput("OtherMatrixAInput2", label = "Choose Other Matrix Material",
                                                choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                    , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("othermatrixAEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                               # mass frac 
                                   , numericInput("othermatrixAfrac2","Other Material A Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                               
                               # Y/N use additional matrix materials
                                    , checkboxInput("othermatrixBUSERYN2", "Use Additional Matrix Materials?",FALSE)
                               # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXB TO THAT TYPE
                                    , conditionalPanel(
                                         condition = "input.othermatrixBUSERYN2 == true"
                                         , selectizeInput("OtherMatrixBInput2", label = "Choose Other Matrix Material",
                                                  choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                        , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("othermatrixBEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                                 # mass frac 
                                        , numericInput("othermatrixBfrac2","Other Material B Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                                 
                                 # Y/N use additional matrix materials
                                       , checkboxInput("othermatrixCUSERYN2", "Use Additional Matrix Materials?",FALSE)
                                 # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXC TO THAT TYPE
                                          , conditionalPanel(
                                               condition = "input.othermatrixCUSERYN2 == true"
                                               , selectizeInput("OtherMatrixCInput2", label = "Choose Other Matrix Material",
                                                    choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                                , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("othermatrixCEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                                               , numericInput("othermatrixCfrac2","Other Material C Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                                 )))
                             
                             # Y/N use inserts
                             , checkboxInput("insertsAUSERYN2", "Use Inserts?",FALSE)
                             , conditionalPanel(
                                        condition = "input.insertsAUSERYN2 == true"
                                      , selectizeInput("InsertsAInput2", label = "Choose Insert Type",
                                                choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                                options = list(placeholder = 'Choose Insert     '))
                                      , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("insertsAEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                                      , numericInput("insertsAfrac2","Insert Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                               
                                      , checkboxInput("insertsBUSERYN2", "Use Additional Insert?",FALSE)
                                      , conditionalPanel(
                                          condition = "input.insertsBUSERYN2 == true"
                                            , selectizeInput("InsertsBInput2", label = "Choose Additional Insert Type",
                                                  choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                                  options = list(placeholder = 'Choose Insert     '))
                                           , fluidRow(column(4, h5("Embodied Energy:")), column(1,textOutput("insertsBEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                                 # mass frac 
                                         , numericInput("insertsBfrac2","Additional Insert Mass Fraction", 0.0,  min = 0.0, max = 1.0,0.1)
                               ))
                             # ERROR IF MASS FRACS + FIBER MASS FRAC =/= 1
                             
                             
                             
                            ))
                  
                  
                  #END TAB
                    ),
                  
                        # MoldingTab ----  
                        tabPanel("Molding", h1("Molding, Curing & Finishing"),
                                 p("Enter information on modeled part: curing technology, amount of finishing, 
                                   finishing scrap rate and confirm the molding technology and molding yield")
                                 , fluidRow(
                                   #TechSet1
                                   column(6
                                   , h2("Technology Set 1")
                                   , h5(textOutput("partname1c"), textOutput("partweight1c"))
                                   , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname1c")))
                                   
                                   #Molding Stuff
                                   #molding yield
                                   , fluidRow(column(4, h5("Default Molding Yield: ")), column(1, textOutput("moldyieldNum1")), column(1,checkboxInput("moldyieldUSERYN1", "Use Default?",TRUE)))
                                   , conditionalPanel(
                                     condition = "input.moldyieldUSERYN1 == false",
                                     numericInput("moldyieldUSERNum1","User defined molding yield", 0.0,  min = 0.0, max = 1.0,0.1))
                                   , numericInput("moldyieldrecycle1",p("Molding Recycle Rate", span( h6("(% scrap that is recycled)"))), 0.0,  min = 0.0, max = 1.0,0.1)
                                  
                                   #curing tech & energy
                                   , selectizeInput("cureInput1", label = "Curing Technology Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Curing Technology    '))
                                   , fluidRow(column (4,h5("Embodied Energy:")),column(1,textOutput("cureEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                                   
                                   #finishing level & scrap
                                   , br()
                                   , selectizeInput("finishInput1", label = "Finishing Level Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Finishing Level    '))
                                   , fluidRow(column (4,h5("Embodied Energy:")),column(1,textOutput("finishEnergyNum1"), align = "right"), column(1, "MJ/kg"))
                                   , numericInput("finishscrap1","Finish Scrap Rate", 0.0,  min = 0.0, max = 1.0,0.1)
                                   , numericInput("finishscraprecycle1",p("Finish Recycle Rate", span( h6("(% scrap that is recycled)"))), 0.0,  min = 0.0, max = 1.0,0.1)
                                   )
                                 #TechSet2
                                 , column(6
                                   , h2("Technology Set 2")
                                   , h5(textOutput("partname2c"), textOutput("partweight2c"))
                                   , fluidRow(column(3, h5("Molding Techology: ")), column(3, textOutput("moldname2c")))
                                          
                                   #Molding Stuff
                                   #molding yield
                                   , fluidRow(
                                     column(4, h5("Default Molding Yield: ")), column(1, textOutput("moldyieldNum2")), column(1,checkboxInput("moldyieldUSERYN2", "Use Default?",TRUE)))
                                   , conditionalPanel(
                                     condition = "input.moldyieldUSERYN2 == false",
                                     numericInput("moldyieldUSERNum2","User defined molding yield", 0.0,  min = 0.0, max = 1.0,0.1))
                                   , numericInput("moldyieldrecycle2",p("Finish Recycle Rate", span( h6("(% scrap that is recycled)"))), 0.0,  min = 0.0, max = 1.0,0.1)
                                  
                                    #curing tech & energy
                                   , selectizeInput("cureInput2", label = "Curing Technology Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Curing Technology    '))
                                   , fluidRow(column (4,h5("Embodied Energy:")),column(1,textOutput("cureEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                                   
                                   #finishing level & scrap
                                   , br()
                                   , selectizeInput("finishInput2", label = "Finishing Level Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Finishing Level    '))
                                   , fluidRow(column (4,h5("Embodied Energy:")),column(1,textOutput("finishEnergyNum2"), align = "right"), column(1, "MJ/kg"))
                                   , numericInput("finishscrap2","Finish Scrap Rate", 0.0,  min = 0.0, max = 1.0,0.1)
                                   , numericInput("finishscraprecycle2",p("Finish Recycle Rate", span( h6("(% scrap that is recycled)"))), 0.0,  min = 0.0, max = 1.0,0.1)
                                   )
                                 #End fluid row 
                                 )
                                 #EndTab
                                 ),
                        # SummaryTab ----
                        tabPanel("Summary", h1("Summary"),
                                 p("This is where users will be able to review all of the previous choices")),
                        # ResultsTab ---- 
                        tabPanel("Results", h1("Results"),
                                 p("This is where users will be able to graphically compare the two technology pathways"))
                        
                        # End ----
                        ))
