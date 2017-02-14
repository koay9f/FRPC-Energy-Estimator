
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)

shinyUI(navbarPage("CFRP Energy Use Estimation Tool", position = "static-top", fluid = FALSE, theme = "bootstrap.css",  
                   
                  
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
                                  , wellPanel(
                                    textInput("name1", "Part Name","Part 1")
                                  , numericInput("finalweight1","Final Part Weight (kg)", 1,  min = 0,NA,NA)
                                  , selectizeInput("moldingInput1", label = "Molding Technology Options",
                                                   choices = NULL, selected = "", multiple = FALSE,
                                                   options = list(placeholder = 'Choose Molding Technology    '))
                                  )
                                  , fluidRow(column (8,h5("Embodied Energy:")),column(4,textOutput("EnergyNum1")))
                                  , wellPanel( style = "background-color: #FFCD00;"
                                  , checkboxInput("insertsAUSERYN1", "Does the part use inserts or a core?",FALSE)
                                  , conditionalPanel(
                                    condition = "input.insertsAUSERYN1 == true"
                                    , p("The final mass of the part should include the mass of the inserts or cores.")
                                   #THIS SHOULD BE MASS OF INSERT AND THEN NEED A CALC TO DETERMINE THE TRUE MASS FRACS
                                    , numericInput("insertsAfrac1","Insert/Core Mass", 0.0,  min = 0.0, NA, NA)
                                  
                                   , checkboxInput("insertsBUSERYN1", "Use Additional Insert/Core Material?",FALSE)
                                  
                                   #THIS SHOULD BE MASS OF INSERT AND THEN NEED A CALC TO DETERMINE THE TRUE MASS FRACS
                                   , conditionalPanel(
                                     condition = "input.insertsBUSERYN1 == true"
                                   , numericInput("insertsBfrac1","Additional Insert/Core Mass", 0.0,  min = 0.0, NA, NA))
                                  ))
                                  )
                                   
                             #TechSet2
                                , column(6
                                  , h2("Technology Set 2")
                                  , wellPanel(
                                   textInput("name2", "Part Name","Part 2")
                                  , numericInput("finalweight2","Final Part Weight (kg)", 1, min = 0,NA,NA)
                                  , selectizeInput("moldingInput2", label = "Molding Technology Options",
                                                   choices = NULL, selected = "", multiple = FALSE,
                                                   options = list(placeholder = 'Choose Molding Technology    '))
                                  )
                                  , fluidRow(column (8,h5("Embodied Energy:")),column(4,textOutput("EnergyNum2")))
                                  
                                  , wellPanel( style = "background-color: #FFCD00;"
                                  , checkboxInput("insertsAUSERYN2", "Does the part use inserts or a core?",FALSE)
                                  , conditionalPanel(
                                    condition = "input.insertsAUSERYN2 == true"
                                    , p("The final mass of the part should include the mass of the inserts or cores.")
                                    #THIS SHOULD BE MASS OF INSERT AND THEN NEED A CALC TO DETERMINE THE TRUE MASS FRACS
                                    , numericInput("insertsAfrac2","Insert/Core Mass", 0.0,  min = 0.0, NA, NA)
                                    
                                    , checkboxInput("insertsBUSERYN2", "Use Additional Insert/Core Material?",FALSE)
                                    
                                    #THIS SHOULD BE MASS OF INSERT AND THEN NEED A CALC TO DETERMINE THE TRUE MASS FRACS
                                    , conditionalPanel(
                                      condition = "input.insertsBUSERYN2 == true"
                                      , numericInput("insertsBfrac2","Additional Insert/Core Mass", 0.0,  min = 0.0, NA, NA))
                                  )
                                  
                                  
                                   ))
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
                                          , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname1"), align = "right"))
                                          , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight1"), align = "right"))
                                          , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort1"), align = "right"))
                                          , fluidRow(column(8, h5("Default Mass Fiber Fraction: ")), column(4, textOutput("moldfracNum1"), align = "right"))
                                          , wellPanel( 
                                          numericInput("moldfracUSERNum1","Fiber Mass Fraction",  min = 0.0, max = 100.0, value = 0.0,5.0)
                                          , selectizeInput("fiberInput1", label = "Fiber Type",
                                                  choices = NULL, selected = "", multiple = FALSE,
                                                  options = list(placeholder = 'Choose Fiber Type/Tow     '))
                                          )
                                          , column (8,h5("Embodied Energy:")),column (4,textOutput("fiberEnergyNum1"), align = "right")   
                                     )
                                   #TechSet2
                                   , column(6,
                                            h2("Technology Set 2")
                                            , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname2"), align = "right"))
                                            , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight2"), align = "right"))                                            
                                            , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort2"), align = "right"))
                                            , fluidRow(column(8, h5("Default Mass Fiber Fraction: ")), column(4, textOutput("moldfracNum2"), align = "right"))
                                            , wellPanel(
                                            numericInput("moldfracUSERNum2","Fiber Mass Fraction", min = 0.0, max = 100.0, value = 0.0,5.0)
                                            , selectizeInput("fiberInput2", label = "Fiber Type",
                                                      choices = NULL, selected = "", multiple = FALSE,
                                                      options = list(placeholder = 'Choose Fiber Type/Tow     '))
                                            )
                                            , column (8,h5("Embodied Energy:")),column (4,textOutput("fiberEnergyNum2"), align = "right")
                                             )
                               )
                                 #EndFiber   
                        ),
                        # MatrixTab ----
                        tabPanel("Matrix", h1("Matrix Materials Manufacturing"), 
                                 p("Enter information on modeled part: primary matrix material and other materials 
                                   (additional matrix, additives, fillers and inserts) and the mass fraction of each")
                  , fluidRow( 
                           #Matrix Techset1 ----
                    column(6
                           , h2("Technology Set 1")
                           , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname1b"), align = "right"))
                           , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight1b"), align = "right"))
                           , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort1b"), align = "right"))
                           , fluidRow(column(8, h5("Fiber Mass Fraction:")), column(4, textOutput("fiberfrac1b"), align = "right"))
                           #Matrix Stuff
                              #Choose Primary Matrix
                           , wellPanel( 
                           selectizeInput("PriMatrixInput1", label = "Choose Primary Matrix Material",
                                           choices = NULL,  selected = "",  multiple = FALSE,
                                           options = list(placeholder = 'Choose Matrix  '))
                           
                           # mass frac 
                           , numericInput("primatrixfrac1","Primary Matrix Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                           )
                           , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("primatrixEnergyNum1"), align = "right"))
                             # Additional Material A 
                          , wellPanel( style = "background-color: #FFCD00;"
                          , checkboxInput("othermatrixAUSERYN1", "Use Additional Matrix Materials?",FALSE)
                          # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXA TO THAT TYPE
                          , conditionalPanel(
                             condition = "input.othermatrixAUSERYN1 == true"
                             , selectizeInput("OtherMatrixAInput1", label = "Choose Other Matrix Material",
                                           choices = NULL,  selected = "Not Used",  multiple = FALSE)
                              , numericInput("othermatrixAfrac1","Other Material A Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                          ))
                          
                          , conditionalPanel(
                            condition = "input.othermatrixAUSERYN1 == true"
                          , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("othermatrixAEnergyNum1"), align = "right")))
                          
                              # Additional Material B 
                             , wellPanel(style = "background-color: #FFCD00;"
                          , checkboxInput("othermatrixBUSERYN1", "Use Additional Matrix Materials?",FALSE)
                          # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXB TO THAT TYPE
                              , conditionalPanel(
                                   condition = "input.othermatrixBUSERYN1 == true"
                                 , selectizeInput("OtherMatrixBInput1", label = "Choose Other Matrix Material",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE)
                            # mass frac 
                                , numericInput("othermatrixBfrac1","Other Material B Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                              ))
                          
                          , conditionalPanel(
                            condition = "input.othermatrixBUSERYN1 == true"
                            , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("othermatrixBEnergyNum1"), align = "right")))
                            
                              # Additional Material C 
                             ,  wellPanel(style = "background-color: #FFCD00;"
                          , checkboxInput("othermatrixCUSERYN1", "Use Additional Matrix Materials?",FALSE)
                          # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXC TO THAT TYPE
                              , conditionalPanel(
                            condition = "input.othermatrixCUSERYN1 == true"
                                  , selectizeInput("OtherMatrixCInput1", label = "Choose Other Matrix Material",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE)
                            # mass frac 
                                 , numericInput("othermatrixCfrac1","Other Material C Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                              ))
                          
                          , conditionalPanel(
                            condition = "input.othermatrixCUSERYN1 == true"
                            , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("othermatrixCEnergyNum1"), align = "right")))
                                # Insert 
                          ,  conditionalPanel(
                            condition = "input.insertsAUSERYN1 == true"
                            , wellPanel(style = "background-color: #FFCD00;"
                              , selectizeInput("InsertsAInput1", label = "Choose Insert Type",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                             options = list(placeholder = 'Choose Insert     '))    )
                            # Energy 
                            , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("insertsAEnergyNum1"), align = "right"))
                            
                          , conditionalPanel( 
                            condition = "input.insertsBUSERYN1 == true"
                            , wellPanel(style = "background-color: #FFCD00;"
                              , selectizeInput("InsertsBInput1", label = "Choose Additional Insert Type",
                                             choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                             options = list(placeholder = 'Choose Insert     ')))
                            # Energy 
                            , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("insertsBEnergyNum1"), align = "right"))
                            
                          ))
                              # End TS1
                    
                    )    
                           # Matrix TechSet2 ----
                    , column(6
                             , h2("Technology Set 2")
                             , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname2b"), align = "right"))
                             , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight2b"), align = "right"))
                             , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort2b"), align = "right"))
                             , fluidRow(column(8, h5("Fiber Mass Fraction:")), column(4, textOutput("fiberfrac2b"), align = "right"))
                             
                             #Matrix Stuff
                             #Choose Primary Matrix 
                             ,  wellPanel(selectizeInput("PriMatrixInput2", label = "Choose Primary Matrix Material",
                                              choices = NULL,  selected = "",  multiple = FALSE,
                                              options = list(placeholder = 'Choose Matrix  '))
                             # mass frac 
                             , numericInput("primatrixfrac2","Primary Matrix Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0))
                             , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("primatrixEnergyNum2"), align = "right"))
                             
                             # Additional Material A  
                             ,  wellPanel(style = "background-color: #FFCD00;"
                               , checkboxInput("othermatrixAUSERYN2", "Use Additional Matrix Materials?",FALSE)
                             # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXA TO THAT TYPE
                                 , conditionalPanel(
                                     condition = "input.othermatrixAUSERYN2"
                                    , selectizeInput("OtherMatrixAInput2", label = "Choose Other Matrix Material",
                                                choices = NULL,  selected = "Not Used",  multiple = FALSE)
                               # mass frac 
                                   , numericInput("othermatrixAfrac2","Other Material A Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)))
                               , conditionalPanel(condition = "input.othermatrixAUSERYN2"
                                 ,fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("othermatrixAEnergyNum2"), align = "right")))
                               
                               # Additional Mateiral B  
                                    , wellPanel(style = "background-color: #FFCD00;"
                                      , checkboxInput("othermatrixBUSERYN2", "Use Additional Matrix Materials?",FALSE)
                               # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXB TO THAT TYPE
                                    , conditionalPanel(
                                         condition = "input.othermatrixBUSERYN2 == true"
                                         , selectizeInput("OtherMatrixBInput2", label = "Choose Other Matrix Material",
                                                  choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                 # mass frac 
                                        , numericInput("othermatrixBfrac2","Other Material B Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)))
                             , conditionalPanel(
                               condition = "input.othermatrixBUSERYN2 == true"    
                             , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("othermatrixBEnergyNum2"), align = "right")))
                                 
                                 # Additional Material C 
                                       , wellPanel(style = "background-color: #FFCD00;"
                                         , checkboxInput("othermatrixCUSERYN2", "Use Additional Matrix Materials?",FALSE)
                                 # WOULD LIKE TO ADD BOX TO choose additive/fiber/matrix AND LIMIT OTHERMATRIXC TO THAT TYPE
                                          , conditionalPanel(
                                               condition = "input.othermatrixCUSERYN2 == true"
                                               , selectizeInput("OtherMatrixCInput2", label = "Choose Other Matrix Material",
                                                    choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                               , numericInput("othermatrixCfrac2","Other Material C Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)))
                             , conditionalPanel(
                               condition = "input.othermatrixCUSERYN2 == true"
                               , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("othermatrixCEnergyNum2"), align = "right")))
                                               
                                 
                             
                             # Inserts 
                             , conditionalPanel(
                                        condition = "input.insertsAUSERYN2 == true"
                                      , wellPanel(style = "background-color: #FFCD00;"
                                        , selectizeInput("InsertsAInput2", label = "Choose Insert Type",
                                                choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                                options = list(placeholder = 'Choose Insert     ')))
                                      , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("insertsAEnergyNum2"), align = "right"))
                                      
                                      , conditionalPanel(
                                          condition = "input.insertsBUSERYN2 == true"
                                            , wellPanel(style = "background-color: #FFCD00;"
                                              , selectizeInput("InsertsBInput2", label = "Choose Additional Insert Type",
                                                  choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                                  options = list(placeholder = 'Choose Insert     ')))
                                 # mass frac 
                                 , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("insertsBEnergyNum2"), align = "right"))
                                 
                               ))

                            # End TS2  
                             
                            ))
                  
                  
                            #END TAB 
                    ),
                  
                        # IntTab ----
                      tabPanel("Intermediate", h1("Fiber Intermediate Manufacturing & Layup") 
                           , p("Enter information on modeled part: intermediate type layup scrap rate")
                           
                           #Display Part Name & Weight & Molding Process & Fiber Fraction
                           , fluidRow( 
                             #Techset1
                             column(6
                                    , h2("Technology Set 1")
                                    , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname1a"), align = "right"))
                                    , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight1a"), align = "right"))
                                    , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort1a"), align = "right"))
                                    , wellPanel(selectizeInput("intInput1", label = "Choose Fiber Intermediate",
                                                     choices = NULL,  selected = "",  multiple = FALSE,
                                                     options = list(placeholder = 'Choose Fiber Intermediate     ')))
                                    , fluidRow(column(8, h5("Embodied Energy:")), column(4,textOutput("intEnergyNum1"), align = "right"))
                                    , fluidRow(column(8, h5("Default Layup Scrap Rate: ")), column(4, textOutput("intscrapNum1"), align = "right"))
                                    , wellPanel(numericInput("intscrapUSERNum1","Layup Scrap Rate", 0.0,  min = 0.0, max = 100.0,5.0)
                                                ,numericInput("intscraprecycle1","Layup Recycle Rate (% scrap that is recycled)", 
                                                              0.0,  min = 0.0, max = 100.0,5.0))
                             )    
                             #TechSet2
                             , column(6
                                      , h2("Technology Set 2")
                                      , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname2a"), align = "right"))
                                      , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight2a"), align = "right"))
                                      , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort2a"), align = "right"))
                                      , wellPanel(selectizeInput("intInput2", label = "Choose Fiber Intermediate",
                                                       choices = NULL,  selected = "",  multiple = FALSE,
                                                       options = list(placeholder = 'Choose Fiber Intermediate     ')))
                                      , fluidRow(column(8, h5("Embodied Energy:")), column (4,textOutput("intEnergyNum2"), align = "right"))
                                      , fluidRow(column(8, h5("Default Layup Scrap Rate: ")), column(4, textOutput("intscrapNum2"), align = "right"))
                                      , wellPanel(numericInput("intscrapUSERNum2","Layup Scrap Rate", 0.0,  min = 0.0, max = 100.0, 5.0)
                                                  ,numericInput("intscraprecycle2","Layup Recycle Rate (% scrap that is recycled)",
                                                                0.0,  min = 0.0, max = 100.0,5.0))
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
                                   , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname1c"), align = "right"))
                                   , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight1c"), align = "right"))
                                   , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort1c"), align = "right"))
                                   
                                   
                                   #Molding Stuff
                                   #molding yield
                                   , fluidRow(column(8, h5("Default Molding Yield: ")), column(4, textOutput("moldyieldNum1"), align = "right"))
                                   , wellPanel(numericInput("moldyieldUSERNum1","Molding Yield", 0.0,  min = 0.0, max = 100.0,5.0)
                                               , numericInput("moldyieldrecycle1","Molding Recycle Rate (% scrap that is recycled)"
                                                  , 0.0,  min = 0.0, max = 100.0,5.0))
                                  , br()
                                   #curing tech & energy
                                   , wellPanel(selectizeInput("cureInput1", label = "Curing Technology Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Curing Technology    ')))
                                   , fluidRow(column (8,h5("Embodied Energy:")),column(4,textOutput("cureEnergyNum1"), align = "right"))
                                   
                                   #finishing level & scrap
                                   , br()
                                   , wellPanel(style = "background-color: #FFCD00;"
                                     , selectizeInput("finishInput1", label = "Finishing Level Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Finishing Level    ')))
                                   , fluidRow(column (8,h5("Embodied Energy:")),column(4,textOutput("finishEnergyNum1"), align = "right"))
                                   , wellPanel(style = "background-color: #FFCD00;"
                                     , numericInput("finishscrap1","Finish Scrap Rate", 0.0,  min = 0.0, max = 100.0,5)
                                               , numericInput("finishscraprecycle1", "Finish Recycle Rate (% scrap that is recycled)", 
                                                              0.0,  min = 0.0, max = 100.0,5.0))
                                   )
                                 #TechSet2
                                 , column(6
                                   , h2("Technology Set 2")
                                   , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname2c"), align = "right"))
                                   , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight2c"), align = "right"))
                                   , fluidRow(column(8, h5("Molding Techology: ")), column(4, textOutput("moldshort2c"), align = "right"))
                                          
                                   #Molding Stuff
                                   #molding yield
                                   , fluidRow(
                                     column(8, h5("Default Molding Yield: ")), column(4, textOutput("moldyieldNum2"), align = "right"))
                                   , wellPanel(numericInput("moldyieldUSERNum2","Molding Yield", 0.0,  min = 0.0, max = 100.0,5)
                                        , numericInput("moldyieldrecycle2", "Molding Recycle Rate (% scrap that is recycled)", 
                                                  0.0,  min = 0.0, max = 100.0,5.0))
                                  , br()
                                    #curing tech & energy
                                   , wellPanel(selectizeInput("cureInput2", label = "Curing Technology Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Curing Technology    ')))
                                   , fluidRow(column (8,h5("Embodied Energy:")),column(4,textOutput("cureEnergyNum2"), align = "right"))
                                   
                                   #finishing level & scrap
                                   , br()
                                   , wellPanel(style = "background-color: #FFCD00;"
                                     , selectizeInput("finishInput2", label = "Finishing Level Options",
                                                    choices = NULL, selected = "", multiple = FALSE,
                                                    options = list(placeholder = 'Choose Finishing Level    ')))
                                   , fluidRow(column (8,h5("Embodied Energy:")),column(4,textOutput("finishEnergyNum2"), align = "right"))
                                   , wellPanel(style = "background-color: #FFCD00;"
                                     , numericInput("finishscrap2","Finish Scrap Rate", 0.0,  min = 0.0, max = 100.0,5.0)
                                        , numericInput("finishscraprecycle2","Finish Recycle Rate (% scrap that is recycled)"
                                                       , 0.0,  min = 0.0, max = 100.0,5.0))
                                   )
                                 #End fluid row 
                                 )
                                 #EndTab
                                 ),
                  # # TEST TAB ----
                  #  tabPanel("TEST", h1("TEST"),
                  #           tableOutput("table1a"), tableOutput("table2a"), tableOutput("table1b"), tableOutput("table2b"),tableOutput("table3"), tableOutput("table4")
                  #         
                  #  ),
                        # SummaryTab ----
                        tabPanel("Summary", h1("Summary"),
                                 p("Summary of User Choices and Embodied Energy")
                                 #Display Part Name & Weight & Molding Process & Fiber Fraction
                                 , fluidRow(
                                   #TechSet1
                                   column(6,
                                          h2("Technology Set 1")
                                          , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname1d"), align = "right"))
                                          , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight1d"), align = "right"))
                                          , tableOutput("Table.mat.1")
                                          , tableOutput("Table.pro.1")
                                   )
                                   #TechSet2
                                   , column(6,
                                            h2("Technology Set 2")
                                            , fluidRow(column(4, h5("Part Name: ")), column(8, textOutput("partname2d"), align = "right"))
                                            , fluidRow(column(8, h5("Part Weight:")), column(4, textOutput("partweight2d"), align = "right"))
                                            , tableOutput("Table.mat.2")
                                            , tableOutput("Table.pro.2")
                                   ))
                                            
                                 #End Tab
                                 ),
                  
                        # ResultsTab ---- 
                        tabPanel("Results", h1("Results"),
                                 #p("This is where users will be able to graphically compare the two technology pathways"),
                                  plotlyOutput("plot1")
                        )
                  
                        
                        # End ----
                        ))
