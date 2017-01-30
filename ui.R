
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)

shinyUI(fluidPage(theme = "bootstrap.css",
                  
                  # Application title
                  titlePanel("CFRP Energy Use Estimation Tool"),
                  
                  sidebarLayout(
                    sidebarPanel( width = 0),
                    
                    
                    mainPanel(
                      tabsetPanel(  
                      # GuideTab ----
                        tabPanel("Guide",h1("Tool Guide"),img(src = "CFRPScope.png", height = 300),p(("This tool is 
                            being developed by ORNL to provide CFRP researchers and manufacturers the ability 
                            to quickly estimate the embodied energy use of their CFRP manufactuing process 
                            and compare it to other processes"))),
                      
                      # InitialTab ----   
                      tabPanel("Initial", h1("Initial Inputs"), p("Enter information on modeled part: 
                                 part name, part weight, and molding technology to be used"),
                        fluidRow(
                          column(6,
                               h3("Technology Set 1"),
                               textInput("name1", "Part Name","Part 1"),
                               numericInput("finalweight1","Final Part Weight (kg)", 1,  min = 0,NA,NA)
                               
                           ),
                           column(6,
                               h3("Technology Set 2"),
                               textInput("name2", "Part Name","Part 2"),
                               numericInput("finalweight2","Final Part Weight (kg)", 1, min = 0,NA,NA)
                           )
                        ),
                        fluidRow(h3("Molding Technology Options")),
                        fluidRow(
                          column(6,
                        selectizeInput("moldingInput1", label = "",
                                       choices = NULL,
                                       selected = "",
                                       multiple = FALSE,
                                       options = list(placeholder = 'Choose Molding Technology')),
                        h3("Embodied energy:"),
                        fluidRow(column (3,wellPanel(h4(textOutput("EnergyNum1")))), column(2, offset = 1,h4("MJ/kg")))
                        ),
                        column(6,
                               selectizeInput("moldingInput2", label = "",
                                              choices = NULL,
                                              selected = "",
                                              multiple = FALSE,
                                              options = list(placeholder = 'Choose Molding Technology')),
                               h3("Embodied energy:"),
                               fluidRow(column (3,wellPanel(h4(textOutput("EnergyNum2")))), column(2, offset = 1,h4("MJ/kg")))
                        )
                       
                        )),
                      
                      # FiberTab ----
                        tabPanel("Fiber", h1("Fiber Manufacturing & Layup") 
                       , p("Enter information on modeled part: fiber type, 
                          tow and intermediates, and fiber fraction and layup scrap rate")
                        
                      #Display Part Name & Weight & Molding Process
                      , fluidRow( 
                      column(6,
                            h3("Technology Set 1")
                       ,    h4(textOutput("partname1"), textOutput("partweight1"))
                       ,    h5(div("Molding Techology: ", textOutput("moldname1")))

                      #Display fiber fraction associated with molding process
                      ,    h5(div("Default Fiber Fraction: ",textOutput("moldfracNum1")))
                       ),
                      #Place to enter Mass fraction?
                      
                      #select tow size (don't really need fiber type right now... ask sujit)
                      #select intermediate
                      #display default scrap
                      #enter user scrap
                      #enter recycle
                      column(6,
                            h3("Technology Set 2")
                       ,    h4(textOutput("partname2"), textOutput("partweight2"))
                       ,    h5(div("Molding Techology: ", textOutput("moldname2")))

                             #Display fiber fraction associated with molding process
                       ,    h5(div("Default Fiber Fraction: ",textOutput("moldfracNum2")))
                      )                       
                       
                      )),
                      
                      
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
                      )))))
