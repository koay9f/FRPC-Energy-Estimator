
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
                  
                  # Sidebar with a slider input for number of bins
                  sidebarLayout(
                    sidebarPanel( width = 0),
                    
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Guide",h1("Tool Guide"),img(src = "CFRPScope.png", height = 300),p(("This tool is being developed by ORNL to provide CFRP researchers and manufacturers the ability to quickly estimate the embodied energy use of their CFRP manufactuing process and compare it to other processes"))),
                        tabPanel("Initial Inputs", h1("Initial"), p("This is where users will input information like part name, part weight, and molding technology to be used")),
                        tabPanel("Fiber", h1("Fiber Manufacturing"), p("This is where users will choose fiber type, tow and intermediates and set fiber fraction and layup scrap rate")),
                        tabPanel("Matrix", h1("Matrix Materials Manufacturing"), 
                                 p("This is where users will choose the primary matrix material and other materials (additional matrix, additives, fillers and inserts) and the mass fraction of each")),
                        
                        
                        tabPanel("Molding", h1("Molding, Curing & Finishing"),
                                 p("This is where users will choose curing technology, amount of finishing, finishing scrap rate and confirm the molding technology and molding yield"),
                                 selectizeInput("moldingInput", label = (h3("Molding Technology Options")),
                                                choices = NULL,
                                                selected = "",
                                                multiple = FALSE   ),
                                 h3("Embodied energy:", textOutput("EnergyNum"),"MJ/kg")
                        ),
                        tabPanel("Summary", h1("Summary"),
                                 p("This is where users will be able to review all of the previous choices")),
                        tabPanel("Results", h1("Results"),
                                 p("This is where users will be able to graphically compare the two technology pathways"))
                      )))))
