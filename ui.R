
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(plotly)
library(DT)
library(shinyjs)


shinyUI(fluidPage(title = "FRPC Tool",
  theme = "bootstrap.css",
                  tags$head(
                        tags$style(HTML(" .shiny-output-error-validation {color: red; }"))
                    ,   tags$style(HTML(" .shiny-notification-message {color: #007833;background-color: #e7f1d8; border-color: #007833;}"))
                    ,   tags$style(HTML(" table tbody tr:last-child {font-weight:bold;}")) 
                    ,   tags$style(HTML(id = "diagram", "table tbody tr:last-child {font-weight:bold;}"))
                  ), 
useShinyjs(), #(shinyjs is used for the custom data check marks)
                  titlePanel(h1("FRPC Energy Use Estimation Tool", style = "color:#007833")),
                  navlistPanel( 
            
            # Guide Tab ----
            tabPanel("Guide",h1("Tool Guide")
                     , tags$img(src='ToolScope.svg', style = "height: 300; width: 100%; max-width: 778.3079951422452px;")
                     , br()
                     , hr()
                     , br()
                      , strong('This tool has been developed by ORNL to provide composite researchers and manufacturers the ability to quickly estimate 
                         the embodied energy use of their composite manufacturing process and compare it to other conventional processes.')  
                     , hr()    
                     , p(icon("table"),'   A table for comparing molding technologies is available on the "Molding Properties" Page.')  
                      , p(icon("internet-explorer"),'   Some features of this tool are not fully suppported in Internet Explorer.')
                     , p(icon("envelope-o"), '   If you wish to permanently add a material or process to our database or otherwise wish to comment on this tool, please contact 
                         the developers by', span(strong(" email:")), 'Kristina Armstrong (', span('armstrongko@ornl.gov', style = "text-decoration: underline"),  ') or Sujit Das (', span('dass@ornl.gov', style = "text-decoration: underline"),').')
                     , helpText(a("Or connect with us on GitHub", href = "https://github.com/koay9f/FRPC-Energy-Estimator", target= "_blank"))
                     , p(icon("save"),strong("Saving:"), "When you exit the application, all data is lost.", style = "color: red")
                      ,p(icon("download"),'To rerun in future sessions, download the "Input" files from the Download Page at the end of your session.')
                     , p(icon("upload"), 'In later sessions, you can upload this on the Upload Page, and the forms will autopopulate.')
                     , hr()
                     , p(icon("file-zip-o"),'   For more help or information,', span(strong( "download" )), 'the Tool Documentation. This includes includes energy data,
                         molding process properties, references and details of tool computations' ) 
                     , downloadButton('info', "Download Tool Documentation Zip File")
                     , p("If you are unable to download the background zip file, try", span(strong("reloading")), "the application.",  
                         span("Note: you will lose any information you have entered into the app.", style = "color:red"))
                   ),
            # Upload Tab ----
            tabPanel("Upload", h1("Upload Data")
                     , p ("Upload a previous run from this tool.  If CSV file was changed, errors may occur.")
                     , fileInput("Re_Custom", "Choose CSV File to Upload Custom Data",
                                 accept = c('csv', 'comma-separated-values','.csv')) 
                     , fileInput("re_input1", "Choose CSV File for Technology Set 1",
                                 accept = c('csv', 'comma-separated-values','.csv'))
                     , fileInput("re_input2", "Choose CSV File for Technology Set 2",
                                 accept = c('csv', 'comma-separated-values','.csv'))
               ),
            
            # Add data tab ----
            {tabPanel("Custom Data",h1("Custom Data")
                     # This tool can also incorporate custom data using the form below
                   # Which & Instructions ---- 
                     , selectizeInput("add_data_which", label = "What type of custom data would you like to add?",
                                      choices = c("Instructions", "Fiber Type", "Matrix", "Filler", "Additive", 
                                                  "Insert/Core Material", "Intermediate", "Molding Technology", "Curing Technology", "Finish")
                                      , selected = "Instructions", multiple = FALSE)
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Instructions"'
                       , tags$li('Multiple custom data points can be added for each process segment.')
                       , tags$li('If the same name is used for different custom energy values, the last added will be used by the tool. 
                                 This can also be used to temporarily override the energy values for data already in the provided datasets.')
                       , tags$li('Download the .csv file with all of the custom data from this session to save time in a future session.')
                       , p (icon('github'), "If you wish to directly edit the available data,"
                            , span(strong("download R studio ")), "and a copy of this app from GitHub. ")
                       , helpText(a("Click here to view this app on GitHub", href = "https://github.com/koay9f/FRPC-Energy-Estimator", target= "_blank"))
                       ,p(icon('envelope-open-o'),' Alternatively, contact Kristina Armstrong (', span('armstrongko@ornl.gov', style = "text-decoration: underline"),  
                            ') or Sujit Das (', span('dass@ornl.gov', style = "text-decoration: underline"),'). and 
                            we can work with you to add your technology to our tool and grow our database of composite manufacturing technologies.')
                         )
                     # Fiber ----
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Fiber Type"'
                       , fluidRow(column(5, h4("Custom Fiber Type")) )
                       ,  fluidRow(
                         column(4, textInput("fiber_add", "Name", "Custom Fiber Type"))
                         , column (4, numericInput("fiber_add_E", "Specific Embodied Energy (MJ/kg fiber)", 0, min = 0, NA, NA))
                         , column(3,actionButton("gofiber", strong("Add Fiber Data")), style = "padding:25px;", align = "right") 
                         , column(1,         hidden(
                                    p(id = "fibi", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                      
                         ))
                     # Insert/Core ----
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Insert/Core Material"'
                       , fluidRow(column(5,h4("Custom Insert or Core")))
                       , fluidRow(column(4, textInput("insert_add", "Name", "Custom Insert/Core"))
                                  , column (4, numericInput("insert_add_E", "Specific Embodied Energy (MJ/kg)", 0, min = 0, NA, NA))
                                  , column(3,actionButton("goinsert", strong("Add Insert/Core Data")), style = "padding:25px;")
                                  , column(1,         hidden(
                                    p(id = "insi", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                                  ))
                     # Matrix ---- 
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Matrix"'
                       , fluidRow(column(5, h4("Custom Matrix Polymer") ))
                       , fluidRow(column(4, textInput("matrix_add", "Name", "Custom Matrix"))
                                  , column (4, numericInput("matrix_add_E", "Specific Embodied Energy (MJ/kg)", 0, min = 0, NA, NA))
                                  , column(3,actionButton("gomatrix", strong("Add Matrix Data")), style = "padding:25px;")
                                  , column(1,         hidden(
                                    p(id = "mati", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                                  ))
                     # Filler ---- 
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Filler"'
                       , fluidRow(column(5, h4("Custom Matrix Filler") ))
                       , fluidRow(column(4, textInput("filler_add", "Name", "Custom Filler"))
                                  , column (4, numericInput("filler_add_E", "Specific Embodied Energy (MJ/kg)", 0, min = 0, NA, NA))
                                  , column(3,actionButton("gofiller", strong("Add Filler Data")), style = "padding:25px;")
                                  , column(1,         hidden(
                                    p(id = "fili", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                                  ))
                     # Additive ---- 
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Additive"'
                       , fluidRow(column(5, h4("Custom Matrix Additive") ))
                       , fluidRow(column(4, textInput("additive_add", "Name", "Custom Additive"))
                                  , column (4, numericInput("additive_add_E", "Specific Embodied Energy (MJ/kg)", 0, min = 0, NA, NA))
                                  , column(3,actionButton("goadditive", strong("Add Additive Data")), style = "padding:25px;")
                                  , column(1,         hidden(
                                    p(id = "addi", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                                  ))
                     # Intermediate ---- 
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Intermediate"'
                       , fluidRow(column(5,h4("Custom Intermediate")))
                       , fluidRow(column(3, textInput("int_add", "Name", "Custom Intermediate"))
                                  , column (3, numericInput("int_add_E", "Specific Embodied Energy (MJ/kg fiber)", 0, min = 0, NA, NA))
                                  , column (3,  checkboxInput("int_add_PP", "Does the intermediate material include matrix material (i.e. prepregs)?",FALSE))
                                  , column(2,actionButton("goint", strong("Add Intermediate Data")), style = "padding:25px;")
                                  , column(1,         hidden(
                                    p(id = "inti", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                       )
                       , fluidRow(column(12, p("Normally the options available for fiber intermediate are dependent on the user's choice for molding technology.  
                                                             If a custom intermediate has been added, this is overridden and all options for 
                                                             intermediates (including the custom process) will be available.")
                                                       , p("In this tool, the matrix mass of a prepreg is accounted for when calculating change in material mass as material
                                                           is scrapped.  The mass of the matrix is not included for when calculating the embodied energy of the intermediate.")))
                       )
                     # Molding ---- 
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Molding Technology"'
                       , h4("Custom Molding Process")
                       , br()
                       , p("Normally the options available for fiber intermediate and curing process are dependent on the user's choice for molding technology.  
                           If a custom molding process has been added and chosen, this is overridden and all options for these will be available.")
                       , fluidRow(
                         column(4, textInput("mold_add", "Name", "Custom Mold Tech"))
                         , column(4, checkboxInput("mold_add_EYN", "Is the specific embodied Energy of the molding process known?",FALSE), style = "padding:15px;")
                         , column(3,actionButton("gomold", strong("Add Molding Data")), style = "padding:25px;")
                         , column(1,         hidden(
                           p(id = "moldi", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                         )
                       , br()
                       , conditionalPanel( 
                         condition = "input.mold_add_EYN == true"
                         , numericInput("mold_add_E_Y", "Specific Embodied Energy (MJ/kg)", 0, min = 0, NA, NA)
                         )
                       , conditionalPanel( 
                         condition = "input.mold_add_EYN == false"
                         , fluidRow(
                           column(3, strong("Calculated Embodied Energy: "))
                           , column(2, textOutput("calcedmoldE"))                                 )
                         , br()
                         , h3("Process Information")
                         , p('If the specific energy of the molding process is unknown, it can be estimated using the rated power of the equipment
                             envolved in the molding process.  If multiple pieces of equipment are used, multiply the rated power of the equipment
                             by the fraction of the rated power (i.e., the power used) and enter the sum into the"Rated Equipment Power (kW)" input 
                             and leave the "Percent of Rated Power (%)" at 100.')
                         , numericInput("mold_add_E_m", "Mass of object molded (kg)", 1, min = 0, NA, NA)
                         , h4("Vacuum Motor")
                         , fluidRow(
                           column(4, numericInput("mold_add_E_P_M", "Rated Vacuum Motor Power (kW)", 0, min = 0, NA, NA))
                           , column(4, numericInput("mold_add_E_per_M", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(4, numericInput("mold_add_E_t_M", "Motor Running Time (min)", 0, min = 0, NA, NA)) )
                         , h4("Pump")
                         , fluidRow(
                           column(4, numericInput("mold_add_E_P_p", "Rated Pump Power (kW)", 0, min = 0, NA, NA))
                           , column(4, numericInput("mold_add_E_per_p", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(4, numericInput("mold_add_E_t_p", "Pump Running Time (min)", 0, min = 0, NA, NA)) )
                         , h4("Compressor")
                         , fluidRow(
                           column(4, numericInput("mold_add_E_P_c", "Rated Compressor Power (kW)", 0, min = 0, NA, NA))
                           , column(4, numericInput("mold_add_E_per_c", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(4, numericInput("mold_add_E_t_c", "Compressor Running Time (min)", 0, min = 0, NA, NA)) )
                         , h4("Process Heating")
                         , fluidRow(
                           column(2, numericInput("mold_add_E_P_h1", "Rated Heater Power", 0, min = 0, NA, NA))
                           , column(2, selectizeInput("mold_add_E_u_h1", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                           , column(3, numericInput("mold_add_E_per_h1", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(3, numericInput("mold_add_E_t_h1", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                           , column(2, checkboxInput("mold_add_row1", "Add another time segment?", FALSE))  )
                         , conditionalPanel( 
                           condition = "input.mold_add_row1 == true"
                           , fluidRow(
                               column(2, numericInput("mold_add_E_P_h2", "Rated Heater Power", 0, min = 0, NA, NA))
                             , column(2, selectizeInput("mold_add_E_u_h2", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                             , column(3, numericInput("mold_add_E_per_h2", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                             , column(3, numericInput("mold_add_E_t_h2", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                             , column(2, checkboxInput("mold_add_row2", "Add another time segment?", FALSE))   )
                           , conditionalPanel( 
                             condition = "input.mold_add_row2 == true"
                             , fluidRow(
                               column(2, numericInput("mold_add_E_P_h3", "Rated Heater Power", 0, min = 0, NA, NA))
                               , column(2, selectizeInput("mold_add_E_u_h3", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                               , column(3, numericInput("mold_add_E_per_h3", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                               , column(3, numericInput("mold_add_E_t_h3", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                             , column(2, checkboxInput("mold_add_row3", "Add another time segment?", FALSE))      )
                             , conditionalPanel( 
                               condition = "input.mold_add_row3 == true"
                               , fluidRow(
                                 column(2, numericInput("mold_add_E_P_h4", "Rated Heater Power", 0, min = 0, NA, NA))
                                 , column(2, selectizeInput("mold_add_E_u_h4", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                 , column(3, numericInput("mold_add_E_per_h4", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                 , column(3, numericInput("mold_add_E_t_h4", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                 , column(2, checkboxInput("mold_add_row4", "Add another time segment?", FALSE))   )
                               , conditionalPanel( 
                                 condition = "input.mold_add_row4 == true"
                                 , fluidRow(
                                   column(2, numericInput("mold_add_E_P_h5", "Rated Heater Power", 0, min = 0, NA, NA))
                                   , column(2, selectizeInput("mold_add_E_u_h5", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                   , column(3, numericInput("mold_add_E_per_h5", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                   , column(3, numericInput("mold_add_E_t_h5", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                   , column(2, checkboxInput("mold_add_row5", "Add another time segment?", FALSE))      )
                                 , conditionalPanel( 
                                   condition = "input.mold_add_row5 == true"
                                   , fluidRow(
                                     column(2, numericInput("mold_add_E_P_h6", "Rated Heater Power", 0, min = 0, NA, NA))
                                     , column(2, selectizeInput("mold_add_E_u_h6", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                     , column(3, numericInput("mold_add_E_per_h6", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                     , column(3, numericInput("mold_add_E_t_h6", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                     , column(2, checkboxInput("mold_add_row6", "Add another time segment?", FALSE))   )
                                   , conditionalPanel( 
                                     condition = "input.mold_add_row6 == true"
                                     , fluidRow(
                                       column(2, numericInput("mold_add_E_P_h7", "Rated Heater Power", 0, min = 0, NA, NA))
                                       , column(2, selectizeInput("mold_add_E_u_h7", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                       , column(3, numericInput("mold_add_E_per_h7", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                       , column(3, numericInput("mold_add_E_t_h7", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                       , column(2, checkboxInput("mold_add_row7", "Add another time segment?", FALSE))      )
                                     , conditionalPanel( 
                                       condition = "input.mold_add_row7 == true"
                                       , fluidRow(
                                         column(2, numericInput("mold_add_E_P_h8", "Rated Heater Power", 0, min = 0, NA, NA))
                                         , column(2, selectizeInput("mold_add_E_u_h8", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                         , column(3, numericInput("mold_add_E_per_h8", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                         , column(3, numericInput("mold_add_E_t_h8", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                         , column(2, checkboxInput("mold_add_row8", "Add another time segment?", FALSE))   )
                                       , conditionalPanel( 
                                         condition = "input.mold_add_row8 == true"
                                         , fluidRow(
                                           column(2, numericInput("mold_add_E_P_h9", "Rated Heater Power", 0, min = 0, NA, NA))
                                           , column(2, selectizeInput("mold_add_E_u_h9", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                           , column(3, numericInput("mold_add_E_per_h9", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                           , column(3, numericInput("mold_add_E_t_h9", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                           , column(2, checkboxInput("mold_add_row9", "Add another time segment?", FALSE))      )
                                         , conditionalPanel( 
                                           condition = "input.mold_add_row9 == true"
                                           , fluidRow(
                                             column(2, numericInput("mold_add_E_P_h0", "Rated Heater Power", 0, min = 0, NA, NA))
                                             , column(2, selectizeInput("mold_add_E_u_h0", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                             , column(3, numericInput("mold_add_E_per_h0", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                             , column(3, numericInput("mold_add_E_t_h0", "Heater Running Time (min)", 0, min = 0, NA, NA))       )   
                                         
                                         ))) #10 , 9 , 8
                                         )))  #7, 6, 5
                                         ))) # 4, 3, 2 - 1 does not have a conditional panel
                             
                         , h4("Other")
                         , fluidRow(
                           column(3, numericInput("mold_add_E_P_o", "Rated Equipment Power (kW)", 0, min = 0, NA, NA))
                           , column(3, selectizeInput("mold_add_E_u_o", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                           , column(3, numericInput("mold_add_E_per_o", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(3, numericInput("mold_add_E_t_o", "Equipment Running Time (min)", 0, min = 0, NA, NA)) )
                         ))
                     # Curing ---- 
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Curing Technology"'
                       , h4("Custom Curing Process")
                       , br()
                       , p("Normally the options available for curing process are dependent on the user's choice for molding technology.  
                                                             If a custom curing process has been added, this is overridden and all options for 
                           curing (including the custom process) will be available.")
                       , fluidRow(
                          column(4, textInput("cure_add", "Name", "Custom Cure Tech"))
                         , column(4, checkboxInput("cure_add_EYN", "Is the specific embodied energy of the curing process known?",FALSE), style = "padding:15px;")
                         , column(3,actionButton("gocure", strong("Add Curing Data")), style = "padding:25px;")
                         , column(1,         hidden(
                           p(id = "curei", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                         )
                       , br()
                       , conditionalPanel( 
                         condition = "input.cure_add_EYN == true"
                         , numericInput("cure_add_E_Y", "Specific Embodied Energy (MJ/kg)", 0, min = 0, NA, NA)
                         )
                       , conditionalPanel( 
                         condition = "input.cure_add_EYN == false"
                         , fluidRow(
                           column(3, strong("Calculated Embodied Energy: "))
                         , column(2, textOutput("calcedcureE")))
                         , br()
                         , h3("Process Information")
                         , p('If the specific energy of the curing process is unknown, it can be estimated using the rated power of the equipment
                                      envolved in the curing process.  If multiple pieces of 
                                      equipment are used, multiply the rated power of the equipment
                                      by the fraction of the rated power (i.e., the power used) and 
                                      enter the sum into the"Rated Equipment Power (kW)" input and leave the "Percent
                                      of Rated Power (%)" at 100.')
                         , numericInput("cure_add_E_m", "Mass of object cured (kg)", 1, min = 0, NA, NA)
                         , h4("Vacuum Motor")
                         , fluidRow(
                                column(4, numericInput("cure_add_E_P_M", "Rated Vacuum Motor Power (kW)", 0, min = 0, NA, NA))
                              , column(4, numericInput("cure_add_E_per_M", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                              , column(4, numericInput("cure_add_E_t_M", "Motor Running Time (min)", 0, min = 0, NA, NA)) )
                         , h4("Pump")
                         , fluidRow(
                               column(4, numericInput("cure_add_E_P_p", "Rated Pump Power (kW)", 0, min = 0, NA, NA))
                             , column(4, numericInput("cure_add_E_per_p", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                             , column(4, numericInput("cure_add_E_t_p", "Pump Running Time (min)", 0, min = 0, NA, NA)) )
                         , h4("Compressor")
                         , fluidRow(
                           column(4, numericInput("cure_add_E_P_c", "Rated Compressor Power (kW)", 0, min = 0, NA, NA))
                           , column(4, numericInput("cure_add_E_per_c", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(4, numericInput("cure_add_E_t_c", "Compressor Running Time (min)", 0, min = 0, NA, NA)) )
                         , h4("Process Heating")
                         , fluidRow(
                           column(2, numericInput("cure_add_E_P_h1", "Rated Heater Power", 0, min = 0, NA, NA))
                           , column(2, selectizeInput("cure_add_E_u_h1", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                           , column(3, numericInput("cure_add_E_per_h1", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(3, numericInput("cure_add_E_t_h1", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                           , column(2, checkboxInput("cure_add_row1", "Add another time segment?", FALSE))  )
                         , conditionalPanel( 
                           condition = "input.cure_add_row1 == true"
                           , fluidRow(
                             column(2, numericInput("cure_add_E_P_h2", "Rated Heater Power", 0, min = 0, NA, NA))
                             , column(2, selectizeInput("cure_add_E_u_h2", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                             , column(3, numericInput("cure_add_E_per_h2", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                             , column(3, numericInput("cure_add_E_t_h2", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                             , column(2, checkboxInput("cure_add_row2", "Add another time segment?", FALSE))   )
                           , conditionalPanel( 
                             condition = "input.cure_add_row2 == true"
                             , fluidRow(
                               column(2, numericInput("cure_add_E_P_h3", "Rated Heater Power", 0, min = 0, NA, NA))
                               , column(2, selectizeInput("cure_add_E_u_h3", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                               , column(3, numericInput("cure_add_E_per_h3", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                               , column(3, numericInput("cure_add_E_t_h3", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                               , column(2, checkboxInput("cure_add_row3", "Add another time segment?", FALSE))      )
                             , conditionalPanel( 
                               condition = "input.cure_add_row3 == true"
                               , fluidRow(
                                 column(2, numericInput("cure_add_E_P_h4", "Rated Heater Power", 0, min = 0, NA, NA))
                                 , column(2, selectizeInput("cure_add_E_u_h4", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                 , column(3, numericInput("cure_add_E_per_h4", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                 , column(3, numericInput("cure_add_E_t_h4", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                 , column(2, checkboxInput("cure_add_row4", "Add another time segment?", FALSE))   )
                               , conditionalPanel( 
                                 condition = "input.cure_add_row4 == true"
                                 , fluidRow(
                                   column(2, numericInput("cure_add_E_P_h5", "Rated Heater Power", 0, min = 0, NA, NA))
                                   , column(2, selectizeInput("cure_add_E_u_h5", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                   , column(3, numericInput("cure_add_E_per_h5", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                   , column(3, numericInput("cure_add_E_t_h5", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                   , column(2, checkboxInput("cure_add_row5", "Add another time segment?", FALSE))      )
                                 , conditionalPanel( 
                                   condition = "input.cure_add_row5 == true"
                                   , fluidRow(
                                     column(2, numericInput("cure_add_E_P_h6", "Rated Heater Power", 0, min = 0, NA, NA))
                                     , column(2, selectizeInput("cure_add_E_u_h6", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                     , column(3, numericInput("cure_add_E_per_h6", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                     , column(3, numericInput("cure_add_E_t_h6", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                     , column(2, checkboxInput("cure_add_row6", "Add another time segment?", FALSE))   )
                                   , conditionalPanel( 
                                     condition = "input.cure_add_row6 == true"
                                     , fluidRow(
                                       column(2, numericInput("cure_add_E_P_h7", "Rated Heater Power", 0, min = 0, NA, NA))
                                       , column(2, selectizeInput("cure_add_E_u_h7", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                       , column(3, numericInput("cure_add_E_per_h7", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                       , column(3, numericInput("cure_add_E_t_h7", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                       , column(2, checkboxInput("cure_add_row7", "Add another time segment?", FALSE))      )
                                     , conditionalPanel( 
                                       condition = "input.cure_add_row7 == true"
                                       , fluidRow(
                                         column(2, numericInput("cure_add_E_P_h8", "Rated Heater Power", 0, min = 0, NA, NA))
                                         , column(2, selectizeInput("cure_add_E_u_h8", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                         , column(3, numericInput("cure_add_E_per_h8", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                         , column(3, numericInput("cure_add_E_t_h8", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                         , column(2, checkboxInput("cure_add_row8", "Add another time segment?", FALSE))   )
                                       , conditionalPanel( 
                                         condition = "input.cure_add_row8 == true"
                                         , fluidRow(
                                           column(2, numericInput("cure_add_E_P_h9", "Rated Heater Power", 0, min = 0, NA, NA))
                                           , column(2, selectizeInput("cure_add_E_u_h9", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                           , column(3, numericInput("cure_add_E_per_h9", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                           , column(3, numericInput("cure_add_E_t_h9", "Heater Running Time (min)", 0, min = 0, NA, NA)) 
                                           , column(2, checkboxInput("cure_add_row9", "Add another time segment?", FALSE))      )
                                         , conditionalPanel( 
                                           condition = "input.cure_add_row9 == true"
                                           , fluidRow(
                                             column(2, numericInput("cure_add_E_P_h0", "Rated Heater Power", 0, min = 0, NA, NA))
                                             , column(2, selectizeInput("cure_add_E_u_h0", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                                             , column(3, numericInput("cure_add_E_per_h0", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                                             , column(3, numericInput("cure_add_E_t_h0", "Heater Running Time (min)", 0, min = 0, NA, NA))       )   
                                           
                                         ))) #10 , 9 , 8
                                   )))  #7, 6, 5
                             ))) # 4, 3, 2 - 1 does not have a conditional panel
                         , h4("Other")
                         , fluidRow(
                           column(3, numericInput("cure_add_E_P_o", "Rated Equipment Power (kW)", 0, min = 0, NA, NA))
                           , column(3, selectizeInput("cure_add_E_u_o", "Units", choices = c("kW (electricity)", "BTU/h (Natural Gas)"), selected = "kW (electricity)"))
                           , column(3, numericInput("cure_add_E_per_o", "Percent of Rated Power (%)", 100, min = 0, 100, 5))
                           , column(3, numericInput("cure_add_E_t_o", "Equipment Running Time (min)", 0, min = 0, NA, NA)) )
                           ))
                     # Finishing ---- 
                     , conditionalPanel(
                       condition = 'input.add_data_which == "Finish"'
                       , fluidRow(column(5,h4("Custom Finishing Process")))
                       , fluidRow(column(4, textInput("finish_add", "Name", "Custom Finish"))
                                  , column (4, numericInput("finish_add_E", "Specific Embodied Energy (MJ/kg)", 0, min = 0, NA, NA))
                                  , column(3,actionButton("gofinish", strong("Add Finishing Data")), style = "padding:25px;")
                                  , column(1,         hidden(
                                    p(id = "fini", icon("check", "fa-2x"))), style = "padding:25px;", align = "left" )
                       ))
                   # End Tab ----
                       )},
            # Initial Tab ----   
            tabPanel("Initial Inputs", h1("Initial Inputs")
                     #, p("Enter information on modeled part:  part name, part weight, and molding technology to be used")
                     , fluidRow(
                #TechSet1
                       column(6
                              , h2("Technology Set 1")
                              , wellPanel(
                                textInput("name1", "Part Name","Part 1")
                                , numericInput("finalweight1","Final Part Weight (kg)", 1,  min = 0,NA,NA)
                                , selectizeInput("moldingInput1", label = "Molding Process",
                                                 choices = NULL, multiple = FALSE,
                                                 options = list(placeholder = 'Choose Molding Process    '))  )
                              , fluidRow(column (6,strong("Embodied Energy:")),column(6,textOutput("EnergyNum1"), align = "right"))
                              , wellPanel( style = "background-color: #FFCD00;"
                                           , checkboxInput("insertsAUSERYN1", strong("Use inserts or a core?", style = "font-size:120%"), FALSE)
                                              , conditionalPanel(
                                                condition = "input.insertsAUSERYN1 == true"
                                                , p("The final mass of the part should include the mass of the inserts or cores.")
                                                , numericInput("insertsAfrac1","Insert/Core Mass", 0.0,  min = 0.0, NA, 0.1)
                                           , checkboxInput("insertsBUSERYN1", strong("Use Additional Insert/Core Material?", style = "font-size:120%"),FALSE)
                                               , conditionalPanel(
                                                 condition = "input.insertsBUSERYN1 == true"
                                                 , numericInput("insertsBfrac1","Additional Insert/Core Mass", 0.0,  min = 0.0, NA, 0.1))))
                              )
                 #TechSet2
                       , column(6
                              , h2("Technology Set 2")
                              , wellPanel(
                                 textInput("name2", "Part Name","Part 2")
                                 , numericInput("finalweight2","Final Part Weight (kg)", 1, min = 0,NA,NA)
                                , selectizeInput("moldingInput2", label = "Molding Process",
                                                   choices = NULL, selected = "", multiple = FALSE,
                                                   options = list(placeholder = 'Choose Molding Process    '))
                                )
                              , fluidRow(column (6,strong("Embodied Energy:")),column(6,textOutput("EnergyNum2"), align = "right"))
                              , wellPanel( style = "background-color: #FFCD00;"
                                             , checkboxInput("insertsAUSERYN2", strong("Use inserts or a core?", style = "font-size:120%"),FALSE)
                                             , conditionalPanel(
                                               condition = "input.insertsAUSERYN2 == true"
                                               , p("The final mass of the part should include the mass of the inserts or cores.")
                                               #THIS SHOULD BE MASS OF INSERT AND THEN NEED A CALC TO DETERMINE THE TRUE MASS FRACS
                                               , numericInput("insertsAfrac2","Insert/Core Mass", 0.0,  min = 0.0, NA, NA)
                                               
                                               , checkboxInput("insertsBUSERYN2", strong("Use Additional Insert/Core Material?", style = "font-size:120%"),FALSE)
                                               
                                               #THIS SHOULD BE MASS OF INSERT AND THEN NEED A CALC TO DETERMINE THE TRUE MASS FRACS
                                               , conditionalPanel(
                                                 condition = "input.insertsBUSERYN2 == true"
                                                 , numericInput("insertsBfrac2","Additional Insert/Core Mass", 0.0,  min = 0.0, NA, NA)) ))
                              )
                 #EndTab
                 )),
            
            # Fiber Tab ----
            tabPanel("Fiber", h1("Fiber & Fiber Manufacturing") 
                     #, p("Enter information on modeled part: fiber type, tow, and fiber mass fraction")
                      , fluidRow(
                       #TechSet1
                       column(6,
                              h2("Technology Set 1")
                              , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname1"), align = "left"), column(3,textOutput("partweight1"), align = "right"))
                              , fluidRow(column(4, strong("Molding Techology: ")), column(5, textOutput("moldshort1"), align = "left"))
                              , hr()
                              , fluidRow(column(8, strong("Default Mass Fiber Fraction: ")), column(4, textOutput("moldfracNum1"), align = "right"))
                              , wellPanel( 
                                 selectizeInput("fiberInput1", label = "Fiber Type",
                                                 choices = NULL, selected = "", multiple = FALSE,
                                                 options = list(placeholder = 'Choose Fiber Type/Tow     '))
                                , numericInput("moldfracUSERNum1","Fiber Mass Fraction",  min = 0.0, max = 100.0, value = 0.0,5.0)
                              )
                              , column (6,strong("Embodied Energy:")),column (6,textOutput("fiberEnergyNum1"), align = "right")  
                       )
                       #TechSet2
                       , column(6,
                                h2("Technology Set 2")
                                , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname2"), align = "left"), column(3,textOutput("partweight2"), align = "right"))                                            
                                , fluidRow(column(4, strong("Molding Techology: ")), column(5, textOutput("moldshort2"), align = "left"))
                                , hr()
                                , fluidRow(column(8, strong("Default Mass Fiber Fraction: ")), column(4, textOutput("moldfracNum2"), align = "right"))
                                , wellPanel(
                                   selectizeInput("fiberInput2", label = "Fiber Type",
                                                   choices = NULL, selected = "", multiple = FALSE,
                                                   options = list(placeholder = 'Choose Fiber Type/Tow     '))
                                  , numericInput("moldfracUSERNum2","Fiber Mass Fraction", min = 0.0, max = 100.0, value = 0.0,5.0)
                                  )
                                , column (6,strong("Embodied Energy:")),column (6,textOutput("fiberEnergyNum2"), align = "right")
                       )
                     #EndFiber 
                     )),
            # Matrix Tab ----
            {tabPanel("Matrix", h1("Matrix Materials Manufacturing")
                    # Enter information on modeled part: primary matrix material and other materials 
                    #(additional matrix, additives, fillers and inserts) and the mass fraction of each
                     , fluidRow( 
                  # Matrix Techset1 ----
                   column(6
                          , h2("Technology Set 1")
                          , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname1b"), align = "left"), column(3,textOutput("partweight1b"), align = "right"))
                          , fluidRow(column(4, strong("Fiber:")), column(5, textOutput("fibername1b"), align = "left"), column(3,textOutput("fiberfrac1b"), align = "right"))
                          , fluidRow(column(4, strong("Molding Techology: ")), column(8, textOutput("moldshort1b"), align = "left"))
                          , hr()
                          #Choose Primary Matrix
                          , wellPanel( 
                            selectizeInput("PriMatrixInput1", label = "Primary Matrix Material",
                                           choices = NULL,  selected = "",  multiple = FALSE,
                                           options = list(placeholder = 'Choose Matrix Material '))
                            , numericInput("primatrixfrac1","Primary Matrix Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0))
                          , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("primatrixEnergyNum1"), align = "right"))
                          , fluidRow(column(8, strong("Mass Fraction Check:")), column(4,textOutput("massfracsum1"), align = "right"))
                          
                          # Additional Material 
                          , wellPanel( style = "background-color: #FFCD00;"
                                 , checkboxInput("othermatrixAUSERYN1", strong("Use Additional Matrix Materials?", style = "font-size:120%"),FALSE)
                            # Additional Material A 
                                 , conditionalPanel(
                                   condition = "input.othermatrixAUSERYN1 == true"
                                   , selectizeInput("types1a", label = "Additional Matrix Material Type",
                                     choices = c("Matrix", "Additive", "Filler", "Not Used"), selected = "Not Used", multiple = FALSE)
                                   , selectizeInput("OtherMatrixAInput1", label = "Additional Matrix Material",
                                      choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                   , numericInput("othermatrixAfrac1","Additional Material A Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                                       ))
                         , conditionalPanel(
                            condition = "input.othermatrixAUSERYN1 == true"
                            , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("othermatrixAEnergyNum1"), align = "right"))
                             # Additional Material B
                            , wellPanel(style = "background-color: #FFCD00;"
                                , checkboxInput("othermatrixBUSERYN1", strong("Use Additional Matrix Materials?", style = "font-size:120%"),FALSE)
                                , conditionalPanel(
                                   condition = "input.othermatrixBUSERYN1 == true"
                                   , selectizeInput("types1b", label = "Additional Matrix Material Type",
                                       choices = c("Matrix", "Additive", "Filler", "Not Used"), selected = "Not Used", multiple = FALSE)
                                   , selectizeInput("OtherMatrixBInput1", label = "Additional Matrix Material",
                                        choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                   , numericInput("othermatrixBfrac1","Additional Material B Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                                        ))
                            )
                          , conditionalPanel(
                            condition = "input.othermatrixBUSERYN1 == true"
                            , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("othermatrixBEnergyNum1"), align = "right"))
                            # Additional Material C
                            ,  wellPanel(style = "background-color: #FFCD00;"
                                 , checkboxInput("othermatrixCUSERYN1", strong("Use Additional Matrix Materials?", style = "font-size:120%"),FALSE)
                                 , conditionalPanel(
                                    condition = "input.othermatrixCUSERYN1 == true"
                                    , selectizeInput("types1c", label = "Additional Matrix Material Type",
                                        choices = c("Matrix", "Additive", "Filler", "Not Used"), selected = "Not Used", multiple = FALSE)      
                                    , selectizeInput("OtherMatrixCInput1", label = "Additional Matrix Material",
                                        choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                    , numericInput("othermatrixCfrac1","Additional Material C Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                                         ))
                            )
                          , conditionalPanel(
                            condition = "input.othermatrixCUSERYN1 == true"
                            , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("othermatrixCEnergyNum1"), align = "right"))
                            )
                         
                          # Insert 
                          ,  conditionalPanel(
                             condition = "input.insertsAUSERYN1 == true"
                             , wellPanel(style = "background-color: #FFCD00;"
                                , selectizeInput("InsertsAInput1", label = "Insert/Core Type",
                                    choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                    options = list(placeholder = 'Choose Insert     '))    )
                            , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("insertsAEnergyNum1"), align = "right"))
                            
                            , conditionalPanel( 
                              condition = "input.insertsBUSERYN1 == true"
                              , wellPanel(style = "background-color: #FFCD00;"
                                  , selectizeInput("InsertsBInput1", label = "Additional Insert/Core Type",
                                       choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                       options = list(placeholder = 'Choose Insert     ')))
                              , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("insertsBEnergyNum1"), align = "right"))
                          ))
                              # End TS1
                          )    
                  # Matrix TechSet2 ----
                   , column(6
                            , h2("Technology Set 2")
                            , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname2b"), align = "left"), column(3,textOutput("partweight2b"), align = "right"))
                            , fluidRow(column(4, strong("Fiber:")), column(5, textOutput("fibername2b"), align = "left"), column(3,textOutput("fiberfrac2b"), align = "right"))
                            , fluidRow(column(4, strong("Molding Techology: ")), column(8, textOutput("moldshort2b"), align = "left"))
                            , hr()
                            #Choose Primary Matrix 
                            ,  wellPanel(
                              selectizeInput("PriMatrixInput2", label = "Primary Matrix Material",
                                     choices = NULL,  selected = "",  multiple = FALSE,
                                     options = list(placeholder = 'Choose Matrix Material  '))
                               , numericInput("primatrixfrac2","Primary Matrix Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0))
                            , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("primatrixEnergyNum2"), align = "right"))
                            , fluidRow(column(8, strong("Mass Fraction Check:")), column(4,textOutput("massfracsum2"), align = "right"))
                            
                            # Additional Material 
                            ,  wellPanel(style = "background-color: #FFCD00;"
                                 , checkboxInput("othermatrixAUSERYN2", strong("Use Additional Matrix Materials?", style = "font-size:120%"),FALSE)
                                  # Additional Material A
                                  , conditionalPanel(
                                      condition = "input.othermatrixAUSERYN2"
                                  , selectizeInput("types2a", label = "Additional Matrix Material Type",
                                         choices = c("Matrix", "Additive", "Filler", "Not Used"), selected = "Not Used", multiple = FALSE)
                                   , selectizeInput("OtherMatrixAInput2", label = "Additional Matrix Material",
                                        choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                   , numericInput("othermatrixAfrac2","Additional Material A Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0)
                                        ))
                            , conditionalPanel(
                              condition = "input.othermatrixAUSERYN2"
                                  ,fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("othermatrixAEnergyNum2"), align = "right"))
                                   # Additional Mateiral B  
                                  , wellPanel(style = "background-color: #FFCD00;"
                                  , checkboxInput("othermatrixBUSERYN2", strong("Use Additional Matrix Materials?", style = "font-size:120%"),FALSE)
                                   , conditionalPanel(
                                     condition = "input.othermatrixBUSERYN2 == true"
                                       , selectizeInput("types2b", label = "Additional Matrix Material Type",
                                                choices = c("Matrix", "Additive", "Filler", "Not Used"), selected = "Not Used", multiple = FALSE)
                                       , selectizeInput("OtherMatrixBInput2", label = "Additional Matrix Material",
                                                 choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                       , numericInput("othermatrixBfrac2","Additional Material B Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0))
                                  ))
                            , conditionalPanel(
                              condition = "input.othermatrixBUSERYN2 == true"    
                              , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("othermatrixBEnergyNum2"), align = "right"))
                              # Additional Material C 
                              , wellPanel(style = "background-color: #FFCD00;"
                                   , checkboxInput("othermatrixCUSERYN2", strong("Use Additional Matrix Materials?", style = "font-size:120%"),FALSE)
                                   , conditionalPanel(
                                        condition = "input.othermatrixCUSERYN2 == true"
                                       , selectizeInput("types2c", label = "Additional Matrix Material Type",
                                                  choices = c("Matrix", "Additive", "Filler", "Not Used"), selected = "Not Used", multiple = FALSE)
                                       , selectizeInput("OtherMatrixCInput2", label = "Additional Matrix Material",
                                                    choices = NULL,  selected = "Not Used",  multiple = FALSE)
                                      , numericInput("othermatrixCfrac2","Additional Material C Mass Fraction", 0.0,  min = 0.0, max = 100.0,5.0))))
                            , conditionalPanel(
                              condition = "input.othermatrixCUSERYN2 == true"
                              , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("othermatrixCEnergyNum2"), align = "right"))
                              )
                           
                            # Inserts 
                            , conditionalPanel(
                              condition = "input.insertsAUSERYN2 == true"
                              , wellPanel(style = "background-color: #FFCD00;"
                                          , selectizeInput("InsertsAInput2", label = "Insert/Core Type",
                                                           choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                                           options = list(placeholder = 'Choose Insert     ')))
                              , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("insertsAEnergyNum2"), align = "right"))
                             , conditionalPanel(
                                condition = "input.insertsBUSERYN2 == true"
                                , wellPanel(style = "background-color: #FFCD00;"
                                            , selectizeInput("InsertsBInput2", label = "Additional Insert/Core Type",
                                                             choices = NULL,  selected = "Not Used",  multiple = FALSE,
                                                             options = list(placeholder = 'Choose Insert     ')))
                                , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("insertsBEnergyNum2"), align = "right"))
                              ))
                             # End TS2  
                           ))
                #END TAB 
                 )}  ,
                      
            # Intermediate Tab ----
            tabPanel("Intermediate", h1("Fiber Intermediate Manufacturing & Layup") 
                     #, p("Enter information on modeled part: intermediate type layup scrap rate")
                     , fluidRow( 
                       #Techset1
                       column(6
                          , h2("Technology Set 1")
                          , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname1a"), align = "left"), column(3,textOutput("partweight1a"), align = "right"))
                          , fluidRow(column(4, strong("Fiber:")), column(5, textOutput("fibername1a"), align = "left"), column(3,textOutput("fiberfrac1a"), align = "right"))
                          , fluidRow(column(4, strong("Matrix:")), column(5, textOutput("primatrixname1a"), align = "left"), column(3,textOutput("primatrixfrac1a"), align = "right"))
                          , fluidRow(column(4, strong("Molding Techology: ")), column(8, textOutput("moldshort1a"), align = "left"))
                          , hr()
                          , wellPanel(style = "background-color: #FFCD00;"
                                      , checkboxInput("intYN1", strong("View all Fiber Intermediates?", style = "font-size:120%"),FALSE)
                          )
                          , wellPanel(selectizeInput("intInput1", label = "Fiber Intermediate",
                                                     choices = NULL,  selected = "",  multiple = FALSE,
                                                     options = list(placeholder = 'Choose Fiber Intermediate Type   ')))
                          , fluidRow(column(6, strong("Embodied Energy:")), column(6,textOutput("intEnergyNum1"), align = "right"))
                          , fluidRow(column(8, strong("Default Layup Scrap Rate: ")), column(4, textOutput("intscrapNum1"), align = "right"))
                          , wellPanel(numericInput("intscrapUSERNum1","Layup Scrap Rate", 0.0,  min = 0.0, max = 100.0,5.0)
                                      ,numericInput("intscraprecycle1","Layup Recycle Fraction (% scrap that is recycled)", 
                                                    0.0,  min = 0.0, max = 100.0,5.0))
                       )    
                       #TechSet2
                       , column(6
                          , h2("Technology Set 2")
                          , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname2a"), align = "left"), column(3,textOutput("partweight2a"), align = "right"))
                          , fluidRow(column(4, strong("Fiber:")), column(5, textOutput("fibername2a"), align = "left"), column(3,textOutput("fiberfrac2a"), align = "right"))
                          , fluidRow(column(4, strong("Matrix:")), column(5, textOutput("primatrixname2a"), align = "left"), column(3,textOutput("primatrixfrac2a"), align = "right"))
                          , fluidRow(column(4, strong("Molding Techology: ")), column(8, textOutput("moldshort2a"), align = "left"))
                          , hr()
                          , wellPanel(style = "background-color: #FFCD00;"
                                      , checkboxInput("intYN2", strong("View all Fiber Intermediates?", style = "font-size:120%"),FALSE)
                          )
                          , wellPanel(selectizeInput("intInput2", label = "Fiber Intermediate",
                                                     choices = NULL,  selected = "",  multiple = FALSE,
                                                     options = list(placeholder = 'Choose Fiber Intermediate Type     ')))
                          , fluidRow(column(6, strong("Embodied Energy:")), column (6,textOutput("intEnergyNum2"), align = "right"))
                          , fluidRow(column(8, strong("Default Layup Scrap Rate: ")), column(4, textOutput("intscrapNum2"), align = "right"))
                          , wellPanel(numericInput("intscrapUSERNum2","Layup Scrap Rate", 0.0,  min = 0.0, max = 100.0, 5.0)
                                      ,numericInput("intscraprecycle2","Layup Recycle Fraction (% scrap that is recycled)",
                                                    0.0,  min = 0.0, max = 100.0,5.0))
                       ))
                     #END TAB
            ),
            
            # Molding Tab ----  
            tabPanel("Molding", h1("Molding, Curing & Finishing")
                   # , p("Enter information on modeled part: curing technology, amount of finishing, 
                    #   finishing scrap rate and confirm the molding technology and molding yield")
                     , fluidRow(
                       #TechSet1
                       column(6
                          , h2("Technology Set 1")
                          , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname1c"), align = "left"), column(3,textOutput("partweight1c"), align = "right"))
                          , fluidRow(column(4, strong("Fiber:")), column(5, textOutput("fibername1c"), align = "left"), column(3,textOutput("fiberfrac1c"), align = "right"))
                          , fluidRow(column(4, strong("Matrix:")), column(5, textOutput("primatrixname1c"), align = "left"), column(3,textOutput("primatrixfrac1c"), align = "right"))
                          , fluidRow(column(4, strong("Intermediate")), column(5, textOutput("intname1c"), align = "left"), column(3,textOutput("intscrapNum1c"), align = "right"))
                          , fluidRow(column(4, strong("Molding Techology: ")), column(8, textOutput("moldshort1c"), align = "left"))
                          , hr()
                          #Molding Yield
                          , fluidRow(column(8, strong("Default Molding Yield: ")), column(4, textOutput("moldyieldNum1"), align = "right"))
                          , wellPanel(numericInput("moldyieldUSERNum1","Molding Yield", 0.0,  min = 0.0, max = 100.0,5.0)
                                      , numericInput("moldyieldrecycle1","Molding Recycle Fraction (% scrap that is recycled)"
                                                     , 0.0,  min = 0.0, max = 100.0,5.0))
                          , br()
                          #curing tech & energy
                          , wellPanel(style = "background-color: #FFCD00;"
                                  , checkboxInput("cureYN1", strong("View all Curing Processes?", style = "font-size:120%"),FALSE))
                          , wellPanel(selectizeInput("cureInput1", label = "Curing Process",
                                             choices = NULL, selected = "", multiple = FALSE,
                                             options = list(placeholder = 'Choose Curing Process    ')))
                          , fluidRow(column (6,strong("Embodied Energy:")),column(6,textOutput("cureEnergyNum1"), align = "right"))
                          , br()
                          #finishing level & scrap
                          , wellPanel(style = "background-color: #FFCD00;"
                                      , selectizeInput("finishInput1", label = "Finishing Level",
                                             choices = NULL, selected = "", multiple = FALSE,
                                             options = list(placeholder = 'Choose Finishing Level    ')))
                          , fluidRow(column (6,strong("Embodied Energy:")),column(6,textOutput("finishEnergyNum1"), align = "right"))
                          , wellPanel(style = "background-color: #FFCD00;"
                                      , numericInput("finishscrap1","Finish Scrap Rate", 0.0,  min = 0.0, max = 100.0,5)
                                      , numericInput("finishscraprecycle1", "Finish Recycle Fraction (% scrap that is recycled)", 
                                                     0.0,  min = 0.0, max = 100.0,5.0))
                   )
                       #TechSet2
                       , column(6
                                , h2("Technology Set 2")
                                , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname2c"), align = "left"), column(3,textOutput("partweight2c"), align = "right"))
                                , fluidRow(column(4, strong("Fiber:")), column(5, textOutput("fibername2c"), align = "left"), column(3,textOutput("fiberfrac2c"), align = "right"))
                                , fluidRow(column(4, strong("Matrix:")), column(5, textOutput("primatrixname2c"), align = "left"), column(3,textOutput("primatrixfrac2c"), align = "right"))
                                , fluidRow(column(4, strong("Intermediate")), column(5, textOutput("intname2c"), align = "left"), column(3,textOutput("intscrapNum2c"), align = "right"))
                                , fluidRow(column(4, strong("Molding Techology: ")), column(8, textOutput("moldshort2c"), align = "left"))
                                , hr()
                                #Molding Yield
                                , fluidRow(column(8, strong("Default Molding Yield: ")), column(4, textOutput("moldyieldNum2"), align = "right"))
                                , wellPanel(numericInput("moldyieldUSERNum2","Molding Yield", 0.0,  min = 0.0, max = 100.0,5)
                                            , numericInput("moldyieldrecycle2", "Molding Recycle Fraction (% scrap that is recycled)", 
                                                           0.0,  min = 0.0, max = 100.0,5.0))
                                , br()
                                #curing tech & energy
                                , wellPanel(style = "background-color: #FFCD00;"
                                            , checkboxInput("cureYN2", strong("View all Curing Processes?", style = "font-size:120%"),FALSE))
                                , wellPanel(selectizeInput("cureInput2", label = "Curing Process",
                                                 choices = NULL, selected = "", multiple = FALSE,
                                                 options = list(placeholder = 'Choose Curing Process    ')))
                                , fluidRow(column (6,strong("Embodied Energy:")),column(6,textOutput("cureEnergyNum2"), align = "right"))
                                , br()
                                #finishing level & scrap
                               , wellPanel(style = "background-color: #FFCD00;"
                                            , selectizeInput("finishInput2", label = "Finishing Level",
                                                       choices = NULL, selected = "", multiple = FALSE,
                                                       options = list(placeholder = 'Choose Finishing Level    ')))
                                , fluidRow(column (6,strong("Embodied Energy:")),column(6,textOutput("finishEnergyNum2"), align = "right"))
                                , wellPanel(style = "background-color: #FFCD00;"
                                            , numericInput("finishscrap2","Finish Scrap Rate", 0.0,  min = 0.0, max = 100.0,5.0)
                                            , numericInput("finishscraprecycle2","Finish Recycle Fraction (% scrap that is recycled)"
                                                           , 0.0,  min = 0.0, max = 100.0,5.0))
                       ))
                     #EndTab
                     ),
            # Summary Tab ----
            tabPanel("Summary", h1("Summary")
                    #, p("Summary of User Choices and Embodied Energy")
                     #Display Part Name & Weight & Molding Process & Fiber Fraction
                    #, fluidRow(actionButton("calculate", strong("Run Model")))
                     , fluidRow(
                       #TechSet1
                       column(6,
                              h2("Technology Set 1")
                              , fluidRow(column(4, strong("Part: ")), column(5, textOutput("partname1d"), align = "left"), column(3,textOutput("partweight1d"), align = "right"))
                               )
                       #TechSet2
                       , column(6,
                                h2("Technology Set 2")
                                , fluidRow(column(4, strong("Part Name: ")), column(5, textOutput("partname2d"), align = "left"), column(3,textOutput("partweight2d"), align = "right"))
                                 ))
                     , hr()
                     , fluidRow(
                       column(6,
                                tableOutput("Table.mat.1")
                              , br()
                              , tableOutput("Table.pro.1")
                              , style = "border-right:2px solid #000;")
                       , column(6,
                                  tableOutput("Table.mat.2")
                                , br()
                                , tableOutput("Table.pro.2"))
                     )
                     #End Tab
            ),
            
            # Results Tab ---- 
            tabPanel("Results", h1("Results")
                     #p("This is where users will be able to graphically compare the two technology pathways"),
                  , selectizeInput("whichgraph", label = "Type of Graph"
                                   , choices = c("Bar", "Pie")
                                   , selected = "Bar", multiple = FALSE)
                    , conditionalPanel(
                       condition = 'input.whichgraph == "Bar"'
                      , plotlyOutput("plot1") )
                     , conditionalPanel(
                       condition = 'input.whichgraph == "Pie"'
                     #, plotOutput("plot2") )
                     , column(6, 
                              h2("Technology Set 1"), plotlyOutput("plot2a", width = "100%", height = "auto"))
                     , column (6, 
                              h2("Technology Set 2"), plotlyOutput("plot2b", width = "100%", height = "auto"))
                     )
                     , hr()
                     , tableOutput("ResultsTable")
            ),
            
            
            # Download Tab ----
            tabPanel("Downloads", h1("Downloads")
                     , fluidRow(  
                       column(6, h2("Technology Set 1")), column(6, h2("Technology Set 2"))
                       
                       , column(12, h2("Results")
                                , tags$li('Download the results of the tool, including chosen materials/processes,
                                      mass fraction or yield, specific and embodied energy for each stage and mass evaluated at each stage.'))
                       , column(6 
                                , br()
                                , textInput("DLR1_name", "Name Results File 1", value = paste("Results1", Sys.Date()))
                                , downloadButton('DL_results1', "Download Results"))
                       , column(6 
                                , br()
                                , textInput("DLR2_name", "Name Results File 2", value = paste("Results2", Sys.Date()))
                                , downloadButton('DL_results2', "Download Results")) )
                     
                     , hr() 
                     , fluidRow(
                       column(12 , h2("Custom Data")
                              , tags$li("Download the custom data added in the current session.")
                              , tags$li("This file can be uploaded in a later session instead of manual data entry.")
                              , tags$li(strong("We recommend that users not change the data in these files manually."), style = "color: red")
                              , br()
                              , textInput("DLCustom_name", "Name Custom Data File", value = paste("Add_Custom", Sys.Date()))
                              , downloadButton('DL_custom', "Download Custom Data")))
                     , fluidRow(
                       column(12, h2("Input files")
                              , tags$li("Download files with all the options chosen in the current session")
                              , tags$li("This file can be uploaded in a later session instead of manual data entry.")
                              , tags$li(strong("We recommend that users not change the data in these files manually."), style = "color: red"))
                       , column(6
                                , br()
                                , textInput("DLI1_name", "Name Set 1 Input File", value = paste("Inputs1", Sys.Date()))
                                , downloadButton('DL_inputs1', "Download Inputs"))
                       , column(6
                                , br()
                                , textInput("DLI2_name", "Name Set 2 Input File", value = paste("Inputs2", Sys.Date()))
                                , downloadButton('DL_inputs2', "Download Inputs")) )
                     , hr()
                     , fluidRow(
                       column(12, h2("Calculations")
                              , tags$li('Download the calculation files made by this tool.')
                              , tags$li('Requires both technology sets.  Check the "Results" Page, if both
                                       table are displaying with no errors, then this is ready to be downloaded.')
                              ,br()
                              , textInput("DLC_name", "Name Calculation Files", value = paste("Tool_Calculations", Sys.Date()))
                              , downloadButton('zipcalcs', "Download Calculation Zip File")))
            ),
            
            # Molding Properties Tab ----
            tabPanel("Molding Properties", h1("Molding Properties")
                     , fluidRow(
                       p('The columns and rows visible can be changed by clicking the "Select all..." boxes 
                        or within the "... Visible" boxes and selecting from the list or using the', span(strong(' "Backspace" ')), "or", 
                         span(strong('"Delete"')), 'buttons on your keyboard to remove options')
                     , column(6,
                     
                     
                     checkboxInput('show_allmolds', "Select all Molding Technology Options", value = TRUE)
                     , selectizeInput('show_molds', 
                                    label =  'Molding Technologies Visible:', 
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = TRUE,
                                    width = '100%')
                     )
                     , column(6,
                     checkboxInput('show_allvars', "Select all Columns", value = FALSE)
                    ,  selectizeInput('show_vars', 
                                       label =  'Columns Visible:', 
                                       choices = NULL,
                                        selected = NULL,
                                       multiple = TRUE,
                                      width = '100%')
                     )
                     )
                    
                    ,  DT::dataTableOutput("props")
                    
                    
            ),
            # Ref Tab ----
             tabPanel ("References", h1("References")
                , p("To view citations for process and material embodied energy, choose Type and then the specific process/material")
                , selectizeInput("cite_type", label = "Type", 
                                 choices = c("Select Type","Fiber", "Matrix", "Additive", "Filler", "Insert/Core", "Intermediate", "Mold", "Cure", "Finishing"),
                                 selected = "",
                                 multiple = FALSE,
                                 options = list(placeholder = "Select Type"))
                , selectizeInput("cite_specific", label = "Process/Material", 
                                 choices = NULL,
                                 multiple = FALSE,
                                 options = list(placeholder = "Select Process/Material"))
                , textOutput("citation1")
                , br()
                , textOutput("citation2")
                , br()
                , textOutput("citation3")
                , br()
                , textOutput("citation4")
                , br()
                , textOutput("citation5")
                 )
          
            #TEST TAB ----
      # Tab to display test tables or other tests
 # , tabPanel("TEST", h1("TEST")
# #To use, remove "#' (do not remove from this line) # Currently testing yield & energy calculation data
  #   , tableOutput("yieldtab1")
  #   , tableOutput("yieldtab2")
  #   , tableOutput("energytab1")
  #   , tableOutput("energytab2")
  #   , tableOutput("testtable1")
  #   , tableOutput("testtable2")
    # , tableOutput("testtable")
    # , textOutput("testtext1")
  #   , textOutput("testtext2")
# 
# )
                 
            # End ----
            , widths = c(2,10))))
