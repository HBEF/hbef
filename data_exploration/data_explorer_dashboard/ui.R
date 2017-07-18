library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard)


########### IMPORTANT LISTS #############


###  Lists for the sidebar  ###
#Edit if there are values that do not appear or are not relevant to your data. 

solutes_cations <- list("Aluminum (Al)" = "Al",
                        "Magnesium (Mg)" = "Mg",
                        "Calcium (Ca)" = "Ca",
                        "Sodium (Na)" = "Na",
                        "Potassium (K)" = "K")

solutes_anions <- list("Phosphate (PO4)" = "PO4",
                        "Sulfate (SO4)" = "SO4",
                        "Nitrate (NO3)" = "NO3",
                        "Silicon Dioxide (SiO2)" = "SiO2",
                        "Chloride (Cl)" = "Cl",
                        "Bicarbonate (HCO3)" = "HCO3")
solutes_H <- list("Hydrogen (H)" = "H", "Dissolved Inorganic Carbon" = "DIC")

all_solutes <- c(solutes_cations, solutes_anions, solutes_H)

watersheds <- list("Watershed 1" = "1",
                   "Watershed 2" = "2", 
                   "Watershed 3" = "3",
                   "Watershed 4" = "4",
                   "Watershed 5" = "5",
                   "Watershed 6" = "6",
                   "Watershed 7" = "7",
                   "Watershed 8" = "8",
                   "Watershed 9" = "9")

water_sources <- list("Streamflow (Q)" = "streamflow", 
                     "Precipitation (P)" = "precipitation")

granularity <- list( "Week" = "week",
                    "Month (VWC)" = "month",
                    "Year (VWC)" = "year")

time_variables <- list("Solute Concentration" = "concentration", 
                       "pH" = "pH",
                       "Charge Balance" = "charge balance",
                       "Temperature" = "temp", 
                       "Acid-Neutralizing Capacity" = "anc", 
                       "Specific Conductivity" = "spcond")
units <- list("uEquivalent/L" = "concentration_ueq","uMole/L" = "concentration_umol", "mg/L" = "concentration_mg")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################



shinyUI(
  
  dashboardPage(skin = "black",
    dashboardHeader(title = "Exploratory Dashboard"),
    dashboardSidebar(
      width = 50,
      sidebarMenu(
        menuItem(" ", tabName = "dashboard", icon = icon("home")),
        menuItem(" ", tabName = "exploratory", icon = icon("search-plus"))
      )
    ),
    dashboardBody(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      tags$script('
        $(document).on("keydown", function (e) {
        if (e.keyCode === 13) {
          $("#go").click()};
        });
        '),
      tags$head(includeScript(system.file('www', 'ajax.js'))),
      tags$head(includeScript(system.file('www', 'iframeResizer.contentWindow.min.js'))),
      tags$head(includeScript(system.file('www', 'app.js'))),
      tags$head(tags$style(HTML(
        "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
      
      tabItems(
        ##########################################################
        #### ------------  Dashboard Tab Content  ----------- ####
        ##########################################################
        
        tabItem(tabName = "dashboard",
        
        ### ---- Choose a Watershed 
        box(width = 13,
          # Watersheds
          column(5, selectInput("watersheds", label = "",
                                choices = watersheds, multiple = TRUE,
                                selected = "6"))),
        
        ###  ---- PQ GRAPH
        fluidRow(
            tabBox(width = 12, side="right", selected = shiny::icon("circle"),
                   ######## OPTIONS
                   ##Units - Axis Log
                   tabPanel(shiny::icon("gear"),
                            fluidRow(
                              box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                  
                                  ##Units - Y Axis Log
                                  column(6, selectInput("log_pq", label = "Y Axis",
                                                        choices = c("linear", "log"), 
                                                        selected = "linear"))))),
                   ######## PLOT 
                   tabPanel(shiny::icon("circle"),
                            div(class = "titleRow", fluidRow(column(5, tags$h2("Hydrologic Flux")),
                             ##Granularity
                             column(3,  offset = 4, selectInput("granularity", label = "",
                                                                choices = granularity,
                                                                selected = "year")))
                            ),
                            ## Time Plot
                            plotlyOutput("plot_pq")
                   ) #Closes tabpanel
                   
            )# Closes tab Box
            
          ),# Closes Time Row
      
      
        ###  ---- PQ GRAPH END -----
        
        
        fluidRow(
        ###  ---- TIME GRAPH
        #### Main area
        column(9,   
        fluidRow(
         tabBox(width = 12, side="right", selected = shiny::icon("circle"),
                ######## OPTIONS
                ##Units - Axis Log
                tabPanel(shiny::icon("gear"),
                         fluidRow(
                           box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                               
                               ##Units - Y Axis Log
                               column(6, selectInput("log_time", label = "Y Axis",
                                                     choices = c("linear", "log"), 
                                                     selected = "linear"))))),
                ######## PLOT 
                tabPanel(shiny::icon("circle"),
                         div(class = "titleRow", fluidRow(column(5, tags$h2("Time Series Data")),
                          ##Granularity
                          column(4,  offset = 2, selectInput("granularity_time", label = "",
                                                             choices = granularity,
                                                             selected = "year")))
                         ),
                         ## Time Plot
                         conditionalPanel(condition = "input.yaxis_time != 'charge balance'", plotlyOutput("plot_time")),
                         conditionalPanel(condition = "input.yaxis_time == 'charge balance'", plotlyOutput("plot_charge"))
                         
                ) #Closes tabpanel
                
            )# Closes tab Box
         
            ),# Closes Time Row
        
        
        fluidRow(
          
          ###  ---- CQ GRAPH
          
          tabBox(width = 6, side="right", selected = shiny::icon("circle"),
          ######## OPTIONS
             ##Units - Axis Log
             tabPanel(shiny::icon("gear"),
                      fluidRow(
                        box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                    
                      ##Units - X Axis Log
                          column(6, selectInput("log_cq_x", label = "X Axis",
                                                                 choices = c("linear", "log"), 
                                                                 selected = "linear")),  
                      ##Units - Y Axis Log
                      column(6, selectInput("log_cq_y", label = "Y Axis",
                                            choices = c("linear", "log"), 
                                            selected = "linear")))),
                      fluidRow(
                        box(width = 12, title = "Animation", collapsible = TRUE, collapsed = TRUE, 
                        ##Leave trace
                        column(12, checkboxInput("trace_cq", label = ("Leave Trace"),
                                                  value = TRUE)),
                      ##Animation Speed
                        column(12, sliderInput("animation_speed_cq", label = h4("Speed"),
                                                min = 0.25,
                                                max = 2, 
                                                step = 0.25,
                                                post = "x",
                                                value = 1))))),
                 
              ######## PLOT
              tabPanel(shiny::icon("circle"),
              div(class = "titleRow", fluidRow(column(5, tags$h2("c-Q")),
                  ##Granularity
                  column(4, offset = 2,selectInput("granularity_cq", label = "",
                                                   choices = granularity,
                                                   selected = "year")))),         
                  ## CQ plot
              fluidRow(
                conditionalPanel(condition = "input.yaxis_time == 'concentration' || input.yaxis_time == 'charge balance'", plotlyOutput("plot_cq")))
              
              )#Closes Row
              
              
          ),
          
          ###  ---- FLUX GRAPH
          tabBox(width = 6, side="right", selected = shiny::icon("circle"),
          ######## OPTIONS
          ##Units - Axis Log
          tabPanel(shiny::icon("gear"),
                   fluidRow(
                     box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                         
                         ##Units - Y Axis Log
                         column(6, selectInput("log_flux", label = "Y Axis",
                                               choices = c("linear", "log"), 
                                               selected = "linear"))))),
          ######## PLOT 
          tabPanel(shiny::icon("circle"),
          div(class = "titleRow", fluidRow(column(5, tags$h2("Flux")),
                ##Granularity
                column(4,  offset = 2, selectInput("granularity_flux", label = "",
                                                   choices = granularity,
                                                   selected = "year")))
              ),
              ## Flux Plot
          conditionalPanel(condition = "input.yaxis_time == 'concentration' || input.yaxis_time == 'charge balance'", 
                           plotlyOutput("plot_flux"))
          ) #Closes tabpanel
          
        )# Closes tab Box
        
        )# Closes CQ and Flux Row
        
        ),#Closes timegraph and flux and pq column. 
        
        column(3,
        
          #### Side Bar Area
          box(width = 13, height = "1050px", id = "sidebar",
              #Y Axis
              fluidRow(column(12,
                              selectInput("yaxis_time", label = "",
                                          choices = time_variables,
                                          selected = "flux"))),
              ##Water Sources
              fluidRow(
                column(12, checkboxGroupInput("water_sources", label = h4("Water Sources"),
                                              choices = water_sources,
                                              selected = c("precipitation", "streamflow"),
                                              inline = FALSE))),
              ##Units  
              fluidRow(
                column(12, conditionalPanel(condition = "input.yaxis_time == 'concentration'",
                       selectInput("units", label = h4("Units"),
                                       choices = units,
                                       selected = "uEquivalent/L"))),
              
              #Solutes
            
                column(12, conditionalPanel(condition = "input.yaxis_time == 'concentration' || input.yaxis_time == 'charge balance'", 
                                            actionLink("select_all_ions", h4("Solutes")),
                
                #Cations
               
                       actionLink("select_all_cations", h5("Cations")),
                       checkboxGroupInput("solutes_cations", label = "",
                                          choices = solutes_cations,
                                          selected = "Na"),
                
                #Anions
                
                actionLink("select_all_anions", h5("Anions")),
                       checkboxGroupInput("solutes_anions", label = "",
                                          choices = solutes_anions,
                                          selected = "SO4)"),
              #Hydrogen  
              
            
                checkboxGroupInput("solutes_H", label = h4(""),
                                              choices = solutes_H,
                                              selected = ""))))
              
            ) #Closes sidebar box
        )
          ) #Closes Time Graph Row.
      
          ###  ---- TIME GRAPH END -----
      
        
        
        
      ),#Closes Dashboard tab item
      
      ##########################################################
      #### -------  End of Dashboard Tab Content  --------- ####
      ##########################################################
      
      
      ##########################################################
      #### ---------  Exploratory Bubble Content  --------- ####
      ##########################################################
      
      
      tabItem(tabName = "exploratory",
              
            fluidRow(
              tabBox(width = 9, side="right", selected = shiny::icon("circle"),
                     ######## OPTIONS
                     ##Units - Axis Log
                     tabPanel(shiny::icon("gear"),
                              fluidRow(
                                box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                    
                                    ##Units - X Axis Log
                                    column(6, selectInput("log_bubble_x", label = "X Axis",
                                                          choices = c("linear", "log"), 
                                                          selected = "linear")),  
                                    ##Units - Y Axis Log
                                    column(6, selectInput("log_bubble_y", label = "Y Axis",
                                                          choices = c("linear", "log"), 
                                                          selected = "linear")))),
                              fluidRow(
                                box(width = 12, title = "Animation", collapsible = TRUE, collapsed = TRUE, 
                                    ##Leave trace
                                    column(12, checkboxInput("trace_bubble", label = ("Leave Trace"),
                                                             value = TRUE)),
                                    ##Animation Speed
                                    column(12, sliderInput("animation_speed_bubble", label = h4("Speed"),
                                                           min = 0.25,
                                                           max = 2, 
                                                           step = 0.25,
                                                           post = "x",
                                                           value = 1))))),
                     
                     ######## PLOT
                     tabPanel(shiny::icon("circle"),
                              div(class = "titleRow", fluidRow(column(5, tags$h2("Exploratory Bubble Graph")), 
                                 ##Granularity
                                 column(4, offset = 2,selectInput("granularity_bubble", label = "",
                                                                  choices = granularity,
                                                                  selected = "year")))),
              
              
                    ## Main Plot Area
                      #Solutes Y Input
                      fluidRow(column(4, textInput("solutesy_formula", label = "", value = "Ca + Na + Mg", 
                                                   placeholder = "type in desired formula")),
                               column(2, selectInput("solutesy_source", label = "", choices = c("P" = "precipitation", "Q" = "streamflow"),
                                                                 selected = "streamflow")), 
                               column(1, offset = 4, actionButton("go", "Plot"))
                      ),
                      #Bubble Plot
                      fluidRow(plotlyOutput("bubblePlot")),
                      
                      #Solutes X Input
                      fluidRow(column(4, offset = 3, textInput("solutesx_formula", label = "", value = "SO4 + NO3", 
                                                               placeholder = "type in desired formula")), 
                              column(2, offset = 0, selectInput("solutesx_source", label = "", choices = c("P" = "precipitation", "Q" = "streamflow"),
                                                                selected = "streamflow"))
                      ))),
                    
                    ## Sidebar Area
                    box(width = 3,
                        ##Watersheds
                        fluidRow(
                          column(12, actionLink("select_all_ws", h4("Watersheds")), 
                                 selectInput("watersheds_bubble", label = "",
                                             choices = watersheds, multiple = TRUE,
                                             selected = "6"))),
                        ##Units  
                        fluidRow(
                          column(12, selectInput("units_bubble", label = h4("Units"),
                                                 choices = c(units, "flux"),
                                                 selected = "uEquivalent/L"))),
                  
                        ##Date Range
                        fluidRow(
                          column(12, sliderInput("date_range_bubble", label = h4("Date Range"),
                                                 min = as.Date("1962-01-01"),
                                                 max = as.Date("2014-01-01"),
                                                 value = c(as.Date("1962-01-01"), as.Date("2014-01-01"))))),
                        
                        ##Sizing
                        fluidRow(
                          column(12, selectInput("sizing_bubble", label = h4("Bubble Size"),
                                                 choices = c("None" = 10, 
                                                             "Precipitation (P)" = "water_mm_precipitation", 
                                                             "Streamflow (Q)" = "water_mm_streamflow"))))
                    
              )#Closes Sidebar Box 
            ) #Closes Fluid Row
          )#Closes Exploratory Bubble tab
      
      ##########################################################
      #### ------  End of Exploratory Bubble Content  ----- ####
      ##########################################################
      
      )#Closes TabItems
    )#Closes Dashboard Body
  )#Closes dashboardPage
) #Closes ShinyUI

  