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
solutes_H <- list("Hydrogen (H)" = "H")

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

water_sources <- list("Precipitation (P)" = "precipitation",
                     "Streamflow (Q)" = "streamflow")

granularity <- list( "Week" = "week",
                    "Month" = "month",
                    "Year" = "year")

time_variables <- list("concentration" = "concentration", 
                       "pH" = "pH",
                       "temperature" = "temp", 
                       "acid-neutralizing capacity" = "anc", 
                       "dic" = "dic",
                       "specific conductivity" = "spcond")
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
          #### Main area 
          box(width = 9, height = "600px", status = "primary", id = "pq",
              div(class = "titleRow", fluidRow(
              column(9, tags$h2("Hydrologic Flux")),
              ##Granularity
              column(2, selectInput("granularity", label = "",
                                     choices = granularity,
                                     selected = "year")))),
                
              ##PQ plot
              fluidRow(plotlyOutput("plot_pq")),
              
              ##Units - Y Axis Log
              fluidRow(column(2, offset = 9, selectInput("log_pq", label = "Y Axis",
                                                         choices = c("linear", "log"), 
                                                         selected = "linear")))
          )
        
          #### Sidebar area
          #box(width = 2, height = ) #Closes sidebar box
        
        ), #Closes PQ Graph Row 
      
        ###  ---- PQ GRAPH END -----
        
        
        fluidRow(
        ###  ---- TIME GRAPH
        #### Main area
        column(9,   
        box(width = 13, height = "600px", id = "time",
              div(class = "titleRow", fluidRow(
                column(9, tags$h2("Time Data")),
                ##Granularity
                column(2,selectInput("granularity_time", label = "",
                                      choices = granularity,
                                      selected = "year")))
              ),#Closes div
              ## Time graph Plot
              plotlyOutput("plot_time"),
              
              ##Units - Y Axis Log
              fluidRow(column(2, offset = 9, selectInput("log_time", label = "Y Axis",
                                                         choices = c("linear", "log"), 
                                                         selected = "linear")))
          ),
        
        
        fluidRow(
          ###  ---- CQ GRAPH
          #### Main area CQ Graph
          box(width = 6,
              div(class = "titleRow", fluidRow(
                column(5, tags$h2("CQ")),
              fluidRow(
                ##Granularity
                column(4, offset = 2,selectInput("granularity_cq", label = "",
                                                 choices = granularity,
                                                 selected = "year")))
              )),#Closes Row
              ## CQ plot
              plotlyOutput("plot_cq"),
              ##Units - Y Axis Log
              fluidRow(column(4, offset = 7, selectInput("log_cq_y", label = "Y Axis",
                                                         choices = c("linear", "log"), 
                                                         selected = "linear"))),
              ##Units - X Axis Log
              fluidRow(column(4, offset = 7, selectInput("log_cq_x", label = "X Axis",
                                                         choices = c("linear", "log"), 
                                                         selected = "linear")))
          ),
          
          ###  ---- FLUX GRAPH
          #### Main area Flux Graph  
          box(width = 6,
              fluidRow(
                div(class = "titleRow", fluidRow(
                  column(5, tags$h2("Flux")),
                ##Granularity
                column(4,  offset = 2, selectInput("granularity_cq", label = "",
                                                   choices = granularity,
                                                   selected = "year")))
              )),#Closes Row
              ## Flux Plot
              plotlyOutput("plot_flux"),
              ##Units - Y Axis Log
              fluidRow(column(4, offset = 7, selectInput("log_flux", label = "Y Axis",
                                                         choices = c("linear", "log"), 
                                                         selected = "linear")))
          ) #Closes Box
          
        ) #Closes CQ and Flux Row
        
        ),
        
        column(3,
        
          #### Side Bar Area
          box(width = 13, height = "1000px", id = "sidebar",
              #Y Axis
              fluidRow(column(12,
                              selectInput("yaxis_time", label = "",
                                          choices = time_variables,
                                          selected = "flux"))),
              ##Units  
              fluidRow(
                column(12, conditionalPanel(condition = "input.yaxis_time == 'concentration'",
                       selectInput("units", label = h4("Units"),
                                       choices = units,
                                       selected = "uEquivalent/L"))),
              
              #Solutes
            
                column(12, conditionalPanel(condition = "input.yaxis_time == 'concentration'", 
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
                                              selected = "")))),
              ##Date Range
              fluidRow(
                column(12, sliderInput("date_range", label = h4("Date Range"),
                                       min = as.Date("1962-01-01"),
                                       max = as.Date("2014-01-01"),
                                       value = c(as.Date("1965-01-01"), as.Date("2013-01-01"))))), 
              ##Leave trace
              fluidRow(
                column(12, checkboxInput("trace", label = ("Leave Trace"),
                                         value = TRUE))),
              ##Animation Speed
              fluidRow(
                column(12, sliderInput("animation_speed", label = h4("Speed"),
                                       min = 0.25,
                                       max = 2, 
                                       step = 0.25,
                                       post = "x",
                                       value = 1)))
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
              ## Main Plot Area
              box(width = 9, height = "800px",
                #Solutes Y Input
                fluidRow(column(6, textInput("solutesy_formula", label = "", value = "Ca + Na + Mg", 
                                             placeholder = "type in desired formula"))),
                #Bubble Plot
                fluidRow(div(style = "height:600px;",
                             plotlyOutput("bubblePlot"))),
                
                #Solutes X Input
                fluidRow(column(6, offset = 3, textInput("solutesx_formula", label = "", value = "SO4 + NO3", 
                                                         placeholder = "type in desired formula"))),
                ##Units - Y Axis Log
                fluidRow(column(2, offset = 9, selectInput("log_bubble_y", label = "Y Axis",
                                                           choices = c("linear", "log"), 
                                                           selected = "linear"))),
                ##Units - X Axis Log
                fluidRow(column(2, offset = 9, selectInput("log_bubble_x", label = "X Axis",
                                                           choices = c("linear", "log"), 
                                                           selected = "linear")))
                ),
              
              ## Sidebar Area
              box(width = 3,
                  ##Watersheds
                  fluidRow(
                    column(12, actionLink("select_all_ws", h4("Watersheds")), 
                           selectInput("watersheds_bubble", label = "",
                                       choices = watersheds, multiple = TRUE,
                                       selected = "6"))),
                  ##Water Sources
                  fluidRow(
                    column(12, checkboxGroupInput("water_sources_bubble", label = h4("Water Sources"),
                                                  choices = water_sources,
                                                  selected = "streamflow",
                                                  inline = TRUE))),
                  ##Units  
                  fluidRow(
                    column(12, selectInput("units_bubble", label = h4("Units"),
                                           choices = units,
                                           selected = "uEquivalent/L")),
                    column(12, checkboxInput("log_bubble", label = ("ln"),
                                             value = FALSE))),
                  ##Granularity
                  fluidRow(
                    column(12, selectInput("granularity_bubble", label = h4("Granularity"),
                                           choices = granularity,
                                           selected = "year"))),
                  ##Date Range
                  fluidRow(
                    column(12, sliderInput("date_range_bubble", label = h4("Date Range"),
                                           min = as.Date("1962-01-01"),
                                           max = as.Date("2014-01-01"),
                                           value = c(as.Date("1965-01-01"), as.Date("2013-01-01"))))), 
                  ##Leave trace
                  fluidRow(
                    column(12, checkboxInput("trace_bubble", label = ("Leave Trace"),
                                             value = TRUE))),
                  ##Leave trace
                  fluidRow(
                    column(12, sliderInput("animation_speed_bubble", label = h4("Speed"),
                                           min = 0.25,
                                           max = 2, 
                                           step = 0.25,
                                           post = "x",
                                           value = 1)))
                    
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

  