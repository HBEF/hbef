library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)
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
solutes_H <- list("Hydrogen (H)" = "H",
                  "pH" = "pH")

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
        menuItem(" ", tabName = "widget", icon = icon("search-plus"))
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
        # First tab content
        tabItem(tabName = "dashboard",
      
        box(width = 13,
        ##Watersheds
        
          #column(3, actionLink("select_all_ws",  h4("Watersheds"))),
          column(5, selectInput("watersheds", label = "",
                                choices = watersheds, multiple = TRUE,
                                selected = "8"))),
                
      
      fluidRow(
        box(width = 10, height = "400px",
            fluidRow(##Granularity
            column(2, offset = 9, selectInput("granularity", label = "",
                                   choices = granularity,
                                   selected = "week"))),
            
            verbatimTextOutput("zoom"),
          
          plotlyOutput("plot_pq")
          
          
        ),
        
        box(width = 2, 
    
          ##Units  
          fluidRow(
            column(12, checkboxInput("log", label = ("ln"),
                                     value = FALSE)))
        ) #Closes sidebar box
        
      ), #Closes Row #1 
      
      
      
      
      
      
      
      fluidRow(
        box(width = 10, height = "400px",
            fluidRow(
              ##Granularity
              column(2,  offset = 9,selectInput("granularity_time", label = "",
                                    choices = granularity,
                                    selected = "week"))
            ),#Closes Row
            plotlyOutput("plot_time")
            
        ),
        
        box(width = 2,
            
            #Solutes
            
            fluidRow(column(12,
                            selectInput("solutesy", label = "",
                                        choices = list("Cations" = solutes_cations, "Anions" = solutes_anions, "Hydrogen" = solutes_H),
                                        selected = "Na", 
                                        multiple = TRUE, 
                                        selectize = TRUE))),
            
            ##Units  
            fluidRow(
              column(12, selectInput("units", label = h4("Units"),
                                     choices = units,
                                     selected = "uEquivalent/L")),
              column(12, checkboxInput("log", label = ("ln"),
                                       value = FALSE))),
            
            
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
            
            ##Leave trace
            fluidRow(
              column(12, sliderInput("animation_speed", label = h4("Speed"),
                                     min = 0.25,
                                     max = 2, 
                                     step = 0.25,
                                     post = "x",
                                     value = 1)))
            
        ) #Closes sidebar box
        
      ), #Closes Row #2
      
      
      
      
      
      
      
      
      fluidRow(
        box(width = 6,
            fluidRow(
            ##Granularity
            column(4, offset = 7,selectInput("granularity_cq", label = "",
                                  choices = granularity,
                                  selected = "week"))
            ),#Closes Row
            plotlyOutput("plot_cq")),
        box(width = 6,
            fluidRow(
              ##Granularity
              column(4,  offset = 7, selectInput("granularity_cq", label = "",
                                    choices = granularity,
                                    selected = "week"))
            ),#Closes Row
            plotlyOutput("plot_flux")
            
        ) #Closes Box
        
      ) #Closes Row #3
      
      
      
        ),#Closes Tabitem 1
      
      tabItem(tabName = "widget",
              
            fluidRow(
              
              box(width = 9, height = "800px",
                #Solutes Y
                fluidRow(column(6,
                                textInput("solutesy_formula", label = "", value = "Ca + Na + Mg", placeholder = "type in desired formula"))),
                fluidRow(div(style = "height:600px;",
                  plotlyOutput("bubblePlot"))),
                
                #Solutes X
          
                fluidRow(column(6, offset = 3,
                                    textInput("solutesx_formula", label = "", value = "SO4 + NO3", placeholder = "type in desired formula")))
                         
                
                ),
              
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
                    
                  )#Closes Row
              
  
              
              ) #Closes Fluid Row
            
            )#Closes Tab 2 
      
        )#Closes TabItems
      
    )#Closes Dashboard Body
      
      
  )#closes dashboardPage
  
) #closes ShinyUI

  