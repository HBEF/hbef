library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)
library(directlabels)
library(shinydashboard)


########### IMPORTANT LISTS ############


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

solutes_NO3 <- list("Nitrate (NO3)" = "NO3")

watersheds <- list("Watershed 1" = "1",
                   "Watershed 2" = "2", 
                   "Watershed 3" = "3",
                   "Watershed 4" = "4",
                   "Watershed 5" = "5",
                   "Watershed 6" = "6",
                   "Watershed 7" = "7",
                   "Watershed 8" = "8",
                   "Watershed 9" = "9")

watersheds1 <- list("Watershed 1" = "1",
                    "Watershed 6" = "6")

water_sources <- list("Precipitation (P)" = "precipitation",
                      "Discharge (Q)" = "streamflow")

granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month",
                    "Week" = "week")
granularity3 <- list("Year (VWC)" = "year",
                     "Month (VWC)" = "month",
                     "Week" = "week")

units <- list("uEquivalent/L","uMole/L", "mg/L", "flux")

units3 <- list("uEquivalent/L","uMole/L", "mg/L", "flux", "normalized_flux")

units_lai <- list("meterSquaredPerMeterSquared", ("NO^2"))

units_flux <- list("flux")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################

shinyUI(  dashboardPage(skin = "black",
                        dashboardHeader(title = "Ice Storm"),
                        dashboardSidebar(
                          width = 150,
                          sidebarMenu(
                            menuItem("LAI", tabName = "LAI", icon = icon("leaf")),
                            menuItem("NO3 General", tabName = "General", icon = icon("line-chart")),
                            menuItem("Flux", tabName = "Flux", icon = icon("arrows-v"))
                          )
                        ),
                        dashboardBody(
                          ########### HEAD - DO NOT EDIT ################################################
                          tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
                          tags$head(includeScript(system.file('www', 'ajax.js'))),
                          tags$head(includeScript(system.file('www', 'hubbard.js'))),
                          tags$head(tags$style(HTML(
                            "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
                          ###############################################################################
                          
                          ########### BODY ##############################################################
                          
                          tabItems(
                            
                            
                            ###############################################################################
                            #### ------------  LAI  Tab ------------------------------------------- #######
                            ###############################################################################
                            
                            tabItem(tabName = "LAI",
                                    
                                    ########### TITLE ####################
                                    fluidRow(tags$div(class = "container_question",
                                                      tags$h3("How do ice storms affect vegetation?"))
                                    ),
                                    
                                    #############################################
                                    
                                    ########### GRAPH FOR QUESTION #1 ##########
                                    
                                    fluidRow(
                                      column(9,
                                             tabBox(width = 12, height = "600px", side="right", selected = shiny::icon("circle"),
                                                    ######## OPTIONS
                                                    ###Units - Axis Log
                                                    tabPanel(shiny::icon("gear"),
                                                             fluidRow(
                                                               box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                                                   
                                                                   ##Units - Y Axis Log
                                                                   column(6, selectInput("log1", label = "Y Axis",
                                                                                         choices = c("linear", "log"), 
                                                                                         selected = "linear"))))),
                                                    ######## PLOT 
                                                    tabPanel(shiny::icon("circle"),
                                                             div(class = "titleRow", fluidRow(column(5, tags$h2("LAI")),
                                                                                              ##Watershed
                                                                                              column(3,  offset = 3,
                                                                                                     fluidRow(
                                                                                                       selectInput("watersheds1", label = "",
                                                                                                                   choices = watersheds1,
                                                                                                                   selected = "1"))))
                                                             ),
                                                             ## Plot
                                                             plotlyOutput("lai_plot")
                                                    ) #Closes tabpanel
                                                    
                                             )# Closes tab Box
                                             
                                      ) #Closes the column
                                      
                                      ######## SIDEBAR
                                      
                                    ),#Closes graph row
                                    
                                    ########### END OF GRAPH FOR QUESTION #1 ##########
                                    
                                    ########### TEXT FOR QUESTION #1 ##########
                                    
                                    tags$div(class = "",
                                             fluidRow(column(width = 9,
                                                             p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  The leaf area index
                                       (LAI) is one way to track the regrowth of the canopy."))))
                                    ########### END OF QUESTION #1 ##########
                            ), # Closes Intro Tab
                            
                            ###############################################################################
                            #### ------------  End of LAI Tab ------------------------------------- #######
                            ###############################################################################
                            
                            
                            
                            ###############################################################################
                            #### ------------  NO3  General  Tab ---------------------------------- #######
                            ###############################################################################
                            
                            tabItem(tabName = "General",
                                    
                                    ########### TITLE ####################
                                    fluidRow(tags$div(class = "container_question", 
                                                      tags$h3("How do NO3 concentrations change following an ice storm?")) 
                                    ),
                                    
                                    #############################################
                                    
                                    ########### GRAPH FOR QUESTION #1 ##########
                                    
                                    fluidRow(
                                      column(9,
                                             tabBox(width = 12, height = "600px", side="right", selected = shiny::icon("circle"),
                                                    ######## OPTIONS
                                                    ###Units - Axis Log
                                                    tabPanel(shiny::icon("gear"),
                                                             fluidRow(
                                                               box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                                                   
                                                                   ##Units - Y Axis Log
                                                                   column(6, selectInput("log", label = "Y Axis",
                                                                                         choices = c("linear", "log"), 
                                                                                         selected = "linear"))))),
                                                    ######## PLOT 
                                                    tabPanel(shiny::icon("circle"),
                                                             div(class = "titleRow", fluidRow(column(5, tags$h2("Water Chemistry")),
                                                                                              ##Granularity
                                                                                              column(3,  offset = 4, selectInput("granularity", label = "",
                                                                                                                                 choices = granularity,
                                                                                                                                 selected = "month")))),
                                                             ## Time Plot
                                                             plotlyOutput("NO3_plot")
                                                    ) #Closes tabpanel
                                                    
                                             )# Closes tab Box
                                             
                                      ), #Closes the column
                                      
                                      ######## SIDEBAR
                                      column(3, 
                                             box(width = 13, height = "600px", id = "sidebar",
                                                 
                                                 ##Watersheds
                                                 fluidRow(
                                                   column(12, h4("Watersheds"), 
                                                          selectInput("watersheds2", label = "",
                                                                      choices = watersheds, multiple = T,
                                                                      selected = "6"))),
                                                 
                                                 ##Water Sources
                                                 fluidRow(
                                                   column(12, checkboxGroupInput("water_sources2", label = h4("Water Sources"),
                                                                                 choices = water_sources,
                                                                                 selected = "streamflow"))),
                                                 
                                                 ##Units  
                                                 fluidRow(
                                                   column(12, selectInput("units", label = h4("Units"),
                                                                          choices = units,
                                                                          selected = "mg/L"))),
                                                 
                                                 ##Date Range
                                                 sliderInput("date_range2", label = h4("Date Range"),
                                                             min = as.Date("1962-01-01"),
                                                             max = as.Date("2014-01-01"),
                                                             value = c(as.Date("1997-01-01"), as.Date("2001-01-01")),
                                                             timeFormat = "%b %Y"))
                                             
                                      )#Closes the column
                                      
                                    ),#Closes graph row
                                    
                                    
                                    ########### END OF GRAPH FOR QUESTION #1 ##########
                                    
                                    
                                    ########### TEXT FOR QUESTION #1 ##########
                                    
                                    tags$div(class = "",
                                             fluidRow(column(width = 9,
                                                             p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  Some effects of the 
                                       storm can be tracked by the NO3 streamflow data."))))
                                    
                                    ########### END OF QUESTION #1 ##########
                            ),# Closes Intro Tab
                            
                            ###############################################################################
                            #### ------------ End of NO3 General Tab ------------------------------ #######
                            ###############################################################################  
                            
                            
                            ###############################################################################
                            #### ------------  Flux Tab  ---------------------------------------- #######
                            ###############################################################################
                            
                            tabItem(tabName = "Flux",
                                    
                                    ########### TITLE ####################
                                    fluidRow(tags$div(class = "container_question", 
                                                      tags$h3("Post-ice storm NO3 flux Plots Replication (Bernhardt, 2003)")) 
                                    ),
                                    
                                    #############################################
                                    
                                    ########### GRAPH FOR QUESTION #1 ##########
                                    fluidRow(
                                      column(9,
                                             #------ Box 1 --------#
                                             tabBox(width = 12, height = "600px", side="right", selected = shiny::icon("circle"),
                                                    ######## OPTIONS
                                                    ###Units - Axis Log
                                                    tabPanel(shiny::icon("gear"),
                                                             fluidRow(
                                                               box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE 
                                                                   ))),
                                                    ######## PLOT 
                                                    tabPanel(shiny::icon("circle"),
                                                             div(class = "titleRow", fluidRow(column(9, tags$h2("Streamflow flux (ws1, ws6)")))),
                                                                                              ## Time Plot
                                                                                              plotlyOutput("NO3_output")
                                                             ) #Closes tabpanel
                                                             
                                                             ),# Closes tab Box
                                                             
                                                             #------ End of Box 1 --------#
                                                             
                                                             
                                                             #------ Box 3 --------#
                                                             
                                                             tabBox(width = 12, height = "600px", side="right", selected = shiny::icon("circle"),
                                                                    ######## OPTIONS
                                                                    ###Units - Axis Log
                                                                    tabPanel(shiny::icon("gear"),
                                                                             fluidRow(
                                                                               box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE
                                                                                   ))),
                                                                    ######## PLOT 
                                                                    tabPanel(shiny::icon("circle"),
                                                                             div(class = "titleRow", fluidRow(column(9, tags$h2("Streamflow flux normalized against ws6 (ws2, ws4, ws5)")))),
                                                                             ## Time Plot
                                                                             plotlyOutput("NO3_excess")
                                                                    ) #Closes tabpanel
                                                                    
                                                             )# Closes tab Box
                                                             
                                                             
                                                    ), #Closes the column
                                                    
                                                    ######## SIDEBAR
                                                    column(3, 
                                                           box(width = 13, height = "600px", id = "sidebar",
                                                               
                                                               ##Granularity
                                                               fluidRow(
                                                                 column(12, selectInput("granularity3", label = h4("Granularity"),
                                                                                        choices = granularity,
                                                                                        selected = "year"))),
                                                               ##Date Range
                                                               sliderInput("date_range3", label = h4("Date Range"),
                                                                           min = as.Date("1962-01-01"),
                                                                           max = as.Date("2014-01-01"),
                                                                           value = c(as.Date("1963-01-01"), as.Date("2004-01-01")),
                                                                           timeFormat = "%b %Y"))
                                                           
                                                    )#Closes the column
                                                    
                                             ),#Closes graph row
                                             
                                             ########### END OF GRAPH FOR QUESTION #1 ##########
                                             
                                             ########### TEXT FOR QUESTION #1 ##########
                                             
                                             tags$div(class = "",
                                                      fluidRow(column(width = 9,
                                                                      p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  Some effects of the 
                                       storm can be tracked by the NO3 flux data."))))
                                             
                                             ########### END OF QUESTION #1 ##########
                                      )# Closes Intro Tab
                                      
                                      ###############################################################################
                                      #### ------------  End of Flux Tab ------------------------------- #######
                                      ###############################################################################    
                                      
                                    )# Closes Tabset Panel for Main Tabs
                            )#Closes Dashboard Body
                          )#closes FluidPage
                        ) #closes ShinyUI
                        
                        