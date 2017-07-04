library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)
library(directlabels)


########### IMPORTANT LISTS #############


###  Lists for the sidebar  ###
#Edit if there are values that do not appear or are not relevant to your data. 

solutes_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Aluminum (Al)" = "Al")

solutes_anions <- list("Sulfate (SO4)" = "SO4",
                       "Nitrate (NO3)" = "NO3",
                       "Chlorine (Cl)" = "Cl")
solutes_H <- c("Hydrogen (H)" = "H")

watersheds <- list("Watershed 1" = 1,
                   "Watershed 2" = 2, 
                   "Watershed 3" = 3,
                   "Watershed 4" = 4,
                   "Watershed 5" = 5,
                   "Watershed 6" = 6,
                   "Watershed 7" = 7,
                   "Watershed 8" = 8,
                   "Watershed 9" = 9)

water_sources <- list("Add Precipitation" = "precip",
                      "No Precipitation" = "flow")

granularity <- list("Year" = "year",
                    "Month" = "month")

units <- list("ueq/L","umol/L", "umg/L")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################

shinyUI(fluidPage(
  
  ########### HEAD - DO NOT EDIT ################################################
  theme = "app.css",
  tags$head(includeScript(system.file('www', 'ajax.js'))),
  tags$head(includeScript(system.file('www', 'hubbard.js'))),
  tags$head(tags$style(HTML(
    "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
  ###############################################################################
  
  ########### BODY ##############################################################
 tabsetPanel(id = "top", type = "pills",
   tabPanel("Compare Watersheds",
            titlePanel("Dilutification of Streamwater"),
            fluidRow( 
              tags$h3("How are solute concentrations in 
                      streamwater changing over time?")
              ),
            sidebarLayout(position = "right",
              sidebarPanel(
                           #Solutes
                           fluidRow(
                             column(12, 
                                    selectInput("sol", label = "Solutes",
                                                choices = 
                                                  c(solutes_anions, solutes_cations,
                                                    solutes_H),
                                                selected = "Na"))),
                           
                           
                           ##Watersheds
                           fluidRow(
                             column(12, actionLink("select_all_ws", h4("Watersheds")), 
                                    selectInput("watersheds", label = "",
                                                choices = watersheds, multiple = TRUE,
                                                selected = 6))),
                           
                           ##Water Sources
                           fluidRow(
                             column(12, selectInput("water_sources", 
                                                    label = h4("Adding Precipitation"),
                                                    choices = water_sources,
                                                    selected = "flow"))),
                           
                           ##Units  
                           fluidRow(
                             column(12, selectInput("units", label = h4("Units"),
                                                    choices = units,
                                                    selected = "ueq/L"))),
                           fluidRow(h4("Applying the Natural Logarithm"),
                                    column(12, checkboxInput("log", label = ("ln"),
                                                             value = FALSE))),
                           ##Granularity
                           fluidRow(
                             column(12, selectInput("granularity", label = h4("Granularity"),
                                                    choices = granularity,
                                                    selected = "year"))),
                           
                           ##Date Range
                           sliderInput("date_range", label = h4("Date Range"),
                                       min = as.Date("1962-01-01"),
                                       max = as.Date("2014-01-01"),
                                       value = c(as.Date("1965-01-01"), as.Date("2013-01-01")))),
                           
                         mainPanel(fluidRow(
                           column(8, plotlyOutput("plot1", width = "100%", height = "100%")),
                           column(4, img(src = "legend.png", height = 200, width = 200))))
            ),#End of sidebarLayout
                          
                        
                           
                           
                           
                         
            fluidRow(
              column(10, offset = 1,
              tags$h3("Summary of Dilutification"),
              tags$p("Over time, aquatic organisms have physiologically
                     adapted to certain ionic conditions in the 
                     streamwater they inhabit. In these organisms,
                     as in all organisms, an osmotic balance at the
                     cellular level is controlled by ion concentrations.
                     When streamwater ion concentrations change 
                     drastically, therefore, the cellular osmotic 
                     balance is disrupted, causing the cells of aquatic
                     organisms to swell and burst or shrivel and shrink
                     depending on the ionic concentration differential between
                     the cells and the outside environment. Changing ion
                     concentrations in streamwater can thus lead to major
                     die-offs of aquatic organisms."),
              tags$p("For unknown reasons, the streamwater at Hubbard Brook 
                     is becoming increasingly diluted and deionized. 
                     The concentrations of most nutrients in streamwater
                     are diminishing as time goes on. One can visualize
                     this phenomenon with the data tools in this module.
                     Best-fit lines are added to the data to show the 
                     overall decreasing trend in ion concentration.
                     Select any ion (or solute) and watershed to see how
                     the concentrations are changing.")),
              column(2)
              )#Closes text fluid row
   
   ),#End of tabPanel
   tabPanel("Compare Solutes",
            titlePanel("Dilutification of Streamwater"),
            fluidRow( 
              tags$h3("How are solute concentrations in 
                      streamwater changing over time?")
              ),
            sidebarLayout(position = "right",
                          sidebarPanel(
                            #Solutes
                            fluidRow(
                              column(12, actionLink("select_all_ions", h4("Solutes"))),
                              
                              #Cations
                              column(6,
                                     actionLink("select_all_cations", h5("Cations")),
                                     checkboxGroupInput("solutes_cations", label = "",
                                                        choices = solutes_cations,
                                                        selected = "Na")),
                              
                              #Anions
                              
                              column(6, actionLink("select_all_anions", h5("Anions")),
                                     checkboxGroupInput("solutes_anions", label = "",
                                                        choices = solutes_anions,
                                                        selected = "SO4)"))),
                            #Hydrogen  
                            
                            fluidRow(
                              column(12, checkboxGroupInput("solutes_H", 
                                                            label = h5("Hydrogen"),
                                                            choices = solutes_H,
                                                            selected = ""))),
                            
                            ##Watersheds
                            fluidRow(
                              column(12, 
                                     selectInput("watersheds2", label = h4("Watersheds"),
                                                 choices = watersheds, multiple = TRUE,
                                                 selected = 6))),
                            
                            ##Water Sources
                            fluidRow(
                              column(12, selectInput("water_sources2", 
                                                     label = h4("Adding Precipitation"),
                                                     choices = water_sources,
                                                     selected = "flow"))),
                            
                            ##Units  
                            fluidRow(
                              column(12, selectInput("units2", label = h4("Units"),
                                                     choices = units,
                                                     selected = "ueq/L"))),
                            fluidRow(h4("Applying the Natural Logarithm"),
                              column(12, checkboxInput("log2", label = ("ln"),
                                                       value = FALSE))),
                            ##Granularity
                            fluidRow(
                              column(12, selectInput("granularity2", label = h4("Granularity"),
                                                     choices = granularity,
                                                     selected = "year"))),
                            
                            ##Date Range
                            sliderInput("date_range2", label = h4("Date Range"),
                                        min = as.Date("1962-01-01"),
                                        max = as.Date("2014-01-01"),
                                        value = c(as.Date("1965-01-01"), as.Date("2013-01-01")))
                            
                            
                          ),#End of sidebarPanel 
                          
                          mainPanel(
                            fluidRow(
                              column(8, plotlyOutput("plot2", width = "100%", height = "100%")),
                              column(4, img(src = "legend.png",
                                            height = 200, width = 200)))
                          
                            
                            
                          )#End of mainPanel
            ),#End of sidebarLayout
            fluidRow(
              column(10, offset = 1,
              tags$h3("Summary of Dilutification"),
              tags$p("Over time, aquatic organisms have physiologically
                     adapted to certain ionic conditions in the 
                     streamwater they inhabit. In these organisms,
                     as in all organisms, an osmotic balance at the
                     cellular level is controlled by ion concentrations.
                     When streamwater ion concentrations change 
                     drastically, therefore, the cellular osmotic 
                     balance is disrupted, causing the cells of aquatic
                     organisms to swell and burst or shrivel and shrink
                     depending on the ionic concentration differential between
                     the cells and the outside environment. Changing ion
                     concentrations in streamwater can thus lead to major
                     die-offs of aquatic organisms."),
                    tags$p("For unknown reasons, the streamwater at Hubbard Brook 
                           is becoming increasingly diluted and deionized. 
                           The concentrations of most nutrients in streamwater
                           are diminishing as time goes on. One can visualize
                           this phenomenon with the data tools in this module.
                           Best-fit lines are added to the data to show the 
                           overall decreasing trend in ion concentration.
                           Select any ion (or solute) and watershed to see how
                           the concentrations are changing.")
                  ),
              column(2))#Closes text fluid row
            
            )#End of tabPanel
 )#End of tabsetPanel
))#End of shinyUI and fluidPage