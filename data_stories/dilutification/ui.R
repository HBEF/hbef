library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)



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
                       "Chloride (Cl)" = "Cl")
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

granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month")

units <- list("ueq/L" = "ueq/L","umol/L" ="umol/L", "mg/L" = "mg/L")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################

shinyUI(dashboardPage(skin = "black",
                      dashboardHeader(title = tags$a(href="http://vcm-192.vm.duke.edu/","HB-WER Viz"), titleWidth = 200),
                      dashboardSidebar(
                        width = 200,
                        sidebarMenu(
                          menuItem("By Watershed", tabName = "watersheds", icon = icon("home")),
                          menuItem("By Solute", tabName = "solutes", icon = icon("search-plus")),
                        # footer here
                          tags$div(class = "footer",tags$ul(
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#menu", "HOME")),
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#datastories","DATA STORIES")),
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#exploratory","EXPLORATORY TOOLS")),
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#aboutus","ABOUT US")))
                        ))
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
    tabItem(tabName = "watersheds", 
            fluidRow(column(9,tags$h2("Dilutification of Streamwater"))),
            fluidRow(column(9, tags$div(class = "container_question",
              tags$h1("How are solute concentrations in 
                      streamwater changing over time?")))
              ),
            
            fluidRow(column(9,
                            tabBox(width = 12, height = "1000px", side="right", 
                                   selected = shiny::icon("circle"),
                                   ###Units - Axis Log
                                   tabPanel(shiny::icon("gear"),
                                            fluidRow(
                                              box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                                  
                                                  ##Units - Y Axis Log
                                                  column(6, selectInput("log1", label = "Y Axis",
                                                                        choices = c("linear", "log"), 
                                                                        selected = "linear"))))),
                                  tabPanel(shiny::icon("circle"),
                                   div(class = "titleRow", fluidRow(column(5, tags$h2("")),
                                                                    ##Granularity
                                                                    column(3,  offset = 4, selectInput("granularity", label = "",
                                                                                                       choices = granularity,
                                                                                                       selected = "year")))
                                   ),
                                   
                                   fluidRow(
                                    column(12, plotlyOutput("plot1", width = "100%", height = "100%")))
                                  ))),
            column(3,
              box(width = 13, height = "700px", id = "sidebar",
                           #Solutes
                           fluidRow(
                             column(12, 
                                    selectInput("sol", label = h4("Solutes"),
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
                             column(12, 
                             checkboxGroupInput("water_sources", label = h4("Adding Precipitation"),
                                                choices = list("Streamflow" = "streamflow",
                                                               "Precipitation" = "precipitation"),
                                                selected = "streamflow"))
                             ),
                           
                           ##Units  
                           fluidRow(
                             column(12, selectInput("units", label = h4("Units"),
                                                    choices = units,
                                                    selected = "ueq/L"))),
                           
                           ##Date Range
                           sliderInput("date_range", label = h4("Date Range"),
                                       min = as.Date("1962-01-01"),
                                       max = as.Date("2014-01-01"),
                                       value = c(as.Date("1965-01-01"), as.Date("2013-01-01"))))

            )#end of sidebar column
    
   
   )#End of fluidRow
    ), #end of tabItem
   tabItem(tabName = "solutes",
            fluidRow(column(9,tags$h2("Dilutification of Streamwater"))),
            fluidRow(column(9,tags$div(class = "container_question", 
              tags$h1("How are solute concentrations in 
                      streamwater changing over time?")))
              ),
           fluidRow(column(9,
           tabBox(width = 12, height = "600px", side="right", 
                  selected = shiny::icon("circle"),
                  ###Units - Axis Log
                  tabPanel(shiny::icon("gear"),
                           fluidRow(
                             box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                 
                                 ##Units - Y Axis Log
                                 column(6, selectInput("log2", label = "Y Axis",
                                                       choices = c("linear", "log"), 
                                                       selected = "linear"))))),
                  tabPanel(shiny::icon("circle"),
                           column(3, offset = 9, selectInput("granularity2", label = h4("Granularity"),
                                                             choices = granularity,
                                                             selected = "year")),
                           fluidRow(
                             column(12, plotlyOutput("plot2", width = "100%", height = "100%")))
                  ))),
            column(3,
                      box(width = 13, height = "1100px", id = "sidebar",
                            #Solutes
                            fluidRow(
                              column(12, actionLink("select_all_ions", h4("Solutes")),
                              
                              #Cations
                              
                                     actionLink("select_all_cations", h5("Cations")),
                                     checkboxGroupInput("solutes_cations", label = "",
                                                        choices = solutes_cations,
                                                        selected = "Na"),
                              
                              #Anions
                              
                                     actionLink("select_all_anions", h5("Anions")),
                                     checkboxGroupInput("solutes_anions", label = "",
                                                        choices = solutes_anions,
                                                        selected = "SO4"))),
                            #Hydrogen  
                            
                            fluidRow(
                              column(12, actionLink("select_all_H", h5("Hydrogen")),
                                     checkboxGroupInput("solutes_H", 
                                                            label = "",
                                                            choices = solutes_H,
                                                            selected = ""))),
                            
                            ##Watersheds
                            fluidRow(
                              column(12, 
                                     selectInput("watersheds2", label = h4("Watersheds"),
                                                 choices = watersheds,
                                                 selected = 6))),
                            
                            ##Water Sources
                            fluidRow(
                              column(12, 
                                     checkboxGroupInput("water_sources2", label = h4("Adding Precipitation"),
                                                        choices = list("Streamflow" = "streamflow",
                                                                       "Precipitation" = "precipitation"),
                                                        selected = "streamflow"))),
                            
                            ##Units  
                            fluidRow(
                              column(12, selectInput("units2", label = h4("Units"),
                                                     choices = units,
                                                     selected = "ueq/L"))),
                          
                            
                            ##Date Range
                           fluidRow(column(12,
                            sliderInput("date_range2", label = h4("Date Range"),
                                        min = as.Date("1962-01-01"),
                                        max = as.Date("2014-01-01"),
                                        value = c(as.Date("1965-01-01"), as.Date("2013-01-01")))))
                            
                            
                          )#End of box 
                          
                    
            )#End of column
            
            )#End of fluidRow
 )#End of tabItem
)#End of tabItems
)#End of dashboard body
)#End of dashboardPage
)#End of shinyUI