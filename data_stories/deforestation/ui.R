library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard)


solutes_cations <- list("Potassium (K)" = "Potassium",
                        "Sodium (Na)" = "Sodium",
                        "Calcium (Ca)" = "Calcium",
                        "Magnesium (Mg)" = "Magnesium",
                        "Aluminum (Al)" = "Aluminum")

solutes_anions <- list("Sulfate (SO4)" = "Sulfate",
                       "Nitrate (NO3)" = "Nitrate",
                       "Chloride (Cl)" = "Chloride")
solutes_H <- c("Hydrogen (H)" = "Hydrogen Ion")

granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month")

# Application title
shinyUI(dashboardPage(skin = "black",
                      dashboardHeader(title = tags$a(href="http://vcm-192.vm.duke.edu/","HB-WER Viz"), titleWidth = 200),
                      dashboardSidebar(
                        width = 200,
                        sidebarMenu(
                          menuItem("Intro", tabName = "intro", icon = icon("home")),
                          menuItem("Concentration", tabName = "solutes", icon = icon("home")),
                          menuItem("Quantities", tabName = "streamflow", icon = icon("search-plus")),
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
  tags$head(includeScript(system.file('www','google_analytics.js'))),
  tags$head(tags$style(HTML(
    "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
  ###############################################################################
  
  ########### BODY ##############################################################
  
  tabItems(
    tabItem(tabName = "intro",
            
            #############################################
            
            ########### TEXT ###########
            
            #title and text to overlay intro image
            fluidRow(tags$div(class = "intro-text",
                              h1("understanding deforestation")
                              ))
            
            
            ########### END ###########
                              ),# Closes Intro Tab
    
              tabItem(tabName = "solutes",
                       fluidRow(column(9,tags$h1("Effects of Deforestation"))),
                       fluidRow(column(9, 
                         tags$div(class = "container_question",
                          tags$h1("What effect does deforestation have on 
                                 solute concentrations in streamwater?")))
                         ),
                      fluidRow(column(9,
                                      tabBox(width = 12, height = "600px", side="right", 
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
                                                        column(12, plotlyOutput("s.plot", width = "100%", height = "100%")))
                                                       
                                                        #column(3, img(src = "source.png", height = 75, width = 100)))
                                             ))),
                       column(3,
                                  box(width = 13, height = "1100px", id = "sidebar",
                                       #Solutes
                                       fluidRow(
                                         column(12, actionLink("select_all_ions", h3("Solutes"))),
                                         
                                         #Cations
                                         column(12,
                                                actionLink("select_all_cations", h5("Cations")),
                                                checkboxGroupInput("solutes_cations", label = "",
                                                                   choices = solutes_cations,
                                                                   selected = "Sodium")),
                                         
                                         #Anions
                                         
                                         column(12, actionLink("select_all_anions", h5("Anions")),
                                                checkboxGroupInput("solutes_anions", label = "",
                                                                   choices = solutes_anions,
                                                                   selected = ""))),
                                       #Hydrogen  
                                       
                                       fluidRow(
                                         column(12, actionLink("select_all_H", h5("Hydrogen")),
                                                checkboxGroupInput("solutes_H", 
                                                                       label = "",
                                                                       choices = solutes_H,
                                                                       selected = ""))),
                                       fluidRow(column(12,
                                                       selectInput("units", label = h3("Units"),
                                                                   choices = list("Eq/ha-yr" = "Eq/ha-yr",
                                                                                  "ueq/L" = "ueq/L",
                                                                                  "mg/L" = "mg/L",
                                                                                  "umol/L" = "umol/L"),
                                                                   selected = "ueq/L"))),
                                       
                                       fluidRow(column(12,
                                                       checkboxGroupInput("p", label = h3("Adding Precipitation"),
                                                                          choices = list("Streamflow" = "streamflow",
                                                                                         "Precipitation" = "precipitation"),
                                                                          selected = "streamflow"))),
                                      
                                       fluidRow(column(12, 
                                                       sliderInput("dates", label = h3("Date Range"),
                                                                   min = as.Date("1962-01-01"),
                                                                   max = as.Date("2014-01-01"),
                                                                   value = c(as.Date("1965-01-01"), as.Date("1985-01-01"))))))
                                     )#end of column
                       
                       
                                       )), #end of fluidRow
              tabItem(tabName = "streamflow",
                      fluidRow(column(9, tags$h1("Effects of Deforestation"))),
                      fluidRow(column(9,
                        tags$div(class = "container_question",
                                 tags$h1("What effect does deforestation have on 
                                 streamflow?")))
                      ),
                      fluidRow(column(9,
                                      tabBox(width = 12, height = "800px", side="right", 
                                             selected = shiny::icon("circle"),
                                             ###Units - Axis Log
                                             tabPanel(shiny::icon("gear"),
                                                      fluidRow(
                                                        box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                                            
                                                            ##Units - Y Axis Log
                                                            column(6, selectInput("ln.dis", label = "Y Axis",
                                                                                  choices = c("linear", "log"), 
                                                                                  selected = "linear"))))),
                                             tabPanel(shiny::icon("circle"),
                                                      div(class = "titleRow", fluidRow(column(5, tags$h2("")),
                                                                                       ##Granularity
                                                                                       column(3,  offset = 4, selectInput("granularity2", label = "",
                                                                                                                          choices = granularity,
                                                                                                                          selected = "year")))
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(12, plotlyOutput("d.plot", width = "100%", height = "100%")))
                                                        #column(3, img(src = "source.png", height = 75, width = 100)))
                                             ))),
                        column(3,
                                    box(width = 13, height = "600px", id = "sidebar",
                                       
                                       fluidRow(column(12, 
                                                       checkboxGroupInput("p.dis", label = h3("Adding Precipitation"),
                                                                   choices = list("Streamflow" = "streamflow",
                                                                                  "Precipitation" = "precipitation"),
                                                                   selected = "streamflow"))),
                                       fluidRow(column(12, 
                                                       sliderInput("dates.dis", label = h3("Date Range"),
                                                                   min = as.Date("1962-01-01"),
                                                                   max = as.Date("2014-01-01"),
                                                                   value = c(as.Date("1965-01-01"), as.Date("1985-01-01"))))))
                                     
                                     ) #end of column
                       
                       
                       
                       
                                     ))#end of tabItem
  ) #end of tabItems
  )#end of dashboardBody
              )#end of dashboardPage
  )#end of ShinyUI 
