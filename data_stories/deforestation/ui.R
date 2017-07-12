library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggiraph)

solutes_cations <- list("Potassium (K)" = "Potassium",
                        "Sodium (Na)" = "Sodium",
                        "Calcium (Ca)" = "Calcium",
                        "Magnesium (Mg)" = "Magnesium",
                        "Aluminum (Al)" = "Aluminum")

solutes_anions <- list("Sulfate (SO4)" = "Sulfate",
                       "Nitrate (NO3)" = "Nitrate",
                       "Chloride (Cl)" = "Chloride")
solutes_H <- c("Hydrogen (H)" = "Hydrogen Ion")

  # Application title
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
    tabPanel("Solute Concentrations",
             titlePanel("Effects of Deforestation"),
             fluidRow( 
               tags$h3("What effect does deforestation have on 
                       solute concentrations in streamwater?")
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
                                             selected = "Sodium")),
                   
                   #Anions
                   
                   column(6, actionLink("select_all_anions", h5("Anions")),
                          checkboxGroupInput("solutes_anions", label = "",
                                             choices = solutes_anions,
                                             selected = ""))),
                 #Hydrogen  
                 
                 fluidRow(
                   column(12, checkboxGroupInput("solutes_H", 
                                                 label = h5("Hydrogen"),
                                                 choices = solutes_H,
                                                 selected = ""))),
                 fluidRow(column(12,
                 selectInput("units", label = h3("Units"),
                             choices = list("Eq/ha-yr" = "Eq/ha-yr",
                                            "ueq/L" = "ueq/L",
                                            "mg/L" = "mg/L",
                                            "umol/L" = "umol/L"),
                             selected = "ueq/L"))),
                 #checkbox input for selecting whether to apply the logarithm
                 fluidRow(column(12,
                 h3("Applying the Natural Logarithm"),
                 checkboxInput("ln", label = "Natural Log",
                               value = FALSE))),
                 fluidRow(column(12,
                 selectInput("granularity", label = h3("Granularity"),
                             choices = list("Month (VWC)" = "month",
                                            "Year (VWC)" = "year"),
                             selected = "year"))),
                 fluidRow(column(12,
                 selectInput("p", label = h3("Adding Precipitation"),
                             choices = list("Without Precipitation" = "noprecip",
                                            "With Precipitation" = "precip"),
                             selected = "noprecip"))),
                 fluidRow(column(12, 
                 sliderInput("dates", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1985-01-01")))))),
               mainPanel(fluidRow(
                 column(8, plotlyOutput("s.plot", width = "100%", height = "100%")),
                 column(4, img(src = "source.png", height = 150, width = 200))),
                 fluidRow(p("The solid black lines represent the beginning of cutting
                          of the watershed."))))
               
               
             ),
    tabPanel("Discharge and Precipitation Quantities",
             titlePanel("Effects of Deforestation"),
             fluidRow( 
               tags$h3("What effect does deforestation have on 
                       streamflow?")
               ),
             sidebarLayout(position = "right",
               sidebarPanel(
                 fluidRow(column(12,
                 selectInput("granularity2", label = h3("Time Scale"),
                             choices = list("Month (VWC)" = "month",
                                            "Year (VWC)" = "year"),
                             selected = "year"))),
                 #checkbox input for selecting whether to apply the logarithm
                 fluidRow(column(12, 
                 h3("Applying the Natural Logarithm"),
                 checkboxInput("ln.dis", label = "Natural Log",
                               value = FALSE))),
                 fluidRow(column(12, 
                 selectInput("p.dis", label = h3("Adding Precipitation"),
                             choices = list("Without Precipitation" = "noprecip",
                                            "With Precipitation" = "precip"),
                             selected = "noprecip"))),
                 fluidRow(column(12, 
                 sliderInput("dates.dis", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1985-01-01")))))),
               
               mainPanel(fluidRow(
                 column(8, plotlyOutput("d.plot", width = "100%", height = "100%")),
                 column(4, img(src = "source.png", height = 150, width = 200))
                 ),
                 fluidRow(p("The solid black lines represent the beginning
                            of cutting in the watershed."))))
               
             
             
             
    ))
 )#end of fluidPage
)#end of ShinyUI 
  