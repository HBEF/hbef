library(shiny)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(rio)
library(stringr)
library(plotly)
library(ggthemes)
library(devtools)
library(directlabels)
library(ggiraph)

#DATA

imported_data <- readRDS("precip_stream_data_long.rds")
imported_diff_data <- readRDS("precip_stream_diff_data_long.rds")

my_theme <- theme_fivethirtyeight() + 
  theme(rect = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#dddddd"), 
        text = element_text(family = "Helvetica", size = 12), 
        legend.position = "none", legend.direction = "vertical", legend.title = element_blank(),
        strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
        axis.title= element_text(NULL), axis.title.x= element_blank(), 
        axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")

solute_palette <- c(cation, anion, hydro)
source_shapes <- c("flow" = 16, "precip"= 21)

fluidPage(theme = "exploratory.css",
                tags$head(tags$style(HTML(
                  "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
                tags$head(tags$script(src="exploratory.js")),
                fluidRow(
                  h1("Water Chemistry Exploration"),
                  column(5, h2("I am interested in data from a")),
                  column(6, selectInput(inputId = "filter",
                                        label = "", 
                                        choices = c("Watershed", "Solute", "Water Source"),
                                        selected = "Watershed")
                  )),
                
                fluidRow(
                  column(2, h2("specifically,")),
                  column(6, 
                         conditionalPanel(condition = "input.filter == 'Watershed'", 
                                          selectInput(inputId = "filterws",
                                                      label = "",
                                                      choices= c("1","2", "3","4", "5","6", "7","8","9"),
                                                      selected = "6")),
                         
                         conditionalPanel(condition = "input.filter == 'Solute'", 
                                          selectInput(inputId = "filtersolute",
                                                      label = "",
                                                      choices= c("Al","Ca","Cl", "H","K","Mg","Na","NH4","NO3","pH","PO4","SiO2","SO4"),
                                                      selected = "Ca")),
                         conditionalPanel(condition = "input.filter == 'Water Source'", 
                                          selectInput(inputId = "filtersource",
                                                      label = "",
                                                      choices= c("flow","precip"),
                                                      selected = "precip")))),
                
                fluidRow(
                  column(10,
                         tabsetPanel( 
                           tabPanel("view1", column(3, plotlyOutput("view1a"))), 
                           tabPanel("view2", plotlyOutput("view2")))
                  ),
                  column(2,
                         conditionalPanel(condition = "input.filter == 'Watershed' || input.filter == 'Solute'",
                                          checkboxGroupInput(inputId = "source",
                                                             label = "Choose source to display",
                                                             choices= c("flow","precip"),
                                                             selected = "precip")
                         ),
                         
                         conditionalPanel(condition = "input.filter == 'Solute' || input.filter == 'Water Source'",
                                          checkboxGroupInput(inputId = "ws",
                                                             label = "Choose ws to display",
                                                             choices= c("1","2", "3","4", "5","6", "7","8","9"),
                                                             selected = "6")),
                         
                         conditionalPanel(condition = "input.filter == 'Water Source' || input.filter == 'Watershed'",
                                          checkboxGroupInput(inputId = "solute", label = "Choose solutes to display",
                                                             choices= c("Al","Ca","Cl", "H","K","Mg","Na","NH4","NO3","pH","PO4","SiO2","SO4"),
                                                             selected = "Ca")),
                         
                         selectInput(inputId = "granularity",
                                     label = "Granularity", 
                                     choices = c("Month", "Year"),
                                     selected = "Year"),
                         selectInput(inputId = "units",
                                     label = "Concentration Units", 
                                     choices = c("uEquivalent/L","uMole/L", "uMg/L", "flux"),
                                     selected = "uEquivalent/L"),
                         sliderInput("timeframe", label = "Select time frame",
                                     min = as.Date("1963/06/01"), 
                                     max = as.Date("2013/06/01"), 
                                     # step = 30, #is there a way to make the steps go by month?
                                     value = c(as.Date("1963/06/01"), as.Date("2013/06/01"))),
                         checkboxInput("logscale", label = "log", value = FALSE),  
                         actionButton(inputId = "update", label = "update"))
                ))
