library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)
library(directlabels)


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
                       "Chlorine (Cl)" = "Cl",
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

water_sources <- list("Precipitation (P)" = "precip",
                      "Discharge (Q)" = "flow")

granularity <- list("Year" = "year",
                    "Month" = "month",
                    "Week" = "week")

units <- list("uEquivalent/L","uMole/L", "uMg/L", "flux")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################

shinyUI(fluidPage(
  
  ########### HEAD - DO NOT EDIT ################################################
  theme = "app.css",
  tags$head(includeScript(system.file('www', 'ajax.js'))),
  tags$head(includeScript(system.file('www', 'iframeResizer.contentWindow.min.js'))),
  tags$head(includeScript(system.file('www', 'app.js'))),
  tags$head(tags$style(HTML(
    "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
  ###############################################################################
  
  ########### BODY ##############################################################
  
  tabsetPanel(id = "top", type = "pills",
              
              
              ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MAIN TAB # 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              tabPanel("Causes",
                       
                       
                       
                       ########################### QUESTION #1 ###################################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("Framework for Ice Storm... still in progress!"))
                       ),
                       
                       
                       #---------VISUALIZATION FOR QUESTION #1 ---------#
                       
                       fluidRow(
                         
                         sidebarLayout(
                           ############## SIDE BAR 1 ################ 
                           #You can edit what the default selected options are. 
                           #You can also delete inputs if you are not allowing 
                           #the user to change that particular input. 
                           
                           sidebarPanel(
                             
                             ##Watersheds
                             fluidRow(
                               column(12, actionLink("select_all_ws", h4("Watersheds")), 
                                      selectInput("watersheds", label = "",
                                                  choices = watersheds, multiple = TRUE,
                                                  selected = "6"))),
                             
                             ##Units  
                             fluidRow(
                               column(12, selectInput("units", label = h4("Units"),
                                                      choices = units,
                                                      selected = "mg/L")),
                               column(12, checkboxInput("log", label = ("ln"),
                                                        value = FALSE))),

                             ##Date Range
                             sliderInput("date_range", label = h4("Date Range"),
                                         min = as.Date("1962-01-01"),
                                         max = as.Date("2014-01-01"),
                                         value = c(as.Date("1965-01-01"), as.Date("2013-01-01"))), width = 4),
                           
                           
                           ############## END OF SIDEBAR 1 #######
                           
                           ############## GRAPH 1 #################### 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph", 
                                              tabsetPanel(id = "plot_tab",
                                                          
                                                          ### PLOT VIEW 1
                                                          tabPanel("Watershed 1", plotlyOutput("ws1_lai_plot", height = "auto")),
                                                          
                                                          ### PLOT VIEW 2
                                                          tabPanel("Watershed 6", plotlyOutput("ws6_lai_plot", height = "auto"))
                                              )), width = 8), 
                           position = "right"
                         )
                         ############## END OF GRAPH 1 ################ 
                       ),
                       
                       #---------END OF VISUALIZATION FOR QUESTION #1 ---------#
                       
                       
                       #--------- TEXT QUESTION #1 ----------------------------#
                       
                       tags$div(class = "container_paragraph", fluidRow(
                         tags$p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  Some effects of the 
                                storm can be tracked by the NO3 streamflow data.")
                         ))
                       #--------- END OF TEXT QUESTION #1 ----------------------------#
                       
                       ########################### END OF QUESTION #1 ###################################
                       
                       
                         ),  ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END OF MAIN TAB # 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              
              
              
              
              
              
              
              
              
              
              ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MAIN TAB # 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              tabPanel("Alternative Sidebar",
                       
                       
                       
                       ########################### QUESTION #2 ###################################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("What happens if you need more time to finish this story..?
                                                                        (Obviously still in progress...)"))
                       ),
                       
                       
                       #---------VISUALIZATION FOR QUESTION #1 ---------#
                       
                       fluidRow(
                         
                         sidebarLayout(
                           ############## SIDE BAR 2 ################ 
                           #You can edit what the default selected options are. 
                           #You can also delete inputs if you are not allowing 
                           #the user to change that particular input. 
                           
                           sidebarPanel(
                             
                             #Solutes
                             fluidRow(
                               column(12, actionLink("select_all_ions", h4("Solutes"))),
                               column(12,
                                      selectInput("solutes", label = "",
                                                  choices = all_solutes,
                                                  selected = "Na"))),
                             
                             ##Watersheds
                             fluidRow(
                               column(12, actionLink("select_all_ws2", h4("Watersheds")), 
                                      selectInput("watersheds2", label = "",
                                                  choices = watersheds, multiple = TRUE,
                                                  selected = "6"))),
                             
                             ##Water Sources
                             fluidRow(
                               column(12, checkboxGroupInput("water_sources2", label = h4("Water Sources"),
                                                             choices = water_sources,
                                                             selected = "precip",
                                                             inline = TRUE))),
                             
                             ##Units  
                             fluidRow(
                               column(12, selectInput("units", label = h4("Units"),
                                                      choices = units,
                                                      selected = "mg/L")),
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
                                         value = c(as.Date("1965-01-01"), as.Date("2013-01-01"))), width = 4),
                           
                           
                           ############## END OF SIDEBAR 2 #######
                           
                           ############## GRAPH 2 #################### 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph", tabsetPanel(id = "plot_tab",
                                                                                   
                                                                                   ### PLOT VIEW 1
                                                                                   tabPanel("Plot1",plotlyOutput("")),
                                                                                   
                                                                                   ### PLOT VIEW 2
                                                                                   tabPanel("Plot2",plotlyOutput(""))
                           )), width = 8), 
                           position = "right"
                         )
                         ############## END OF GRAPH 2 ################ 
                       ),
                       
                       #---------END OF VISUALIZATION FOR QUESTION #1 ---------#
                       
                       
                       #--------- TEXT QUESTION #1 ----------------------------#
                       
                       tags$div(class = "container_paragraph", fluidRow(
                         tags$p("Deforestation, the removal of forest trees,is harmful to the environment for a number of reasons, 
                                some of which are less obvious than others.
                                Deforestation results in habitat loss for woodland-
                                dwelling species, causing die-offs and concurrent declines
                                in biodiversity. Around eighty percent of land animals
                                and plants on Earth reside in forests (National Geographic Society
                                2017). It follows that the impact of widespread deforestation
                                on wildlife is not insignificant. Deforestation also 
                                accelerates climate change, as the loss of forests
                                that absorb carbon dioxide tips the balance 
                                so that more of this greenhouse gas enters 
                                that atmosphere, causing global warming. On
                                the smaller scale, deforestation can trigger
                                regional climate change because the ground 
                                cover from the trees is eliminated, allowing sun
                                rays to penetrate where they were previously 
                                blocked. This causes soils to dry out, which can
                                transform once forested land into deserts. 
                                Without canopy cover to block sunlight during
                                the day and retain heat during the night, 
                                temperature fluctuations become more severe
                                and harmful to wildlife. Deforestation also
                                leads to drier climates because less water 
                                is transpired, or released into the air, by
                                trees. This negatively impacts the water cycle
                                (National Geographic Society 2017).")
                         ))
                       #--------- END OF TEXT QUESTION #1 ----------------------------#
                       
                       ########################### END OF QUESTION #2 ###################################
                       
                       
                         ) ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END OF MAIN TAB # 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              
              
                       )# Closes Tabset Panel for Main Tabs
  
                       )#closes FluidPage
              ) #closes ShinyUI

