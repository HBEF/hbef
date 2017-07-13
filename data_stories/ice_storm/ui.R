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
              
              
              ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MAIN TAB # 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              tabPanel("LAI",
                       
                       
                       
                       ########################### QUESTION #1 ###################################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("How do ice storms affect vegetation?"))
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
                               column(12, h4("Watersheds"), 
                                      selectInput("watersheds1", label = "",
                                                  choices = watersheds1,
                                                  selected = "1"))), width = 4),
                           
                           ############## END OF SIDEBAR 1 #######
                           
                           ############## GRAPH 1 #################### 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph", 
                                              tabsetPanel(id = "plot_tab",
                                                          
                                                          ### PLOT VIEW 1
                                                          tabPanel("LAI by plot", plotlyOutput("lai_plot", height = "auto"))
                                              )), width = 8), 
                           position = "right"
                         )
                         ############## END OF GRAPH 1 ################ 
                       ),
                       
                       #---------END OF VISUALIZATION FOR QUESTION #1 ---------#
                       
                       
                       #--------- TEXT QUESTION #1 ----------------------------#
                       
                       tags$div(class = "container_paragraph", fluidRow(
                         tags$p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  The leaf area index
                                (LAI) is one way to track the regrowth of the canopy.")
                         ))
                       #--------- END OF TEXT QUESTION #1 ----------------------------#
                       
                       ########################### END OF QUESTION #1 ###################################
                       
                       
                         ),  ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END OF MAIN TAB # 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              
              

              ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MAIN TAB # 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              tabPanel("NO3 general",
                       
                       
                       
                       ########################### QUESTION #2 ###################################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("How do NO3 concentrations change following an ice storm?"))
                       ),
                       
                       
                       #---------VISUALIZATION FOR QUESTION #2 ---------#
                       
                       fluidRow(
                         
                         sidebarLayout(
                           ############## SIDE BAR 2 ################ 
                           #You can edit what the default selected options are. 
                           #You can also delete inputs if you are not allowing 
                           #the user to change that particular input. 
                           
                           sidebarPanel(
                             
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
                                                             selected = "streamflow",
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
                                                      selected = "month"))),
                             
                             ##Date Range
                             sliderInput("date_range2", label = h4("Date Range"),
                                         min = as.Date("1962-01-01"),
                                         max = as.Date("2014-01-01"),
                                         value = c(as.Date("1997-01-01"), as.Date("2001-01-01"))), width = 4),
                           
                           
                           ############## END OF SIDEBAR 2 #######
                           
                           ############## GRAPH 2 #################### 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph", tabsetPanel(id = "plot_tab",
                                                                                   
                                                                                   ### PLOT VIEW 1
                                                                                   tabPanel("Plot1",plotlyOutput("NO3_plot"))

                           )), width = 8), 
                           position = "right"
                         )
                         ############## END OF GRAPH 2 ################ 
                       ),
                       
                       #---------END OF VISUALIZATION FOR QUESTION #2 ---------#
                       
                       
                       #--------- TEXT QUESTION #2 ----------------------------#
                       
                       tags$div(class = "container_paragraph", fluidRow(
                         tags$p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  Some effects of the 
                                storm can be tracked by the NO3 streamflow data.")
                         ))
                       #--------- END OF TEXT QUESTION #2 ----------------------------#
                       
                       ########################### END OF QUESTION #2 ###################################
                       
                       
                         ), ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END OF MAIN TAB # 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              
              
              ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MAIN TAB # 3 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              tabPanel("NO3 flux",
                       
                       
                       
                       ########################### QUESTION #3 ###################################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("Post-ice storm NO3 flux"))
                       ),
                       
                       
                       #---------VISUALIZATION FOR QUESTION #3 ---------#
                       
                       fluidRow(
                         
                         sidebarLayout(
                           ############## SIDE BAR 3 ################ 
                           #You can edit what the default selected options are. 
                           #You can also delete inputs if you are not allowing 
                           #the user to change that particular input. 
                           
                           sidebarPanel(

                             ##Granularity
                             fluidRow(
                               column(12, selectInput("granularity3", label = h4("Granularity"),
                                                      choices = granularity,
                                                      selected = "year"))),
                             
                             ##Date Range
                             sliderInput("date_range3", label = h4("Date Range"),
                                         min = as.Date("1962-01-01"),
                                         max = as.Date("2014-01-01"),
                                         value = c(as.Date("1963-01-01"), as.Date("2004-01-01"))), width = 4),
                           
                           
                           ############## END OF SIDEBAR 3 #######
                           
                           ############## GRAPH 3 #################### 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph", tabsetPanel(id = "plot_tab",
                                                                                   
                                                                                   ### PLOT VIEW 1
                                                                                   tabPanel("NO3 Plots Replication (2003 paper)",
                                                                                            h4("Streamflow flux (ws1, ws6)"),
                                                                                            plotlyOutput("NO3_output", height = "auto"),
                                                                                            h4("Streamflow flux normalized against ws6 (ws2, ws4, ws5)"),
                                                                                            plotlyOutput("NO3_excess", height = "auto"))
                           )), width = 8), 
                           position = "right"
                         )
                         ############## END OF GRAPH 3 ################ 
                       ),
                       
                       #---------END OF VISUALIZATION FOR QUESTION #3 ---------#
                       
                       
                       #--------- TEXT QUESTION #3 ----------------------------#
                       
                       tags$div(class = "container_paragraph", fluidRow(
                         tags$p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  Some effects of the 
                                storm can be tracked by the NO3 flux data.")
                         ))
                       #--------- END OF TEXT QUESTION #3 ----------------------------#
                       
                       ########################### END OF QUESTION #3 ###################################
                       
                       
                         ) ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END OF MAIN TAB # 3 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-####
              
              
              
                       )# Closes Tabset Panel for Main Tabs
  
                       )#closes FluidPage
              ) #closes ShinyUI

