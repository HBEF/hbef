library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)
library(directlabels)
library(magrittr)
library(timevis)


########### IMPORTANT LISTS #############


###  Lists for the sidebar  ###
#Edit if there are values that do not appear or are not relevant to your data. 

solutes_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Aluminum (Al)" = "Al")

solutes_base_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg")

solutes_anions <- list("Phosphate (PO4)" = "PO4",
                       "Sulfate (SO4)" = "SO4",
                       "Nitrate (NO3)" = "NO3",
                       "Silicon Dioxide (SiO2)" = "SiO2",
                       "Chloride (Cl)" = "Cl",
                       "Bicarbonate (HCO3)" = "HCO3")
solutes_anions3 <- list("Sulfate (SO4)" = "SO4",
                        "Nitrate (NO3)" = "NO3")

solutes_Al_anions3 <- list("Sulfate (SO4)" = "SO4",
                        "Nitrate (NO3)" = "NO3",
                        "Aluminum (Al)" = "Al")

solutes_H <- list("Hydrogen (H)" = "H",
                  "pH" = "pH")

watersheds <- list("Watershed 1" = "1",
                   "Watershed 2" = "2", 
                   "Watershed 3" = "3",
                   "Watershed 4" = "4",
                   "Watershed 5" = "5",
                   "Watershed 6" = "6",
                   "Watershed 7" = "7",
                   "Watershed 8" = "8",
                   "Watershed 9" = "9")

watersheds6 <- list("Watershed 6" = "6")
                   
water_sources <- list("Precipitation (P)" = "precipitation",
                      "Streamflow (Q)" = "streamflow")

granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month",
                    "Week" = "week")

granularity1 <- list("Year (VWC)" = "year",
                     "Month (VWC)" = "month")

granularity3 <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month",
                    "Week" = "week")

units <- list("uEquivalent/L","uMole/L", "mg/L", "flux")

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
              
              
              ### ------------------------ MAIN TAB 1 -------------------------------####
              tabPanel("Intro",
                       
                       
                       
                       ########### QUESTION #1 ####################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("How does pH change when acid rain is mitigated?")) #acid rain intro and annotated pH graph
                       ),
                       
                       #############################################
                       
                       ########### GRAPH FOR QUESTION #1 ##########
                       
                       fluidRow(
                         
                         sidebarLayout(
                           ############## SIDE BAR 1 ################ 
                           #You can edit what the default selected options are. 
                           #You can also delete inputs if you are not allowing 
                           #the user to change that particular input. 
                           
                           sidebarPanel(
                             
                             ##Water Sources
                             fluidRow(
                               column(12, checkboxGroupInput("water_sources1", label = h4("Water Sources"),
                                                             choices = water_sources,
                                                             selected = "precipitation",
                                                             inline = TRUE))),
                             ##Granularity
                             fluidRow(
                               column(12, selectInput("granularity1", label = h4("Granularity"),
                                                      choices = granularity1,
                                                      selected = "year"))),
                             
                             ##Date Range
                             sliderInput("date_range1", label = h4("Date Range"),
                                         min = as.Date("1962-01-01"),
                                         max = as.Date("2014-01-01"),
                                         value = c(as.Date("1962-01-01"), as.Date("2014-01-01")), timeFormat = "%b %Y"), width = 4),
                           
                           
                           ############## END OF SIDEBAR 1 #######
                           
                           ############## GRAPH 1 ################ 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph", tabsetPanel(id = "plot_tab1",
                                                                                   
                                                                                   ### PLOT VIEW 1
                                                                                   tabPanel("pH improvement", 
                                                                                            plotlyOutput("pH_intro", height = "auto"))
                           )), width = 8), 
                           position = "right"
                         )
                         ############## END OF GRAPH 1 ################ 
                       ),
                       
                       ########### END OF GRAPH FOR QUESTION #1 ##########
                       
                       ########### TEXT FOR QUESTION #1 ##################
                       tags$div(class = "container_paragraph",
                                fluidRow(column(width = 9,
                                                p("Placeholder text..."))),
                                fluidRow(column(width = 9,
                                                #insert widget that links to a quizlet or something here
                                                h4("Let's see how much you know offhand about acid rain... Click",
                                                   tags$a(href = "https://www.surveymonkey.com/r/RGNNTMH", "here"),
                                                   "to take the acid rain quiz.")
                                ),
                                column(3))
                       )
                       ########### END OF TEXT FOR QUESTION #1 ###############
              ),  ### ------------------------ END OF MAIN TAB 1 -------------------------------####
              
              
              
              
              
              
              
              ### ------------------------ MAIN TAB 2 -------------------------------####
              
              tabPanel("Chemistry",
                       
                       
                       
                       ########### QUESTION #2 ####################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("What does acid rain do?")) #acid rain chemistry
                       ),
                       
                       #############################################
                       
                       ########### GRAPH FOR QUESTION #2 ##########
                       
                       fluidRow(
                         
                         sidebarLayout(
                           ############## SIDE BAR 2 ################ 
                           #You can edit what the default selected options are. 
                           #You can also delete inputs if you are not allowing 
                           #the user to change that particular input. 
                           
                           sidebarPanel(
                  
                             #Solutes
                             fluidRow(
                               column(12, actionLink("select_all_ions2", h4("Solutes"))),
                               
                               #Cations
                               column(6,
                                      actionLink("select_all_cations2", h5("Cations")),
                                      checkboxGroupInput("solutes_cations2", label = "",
                                                         choices = solutes_cations,
                                                         selected = "Ca")),
                               
                               #Anions
                               
                               column(6, actionLink("select_all_anions2", h5("Anions")),
                                      checkboxGroupInput("solutes_anions2", label = "",
                                                         choices = solutes_anions,
                                                         selected = ""))),
                             #Hydrogen  
                             
                             fluidRow(
                               column(12, checkboxGroupInput("solutes_H2", label = h4(""),
                                                             choices = solutes_H,
                                                             selected = ""))),
                             ##Watersheds
                             fluidRow(
                               column(12, h4("Watersheds"), 
                                      selectInput("watersheds2", label = "",
                                                  choices = watersheds6,
                                                  selected = "6"))),
                             
                             ##Water Sources
                             fluidRow(
                               column(12, checkboxGroupInput("water_sources2", label = h4("Water Sources"),
                                                             choices = water_sources,
                                                             selected = c("precipitation", "streamflow"),
                                                             inline = TRUE))),
                             
                             ##Units  
                             fluidRow(
                               column(12, selectInput("units2", label = h4("Units"),
                                                      choices = units,
                                                      selected = "mg/L")),
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
                                         value = c(as.Date("1965-01-01"), as.Date("2013-01-01")),
                                         timeFormat = "%b %Y"), width = 4),
                           
                           
                           ############## END OF SIDEBAR 2 #######
                           
                           ############## GRAPH 2 ################ 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph",
                                              tabsetPanel(id = "plot_tab2",
                                                          
                                                          ### PLOT VIEW 1
                                                          tabPanel("Precipitation and discharge chemistry",
                                                                   h4("Changes in chemical concentrations explained in part by acid rain"),
                                                                   plotlyOutput("chemistry", height = "auto"),
                                                                   plotlyOutput("pH_streamflow", height= "auto"),
                                                                   plotlyOutput("pH_streamflow1990", height = "auto")
                                                          )
                                                          
                                                          # ### PLOT VIEW 2
                                                          # tabPanel("Flux chemistry",
                                                          #          #   plotlyOutput("fluxAlAcids", height = "auto")  ###Not showing since not sure how to interpret...
                                                          # )
                                              )), width = 8), 
                           position = "right"
                         )
                         ############## END OF GRAPH 2 ################ 
                       ),
                       
                       ########### END OF GRAPH FOR QUESTION #2 ##########
                       
                       ########### TEXT FOR QUESTION #2 ##################
                       tags$div(class = "container_paragraph",
                                fluidRow(column(width = 9,
                                                p("More placeholder text..."))),
                                fluidRow(column(width = 9,
                                                p("")),
                                         column(width = 3, #make a text box
                                                h5(strong("Soil buffer:"), "chemicals naturally present in the soil, which neutralize the
                                         strong acidity of acid rain at the expense of losing base cations in the
                                         neutralizing reactions")))
                       )
                       ########### END OF TEXT FOR QUESTION #2 ###############
                       
              ),### ------------------------ END MAIN TAB 2 -------------------------------####
              
              
              ### ------------------------ MAIN TAB 3 -------------------------------####
              
              tabPanel("Policy",
                       
                       
                       
                       ########### QUESTION #3 ####################
                       
                       fluidRow(
                         tags$div(class = "container_question", tags$h3("How have policies altered the effects of acid rain?")) #acid rain history/policy
                       ),
                       
                       #############################################
                       
                       ########### GRAPH FOR QUESTION #3 ##########
                       
                       fluidRow(
                         
                         sidebarLayout(
                           ############## SIDE BAR 3 ################ 
                           #You can edit what the default selected options are. 
                           #You can also delete inputs if you are not allowing 
                           #the user to change that particular input. 
                           
                           sidebarPanel(
                             
                             ##Watersheds
                             fluidRow(
                               column(12, h4("Watersheds"), 
                                      selectInput("watersheds3", label = "",
                                                  choices = watersheds6,
                                                  selected = "6"))),
                             
                             ##Water Sources
                             fluidRow(
                               column(12, checkboxGroupInput("water_sources3", label = h4("Water Sources"),
                                                             choices = water_sources,
                                                             selected = c("streamflow", "precipitation"),
                                                             inline = TRUE))),
                             
                             ##Units  
                             fluidRow(
                               column(12, selectInput("units3", label = h4("Units"),
                                                      choices = units,
                                                      selected = "mg/L")),
                               column(12, checkboxInput("log3", label = ("ln"),
                                                        value = FALSE))),
                             ##Granularity
                             fluidRow(
                               column(12, selectInput("granularity3", label = h4("Granularity"),
                                                      choices = granularity,
                                                      selected = "year"))),
                             
                             ##Date Range
                             sliderInput("date_range3", label = h4("Date Range"),
                                         min = as.Date("1962-01-01"),
                                         max = as.Date("2014-01-01"),
                                         value = c(as.Date("1965-01-01"), as.Date("2013-01-01")), timeFormat = "%b %Y"), width = 4),
                           
                           
                           ############## END OF SIDEBAR 3 #######
                           
                           ############## GRAPH 3 ################ 
                           #Edit the name of the plot based on the name given in the server.R file 
                           mainPanel(tags$div(class="container_graph", 
                                              tabsetPanel(id = "plot_tab3",
                                                          
                                                          ### PLOT VIEW 1
                                                          tabPanel("Combined", 
                                                                   h4("Decrease in SOx and NOx concentrations"),

                                                                   #Anions
                                                                   checkboxGroupInput("solutes_anions3", label = "",
                                                                                      choices = solutes_anions3,
                                                                                      selected = c("SO4", "NO3")),
                                                                   plotlyOutput("policy_SO4_NO3", height = "auto"),
                                                                   
                                                                   h4("Decrease in loss of base cations"),
                                                                   #Cations
                                                                   actionLink("select_all_cations3", h5("Base Cations")),
                                                                   checkboxGroupInput("solutes_cations3", label = "",
                                                                                      choices = solutes_base_cations,
                                                                                      selected = c("K", "Na", "Ca", "Mg")),
                                                                   plotlyOutput("policy_base_cations", height = "auto"),
                                                                   
                                                                   h4("Decrease in toxic Al streamflow"),
                                                                   #Al and SO4, NO3
                                                                   checkboxGroupInput("solutes_Al_anions3", label = "",
                                                                                      choices = solutes_Al_anions3,
                                                                                      selected = c("SO4", "NO3", "Al")),
                                                                   plotlyOutput("policy_Al", height = "auto"))
                                              )
                                              ), width = 8), 
                           position = "right"
                         ),
                         
                         fluidRow(column(width = 11, offset = 1,
                                         timevisOutput("timeline")))
                         
                         ############## END OF GRAPH 3 ################ 
                       ),
                       
                       ########### END OF GRAPH FOR QUESTION #3 ##########
                       
                       ########### TEXT FOR QUESTION #3 ##################
                       tags$div(class = "container_paragraph",
                                fluidRow(column(width = 9,
                                                p("Yet more placeholder text...")))
                       )
                       ########### END OF TEXT FOR QUESTION #3 ###############
                       
              )### ------------------------ END MAIN TAB 3 -------------------------------####
              
  )# Closes Tabset Panel for Main Tabs
  
)#closes FluidPage
) #closes ShinyUI


