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
library(shinydashboard)
library(grid)


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

solutes_H <- list("Hydrogen (H)" = "H")

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
                      "Discharge (Q)" = "streamflow")

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

shinyUI(
  dashboardPage(skin = "black",
                dashboardHeader(title = tags$a(href="http://vcm-192.vm.duke.edu/","HB-WER Viz"), titleWidth = 200),
                dashboardSidebar(
                  width = 200,
                  sidebarMenu(
                    menuItem("Introduction", tabName = "introduction", icon = icon("home")),
                    menuItem("Chemistry", tabName = "chemistry", icon = icon("flask")),
                    menuItem("Policy", tabName = "policy", icon = icon("book")),
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
                  
                  tabItems(
                    
                    
                    ###############################################################################
                    #### ------------  Introduction  Tab ---------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "introduction",
                            
                            ########### TITLE ####################
                            fluidRow(column(9,tags$h1("How does pH change when acid rain is mitigated?")) #acid rain intro and annotated pH graph
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
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log1", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(5, tags$h2("pH")),
                                                                                      ##Granularity
                                                                                      column(3,  offset = 4, selectInput("granularity1", label = "",
                                                                                                                         choices = granularity,
                                                                                                                         selected = "year")))
                                                     ),
                                                     ## Time Plot
                                                     plotlyOutput("pH_intro")
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "600px", id = "sidebar",
                                         ##Water Sources
                                         fluidRow(
                                           column(12, checkboxGroupInput("water_sources1", label = h4("Select Water Sources"),
                                                                         choices = water_sources,
                                                                         selected = "precipitation"))),
                                         
                                         ##Date Range
                                         fluidRow(
                                           sliderInput("date_range1", label = h4("Select Date Range"),
                                                       min = as.Date("1962-01-01"),
                                                       max = as.Date("2014-01-01"),
                                                       value = c(as.Date("1962-01-01"), as.Date("2013-01-01")), timeFormat = "%b %Y")))
                                     
                              )#Closes the column
                              
                            ),#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            tags$div(class = "text-container",
                                     fluidRow(column(width = 9,
                                                     p("Air pollution amplifies acid rain, which washes nutrients out of the 
                               soil and releases toxins into the streamflow that inhibit ecosystem 
                               growth.  United States policy has largely mitigated the effects 
                               of acid rain, and the long term data from Hubbard Brook is able 
                               to show the story of an ecosystem on the long path to recovery."),
                                                     p("In order to best understand the canonical story of acid rain, this 
                               module will focus on watershed six - the biogeochemical reference - 
                               to observe the effects of acid rain without experimental 
                               disruptions.  The dataset for watershed six dates back to 1963, 
                               which coincidentally is the year of the first rendition of the 
                               Clean Air Act.  This was also eight years after the first 
                               legislation on air pollution (The Air Pollution Control Act of 1955.)
                               Thus, the data concretely describes more of the ecosystem recovery 
                               following the effects of acid rain."),
                                                     p("Ecosystem recovery can in part be tracked by the increasing 
                               (de-acidifying) pH, as seen over the past 60+ years as new 
                               policies are implemented to address air quality (see plot above.)"))),
                                     fluidRow(column(width = 9,
                                                     #insert widget that links to a quizlet or something here
                                                     p("Let's see how much you know offhand about acid rain... Click",
                                                       tags$a(href = "https://www.surveymonkey.com/r/RGNNTMH", "here"),
                                                       "to take the acid rain quiz."))))
                            ########### END OF QUESTION #1 ##########
                    ), # Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of Introduction Tab ---------------------------- #######
                    ###############################################################################                     
                    
                    
                    ###############################################################################
                    #### ------------  Chemistry  Tab ---------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "chemistry",
                            
                            ########### TITLE ####################
                            fluidRow(column(9,tags$h1("What does acid rain do to the different solutes in the water?"))
                            ),
                            
                            #############################################
                            
                            ########### GRAPH FOR QUESTION #1 ##########
                            
                            fluidRow(
                              column(9,
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log2", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(5, tags$h2("Water Chemistry (ws6)")),
                                                                                      ##Granularity
                                                                                      column(3,  offset = 4, selectInput("granularity2", label = "",
                                                                                                                         choices = granularity,
                                                                                                                         selected = "year")))),
                                                     ## Time Plot
                                                     fluidRow(plotlyOutput("chemistry"))
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         #Solutes
                                         fluidRow(
                                           actionLink("select_all_ions2", h4("Select Solutes")),
                                           div(class = "scrollbox",
                                               #Cations
                                               actionLink("select_all_cations2", h5("Cations")),
                                               checkboxGroupInput("solutes_cations2", label = p(""),
                                                                  choices = solutes_cations,
                                                                  selected = "Ca"),
                                               
                                               #Hydrogen  
                                               checkboxGroupInput("solutes_H2", label = h4(""),
                                                                  choices = solutes_H,
                                                                  selected = ""),
                                               
                                               #Anions
                                               actionLink("select_all_anions2", h5("Anions")),
                                               checkboxGroupInput("solutes_anions2", label = p(""),
                                                                  choices = solutes_anions,
                                                                  selected = ""))),
                                         
                                         
                                         ##Water Sources
                                         fluidRow(
                                           column(12, checkboxGroupInput("water_sources2", label = h4("Select Water Sources"),
                                                                         choices = water_sources,
                                                                         selected = c("precipitation", "streamflow")))),
                                         
                                         ##Units  
                                         fluidRow(
                                           column(12, selectInput("units2", label = h4("Select Units"),
                                                                  choices = units,
                                                                  selected = "mg/L"))),
                                         
                                         ##Date Range
                                         sliderInput("date_range2", label = h4("Select Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1965-01-01"), as.Date("2013-01-01")),
                                                     timeFormat = "%b %Y"))
                                     
                              )#Closes the column
                              
                            ),#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            tags$div(class = "text-container",
                                     fluidRow(column(width = 9,
                                                     p("Air pollution amplifies acid rain, which washes nutrients out of the 
                                       soil and releases toxins into the streamflow that inhibit ecosystem 
                                       growth.  United States policy has largely mitigated the effects 
                                       of acid rain, and the long term data from Hubbard Brook is able 
                                       to show the story of an ecosystem on the long path to recovery."))))
                            
                            ########### END OF QUESTION #1 ##########
                    ),# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of Chemistry Tab ------------------------------- #######
                    ###############################################################################  
                    
                    
                    ###############################################################################
                    #### ------------  Policy Tab  ---------------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "policy",
                            
                            ########### TITLE ####################
                            fluidRow(column(9, tags$h1("How have policies altered the effects of acid rain? (As seen in ws6)")) 
                            ),
                            
                            #############################################
                            
                            ########### GRAPH FOR QUESTION #1 ##########
                            
                            fluidRow(
                              column(9,
                                     #------ Box 1 --------#
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log3", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Decrease in SO4 and NO3")),
                                                                                      ##Granularity
                                                                                      column(3, selectInput("granularity3", label = "",
                                                                                                            choices = granularity,
                                                                                                            selected = "year")))),
                                                     #Solutes
                                                     fluidRow(column(12, checkboxGroupInput("solutes_anions3", label = "",
                                                                                            choices = solutes_anions3,
                                                                                            selected = c("SO4", "NO3"), 
                                                                                            inline = TRUE))),
                                                     
                                                     ## Time Plot
                                                     fluidRow(column(12,plotlyOutput("policy_SO4_NO3")))
                                            ) #Closes tabpanel
                                            
                                     ),# Closes tab Box
                                     
                                     #------ End of Box 1 --------#
                                     
                                     #------ Box 2 --------#
                                     
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log4", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Decrease in loss of base cations")),
                                                                                      ##Granularity
                                                                                      column(3, selectInput("granularity4", label = "",
                                                                                                            choices = granularity, selected = "year")))),
                                                     #Solutes
                                                     fluidRow(checkboxGroupInput("solutes_cations3", label = "",
                                                                                 choices = solutes_base_cations,
                                                                                 selected = c("K", "Na", "Ca", "Mg"), 
                                                                                 inline = TRUE)), 
                                                     ## Time Plot
                                                     fluidRow(plotlyOutput("policy_base_cations"))
                                            ) #Closes tabpanel
                                            
                                     ), # Closes tab Box
                                     
                                     #------ End of Box 2 --------#
                                     
                                     #------ Box 3 --------#
                                     
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log5", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Decrease in toxic Al streamflow")),
                                                                                      ##Granularity
                                                                                      column(3, selectInput("granularity5", label = "",
                                                                                                            choices = granularity,
                                                                                                            selected = "year")))),
                                                     #Solutes
                                                     fluidRow(checkboxGroupInput("solutes_Al_anions3", label = "",
                                                                                 choices = solutes_Al_anions3,
                                                                                 selected = c("Al"), 
                                                                                 inline = TRUE)),
                                                     ## Time Plot
                                                     fluidRow(plotlyOutput("policy_Al"))
                                            ) #Closes tabpanel
                                            
                                     ),# Closes tab Box
                                     
                                     #------ End of Box 3 --------#
                                     
                                     #------ Box 4 --------#
                                     
                                     tabBox(width = 12, height = "400px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Timeline of acid rain history"))
                                                     )),
                                                     
                                                     ## Time Plot
                                                     fluidRow(timevisOutput("timeline"))
                                            ) #Closes tabpanel
                                            
                                     ) # Closes tab Box
                                     
                                     #------ End of Box 4 --------#
                                     
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         ##Water Sources
                                         fluidRow(
                                           column(12, checkboxGroupInput("water_sources3", label = h4("Water Sources"),
                                                                         choices = water_sources,
                                                                         selected = c("precipitation", "streamflow")))),
                                         
                                         ##Units  
                                         fluidRow(
                                           column(12, selectInput("units3", label = h4("Units"),
                                                                  choices = units,
                                                                  selected = "mg/L"))),
                                         
                                         ##Date Range
                                         sliderInput("date_range3", label = h4("Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1965-01-01"), as.Date("2013-01-01")),
                                                     timeFormat = "%b %Y"))
                                     
                              )#Closes the column
                              
                            ),#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            tags$div(class = "text-container",
                                     fluidRow(column(width = 9,
                                                     p("Air pollution amplifies acid rain, which washes nutrients out of the 
                                       soil and releases toxins into the streamflow that inhibit ecosystem 
                                       growth.  United States policy has largely mitigated the effects 
                                       of acid rain, and the long term data from Hubbard Brook is able 
                                       to show the story of an ecosystem on the long path to recovery."))))
                            
                            ########### END OF QUESTION #1 ##########
                    )# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of Policy Tab ------------------------------- #######
                    ###############################################################################    
                    
                    
                    
                  )# Closes Tabset Panel for Main Tabs
                )#Closes Dashboard Body
  )#Closes Dashboard Page
) #closes ShinyUI


