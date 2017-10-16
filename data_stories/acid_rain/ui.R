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

#solutes list specifically for the base cations plot in the policy tab
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

#solutes list specifically for the SO4 NO3 graph in the policy tab
solutes_anions3 <- list("Sulfate (SO4)" = "SO4",
                        "Nitrate (NO3)" = "NO3")

#solutes list specifically for the Al graph in the policy tab
solutes_anions_Al <- list("Sulfate (SO4)" = "SO4",
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
                      "Streamflow (Q)" = "streamflow")

granularity <- list("Year (VWC)" = "year",
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
                    menuItem("Intro", tabName = "intro", icon = icon("tint")),
                    menuItem("pH", tabName = "pH", icon = icon("list-alt")),
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
                  tags$head(includeScript(system.file('www','google_analytics_1.js'))),
                  tags$head(includeScript(system.file('www','google_analytics_2.js'))),
                  tags$head(includeScript(system.file('www','google_analytics_3.js'))),
                  tags$head(tags$style(HTML(
                    "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
                  ###############################################################################
                  
                  tabItems(
                    
                    ###############################################################################
                    #### ------------  Intro  Tab ----------------------------------------- #######
                    ###############################################################################
                    
                    #If you change this, make sure to change it above in the menuItem
                    tabItem(tabName = "intro",
                            
                            #############################################
                            
                            ########### TEXT ###########
                            
                            #title and text to overlay intro image
                            fluidRow(tags$div(class = "intro-text",
                                              h1("understanding acid rain")
                                              ))
                            
                            ########### END ###########
                    ),# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------ End of Intro Tab ------------------------------------ #######
                    ###############################################################################  
                    
                    
                    
                    ###############################################################################
                    #### ------------  pH  Tab -------------------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "pH",
                            
                            ########### TITLE ####################
                            fluidRow(column(9,tags$h1("How does pH change when acid rain is mitigated?"))
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
                                                     div(class = "titleRow", fluidRow(column(5, tags$h2(" ")),
                                                                                      ##Granularity
                                                                                      column(3,  offset = 4, selectInput("granularity1", label = "",
                                                                                                                         choices = granularity,
                                                                                                                         selected = "year")))
                                                     ),
                                                     ## pH plot over time
                                                     plotlyOutput("pH")
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
                                           sliderInput("date_range_pH", label = h4("Select Date Range"),
                                                       min = as.Date("1962-01-01"),
                                                       max = as.Date("2014-01-01"),
                                                       value = c(as.Date("1962-01-01"), as.Date("2013-01-01")), timeFormat = "%b %Y")))
                                     
                              )#Closes the column
                              
                            )#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            
                    ), # Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of pH Tab ---------------------------- #######
                    ###############################################################################                     
                    
                    
                    ###############################################################################
                    #### ------------  Chemistry  Tab ---------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "chemistry",
                            
                            ########### TITLE ####################
                            fluidRow(column(9,tags$h1("How does acid rain affect solute concentrations?"))
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
                                                     div(class = "titleRow", fluidRow(column(5, tags$h2(" ")),
                                                                                      ##Granularity
                                                                                      column(3,  offset = 4, selectInput("granularity2", label = "",
                                                                                                                         choices = granularity,
                                                                                                                         selected = "year")))),
                                                     ## plot of solute conc in streamflow and precipitation over time in watershed 6
                                                     fluidRow(plotlyOutput("chemistry"))
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         #Solutes
                                         fluidRow(
                                           actionLink("select_all_ions", h4("Select Solutes")),
                                           div(class = "scrollbox",
                                               #Cations
                                               actionLink("select_all_cations", h5("Cations")),
                                               checkboxGroupInput("solutes_cations", label = p(""),
                                                                  choices = solutes_cations,
                                                                  selected = "Ca"),
                                               
                                               #Hydrogen  
                                               checkboxGroupInput("solutes_H", label = h4(""),
                                                                  choices = solutes_H,
                                                                  selected = ""),
                                               
                                               #Anions
                                               actionLink("select_all_anions", h5("Anions")),
                                               checkboxGroupInput("solutes_anions", label = p(""),
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
                                         sliderInput("date_range_chem", label = h4("Select Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1965-01-01"), as.Date("2013-01-01")),
                                                     timeFormat = "%b %Y"))
                                     
                              )#Closes the column
                              
                            )#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
        
                            
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
                            
                            ####Policy Timeline#####
                            tags$div(class = "container_question",
                                fluidRow(column(9, h4("Acid Rain Key Events Timeline"))),
                                fluidRow(column(9, timevisOutput("timeline")))),
                            
                            
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
                                                     fluidRow(column(9, offset = 6, checkboxGroupInput("solutes_anions_Al", label = "",
                                                                                                       choices = solutes_anions_Al,
                                                                                                       selected = c("Al"), 
                                                                                                       inline = TRUE))),
                                                     ## Time Plot
                                                     fluidRow(plotlyOutput("policy_Al"))
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                                     #------ End of Box 3 --------#
                                     
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
                                         sliderInput("date_range_policy", label = h4("Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1965-01-01"), as.Date("2013-01-01")),
                                                     timeFormat = "%b %Y"))
                                     
                              )#Closes the column
                              
                            )#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            ########### END OF QUESTION #1 ##########
                    )# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of Policy Tab ------------------------------- #######
                    ###############################################################################    
                    
                    
                    
                  )# Closes Tabset Panel for Main Tabs
                )#Closes Dashboard Body
  )#Closes Dashboard Page
) #closes ShinyUI


