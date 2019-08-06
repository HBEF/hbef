library(ggplot2)
#library(lubridate) #detach("package:<packageName>", unload=TRUE)
#library(readr) #If this is only run once to create the ui, then I don't think these are needed...
#library(tidyr)
#library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)
library(directlabels)
library(shinydashboard)


########### IMPORTANT LISTS ############

###  Lists for the sidebar  ###
#Edit if there are values that do not appear or are not relevant to your data. 

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

#watershed choices for LAI plot
watersheds1 <- list("Watershed 1" = "1",
                    "Watershed 6" = "6")

water_sources <- list("Precipitation (P)" = "precipitation",
                      "Discharge (Q)" = "streamflow")

granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month",
                    "Week" = "week")

units <- list("uEquivalent/L","uMole/L", "mg/L", "flux")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################

shinyUI(
  dashboardPage(skin = "black",
                dashboardHeader(title = tags$a(href="http://hbwater.org/","HB-WER Viz"), titleWidth = 200),
                dashboardSidebar(
                  width = 200,
                  sidebarMenu(
                    #icons from fontawesome.io/icons
                    menuItem("Intro", tabName = "intro", icon = icon("snowflake-o")),
                    menuItem("NO3 trends", tabName = "trends", icon = icon("line-chart")),
                    menuItem("NO3 flux (Q)", tabName = "flux", icon = icon("arrows-v")),
                    menuItem("Vegetation", tabName = "vegetation", icon = icon("leaf")),
                    # footer here
                    tags$div(class = "footer",tags$ul(
                      tags$li(tags$a(href="http://hbwater.org/#menu", "HOME")),
                      tags$li(tags$a(href="http://hbwater.org/#datastories","DATA STORIES")),
                      tags$li(tags$a(href="http://hbwater.org/#exploratory","EXPLORATORY TOOLS")),
                      tags$li(tags$a(href="http://hbwater.org/#aboutus","ABOUT US")))
                    )
                  )
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
                  
                  ########### BODY ##############################################################
                  
                  tabItems(
                    
                    ###############################################################################
                    #### ------------  Intro  Tab ----------------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "intro",
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            #text that overlays image specified in the css file
                            fluidRow(tags$div(class = "intro-text",
                                              h1("understanding ice storms")))
                            
                            ########### END OF QUESTION #1 ##########
                    ),# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------ End of Intro Tab ------------------------------------ #######
                    ###############################################################################  
                    
                    
                    ###############################################################################
                    #### ------------  NO3  trends  Tab ----------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "trends",
                            
                            ########### TITLE ####################
                            fluidRow(tags$div(class = "container_question", 
                                              tags$h1("How do NO3 concentrations change following an ice storm?")) 
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
                                                                                 column(6, selectInput("log", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(5, tags$h2("")), #put graph title if desired
                                                                                      ##Granularity
                                                                                      column(3,  offset = 4, selectInput("granularity", label = "",
                                                                                                                         choices = granularity,
                                                                                                                         selected = "month")))),
                                                     ## NO3 over time plot
                                                     plotlyOutput("NO3_plot")
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         
                                         ##Watersheds
                                         fluidRow(
                                           column(12, h4("Watersheds"), 
                                                  selectInput("watersheds2", label = "",
                                                              choices = watersheds, multiple = T,
                                                              selected = "6"))),
                                         
                                         ##Units  
                                         fluidRow(
                                           column(12, selectInput("units", label = h4("Units"),
                                                                  choices = units,
                                                                  selected = "mg/L"))),
                                         
                                         ##Date Range
                                         sliderInput("date_range2", label = h4("Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1997-01-01"), as.Date("2001-01-01")),
                                                     timeFormat = "%b %Y")) #formats time into "Jan 1962" format
                                     
                              )#Closes the column
                              
                            ),#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            tags$div(class = "",
                                     fluidRow(column(width = 9,
                                                     p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm
                                that damaged the experimental watersheds.  Some effects of the 
                                       storm can be tracked by the NO3 streamflow data."))))
                            
                            ########### END OF QUESTION #1 ##########
                    ),# Closes trends Tab
                    
                    ###############################################################################
                    #### ------------ End of NO3 trends Tab ------------------------------- #######
                    ###############################################################################  
                    
                    
                    ###############################################################################
                    #### ------------  Flux Tab  ------------------------------------------ #######
                    ###############################################################################
                    
                    tabItem(tabName = "flux",
                            
                            ########### TITLE ####################
                            fluidRow(tags$div(class = "container_question", 
                                              tags$h1("Post-ice storm NO3 flux: plot replication (Bernhardt et. al, 2003)")) #full paper details in text below
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
                                                                                 column(6, selectInput("log_flux", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Streamflow flux")))),
                                                     ## Time Plot
                                                     plotlyOutput("NO3_output")
                                            ) #Closes tabpanel
                                            
                                     ),# Closes tab Box
                                     
                                     #------ End of Box 1 --------#
                                     
                                     #------ Box 2 --------#
                                     
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Normalized streamflow flux")))),
                                                     
                                                     #text above plot
                                                     p("*Note the 1966 devegetation spike (ws2), the 1971 stripcutting spike (ws4),
                                                       and the 1983 whole tree harvest spike (ws5)."),
                                                     p("To better see the effects of the ice storm, click and drag to zoom in."),
                                                     
                                                     ## plot of NO3 streamflow that exceeded ws6
                                                     plotlyOutput("NO3_excess")
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                                     #------ End of Box 2 --------#
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         
                                         ##Granularity
                                         fluidRow(
                                           column(12, selectInput("granularity3", label = h4("Granularity"),
                                                                  choices = granularity,
                                                                  selected = "year"))),
                                         ##Date Range
                                         sliderInput("date_range3", label = h4("Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1963-01-01"), as.Date("2004-01-01")),
                                                     timeFormat = "%b %Y"))
                                     
                              )#Closes the column
                              
                            ),#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            tags$div(class = "",
                                     fluidRow(column(width = 9,
                                                     p("On January 7-8, 1998 the HBEF was hit by a powerful ice 
                                                       storm that damaged the experimental watersheds.  Some 
                                                       effects of the storm can be tracked by the NO3 flux data. 
                                                       The above plots were recreated from the 2003 paper 
                                                       'In-stream uptake dampens effects of major forest 
                                                       disturbance on watershed nitrogen export' by Emily Bernhardt, 
                                                       Gene Likens, and Donald Buso."))))
                            
                            ########### END OF QUESTION #1 ##########
                    ),# Closes Flux Tab
                    
                    ###############################################################################
                    #### ------------  End of Flux Tab ------------------------------------ #######
                    ############################################################################### 
                    
                    
                    ###############################################################################
                    #### ------------  Vegetation  Tab ------------------------------------ #######
                    ###############################################################################
                    
                    tabItem(tabName = "vegetation",
                            
                            ########### TITLE ####################
                            fluidRow(tags$div(class = "container_question",
                                              tags$h1("How do ice storms affect vegetation?"))
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
                                                                                 column(6, selectInput("log_counts", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", 
                                                         fluidRow(column(7, tags$h2("Decline in leaf counts across species due to ice storm"),
                                                                         p("(click on key to focus on specific species)")
                                                         ))),
                                                     ## plot of leaf counts from different tree species over time
                                                     plotlyOutput("leaf_count")
                                                     
                                            ) #Closes tabpanel
                                            
                                     ), # Closes tab Box
                                     
                                     #------ End of Box 1 --------#
                                     
                                     #------ Box 2 --------#
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(6, tags$h2("Vegetation increase after ice storm by plot")),
                                                                                      ##Granularity
                                                                                      column(3, offset = 2, h4("Watersheds"),
                                                                                             selectInput("watersheds1", label = "",
                                                                                                         choices = watersheds1,
                                                                                                         selected = "1")))),
                                                     ## plot of LAI by plot number
                                                     plotlyOutput("lai_plot")
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                                     #------ End of Box 2 --------#
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         
                                         ##Date Range for leaf counts
                                         sliderInput("date_range_count", label = h4("Date Range"),
                                                     min = 1993,
                                                     max = 2013,
                                                     value = c(1997, 2001),
                                                     sep = "")
                                         
                                     )#Closes the box
                                     
                              )#Closes the column
                              
                            ),#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            tags$div(class = "",
                                     fluidRow(column(width = 9,
                                                     p("On January 7-8, 1998 the HBEF was hit by a powerful ice storm 
                                                       that damaged the experimental watersheds.  The leaf area index 
                                                       (LAI) is one way to track the regrowth of the canopy."))))
                            
                            ########### END OF QUESTION #1 ##########
                    ) # Closes Vegetation Tab
                    
                    ###############################################################################
                    #### ------------  End of Vegetation Tab ------------------------------ #######
                    ###############################################################################
                    
                  )# Closes Tabset Panel for Main Tabs
                )#Closes Dashboard Body
  )#closes FluidPage
) #closes ShinyUI

