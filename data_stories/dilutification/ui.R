library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard)


########### IMPORTANT LISTS #############


###  Lists for the sidebar  ###

#List of cations to select from
solutes_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Aluminum (Al)" = "Al")

#List of anions to select from
solutes_anions <- list("Sulfate (SO4)" = "SO4",
                       "Nitrate (NO3)" = "NO3",
                       "Chloride (Cl)" = "Cl")

#Separate option for Hydrogen
solutes_H <- c("Hydrogen (H)" = "H")

#List of watersheds to select from
watersheds <- list("Watershed 1" = 1,
                   "Watershed 2" = 2, 
                   "Watershed 3" = 3,
                   "Watershed 4" = 4,
                   "Watershed 5" = 5,
                   "Watershed 6" = 6,
                   "Watershed 7" = 7,
                   "Watershed 8" = 8,
                   "Watershed 9" = 9)

#This list allows users to select whether or not to add
#precipitation to the graphs
water_sources <- list("Add Precipitation" = "precipitation",
                      "No Precipitation" = "streamflow")

#List of granularities to choose from
granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month")

#List of units for users to choose how data is displayed
units <- list("ueq/L" = "ueq/L","umol/L" ="umol/L", "mg/L" = "mg/L")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################

shinyUI(dashboardPage(skin = "black", #creates a dashboard page
                      dashboardHeader(title = tags$a(href="http://vcm-192.vm.duke.edu/",
                                                     "HB-WER Viz"), titleWidth = 200),
                      #creates a header for the page that links back to the main site
                      dashboardSidebar(
                        #creates a sidebar that allows users to select from a menu of pages
                        #relating to dilutification
                        width = 200,
                        sidebarMenu(
                          menuItem("Intro", tabName = "intro", icon = icon("home")),
                          menuItem("By Watershed", tabName = "watersheds", icon = icon("search-plus")),
                          menuItem("By Solute", tabName = "solutes", icon = icon("search-plus")),
                        # footer here
                          tags$div(class = "footer",tags$ul(
                            #creates a footer with links back to other pages
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#menu", "HOME")),
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#datastories","DATA STORIES")),
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#exploratory","EXPLORATORY TOOLS")),
                            tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#aboutus","ABOUT US")))
                        ))
                      ),
  
  dashboardBody(
  #starts the body of the dashboard, divided into different tabs
  ########### HEAD - DO NOT EDIT ################################################
  #these commands set the aesthetics of the page
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
   #these commands divide the dashboard into tabs
   tabItem(tabName = "intro",
           
           #############################################
           
           ########### TEXT ###########
           
           #title and text to overlay intro image
           fluidRow(tags$div(class = "intro-text",
                             h1("understanding dilutification")))
                             
           
           ########### END ###########
                             ),# Closes Intro Tab
    tabItem(tabName = "watersheds", 
            #creates a tab to view data by watershed
            fluidRow(column(9,tags$h2("Dilutification of Streamwater"))),
            #adds a row of width 9 to provide the title of the page
            fluidRow(column(9, tags$div(class = "container_question",
              tags$h1("How are solute concentrations in 
                      streamwater changing over time?")))
              ),
            #adds a row of width 9 that presents a container question
            #that motivates the visualization
            
            fluidRow(column(9,
                            tabBox(width = 12, height = "1000px", side="right", 
                                   selected = shiny::icon("circle"),
                                   #creates a tab box above the graph that allows
                                   #users to choose the granularity and whether 
                                   #to display a linear or log scale
                                   ###Units - Axis Log
                                   tabPanel(shiny::icon("gear"),
                                   #creates a tab panel displayed with a gear that allows select of 
                                   #a linear or log scale
                                            fluidRow(
                                              box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                                  
                                                  ##Units - Y Axis Log
                                                  column(6, selectInput("log1", label = "Y Axis",
                                                                        choices = c("linear", "log"), 
                                                                        selected = "linear"))))),
                                  tabPanel(shiny::icon("circle"),
                                   #creates a tab panel preceded by a circle icon that allows users to 
                                   #select the granularity
                                   div(class = "titleRow", fluidRow(column(5, tags$h2("")),
                                                                    ##Granularity
                                                                    column(3,  offset = 4, 
                                                                           selectInput("granularity", label = "",
                                                                                                       choices = granularity,
                                                                                                       selected = "year")))
                                   ),
                                   
                                   fluidRow(
                                    #initializes the space for the plot output based on watershed 
                                    #as the facetting variable
                                    column(12, plotlyOutput("plot1", width = "100%", height = "100%")))
                                  ))), #ends initial column of width 9
            column(3,
              #creates the sidebar with user input choices for the graph
              box(width = 13, height = "700px", id = "sidebar",
                           #Solutes
                           fluidRow(
                             column(12, 
                                    #allows user to select the solutes to display
                                    selectInput("sol", label = h4("Solutes"),
                                                choices = 
                                                  c(solutes_anions, solutes_cations,
                                                    solutes_H),
                                                selected = "Na"))),
                           
                           
                           ##Watersheds
                           fluidRow(
                             column(12, actionLink("select_all_ws", h4("Watersheds")),
                                    #the actionLink command allows the user to select 
                                    #all the watersheds to display at once
                                    selectInput("watersheds", label = "",
                                                choices = watersheds, multiple = TRUE,
                                                #multiple must equal TRUE so that 
                                                #more than one watershed can be selected
                                                selected = 6))),
                           
                           ##Water Sources
                           fluidRow(
                             column(12, 
                             #user is allowed to select both streamflow and precipitation,
                             #or just streamflow
                             checkboxGroupInput("water_sources", label = h4("Adding Precipitation"),
                                                choices = list("Streamflow" = "streamflow",
                                                               "Precipitation" = "precipitation"),
                                                selected = "streamflow"))
                             ),
                           
                           ##Units  
                           fluidRow(
                             #row that displays choices of units
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
           #tab for facetting by solute
            fluidRow(column(9,tags$h2("Dilutification of Streamwater"))),
            #row for title of page
            fluidRow(column(9,tags$div(class = "container_question",
            #row for container question, that motivates the visualization
              tags$h1("How are solute concentrations in 
                      streamwater changing over time?")))
              ),
           fluidRow( #row for the graph as well as the selection of the scale
                     #and ganularity
             column(9,
           tabBox(width = 12, height = "600px", side="right", 
                  selected = shiny::icon("circle"),
                  ###Units - Axis Log
                  tabPanel(shiny::icon("gear"),
                  #tab panel headed by a gear icon which allows selection of 
                  #a linear or a log scale
                           fluidRow(
                             box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                 
                                 ##Units - Y Axis Log
                                 column(6, selectInput("log2", label = "Y Axis",
                                                       choices = c("linear", "log"), 
                                                       selected = "linear"))))),
                  tabPanel(shiny::icon("circle"),
                  #tab panel designated by a circle icon which allows selection
                  #of the granularity of the data to display
                           column(3, offset = 9, selectInput("granularity2", label = h4("Granularity"),
                                                             choices = granularity,
                                                             selected = "year")),
                           fluidRow(
                             #space for the plot of solute concentration facetted
                             #by solute
                             column(12, plotlyOutput("plot2", width = "100%", height = "100%")))
                  ))),
            column(3,
                      box(width = 13, height = "1100px", id = "sidebar",
                          #box for the sidebar with choices for the user 
                          #to modify the graph
                          
                            #Solutes
                            fluidRow(
                              column(12, actionLink("select_all_ions", h4("Solutes")),
                              #this actionLink command gives the option to 
                              #select all ions, as defined in the server function
                              
                              #Cations
                              
                                     actionLink("select_all_cations", h5("Cations")),
                                     #this actionLink command gives the option to 
                                     #select all cations, when connected with related
                                     #code in the server function                                             
                                     checkboxGroupInput("solutes_cations", label = "",
                                                        choices = solutes_cations,
                                                        selected = "Na"),
                              
                              #Anions
                              
                                     actionLink("select_all_anions", h5("Anions")),  
                              #this actionLink command gives the option to 
                              #select all anions, when connected with related
                              #code in the server function
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
                          
                            #you can only select one watershed in this tab
                            fluidRow(
                              column(12, 
                                     selectInput("watersheds2", label = h4("Watersheds"),
                                                 choices = watersheds,
                                                 selected = 6))),
                            
                            ##Water Sources
                            fluidRow(
                              #user is allowed to select both streamflow and precipitation,
                              #or just streamflow
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