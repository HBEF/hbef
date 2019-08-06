library(plotly)
library(ggplot2)
library(shinydashboard)
library(shiny)

shinyUI(
  dashboardPage(skin = "black",
                #Creates title that links back to the main page
                dashboardHeader(title = tags$a(href="http://hbwater.org/","HB-WER Viz"), titleWidth = 200),
                
                #Creates dashboard sidebard with data story tabs and links to 
                #pages on the website
                dashboardSidebar(
                  width = 200,
                  #width affects the width of the graphs as well as the width of 
                  #the dashboard sidebar, increasing the width decreases the 
                  #graph widths
                  sidebarMenu(
                    menuItem("Intro", tabName = "intro", icon = icon("tint")),
                    menuItem("Biomass Graphs", tabName = "bargraphs", icon = icon("home")),
                    menuItem("ET Graphs", tabName = "et", icon = icon("search-plus")),
                    menuItem("Streamflow Graphs", tabName = "streamflow", icon = icon("search-plus")),
                    menuItem("Soil Carbon Graphs", tabName = "carbon", icon = icon("search-plus")),
                    tags$div(class = "footer",tags$ul(
                      tags$li(tags$a(href="http://hbwater.org/#menu", "HOME")),
                      tags$li(tags$a(href="http://hbwater.org/#datastories","DATA STORIES")),
                      tags$li(tags$a(href="http://hbwater.org/#exploratory","EXPLORATORY TOOLS")),
                      tags$li(tags$a(href="http://hbwater.org/#aboutus","ABOUT US")))
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
    tabItem(tabName = "intro",
            
            #############################################
            
            ########### TEXT ###########
            
            #title and text to overlay intro image
            fluidRow(tags$div(class = "intro-text",
                              h1("understanding the calcium experiment"))
            )
            ########### END ###########
                              ),
    #Tab for the biomass bar plots
    tabItem(tabName = "bargraphs",
            #Title indicates that this is the calcium experiment data story
            fluidRow(column(9,tags$h1("Calcium Experiment"))),
            #Question gives overarching query we are trying to answer
             fluidRow(column(9,
               tags$div(class = "container_question", 
                        tags$h3("What happens to biomass when calcium is added 
                                to the environment?")))
             ),
            #Space for the biomass bar plot
             fluidRow(
               column(9, 
                      box(width = "100%",
                          height = "100%",
                          fluidRow(column(12, plotlyOutput("bm.plot"))),
                          fluidRow(p("*The solid black line represents the calcium
                                     application at Watershed 1.")))),
               column(3,
                 #sidebar with widgets that control graph presentation
                 box(width = 13, height = "300px", id = "sidebar",
                   fluidRow(
                     #Select species biomass to display
                     column(12, selectInput("species", 
                                            label = h4("Total Biomass or By Species"),
                                            choices = list("Total Biomass" = "all",
                                                           "Sugar maple" = "ACSA",
                                                           "American beech" = "FAGR",
                                                           "Yellow birch" = "BEAL",
                                                           "White ash" = "FRAM",
                                                           "Mountain maple" = "ACSP",
                                                           "Striped maple/Moose wood"
                                                           = "ACPE",
                                                           "Pin/fire cherry" = "PRPE",
                                                           "Choke cherry" = "PRVI",
                                                           "Balsam fir" = "ABBA",
                                                           "Red spruce" = "PIRU",
                                                           "White/paper birch" = "BEPA",
                                                           "Mountain ash" = "SOAM",
                                                           "Red maple" = "ACRU",
                                                           "Eastern hemlock" = "TSCA",
                                                           "Quaking aspen" = "POTR",
                                                           "Black cherry" = "PRSE",
                                                           "Shadbush" = "AMSP",
                                                           "Big-tooth aspen" = "POGR",
                                                           "Willow" = "SASP",
                                                           "Alternate-leaved dogwood" =
                                                             "COAL",
                                                           "Cherry (unspecified)" =
                                                             "PRSP",
                                                           "Red elderberry" = "SARA"),
                                            selected = "all"))
                            
                            ),
                   fluidRow(
                     #select elevation at which the biomass is aggregated
                     column(12, selectInput("elevation",
                                            label = h4("Elevation"),
                                            choices = list("All Elevations" = "all",
                                                           "400m - 500m" = 400,
                                                           "500m - 600m" = 500,
                                                           "600m - 700m" = 600,
                                                           "700m - 800m" = 700),
                                            selected = "all"
                   
                     )
                            ) #closes column
                     
                   ) #closes fluidRow
                     
                   )) #end of sidebar column
               
             ) #closes graph fluidRow
             ),#End of bargraphs tab item
    #Tab for carbon percentage graph
    tabItem(tabName = "carbon",
            #Title of the data story
            fluidRow(column(9,tags$h1("Calcium Experiment"))),
            #Overarching question this tab seeks to answer
            fluidRow(column(9,
              tags$div(class = "container_question", 
                       tags$h3("What happens to organic carbon in soil
                               when calcium is added 
                               to the environment?")))
                       ),
            #space for carbon percentage plot
            fluidRow(
              column(12, 
                     box(width = "100%",
                         fluidRow(column(12, plotlyOutput("c.plot"))),
                         fluidRow(p("*The solid black line represents the calcium
                                    application at Watershed 1.")))))
      
    ),
    #tab for streamflow graph
    tabItem(tabName = "streamflow", 
            #Title of data story
            fluidRow(column(9,tags$h1("Calcium Experiment"))),
            #Overarching question this tab hopes to answer
            fluidRow(column(9,
              tags$div(class = "container_question", 
                       tags$h3("What happens to streamflow when 
                               calcium is added to the environment?"))
                       )),
            fluidRow(column(9,
            box(width = 13, height = "1000px",
            #Granularity widget inside graph box
            div(class = "titleRow", fluidRow(column(5, tags$h2("")),
              column(3, offset = 4, selectInput("granularity",
                                     label = h4("Granularity"),
                                     choices = list("Week" = "week",
                                                    "Month" = "month",
                                                    "Year" = "year"),
                                     selected = "year"
                                     
              )
              ) #closes column
              
            )),
            fluidRow(
              #Space for graph of streamflow over time related to the calcium 
              #application
              column(12, 
                     box(width = "100%",
                         height = "100%",
                         fluidRow(column(12, plotlyOutput("s.plot", width = "100%",
                                                          height= "100%"))),
                         fluidRow(p("*The solid black line represents the calcium
                                    application at Watershed 1."))))))),
            column(3,
                   #Sidebar for widgets controling the graph output
                   box(width = 13, height = "250px", id = "sidebar",
                       fluidRow(
                         #date range widget
                         column(12, sliderInput("date_range", label = h4("Date Range"),
                                min = as.Date("1962-01-01"),
                                max = as.Date("2014-01-01"),
                                value = c(as.Date("1998-01-01"), as.Date("2013-01-01"))))
                       )
                       
                   )#end of box
                   ) #end of column
            ) #end of graph fluidRow
            
      
    ),#end of tabItem
    
    #Tab for evapotranspiration graphs
    tabItem(tabName = "et", 
            #Title of data story
            fluidRow(column(9,tags$h1("Calcium Experiment"))),
            #Overarching question for this tab to answer
            fluidRow(column(9,
              tags$div(class = "container_question", 
                       tags$h3("What happens to evapotranspiration amounts when 
                               calcium is added to the environment?")))
                       ),
            fluidRow(column(9,
                            #Space for setting granularity within the graph box
                            box(width = 13, height = "1000px",
                                div(class = "titleRow", fluidRow(column(5, tags$h2("")),
                                  column(3, offset = 4, selectInput("granularity.et",
                                                                    label = h4("Granularity"),
                                                                    choices = list("Week" = "week",
                                                                                   "Month" = "month",
                                                                                   "Year" = "year"),
                                                                    selected = "year"
                                                                    
                                  )
                                  ) #closes column
                                  
                                )),
                                fluidRow(
                                  column(12, 
                                         #Evapotranspiration graph
                                         box(width = "100%",
                                             height = "100%",
                                             fluidRow(column(12, plotlyOutput("et.plot", width = "100%",
                                                                              height = "100%"))),
                                             fluidRow(p("*The solid black line represents the calcium
                                    application at Watershed 1."))))))),
                     column(3,
                            #Sidebar with widgets
                            box(width = 13, height = "250px", id = "sidebar",
                                fluidRow(
                                  #Date slider widget
                                  column(12, sliderInput("date_range_et", label = h4("Date Range"),
                                                         min = as.Date("1962-01-01"),
                                                         max = as.Date("2014-01-01"),
                                                         value = c(as.Date("1998-01-01"), as.Date("2002-01-01"))))
                                )
                                
                            )#end of box
                     ) #end of column
            ) #end of graph fluidRow
            
            
            )#end of tabItem
    )#end of tabItems
              
              
              
              
  
  
)))