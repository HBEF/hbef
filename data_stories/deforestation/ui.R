library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard)


solutes_cations <- list("Potassium (K)" = "Potassium",
                        "Sodium (Na)" = "Sodium",
                        "Calcium (Ca)" = "Calcium",
                        "Magnesium (Mg)" = "Magnesium",
                        "Aluminum (Al)" = "Aluminum")

solutes_anions <- list("Sulfate (SO4)" = "Sulfate",
                       "Nitrate (NO3)" = "Nitrate",
                       "Chloride (Cl)" = "Chloride")
solutes_H <- c("Hydrogen (H)" = "Hydrogen Ion")

granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month")

# Application title
shinyUI(dashboardPage(skin = "black",
                      dashboardHeader(title = tags$a(href="http://hbef.streampulse.org/","HB-WER Viz"), titleWidth = 200),
                      dashboardSidebar(
                        width = 200,
                        sidebarMenu(
                          menuItem("Intro", tabName = "intro", icon = icon("home")),
                          menuItem("Chemistry", tabName = "solutes", icon = icon("home")),
                          menuItem("Hydrology", tabName = "streamflow", icon = icon("search-plus")),
                          tags$div(class = "footer",tags$ul(
                            tags$li(tags$a(href="http://hbef.streampulse.org/#menu", "HOME")),
                            tags$li(tags$a(href="http://hbef.streampulse.org/#datastories","DATA STORIES")),
                            tags$li(tags$a(href="http://hbef.streampulse.org/#exploratory","EXPLORATORY TOOLS")),
                            tags$li(tags$a(href="http://hbef.streampulse.org/#aboutus","ABOUT US")))
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
  
  ########### BODY ##############################################################
  
  tabItems(
    tabItem(tabName = "intro",
            
            #############################################
            
            ########### TEXT ###########
            
            #title and text to overlay intro image
            fluidRow(tags$div(class = "intro-text",
                              h1("understanding deforestation"),
                              hr(),
                              h5("Scientists at Hubbard Brook were interested in 
                                 the effects of logging operations on the surrounding 
                                 ecosystems, and questioned how deforestation would impact
                                 biogeochemical cycles and hydrology. Hubbard Brook 
                                 scientists therefore decided to conduct experiments 
                                 of deforestation and tree cutting at the watershed 
                                 level. In December 1965, Watershed 2 of Hubbard Brook 
                                 was clear-cut and all felled trees were left in place. 
                                 Herbicide was applied for a 3 year period to prevent 
                                 regrowth of vegetation. A few years later, Watershed 4
                                 was strip cut in three phases during 1970, 1972, and 1974,
                                 and the felled material was removed. Finally, whole-tree 
                                 harvest was completed in Watershed 5 during 1983-1984, 
                                 where all biomass was clear-cut and removed. Figure 1 
                                 shows an aerial view of some of these deforested or strip-cut watersheds.",
                                 p(div(img(src = "watersheds.jpg", width = "100%", height = "100%"),
                                       style="text-align: center;")),
                                 tags$span(HTML(paste("FIGURE 1- From left to right, Watershed 5 
                                                      is shown (whole-tree harvest in 1983-1984), 
                                                      then Watershed 4 (strip cut in 1970, 1972, and 
                                                      1974) and Watershed 2 (devegetated ending in 1968).
                                                      This picture can be retrieved from this",
                                                      a(href = "https://lternet.edu/node/151", " link"),
                                                      ".", sep = "")), style = "font-weight:bold;"),
                                 p(div("Scientists found that these deforestation experiments had a 
                                 considerable impact on hydrology and biogeochemistry in 
                                 Watershed 2, with similar but reduced impacts in Watersheds
                                 4, 5, and 101. During the three years that Watershed 2 was 
                                 devegetated, annual streamflow increased by 40, 28, and 26 
                                 percent. With the forest cut down, scientists explain that 
                                 this is due to the lack of transpiration of water by vegetation,
                                 which increases the total amount of water available for 
                                 streamflow. Scientists also found that about five months 
                                 after the deforestation of Watershed 2, large increases in
                                 the stream water concentration of all major nutrients were 
                                 observed, except for ammonium, sulfate, and bicarbonate. 
                                 The heightened concentration of nitrate, in particular, exceeded
                                 the drinking water standard of 40 mg/L, which can have adverse 
                                 health effects, especially for pregnant women and infants.")),
                                 p(div("The increase in concentration of nutrients in 
                                       stream water was a result of a disruption of the
                                       nitrogen cycle by deforestation. Normally, when 
                                       organic matter falls to the forest floor, microbes
                                       decompose the nitrogen in this dead organic matter
                                       into ammonium and nitrify the ammonium into nitrate.
                                       When plants are present, they reabsorb the ammonium
                                       and nitrate through their roots. However, if trees
                                       are cut down, they cannot reabsorb these nitrogen
                                       compounds, and the compounds are instead released
                                       into stream water. The nitrate ions and hydrogen
                                       ions produced from nitrification, which aren’t
                                       absorbed by plants, further mobilize other ions
                                       through reactions with the soil complex. Figures
                                       2 and 3 provide diagrams of the two nitrogen cycles.")),
                                   p(div(img(src = "before.jpg", width = "100%", height = "100%"),
                                         style="text-align: center;")),
                                   tags$span("FIGURE 2- Diagram of the nitrogen cycle
                                             in an undisturbed forested ecosystem",
                                             style = "font-weight:bold;"),
                                 p(div(img(src = "after.jpg", width = "100%", height = "100%"),
                                       style = "text-align: center;")),
                                 tags$span("FIGURE 3- Diagram of the nitrogen cycle
                                           after deforestation in an ecosystem",
                                           style = "font-weight:bold;"),
                                 p(div("Compared to the reference watershed, Watershed 6,
                                       Watershed 2 also showed acidification of stream
                                       water due to the altered nitrogen cycle caused 
                                       by tree cutting, which released more hydrogen ions into streams.")),
                                 tags$span(p(div("What to look for in this data story")),
                                           style = "font-weight:bold;"),
                                 p(div("You can see that stream flow increased after 
                                       deforestation/tree cutting events in the affected watersheds.")),
                                 p(div("How does the stream flow compare to the inputted 
                                       precipitation in these watersheds after deforestation
                                       or tree cutting events?")),
                                 p(div("You can observe that concentrations of some nutrients
                                       increase in stream water after deforestation/tree 
                                       cutting events.")),
                                 p(div("How does the concentration of nutrients in stream water
                                       compare to the concentration of these nutrients in
                                       rainwater before and after deforestation or tree cutting
                                       events? Where are these nutrients in the stream water 
                                       coming from?")),
                                 p(div("Which nutrients increase in concentration in 
                                       stream water after deforestation or tree cutting 
                                       events? Which nutrients stay the same in concentration
                                       after these events? Why might that be?")),
                                 tags$span(p(div("RESOURCES TO LEARN MORE")), style = "font-weight:bold;",
                                           style = "text-decoration: underline;", style = "text-align: left;"),
                                 p(div(em("Free internet resources"))),
                                 p(div("From Hubbard Brook Ecosystem Study,",
                                       a(href = "https://hubbardbrook.org/experimental-watersheds",
                                         "Experimental Watersheds"))),
                                 p(div("From Teaching Issues and Experiments in Ecology,",
                                       a(href = "http://tiee.esa.org/vol/v1/data_sets/hubbard/hubbard_faculty.html",
                                         "Hubbard Brook Streamflow Response to Deforestation"))),
                                 p(div(em("Books"))),
                                 p(tags$div("Holmes, R. T., & Likens, G. E. (2016).", a(href = "https://www.amazon.com/Hubbard-Brook-Story-Forest-Ecosystem/dp/0300203640",
                                                                                        em(span("Hubbard Brook: The story of a forest ecosystem.", style = "text-decoration:underline;"))),
                                            " New Haven: Yale University Press.", style = "text-align: left;")),
                                 p(div(em("Key Papers"))),
                                 p(div(HTML(paste("Likens, G. E., Bormann, F. H., Johnson, N. M., Fisher, D. W., & Pierce, R. S. (1970).",
                                       a(href = "http://www.ecoplexity.org/files/Lickens%20et%20al,%201970.pdf",
                                         span("Effects of Forest Cutting and Herbicide Treatment on Nutrient
                                         Budgets in the Hubbard Brook Watershed-Ecosystem.", style = "text-decoration:underline;")),
                                       em("Ecological Monographs, 40"), "(1).", sep = "")))),
                                 p(div(HTML(paste("Likens, Gene E, F. H Bormann, and N. M Johnson. 1969.",
                                                  a(href = "http://science.sciencemag.org/content/163/3872/1205",
                                                    HTML(paste("\"", span("Nitrification: Importance To Nutrient Losses From A Cutover Forested Ecosystem",
                                                         style = "text-decoration:underline;"), "\"", sep = ""))),
                                                  em(". Science"),  " 163: 1205-1206.", sep = "")))),
                                 p(div(HTML(paste("Pardo, Linda H, Charles T Driscoll, and Gene E Likens. 1995. ",
                                                  a(href = "https://www.researchgate.net/publication/226282221_Patterns_of_Nitrate_Loss_From_a_Chronosequence_of_Clear-Cut_Watersheds",
                                                    HTML(paste("\"", span("Patterns Of Nitrate Loss From A Chronosequence Of Clear-Cut Watersheds",
                                                         style = "text-decoration:underline;"), "\"", sep = ""))),
                                                  em(". Water, Air, and Soil Pollution "), "85: 1659-1664.", sep = "")))))
                              ))
            
            
            ########### END ###########
                              ),# Closes Intro Tab
    
              tabItem(tabName = "solutes",
                       fluidRow(column(9,tags$h1("Effects of Deforestation"))),
                       fluidRow(column(9, 
                         tags$div(class = "container_question",
                          tags$h1("What effect does deforestation have on 
                                 solute concentrations in streamwater?")))
                         ),
                      fluidRow(column(9,
                                      tabBox(width = 12, height = "1100px", side="right", 
                                             selected = shiny::icon("circle"),
                                             ###Units - Axis Log
                                             tabPanel(shiny::icon("gear"),
                                                      fluidRow(
                                                        box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                                            
                                                            ##Units - Y Axis Log
                                                            column(6, selectInput("log1", label = "Y Axis",
                                                                                  choices = c("linear", "log"), 
                                                                                  selected = "linear"))))),
                                             tabPanel(shiny::icon("circle"),
                                                      div(class = "titleRow", fluidRow(column(5, tags$h2("")),
                                                                                       ##Granularity
                                                                                       column(3,  offset = 4, selectInput("granularity", label = "",
                                                                                                                          choices = granularity,
                                                                                                                          selected = "year")))
                                                      ),
                                                    
                                                      fluidRow(
                                                        column(12, plotlyOutput("s.plot", width = "100%", height = "100%")))
                                                       
                                                        #column(3, img(src = "source.png", height = 75, width = 100)))
                                             ))),
                       column(3,
                                  box(width = 13, height = "1100px", id = "sidebar",
                                       #Solutes
                                       fluidRow(
                                         column(12, actionLink("select_all_ions", h3("Solutes"))),
                                         
                                         #Cations
                                         column(12,
                                                actionLink("select_all_cations", h5("Cations")),
                                                checkboxGroupInput("solutes_cations", label = "",
                                                                   choices = solutes_cations,
                                                                   selected = "Sodium")),
                                         
                                         #Anions
                                         
                                         column(12, actionLink("select_all_anions", h5("Anions")),
                                                checkboxGroupInput("solutes_anions", label = "",
                                                                   choices = solutes_anions,
                                                                   selected = ""))),
                                       #Hydrogen  
                                       
                                       fluidRow(
                                         column(12, actionLink("select_all_H", h5("Hydrogen")),
                                                checkboxGroupInput("solutes_H", 
                                                                       label = "",
                                                                       choices = solutes_H,
                                                                       selected = ""))),
                                       fluidRow(column(12,
                                                       selectInput("units", label = h3("Units"),
                                                                   choices = list("Eq/ha-yr" = "Eq/ha-yr",
                                                                                  "ueq/L" = "ueq/L",
                                                                                  "mg/L" = "mg/L",
                                                                                  "umol/L" = "umol/L"),
                                                                   selected = "ueq/L"))),
                                       
                                       fluidRow(column(12,
                                                       checkboxGroupInput("p", label = h3("Adding Precipitation"),
                                                                          choices = list("Streamflow" = "streamflow",
                                                                                         "Precipitation" = "precipitation"),
                                                                          selected = "streamflow"))),
                                      
                                       fluidRow(column(12, 
                                                       sliderInput("dates", label = h3("Date Range"),
                                                                   min = as.Date("1962-01-01"),
                                                                   max = as.Date("2014-01-01"),
                                                                   value = c(as.Date("1965-01-01"), as.Date("1985-01-01"))))))
                                     )#end of column
                       
                       
                                       )), #end of fluidRow
              tabItem(tabName = "streamflow",
                      fluidRow(column(9, tags$h1("Effects of Deforestation"))),
                      fluidRow(column(9,
                        tags$div(class = "container_question",
                                 tags$h1("What effect does deforestation have on 
                                 streamflow?")))
                      ),
                      fluidRow(column(9,
                                      tabBox(width = 12, height = "1100px", side="right", 
                                             selected = shiny::icon("circle"),
                                             ###Units - Axis Log
                                             tabPanel(shiny::icon("gear"),
                                                      fluidRow(
                                                        box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE, 
                                                            
                                                            ##Units - Y Axis Log
                                                            column(6, selectInput("ln.dis", label = "Y Axis",
                                                                                  choices = c("linear", "log"), 
                                                                                  selected = "linear"))))),
                                             tabPanel(shiny::icon("circle"),
                                                      div(class = "titleRow", fluidRow(column(5, tags$h2("")),
                                                                                       ##Granularity
                                                                                       column(3,  offset = 4, selectInput("granularity2", label = "",
                                                                                                                          choices = granularity,
                                                                                                                          selected = "year")))
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(12, plotlyOutput("d.plot", width = "100%", height = "100%")))
                                                        #column(3, img(src = "source.png", height = 75, width = 100)))
                                             ))),
                        column(3,
                                    box(width = 13, height = "600px", id = "sidebar",
                                       
                                       fluidRow(column(12, 
                                                       checkboxGroupInput("p.dis", label = h3("Adding Precipitation"),
                                                                   choices = list("Streamflow" = "streamflow",
                                                                                  "Precipitation" = "precipitation"),
                                                                   selected = "streamflow"))),
                                       fluidRow(column(12, 
                                                       sliderInput("dates.dis", label = h3("Date Range"),
                                                                   min = as.Date("1962-01-01"),
                                                                   max = as.Date("2014-01-01"),
                                                                   value = c(as.Date("1965-01-01"), as.Date("1985-01-01"))))))
                                     
                                     ) #end of column
                       
                       
                       
                       
                                     ))#end of tabItem
  ) #end of tabItems
  )#end of dashboardBody
              )#end of dashboardPage
  )#end of ShinyUI 
