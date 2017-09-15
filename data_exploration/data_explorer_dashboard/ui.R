library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(shinyjs)
library(plotly)
library(shinydashboard)


########### IMPORTANT LISTS #############


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
solutes_H <- list("Hydrogen (H)" = "H", "Dissolved Inorganic Carbon" = "DIC")

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

water_sources <- list("Streamflow (Q)" = "streamflow", 
                     "Precipitation (P)" = "precipitation")

granularity <- list( "Week" = "week",
                    "Month (VWC)" = "month",
                    "Year (VWC)" = "year")

time_variables <- list("Solute Concentration" = "concentration", 
                       "pH" = "pH",
                       "Charge Balance" = "chargebalance",
                       "Temperature" = "temp", 
                       "Acid-Neutralizing Capacity" = "anc", 
                       "Specific Conductivity" = "spcond")
units <- list("uEquivalent/L" = "concentration_ueq","uMole/L" = "concentration_umol", "mg/L" = "concentration_mg")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################



shinyUI(
  dashboardPage(
    skin = "black",
    dashboardHeader(title = tags$a(href="http://vcm-192.vm.duke.edu/","HB-WER Viz"), titleWidth = 200),
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
        menuItem("Bubble Plot", tabName = "exploratory", icon = icon("search-plus")),
        # footer here
        tags$div(class = "footer",tags$ul(
          tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#menu", "HOME")),
          tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#datastories","DATA STORIES")),
          tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#exploratory","EXPLORATORY TOOLS")),
          tags$li(tags$a(href="http://vcm-192.vm.duke.edu/#aboutus","ABOUT US")))
        ))
      
    ),
    dashboardBody(
      useShinyjs(),  # Set up shinyjs
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      tags$script('
        $(document).on("keydown", function (e) {
        if (e.keyCode === 13) {
          $("#go_bubble").click()};
        });

        $(document).on("keydown", function (e) {
        if (e.keyCode === 13) {
                  $("#go_exploratory").click()};
                  });
        '),
      tags$head(includeScript(system.file('www', 'ajax.js'))),
      tags$head(includeScript(system.file('www', 'iframeResizer.contentWindow.min.js'))),
      tags$head(includeScript(system.file('www', 'app.js'))),
      tags$head(includeScript(system.file('www','google_analytics_1.js'))),
      tags$head(includeScript(system.file('www','google_analytics_2.js'))),
      tags$head(includeScript(system.file('www','google_analytics_3.js'))),
      tags$head(tags$style(HTML(
        "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
      
      tabItems(
        
        ################################################################################################
        #### ------------  Dashboard Tab Content  ------------------------------------------------------ 
        #################################################################################################
        
        tabItem(tabName = "dashboard",
        # Global Settings Box >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> --------
            fluidRow( 
              tabBox(width = 12, side="right", selected = shiny::icon("circle"),
                     
        ## ---- Global Settings Tab -----------------------------------------------------------------
             tabPanel(shiny::icon("gear"),
                      div(class = "settingsRow", fluidRow(column(6, offset = 6, tags$h3("Global Settings")))),
                      #### Color Mode =================================
                      fluidRow(column(6, offset = 6,
                        box(width = 12, title = "Color Mode", collapsible = TRUE, collapsed = TRUE, 
                            column(12, selectInput("colormode_global", label = "",
                                                  choices = c("Compare Watersheds" = "ws",
                                                              "Compare Solutes"="solute"),
                                                  selected = "solute"))))),
                      #### Granularity =================================
                      fluidRow(column(6, offset = 6,
                        box(width = 12, title = "Granularity", collapsible = TRUE, collapsed = TRUE,
                          column(12, selectInput("granularity_global", label = "",
                                                choices = granularity,
                                                selected = "year"))))), 
                      #### Axis =================================
                      fluidRow(column(6, offset = 6,
                        box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE,
                            column(12, selectInput("log_global_y", label = "Y Axis",
                                                  choices = c("linear", "log"), 
                                                  selected = "linear"))))),
                      #### Animation =================================
                      fluidRow(column(6, offset = 6,
                         box(width = 12, title = "Animation", collapsible = TRUE, collapsed = TRUE, 
                             ##Animate ? ############################# 
                             column(5, selectInput("animate_global", label = ("Animate"),
                                                   choices = c("Animate", "Still"), 
                                                   selected = "Still")),  
                             ##Leave trace ############################# 
                             column(6, selectInput("trace_global", label = ("Leave Trace"),
                                                   choices = c("Leave Trace", "No Trace"), 
                                                   selected = "Leave Trace")),
                             ##Animation Speed ############################# 
                             column(12, sliderInput("animation_speed_global", label = h4("Speed"),
                                                    min = 0.25,
                                                    max = 2, 
                                                    step = 0.25,
                                                    post = "x",
                                                    value = 1))
                             )))
                      ),
        
        
        ####### ---- Main Choose Watershed Tab -----------------------------------------------------
             tabPanel(shiny::icon("circle"),
                #### Watersheds  =================================
                fluidRow(column(4, tags$h3("Select Watershed")),
                  column(5, selectizeInput("watersheds", label = "",
                                      choices = watersheds, multiple = TRUE,
                                      selected = "6", 
                                      options = list(maxItems = 5))))
                )
        )),
        
        
      # PQ Graph Box >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>--------
        fluidRow(column(9,    
           fluidRow(
              tabBox(width = 12, side="right", selected = shiny::icon("circle"),
        
        ####### ---- PQ Settings Tab -----------------------------------------------------
            tabPanel(shiny::icon("gear"),
                    #### Axis =================================
                    fluidRow(column(6, offset = 6,
                                    box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE,
                                        column(6, selectInput("log_pq", label = "Y Axis",
                                                              choices = c("linear", "log"),
                                                              selected = "linear")))))),
        ####### ---- PQ Plot Tab -----------------------------------------------------
            tabPanel(shiny::icon("circle"),
                     div(class = "titleRow", fluidRow(column(5, tags$h2("Hydrologic Flux")),
                    #### Granularity =================================
                     column(3,  offset = 4, selectInput("granularity_pq", label = "",
                                                        choices = granularity,
                                                        selected = "year")))),
                    #### PQ Plot =================================
                      plotlyOutput("plot_pq")
            
           ) #Closes PQ Plot Tab
        )# Closes PQ Tab Box
      )# Closes PQ Row
      )),# Closes PQ Row and Column
      
        
      # Time Graph Box >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>--------
        fluidRow(column(9,    
            fluidRow(
              tabBox(width = 12, side="right", selected = shiny::icon("circle"),
      ####### ---- Time Settings Tab -----------------------------------------------------
                tabPanel(shiny::icon("gear"),
                     #### Axis =================================
                      fluidRow(column(6, offset = 6,
                                      box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE,
                                          column(6, selectInput("log_time", label = "Y Axis",
                                                                choices = c("linear", "log"),
                                                                selected = "linear"))))),
                     #### Animation =================================
                     fluidRow(column(6, offset = 6,
                                     box(width = 12, title = "Animation", collapsible = TRUE, collapsed = TRUE,
                        ##Animate ? ############################# 
                        column(12, selectInput("animate_time", label = ("Animate"),
                                             choices = c("Animate", "Still"), 
                                             selected = "Still")),
                        ##Leave trace #############################
                        column(12, selectInput("trace_time", label = ("Leave Trace"),
                                               choices = c("Leave Trace", "No Trace"),
                                               selected = "Leave Trace")),
                        ##Animation Speed #############################
                        column(12, sliderInput("animation_speed_time", label = h4("Speed"),
                                               min = 0.25, max = 2,step = 0.25,
                                               post = "x", value = 1)))))),
      ####### ---- Time Plot Tab -----------------------------------------------------
                tabPanel(shiny::icon("circle"),
                         div(class = "titleRow", fluidRow(column(5, tags$h2("Time Series Data")),
                    #### Granularity =================================
                    column(3,  offset = 4, selectInput("granularity_time", label = "",
                                                       choices = granularity,
                                                       selected = "year")))),
                    #### Time or Charge Plot =================================
                    conditionalPanel(condition = "input.yaxis_time != 'chargebalance'", plotlyOutput("plot_time")),
                    conditionalPanel(condition = "input.yaxis_time == 'chargebalance'", plotlyOutput("plot_charge"))
                    #verbatimTextOutput("brush")     
                ) #Closes Time Plot Tab Panel
                
            )# Closes Time Tab Box
         
      ),# Closes Time Row
        
      # CQ Graph Box >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>--------  
        fluidRow(
          tabBox(width = 6, side="right", selected = shiny::icon("circle"),
      ####### ---- CQ Settings Tab -----------------------------------------------------
              tabPanel(shiny::icon("gear"),
                    #### Axis =================================
                    fluidRow(column(12, offset = 0,
                                    box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE,
                                        # x axis
                                        column(5, selectInput("log_cq_x", label = "X Axis",
                                              choices = c("linear", "log"),
                                              selected = "linear")),
                                        # y axis
                                        column(5, selectInput("log_cq_y", label = "Y Axis",
                                            choices = c("linear", "log"), 
                                            selected = "linear"))))),
                    #### Animation =================================
                      fluidRow(column(12, offset = 0,
                        box(width = 12, title = "Animation", collapsible = TRUE, collapsed = TRUE,
                        ##Animate ? #############################
                            column(5, selectInput("animate_cq", label = ("Animate"),
                                                  choices = c("Animate", "Still"),
                                                  selected = "Still")),
                        ##Leave trace #############################
                            column(5, selectInput("trace_cq", label = ("Leave Trace"),
                                               choices = c("Leave Trace", "No Trace"), 
                                               selected = "Leave Trace")),
                        ##Animation Speed #############################
                            column(5, sliderInput("animation_speed_cq", label = h4("Speed"),
                                                  min = 0.25, max = 2, step = 0.25, post = "x",value = 1)))))),
      ####### ---- CQ Plot Tab -----------------------------------------------------
              tabPanel(shiny::icon("circle"),
              div(class = "titleRow", fluidRow(column(5, tags$h2("c-Q")),
                  #### Granularity =================================
                  column(4, offset = 2,selectInput("granularity_cq", label = "",
                                                   choices = granularity,
                                                   selected = "year")))),
                  #### CQ Plot =================================
              fluidRow(
                conditionalPanel(condition = "input.yaxis_time == 'concentration' || input.yaxis_time == 'chargebalance'", 
                                 plotlyOutput("plot_cq")))
              )#Closes Row  
          ),
      
      # Flux Graph Box >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>--------
          tabBox(width = 6, side="right", selected = shiny::icon("circle"),
          ####### ---- Flux Settings Tab -----------------------------------------------------
          tabPanel(shiny::icon("gear"),
                   #### Axis =================================
                   fluidRow(
                     box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE,
                         column(6, selectInput("log_flux", label = "Y Axis",
                                               choices = c("linear", "log"), 
                                               selected = "linear")))),
                   #### Animation =================================
                   fluidRow(
                     box(width = 12, title = "Animation", collapsible = TRUE, collapsed = TRUE,
                         ##Animate ? #############################
                         column(12, selectInput("animate_flux", label = ("Animate"),
                                                choices = c("Animate", "Still"), 
                                                selected = "Still")),  
                         ## Leave Trace #############################
                         column(12, selectInput("trace_flux", label = ("Leave Trace"),
                                                choices = c("Leave Trace", "No Trace"), 
                                                selected = "Leave Trace")),
                         ##Animation Speed #############################
                         column(12, sliderInput("animation_speed_flux", label = h4("Speed"),
                                                min = 0.25, max = 2, step = 0.25,post = "x",value = 1))))),
          
          ####### ---- Flux Plot Tab -----------------------------------------------------
          tabPanel(shiny::icon("circle"),
          div(class = "titleRow", fluidRow(column(5, tags$h2("Flux")),
                 #### Granularity =================================
                 column(4,  offset = 2, selectInput("granularity_flux", label = "",
                                                    choices = granularity,
                                                    selected = "year")))),
                #### Flux Plot =================================
                conditionalPanel(condition = "input.yaxis_time == 'concentration' || input.yaxis_time == 'chargebalance'", 
                           plotlyOutput("plot_flux"))
          ) #Closes tabpanel
          
        )# Closes tab Box
        
        )# Closes CQ and Flux Row
        
      ),#Closes timegraph and flux and pq column. 
       
      # Sidebar >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>-------- 
        column(3,
          box(width = 13, height = "1050px", id = "sidebar",
              #### Y_axis time =================================
              fluidRow(column(12,
                              selectInput("yaxis_time", label = "",
                                          choices = time_variables,
                                          selected = "flux"))),
              #### Water Sources =================================
              fluidRow(
                column(12, checkboxGroupInput("water_sources", label = h4("Water Sources"),
                                              choices = water_sources,
                                              selected = c("precipitation", "streamflow"),
                                              inline = FALSE))),
              #### Units  =================================  
              fluidRow(
                column(12, conditionalPanel(condition = "input.yaxis_time == 'concentration'",
                       selectInput("units", label = h4("Units"),
                                       choices = units,
                                       selected = "uEquivalent/L")))),
              
              #### Solutes =================================
              fluidRow(
                div(class="solutes_solutesmd", id = "solutes_col", column(12, conditionalPanel(condition = "input.yaxis_time == 'concentration' || input.yaxis_time == 'chargebalance'", 
                                            actionLink("select_all_ions", h4("Solutes")),
                  ##Cations #############################
                  actionLink("select_all_cations", h5("Cations")),
                  checkboxGroupInput("solutes_cations", label = "",
                                     choices = solutes_cations,
                                     selected = c("Na", "K")),
                  ##Anions #############################
                  actionLink("select_all_anions", h5("Anions")),
                  checkboxGroupInput("solutes_anions", label = "",
                                     choices = solutes_anions,
                                     selected = c("SO4")),
                  ##Hydrogen #############################
                  checkboxGroupInput("solutes_H", label = h4(""),
                                     choices = solutes_H,
                                     selected = ""))))),
              fluidRow(
                column(12, actionButton("go_exploratory", "UPDATE")))
              
            ) #Closes sidebar box
        )
  ) #Closes Time Graph Row.
      
      
),#Closes Dashboard tab item
      
      
      ################################################################################################
      #### ------------  End of Dashboard Tab Content  -----------------------------------------------
      #################################################################################################
      


      
      ################################################################################################
      #### ------------  Exploratory Bubble Tab Content  ----------------------------------------------
      #################################################################################################
      
      tabItem(tabName = "exploratory",
            fluidRow(
      # Exploratory Bubble Graph Box >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
              tabBox(width = 9, side="right", selected = shiny::icon("circle"),
               ####### ---- Bubble Graph Settings Tab -------------------------------------------------
               tabPanel(shiny::icon("gear"),
                        fluidRow(column(6, offset = 6,
                                        box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = TRUE,
                         #### Axis =================================
                        column(6, selectInput("log_bubble_x", label = "X Axis",
                                              choices = c("linear", "log"),
                                              selected = "linear")),
                        column(6, selectInput("log_bubble_y", label = "Y Axis",
                                              choices = c("linear", "log"),
                                              selected = "linear"))))),
                        #### Animation =================================
                        fluidRow(column(6, offset = 6,
                          box(width = 12, title = "Animation", collapsible = TRUE, collapsed = TRUE,
                              ##Animate ? #############################
                              column(4, selectInput("animate_bubble", label = ("Animate"),
                                                    choices = c("Animate", "Still"),
                                                    selected = "Still")),
                              ##Leave trace #############################
                              column(12, selectInput("trace_bubble", label = ("Leave Trace"),
                                                     choices = c("Leave Trace", "No Trace"),
                                                     selected = "Leave Trace")),
                              ##Animation Speed #############################
                              column(12, sliderInput("animation_speed_bubble", label = h4("Speed"),
                                                     min = 0.25, max = 2, step = 0.25, post = "x", value = 1)))))),
               
               ####### ---- Bubble Graph Plot Tab -------------------------------------------------
                tabPanel(shiny::icon("circle"), 
                         div(class = "titleRow", fluidRow(column(5, tags$h2("Exploratory Bubble Graph")),
                         #### Granularity =================================
                         column(4, offset = 2,selectInput("granularity_bubble", label = "",
                                                          choices = granularity,
                                                          selected = "year")))),
                         #### Bubble Plot =================================
                         #Solutes Y Input
                         fluidRow(column(4, offset = 1,textInput("solutesy_formula", label = "", value = "Ca + Na + Mg", 
                                                   placeholder = "type in desired formula")),
                               column(2, selectInput("solutesy_source", label = "", choices = c("P" = "precipitation", "Q" = "streamflow"),
                                                                 selected = "streamflow")), 
                               column(1, offset = 3, actionButton("go_bubble", "PLOT"))),
                         #Bubble Plot
                         fluidRow(div(style = "height:500px;", (plotlyOutput("bubblePlot")))),
                         #Solutes X Input
                         fluidRow(column(4, offset = 3, textInput("solutesx_formula", label = "", value = "SO4 + NO3",
                                                                  placeholder = "type in desired formula")),
                                  column(2, offset = 0, selectInput("solutesx_source", label = "", 
                                                                    choices = c("P" = "precipitation", "Q" = "streamflow"),
                                                                    selected = "streamflow"))))),
       # Sidebar >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>--------
                box(width = 3, height = 800,
                    #### Watersheds =================================
                    fluidRow(column(12, actionLink("select_all_ws", h4("Select Watersheds")),
                                    selectInput("watersheds_bubble", label = "",
                                                choices = watersheds, multiple = TRUE,
                                                selected = "6"))),
                    #### Units =================================
                    fluidRow(column(12, selectInput("units_bubble", label = h4("Select Units"),
                                                    choices = c(units, "flux"),
                                                    selected = "uEquivalent/L"))),
                    #### Date Range =================================
                    fluidRow(column(12, sliderInput("date_range_bubble", label = h4("Select Date Range"),
                                                    min = as.Date("1962-01-01"),
                                                    max = as.Date("2014-01-01"),
                                                    value = c(as.Date("1962-01-01"), 
                                                              as.Date("2014-01-01"))))),
                    #### Sizing =================================
                    fluidRow(column(12, selectInput("sizing_bubble", label = h4("Select Bubble Size"),
                                                 choices = c("None" = 1, 
                                                             "Precipitation (P)" = "water_mm_precipitation", 
                                                             "Streamflow (Q)" = "water_mm_streamflow")))),
                    
                    #### Coloring =================================
                    fluidRow(column(12, selectInput("coloring_bubble", label = h4("Select Bubble Color"),
                                                    choices = c("None" = 1, 
                                                                "Date" = "date", 
                                                                "Watershed" = "ws"))))
                    
              )#Closes Sidebar Box 
            ) #Closes Fluid Row
          )#Closes Exploratory Bubble tab
      
        ################################################################################################
        #### ------------  End of Exploratory Bubble Tab Content  --------------------------------------
        #################################################################################################
      
      )#Closes TabItems
    )#Closes Dashboard Body
  )#Closes dashboardPage
) #Closes ShinyUI

  