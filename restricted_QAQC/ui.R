# User Interface instructions for HBEF Dashboard
# Created by Maria-Carolina Simao (carolina.m.simao - at - gmail - dot - com)
# Initialized January 2018
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dygraphs)         # makes dynamic graphs used in QA/QC tab
library(rhandsontable)    # essential for "Excel-like Entry" tab
library(shiny)            # basis of entire app, allows us to create reactive dashboard

message("hello, I'm in ui.R")

# Lists ----
#*****************

# If you add to this list, must update colors_cations list as well
solutes_cations <- list("TOTAL Cation Charge" = "cationCharge",
                        "Calcium (Ca)" = "Ca", 
                        "Magnesium (Mg)" = "Mg", 
                        "Potassium (K)" = "K", 
                        "Sodium (Na)" = "Na", 
                        "TM Aluminum (Al)" = "TMAl", 
                        "OM Aluminum (Al)" = "OMAl", 
                        "Aluminum (Al) ICP" = "Al_ICP", 
                        "Ammonium (NH4)" = "NH4", 
                        "Manganese (Mn)" = "Mn", 
                        "Iron (Fe)" = "Fe")

# If you add to this list, must update colors_anions list as well
solutes_anions <- list("TOTAL Anion Charge" = "anionCharge",
                       "Sulfate (SO4)" = "SO4", 
                       "Nitrate (NO3)" = "NO3", 
                       "Chloride (Cl)" = "Cl", 
                       "Phosphate (PO4)" = "PO4", 
                       "Fluorine (F)" = "F")

# If you add to this list, must update colors_other list as well
solutes_other <- list("pH (3 Star)" = "pH", 
              "pH (Metrohm)"="pHmetrohm",
              "Dissolved Organic Carbon (DOC)" = "DOC", 
              "Total Dissolved Nitrogen (TDN)" = "TDN", 
              "Dissolved Organic Nitrogen (DON)" = "DON", 
              "Dissolved Inorganic Carbon (DIC)" = "DIC", 
              "Silica (SiO2)" = "SiO2", 
              "Acid Neutralizing Capacity 960" = "ANC960", 
              "Acid Neutralizing Capacity Met" = "ANCMet", 
              "Specific Conductivity" = "spCond", 
              "Theoretical Conductivity" = "theoryCond", 
              "Water Temperature" = "temp", 
              "Ion Balance" = "ionBalance")


# Lists of Sites 
#***************
sites_streams <- list("Watershed 1" = "W1",
                      "Watershed 2" = "W2", 
                      "Watershed 3" = "W3",
                      "Watershed 4" = "W4",
                      "Watershed 5" = "W5",
                      "Watershed 6" = "W6",
                      "Watershed 7" = "W7",
                      "Watershed 8" = "W8",
                      "Watershed 9" = "W9",
                      "HBK", 
                      "ML70",
                      "PLY")

#Precipitation sites
# If you update this list, also update conditional panel below
sites_precip <- list("RG11", "RG23", "STA/22", "N", "S") 

wateryears <- list("2014", "2015", "2016", "2017")

# water_sources <- list("Precipitation (P)" = "precipitation",
#                       "Streamflow (Q)" = "streamflow")
# 
# granularity <- list("Year (VWC)" = "year",
#                     "Month (VWC)" = "month",
#                     "Week" = "week")
# 
# units <- list("ueq/L","umol/L", "mg/L", "flux")


# **********************************************************************
#                    ---- USER INTERFACE CODE ----
# **********************************************************************

# Define User Interface for application (dashboard)

shinyUI(
   
   fluidPage(
     
      #includeCSS("style.css"),

      navbarPage(title = p(strong("HBEF Dashboard")),
         #*********************************************************
         # ***DATA INPUT tab *** ----
         #*********************************************************
         # Code initially copied from: https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
         navbarMenu("Data Input",
            tabPanel("Upload", # Upload Panel ----
               # Sidebar layout with input and output definitions
               sidebarLayout(
                  # Sidebar panel for inputs
                sidebarPanel(
                  # Input: Select a file
                  fileInput("FILE_UPLOAD", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                  ), 
                  actionButton("SUBMIT", label = "Submit"),
                  # Horizontal line
                  # tags$hr(),
                  # Input: Checkbox to indicate whether file has header
                  checkboxInput("HEADER", "Data includes header (column names)", TRUE),
                  # Input: Select separator to indicate how data is separated
                  radioButtons("sep", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"
                                ),
                                selected = ","
                  ),
                  # # Input: Select quotes
                  # radioButtons("quote", "Quote",
                  #              choices = c(None = "",
                  #                          "Double Quote" = '"',
                  #                          "Single Quote" = "'"),
                  #              selected = '"'),
                                  
                  # Horizontal line
                  # tags$hr(),
                  # Input: Select number of rows to display in output chart
                  radioButtons("disp", "Display",
                                choices = c(Head = "head", 
                                            All = "all"),
                                            selected = "head")
                  ), # end of sidebarPanel
                  mainPanel(
                     # Shows data file as a table 
                     tableOutput("FILE_PREVIEW")
                  ) # end of mainPanel            
               ) # end of Sidebar Layout
            ), # end of tabPanel for "Upload" 
     
            tabPanel("Excel-like Entry", # Excel-like Entry tab ----
               rHandsontableOutput("EXCEL")) 
         ), # END of "Data Input" navbarMenu 
        
         #*********************************************************
         # ***QA/QC tab ***----
         #*********************************************************
         # Each input is given an name ID based on the data it contains + the number
         # panel it's in, e.g. WATERYEAR1 (datatype+PanelNumber)
         # These names are what's used in the server.R file.
         navbarMenu("QA/QC", 
            tabPanel("1 Solute/1 Site", # Panel 1 - 1 Solute/1 Site ####
               sidebarLayout(
                  # Sidebar with tabs for Solute, Sites, Options
                  sidebarPanel(
                     selectInput(
                       "WATERYEAR1",
                       label = "Water Year",
                       choices = wateryears,
                       selected = 2014
                     ),
                     hr(),
                     selectInput(
                       "SOLUTES1",
                       label = "Solute",
                       choices = c(solutes_cations, solutes_anions, solutes_other),
                       selected = "Ca"
                     ),
                     helpText(textOutput("LIMITS1"), 
                              style = "color:#fc9272; font-size:85%;"
                     ),
                     hr(),
                     selectInput(
                       "SITES1",
                       label = "Site",
                       choices = c(sites_streams, sites_precip),
                       selected = "W1"
                     ),
                     hr(),
                     p(strong("Additional Options:")),
                     checkboxInput("HYDROLOGY1",
                                   label = "Hydrology",
                                   value = FALSE
                     ),
                     conditionalPanel(
                        # this panel only appears when discharge/precipitation button is clicked
                        condition = "input.HYDROLOGY1 == true", 
                        p(radioButtons("GAGEHT_or_Q1", 
                                       "Select data source:",
                                       choices = c("Gage Height (mm)" = "gageHt", 
                                                   "Q (L/s)" = "flowGageHt"),
                                       selected = "GageHt",
                                       inline = FALSE)
                        ), 
                        style = "color:#3182bd;"
                     ),
                     p("Hydrology shows discharge for watershed sites, 
                       and precipitation for rain gage sites." , 
                       style = "color:#666666; font-size:85%;"
                     ),
                     checkboxInput("SOLUTES_HIST1",
                                   label = "Historical Data",
                                   value = FALSE
                     ),
                     conditionalPanel( 
                        # this panel only appears if historical data is clicked and a rain gage site is selected
                        condition = "input.SOLUTES_HIST1 == true &&
                                     (input.SITES1 == 'RG11' ||
                                      input.SITES1 =='RG23' ||
                                      input.SITES1 == 'STA/22')", # !!! oppportunity to optimize code here: figure out how to make this a condition of whether site belong to 'sites_precip' list (instead of listing each site). operatior is 'in', but not working in way I'd assume
                        # placing radio button in paragraph tag to be able to make text desired color
                        p(radioButtons("N_or_S1", 
                                       "Site direction for historical data:",
                                       choices = c("North" = "N", 
                                                   "South" = "S"),
                                       selected = "N",
                                       inline = TRUE)
                        ), 
                        style = "color:#A9A9A9;"
                     ), 
                     conditionalPanel(
                        # this panel only appears when historical data is clicked
                        condition = "input.SOLUTES_HIST1 == true",
                        p("Although historical data are shown as continuous, 
                          these lines are derived from median values per month."
                        ), 
                        style = "color:#666666; font-size:85%;"
                     ),
                     width = 3
                  ), #closes sidebarPanel
                   
                  mainPanel(
                     # Where outputs ('results') are shown
                     #flowLayout(), 
                     fluidRow(column(width = 9, tags$h4(textOutput("TITLE1"))),    # Title
                              column(width = 3,                                    # Print button
                                     downloadButton("PRINT1", "Print Graph"), 
                                     class='rightAlign'
                              ) 
                     ),
                     hr(),
                     dygraphOutput("GRAPH1")
                     # for testing purposes
                     # hr(),
                     # h4("Table of Selected Data"),
                     # HTML("<p>Search bar finds specific values within selected data (e.g. '2014-06', '5.'). <br> Arrows (to the right of column names) sort data in ascending or descending order.</p>"),
                     # dataTableOutput("TABLE1") 
                  ) # closes mainPanel
                   
               ) #closes sidebarLayout
                 
            ), # END of Panel 1 tabPanel
         
            tabPanel("Multiple Solutes", # Panel 2 - Multiple Solutes #### 
               sidebarLayout(
                  # Sidebar with tabs for Solute, Sites, Options
                  sidebarPanel(
                     selectInput(
                        "WATERYEAR2",
                        label = "Water Year",
                        choices = wateryears,
                        selected = 2014
                     ),
                     selectInput("SITES2",
                                label = "Site",
                                choices = c(sites_streams, sites_precip),
                                selected = "W1"
                     ),
                     p(strong(checkboxInput("HYDROLOGY2",
                               label = "Hydrology",
                               value = FALSE))
                     ),
                     conditionalPanel(
                        # this panel only appears when discharge/precipitation button is clicked
                        condition = "input.HYDROLOGY2 == true", 
                        p(radioButtons("GAGEHT_or_Q2", 
                                      "Select data source:",
                                      choices = c("Gage Height (mm)" = "GageHt", 
                                                  "Q (L/s)" = "Q"),
                                      selected = "GageHt",
                                      inline = FALSE)
                        ), 
                        style = "color:#3182bd;"
                     ),
                     p("Hydrology shows discharge for watershed sites, and precipitation for rain gage sites." , 
                         style = "color:#666666; font-size:85%;"
                     ),
                     checkboxGroupInput("SOLUTES2", 
                                       label = "Solutes",
                                       choices = c(solutes_cations, solutes_anions, solutes_other),
                                       selected = "Ca"
                     ),
                     width = 3
                  ), # closes sidebarPanel
          
                  # Plot
                  mainPanel(
                     tags$h4(textOutput("TITLE2")),
                     hr(),
                     dygraphOutput("GRAPH2"),
                     #plotOutput("GRAPH")
                     hr(),
                     h4("Table of Selected Data"),
                     HTML("<p>Search bar finds specific values within selected 
                          data (e.g. '2014-06', '5.'). <br> Arrows (to the right
                          of column names) sort data in ascending or descending
                          order.</p>"
                     ),
                     # used when testing data sorting, but requested to keep
                     dataTableOutput("TABLE2")
                  ) # closes mainPanel
               ) # closes sidebarLayout
            ),# END of Panel 2 tabPanel
       
            tabPanel("Multiple Sites", # Panel 3 - Multiple Sites ####
               sidebarLayout(
                  # Sidebar with tabs for Solute, Sites, Options
                  sidebarPanel(
                     selectInput(
                       "WATERYEAR3",
                       label = "Water Year",
                       choices = wateryears,
                       selected = 2014
                     ),
                     selectInput("SOLUTES3",
                                label = "Solute",
                                choices = c(solutes_cations, solutes_anions, solutes_other),
                                selected = "Ca"
                     ),
                     helpText(textOutput("LIMITS3"), style = "color:#fc9272; font-size:85%;"),
                     radioButtons("HYDROLOGY3",
                                  label = "Hydrology (median):",
                                  choices = c("Discharge", "Precipitation", "None"),
                                  selected = "None"
                     ),
                     conditionalPanel(
                     # this panel only appears when discharge/precipitation button is clicked
                        condition = "input.HYDROLOGY3 == 'Discharge' || input.HYDROLOGY3 == 'Precipitation'", 
                        p(radioButtons("GAGEHT_or_Q3", 
                                        "Select hydrology data source:",
                                        choices = c("Gage Height (mm)" = "GageHt", 
                                                    "Q (L/s)" = "Q"),
                                        selected = "GageHt",
                                        inline = FALSE)), style = "color:#3182bd;"
                     ),
                     p("Discharge shows daily median of all watershed sites." , 
                       style = "color:#666666; font-size:85%;"
                     ),
                     p("Precipitation shows daily median of all rain gage sites." , 
                       style = "color:#666666; font-size:85%;"
                     ),
                     checkboxGroupInput("SITES3", 
                                       label = "Sites",
                                       choices = c(sites_streams, sites_precip),
                                       selected = "W1"
                     ),
                     width = 3
                  ), # closes sidebarPanel
                    
                  # Plot
                  mainPanel(
                       tags$h4(textOutput("TITLE3")),
                       hr(),
                       dygraphOutput("GRAPH3"),
                       #plotOutput("GRAPH")
                       hr(),
                       h4("Table of Selected Data"),
                       HTML("<p>Search bar finds specific values within selected data 
                            (e.g. '2014-06', '5.'). <br> Arrows (to the right of column names) 
                            sort data in ascending or descending order.</p>"
                        ),
                       # used when testing data sorting
                       dataTableOutput("TABLE3")
                       #textOutput("TEST3.TEXT")
                       
                  ) # closes mainPanel
               ) # closes sidebarLayout
            ), # END of Panel 3 tabPanel
        
            tabPanel("Free-for-all", # Panel 4 - Free-for-all ####
               sidebarLayout(
                 # Sidebar with tabs for Solute, Sites, Options
                  sidebarPanel(
                     tabsetPanel(
                        #********************
                        # General tab
                        #********************
                        tabPanel("General",
                           sliderInput(
                              "DATE4",
                              label = h4("Date Range"),
                              min =as.Date("1963-06-01"),
                              max = as.Date(maxDate),
                              value = as.Date(c(maxDate-365, maxDate)),
                              timeFormat = "%b %Y",
                              dragRange = TRUE
                           ),
                           checkboxInput(
                              "HYDROLOGY4",
                              label = "View hydrology",
                              value = TRUE
                           ),
                           # this panel only appears when hydology option is selected
                           conditionalPanel(
                              condition = "input.HYDROLOGY4 == true",
                              selectInput("PRECIP_SITE4",
                                          label = "Source of precipitation data:",
                                          choices = c(sites_precip),
                                          selected = "RG11"), # !!! need to field this from what's in data...
                              checkboxGroupInput(
                                 "PRECIP_SOURCE4",
                                 label = "Precipitation data source:",
                                 choices = c("Collector Catch" = "precipCatch",
                                             "ETI" = "precipETI"
                                 ),
                                 selected = "Collector Catch"
                              ),
                              selectInput("FLOW_SITE4",
                                          label = "Source of flow data:",
                                          choices = c(sites_streams),
                                          selected = "W1"), # !!! need to field this from what's in data...
                              checkboxGroupInput(
                                 "FLOW_SOURCE4",
                                 label = "Flow data source:",
                                 choices = c("Gage Height" = "gageHt",
                                             "Q (estimated from Gage Height)" = "flowGageHt",
                                             "Q (ETI)" = "flowSensor"
                                 ),
                                 selected = "Gage Height"
                              ),
                              checkboxInput("HYDROLIMB4",
                                          label = "Hydrograph limb",
                                          value = FALSE
                              ),
                              style = "color:#3182bd;"
                           ), #end of conditional panel
                           checkboxInput("FIELDCODE4",
                                         label = "Show field codes",
                                         value = FALSE
                           )
                        ), 
                        #********************
                        # Solutes tab
                        #********************
                        tabPanel("Solutes",
                           checkboxGroupInput("SOLUTES4",
                                               label = "Solute",
                                               choices = c(solutes_cations, solutes_anions, solutes_other),
                                               selected = "Ca"
                           ),
                           helpText(textOutput("LIMITS4"), 
                                    style = "color:#fc9272; font-size:85%;"
                           )
                        ),
                        #********************
                        # Sites tab
                        #********************
                        tabPanel("Sites",
                          checkboxGroupInput("SITES4", 
                                             label = "Sites",
                                             choices = c(sites_streams, sites_precip),
                                             selected = "W1"
                           )
                        ) #end of tabPanel "Sites"
                     ), #end of tabsetPanel
                     width = 3
                  ), # closes sidebarPanel
                       
                  # Plot
                  mainPanel(
                     tags$h4(textOutput("TITLE4")),
                     hr(),
                     plotOutput("GRAPH_PRECIP4"),
                     plotOutput("GRAPH_MAIN4"),
                     plotOutput("GRAPH_FLOW4"),
                     hr(),
                     h4("Table of Selected Data"),
                     HTML("<p>Search bar finds specific values within selected 
                          data (e.g. '2014-06', '5.'). <br> Arrows (to the right 
                          of column names) sort data in ascending or descending 
                          order.</p>"
                     ),
                     # used when testing data sorting
                     dataTableOutput("TABLE4")
                     #textOutput("TEST4.TEXT")
                  ) # closes mainPanel
               ) # closes sidebarLayout
            ), # Closes Panel 4 tabPanel
        
            tabPanel("Summary Table", # Panel 5 - Summary Table ####
               rHandsontableOutput("HOT5") # HOT = HandsOnTable
            ) # closes Panel 5 tabPanel
        
         ),# END of QA/QC navbarMenu
        
         #*********************************************************
         # ***DATA DOWNLOAD tab***----
         #*********************************************************
         tabPanel("Data Download"),
         #********************************************************* 
         # ***DATA ARCHIVE? tab*** ----
         #*********************************************************
         tabPanel("Approve & Archive Data")
        
         # *REMEMBER* that when data is archived, the data in 'initial' and 
         # 'current' tables should be cleared in MySQL hbef database
        
      ) # END of navbarPage()
   ) # closes fluidPage()  
) # closes shinyUI()

