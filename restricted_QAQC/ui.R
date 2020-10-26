# testing
# User Interface instructions for HBEF Dashboard
# Created by Maria-Carolina Simao (carolina.m.simao - at - gmail - dot - com)
# Initialized January 2018
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#   http://shiny.rstudio.com/
#

library(dygraphs)      # makes dynamic graphs used in QA/QC tab
library(lubridate)
library(rhandsontable)   # essential for "Excel-like Entry" tab
library(shiny)        # basis of entire app, allows us to create reactive dashboard
library(shinyWidgets) #for airDatepicker

message("hello, I'm in ui.R")


# **********************************************************************
#              ---- USER INTERFACE CODE ----
# **********************************************************************

# Define User Interface for application (dashboard)

shinyUI(
  fluidPage(
    titlePanel("", windowTitle = "HBEF Dashboard"),
    # useShinyjs(),
    #tags$head(
      #includeCSS("//cdnjs.cloudflare.com/ajax/libs/dygraph/2.1.0/dygraph.min.css"),
      #includeScript(path="//cdnjs.cloudflare.com/ajax/libs/dygraph/1.1.1/dygraph-combined.js")
    #),
    #includeCSS("style.css"),
    #HMTL(<script type="text/javascript" src="/www/dygraph-combined.js"></script>),
    navbarPage(title = p(strong("HBEF Dashboard")),
      tabPanel("Main",
        h3("New!", style="color:green"),
        p(HTML("Visit the <strong>Multiple Solutes</strong> tab to view raw, continuous water quality and sensor data."),
            style="color:green"),
        br(),
        p("Please send any issues or comments to Mike Vlah at",
          a(href="mailto:vlahm13@gmail.com?subject=[HBEF%20Dashboard]", "vlahm13@gmail.com"),
          style="font-size:85%;"
        ),
        tags$br(),
        h4("Helpful Resources:"),
        tags$div(HTML('Buso et al. 2000. <strong><a href=https://www.esf.edu/quest/documents/Busoetal.2000.HBstreamflowandchemistryGTR..pdf>
                  Chemistry of Precipitation, Streamwater, and Lakewater from the
                  Hubbard Brook Ecosystem Study: A Record of Sampling Protocols and
                  Analytical Procedures.</a></strong> USDA Forest Service, Northeastern Research
                  Station, General Technical Report NE-275.</li>')),
        tags$br(),
        downloadLink("DOWNLOAD_TEMPLATE", label = "Template for Data Uploads"),
        tags$br(),
        downloadLink("DOWNLOAD_TEMPLATE_EXAMPLE", label = "Template Example for Data Uploads")
      ), #end of Main tabPanel

      #*********************************************************
      # ***UPLOAD tab *** ----
      #*********************************************************
      # Code initially copied from: https://github.com/rstudio/shiny-examples/blob/master/009-upload/app.R
        navbarMenu("Upload",
          tabPanel("CSV file upload",
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
                # Input: Checkbox to indicate whether file has header
                checkboxInput("HEADER", "Data includes header (column names)", TRUE),
                # Input: Select number of rows to display in output chart
                p("Note: The above must be checked for upload to work." ,
                  style = "color:#666666; font-size:85%;"
                ),
                tags$hr(), # horizontal line
                radioButtons("UPLOAD_DISPLAY", "Display",
                         choices = c("First few rows" = "head",
                                 "All rows" = "all"),
                         selected = "head"
                ),
                tags$hr(), # horizontal line
                actionButton("SUBMIT", label = "Submit to Database")
                # Input: Select separator to indicate how data is separated
                # radioButtons("sep", "Separator",
                #          choices = c(Comma = ",",
                #                  Semicolon = ";",
                #                  Tab = "\t"
                #          ),
                #          selected = ","
                # ),
                # # Input: Select quotes
                # radioButtons("quote", "Quote",
                #          choices = c(None = "",
                #                  "Double Quote" = '"',
                #                  "Single Quote" = "'"),
                #          selected = '"'),
                ), # end of sidebarPanel
                mainPanel(
                  #tags$head(tags$script(src="www/dygraphs-1.1.1/dygraph-combined.js")),
                  # Shows data file as a table
                  DT::dataTableOutput("FILE_PREVIEW")
                ) # end of mainPanel
              ) # end of Sidebar Layout
          ), # END of "Upload" tabPanel
          tabPanel('Field notes upload',
              fluidRow(
                  column(12,
                      h2('Instructions'),
                      p(paste('Field notes should be compiled by date',
                          'and uploaded as PDF files. File names must begin',
                          'with the date, formatted as "YYYYMMDD", and end',
                          'with ".pdf".')),
                      p(paste('For standard weekly notes, the filename',
                          'should be e.g. "20190101 Notes.pdf". For event notes,',
                          'it might be something like "20190101 W9 rain on ',
                          'snow event.pdf".')),
                      p(paste('If the submission is successful, you will see a blue',
                          'notification in the lower right. Otherwise, you will',
                          'see a red error message.')),
                      br(),
                      fileInput('NOTE_UPLOAD', 'Choose PDF File(s)',
                          multiple=TRUE, accept='application/pdf'),
                      hr(),
                      actionButton('SUBMIT_NOTE', label='Submit')
                  )
              )
          )
      ),
      #*********************************************************
      # ***QA/QC tab ***----
      #*********************************************************
      # Each input is given an name ID based on the data it contains + the number
      # panel it's in, e.g. WATERYEAR1 (datatype+PanelNumber)
      # These names are what's used in the server.R file.
      navbarMenu("QA/QC",
        # Panel 1 - 1 Solute/1 Site ###############
        tabPanel("1 Solute/1 Site",
          sidebarLayout(
            # Sidebar with tabs for Solute, Sites, Options
            sidebarPanel(
              # radioButtons('wateryearOrRange1',
              #   'Choose date selection method',
              #   choices=c('Water Year'='wateryr', 'Date Range'='daterng')),
              # conditionalPanel(
              #   condition = "input.wateryearOrRange1 == wateryr",
                selectInput(
                  "WATERYEAR1",
                  label = "Water Year",
                  choices = rev(wateryears),
                  selected = wateryears[length(wateryears)]
                ),
              # ),
              # conditionalPanel(
              #   condition = "input.wateryearOrRange1 == daterng",
              #   selectInput(
              #    "DATERANGE1",
              #    label = "Date Range",
              #    # choices = wateryears,
              #    # selected = wateryears[1]
              #   )
              # ),
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

              # conditionalPanel(condition="input.SITES1 == 'W3'",
              #   p(paste('Prototype sensor data display below. Currently only',
              #     'available for watershed 3 and water year 2016.'),
              #     style='color:red'),
              #   selectInput("SENSORVAR1", label="Sensor variable overlay",
              #     choices=c('None', sensorvars)),
              #   hr()
              # ),
              # conditionalPanel(condition="input.SITES1 != 'W3'",

                checkboxInput(
                  "HYDROLOGY1",
                  label = "Hydrology",
                  value = FALSE
                ),
                conditionalPanel(
                  # this panel only appears when Hydrology button is clicked AND a stream site is selected
                  # !!! this condition could be optimized to be when sites %in% sites_stream (in javascript)
                  condition = "input.HYDROLOGY1 == true && (input.SITES1 == 'W1' || input.SITES1 == 'W2' || input.SITES1 == 'W3' || input.SITES1 == 'W4' || input.SITES1 == 'W5' || input.SITES1 == 'W6' || input.SITES1 == 'W7' || input.SITES1 == 'W8' || input.SITES1 == 'W9' || input.SITES1 == 'HBK' || input.SITES1 == 'ML70' || input.SITES1 == 'PLY')",
                  # sites_stream.includes(input.SITES1)
                 p(radioButtons("Flow_or_Precip1",
                  "Select data source:",
                  choices = c("Gage Height (ft)" = "gageHt",
                    "Q from Gage Height (L/s)" = "flowGageHt"),
                  selected = "gageHt",
                  inline = FALSE)
                 ),
                 style = "color:#3182bd;"
                ),
                p("Hydrology shows discharge for watershed sites,
                  and precipitation for rain gage sites." ,
                  style = "color:#666666; font-size:85%;"
                ),
              # ),
              checkboxInput("SOLUTES_HIST1",
                        label = "Historical Data",
                        value = FALSE
              ),
              conditionalPanel(
                # this panel appears when historical data is clicked
                condition = "input.SOLUTES_HIST1 == true",
                p("Although historical data are shown as continuous,
                  these lines are derived from median values per month."
                ),
                p("Historical data finds the median value of all stream sites
                  when a watershed site is selected, and the median value of all
                  precipitation sites when a rain gage site is selected."),
                style = "color:#666666; font-size:85%;"
              ),
              checkboxInput("OMIT_STORMS1",
                  label = "Omit Storm Data (Code 911)",
                  value = FALSE
              ),
              width = 3
            ), #closes sidebarPanel

            mainPanel(
              # Where outputs ('results') are shown
              #flowLayout(),
              fluidRow(column(width = 9, tags$h4(textOutput("TITLE1"))),   # Title
                    column(width = 3,                        # Print button
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
              # DT::dataTableOutput("TABLE1")
            ) # closes mainPanel

          ) #closes sidebarLayout

        ), # END of Panel 1 tabPanel

        # Panel 2 - Multiple Solutes ###############
        tabPanel("Multiple Solutes",
          sidebarLayout(
            # Sidebar with tabs for Solute, Sites, Options
            sidebarPanel(
              radioButtons("wateryearOrRange2",
                'Choose date selection method',
                choices=c('Water Year'='wateryr', 'Date Range'='daterng')),
              conditionalPanel(
                condition = "input.wateryearOrRange2 == 'wateryr'",
                selectInput(
                  "WATERYEAR2",
                  label = "Water Year",
                  choices = wateryears,
                  selected = wateryears[1])),
              conditionalPanel(
                condition = "input.wateryearOrRange2 == 'daterng'",
                sliderInput(
                  "DATE2",
                  label = "Date Range",
                  min =as.Date("1963-06-01"),
                  max = maxDate,
                  value = c(maxDate-365, maxDate),
                  width = "100%",
                  timeFormat = "%b %Y",
                  step = 30,
                  dragRange = TRUE)),
                            # selectInput(
              #   "WATERYEAR2",
              #   label = "Water Year",
              #   choices = wateryears,
              #   selected = wateryears[1]
              # ),
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
                # this panel only appears when Hydrology button is clicked AND a stream site is selected
                # !!! this condition could be optimized to be when sites %in% sites_stream (in javascript)
                condition = "input.HYDROLOGY2 == true && (input.SITES2 == 'W1' || input.SITES2 =='W2' || input.SITES2 == 'W3' || input.SITES2 == 'W4' || input.SITES2 == 'W5' || input.SITES2 == 'W6' || input.SITES2 == 'W7' || input.SITES2 == 'W8' || input.SITES2 == 'W9' || input.SITES2 == 'HBK' || input.SITES2 == 'ML70' || input.SITES2 == 'PLY')",
                p(radioButtons("Flow_or_Precip2",
                          "Select data source:",
                          choices = c("Gage Height (ft)" = "gageHt",
                                  "Q from Gage Height (L/s)" = "flowGageHt"),
                          selected = "gageHt",
                          inline = FALSE)
                ),
                style = "color:#3182bd;"
              ),
              p("Hydrology shows discharge for watershed sites,
                and precipitation for rain gage sites." ,
                style = "color:#666666; font-size:85%;"
              ),
              checkboxInput("OMIT_STORMS2",
                  label = "Omit Storm Data (Code 911)",
                  value = FALSE
              ),

               checkboxInput("SHOWSENS2",
                  label = HTML(paste0("Overlay continuous water quality data.<br>",
                     "(Available for watersheds 3, 6, and 9)")),
                  value = FALSE
               ),
               conditionalPanel(condition="input.SHOWSENS2 == true",
                  p(paste('Provisional sensor data. Not available for download.'),
                     style='color:red'),
                  selectInput("SENSORVAR2", label="Select variable",
                     choices=c('None', sensorvars)),
                  p(HTML('<strong>Adjust y-axis limits</strong>')),
                  p('(Accepts typed input)'),
                  column(width=6,
                      numericInput('YLIMlo2', label='', value=NULL)
                  ),
                  column(width=6,
                      numericInput('YLIMhi2', label='', value=NULL)
                  )
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

              conditionalPanel(condition="input.SHOWSENS2 == true && input.SENSORVAR2 != 'None'",
                 dygraphOutput("GRAPH2sens"),
              ),
              #plotOutput("GRAPH")
              hr(),
              h4("Table of Selected Data"),
              HTML("<p>Search bar finds specific values within selected
                  data (e.g. '2014-06', '5.'). <br> Arrows (to the right
                  of column names) sort data in ascending or descending
                  order.</p>"
              ),
              # used when testing data sorting, but requested to keep
              DT::dataTableOutput("TABLE2")
            ) # closes mainPanel
          ) # closes sidebarLayout
        ),# END of Panel 2 tabPanel

        # Panel 3 - Multiple Sites ###############
        tabPanel("Multiple Sites",
          sidebarLayout(
            # Sidebar with tabs for Solute, Sites, Options
            sidebarPanel(
              radioButtons('wateryearOrRange3',
                'Choose date selection method',
                choices=c('Water Year'='wateryr', 'Date Range'='daterng')),
              conditionalPanel(
                condition = "input.wateryearOrRange3 == 'wateryr'",
                selectInput(
                  "WATERYEAR3",
                  label = "Water Year",
                  choices = wateryears,
                  selected = wateryears[1]
                )
              ),
              conditionalPanel(
                condition = "input.wateryearOrRange3 == 'daterng'",
                sliderInput(
                  "DATE3",
                  label = "Date Range",
                  min =as.Date("1963-06-01"),
                  max = maxDate_current,
                  value = c(maxDate_current-365, maxDate_current),
                  width = "100%",
                  timeFormat = "%b %Y",
                  step = 30,
                  dragRange = TRUE
                )
              ),
              # selectInput(
              #  "WATERYEAR3",
              #  label = "Water Year",
              #  choices = wateryears,
              #  selected = wateryears[1]
              # ),
              selectInput("SOLUTES3",
                      label = "Solute",
                      choices = c(solutes_cations, solutes_anions, solutes_other),
                      selected = "Ca"
              ),
              helpText(textOutput("LIMITS3"),
                    style = "color:#fc9272; font-size:85%;"),
              radioButtons("HYDROLOGY3",
                       label = "Hydrology (median):",
                       choices = c("Discharge", "Precipitation", "None"),
                       selected = "None"
              ),
              conditionalPanel(
              # this panel only appears when discharge/precipitation button is clicked
                condition = "input.HYDROLOGY3 == 'Discharge'",
                p(radioButtons("Flow_or_Precip3",
                           "Select discharge data source:",
                           choices = c("Gage Height (ft)" = "gageHt",
                                   "Q from Gage Height (L/s)" = "flowGageHt"),
                           selected = "gageHt",
                           inline = FALSE)), style = "color:#3182bd;"
              ),
              p("Discharge shows daily median of all watershed sites." ,
                style = "color:#666666; font-size:85%;"
              ),
              p("Precipitation shows daily median of all rain gage sites." ,
                style = "color:#666666; font-size:85%;"
              ),
              checkboxInput("OMIT_STORMS3",
                  label = "Omit Storm Data (Code 911)",
                  value = FALSE
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
                DT::dataTableOutput("TABLE3")
                #textOutput("TEST3.TEXT")

            ) # closes mainPanel
          ) # closes sidebarLayout
        ), # END of Panel 3 tabPanel

        # Panel 4 - Free-for-all ###############
        tabPanel("Free-for-all",
          sidebarLayout(
            # Sidebar with tabs for Solute, Sites, Options
            sidebarPanel(
              tabsetPanel(
                #********************
                # General tab
                #********************
                tabPanel("General",
                  br(),

                  # Options for "Precipitation" Graph
                  #**********************************
                  p("Precipitation", style = "font-weight:bold; font-size:1.1em;"),
                  checkboxInput("PRECIP4_OPTION",
                        label = p("Show graph", style = "font-size:0.9em; color:#919191;"),
                        value = TRUE
                  ),
                  # below only appears if "Precipitation" is selected
                  conditionalPanel(
                    condition = "input.PRECIP4_OPTION == true",
                    radioButtons(
                      "PRECIP_SOURCE4",
                      label = "Precip data source:",
                      choices = c("Collector Catch (mm)" = "precipCatch",
                              "ETI" = "precipETI"
                      ),
                      selected = "precipCatch"
                    ),
                    style = "color:#919191; font-size:0.9em;"
                  ), #end of conditional panel

                  hr(),

                  # Options for "Solutes" Graph
                  #****************************
                  p("Solutes", style = "font-weight:bold; font-size:1.1em;"),
                  checkboxInput("SOLUTE4_OPTION",
                            label = p("Show graph", style = "font-size:0.9em; color:#919191;"),
                            value = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.SOLUTE4_OPTION == true",
                    checkboxInput("FIELDCODE4",
                              label = "Show field codes",
                              value = FALSE
                    ),
                    checkboxInput("OMIT_STORMS4",
                        label = "Omit Storm Data (Code 911)",
                        value = FALSE
                    ),
                    selectInput("SOLUTES4_COLOR",
                            label = "Colors apply to:",
                            choices = c("Solutes", "Sites"),
                            width = "80%"),
                    style = "color:#919191; font-size:0.9em;"
                  ),
                  # checkboxInput("SHOWSENS4",
                  #     label = "Overlay continuous water quality data",
                  #     value = FALSE
                  # ),
                  # conditionalPanel(condition="input.SHOWSENS4 == true",
                  #   p(paste('Provisional sensor data. Not available for download.'),
                  #     style='color:red'),
                  #   selectInput("SENSORVAR4", label="Select variable",
                  #     choices=c('None', sensorvars)),
                  # ),

                  hr(),

                  # Options for "Discharge" Graph
                  #******************************
                  p("Discharge", style = "font-weight:bold; font-size:1.1em;"),
                  checkboxInput("DISCHARGE4_OPTION",
                            label = p("Show graph", style = "font-size:0.9em; color:#919191;"),
                            value = TRUE
                  ),
                  # below only appears if "Discharge" is selected
                  conditionalPanel(
                    condition = "input.DISCHARGE4_OPTION == true",
                    checkboxInput("HYDROLIMB4",
                              label = "Add hydrograph limb",
                              value = FALSE
                    ),
                    p("Discharge data sources:", style = "font-weight:bold; text-decoration:underline;"),
                    p(selectInput("FLOW_SITE4",
                              label = p("Data from site:", style = "font-weight:bold"),
                              choices = c(sites_streams),
                              selected = "W1",
                              width = "80%"),
                      style = "margin-bottom:0px; font-size:0.9em;"
                    ),
                    radioButtons(
                      "FLOW_SOURCE4",
                      label = p("Data type:", style = "font-weight:bold"),
                      choices = c("Gage Height (ft)" = "gageHt",
                              "Q from Gage Height (L/s)" = "flowGageHt",
                              "Q from Sensor (L/s)" = "flowSens"
                      ),
                      selected = "gageHt"
                    ),
                    style = "color:#919191; margin-top:0px; font-size:0.9em;"
                  ) #end of conditional panel
                ),
                #********************
                # Solutes tab
                #********************
                tabPanel("Solutes",
                  checkboxGroupInput("SOLUTES4",
                                label = "",
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
                              label = "",
                              choices = c(sites_streams, sites_precip),
                              selected = "W1"
                  )
                ) #end of tabPanel "Sites"
              ), #end of tabsetPanel
              width = 3
            ), # closes sidebarPanel

            # Plot
            mainPanel(
              wellPanel(
                   sliderInput(
                     "DATE4",
                     label = "Date Range",
                     min =as.Date("1963-06-01"),
                     max = as.Date(maxDate),
                     value = as.Date(c(maxDate-365, maxDate)),
                     width = "100%",
                     timeFormat = "%b %Y",
                     step = 30,
                     dragRange = TRUE)
              ),
              #tags$h4(textOutput("TITLE4")),
              conditionalPanel(
                condition = "input.PRECIP4_OPTION == true",
                fluidRow(
                  column(12, style = "height:100px;",
                      plotOutput("GRAPH_PRECIP4"))
                )
              ),
              conditionalPanel(
                condition = "input.SOLUTE4_OPTION == true",
                fluidRow(
                  column(12, style = "height:350px;",
                      plotOutput("GRAPH_MAIN4"))
                )
              ),
              conditionalPanel(
                condition = "input.DISCHARGE4_OPTION == true",
                fluidRow(
                  column(12, style = "height:100px;",
                       plotOutput("GRAPH_FLOW4"))
                )
              )
              # use for when testing data selection
              # hr(),
              # h4("Table of Selected Data"),
              # HTML("<p>Search bar finds specific values within selected
              #    data (e.g. '2014-06', '5.'). <br> Arrows (to the right
              #    of column names) sort data in ascending or descending
              #    order.</p>"
              # ),
              # used when testing data sorting
              # DT::dataTableOutput("TABLE4")
            ) # closes mainPanel
          ) # closes sidebarLayout
        ), # Closes Panel 4 tabPanel

        # Panel 5 - Data & Edits ###############
        tabPanel("Data & Edits",
          # change in CSS to make a label in line with the input box
          # to use, wrap input function in tags$div(id = "inline", [input function])
          tags$head(tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
          #inline .form-group { display: table-row;}")),
          fluidRow(
            column(3,
              selectInput(
                "WATERYEAR5",
                label = "Water Year",
                choices = wateryears_current,
                selected = wateryears_current[1]
              )),
            column(3,
              selectInput(
                 "SITES5",
                 label = "Site",
                 choices = c(sites_streams, sites_precip),
                 selected = "W1"
              )),
            column(4),
            column(2,
                 actionButton(
                   "SAVECHANGES5",
                   label = strong("Save Changes"),
                   class='rightAlign'
                 ))
          ), #end of fluidRow
          rHandsontableOutput("HOT"), # HOT = Hands On Table
          hr(),
          wellPanel(style = "background: darkgray;",
          span(p("- - - - - - - - - - - - -
               *CAUTION* Section for Deleting Data - - - - - - - - - - - - -"),
              style="text-align: center;"),
          fluidRow(
            # Column for "Delete Rows" button
            column (8,
                 wellPanel(style = "background: black;
                              color: white;",

                  # Date options
                  fluidRow(

                    column(6,
                        span(h4("Delete by Date(s) & Site(s)"), style="color: white;"),
                        # Date input panels, depending on selected date option
                        conditionalPanel("input.DELETE_DATEOPTION5 == 'Date'",
                            airDatepickerInput('DELETE_DATE5', 'Date:',
                                addon='none', clearButton=TRUE, update_on='close',
                                placeholder='No dates selected')
                        ),
                        conditionalPanel("input.DELETE_DATEOPTION5 == 'Date Range'",
                          airDatepickerInput('DELETE_DATERANGE5', 'Date(s):',
                              addon='none', clearButton=TRUE, update_on='close',
                              placeholder='No dates selected', range=TRUE),
                          p("Dates listed above will also be deleted." ,
                           style = "color: white; font-size:80%;"
                          )
                        ),
                        radioButtons(
                          "DELETE_DATEOPTION5",
                          label = "Date Input Options:",
                          choices = c("Date", "Date Range"),
                          selected = "Date"
                          #inline = TRUE
                        )
                    ), # end of column

                    # Site option & Delete Section button
                    column(6,
                        selectInput(
                          "DELETE_SITE5",
                          label = "Site:",
                          choices = c("All Sites", sites_streams, sites_precip),
                          selected = "All Sites"
                        ),
                        br(),
                        actionButton(
                          "BUTTON_DELETE5",
                          label = strong("Delete Rows"),
                          class = 'rightAlign'
                        )
                    ) # end of column

                  ) # end of fluidRow
                 ) # end of well Panel
           ), #end of Column for "Delete Rows" button

           # Column for "Delete 1 Row" button
           column(4,
              wellPanel(style = "background: black;
                           color: white;",
                span(h4("Delete by UniqueID"), style="color: white;"),
                textInput(
                 "DELETE_UNIQUEID5",
                 label = "UniqueID:  "
                ),
                br(),
                actionButton(
                 "BUTTON_DELETEROW5",
                 label = strong("Delete 1 Row"),
                 class = 'rightAlign'
                )

              ) # end of well Panel
          ) # end of Column for "Delete 1 Row" button

        ) # end of fluidRow
        )
        ) # closes Panel 5 tabPanel

      ),# END of QA/QC navbarMenu

      #*********************************************************
      # ***DOWNLOAD tab***----
      #*********************************************************
      navbarMenu("Download",
        tabPanel("Numeric data download",
          sidebarLayout(
            sidebarPanel(
              selectInput("DOWNLOAD_DATASET", "Choose a dataset:",
                      choices = c("Current",
                              # "Initial",
                              # "Chemistry",
                              "Historical",
                              "All")),
              radioButtons("DOWNLOAD_FILETYPE", "File type:",
                       choices = c("csv",
                               "tsv")),
              downloadButton('DOWNLOAD_DATA', 'Download')
            ),
            mainPanel(
              # tableOutput("table")
            )
          ) # end of sidebarLayout
        ), # end of tabPanel for Data Download
        tabPanel('Field notes download',
            fluidRow(
                column(12,
                    h3('Download field note PDFs'),
                    p(paste('Dates for which field notes are available appear',
                        'in black. All note sets collected on a single day',
                        'will be downloaded together.')),
                    br(),
                    airDatepickerInput('NOTE_DATES', 'Choose date(s)',
                        multiple=TRUE, range=FALSE, separator=', ',
                        minDate=field_note_daterange[1],
                        maxDate=field_note_daterange[2],
                        disabledDates=no_note_days, addon='none',
                        clearButton=TRUE, update_on='close',
                        placeholder='No dates selected'),
                    br(),
                    downloadButton('DOWNLOAD_NOTES', 'Download')
                )
            )
        )
      )
      #*********************************************************
      # ***DATA ARCHIVE? tab*** ----
      #*********************************************************
      # tabPanel("Approve & Archive Data")

      # *REMEMBER* that when data is archived, the data in 'initial' and
      # 'current' tables should be cleared in MySQL hbef database

    ) # END of navbarPage()
  ) # closes fluidPage()
) # closes shinyUI()

