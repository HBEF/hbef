# testing
# User Interface instructions for HBEF Dashboard

library(dygraphs)      
library(lubridate)
library(rhandsontable)
library(shiny)
library(shinyWidgets) #for airDatepicker

# Define User Interface for application (dashboard)

shinyUI(
    fluidPage(
      tags$style(type="text/css", ".recalculating { opacity: 1.0; }"),
        titlePanel("Sensor Data Viewer"),
              tabPanel("Sensor Data",
                       sidebarPanel(
                           sliderInput(
                               "DATE2",
                               label = "Select Date Range",
                               min =as.Date("2019-01-01"),
                               max = maxDate,
                               value = c(maxDate-365, maxDate),
                               width = "100%",
                               timeFormat = "%b %Y",
                               step = 30,
                               dragRange = TRUE),
                           selectInput("SITES2",
                                       label = "Site",
                                       choices = sites_streams,
                                       selected = "W3"
                                       ),
                           # p(strong(checkboxInput("HYDROLOGY2",
                           #                        label = "Hydrology",
                           #                        value = FALSE))
                           #   ),
                           # conditionalPanel(
                           #     # this panel only appears when Hydrology button is clicked AND a stream site is selected
                           #     # !!! this condition could be optimized to be when sites %in% sites_stream (in javascript)
                           #     condition = "input.HYDROLOGY2 == true && (input.SITES2 == 'W1' || input.SITES2 =='W2' || input.SITES2 == 'W3' || input.SITES2 == 'W4' || input.SITES2 == 'W5' || input.SITES2 == 'W6' || input.SITES2 == 'W7' || input.SITES2 == 'W8' || input.SITES2 == 'W9' || input.SITES2 == 'HBK' || input.SITES2 == 'ML70' || input.SITES2 == 'PLY')",
                           #     p(radioButtons("Flow_or_Precip2",
                           #                    "Select data source:",
                           #                    choices = c("Gage Height (ft)" = "gageHt",
                           #                                "Q from Gage Height (L/s)" = "flowGageHt"),
                           #                    selected = "gageHt",
                           #                    inline = FALSE)
                           #       ),
                           #     style = "color:#3182bd;"
                           #     ),
                           #     p("Hydrology shows discharge for watershed sites,
                           #       and precipitation for rain gage sites.",
                           #       style = "color:#666666; font-size:85%;"
                           #       ),
                           # checkboxInput("OMIT_STORMS2",
                           #               label = "Omit Storm Data (Code 911)",
                           #               value = FALSE
                           #               ),
                           # p(paste('Provisional sensor data. Not available for download.'),
                           #   style='color:red'),
                           selectInput("SENSORVAR2", label="Select sensor variable",
                                       choices = sensor_name_map),
                           p(HTML('<strong>Adjust y-axis limits</strong>')),
                           p('(Accepts typed input)'),
                           column(width=6,
                                  numericInput('YLIMlo2', label='', value=NULL)
                                  ),
                           column(width=6,
                                  numericInput('YLIMhi2', label='', value=NULL)
                                  ),
                           checkboxGroupInput("GRAB_ADD2",
                                              label = "Aditional Options:",
                                              choices = "Overlay grab samples"
                           ),
                           checkboxGroupInput("OPTIONS2",
                                              label = "",
                                              choices = "Add line to sensor data",
                                              selected = "Add line to sensor data"
                                              ),
                           conditionalPanel("input.GRAB_ADD2 == 'Overlay grab samples'",
                                            selectInput("SOLUTES2",
                                       label = "Solute Grab Samples",
                                       choices = solutes_other)),
                           # conditionalPanel("input.ADD_GRAB2 == 'Add Hydrology'",
                           #                  radioButtons("HYDROLOGY2",
                           #                               "Hydrology options:",
                           #                               choices = c("Gage Height (ft)" = "gageHt",
                           #                                           "Q from Gage Height (L/s)" = "flowGageHt"),
                           #                               selected = "flowGageHt",
                           #                               inline = FALSE)),
                           width = 3
                           ), # closes sidebarPanel
                       # Plot
                       mainPanel(
                           tags$h4(textOutput("TITLE4")),
                           tags$h6("LIVE STREAMED PROVISIONAL DATA - NOT FOR PUBLICATION"),
                           hr(),
                           dygraphOutput("GRAPGH4"),
                           hr()
                   )
        ) # END of tabPanel()
    ) # closes fluidPage()
) # closes shinyUI()

