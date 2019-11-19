# Shiny server instructions for HBEF Dashboard
# Created by Maria-Carolina Simao (carolina.m.simao - at - gmail - dot - com)
# Initialized January 2018

# Search for '!!!' (without quotes) to find alerts or issues within the following code

# Some code borrowed and updated from https://github.com/akl21/hbef/blob/dev/data_stories/acid_rain/server.R

Sys.setenv(TZ='America/New_York')
options(shiny.maxRequestSize = 1024*1024^2) # allows max file upload size to be 1GB
options(show.error.locations=TRUE) # show error locations

library(colorspace)
library(dplyr)
library(dygraphs)       # allows for interactivity
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(plotly)
library(lubridate)      # Does not work with shinyapps.io: https://stackoverflow.com/questions/28656683/r-script-working-locally-not-working-on-shinyapp-io
library(RColorBrewer)
library(RMariaDB)
library(RMySQL)
library(rhandsontable)
library(reshape2)
library(shiny)
library(stringr)        # needed for str_extract function
library(tidyr)
library(xts)

message("hello, I'm at the top of server.R")

# **Database Password**
# SWITCH DEPENDING ON LOCATION
pass  = readLines('/home/mike/RMySQL.config')   # for remote server
# pass = readLines('~/git/hbef/RMySQL.config')   # for MV's local computer
#pass = readLines('SQL.txt')              # for CSR's local computer

# ***********************************************************************
#              ---- IMPORTANT PRELIMINARY INFO ----
# ***********************************************************************

# Functions ----
# ***********************************

# Replaces codes -999.9, -1, -2, and -3 from data (used before graphing)
removeCodes <- function(dataSet) {
  # if value -999.9 is present in certain columns, replace with NA
  for (i in 1:6) {
    # test data set when needed:
    # test<-dataAll[which(dataAll$temp == -999.9),] #selects all temp -999.9
    current_col_ofData <- codes999.9[i]
    if (current_col_ofData %in% names(dataSet)) {
      ind_col <- which(current_col_ofData == colnames(dataSet), arr.ind = TRUE)
      if (current_col_ofData == "timeEST") {
        dataSet[ind_col][dataSet[ind_col] == "-9999"] <- NA
        # above is essentially the same as:
        # dataAll2$timeEST[dataAll2$timeEST==-999.9] <- NA
      } else {
        dataSet[ind_col][dataSet[ind_col] == -999.9] <- NA
      }
    }
  }
  # if values are -1, -2, or -3, replace with NA
  for (i in 1:23) {
    current_col_ofData <- codes123[i]
    if (current_col_ofData %in% names(dataSet)) {
      ind_col <- which(current_col_ofData == colnames(dataSet), arr.ind = TRUE)
      dataSet[ind_col][dataSet[ind_col] == -1] <- NA
      dataSet[ind_col][dataSet[ind_col] == -2] <- NA
      dataSet[ind_col][dataSet[ind_col] == -3] <- NA
    }
  }

  return(dataSet)
}

# Serves same function as removeCodes() function, but tailored for specific
# data format used in Panel 3
removeCodes3 <- function(dataSet, solute) {
  sites_all <- c(sites_streams, sites_precip)
  # Go through each column of data, and change the
  # columns associated with a site (i.e. the ones that contain solute data)
  c <- ncol(dataSet)
  for (i in 1:c) {
    if (names(dataSet[i]) %in% sites_all) {
      if (solute %in% codes999.9) {
        dataSet[i][dataSet[i] == -999.9] <- NA
      }
      if (solute %in% codes123) {
        dataSet[i][dataSet[i] == -1] <- NA
        dataSet[i][dataSet[i] == -2] <- NA
        dataSet[i][dataSet[i] == -3] <- NA
      }
    }
  }
  return(dataSet)
}


# **** END of Functions ****

# Theme  ----
#******************

# Graph theme
my_theme <- theme_fivethirtyeight() +
  theme(rect = element_rect(fill = NA),
      panel.grid.major = element_line(colour = "#dddddd"),
      text = element_text(family = "Arial", size = 14),
      legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal",
        legend.box.just = "left", legend.title = element_blank(),
        #legend.key.size = unit(2.5, "native"),
      strip.text = element_text(hjust = 1, size = 20, face = "bold"),
      axis.title= element_text(NULL), axis.title.x= element_blank(),
      axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

# Set up color palette for solutes (using 'qual', or qualitative, color palette)
n <- 30 # number of colors
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=(col_vector[1:n])) # to see color wheel

# **** END of Theme ****

# **** END OF IMPORTANT PRELIMINARY INFO ****


# **********************************************************************
#                  ---- SHINY SERVER ----
# **********************************************************************

shinyServer(function(input, output, session) {

  # show start date and time
  message(paste("App opened:", Sys.time()))

  # make sure app stops upon closing browser
  session$onSessionEnded(function() {
      stopApp()
  })

  # ***REACTIVITY*** ----
  # ***********************************

  # Create reactive value which will be used to signal when core data (e.g. 'current')
  # has changed and should be updated. Anytime current data is changed, the value
  # of this variable should be increased by 1.
  changesInData <- reactiveValues()
  changesInData$change_dataCurrent <- 0
  changesInData$change_dataAll <- 0

  # Make a reactive dataAll2 data frame, to be called whenever data is updated
  # (R in dataCurrentR stands for reactive)
  dataCurrentR <- eventReactive(changesInData$change_dataCurrent, {

    # Open database connection
    y = RMariaDB::MariaDB()
    con = dbConnect(y,
               user = 'root',
               password = pass,
               host = 'localhost',
               dbname = 'hbef')

    # Read current data and disconnect from table
    dataCurrentR <- dbReadTable(con, "current")
    message(print(class(dataCurrentR)))
    message(head(dataCurrentR))
    dataCurrentR <- as.data.frame(dataCurrentR)
    message(print(class(dataCurrentR)))
    message(head(dataCurrentR))
    dbDisconnect(con)

    # Clean up data
    dataCurrentR <- standardizeClasses(dataCurrentR)
    # substituting commas with semi-colons. (necessary to prevent problems when downloading csv files)
    dataCurrentR$notes <- gsub(",", ";", dataCurrentR$notes)
    dataCurrentR$sampleType <- gsub(",", ";", dataCurrentR$sampleType)

    # Re-calculate and assign water year variable for current data
    wy_current <- levels(as.factor(dataCurrentR$waterYr))
    wy1_current <- c()
    for (i in 1:length(wy_current)) {
      wy1_current <- c(wy1_current, wy_current[i])
    }
    #wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
    wateryears_current <<- as.list(wy1_current) #assign it globally

    # Update Panel 5 user interface
    updateSelectInput(session, "WATERYEAR5", label = "Water Year", choices = wateryears_current)

    # Trigger update in dataAll
    changesInData$change_dataAll <- changesInData$change_dataAll + 1

    dataCurrentR

  })


  dataAllR <- eventReactive(changesInData$change_dataAll, {
    dataAllR <- bind_rows(select(dataHistorical, -canonical), dataCurrentR())
    dataAllR <- standardizeClasses(dataAllR)

    # Re-calculate and assign water year variable for all data
    wy <- levels(as.factor(dataAllR$waterYr))
    wy1 <- c()
    for (i in 1:length(wy)) {
      wy1 <- c(wy1, wy[i])
    }
    #wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
    wateryears <<- as.list(wy1) #assign it globally

    # Get new maximum date ----
    # used in ui.R for Panel 4 (QA/QC "Free-for-all" graph)
    maxDate <- max(dataAllR$date, na.rm=TRUE)

    # Update Panel 1-4 user interfaces
    updateSelectInput(session, "WATERYEAR1", label = "Water Year", choices = wateryears)
    updateSelectInput(session, "WATERYEAR2", label = "Water Year", choices = wateryears)
    updateSelectInput(session, "WATERYEAR3", label = "Water Year", choices = wateryears)
    updateSliderInput(session, "DATE4",
                label = "Date Range",
                value = as.Date(c(maxDate-365, maxDate)),
                min =as.Date("1963-06-01"),
                max = as.Date(maxDate),
                step = 30)

    dataAllR

  })

  # *Upload Tab* ####
  #************************

  # Upon choosing csv file, grabs and displays file contents
  dataNew <- eventReactive(input$FILE_UPLOAD,{

      #for testing
      #dataNew <-read.csv("data/tests/test_current.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
      message("in dataNew() because of FILE_UPLOAD.")
      dataNew <- read.csv(input$FILE_UPLOAD$datapath,
                    header = input$HEADER,
                    stringsAsFactors = FALSE,
                    na.strings=c(""," ","NA"))
      dataNew <- dataNew[rowSums(is.na(dataNew)) !=ncol(dataNew),] # remove rows with all NA's
      if ("date" %in% names(dataNew)) {
        dataNew$date <- as.Date(dataNew$date, "%m/%d/%y")
      }
      # message(paste("Head of dataNew:",print(head(dataNew))))
      return(dataNew)
   })

  # Upon pressing submit, transfer uploaded file content to 'current' table in database
  observeEvent(input$SUBMIT, {
    # !!! will likely want to make this more advanced later (only show success if there are no errors)
    message("in submit")
    # opening connection to database
    con = dbConnect(MariaDB(),
               user = 'root',
               password = pass,
               host = 'localhost',
               dbname = 'hbef')

    # make needed data type changes to data before uploading
    dataNew <- standardizeClasses(dataNew())

    # upload data
    dbWriteTable(con, "current", dataNew, append=TRUE, row.names=FALSE)

    # close connection to database
    dbDisconnect(con)

    # update reactive value to signal core data has changed,
    # so that dataCurrent & dataAll are recalculated
    changesInData$change_dataCurrent <- changesInData$change_dataCurrent + 1

    showNotification("Submit Complete.", type='message')

  })

  observeEvent(input$SUBMIT_NOTE, {

      ff = input$NOTE_UPLOAD
      if (is.null(ff)) return()
      fn = ff$name

      if(any(! grepl('^[0-9]{8}.*?\\.pdf', fn))){
          showNotification(paste('Filename(s) must begin with the date as',
              'YYYYMMDD and end with ".pdf".'), type='error', duration=NULL,
              closeButton=TRUE)
          return()
      }

      file.copy(ff$datapath, file.path("field_notes", fn) )
      showNotification(paste(length(fn), "file(s) submitted."),
          type='message')
  })

  # *QA/QC Tab* ####
  #************************


  # Panel 1 Reactivity ####
  #************************

  #add sensor Q option to hydrology radio buttons if site W1-W9 selected
  observeEvent(input$SITES1, {

    current_selection = input$Flow_or_Precip1
    ws_site_selected = grepl("^W[0-9]+$", input$SITES1)

    #>>>>temp fix for prototype sensor data plotting; once it's universal, can delete this?
    if(input$SITES1 == 'W3'){
        updateCheckboxInput(session, inputId='HYDROLOGY1', value=FALSE)
    }
    if(input$SITES1 != 'W3'){
        updateSelectInput(session, inputId='SENSORVAR1', selected='None')
    }

    if(! ws_site_selected && current_selection == 'flowSens'){
      current_selection = 'flowGageHt'
    }

    flow_opts = c("Gage Height (ft)" = "gageHt",
              "Q from Gage Height (L/s)" = "flowGageHt")
    if(ws_site_selected){
      flow_opts = append(flow_opts,
                   c("Q from Sensor (L/s)" = "flowSens"))
    }

    updateRadioButtons(session, 'Flow_or_Precip1', "Select data source:",
                 choices=flow_opts, selected = current_selection, inline = FALSE)
  })

  # Solute limit (MDL & LOQ)
  # Finding MDL and LOQ value for solute, if they exist
  MDL1 <- reactive({
    if (input$SOLUTES1 %in% dataLimits$Analyte) {dataLimits$MDL[dataLimits$Analyte == input$SOLUTES1]}
    else {NA}
  })
  LOQ1 <- reactive({
    if (input$SOLUTES1 %in% dataLimits$Analyte) {dataLimits$LOQ[dataLimits$Analyte == input$SOLUTES1]}
    else {NA}
  })

  # Solute unit
  # Finding appropriate units for selected solute and assigning to ylabel1
  ylabel1 <- reactive ({
    # create the character mu with unicode
    mu <- "\U00B5"
    # If solute belong to group with different set of units, label depending on what it is
    if(input$SOLUTES1 %in% other_units) {
      if (input$SOLUTES1 == "DIC")    ylabel1 <- paste(mu,"M/L")
      if (input$SOLUTES1 == "ANC960")  ylabel1 <- paste(mu, "eq/L")
      if (input$SOLUTES1 == "ANCMet")  ylabel1 <- paste(mu, "eq/L")
      if (input$SOLUTES1 == "spCond") ylabel1 <- paste(mu, "S/cm")
      if (input$SOLUTES1 == "temp")   ylabel1 <- "Degrees Celsius"
      if (input$SOLUTES1 %in% c("pH",
                        "pHmetrohm",
                        "cationCharge",
                        "cnionCharge",
                        "theoryCond",
                        "ionBalance")) { ylabel1 <- "(No Units)" }

      ylabel1 <- gsub(" ", "", ylabel1, fixed = TRUE) # removes spaces in expression: https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string
      ylabel1
    }
    # Otherwise, label as 'default' mg/L
    else {
      ylabel1 <- "mg/L"
      ylabel1
    }
  })

  # The following iterations of selecting/filtering/combining datasets are necessary because
  # the graphing method used (dygraphs) plots *all* columns of data in the referenced data set,
  # rather than permitting some data columns to be hidden. Also, the calculations of median
  # and IQR for historical data are necessary because dygraphs cannot plot boxplots when the
  # x-axis is continuous.

  # Grab selected wateryear, site, solute data from data
  dataAll1 <- reactive({
    if (changesInData$change_dataCurrent > 0) dataAll <- dataAllR()
    dataAll1 <- dataAll %>%
     filter(waterYr %in% input$WATERYEAR1) %>%    # Filter data to selected water year
     filter(site %in% input$SITES1) %>%         # Filter data to selected site
     select(one_of("date", input$SOLUTES1))      # Select desired columns of data
    dataAll1 <- removeCodes(dataAll1)
    dataAll1

  }) # END of dataAll1

  # Grab selected wateryear, site, solute, and sensor data from data
  dataAllQ1 <- reactive({
    if (changesInData$change_dataCurrent > 0) dataAll <- dataAllR()
    if (input$SITES1 %in% sites_streams) {
      if (input$Flow_or_Precip1 == 'gageHt'){
        dataAllQ1 <- dataAll %>%
          filter(waterYr %in% input$WATERYEAR1) %>%        # Filter data to selected water year
          filter(site %in% input$SITES1) %>%             # Filter data to selected site
          select(one_of("date", input$SOLUTES1, "gageHt")) %>% # Selected desired columns of data
          rename(Flow_or_Precip = gageHt)               # Rename GageHt to standard name, so that don't have
                                              #  to create alternative graphs
      }
      if (input$Flow_or_Precip1 == 'flowGageHt'){
        dataAllQ1 <- dataAll %>%
          filter(waterYr %in% input$WATERYEAR1) %>%        # Filter data to selected water year
          filter(site %in% input$SITES1) %>%             # Filter data to selected site
          select(one_of("date", input$SOLUTES1, "flowGageHt")) %>%    # Selected desired columns of data
          rename(Flow_or_Precip = flowGageHt)                  # Rename Q to standard name, so that don't have
                                                    #  to create alternative graphs
      }
      if (input$Flow_or_Precip1 == 'flowSens'){
        # dataSensor = dataSensor[order(dataSensor$datetime),]
        yrstart = as.POSIXct(paste0(input$WATERYEAR1, '-06-01'))
        yrend = as.POSIXct(paste0(as.numeric(input$WATERYEAR1) + 1, '-05-31'))
        dataSensor = filter(dataSensor, datetime > yrstart, datetime < yrend)
        dataAllQ1 <- dataAll %>%
          filter(waterYr %in% input$WATERYEAR1) %>%        # Filter data to selected water year
          filter(site %in% input$SITES1) %>%             # Filter data to selected site
          select(-flowGageHt) %>%
          mutate(datetime=as.POSIXct(paste(as.character(date),
            as.character(timeEST)))) %>%
          full_join(dataSensor[,c('datetime', 'Q_Ls', 'watershedID')],
            by=c('site'='watershedID', 'datetime'='datetime')) %>%
          select(-date) %>%
          filter(site %in% input$SITES1) %>%
          rename(flowGageHt = Q_Ls, date = datetime) %>%
          select(one_of("date", input$SOLUTES1, "flowGageHt")) %>%    # Selected desired columns of data
          rename(Flow_or_Precip = flowGageHt)                  # Rename Q to standard name, so that don't have
                                                    #  to create alternative graphs
      }
    }
    if (input$SITES1 %in% sites_precip) {
      dataAllQ1 <- dataAll %>%
        filter(waterYr %in% input$WATERYEAR1) %>%        # Filter data to selected water year
        filter(site %in% input$SITES1) %>%             # Filter data to selected site
        select(one_of("date", input$SOLUTES1, "precipCatch")) %>%    # Selected desired columns of data
        rename(Flow_or_Precip = precipCatch)                  # Rename Q to standard name, so that don't have
                                                  #  to create alternative graphs
    }
    if (input$SITES1 %in% 'W3') {

        # input = list(SENSORVAR1='Nitrate_mg', SITES1='W3', WATERYEAR1='2016', SOLUTES1='Ca')
        SENSORVAR1_S3 = paste('S3', input$SENSORVAR1, sep='__')
        yrstart = as.POSIXct(paste0(input$WATERYEAR1, '-06-01'))
        yrend = as.POSIXct(paste0(as.numeric(input$WATERYEAR1) + 1, '-05-31'))

        y = RMariaDB::MariaDB()
        con = dbConnect(y, user='root', password=pass, host='localhost',
            dbname='hbef')

        res = dbSendQuery(con, paste0("select datetime, ", SENSORVAR1_S3,
            " from sensor3 WHERE watershedID = '",
            substr(input$SITES1, 2, nchar(input$SITES1)), "' and datetime > '",
            yrstart, "' and datetime < '", yrend, "';"))
        dataSensor3 = dbFetch(res)

        dbDisconnect(con)

        dataAllQ1 <- dataAll %>%
            filter(waterYr %in% input$WATERYEAR1) %>%
            filter(site %in% input$SITES1) %>%
            select(-flowGageHt) %>%
            mutate(datetime=as.POSIXct(paste(as.character(date),
                as.character(timeEST)))) %>%
            full_join(dataSensor3, by=c('datetime'='datetime')) %>%
            select(-date) %>%
            rename(date=datetime) %>%
            select(one_of("date", input$SOLUTES1, SENSORVAR1_S3))
        colnames(dataAllQ1)[which(colnames(dataAllQ1) == SENSORVAR1_S3)] = input$SENSORVAR1
            # rename(quo(input$SENSORVAR1) = quo(SENSORVAR1_S3))
    }

    dataAllQ1
  }) # END of dataCurrentQ1

  # filters historical data; i.e. site, solute, from historical data
  dataHistorical1 <- reactive({
    # Selects appropriate historical data set (stream or precip) based on site selected
    if (input$SITES1 %in% sites_streams) siteGroup <- sites_streams
    if (input$SITES1 %in% sites_precip) siteGroup <- sites_precip
    # Filter historical data by stream/precip sites, date, and solute
    dataHistorical1 <- dataHistorical %>%
      filter(site %in% siteGroup) %>%
      select(one_of("date", input$SOLUTES1)) %>%  # Select desired columns of solute data
      separate(date, c("y","m","d"))          # Separate date into year, month, and day (to use month in next code block)

    # Calculate median and IQR values per month
    median <- tapply(dataHistorical1[,4], dataHistorical1$m, median, na.rm=TRUE)
    IQR <- tapply(dataHistorical1[,4], dataHistorical1$m, IQR, na.rm=TRUE)
    IQR.lower <- median - IQR
    IQR.upper <- median + IQR

    # Create dates for display
    # Create list of dates in middle of the month, so that the median/IQR values are plotted in the middle of each month
    date <- NA
    wy <- as.numeric(input$WATERYEAR1)
    wy.1 <- wy + 1
    for (i in 1:12) {
      if (i<6) {date[i] <- paste((as.numeric(input$WATERYEAR1) + 1),"/", i, "/15", sep="")}
      else {date[i] <- paste(input$WATERYEAR1,"/", i, "/15", sep="")}
    }
    date <- as.Date(date)
    # Create a data frame with the relevant data: date, median, upper and lower IQR
    dataHistorical1 <- data.frame(date = date,
                    solute.IQRlower = IQR.lower,
                    solute.median = median,
                    solute.IQRupper = IQR.upper)

    dataHistorical1
  }) # END of dataHistorical1

  # combines site, solute data from recent water year data with historical data
  dataAllHist1 <- reactive ({
    dataAllHist1 <- full_join(dataAll1(), dataHistorical1(), by = "date")
    return(dataAllHist1)
  }) #END of dataAllHist1

  # combines site, solute, and discharge data from recent water year dataset with historical data
  dataAllQHist1 <- reactive ({
    dataAllQHist1 <- full_join(dataAllQ1(), dataHistorical1(), by = "date")
    return(dataAllQHist1)
  }) #END of dataAllQHist1

  # END of PANEL 1

  # Panel 2 Reactivity ####
  #************************

  #add sensor Q option to hydrology radio buttons if site W1-W9 selected
  observeEvent(input$SITES2, {

    current_selection = input$Flow_or_Precip2
    ws_site_selected = grepl("^W[0-9]+$", input$SITES2)
    if(! ws_site_selected && current_selection == 'flowSens'){
      current_selection = 'flowGageHt'
    }

    flow_opts = c("Gage Height (ft)" = "gageHt",
              "Q from Gage Height (L/s)" = "flowGageHt")
    if(ws_site_selected){
      flow_opts = append(flow_opts,
                   c("Q from Sensor (L/s)" = "flowSens"))
    }

    updateRadioButtons(session, 'Flow_or_Precip2', "Select data source:",
                 choices=flow_opts, selected = current_selection, inline = FALSE)
  })

  # !!! Still trying to figure out
  # Solute units
  # Finding appropriate units for selected solutes and assigning to ylabel2
  ylabel2 <- reactive ({
    ylabel2 <- NA #establish variable
    mu <- "\U00B5"
    for (i in 1:length(input$SOLUTES2)) {
    # create the character mu with unicode
     # If solute belong to group with different set of units, label depending on what it is
     if(input$SOLUTES2[i] %in% other_units) {
      if (input$SOLUTES2[i] == "DIC")    ylabel2[i] <- paste(mu,"M/L") # !!! see code from MatthewRss&Aaron to try their way of printing mu
      if (input$SOLUTES2[i] == "ANC960")  ylabel2[i] <- paste(mu, "eq/L")
      if (input$SOLUTES2[i] == "spCond") ylabel2[i] <- paste(mu, "S/cm")
      if (input$SOLUTES2[i] == "temp")   ylabel2[i] <- "Degrees Celsius"
      if (input$SOLUTES2[i] %in% c("pH",
                        "pHmetrohm",
                        "cationCharge",
                        "anionCharge",
                        "theoryCond",
                        "ionBalance")) { ylabel2[i] <- "(No Units)" }
      test <- gsub(" ", "", ylabel2[i], fixed = TRUE) # removes spaces in expression: https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string
     }
     # Otherwise, label as 'default' mg/L
     else {
      ylabel2[i] <- "mg/L"
     }
     yabel2 <- (unique(ylabel2))
     print(paste(c("unique:", ylabel2)))
     yabel2 <- paste(ylabel2, sep="", collapse="")
     print(ylabel2)
     print(paste(c("paste/collapse:", ylabel2)))
     print(class(ylabel2))
    } # end of for loop

  })

  # Lists of selected inputs to be placed in title
  title.Solutes2 <- reactive({
    if (length(input$SOLUTES2) == 1) { paste(input$SOLUTES2) }
    else {paste(input$SOLUTES2, sep=", ")}
  })

  # Isolate selected data from dataAll
  dataAll2 <- reactive({
    # update data variable if underlying data was updated
    if (changesInData$change_dataCurrent > 0) dataAll <- dataAllR()
    # do initial filter of data depending on whether water year or date range was chosen
    if(input$wateryearOrRange2 == 'wateryr'){
      dataAll2 = filter(dataAll, waterYr %in% input$WATERYEAR2)
    } else {
      dataAll2 = filter(dataAll, date > input$DATE2[1] & date < input$DATE2[2])
    }
    # remaining filtering
    dataAll2 = filter(dataAll2, site %in% input$SITES2) %>% # Filter data to selected sites
      select(one_of("date", input$SOLUTES2))      # Keep date and selected input data
    dataAll2
  }) # END of dataAll2()

  # Grab selected wateryear, site, solute, and discharge data from recent data
  dataAllQ2 <- reactive({
    # update data variable if underlying data was updated
    if (changesInData$change_dataCurrent > 0) dataAll <- dataAllR()
    # do initial filter of data depending on whether water year or range was chosen
    if(input$wateryearOrRange2 == 'wateryr'){
      dataAllQ2 = filter(dataAll, waterYr %in% input$WATERYEAR2)
    } else {
      dataAllQ2 = filter(dataAll, date > input$DATE2[1] &
          date < input$DATE2[2])
    }
    # remaining filtering
    if (input$SITES2 %in% sites_streams) {
      if (input$Flow_or_Precip2 == 'gageHt'){
        dataAllQ2 = filter(dataAllQ2, site %in% input$SITES2) %>%             # Filter data to selected site
          select(one_of("date", input$SOLUTES2, "gageHt")) %>% # Selected desired columns of data
          rename(Flow_or_Precip = gageHt)               # Rename GageHt to standard name, so that don't have
                                              #  to create alternative graphs
      }
      if (input$Flow_or_Precip2 == 'flowGageHt'){
        dataAllQ2 = filter(dataAllQ2, site %in% input$SITES2) %>%             # Filter data to selected site
          select(one_of("date", input$SOLUTES2, "flowGageHt")) %>%    # Selected desired columns of data
          rename(Flow_or_Precip = flowGageHt)                  # Rename Q to standard name, so that don't have
                                                    #  to create alternative graphs
      }
      if (input$Flow_or_Precip2 == 'flowSens'){
        if(input$wateryearOrRange2 == 'wateryr'){
          yrstart = as.POSIXct(paste0(input$WATERYEAR2, '-06-01'))
          yrend = as.POSIXct(paste0(as.numeric(input$WATERYEAR2) + 1, '-05-31'))
        } else {
          yrstart = as.POSIXct(input$DATE2[1])
          yrend = as.POSIXct(input$DATE2[2])
        }
        dataSensor = filter(dataSensor, datetime > yrstart, datetime < yrend)
        dataAllQ2 <- dataAllQ2 %>%
          filter(site %in% input$SITES2) %>%             # Filter data to selected site
          select(-flowGageHt) %>%
          mutate(datetime=as.POSIXct(paste(as.character(date),
            as.character(timeEST)))) %>%
          full_join(dataSensor[,c('datetime', 'Q_Ls', 'watershedID')],
            by=c('site'='watershedID', 'datetime'='datetime')) %>%
          select(-date) %>%
          filter(site %in% input$SITES2) %>%
          rename(flowGageHt = Q_Ls, date = datetime) %>%
          select(one_of("date", input$SOLUTES2, "flowGageHt")) %>%    # Selected desired columns of data
          rename(Flow_or_Precip = flowGageHt)                  # Rename Q to standard name, so that don't have
        #  to create alternative graphs
      }
    }
    if (input$SITES2 %in% sites_precip) {
      dataAllQ2 <- dataAllQ2 %>%
        filter(site %in% input$SITES2) %>%             # Filter data to selected site
        select(one_of("date", input$SOLUTES2, "precipCatch")) %>%    # Selected desired columns of data
        rename(Flow_or_Precip = precipCatch)                  # Rename Q to standard name, so that don't have
                                                  #   to create alternative graphs
    }
    dataAllQ2
  }) # END of dataAllQ2

  # **** END of Panel 2 Reactivity ****


  # Panel 3 Reactivity####
  #************************

  #add sensor Q option to hydrology radio buttons if site W1-W9 selected
  observeEvent(input$SITES3, {

    current_selection = input$Flow_or_Precip3
    ws_sites_selected = all(grepl("^W[0-9]+$", input$SITES3))
    if(! ws_sites_selected && current_selection == 'flowSens'){
      current_selection = 'flowGageHt'
    }

    flow_opts = c("Gage Height (ft)" = "gageHt",
              "Q from Gage Height (L/s)" = "flowGageHt")
    if(ws_sites_selected){
      flow_opts = append(flow_opts,
                   c("Q from Sensor (L/s)" = "flowSens"))
    }

    updateRadioButtons(session, 'Flow_or_Precip3', "Select data source:",
                 choices=flow_opts, selected = current_selection, inline = FALSE)
  })

  # Solute limit (MDL & LOQ)
  # Finding MDL and LOQ value for solute, if they exist
  MDL3 <- reactive({
    if (input$SOLUTES3 %in% dataLimits$Analyte) {dataLimits$MDL[dataLimits$Analyte == input$SOLUTES3]}
    else {NA}
  })
  LOQ3 <- reactive({
    if (input$SOLUTES3 %in% dataLimits$Analyte) {dataLimits$LOQ[dataLimits$Analyte == input$SOLUTES3]}
    else {NA}
  })

  # ylabel, depending on selection
  ylabel3 <- reactive ({
    # create the character mu with unicode
    mu <- "\U00B5"
    # If input$SOLUTES2 belong to group with different set of units, label depending on what it is
    if(input$SOLUTES3 %in% other_units) {
     if (input$SOLUTES3 == "DIC")    ylabel3 <- paste(mu,"M/L")
     if (input$SOLUTES3 == "ANC960")  ylabel3 <- paste(mu, "eq/L")
     if (input$SOLUTES3 == "ANCMet")  ylabel3 <- paste(mu, "eq/L")
     if (input$SOLUTES3 == "spCond") ylabel3 <- paste(mu, "S/cm")
     if (input$SOLUTES3 == "temp")   ylabel3 <- "Degrees Celsius"
     if (input$SOLUTES3 %in% c("pH",
                      "pHmetrohm",
                      "cationCharge",
                      "anionCharge",
                      "theoryCond",
                      "ionBalance")) { ylabel3 <- "(No Units)" }
     ylabel3 <- gsub(" ", "", ylabel3, fixed = TRUE) # removes spaces in expression: https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string
     return(ylabel3)
    }
    # Otherwise, label as 'default' mg/L
    else {
     ylabel3 <- "mg/L"
     return(ylabel3)
    }
  })

  # Lists of selected inputs to be placed in title
  title.Sites3 <- reactive({
    if (length(input$SITES3) == 1) {paste(input$SITES3) } else {
      paste(input$SITES3, sep=", ") }
  })

  # filters data to only include data selected by inputs
  dataAll3 <- reactive({
    # update data variable if underlying data was updated
    if (changesInData$change_dataCurrent > 0) dataAll <- dataAllR()
    # do initial filter of data depending on whether water year or date range was chosen
    if(input$wateryearOrRange3 == 'wateryr'){
      dataAll3 = filter(dataAll, waterYr %in% input$WATERYEAR3)
    } else {
      dataAll3 = filter(dataAll, date > input$DATE3[1] &
        date < input$DATE3[2])
    }
    dataAll3 <- dataAll3 %>%
     filter(site %in% input$SITES3) %>%            # Filter data to selected sites
     select(one_of("date", "site", input$SOLUTES3)) %>% # Keep date, site, and solute data
     mutate(i = row_number()) %>%                # Create new columns of data of just row numbers. Necessary to prevent error message of duplicate values after next line of code, but inefficient because doesn't combine rows with duplicate columns.)
     spread_(key_col = "site", value_col = input$SOLUTES3, fill=NA) %>%  # Reshape data so that each place in "sites" is made into a unique column, with corresponding solute value as data
     select(-i)                            # Remove row name variable

     }) # END of dataCurrent3()


  # gathers hydrology data and calculates median hydrology values
  Q3 <- reactive({
    # update data variable if underlying data was updated
    if (changesInData$change_dataCurrent > 0) dataAll <- dataAllR()
    # do initial filter of data depending on whether water year or date range was chosen
    if(input$wateryearOrRange3 == 'wateryr'){
      Q3 = filter(dataAll, waterYr %in% input$WATERYEAR3)
    } else {
      Q3 = filter(dataAll, date > input$DATE3[1] &
          date < input$DATE3[2])
    }

    # if Discharge is selected, finds data for all watershed (stream) sites,
    # and calculates median
    if (input$HYDROLOGY3 == 'Discharge') {
     if (input$Flow_or_Precip3 == 'gageHt') {
      Q3 <- Q3 %>%
        filter(site %in% sites_streams) %>%
        select(one_of("date", input$Flow_or_Precip3)) %>%
        group_by(date) %>%
        summarise(Hydro.med = median(gageHt, na.rm=TRUE))}
     else if(input$Flow_or_Precip3 == 'flowSens'){
      # dataAll$timeEST[is.na(dataAll2$timeEST)] = hms('00:00:00')
      if(input$wateryearOrRange3 == 'wateryr'){
        yrstart = as.POSIXct(paste0(input$WATERYEAR3, '-06-01'))
        yrend = as.POSIXct(paste0(as.numeric(input$WATERYEAR3) + 1, '-05-31'))
      } else {
        yrstart = as.POSIXct(input$DATE3[1])
        yrend = as.POSIXct(input$DATE3[2])
      }
      dataSensor = filter(dataSensor, datetime > yrstart, datetime < yrend)
      Q3 <- Q3 %>%
      filter(site %in% input$SITES3) %>%
        select(-gageHt) %>%
        mutate(datetime=as.POSIXct(paste(as.character(date),
          as.character(timeEST)))) %>%
        full_join(dataSensor[,c('datetime', 'Q_Ls', 'watershedID')],
          by=c('site'='watershedID', 'datetime'='datetime')) %>%
        select(-date) %>%
        filter(site %in% input$SITES3) %>%
        rename(flowSens = Q_Ls, date = datetime) %>%
        select(one_of("date", input$Flow_or_Precip3)) %>%
        group_by(date) %>%
        summarise(Hydro.med = median(flowSens, na.rm=TRUE))
     } else { #i.e. if input$Flow_or_Precip3 == 'flowGageHt'
      Q3 <- Q3 %>%
        filter(site %in% sites_streams) %>%
        select(one_of("date", input$Flow_or_Precip3)) %>%
        group_by(date) %>%
        summarise(Hydro.med = median(flowGageHt, na.rm=TRUE))}
    } # end of Discharge if statement

    # if Precipitation is selected, finds data for all rain gage (precip) sites,
    # and calculates median
    if (input$HYDROLOGY3 == 'Precipitation') {
      Q3 <- Q3 %>%
        filter(site %in% sites_precip) %>%
        select(one_of("date", "precipCatch")) %>%
        group_by(date) %>%
        summarise(Hydro.med = median(precipCatch, na.rm=TRUE))
    } # end of Preciptiation if statement

    Q3

  }) # end of Q3()

  # filters Original (recent water year) data to include data selected by inputs AND discharge/precip
  dataAllQ3 <- reactive({
    dataAll3 = dataAll3()
    dataQ3 = Q3()
    if(class(dataQ3$date) != 'Date') dataAll3$date = as.POSIXct(dataAll3$date)
    dataAllQ3 <- full_join(dataAll3, dataQ3, by = "date")
    return(dataAllQ3)
  }) # END of dataAllQ3()

  # **** END of Panel 3 Reactivity ****

  # Panel 4 Reactivity ####
  #************************

  #add sensor Q option to hydrology radio buttons if site W1-W9 selected
  observeEvent(input$SITES4, {

    current_selection = input$FLOW_SOURCE4
    ws_sites_selected = all(grepl("^W[0-9]+$", input$SITES4))
    if(! ws_sites_selected && current_selection == 'flowSens'){
      current_selection = 'flowGageHt'
    }

    flow_opts = c("Gage Height (ft)" = "gageHt",
              "Q from Gage Height (L/s)" = "flowGageHt")
    if(ws_sites_selected){
      flow_opts = append(flow_opts,
                   c("Q from Sensor (L/s)" = "flowSens"))
    }

    updateRadioButtons(session, 'FLOW_SOURCE4', "Select data source:",
                 choices=flow_opts, selected = current_selection, inline = FALSE)
  })

  # Solute limit (MDL & LOQ)
  # Finding MDL and LOQ value for solute, if they exist
  MDL4 <- reactive({
    if (input$SOLUTES4 %in% dataLimits$Analyte) {dataLimits$MDL[dataLimits$Analyte == input$SOLUTES4]}
    else {NA}
  })
  LOQ4 <- reactive({
    if (input$SOLUTES4 %in% dataLimits$Analyte) {dataLimits$LOQ[dataLimits$Analyte == input$SOLUTES4]}
    else {NA}
  })

  ## Filter data to desired dates
  data4 <- reactive ({
    if (changesInData$change_dataAll > 0) dataAll <- dataAllR()
    data4 <- dataAll %>%
      filter(date >= input$DATE4[1]) %>%
      filter(date <= input$DATE4[2])
    data4 <- removeCodes(data4)
  })
  ## Extract data for Precip plot
  dataPrecip4 <- reactive ({
    dataPrecip4 <- data4() %>%
      #filter(site %in% input$PRECIP_SITE4) %>%
      filter(site %in% sites_precip) %>%
      select(one_of("date", "site", input$PRECIP_SOURCE4))
    if (input$PRECIP_SOURCE4 == "precipCatch") {
      dataPrecip4 <- dataPrecip4 %>%
        group_by(date) %>%
        summarise(medianPrecip = median(precipCatch, na.rm=TRUE))
    }
    if (input$PRECIP_SOURCE4 == "precipETI") {
      dataPrecip4 <- dataPrecip4 %>%
        group_by(date) %>%
        summarise(medianPrecip = median(precipETI, na.rm=TRUE))
    }
    dataPrecip4
  })
  ## Extract data for Solutes (Main) plot
  dataMain4 <- reactive ({
    dataMain4 <- data4() %>%
      filter(site %in% input$SITES4) %>%
      select(one_of("date", "site", input$SOLUTES4, "fieldCode")) %>%  # Keep date, site, solute & fieldcode data
      group_by(date, site) %>%
      gather(key = solute, value = solute_value, -site, -date, -fieldCode)  # Reshape data for ggplot2 plotting
  })
  ## Extract data for Discharge (Flow) plot
  dataFlow4 <- reactive ({
    dataFlow4 <- data4() %>%
      filter(site %in% input$FLOW_SITE4)
    # flow values need to be summarized with median per date,
    # because multiple values for one date make flow graph look strange
    if (input$FLOW_SOURCE4 == "gageHt") {
      dataFlow4 <- dataFlow4 %>%
        select(one_of("date", input$FLOW_SOURCE4)) %>%
        group_by(date) %>%
        summarise(flowMaxPerDate = max(gageHt, na.rm=TRUE))
    }
    if (input$FLOW_SOURCE4 == "flowGageHt") {
      dataFlow4 <- dataFlow4 %>%
        select(one_of("date", input$FLOW_SOURCE4)) %>%
        group_by(date) %>%
        summarise(flowMaxPerDate = max(flowGageHt, na.rm=TRUE))
    }
    if (input$FLOW_SOURCE4 == "flowSens") {
      #!!! Not selecting by input$FLOW_SITE4, but by all SITES4 selected - is this intentional?
      dataFlow4 = filter(dataSensor, datetime > input$DATE4[1],
        datetime < input$DATE4[2], watershedID %in% input$FLOW_SITE4) %>%
        mutate(date=as.Date(datetime)) %>%
        select(date, Q_Ls) %>%
        group_by(date) %>%
        summarise(flowMaxPerDate = max(Q_Ls, na.rm=TRUE))

    }
    dataFlow4
  })
  ## Additional data for Flow plot: hydroGraph labels
  dataFlowHydroGraph4 <- reactive ({
    dataFlowHydroGraph4 <- data4() %>%
      filter(site %in% input$FLOW_SITE4) %>%
      select(one_of("date", "hydroGraph", input$FLOW_SOURCE4))
      # group_by(date) %>%
      # summarise(hydroGraph = first(hydroGraph, na.rm=TRUE), flowSource = max(flowSource, na.rm=TRUE))
    dataFlowHydroGraph4
  })

  # **** END of Panel 4 Reactivity ****



  # Panel 5 Reactivity ####
  #*****************************

  data5 <- reactive({
    if (changesInData$change_dataCurrent > 0) dataCurrent <- dataCurrentR()
    # filter data to selected water year and site
    data5 <- dataCurrent %>%
      filter(waterYr %in% input$WATERYEAR5) %>%
      filter(site %in% input$SITES5)
    # make uniqueID first column
    data5_uniqueID <- select(data5, uniqueID)
    data5_remaining <- select(data5, -uniqueID)
    data5 <- bind_cols(data5_uniqueID, data5_remaining)
    data5
  })

  # *Download Tab* ########################################

  datasetInput <- reactive({
    if (changesInData$change_dataCurrent > 0) {
       dataCurrent <- dataCurrentR()
       dataAll <- dataAllR()
    }
    # Fetch the appropriate data object, depending on the value
    # of input$DATASET
    datasetInput <- switch(input$DOWNLOAD_DATASET,
                    "Current" = dataCurrent,
                    "Historical" = dataHistorical,
                    "Sensor" = dataSensor,
                    "All" = dataAll)
  })


  # ***   OUTPUT    *** ----
  # ***********************************
  # *Main Tab* ###########################################
  output$DOWNLOAD_TEMPLATE <- downloadHandler(
    filename = function() {
      paste("DataTemplate_current", "csv", sep=".")
    },
    content = function(file) {
      file.copy("documentation/DataTemplate_current.csv", file)
    },
    contentType = "text/csv"
  )

  output$DOWNLOAD_TEMPLATE_EXAMPLE <- downloadHandler(
    filename = function() {
      paste("DataTemplate_current_example", "csv", sep=".")
    },
    content = function(file) {
      file.copy("documentation/DataTemplate_current_example.csv", file)
    },
    contentType = "text/csv"
  )

  # *Upload Tab* #########################################

  output$FILE_PREVIEW <- renderDataTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    if(input$UPLOAD_DISPLAY == "head") {
      head(dataNew())
    }
    else {
      dataNew()
    }

    # !!! Need to add in code to warn users if uniqueID will be duplicate with what's in MySQL
    # !!! Need to allow users to delete lines

  })

  # *QA/QC Tab* #########################################

    # Panel 1 Output ####
  #********************

  # Print MDL & LOQ values in panel if available
  output$LIMITS1 <- renderText({
    paste(c("MDL:",MDL1(), "  LOQ:", LOQ1()))
  })

  # Print chart title, describing what's been selected
  output$TITLE1 <-  renderText({
    paste(c(input$SOLUTES1, "from site", input$SITES1,"in water year", input$WATERYEAR1))
  })

  # Main graph. A sequence of if/else statements, depending on what's been selected
  # from input panel. Done in this manner because dygraph() cannot overlay plots, each
  # plot must be started from scratch because it graphs ALL the data within xts data.
  output$GRAPH1 <- renderDygraph({

    ylabel <- ylabel1()
    if (input$HYDROLOGY1 == TRUE)  {
      if (input$SITES1 %in% sites_streams) ylabel2 <- 'Discharge (ft or L/s)'
      if (input$SITES1 %in% sites_precip) ylabel2 <- 'Precipitation (mm)'
      if (input$SOLUTES_HIST1 == TRUE) {

        if(input$Flow_or_Precip1 == 'flowSens'){
          dygraph(xts(c(NA,2,NA), order.by=as.Date(1:3))) %>%
            dyOptions(drawGrid=FALSE, drawXAxis=FALSE, drawYAxis=FALSE,
              drawPoints=FALSE, colors='red') %>%
            dyAnnotation(as.Date(2), width=200, height=70,
              paste('Historical data not currently plottable when',
              'data source = "Q from sensor"'))
        } else {

          # Plots Default + Discharge + Historical data
          data1 <- dataAllQHist1()
          data1 <- removeCodes(data1)
          data1.xts <- xts(data1[,-1], order.by = data1$date)
          #paste(c("XTS:", class(dataCur1$FieldCode)))

          dygraph1 <- dygraph(data1.xts) %>%
            dyAxis("x", label = paste("Water Year", input$WATERYEAR1),
                 axisLabelColor = "black") %>%
            dyAxis("y", label = ylabel,
                 independentTicks=TRUE,
                 axisLabelColor = "black") %>%
            dyAxis('y2',label=ylabel2,
                 independentTicks=TRUE,
                 axisLabelColor = "#3182bd",
                 axisLabelWidth = 70,
                 axisLineColor = "#3182bd") %>%
            dySeries(name = input$SOLUTES1,
                  color = "black",
                  drawPoints = TRUE,
                  pointSize = 3,
                  axis='y') %>%
            dySeries(name = 'Flow_or_Precip',
                  drawPoints = FALSE,
                  fillGraph=T,
                  color = "#3182bd",
                  axis='y2') %>%
            dySeries(c('solute.IQRlower', 'solute.median', 'solute.IQRupper'),
                  strokePattern = c("dashed"),
                  color = c("#A9A9A9"),
                  label = 'median + IQR',
                  axis='y') %>%
            dyLimit(limit = LOQ1(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
            dyLimit(limit = MDL1(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
            dyOptions(drawGrid = FALSE,
                   strokeWidth = 1,
                   fillAlpha = 0.5,
                   connectSeparatedPoints=TRUE,
                   includeZero = TRUE) %>%
            dyLegend(width = 300, showZeroValues = FALSE)

          dygraph1
        }

      } else {

        # Plots Default + Discharge data
        data1 <- dataAllQ1()
        data1 <- removeCodes(data1)
        data1.xts <- xts(data1[,-1], order.by = data1$date)

        dygraph1 <- dygraph(data1.xts) %>%
          dyAxis("x", label = paste("Water Year", input$WATERYEAR1)) %>%
          dyAxis("y", label = ylabel, independentTicks=TRUE) %>%
          dyAxis('y2',label=ylabel2, independentTicks=TRUE,
               axisLabelWidth = 70,
               axisLabelColor = "#3182bd",
               axisLineColor = "#3182bd") %>% # color is light blue
          dySeries(name = input$SOLUTES1,
                color = "#black") %>%
          dySeries(name = 'Flow_or_Precip',
                drawPoints = FALSE,
                fillGraph=T,
                color = "#3182bd",
                axis='y2') %>%
          dyLimit(limit = LOQ1(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
          dyLimit(limit = MDL1(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
          dyOptions(drawGrid = FALSE,
                 drawPoints = TRUE,
                 strokeWidth = 1,
                 pointSize = 3,
                 fillAlpha = 0.5,
                 connectSeparatedPoints=TRUE,
                 includeZero = TRUE)
        dygraph1
      }
    } else {

      if (input$SOLUTES_HIST1 == TRUE) {

        # Plots Default + Historical data
        data1 <- dataAllHist1()
        data1 <- removeCodes(data1)
        data1.xts <- xts(data1[,-1], order.by = data1$date)

        dygraph1 <- dygraph(data1.xts) %>%
          dyAxis("x", label = paste("Water Year", input$WATERYEAR1)) %>%
          dyAxis("y", label = ylabel, independentTicks=TRUE) %>%
          dySeries(name = input$SOLUTES1,
                color = "black",
                drawPoints = TRUE,
                pointSize = 3,
                axis='y') %>%
          dySeries(c('solute.IQRlower', 'solute.median', 'solute.IQRupper'),
                strokePattern = c("dashed"),
                color = "#A9A9A9",
                label = 'median + IQR',
                axis='y') %>%
          dyLimit(limit = LOQ1(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
          dyLimit(limit = MDL1(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
          dyOptions(drawGrid = FALSE,
                 strokeWidth = 1,
                 fillAlpha = 0.3,
                 connectSeparatedPoints=TRUE,
                 includeZero = TRUE)

        dygraph1

      } else {

        if(input$SENSORVAR1 != 'None'){
          #plots default + sensor data

            data1 <- dataAllQ1()
            data1 <- removeCodes(data1)
            data1.xts <- xts(data1[,-1], order.by = data1$date)

            dygraph1 <- dygraph(data1.xts) %>%
                dyAxis("x", label = paste("Water Year", input$WATERYEAR1)) %>%
                dyAxis("y", label = ylabel, independentTicks=TRUE) %>%
                dyAxis('y2',label = input$SENSORVAR1, independentTicks=TRUE,
                    axisLabelWidth = 70,
                    axisLabelColor = "#3182bd",
                    axisLineColor = "#3182bd") %>% # color is light blue
                dySeries(name = input$SOLUTES1,
                    color = "#black") %>%
                dySeries(name = input$SENSORVAR1,
                    drawPoints = FALSE,
                    fillGraph=T,
                    color = "#3182bd",
                    axis='y2') %>%
                dyLimit(limit = LOQ1(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
                dyLimit(limit = MDL1(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
                dyOptions(drawGrid = FALSE,
                    drawPoints = TRUE,
                    strokeWidth = 1,
                    pointSize = 3,
                    fillAlpha = 0.5,
                    connectSeparatedPoints=TRUE,
                    includeZero = TRUE)
            dygraph1

        } else {

          # Plots Default data

          data1 <- dataAll1()
          data1 <- removeCodes(data1)
          data1.xts <- xts(data1, order.by = data1$date)

          #padrange <- c(min(data1.xts$input$SOLUTES1, na.rm=TRUE) - 1, max(data1.xts$input$SOLUTES1, na.rm=TRUE) + 1) # !!! trying to resolve negative number issue (negative values plotting incorrectly)

          dygraph1 <- dygraph(data1.xts) %>%
            dyAxis("x", label = paste("Water Year", input$WATERYEAR1)) %>%
            dyAxis("y", label = ylabel, independentTicks=TRUE) %>%
            dySeries(name = input$SOLUTES1,
                  color = "black",
                  drawPoints = TRUE,
                  strokeWidth = 1,
                  pointSize = 3) %>%
            # dySeries(name = "FieldCode",
            #       color = "black",
            #       drawPoints = TRUE,
            #       strokeWidth = 0,
            #       pointSize = 1) %>%
            # for (i in 1:nrow(data1.xts)) {
            #   dyAnnotation(index(i), data1.xts$FieldCode[i])
            # } %>%
            dyLimit(limit = LOQ1(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
            dyLimit(limit = MDL1(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
            dyOptions(drawGrid = FALSE,
                   connectSeparatedPoints=TRUE,
                   includeZero = TRUE)

          dygraph1
        }
      }
    }

  }) # END of output$GRAPH1

  output$TABLE1 <- renderDataTable(dataAll1()) # for testing purposes


  # Panel 2 Output ####
  #********************

  output$TITLE2 <-  renderText({
    if(input$wateryearOrRange2 == 'wateryr'){
      paste(c(title.Solutes2(), "from site", input$SITES2,"in water year", input$WATERYEAR2))
    } else {
      paste(c(title.Solutes2(), "from site", input$SITES2,"from range of dates"))
    }

  })

  output$GRAPH2 <- renderDygraph({

    # determine x-axis label
    if(input$wateryearOrRange2 == 'wateryr'){
      xlabel <- paste("Water Year ", input$WATERYEAR2)
    } else {
      xlabel = paste("Dates ", input$DATE2[1], " to ", input$DATE2[2])
    }

    if (input$HYDROLOGY2 == TRUE)  {

      #determine y2-axis labels
      if (input$SITES2 %in% sites_streams) ylabel2 <- 'Discharge (ft or L/s)'
      if (input$SITES2 %in% sites_precip) ylabel2 <- 'Precipitation (mm)'

        # Plots Default + Discharge data
        data2 <- dataAllQ2()
        data2 <- removeCodes(data2)
        data2.xts <- xts(data2[,-1], order.by = data2$date)

        dygraph(data2.xts) %>%
          dyAxis("x", label=xlabel) %>%
          dyAxis("y", label = "(various units, dependent on input)", independentTicks=TRUE) %>%
          dyAxis('y2',label=ylabel2, independentTicks=TRUE,
              axisLabelWidth = 70,
              axisLabelColor = "#3182bd",
              axisLineColor = "#3182bd") %>% # color is light blue
          #dySeries(name = input$SOLUTES2) %>%
          dySeries(name = 'Flow_or_Precip',
                label = "Discharge/Precip",
                drawPoints = FALSE,
                fillGraph=T,
                #color = "#3182bd",
                axis='y2') %>%
          dyOptions(drawGrid = FALSE,
                drawPoints = TRUE,
                strokeWidth = 1,
                pointSize = 3,
                fillAlpha = 0.3,
                connectSeparatedPoints=TRUE,
                includeZero = TRUE)

      } else {

        # Plots Default data

        data2 <- dataAll2()
        data2 <- removeCodes(data2)
        data2.xts <- xts(data2, order.by = data2$date)

        # padrange <- c(min(data2.xts$input$SOLUTES2, na.rm=TRUE) - 1, max(data2.xts$input$SOLUTES2, na.rm=TRUE) + 1) # !!! attempt at resolving negative values issue
        # add "valueRange = padrange" in dyAxis if working; currently returns warning that all arguments are missing

        dygraph(data2.xts) %>%
          dyAxis("x", label = xlabel) %>%
          dyAxis("y", label = "(various units, dependent on input)", independentTicks=TRUE) %>%
          # dySeries(name = input$SOLUTES2,
          #       drawPoints = TRUE,
          #       strokeWidth = 1,
          #       pointSize = 3) %>%
          dyOptions(drawGrid = FALSE,
                connectSeparatedPoints=TRUE,
                includeZero = TRUE,
                drawPoints = TRUE,
                strokeWidth = 1,
                pointSize = 3)
      }

  }) # END of output$GRAPH2

  # For testing purposes (of data sorting):
  # ***************************************
  output$TABLE2 <- renderDataTable({
    if (input$HYDROLOGY2 == FALSE) dataAll2()
    else dataAllQ2()
  }) # END of output$TABLE2


  # Panel 3 Output ####
  #********************

  output$TITLE3 <-  renderText({
    if(input$wateryearOrRange3 == 'wateryr'){
      paste(c(input$SOLUTES3, "from site(s)", title.Sites3(),"in water year", input$WATERYEAR3))
    } else {
      paste(c(input$SOLUTES3, "from site(s)", title.Sites3(),"from range of dates"))
    }
  })


  output$LIMITS3 <- renderText({
   paste(c("MDL:",MDL3(), "  LOQ:", LOQ3()))
  })

  output$GRAPH3 <- renderDygraph({

    # determine x-axis label
    if(input$wateryearOrRange3 == 'wateryr'){
      xlabel <- paste("Water Year ", input$WATERYEAR3)
    } else {
      xlabel = paste("Dates ", input$DATE3[1], " to ", input$DATE3[2])
    }

     # Plots Default + Discharge data
     if (input$HYDROLOGY3 == "Discharge" | input$HYDROLOGY3 == "Precipitation") {

      if (input$HYDROLOGY3 == "Discharge")  {

      data3 <- dataAllQ3()
      #data3 <- removeCodes3(data3, input$SOLUTES3)
      data3.xts <- xts(data3[,-1], order.by = data3$date)

      dygraph(data3.xts) %>%
        dyAxis("x", label = xlabel) %>%
        dyAxis("y", label = ylabel3(), independentTicks=TRUE) %>%
        dyAxis('y2',label='Hydrology (ft or L/s)', independentTicks=TRUE,
           axisLabelWidth = 70,
           axisLabelColor = "#3182bd",
           axisLineColor = "#3182bd") %>% # color is light blue
        #dySeries(name = input$SOLUTES2) %>%
        dySeries(name = 'Hydro.med',
            label = "Discharge",
            drawPoints = FALSE,
            fillGraph=T,
            #color = "#3182bd",
            axis='y2') %>%
        dyLimit(limit = LOQ3(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
        dyLimit(limit = MDL3(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
        dyOptions(drawGrid = FALSE,
             drawPoints = TRUE,
             strokeWidth = 1,
             pointSize = 3,
             fillAlpha = 0.3,
             connectSeparatedPoints=TRUE,
             includeZero = TRUE)
      }

      else {

        data3 <- dataAllQ3()
        #data3 <- removeCodes3(data3, input$SOLUTES3)
        data3.xts <- xts(data3[,-1], order.by = data3$date)

        dygraph(data3.xts) %>%
         dyAxis("x", label = xlabel) %>%
         dyAxis("y", label = ylabel3(), independentTicks=TRUE) %>%
         dyAxis('y2',label='Precipitation (mm)', independentTicks=TRUE,
              axisLabelWidth = 70,
              axisLabelColor = "#3182bd",
              axisLineColor = "#3182bd") %>% # color is light blue
         #dySeries(name = input$SOLUTES2) %>%
         dySeries(name = 'Hydro.med',
               label = "Precipitation",
               drawPoints = FALSE,
               fillGraph=T,
               #color = "#3182bd",
               axis='y2') %>%
         dyLimit(limit = LOQ3(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
         dyLimit(limit = MDL3(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
         dyOptions(drawGrid = FALSE,
                drawPoints = TRUE,
                strokeWidth = 1,
                pointSize = 3,
                fillAlpha = 0.3,
                connectSeparatedPoints=TRUE,
                includeZero = TRUE)
      }

    } else {

     # Plots Default data

     data3 <- dataAll3()
     data3 <- removeCodes3(data3, input$SOLUTES3)
     data3.xts <- xts(data3, order.by = data3$date)

     # padrange <- c(min(data3.xts$input$SOLUTES3, na.rm=TRUE) - 1, max(data3.xts$input$SOLUTES3, na.rm=TRUE) + 1) # !!! attempt at resolving negative values issue
     # add "valueRange = padrange" in dyAxis if working; currently returns warning that all arguments are missing

     dygraph(data3.xts) %>%
      dyAxis("x", label = xlabel) %>%
      dyAxis("y", label = ylabel3(), independentTicks=TRUE) %>%
      # dySeries(name = input$SOLUTES3,
      #       drawPoints = TRUE,
      #       strokeWidth = 1,
      #       pointSize = 3) %>%
      dyLimit(limit = LOQ3(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
      dyLimit(limit = MDL3(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
      dyOptions(drawGrid = FALSE,
             connectSeparatedPoints=TRUE,
             includeZero = TRUE,
             drawPoints = TRUE,
             strokeWidth = 1,
             pointSize = 3) # colors = RColorBrewer::brewer.pal(9, "Set1") [but yellow not good]
    }

  }) # END of output$GRAPH3

  # For testing purposes (of data sorting):
  # ***************************************
  output$TABLE3 <- renderDataTable({
    if (input$HYDROLOGY3 == "None") dataAll3()
    else dataAllQ3()
  }) # end of output$TABLE3


  # Panel 4 Output ####
  #********************
  # opar <- par() #save original parameters
  # par(mar = c(5,10,4,2)+0.1)
  # output$TITLE4 <- renderText ({print(input$SITES4)})
  output$GRAPH_PRECIP4 <- renderPlot({
    if (input$PRECIP4_OPTION == TRUE) {
      data <- dataPrecip4()
      x <- data$date
      # get column number of selected precipitation source
      # ind_col <- which(input$PRECIP_SOURCE4 == colnames(data), arr.ind = TRUE)
      y <- data$medianPrecip
      p <- ggplot(data, aes(x, y)) + my_theme +
        geom_col(fill = "cadetblue3", width = 4, na.rm=TRUE) +
        labs(x = "", y = "Precipitation") +
        coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
        scale_y_reverse()
      p
    }
  }, height = 100) # end of output$GRAPH_PRECIP4
  output$GRAPH_MAIN4 <- renderPlot({
    data <- dataMain4()
    x <- data$date
    y <- data$solute_value
    # build ggplot function
    # design <- my_theme +
    #   geom_point(size = 2.5) +
    #   geom_line(alpha = 0.5) +
    #   scale_x_date(date_labels = "%Y-%b")+
    #   coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
    #   scale_color_manual(values = c("black", "#307975", "#691476", "#735E1F", "#6F0D2F", "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")) +
    #   labs(x = "", y = "Solutes")
    if(input$SOLUTES4_COLOR == "Solutes") {
      m <- ggplot(data, aes(x, y, shape=data$site, color=data$solute)) +
        my_theme +
        geom_point(size = 2.5) +
        geom_line(alpha = 0.5) +
        scale_x_date(date_labels = "%Y-%b")+
        coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
        scale_color_manual(values = c("black", "#307975", "#691476", "#735E1F", "#6F0D2F", "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")) +
        labs(x = "", y = "Solutes")
    } else {
      m <- ggplot(data, aes(x, y, shape=data$solute, color=data$site)) +
        my_theme +
        geom_point(size = 2.5) +
        geom_line(alpha = 0.5) +
        scale_x_date(date_labels = "%Y-%b")+
        coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
        scale_color_manual(values = c("black", "#307975", "#691476", "#735E1F", "#6F0D2F", "#7F8D36", "#37096D", "#074670", "#0C2282", "#750D47")) +
        labs(x = "", y = "Solutes")
    }

    # If show field code is selected, add to ggplot
    if (input$FIELDCODE4 == TRUE) {
      m <- m + geom_text(aes(label=data$fieldCode),
                   nudge_y = (max(data$solute_value, na.rm = TRUE) - min(data$solute_value, na.rm = TRUE))/15,
                   check_overlap = TRUE)
    }
    # plot
    m
  }, height = 350) # end of output$GRAPH_MAIN4
  output$GRAPH_FLOW4 <- renderPlot({
    if (input$DISCHARGE4_OPTION == TRUE) {
      data <- dataFlow4()
      x <- data$date
      y <- data$flowMaxPerDate
      f <- ggplot(data, aes(x, y)) + my_theme +
        geom_area(fill = "cadetblue3", na.rm=TRUE) +
        coord_cartesian(xlim = c(input$DATE4[1], input$DATE4[2])) +
        labs(x = "", y = "Discharge")
      if (input$HYDROLIMB4 == TRUE) {
        data.hl <- dataFlowHydroGraph4()
        if (input$FLOW_SOURCE4 == "gageHt") y.hl <- data.hl$gageHt
        if (input$FLOW_SOURCE4 == "flowGageHt") y.hl <- data.hl$flowGageHt
        if (input$FLOW_SOURCE4 == "flowSens") y.hl <- data.hl$flowSensor
        f <- f + geom_text(data = data.hl,
                     aes(x = date,
                        y = y.hl,
                        label = hydroGraph),
                     nudge_y = (max(y.hl, na.rm = TRUE) - min(y.hl, na.rm = TRUE))/15,
                     check_overlap = TRUE)
      }
      f
    }
  }, height = 100) # end of output$GRAPH_FLOW4

  output$TABLE4 <- renderDataTable({
    dataFlowHydroGraph4()
    #head(dataCurrentR())
  }) # end of output$TABLE4

  # Panel 5 Output ####
  #*****************************

  output$HOT <- renderRHandsontable({

    data5 <- data5()
    #the following is necessary to prevent error on remote server
    data5$timeEST <- as.character(data5$timeEST)

    # if (!is.null(input$hot)) { # if there is an rhot user input...
    #   dataSummary <- hot_to_r(input$hot) # convert rhandsontable data to R object and store in data frame
    #   setHot(dataSummary) # set the rhandsontable values
    # }

    rhandsontable(data5, height = 400) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col("uniqueID", readOnly = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1)
  })

  observeEvent(input$SAVECHANGES5,{
     message("inside SAVECHANGES5")
     # openning connection to database
     con = dbConnect(MariaDB(),
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = 'hbef')

      # make handsontable data object into R data frame
      dataChanged <- hot_to_r(input$HOT)
      dataChanged <- standardizeClasses(dataChanged)
      # replace all commas with ";", as commas interfere with downloading csv's
      dataChanged$notes <- gsub(",", ";", dataChanged$notes)
      dataChanged$sampleType <- gsub(",", ";", dataChanged$sampleType)
#      # split changed data table into dataInitial and dataChemistry
#      dataInitialChanged <- dataChanged[, (names(dataChanged) %in% names(dataInitial))]
#      dataChemistryChanged <- dataChanged[, (names(dataChanged) %in% c("uniqueID", names(dataChemistry)))]

      # build MySQL queries, used to delete data that will be replaced
      wateryear5 <- input$WATERYEAR5
      site5 <- input$SITES5

      queryDelete <- paste0('DELETE FROM current ',
                         ' WHERE waterYr = ', wateryear5,
                         ' AND site = "', site5,
                         '" ORDER BY uniqueID;')

      # delete old current data
      dbExecute(con, queryDelete)

      # add changed data
      dbWriteTable(con, "current", dataChanged, append=TRUE, row.names=FALSE)

      # update reactive value to signal core data has changed
      changesInData$change_dataCurrent <- changesInData$change_dataCurrent + 1

      showNotification("Changes Saved.")

      dbDisconnect(con)

    }
  )

  # deletes section of data (specified by input) in current table in MySQL
  observeEvent(input$BUTTON_DELETE5,{
    message("inside BUTTON_DELETE5")
    con = dbConnect(MariaDB(),
               user = 'root',
               password = pass,
               host = 'localhost',
               dbname = 'hbef')
    # check that rows exist; if so, delete, if not, send notification
    # !!! could make cleaner with validate()

      # establish variables
      if (input$DELETE_SITE5 == "All Sites") {
        siteQuery <- ""
      } else {
        siteQuery <- paste0("site = '", input$DELETE_SITE5, "' AND ")
      }
      if (input$DELETE_DATEOPTION5 == "Date") {
        dateQuery <- paste0("date = '", input$DELETE_DATE5, "'")
      }
      if (input$DELETE_DATEOPTION5 == "Date Range") {
        dateQuery <- paste0("date >= '", input$DELETE_DATERANGE5[1], "' AND
                      date <= '", input$DELETE_DATERANGE5[2], "'")
      }
      query <- paste0("DELETE FROM current WHERE ", siteQuery, dateQuery, ";")

      #message(print(query))
      dbExecute(con, query) # delete row with matching uniqueID from current table
      # update reactive value to signal core data has changed
      changesInData$change_dataCurrent <- changesInData$change_dataCurrent + 1
      dbDisconnect(con)
      showNotification("Delete Complete.")

  }
  )

  # deletes 1 row of data in current table in MySQL
  observeEvent(input$BUTTON_DELETEROW5,{
     message("inside BUTTON_DELETEROW5")
     con = dbConnect(MariaDB(),
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = 'hbef')
      # check that row exists; if so, delete, if not, send notification
      # !!! could make cleaner with validate()
      if (input$DELETE_UNIQUEID5 %in% dataCurrent$uniqueID) {
        uID <- input$DELETE_UNIQUEID5
        query <- paste0("DELETE FROM current WHERE uniqueID = '", uID, "';")
        #message(print(query))
        dbExecute(con, query) # delete row with matching uniqueID from current table
        # update reactive value to signal core data has changed
        changesInData$change_dataCurrent <- changesInData$change_dataCurrent + 1
        dbDisconnect(con)
        showNotification("Delete Complete.")
      } else {
        showNotification("ERROR: Unable to find specified UniqueID in current dataset.")
      }
   }
  )

  # *Download Tab* ########################################

  output$table <- renderTable({
    datasetInput()
  })

  # NOTE: download does not work in RStudio, but works when shiny
  #   app is used on browser
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #  it should write out data to that filename.
  output$DOWNLOAD_DATA <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      maxDownloadDate = max(datasetInput()$date, na.rm = TRUE)
      #paste(paste('HBEFdata', input$DOWNLOAD_DATASET, paste('upto', Sys.Date(), sep=""), sep="_"), input$DOWNLOAD_FILETYPE, sep = ".")
      paste(paste('HBEFdata', input$DOWNLOAD_DATASET, Sys.Date(), sep="_"), input$DOWNLOAD_FILETYPE, sep = ".")
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$DOWNLOAD_FILETYPE, "csv" = ",", "tsv" = "\t")

      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep = sep,
              row.names = FALSE)
    }
  )

  output$DOWNLOAD_NOTES = downloadHandler(

      filename=function(dd=input$NOTE_DATES){

          dd = format.Date(sort(dd), '%Y%m%d')

          if(length(dd) == 1){
              fnpt1 = dd
          } else {
              fnpt1 = paste0(dd[1], '-', dd[length(dd)])
          }

          return(paste0(fnpt1, '_fieldnotes.zip'))
      },

      content=function(file){
          regex1 = paste(format.Date(input$NOTE_DATES, '%Y%m%d'), collapse='|')
          fns = list.files('field_notes', pattern=paste0('(', regex1, ')'),
              full.names=TRUE)
          zip(file, fns)
      },

      contentType='application/zip'
  )

  #**** END of Output ****

}) # closes shinyServer
