# Shiny server instructions for HBEF Dashboard
# Created by Maria-Carolina Simao (carolina.m.simao - at - gmail - dot - com)
# Initialized January 2018

# Search for '!!!' (without quotes) to find alerts or issues within the following code

# Some code borrowed and updated from https://github.com/akl21/hbef/blob/dev/data_stories/acid_rain/server.R

Sys.setenv(TZ='America/New_York')
options(shiny.maxRequestSize = 30*1024^2) # allows max file upload size to be 30 MB
options(show.error.locations=TRUE) # show error locations

library(colorspace)
library(dplyr)
library(dygraphs)          # allows for interactivity
library(ggplot2)
library(ggthemes)
#library(lubridate)        # Does not work with shinyapps.io: https://stackoverflow.com/questions/28656683/r-script-working-locally-not-working-on-shinyapp-io
library(RColorBrewer)
library(RMariaDB)
library(RMySQL)
library(rhandsontable)
library(reshape2)
library(shiny)
library(stringr)           # needed for str_extract function
library(tidyr)
library(xts)

message("hello, I'm at the top of server.R")

# **********************************************************************
#                      ---- DATA IMPORT & PREP ----
# **********************************************************************

# Older Data ----
# import historical precipitation data
dataHist.precip <- read.csv("data/HBEF_precipitation_chemistry_1963-2013_PLUSwaterYr.csv") 
  head(dataHist.precip$date)
  class(dataHist.precip$date)
  dataHist.precip$date <- as.Date(dataHist.precip$date, "%m/%d/%Y")           # convert date from factor to date format
  # !!! label why you're doing this
  #dataHist.precip$waterYr <- as.character(dataHist.precip$waterYr)
  #dataHist.precip$waterYr <- as.Date(dataHist.precip$waterYr, "%Y")           # convert date from factor to date format
  #dataHist.precip$waterYr <- format(dataHist.precip$waterYr, "%Y")           # convert date from factor to date format
  
# import historical stream data     
dataHist.stream <- read.csv("data/HBEF_stream_chemistry_1963-2013_PLUSwaterYr.csv")        
   # !!! years are not coming out correctly
   dataHist.stream$date <- as.Date(dataHist.stream$date, "%m/%d/%y")           # convert date from factor to date format
   dataHist.stream$ws <- paste("W", dataHist.stream$ws, sep="")                # change watershed identification to match origData

# import WaterYear 2014-6 data        
dataOrig <- read.csv("data/WY2014-7_PLUSwaterYR_upto20180328.csv")                         
   dataOrig$date <- as.Date(dataOrig$date, "%m/%d/%y")                         # convert date from factor to date format

# import MDL/LOQ data
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv")

# import weekly data entry example
dataWeeklyExample <- read.csv("data/WeeklyDataExample.csv")

# # When grabbing Data from MySQL ----
# y = RMariaDB::MariaDB()
# pass  = readLines('/home/hbef/RMySQL.config')
# con = dbConnect(y,
#                 user = 'root',
#                 password = pass,
#                 host = 'localhost',
#                 dbname = 'hbef')
# tables = dbListTables(con)
# 
# dataInitial <- dbReadTable(con, "initial")
# dataCurrent <- dbReadTable(con, "current")
# dataHistorical <- dbReadTable(con, "historical")
# dataSensor <- dbReadTable(con, "sensor")
# 
# message("head(dataInitial):")
# message(head(dataInitial))
# message("head(dataCurrent):")
# message(head(dataCurrent))
# message("head(dataHistorical):")
# message(head(dataHistorical))
# message("head(dataSensor):")
# message(head(dataSensor))
# 
# dbDisconnect(con)

# Newer Data ----
# Import all datasets & make needed changes
dataInitial <- read.csv("data/formatted/initial.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
   dataInitial$date <- as.Date(dataInitial$date, "%m/%d/%y")
dataCurrent <- read.csv("data/formatted/current_upto20180328.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA")) #, na.strings=c(""," ","NA")
   dataCurrent$date <- as.Date(dataCurrent$date, "%m/%d/%y")
dataSensor <- read.csv("data/formatted/sensor.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
   dataSensor$date <- as.Date(dataSensor$date, "%m/%d/%y")
dataHistorical <- read.csv("data/formatted/historical.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
   dataHistorical$date <- as.Date(dataHistorical$date, "%m/%d/%y")
   # Dates before 1969 are incorrect after above transformation
   # Replace dates before 1970 with date information extracted from 'uniqueID' column
   pre1968_indices <- grep("_196", dataHistorical$uniqueID, value=FALSE)
   for (i in pre1968_indices){
      dateString <- str_extract(dataHistorical$uniqueID[i], "196.....")
      dataHistorical$date[i] <- as.Date(dateString, "%Y%m%d") 
   }
   # !!! write a function that alerts user to duplicates uniqueID?

   # # CODE TO DEAL WITH DUPLICATE ROWS IN DATA
   # # remove rows that are exact duplicates
   # dataHistorical <- distinct(dataHistorical)
   # # find remaining duplicate uniqueID's and count number of them
   # duplicatesHist <- dataHistorical %>%
   #    group_by(uniqueID) %>%
   #    filter(n()>1) %>%
   #    count(uniqueID)
   # # add "Dup" (or "Dup2") to remaining duplicate's 'uniqueID' and 'duplicate' columns
   # for (i in 1:nrow(duplicatesHist)) {
   #    indices <- which(duplicatesHist$uniqueID[i] == dataHistorical$uniqueID)
   #    for (j in 1:length(indices)) {
   #       if (j > 1) {
   #          index <- indices[j]
   #          if (j == 2 ) num <- c() else num <- j-1
   #          dataHistorical$uniqueID[index] <- paste0(dataHistorical$uniqueID[index] ,"_Dup", num)
   #          dataHistorical$duplicate[index]  <- paste0("Dup",num)
   #       }
   #    }
   # }
   # # export
   # write.csv(dataHistorical, 'dataHistorical_Duplicates.csv')
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv", na.strings=c(""," ","NA"))
defClasses <- read.csv("data/formatted/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("data/formatted/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

# # CODE FOR FIRST PLACEMENT OF DATA in MySQL ----
# y = RMariaDB::MariaDB()
# pass  = readLines('/home/hbef/RMySQL.config')
# con = dbConnect(y,
#                 user = 'root',
#                 password = pass,
#                 host = 'localhost',
#                 dbname = 'hbef')
# tables = dbListTables(con)
# message(tables)
# 
# #Insert data into MySQL tables
# dbWriteTable(con, "initial", dataInitial, append=TRUE, row.names=FALSE)
# dbWriteTable(con, "current", dataCurrent, append=TRUE, row.names=FALSE)
# dbWriteTable(con, "historical", dataHistorical, append=TRUE, row.names=FALSE)
# dbWriteTable(con, "sensor", dataSensor, append=TRUE, row.names=FALSE)
# dbDisconnect(con)
   
# ****  END OF DATA IMPORT & PREP ****

# ***********************************************************************
#                    ---- IMPORTANT PRELIMINARY INFO ----
# ***********************************************************************

   
# Functions ----
# ***********************************
   
## Function to re-classify data class (e.g. numeric, character, etc.) of each variable (column) in a data 
## frame. Uses defClasses & defClassesSample data frames to match data column with its intended
## data class.
standardizeClasses <- function(d) {
      # d:              data.frame to be checked for columns with only NA's
      # ColClasses :    vector of desired class types for the data.frame
      r <- nrow(d)
      c <- ncol(d)
      for (i in 1:c) {
         ## 1. Insert an additional row with a sample value for each column
         ### Find index in defClassesSample that corresponds to column in d, save that index
         current_col_ofData <- colnames(d[i])
         ind_col <- which(current_col_ofData == colnames(defClassesSample), arr.ind = TRUE)
         ### Add corresponding sample value to last row of d
         d[r+1,i] <- defClassesSample[1,ind_col]
         ## 2. Define class of each column according to what you specified in defClasses
         ind_row <- which(current_col_ofData == defClasses$VariableName, arr.ind = TRUE)
         switch(defClasses$Class[ind_row],
                integer=as.integer(d[[i]]),
                character=as.character(d[[i]]),
                #numeric=as.numeric(as.character(d[[i]])),
                numeric=as.numeric(d[[i]]),
                Date=as.Date(d[[i]]), #, origin='1970-01-01'
                ### !!! Class below not being used, causing problems
                #POSIXct=as.POSIXct(d[[i]], "%Y-%m-%d %H:%M", tz="EST", usetz=FALSE, na.rm=TRUE),
                factor=as.factor(d[[i]])
         )
      }
      ## 3. Delete last row of sample values
      d <- d[-(r+1),]
      d
   }
   
# Find units for y-axis, depending on solute selected
ylabel <- function(solute) {
      mu <- "\U00B5" 
      # If 'solute' belong to group with different set of units, label depending on what it is
      if(input$solute %in% other_units) { 
         if (input$solute == "DIC")     ylabel3 <- paste(mu,"M/L")
         if (input$solute == "ANC960")  ylabel3 <- paste(mu, "eq/L")
         if (input$solute == "spCond") ylabel3 <- paste(mu, "S/cm")
         if (input$solute == "temp")    ylabel3 <- "Degrees Celsius"
         if (input$solute %in% c("pH3star",
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
   }
   
# **** END of Functions ****

# Theme  ----
#******************

# Graph theme
my_theme <- theme_fivethirtyeight() + 
   theme(rect = element_rect(fill = NA),
         panel.grid.major = element_line(colour = "#dddddd"), 
         text = element_text(family = "Helvetica", size = 12), 
         legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal",
            legend.box.just = "left", legend.title = element_blank(),
         strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
         axis.title= element_text(NULL), axis.title.x= element_blank(), 
         axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
   
# Set up color palette for solutes (using 'qual', or qualitative, color palette)
n <- 30 # number of colors
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=(col_vector[1:n])) # to see color wheel
   
# **** END of Theme ****


# Lists  ----
#*******************************

# Solutes
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

# Sites
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

sites_precip <- list("RG11", "RG23", "STA/22", "N", "S")

# list of solutes that have units other than mg/L for data items 
other_units <- c("pH",
                 "DIC", 
                 "ANC960", 
                 "ANCMet", 
                 "CationCharge",
                 "AnionCharge",
                 "sp.cond", 
                 "TheoryCond",
                 "temp",
                 "IonBalance")

# Set consistent colors for solutes (used when more than one is displayed) - this is not working yet
#***********************************
pal_cations <- brewer.pal(length(solutes_cations), "Paired") # set color palette for group
colors_cations <- list("cationCharge" = pal_cations[1],
                       "Ca"= pal_cations[2], 
                       "Mg" = pal_cations[3], 
                       "K"= pal_cations[4], 
                       "Na"= pal_cations[5], 
                       "TMAl"= pal_cations[6], 
                       "OMAl"= pal_cations[7], 
                       "Al_ICP"= pal_cations[8], 
                       "NH4"= pal_cations[9], 
                       "Mn"= pal_cations[10], 
                       "Fe"= pal_cations[11]) # warning: color palette used for cations group ("Paired") only has 11 colors. If you have >11 cations, need to add colors outside of palette.

pal_anions <- brewer.pal(length(solutes_anions), "Set1")  # set color palette for group
colors_anions <- list("anionCharge" = pal_anions[1],
                       "SO4" = pal_anions[2], 
                       "NO3" = pal_anions[3], 
                       "Cl" = pal_anions[4], 
                       "PO4" = pal_anions[5], 
                       "F" = pal_anions[6]) # warning: color palette used for anions group ("Set1") only has 6 colors. If you have >6 anions, need to add colors outside of palette.

pal_other <- brewer.pal(length(solutes_other), "Set3")  # set color palette for group
colors_other <- list("pH3star" = pal_other[1],
                     "pHmetrohm" = "#000000",
                     "DOC" = pal_other[2], 
                     "TDN" = pal_other[3], 
                     "DON" = pal_other[4], 
                     "DIC" = pal_other[5], 
                     "SiO2" = pal_other[6], 
                     "ANC960" = pal_other[7], 
                     "ANCMet" = pal_other[8], 
                     "spCond" = pal_other[9], 
                     "theoryCond" = pal_other[10], 
                     "temp" = pal_other[11], 
                     "ionBalance" = pal_other[12]) # warning: color palette used for other group ("Set3") only has 12 colors. If you have >12 other solutes, need to add colors outside of palette.

solute_palette <- c(colors_cations, colors_anions, colors_other)

# **** END of Lists for the sidebar ****

# **** END OF IMPORTANT PRELIMINARY INFO ****



# **********************************************************************
#                          ---- SHINY SERVER ----
# **********************************************************************

shinyServer(function(input, output, session) {
   
   # ***REACTIVITY*** ----
   # ***********************************
   
   # # !!! See if you're going to use or delete, for 5 or all years of history
   # histYears <- reactive({
   #   # Select desired historical water years
   #   if (input$SOLUTES_HIST1 == "5") {histYears <- seq((input$WATERYEAR - 5), (input$WATERYEAR - 1))}
   #   if (input$SOLUTES_HIST1 == "all")  {histYears <- seq(min(dataHist$waterYr), (input$WATERYEAR - 1))}
   #   paste(histYears, sep=" ")
   # })
   
   # DATA INPUT Reactivity #### 
   #************************
   
   
   # observeEvent(input$SUBMIT,{
   #       dataNEW <- read.csv(input$FILE_UPLOAD$datapath,
   #                           header = input$HEADER)
   #       dataNew$date <- as.Date(dataNew$date)
   #       dataNEW <- bind_rows(dataInitial, dataNEW)
   #       dataNEW <- standardizeClasses(dataNEW)
   #       print(dataNew)
   # })
   # dataNEW <- eventReactive(input$SUBMIT,{
   #       dataNEW <- read.csv(input$FILE_UPLOAD$datapath, 
   #                           header = input$HEADER)
   #       dataNew$date <- as.Date(dataNew$date)
   #       dataNEW <- bind_rows(dataInitial, dataNEW)
   #       dataNEW <- standardizeClasses(dataNEW)
   # })
   
   # Panel 1 Reactivity #### 
   #************************
   
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
         if (input$SOLUTES1 == "DIC")     ylabel1 <- paste(mu,"M/L")
         if (input$SOLUTES1 == "ANC960")  ylabel1 <- paste(mu, "eq/L")
         if (input$SOLUTES1 == "spCond") ylabel1 <- paste(mu, "S/cm")
         if (input$SOLUTES1 == "temp")    ylabel1 <- "Degrees Celsius"
         if (input$SOLUTES1 %in% c("pH3star",
                                   "pHmetrohm",
                                   "cationCharge",
                                   "cnionCharge",
                                   "theoryCond",
                                   "ionBalance")) { ylabel1 <- "(No Units)" }
         
         test <- gsub(" ", "", ylabel1, fixed = TRUE) # removes spaces in expression: https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string   
   
      } 
      # Otherwise, label as 'default' mg/L
      else {        
         ylabel1 <- "mg/L"
      }
   })
  
   # The following iterations of selecting/filtering/combining datasets are necessary because 
   # the graphing method used (dygraphs) plots *all* columns of data in the referenced data set, 
   # rather than permitting some data columns to be hidden. Also, the calculations of median
   # and IQR for historical data are necessary because dygraphs cannot plot boxplots when the 
   # x-axis is continuous.
   
   # filters recent ('original') data; i.e. wateryear, site, solute data from recent water year data
   dataOrig1 <- reactive({
     
     dataOrig1 <- dataOrig %>% 
       filter(waterYr %in% input$WATERYEAR1) %>%                # Filter data to selected water year
       filter(site %in% input$SITES1) %>%                       # Filter data to selected site
       select(one_of("date", input$SOLUTES1))   # Select desired columns of data

   }) # END of dataOrig1
   
   # filters recent ('original') data and discharge; filters wateryear, site, solute, discharge data from recent water year data
   dataOrigQ1 <- reactive({
      
      # data selection if GageHt is selected as source for Discharge/Precipitation
      if (input$GAGEHT_or_Q1 == 'gageHt'){
         dataOrigQ1 <- dataOrig %>% 
            filter(waterYr %in% input$WATERYEAR1) %>%            # Filter data to selected water year
            filter(site %in% input$SITES1) %>%                   # Filter data to selected site
            select(one_of("date", input$SOLUTES1, "gageHt")) %>% # Selected desired columns of data
            rename(GageHt_or_Q = gageHt)                         # Rename GageHt to standard name, so that don't have to create alternative graphs
      } 
      
      # data selection if Q is selected as source for Discharge/Precipitation
      if (input$GAGEHT_or_Q1 == 'flowGageHt'){
         dataOrigQ1 <- dataOrig %>% 
            filter(waterYr %in% input$WATERYEAR1) %>%            # Filter data to selected water year
            filter(site %in% input$SITES1) %>%                   # Filter data to selected site
            select(one_of("date", input$SOLUTES1, "flowGageHt")) %>%      # Selected desired columns of data
            rename(GageHt_or_Q = flowGageHt)                              # Rename Q to standard name, so that don't have to create alternative graphs
      } 
      
      dataOrigQ1
         
   }) # END of dataOrigQ1
   
   # filters historical data; i.e. site, solute, from historical data
   dataHist1 <- reactive({
     
     # Selects appropriate historical data set (stream or precip) based on site selected
     if (input$SITES1 %in% sites_streams) {
        dataHist <- dataHist.stream
        dataHist <- dataHist %>%          # Keep if only you want to see historical data
           filter(ws %in% input$SITES1)   # from selected watershed
        }
     else {
        dataHist <- dataHist.precip %>% 
            filter(site %in% input$N_or_S1) # Filters rain gage sites to either N or S sites
        } 
     
     # !!! only works for watersheds, i.e. stream data; figure out how to show precip data
     dataHist1 <- dataHist %>% 
       select(one_of("date", input$SOLUTES1)) %>%  # Select desired columns of solute data
       separate(date, c("y","m","d"))              # Separate date into year, month, and day (to use month in next code block)
     
     # Calculate median and IQR values per month
     median <- tapply(dataHist1[,4], dataHist1$m, median, na.rm=TRUE)
     IQR <- tapply(dataHist1[,4], dataHist1$m, IQR, na.rm=TRUE)
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
     dataHist1 <- data.frame(date = date,
                             solute.IQRlower = IQR.lower,
                             solute.median = median,
                             solute.IQRupper = IQR.upper)
     dataHist1
   }) # END of dataHist1
   
   # combines site, solute data from recent water year data with historical data
   dataOrigHist1 <- reactive ({
      dataOrigHist1 <- full_join(dataOrig1(), dataHist1(), by = "date")
      return(dataOrigHist1)
   }) #END of dataOrigHist1
   
   # combines site, solute, and discharge data from recent water year dataset with historical data
   dataOrigQHist1 <- reactive ({
      dataOrigQHist1 <- full_join(dataOrigQ1(), dataHist1(), by = "date")
      return(dataOrigQHist1)
   }) #END of dataOrigQHist1
   
   
   dygraph1 <- reactive ({
      if (input$HYDROLOGY1 == TRUE)   {
         if (input$SOLUTES_HIST1 == TRUE) {
            
            # Plots Default + Discharge + Historical data
            data1 <- dataOrigQHist1()
            data1.xts <- xts(data1[,-1], order.by = data1$date)
            #paste(c("XTS:", class(dataOrig1$FieldCode)))
            
            dygraph1 <- dygraph(data1.xts) %>%
               dyAxis("x", label = paste("Water Year", input$WATERYEAR1),
                      axisLabelColor = "black") %>%
               dyAxis("y", label = ylabel,
                      independentTicks=TRUE,
                      axisLabelColor = "black") %>%
               dyAxis('y2',label='Hydrology (mm or L/s)',
                      independentTicks=TRUE,
                      axisLabelColor = "#3182bd",
                      axisLabelWidth = 70,
                      axisLineColor = "#3182bd") %>%
               dySeries(name = input$SOLUTES1,
                        color = "black",
                        drawPoints = TRUE,
                        pointSize = 3,
                        axis='y') %>%
               dySeries(name = 'GageHt_or_Q',
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
            
         } else {
            
            # Plots Default + Discharge data
            data1 <- dataOrigQ1()
            data1.xts <- xts(data1[,-1], order.by = data1$date)
            
            dygraph1 <- dygraph(data1.xts) %>%
               dyAxis("x", label = paste("Water Year", input$WATERYEAR1)) %>%
               dyAxis("y", label = ylabel, independentTicks=TRUE) %>%
               dyAxis('y2',label='Hydrology (mm or L/s)', independentTicks=TRUE,
                      axisLabelWidth = 70,
                      axisLabelColor = "#3182bd",
                      axisLineColor = "#3182bd") %>% # color is light blue
               dySeries(name = input$SOLUTES1,
                        color = "#black") %>%
               dySeries(name = 'GageHt_or_Q',
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
            data1 <- dataOrigHist1()
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
            
            # Plots Default data
            
            data1 <- dataOrig1()
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
               #          color = "black",
               #          drawPoints = TRUE,
               #          strokeWidth = 0,
               #          pointSize = 1) %>%
               # for (i in 1:nrow(data1.xts)) {
               #    dyAnnotation(index(i), data1.xts$FieldCode[i])
               # } %>%
               dyLimit(limit = LOQ1(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
               dyLimit(limit = MDL1(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
               dyOptions(drawGrid = FALSE,
                         connectSeparatedPoints=TRUE,
                         includeZero = TRUE)
            
            dygraph1
         }
      }
   })
   dygraph1.fun <- function() {
      if (input$HYDROLOGY1 == TRUE)   {
         if (input$SOLUTES_HIST1 == TRUE) {
            
            # Plots Default + Discharge + Historical data
            data1 <- dataOrigQHist1()
            data1.xts <- xts(data1[,-1], order.by = data1$date)
            #paste(c("XTS:", class(dataOrig1$FieldCode)))
            
            dygraph1 <- dygraph(data1.xts) %>%
               dyAxis("x", label = paste("Water Year", input$WATERYEAR1),
                      axisLabelColor = "black") %>%
               dyAxis("y", label = ylabel,
                      independentTicks=TRUE,
                      axisLabelColor = "black") %>%
               dyAxis('y2',label='Hydrology (mm or L/s)',
                      independentTicks=TRUE,
                      axisLabelColor = "#3182bd",
                      axisLabelWidth = 70,
                      axisLineColor = "#3182bd") %>%
               dySeries(name = input$SOLUTES1,
                        color = "black",
                        drawPoints = TRUE,
                        pointSize = 3,
                        axis='y') %>%
               dySeries(name = 'GageHt_or_Q',
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
            
         } else {
            
            # Plots Default + Discharge data
            data1 <- dataOrigQ1()
            data1.xts <- xts(data1[,-1], order.by = data1$date)
            
            dygraph1 <- dygraph(data1.xts) %>%
               dyAxis("x", label = paste("Water Year", input$WATERYEAR1)) %>%
               dyAxis("y", label = ylabel, independentTicks=TRUE) %>%
               dyAxis('y2',label='Hydrology (mm or L/s)', independentTicks=TRUE,
                      axisLabelWidth = 70,
                      axisLabelColor = "#3182bd",
                      axisLineColor = "#3182bd") %>% # color is light blue
               dySeries(name = input$SOLUTES1,
                        color = "#black") %>%
               dySeries(name = 'GageHt_or_Q',
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
            data1 <- dataOrigHist1()
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
            
            # Plots Default data
            
            data1 <- dataOrig1()
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
               #          color = "black",
               #          drawPoints = TRUE,
               #          strokeWidth = 0,
               #          pointSize = 1) %>%
               # for (i in 1:nrow(data1.xts)) {
               #    dyAnnotation(index(i), data1.xts$FieldCode[i])
               # } %>%
               dyLimit(limit = LOQ1(), label = "LOQ", color = "#fc9272", strokePattern = "dotdash") %>%
               dyLimit(limit = MDL1(), label = "MDL", color = "#de2d26", strokePattern = "dotdash") %>%
               dyOptions(drawGrid = FALSE,
                         connectSeparatedPoints=TRUE,
                         includeZero = TRUE)
            
            dygraph1
         }
      }
   } # END of graphs1
   
   
   # END of PANEL 1
   
   # Panel 2 Reactivity ####
   #************************
   
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
         if (input$SOLUTES2[i] == "DIC")     ylabel2[i] <- paste(mu,"M/L") # !!! see code from MatthewRss&Aaron to try their way of printing mu
         if (input$SOLUTES2[i] == "ANC960")  ylabel2[i] <- paste(mu, "eq/L")
         if (input$SOLUTES2[i] == "spCond") ylabel2[i] <- paste(mu, "S/cm")
         if (input$SOLUTES2[i] == "temp")    ylabel2[i] <- "Degrees Celsius"
         if (input$SOLUTES2[i] %in% c("pH3star",
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
   
   # filters Original (recent water year) data to only include data selected by inputs
   dataOrig2 <- reactive({
      dataOrig2 <- dataOrig %>% 
         filter(waterYr %in% input$WATERYEAR2) %>%     # Filter data to selected water year
         filter(site %in% input$SITES2) %>%            # Filter data to selected sites
         select(one_of("date", input$SOLUTES2))        # Keep date and selected input data
   }) # END of dataOrig2()
   
   # filters Original (recent water year) data to include data selected by inputs AND discharge/precip
   dataOrigQ2 <- reactive({
      
      # data selection if GageHt is selected as source for Discharge/Precipitation
      if (input$GAGEHT_or_Q2 == 'gageHt'){
         dataOrigQ2 <- dataOrig %>% 
            filter(waterYr %in% input$WATERYEAR2) %>%            # Filter data to selected water year
            filter(site %in% input$SITES2) %>%                   # Filter data to selected site
            select(one_of("date", input$SOLUTES2, "gageHt")) %>% # Selected desired columns of data
            rename(GageHt_or_Q = gageHt)                         # Rename GageHt to standard name, so that don't have to create alternative graphs
      } 
      
      # data selection if Q is selected as source for Discharge/Precipitation
      if (input$GAGEHT_or_Q2 == 'flowGageHt'){
         dataOrigQ2 <- dataOrig %>% 
            filter(waterYr %in% input$WATERYEAR2) %>%            # Filter data to selected water year
            filter(site %in% input$SITES2) %>%                   # Filter data to selected site
            select(one_of("date", input$SOLUTES2, "flowGageHt")) %>%      # Selected desired columns of data
            rename(GageHt_or_Q = flowGageHt)                              # Rename Q to standard name, so that don't have to create alternative graphs
      } 
      
      dataOrigQ2

   }) # END of dataOrigQ2()
   
   # **** END of Panel 2 Reactivity ****
   
   
   # Panel 3 Reactivity####
   #************************
   
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
       if (input$SOLUTES3 == "DIC")     ylabel3 <- paste(mu,"M/L")
       if (input$SOLUTES3 == "ANC960")  ylabel3 <- paste(mu, "eq/L")
       if (input$SOLUTES3 == "spCond") ylabel3 <- paste(mu, "S/cm")
       if (input$SOLUTES3 == "temp")    ylabel3 <- "Degrees Celsius"
       if (input$SOLUTES3 %in% c("pH3star",
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
   
   # filters Original (recent water year) data to only include data selected by inputs
   dataOrig3 <- reactive({
   
     dataOrig3 <- dataOrig %>% 
       filter(waterYr %in% input$WATERYEAR3) %>%          # Filter data to selected water year
       filter(site %in% input$SITES3) %>%                 # Filter data to selected sites
       select(one_of("date", "site", input$SOLUTES3)) %>% # Keep date, site, and solute data
       mutate(i = row_number()) %>%                       # Create new columns of data of just row numbers. Necessary to prevent error message of duplicate values after next line of code, but inefficient because doesn't combine rows with duplicate columns.)
       spread_(key_col = "site", value_col = input$SOLUTES3, fill=NA) %>%  # Reshape data so that each place in "sites" is made into a unique column, with corresponding solute value as data
       select(-i)                                         # Remove row name variable
     
       }) # END of dataOrig3()
   
   
   # gathers hydrology data and calculates median hydrology values
   Q3 <- reactive({
     
     # if Discharge is selected, finds data for all watershed (stream) sites, and calculates median
     if (input$HYDROLOGY3 == 'Discharge') {
       if (input$GAGEHT_or_Q3 == 'GageHt') {
         Q3 <- dataOrig %>%
           filter(waterYr %in% input$WATERYEAR3) %>%          # Filter data to selected water year
           filter(site %in% sites_streams) %>% 
           select(one_of("date", input$GAGEHT_or_Q3)) %>% 
           group_by(date) %>% 
           summarise(Hydro.med = median(GageHt, na.rm=TRUE))}
       else { #i.e. if input$GAGEHT_or_Q3 == 'Q'
         Q3 <- dataOrig %>%
           filter(waterYr %in% input$WATERYEAR3) %>%          # Filter data to selected water year
           filter(site %in% sites_streams) %>% 
           select(one_of("date", input$GAGEHT_or_Q3)) %>% 
           group_by(date) %>% 
           summarise(Hydro.med = median(Q, na.rm=TRUE))}
       } # end of Discharge if statement
     
     # if Precipitation is selected, finds data for all rain gage (precip) sites, and calculates median
     if (input$HYDROLOGY3 == 'Precipitation') {
       if (input$GAGEHT_or_Q3 == 'GageHt') {
        Q3 <- dataOrig %>%
           filter(waterYr %in% input$WATERYEAR3) %>%          # Filter data to selected water year
           filter(site %in% sites_precip) %>% 
           select(one_of("date", input$GAGEHT_or_Q3)) %>% 
           group_by(date) %>% 
           summarise(Hydro.med = median(GageHt, na.rm=TRUE))}
       else { #i.e. if input$GAGEHT_or_Q3 == 'Q'
         Q3 <- dataOrig %>%
           filter(waterYr %in% input$WATERYEAR3) %>%          # Filter data to selected water year
           filter(site %in% sites_precip) %>% 
           select(one_of("date", input$GAGEHT_or_Q3)) %>% 
           group_by(date) %>% 
           summarise(Hydro.med = median(Q, na.rm=TRUE))}
      } # end of Preciptiation if statement
     
     Q3
     
   }) # end of Q3()
   
   # filters Original (recent water year) data to include data selected by inputs AND discharge/precip
   dataOrigQ3 <- reactive({
     dataOrigQ3 <- full_join(dataOrig3(), Q3(), by = "date")
     return(dataOrigQ3)
   }) # END of dataOrigQ3()
   # **** END of Panel 3 Reactivity ****   
   
   # Panel 4 Reactivity####
   #************************

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

   # Join data together
   # 1. Re-Classify data class of each column to ensure consistency aross tables
   ## (necessary for merging data tables)
   defClassesSample$date <- as.Date(defClassesSample$date, format="%m/%d/%y")
   defClassesSample$date <- as.Date(defClassesSample$date, format="%Y-%m-%d")
   dataHistorical <- standardizeClasses(dataHistorical)
   dataCurrent <- standardizeClasses(dataCurrent)
      # !!! for some reason 'precipCatch' needs to be corrected
      dataCurrent$precipCatch <- as.numeric(dataCurrent$precipCatch)
      ## remove columns that don't match up between datasets
      dataCurrent_without_pHmetrohm <- select(dataCurrent, -pHmetrohm)
   # 2. Join data together
   dataAll <- bind_rows(dataCurrent_without_pHmetrohm, dataHistorical)
   # 3. Filter data according to inputs
   ## Base data
   data4 <- reactive ({
      data4 <- dataAll %>%
         filter(date >= input$DATE4[1]) %>%
         filter(date <= input$DATE4[2])
   })
   ## Data for Precip plot
   dataPrecip4 <- reactive ({
      dataPrecip4 <- data4() %>%
         select(one_of("date", "site", "precipCatch")) %>%
         filter(site %in% sites_precip) %>%
         group_by(date) %>%
         summarise(PrecipMedian = median(precipCatch, na.rm=TRUE))
   })
   ## Data for Main plot
   dataMain4 <- reactive ({
      dataMain4 <- data4() %>%
         filter(site %in% input$SITES4) %>%
         select(one_of("date", "site", input$SOLUTES4, "fieldCode")) %>%  # Keep date, site, solute & fieldcode data
         group_by(date, site) %>%
         gather(key = solute, value = solute_value, -site, -date, -fieldCode)  # Reshape data for ggplot2 plotting
   })
   ## Data for Flow plot
   dataFlow4 <- reactive ({
      dataFlow4 <- data4() %>%
         select(one_of("date", "site", "gageHt", "flowGageHt")) %>%
         filter(site %in% sites_streams) %>%
         group_by(date) %>%
         summarise(gageHtMedian = median(gageHt, na.rm=TRUE),
                   flowGageHtMedian = median(flowGageHt, na.rm=TRUE))
         #           hydroGraph = first(hydroGraph, na.rm=TRUE)) #!!! ability to choose source of stream/flow data
   })
   ## Additional data for Flow plot: hydroGraph labels
   dataFlowHydroGraph4 <- reactive ({
      dataFlowHydroGraph4 <- data4() %>%
         filter(site %in% input$HYDROLIMB_SITE4) %>%
         select(one_of("date", "hydroGraph"))
      #           hydroGraph = first(hydroGraph, na.rm=TRUE)) #!!! ability to choose source of stream/flow data
   })

   # **** END of Panel 4 Reactivity ****
   
   
   # ***OUTPUT*** ----
   # ***********************************
   
   
   # DATA INPUT Output #########################################
   
   # Upload .csv ####
   output$FILE_PREVIEW <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.

      req(input$FILE_UPLOAD)

      # for testing
      # df <- read.csv("data/Formatted/test.csv",
      #                header = TRUE,
      #                stringsAsFactors = FALSE,
      #                na.strings=c(""," ","NA"))
      
      df <- read.csv(input$FILE_UPLOAD$datapath,
                     header = input$HEADER,
                     stringsAsFactors = FALSE, 
                     na.strings=c(""," ","NA"))
      df$date <- as.Date(df$date, "%m/%d/%y") 
      df$date <- as.Date(df$date, "%Y/%m/%d")
      # Remove "archived" column (if it exists)
      if ("archived" %in% colnames(df)) {
         ind <- which("archived" == colnames(df), arr.ind = TRUE)
         df <- df[,-ind]
      }
      
      df <- standardizeClasses(df)
      dataInitial <- standardizeClasses(dataInitial)
      # classes <- NA
      # for (i in 1:ncol(d)) classes[i] <- class(d[[i]])
      dfNew <- bind_rows(dataInitial, df)
      return(dfNew)
#
#       if(input$disp == "head") {
#          return(head(df))
#       }
#       else {
#          return(df)
#       }
      
   })
   
   # Excel-like Entry Output ####
   #*****************************
   
   output$EXCEL <- renderRHandsontable(
     rhandsontable(dataWeeklyExample)
   )
   
   
   # QA/QC tab #########################################
   
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
     ylabel <- paste(ylabel1()) # get ylabel
     dygraph1()
   }) # END of output$GRAPH1
   
   #output$TABLE1 <- renderDataTable(dataOrig1()) # for testing purposes
   
   output$PRINT1 <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {
         paste("HBEF_1Solute1Site_", Sys.time(), ".pdf", sep="") #can add input$var3 if you want people to choose b/w pdf, png, etc.
         },
      content = function(file) {
         # open the device
         pdf(file) #, onefile = FALSE
         # create the plot
         dygraph1.fun()
         # close the device
         dev.off()
         
         # # Copy the report file to a temporary directory before processing it, in
         # # case we don't have write permissions to the current working dir (which
         # # can happen when deployed).
         # tempReport <- file.path(tempdir(), "1Solute1Site.Rmd")
         # file.copy("1Solute1Site.Rmd", tempReport, overwrite = TRUE)
         # 
         # # Set up parameters to pass to Rmd document
         # params <- list(HYDROLOGY1 = input$HYDROLOGY1,
         #                SOLUTES_HIST1 = input$SOLUTES_HIST1,
         #                WATERYEAR1 = input$WATERYEAR1,
         #                SOLUTES1 = input$SOLUTES1, 
         #                LOQ1 = LOQ1(),
         #                MDL1 = MDL1(),
         #                dataOrigQHist1 = dataOrigQHist1(),
         #                dataOrigQ1 = dataOrigQ1(),
         #                dataOrigHist1 = dataOrigHist1(),
         #                dataOrig1 = dataOrig1(),
         #                dataHist1 = dataHist1())
         # 
         # # Knit the document, passing in the `params` list, and eval it in a
         # # child of the global environment (this isolates the code in the document
         # # from the code in this app).
         # rmarkdown::render(tempReport, output_file = file,
         #                   params = params,
         #                   envir = new.env(parent = globalenv())) 
         
      }, # end of content
      contentType = 'image/png'
   ) # end of downloadHandler
   
   
   # Panel 2 Output ####
   #********************
   
   output$TITLE2 <-  renderText({
     paste(c(title.Solutes2(), "from site", input$SITES2,"in water year", input$WATERYEAR2))
   })
   
   output$GRAPH2 <- renderDygraph({
     
     # ylabel2 <- ylabel2()
     # print(ylabel2)
     
     if (input$HYDROLOGY2 == TRUE)   {
           
           # Plots Default + Discharge data
           data2 <- dataOrigQ2()
           data2.xts <- xts(data2[,-1], order.by = data2$date)
           
           dygraph(data2.xts) %>%
              dyAxis("x", label = paste("Water Year", input$WATERYEAR2)) %>%
              dyAxis("y", label = "(various units, dependent on input)", independentTicks=TRUE) %>%
              dyAxis('y2',label='Hydrology (mm or L/s)', independentTicks=TRUE,
                     axisLabelWidth = 70,
                     axisLabelColor = "#3182bd",
                     axisLineColor = "#3182bd") %>% # color is light blue
              #dySeries(name = input$SOLUTES2) %>% 
              dySeries(name = 'GageHt_or_Q',
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
           
           data2 <- dataOrig2() 
           data2.xts <- xts(data2, order.by = data2$date)
           
           # padrange <- c(min(data2.xts$input$SOLUTES2, na.rm=TRUE) - 1, max(data2.xts$input$SOLUTES2, na.rm=TRUE) + 1) # !!! attempt at resolving negative values issue
           # add "valueRange = padrange" in dyAxis if working; currently returns warning that all arguments are missing
           
           dygraph(data2.xts) %>%
              dyAxis("x", label = paste("Water Year", input$WATERYEAR2)) %>%
              dyAxis("y", label = "(various units, dependent on input)", independentTicks=TRUE) %>%
              # dySeries(name = input$SOLUTES2,
              #          drawPoints = TRUE,
              #          strokeWidth = 1,
              #          pointSize = 3) %>%
              dyOptions(drawGrid = FALSE,
                        connectSeparatedPoints=TRUE,
                        includeZero = TRUE,
                        drawPoints = TRUE,
                        strokeWidth = 1,
                        pointSize = 3)
        }
     
   }) # END of output$GRAPH2
   
   # output$GRAPH <- renderPlot({
      
      # basePlot <-  ggplot(data = dataOrigSelected(), aes(x = date, y = SOLUTES_value)) +
      #   scale_x_date(date_labels = "%b", date_break = "1 month") +
      #   geom_point(aes(x = date, y = SOLUTES_value, color = SOLUTES)) +
      #   theme_minimal() 
      # 
      # basePlot
      
      # if (input$DISCHARGE == TRUE) {
      #     # Base map
      #     basePlot +
      #       geom_area(aes(x = date, y = Flow), bg = rgb(204,204,204, maxColorValue = 255), na.rm = TRUE, show.legend = TRUE)
      # }
      
     
   # }) # END of output$graph
   
   # For testing purposes (of data sorting):
   # ***************************************
   output$TABLE2 <- renderDataTable({
      if (input$HYDROLOGY2 == FALSE) dataOrig2()
      else dataOrigQ2()
   }) # END of output$TABLE2
   
   # Panel 3 Output ####
   #********************
   
   output$TITLE3 <-  renderText({
     paste(c(input$SOLUTES3, "from site", title.Sites3(),"in water year", input$WATERYEAR3))
   })
   
   
   output$LIMITS3 <- renderText({
    paste(c("MDL:",MDL3(), "  LOQ:", LOQ3()))
   })
   
   output$GRAPH3 <- renderDygraph({
     
       # Plots Default + Discharge data
       if (input$HYDROLOGY3 == "Discharge" | input$HYDROLOGY3 == "Precipitation") {
         
       if (input$HYDROLOGY3 == "Discharge")   {

       data3 <- dataOrigQ3()
       data3.xts <- xts(data3[,-1], order.by = data3$date)

       dygraph(data3.xts) %>%
         dyAxis("x", label = paste("Water Year", input$WATERYEAR3)) %>%
         dyAxis("y", label = ylabel3(), independentTicks=TRUE) %>%
         dyAxis('y2',label='Hydrology (mm or L/s)', independentTicks=TRUE,
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
           
           data3 <- dataOrigQ3()
           data3.xts <- xts(data3[,-1], order.by = data3$date)
           
           dygraph(data3.xts) %>%
             dyAxis("x", label = paste("Water Year", input$WATERYEAR3)) %>%
             dyAxis("y", label = ylabel3(), independentTicks=TRUE) %>%
             dyAxis('y2',label='Hydrology (mm or L/s)', independentTicks=TRUE,
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
       
       data3 <- dataOrig3() 
       data3.xts <- xts(data3, order.by = data3$date)
       
       # padrange <- c(min(data3.xts$input$SOLUTES3, na.rm=TRUE) - 1, max(data3.xts$input$SOLUTES3, na.rm=TRUE) + 1) # !!! attempt at resolving negative values issue
       # add "valueRange = padrange" in dyAxis if working; currently returns warning that all arguments are missing
       
       dygraph(data3.xts) %>%
         dyAxis("x", label = paste("Water Year", input$WATERYEAR3)) %>%
         dyAxis("y", label = ylabel3(), independentTicks=TRUE) %>%
         # dySeries(name = input$SOLUTES3,
         #          drawPoints = TRUE,
         #          strokeWidth = 1,
         #          pointSize = 3) %>%
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
      if (input$HYDROLOGY3 == "None") dataOrig3()
      else dataOrigQ3()
   }) # end of output$TABLE3 
   
   
   # Panel 4 Output ####
   #********************

   output$TITLE4 <- renderText ({print(input$SITES4)})
   output$GRAPH_PRECIP4 <- renderPlot({
      data <- dataPrecip4()
      x <- data$date
      y <- data$PrecipMedian
      p <- ggplot(data, aes(x, y)) + my_theme +
         geom_col(color="blue", fill = "lightblue", width = 0.9, na.rm=TRUE) +
         labs(x = "", y = "Precipitation (Catch)") +
         scale_y_reverse()
      p
   }) # end of output$GRAPH_PRECIP4
   output$GRAPH_MAIN4 <- renderPlot({
      # data prep
      data <- dataMain4()
      x <- data$date
      y <- data$solute_value
      # build ggplot function
      m <- ggplot(data, aes(x, y, shape=data$solute, color=data$site)) +
         my_theme +
         geom_point(size = 2.5) +
         geom_line(alpha = 0.5) +
         scale_x_date(date_labels = "%Y-%b")
      # If show field code is selected, add to ggplot
      if (input$FIELDCODE4 == TRUE) {
         m <- m + geom_text(aes(label=data$fieldCode), 
                            nudge_y = (max(data$solute_value, na.rm = TRUE) - min(data$solute_value, na.rm = TRUE))/15,
                            check_overlap = TRUE)
      }
      # plot
      m

   }) # end of output$GRAPH_MAIN4
   output$GRAPH_FLOW4 <- renderPlot({
      # # Hydrology plot
      # y <- data$gageHt
      # f <- ggplot(data, aes(x, y)) + my_theme +
      #    geom_area(color="blue", fill = "lightblue", na.rm=TRUE)
      # f
   }) # end of output$GRAPH_FLOW4
   paste(head(dataCurrent))

   output$TABLE4 <- renderDataTable({
      dataMain4()
      #head(dataCurrent)
   }) # end of output$TABLE4

   #**** END of Output ****
   
}) # closes shinyServer




