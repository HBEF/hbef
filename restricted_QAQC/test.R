
library(dplyr)
library(dygraphs)          # allows for interactivity
library(ggplot2)
#library(lubridate)        # Does not work with shinyapps.io: https://stackoverflow.com/questions/28656683/r-script-working-locally-not-working-on-shinyapp-io
library(RColorBrewer)
library(reshape2)
library(shiny)
library(tidyr)
library(xts)


# Playing around ----

# Findings ranges of solute columns
# for DataOrig
ranges <- matrix(rep(NA,90), nrow=30)
j <- 1
for (i in 5:34) {
   temp <- range(dataOrig[,i], na.rm=TRUE)
   ranges[j, 1:3] <- c(temp, colnames(dataOrig[i]))
   j <- j + 1
}
# for dataHist.stream
ranges <- matrix(rep(NA,51), nrow=17)
j <- 1
for (i in 6:22) {
   temp <- range(dataHist.stream[,i], na.rm=TRUE)
   ranges[j, 1:3] <- c(temp, colnames(dataHist.stream[i]))
   j <- j + 1
}

# CODE TO TEST NEW DATA SYSTEM ----
#*****************************

library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)        # needed for str_extract function

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
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv", na.strings=c(""," ","NA"))
defClasses <- read.csv("data/formatted/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("data/formatted/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
   defClassesSample$date <- as.Date(helpClassesSample$date, "%Y-%m-%d")
  
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
# # Code to test standardizeClasses()
#     d <- dataCurrent[1:10, ]
#     testD <- standardizeClasses(d)
#     class(testD$precipCatch)
#     testD.classes <- c()
#     for (i in 1:ncol(testD))
#        testD.classes[i] <- class(testD[[i]])
#     e <- dataHistorical[1:10, ]
#     testE <- standardizeClasses(e)
#     class(testE$precipCatch)
#     testE.classes <- c()
#     for (i in 1:ncol(testE))
#        testE.classes[i] <- class(testE[[i]])

## RE-CLASSIFY DATA CLASS OF EACH COLUMN
## to ensure consistency aross tables (necessary for merging data tables)
dataCurrent <- standardizeClasses(dataCurrent)
   dataCurrent$precipCatch <- as.numeric(dataCurrent$precipCatch) # for some reason this continued as character
dataHistorical <- standardizeClasses(dataHistorical)

# JOIN DATA TOGETHER
## remove & save columns that don't match up between datasets
dataCurrent_without_pHmetrohm <- dataCurrent %>% 
   select(-pHmetrohm)
dataALL <- bind_rows(dataCurrent_without_pHmetrohm, dataHistorical)

# GRAPH EVERYTHING!

my_theme <- theme_fivethirtyeight() + 
   theme(rect = element_rect(fill = NA),
         panel.grid.major = element_line(colour = "#dddddd"), 
         text = element_text(family = "Helvetica", size = 12), 
         legend.position = "none", legend.direction = "vertical", legend.title = element_blank(),
         strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
         axis.title= element_text(NULL), axis.title.x= element_blank(), 
         axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

data <- filter(dataALL, site == "RG11")
data <- data[1:100,] # DELETE AFTER TESTING
x <- data$date
# Precipitation plot
y <- data$precipCatch
p <- ggplot(data, aes(x, y)) + my_theme +
   geom_col(color="blue", fill = "lightblue", na.rm=TRUE) +
   labs(x = "", y = "Precipitation (Catch)") +
   scale_y_reverse()
p
# Main plot
y <- data$Ca
m <- ggplot(data, aes(x, y)) + my_theme +
   geom_point(na.rm=TRUE) +
   scale_x_date(date_labels = "%Y-%b")
m
# Hydrology plot
y <- data$gageHt
h <- ggplot(data, aes(x, y)) + my_theme +
   geom_area(color="blue", fill = "lightblue", na.rm=TRUE)
h


# CODE TO USE DATETIME ----
#*********************
#; fills in  NA's and makes usable [only problem is years before 1969, coming up as 2068]

# Import data
test <- dataHist.precip

# Make "datetime" suitable for graphing
# 1. making date and datetime columns to 'character' class, need this for 'for' loop to work
test$datetime <- as.character(test$datetime)                
test$date <- as.character(test$date)
# 2. 'for' loop: fills empty (NA) datetime elements with date and empty time 
for (i in 1:nrow(test)) {
   if (is.na(test$datetime[i])) {
      test$datetime[i] <- (test$date[i])
      test$datetime[i] <- paste(test$datetime[i], " ", "00:00")
   }
}
# 3. convert datetime to 'POSIXlt' class
# !!! Issue: years before 1969 are not formatted properly
test$datetime <- strptime(test$datetime, "%m/%d/%y %H:%M") 
#test$datetime <- as.Date(test$datetime) # this deletes the time...
# 4. Attempt(s) at resolving issue where years before 1969 are not formatted properly
# test$waterYr <- as.numeric(test$waterYr)
# for (i in 1:nrow(test)) {
#    if (test$waterYr[i] < 1969) {
#       test$datetime[i] <- strptime(test$datetime[i], "%m/%d/19%y %H:%M")
#    }
# }

# Isolate dates you need, Convert those to character, make 20->19 (general exp, or substr()), then convert back to date

# CODE TO TRY AND FIGURE OUT DYGRAPH ISSUES ----
#*******************************************

# Testing how to transform row data into columns data and plot ----
#*******************************************************************

# filters Original (recent water year) data to only include data selected by inputs

  test <- dataOrig %>% 
    filter(waterYr == 2014) %>%          # Filter data to selected water year
    filter(site %in% c("W1", "W2")) %>%                 # Filter data to selected sites
    select(one_of("date", "site", "Ca")) %>% # Keep date, site, and solute data
    mutate(i = row_number()) %>% 
    spread(key = site, value = "Ca", fill = NA)                       # Reshape data so that each place in "sites" is made into a unique column, with corresponding solute value as data
 # END of dataOrig3()


# Testing out dySeries ribbon ----
#******************************

# Works if those colums are the only thing in the data column
Ca_plus1 <- test$Ca + 1 
Ca_1 <- test$Ca - 1
test_IQRsubset <- data.frame(test$date, Ca_1, test$Ca, Ca_plus1)
test_IQRsubset$test.date <- as.Date(test_IQRsubset$test.date,  "%m/%d/%y")

test_IQRsubset.xts <- xts(test_IQRsubset[,-1], order.by = test_IQRsubset$test.date)

dygraph(test_IQRsubset.xts) %>% 
   dyAxis("x", label = paste("Water Year", wy)) %>%
   dyAxis("y", label = 'mg/L', independentTicks=TRUE) %>%
   dySeries(c('Ca_1', 'test.Ca', 'Ca_plus1'))

# Works if additional things are in data column
Ca_plus1 <- test$Ca + 1 
Ca_1 <- test$Ca - 1
test_IQRsubset <- data.frame(test$date, Ca_1, test$Ca, Ca_plus1, test$Mg)
test_IQRsubset$test.date <- as.Date(test_IQRsubset$test.date,  "%m/%d/%y")

test_IQRsubset.xts <- xts(test_IQRsubset[,-1], order.by = test_IQRsubset$test.date)

dygraph(test_IQRsubset.xts) %>% 
   dyAxis("x", label = paste("Water Year", wy)) %>%
   dyAxis("y", label = 'mg/L', independentTicks=TRUE) %>%
   dySeries(c('Ca_1', 'test.Ca', 'Ca_plus1')) %>% 
   dySeries('test.Mg', drawPoints = TRUE)

# Does it work if values are far apart?
dates <- seq(1969, 2063, by=1)
y <- c(rep("NA",18), 1.5)
y <- as.numeric(rep(y, 5))
y_1 <- as.numeric(rep(c(rep("NA",18), 0.5), 5))
y1 <- as.numeric(rep(c(rep("NA",18), 2.5), 5))
Ca <- test$Ca[500:594]

test_IQRsubset <- data.frame(dates, y_1, y, y1, Ca)
test_IQRsubset$dates <- as.Date(as.character(test_IQRsubset$dates),  "%Y")

test_IQRsubset.xts <- xts(test_IQRsubset[, -1], order.by = test_IQRsubset$dates)

dygraph(test_IQRsubset.xts) %>% 
   dyAxis("x", label = paste("Water Year", wy)) %>%
   dyAxis("y", label = 'mg/L', independentTicks=TRUE) %>%
   dySeries(c('y_1', 'y', 'y1')) %>% 
   dySeries('Ca', drawPoints = TRUE) %>% 
   dyOptions(connectSeparatedPoints=TRUE)

print(test)


# Testing out adding dygraphs in chunks ----
#**************************************

#Get data (from "Code to use Datetime" section) 

test.xts <- xts(test[, -3:-1], order.by = test$datetime)

d <- dyAxis('y2',label='Q (L/s?)', independentTicks=TRUE, 
            axisLabelWidth = 70,
            axisLabelColor = "#A9A9A9",
            axisLineColor = "#A9A9A9")

dygraph(test.xts) %>%
   dyAxis("x", label = paste("Water Year", wy)) %>%
   dyAxis("y", label = 'mg/L', independentTicks=TRUE) %>%
   d %>% 
   dySeries(name = 'Q', drawPoints = FALSE, fillGraph=T, strokeWidth = 0, 
            color = "#A9A9A9", axis='y2') %>% 
   dySeries(name = 'solute.median', label = "Historical Median", color = "orange", axis='y') %>%
   dySeries(name = 'solute.IQRupper', label = "Hist.Median + IQR", color = "#EE9572", axis='y') %>%
   dySeries(name = 'solute.IQRlower', label = "Hist.Median - IQR", color = "#EE9572", axis='y') %>%
   dySeries(name = 'date', color = "white", axis='y') %>% 
   dyOptions(includeZero = TRUE,
             drawGrid = FALSE,
             strokeWidth = 1,
             fillAlpha = 0.8, 
             drawPoints = TRUE,
             pointSize = 3) %>%  # stackedGraph=TRUE here will make last series appear on top, etc.
   dyLegend(width = 300, showZeroValues = FALSE)


# Testing bar charts with dygraphs ----
#**********************************

dyBarChart <- function(dygraph) {
   dyPlotter(dygraph = dygraph,
             name = "BarChart",
             path = system.file("plotters/barchart.js",
                                package = "dygraphs"))
}

dyBarSeries <- function(dygraph) {
   dyPlotter(dygraph = dygraph,
             name = "BarSeries",
             path = system.file("plotters/barseries.js",
                                package = "dygraphs"))
}

#Create dataset (use test from above)
Ca_plus1 <- test$Ca + 1 
Ca_1 <- test$Ca - 1
test_IQRsubset <- data.frame(test$datetime, Ca_1, test$Ca, Ca_plus1, test$Mg)

test_IQRsubset.xts <- xts(test_IQRsubset[, -1], order.by = test_IQRsubset$test.datetime)

dygraph(test_IQRsubset.xts) %>% 
   dyAxis("x", label = paste("Water Year", wy)) %>%
   dyAxis("y", label = 'mg/L', independentTicks=TRUE) %>%
   
   dyBarSeries()


test <- dataOrig %>% 
   filter(waterYr == 2014) %>%           # Filter data to selected water year
   filter(site == "PLY") %>%                  # Filter data to selected site
   select(one_of("date", "Ca"))

# CODE TO DRAFT GGPLOT GRAPH ----
#*******************************************

# filter data based on dates, sites, solutes, hydrology, fieldcode, etc.
test.filteredData <- dataOrig %>% 
   select(date, site, Ca, Mg, GageHt, hydro.limb, FieldCode) %>% 
   filter(site %in% c("W2", "W4"))

test.filteredDataPrecip <- dataOrig %>% 
   select(date, site, Ca, Mg, GageHt, hydro.limb, FieldCode) %>% 
   filter(site %in% c("RG23"))

# then plot
p <- ggplot(test.filteredData, aes(x=date, y=Ca)) # will have to transform table 
                                                # to be one column with all solutes,
                                                # with color by solute in geom_point

p + geom_point(x=date, y=Ca)

# CODE TO DRAFT DATABASE CODE ----
#*******************************************

#  ----

data.stream <- read.csv("~/Dropbox/WORK/DataVizProjects/HBEF/Data/HistoricData_HBEF_long_term_chemistry_1963-2013/HBEF_stream_chemistry_1963-2013.csv", header=TRUE)
data.precip <- read.csv("~/Dropbox/WORK/DataVizProjects/HBEF/Data/HistoricData_HBEF_long_term_chemistry_1963-2013/HBEF_precipitation_chemistry_1963-2013.csv", header=TRUE)
data.current <- read.csv("~/Dropbox/WORK/DataVizProjects/HBEF/ShinyDashboardApp_HBEF/data/WY2014-7_PLUSwaterYR_upto20180328.csv", header=TRUE)
#*

#library(RMariaDB)
library(RMySQL)
library(DBI)

#con <- dbConnect(RMySQL::MariaDB(), group = "test")

mysql_driver = MySQL()
PASS = readLines('~/Dropbox/WORK/DataVizProjects/HBEF/hbef.config')
con = dbConnect(RMySQL::MySQL(), user='root', password='frenzied_plover_station', host='localhost',
                   dbname='test')

con <- dbConnect(RMariaDB::MariaDB(), group = "my-db", dbname='test',    
                 username='root', password='frenzied_plover_station')


