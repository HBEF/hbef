# Used to:
# -Import all data
# -Create variables that can be accessed by ui.R and server.R

library(RMariaDB)
library(stringr) 

# **********************************************************************
#                      ---- DATA IMPORT & PREP ----
# **********************************************************************

# import MDL/LOQ data
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv")

# data needed for standardizeClasses() funciton to work
defClasses <- read.csv("data/formatted/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("data/formatted/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

# Grabbing Data from MySQL database ----
# USE WHEN LIVE ON SITE
y = RMariaDB::MariaDB()
pass  = readLines('/home/hbef/RMySQL.config')
con = dbConnect(y,
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = 'hbef')
tables = dbListTables(con)

dataInitial <- dbReadTable(con, "initial")
dataCurrent <- dbReadTable(con, "current")
dataHistorical <- dbReadTable(con, "historical")
dataSensor <- dbReadTable(con, "sensor")
dbDisconnect(con)

# # Grabbing Data from Local Source ----
# # USE WHEN TESTING ON LOCAL COMPUTER
# # Import all datasets & make needed changes
# dataInitial <- read.csv("data/formatted/initial.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#    dataInitial$date <- as.Date(dataInitial$date, "%m/%d/%y")
# dataCurrent <- read.csv("data/formatted/current_upto20180328.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA")) #, na.strings=c(""," ","NA")
#    dataCurrent$date <- as.Date(dataCurrent$date, "%m/%d/%y")
# dataSensor <- read.csv("data/formatted/sensor.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#    dataSensor$date <- as.Date(dataSensor$date, "%m/%d/%y")
# dataHistorical <- read.csv("data/formatted/historical.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#    dataHistorical$date <- as.Date(dataHistorical$date, "%m/%d/%y")
#    # Dates before 1969 are incorrect after above transformation
#    # Replace dates before 1970 with date information extracted from 'uniqueID' column
#    pre1968_indices <- grep("_196", dataHistorical$uniqueID, value=FALSE)
#    for (i in pre1968_indices){
#       dateString <- str_extract(dataHistorical$uniqueID[i], "196.....")
#       dataHistorical$date[i] <- as.Date(dateString, "%Y%m%d")
#    }
#    # !!! write a function that alerts user to duplicates uniqueID?
# 
#    # # CODE TO DEAL WITH DUPLICATE ROWS IN HISTORICAL DATA (one-time use)
#    # # remove rows that are exact duplicates
#    # dataHistorical <- distinct(dataHistorical)
#    # # find remaining duplicate uniqueID's and count number of them
#    # duplicatesHist <- dataHistorical %>%
#    #    group_by(uniqueID) %>%
#    #    filter(n()>1) %>%
#    #    count(uniqueID)
#    # # add "Dup" (or "Dup2") to remaining duplicate's 'uniqueID' and 'duplicate' columns
#    # for (i in 1:nrow(duplicatesHist)) {
#    #    indices <- which(duplicatesHist$uniqueID[i] == dataHistorical$uniqueID)
#    #    for (j in 1:length(indices)) {
#    #       if (j > 1) {
#    #          index <- indices[j]
#    #          if (j == 2 ) num <- c() else num <- j-1
#    #          dataHistorical$uniqueID[index] <- paste0(dataHistorical$uniqueID[index] ,"_Dup", num)
#    #          dataHistorical$duplicate[index]  <- paste0("Dup",num)
#    #       }
#    #    }
#    # }
#    # # export
#    # write.csv(dataHistorical, 'dataHistorical_Duplicates.csv')

# ****  END OF DATA IMPORT & PREP ****

# Find maximum date ----
# used in ui.R for "Free-for-all" panel
maxDate_initial <- max(dataInitial$date, na.rm=TRUE)
maxDate_current <- max(dataCurrent$date, na.rm=TRUE)
maxDate_historical <- max(dataHistorical$date, na.rm=TRUE)
maxDate_sensor <- max(dataSensor$date, na.rm=TRUE)

maxDate <- maxDate_historical # default value if dataCurrent or dataSensor are empty
if (maxDate_sensor > maxDate_current) maxDate <- maxDate_sensor
if (maxDate_sensor < maxDate_current) maxDate <- maxDate_current

