# Used to:
# -Import all data
# -Create variables that can be accessed by ui.R and server.R

library(dplyr)
library(RMariaDB)
library(stringr) 

message("hello, I'm in global.R")

# **********************************************************************
#                      ---- LISTS ----
# **********************************************************************

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

codes999.9 <- c("timeEST", "temp", "ANC960", "ANCMet", 
                "ionError", "ionBalance")
codes123 <- c("pH", "pHmetrohm", "spCond", "au254", "au275",
              "au295", "au350", "au400", "Ca", "Mg", 
              "K", "Na", "TMAl", "OMAl", "Al_ICP", "NH4", 
              "SO4", "NO3", "Cl", "PO4", "DOC", "TDN", "DIC",
              "DON", "SiO2", "Mn", "Fe", "F")


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
                      "ML70")

#Precipitation sites
# If you update this list, also update conditional panel below
sites_precip <- list("RG11", "RG23", "STA/22", "N", "S") 

# wateryears ----> see list after data import

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

# Functions ----
# ***********************************

## Function to re-classify data class (e.g. numeric, character, etc.) of each variable (column) in a data 
## frame. Uses defClasses & defClassesSample data to match data column with its intended
## data class.
standardizeClasses <- function(d) {
   # d:              data.frame to be checked for columns with only NA's
   # ColClasses :    vector of desired class types for the data.frame
   message(paste("In standardizeClasses for", deparse(substitute(d))))
   r <- nrow(d)
   c <- ncol(d)
   for (i in 1:c) {
      # 1. Insert an additional row with a sample value for each column
         ## Find index in defClassesSample that corresponds to column in d, save that index
         current_col_ofData <- colnames(d[i])
         ind_col <- which(current_col_ofData == colnames(defClassesSample), arr.ind = TRUE)
         ## Add corresponding sample value to last row of d
         d[r+1,i] <- defClassesSample[1,ind_col]
      # 2. Define class of each column according to what you specified in defClasses
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

# **********************************************************************
#                      ---- DATA IMPORT & PREP ----
# **********************************************************************

# import MDL/LOQ data
dataLimits <- read.csv("data/Limits_MDL_LOQ.csv")

# data needed for standardizeClasses() function to work
defClasses <- read.csv("data/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("data/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

# Grabbing Data from MySQL database ----
# USE WHEN LIVE ON REMOTE SITE
#**********************************************
y = RMariaDB::MariaDB()
pass  = readLines('/home/hbef/RMySQL.config')
con = dbConnect(y,
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = 'hbef')
tables = dbListTables(con)

## Code for one-time use: to load data into mysql
#dataInitial <- read.csv("data/initial_clean.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#  dataInitial$date <- as.Date(dataInitial$date, "%m/%d/%y")
#  dataInitial <- standardizeClasses(dataInitial)
# dataChemistry <- read.csv("data/chemistry_clean.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#  dataChemistry <- standardizeClasses(dataChemistry)
#dbWriteTable(con, "initial", dataInitial, append=TRUE, row.names=FALSE)
#dbWriteTable(con, "chemistry", dataChemistry, append=TRUE, row.names=FALSE)

# Get data from mysql
dataInitial <- dbReadTable(con, "initial")
message("dataInitial class of date and min waterYr:")
message(class(dataInitial$date))
message(min(dataInitial$waterYr, na.rm=TRUE))
dataChemistry <- dbReadTable(con, "chemistry")
dataHistorical <- dbReadTable(con, "historical")
dataSensor <- dbReadTable(con, "sensor")
dbDisconnect(con)

# Format data as needed
dataInitial <- standardizeClasses(dataInitial)
  # necessary to prevent problems when downloading csv files
  dataInitial$notes <- gsub(",", ";", dataInitial$notes)
  message("dataInitial class of date and min waterYr:")
  message(class(dataInitial$date))
  message(min(dataInitial$waterYr, na.rm=TRUE))
dataChemistry <- standardizeClasses(dataChemistry)
dataHistorical <- standardizeClasses(dataHistorical)

# Create dataCurrrent and dataAll when from MySQL----
#**********************************************
# Create dataCurrent by binding dataInitial with dataChemistry
# if (nrow(dataChemistry) > 1) {
# !!! Need to check what happens when dataChemistry is empty!
# dataInitial <- select(dataInitial, -waterYr)
dataChemistry_minus_waterYr_refNo <- select(dataChemistry, -waterYr, -refNo)
dataCurrent <- full_join(dataInitial, dataChemistry_minus_waterYr_refNo, by = "uniqueID")
dataCurrent <- standardizeClasses(dataCurrent)
# } else {
#    dataCurrent <- dataInitial
# }
dataAll <- bind_rows(dataHistorical, dataCurrent)
dataAll <- standardizeClasses(dataAll)

# # FOR TESTING on remote server
# message("dataInitial from mysql: names, rows, col's")
# message(names(dataInitial))
# message(nrow(dataInitial))
# message(ncol(dataInitial))
# message("dataChemistry from mysql: names, rows, col's")
# message(names(dataChemistry))
# message(nrow(dataChemistry))
# message(ncol(dataChemistry))
# message("dataCurrent from mysql: names, rows, col's")
# message(names(dataCurrent))
# message(nrow(dataCurrent))
# message(ncol(dataCurrent))
# # Writing files to .csv files for checking
# message(getwd())
# write.csv(dataInitial, file="/home/hbef/shiny/temp_csv/dataInitial.csv", append=FALSE)
# write.csv(dataChemistry, file="data/tests/dataChemistry.csv")
# write.csv(dataHistorical, file="data/tests/dataHistorical.csv")
# write.csv(dataSensor, file="data/tests/Sensor.csv")
# write.csv(dataCurrent, file="data/tests/dataCurrent.csv")

# # Grabbing Data from Local Source ----
# # USE WHEN TESTING ON LOCAL COMPUTER
# #**********************************************
# # Import all datasets & make needed changes
# dataInitial <- read.csv("data/initial_withWY2013_minusPLY.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#    dataInitial$date <- as.Date(dataInitial$date, "%m/%d/%y")
#    # substitute all commas with ";" in notess (otherwise sentences get separated in .csv file)
#    dataInitial$notes <- gsub(",", ";",dataInitial$notes)
# dataChemistry <- read.csv("data/chemistry_withWY2013_minusPLY.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
# dataSensor <- read.csv("data/sensor.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#    dataSensor$date <- as.Date(dataSensor$date, "%m/%d/%y")
# dataHistorical <- read.csv("data/historical.csv", stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
#    dataHistorical$date <- as.Date(dataHistorical$date, "%m/%d/%y")
#    # Dates before 1969 are incorrect after above transformation
#    # Replace dates before 1970 with date information extracted from 'uniqueID' column
#    pre1968_indices <- grep("_196", dataHistorical$uniqueID, value=FALSE)
#    for (i in pre1968_indices){
#       dateString <- str_extract(dataHistorical$uniqueID[i], "196.....")
#       dataHistorical$date[i] <- as.Date(dateString, "%Y%m%d")
#    }
# 
# # Format data as needed
# dataInitial <- standardizeClasses(dataInitial)
#    # necessary to prevent problems when downloading csv files
#    dataInitial$notes <- gsub(",", ";", dataInitial$notes)
# dataChemistry <- standardizeClasses(dataChemistry)
#    # necessary to prevent problems when downloading csv files
#    dataChemistry$sampleType <- gsub(",", ";", dataChemistry$sampleType)
# dataHistorical <- standardizeClasses(dataHistorical)
# 
# #Standardize datasets
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
# 
# # Create dataCurrrent & dataAll when on Local Source ----
# #**********************************************
# # Create dataCurrent by binding dataInitial with dataChemistry
# # if (nrow(dataChemistry) > 1) {
#    # !!! Need to check what happens when dataChemistry is empty!
#    # dataInitial <- select(dataInitial, -waterYr)
#    dataChemistry_minus_waterYr <- select(dataChemistry, -waterYr)
#    dataCurrent <- full_join(dataInitial, dataChemistry_minus_waterYr, by = "uniqueID")
#    dataCurrent <- standardizeClasses(dataCurrent)
#    # } else {
# #    dataCurrent <- dataInitial
# # }
#    dataAll <- bind_rows(dataHistorical, dataCurrent)
#    dataAll <- standardizeClasses(dataAll)
   

# ****  END OF DATA IMPORT & PREP ****


# Create water years *list* ----
# used in ui.R and server.R for Panels 1-3 (QA/QC graphs)
wy <- levels(as.factor(dataCurrent$waterYr))
wy1 <- c()
for (i in 1:length(wy)) {
   wy1 <- c(wy1, wy[i])
}
#wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
wateryears <- as.list(wy1)

      
# Find maximum date ----
# used in ui.R for Panel 4 (QA/QC "Free-for-all" graph)
maxDate_initial <- max(dataInitial$date, na.rm=TRUE)
maxDate_current <- max(dataCurrent$date, na.rm=TRUE)
maxDate_historical <- max(dataHistorical$date, na.rm=TRUE)
maxDate_sensor <- max(dataSensor$date, na.rm=TRUE)

maxDate <- maxDate_historical # default value if dataCurrent or dataSensor are empty
if (maxDate_sensor > maxDate_current) maxDate <- maxDate_sensor
if (maxDate_sensor < maxDate_current) maxDate <- maxDate_current

