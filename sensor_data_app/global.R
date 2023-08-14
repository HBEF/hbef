
# Used to:
# -Import all data from hbef (mysql) database upon startup
# -Create variables and functions that can be accessed by ui.R and server.R

# options(shiny.reactlog = TRUE) # Enabling shiny reactivity log

Sys.setenv(TZ='America/New_York')
options(show.error.locations=TRUE) # show error locations

library(dygraphs)
library(jsonlite)
library(lubridate)
library(RMariaDB)
library(RMySQL)
library(rhandsontable)
library(shiny)
library(stringr)
library(tidyr)
library(xts)
library(glue)
library(dplyr)
library(DT)

setwd('../sensor_data_app')


# dbname = 'hbef20200415' #for local testing
dbname = 'hbef' #for reals

source('../restricted_QAQC/helpers.R')
pass=readLines('../../RMySQL.config')


#### Prep options ####

# Cations
solutes_cations <- list("TOTAL Cation Charge" = "cationCharge",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "TM Aluminum (Al)" = "TMAl",
                        "OM Aluminum (Al)" = "OMAl",
                        "Aluminum (Al) ICP" = "Al_ICP",
                        "Ammonium-N (NH4-N)" = "NH4_N",
                        "Manganese (Mn)" = "Mn",
                        "Iron (Fe)" = "Fe")
# Anions
solutes_anions <- list("TOTAL Anion Charge" = "anionCharge",
                       "Sulfate (SO4)" = "SO4",
                       "Nitrate-N (NO3-N)" = "NO3_N",
                       "Chloride (Cl)" = "Cl",
                       "Phosphate (PO4)" = "PO4",
                       "Fluorine (F)" = "F")

# Other
solutes_other <- list("pH (3 Star)" = "pH",
                      "pH (Metrohm)"="pHmetrohm",
                      "Specific Conductivity" = "spCond",
                      "Water Temperature" = "temp")

#map grap to sensor varibles 
sensor_grab_map <- c('TempC' = 'temp',
                     'SpConductivity' = 'spCond')

#sensor unit map 
sensor_unit_map <- c('NO3-N' = 'Sensor value',
                     'Temperature' = 'C',
                     'Conductivity' = 'uS/cm',
                     'Specific Conductivity' = 'uS/cm',
                     'Depth' = 'm',
                     'Dissolved Oxygen %' = '',
                     'Dissolved Oxygen mg/l' = '',
                     'Turbidity FNU' = '',
                     'Turbidity Raw' = '',
                     'FDOM RFU' = '',
                     'FDOM QSU' = '')

sensor_name_map <- c('NO3-N' = 'NO3_N_mg',
                     'Temperature' = 'TempC',
                     'Conductivity' = 'Conductivity',
                     'Specific Conductivity' = 'SpConductivity',
                     'Depth' = 'DepthMeter',
                     'Dissolved Oxygen %' = 'ODOPerCent',
                     'Dissolved Oxygen mg/l' = 'ODOMGL',
                     'Turbidity FNU' = 'TurbidityFNU',
                     'Turbidity Raw' = 'TurbidityRaw',
                     'FDOM RFU' = 'FDOMRFU',
                     'FDOM QSU' = 'FDOMQSU')

codes999.9 <- c("timeEST", "temp", "ANC960", "ANCMet",
                "ionError", "ionBalance")

codes123 <- c("pH", "pHmetrohm", "spCond", "au254", "au275",
              "au295", "au350", "au400", "Ca", "Mg",
              "K", "Na", "TMAl", "OMAl", "Al_ICP", "NH4",
              "SO4", "NO3", "Cl", "PO4", "DOC", "TDN", "DIC",
              "DON", "SiO2", "Mn", "Fe", "F")

# Sites
sites_streams <- list("Watershed 3" = "W3",
                      "Watershed 6" = "W6",
                      "Watershed 9" = "W9")

# Solutes with non mg/l untis 
other_units <- c("pH",
                 "DIC",
                 "ANC960",
                 "ANCMet",
                 "cationCharge",
                 "anionCharge",
                 "spCond",
                 "theoryCond",
                 "temp",
                 "ionBalance")

grab_choices <- c(solutes_cations, solutes_anions, solutes_other)

#### Functions ####

# Function to re-classify data class (e.g. numeric, character, etc.) of each 
# variable (column) in a data frame. Uses defClasses & defClassesSample data to 
# match data column with its intended data class.

standardizeClasses <- function(d) {
  # d:              data.frame to be checked for columns with only NA's
  # ColClasses :    vector of desired class types for the data.frame
  message(paste("In standardizeClasses for", deparse(substitute(d))))
  r <- nrow(d)
  c <- ncol(d)

  # d = rename_all(d, recode, NH4='NH4_N', NO3='NO3_N')

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

#round selected dates to the first of each month
round_date_first <- function(dates){
  
    dates_split <- str_split_fixed(dates, '-', n = Inf)[,1:2]
  
    dates_fin <- paste0(c(paste(dates_split[1,], collapse = '-'), 
                          paste(dates_split[2,], collapse = '-')), 
                        '-01')
    dates_fin <- ymd(dates_fin)
  
    return(dates_fin)
  }

#### Data load and prep ####
# import MDL/LOQ data
dataLimits <- read.csv("../restricted_QAQC/data/Limits_MDL_LOQ.csv")

# data needed for standardizeClasses() function to work
defClasses <- read.csv("../restricted_QAQC/data/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("../restricted_QAQC/data/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

# Grabbing Data from MySQL database
# USE WHEN LIVE ON REMOTE SITE
y = RMariaDB::MariaDB()

con = dbConnect(y,
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = dbname)
tables = dbListTables(con)

# Get data from mysql
# Current data (from 2013+)
dataCurrent <- dbReadTable(con, "current") %>%
  mutate(
    NO3_N=NO3_to_NO3N(NO3),
    NH4_N=NH4_to_NH4N(NH4),
    timeEST = as.character(timeEST)) %>%
  select(-NO3, -NH4) %>%
  filter(date >= as.Date('2013-06-01')) %>%
  arrange(site, date, timeEST)

dataCurrent <- standardizeClasses(dataCurrent)
# necessary to prevent problems when downloading csv files
dataCurrent$notes <- gsub(",", ";", dataCurrent$notes)

#### Creat water years ####

# Water years list for dataAll
# used in ui.R and server.R for Panels 1-3 (QA/QC graphs)
wy <- levels(as.factor(dataCurrent$waterYr))
wy1 <- c()
for (i in 1:length(wy)) {
  wy1 <- c(wy1, wy[i])
}

wy1 <- wy1[as.numeric(wy1) >= 2018]
wateryears <- as.list(wy1)

# Water years list for dataCurrent
# used for Panels 5 (DataEdits)
wy_current <- levels(as.factor(dataCurrent$waterYr))
wy1_current <- c()
for (i in 1:length(wy_current)) {
  wy1_current <- c(wy1_current, wy_current[i])
}
#wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
wateryears_current <- as.list(wy1_current)


# Find maximum date ----
# used in ui.R for Panel 4 (QA/QC "Free-for-all" graph)

maxDate_current <- max(dataCurrent$date, na.rm=TRUE)
maxDate = Sys.Date()


# #### Helper functions ####

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
      tryCatch({dataSet[ind_col][dataSet[ind_col] == -1] <- NA},
               error = function(e) return())
      tryCatch({dataSet[ind_col][dataSet[ind_col] == -2] <- NA},
               error = function(e) return())
      tryCatch({dataSet[ind_col][dataSet[ind_col] == -3] <- NA},
               error = function(e) return())
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

# #### Theme  ####

# Graph theme
# my_theme <- theme_fivethirtyeight() +
#   theme(rect = element_rect(fill = NA),
#         panel.grid.major = element_line(colour = "#dddddd"),
#         text = element_text(family = "Arial", size = 14),
#         legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal",
#         legend.box.just = "left", legend.title = element_blank(),
#         #legend.key.size = unit(2.5, "native"),
#         strip.text = element_text(hjust = 1, size = 20, face = "bold"),
#         axis.title= element_text(NULL), axis.title.x= element_blank(),
#         axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
