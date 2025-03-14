
# Used to:
# -Import all data from hbef (mysql) database upon startup
# -Create variables and functions that can be accessed by ui.R and server.R

#options(shiny.reactlog = TRUE) # Enabling shiny reactivity log

#NOTES:
#tables sensor and sensor3 are not in use.
#sensor2 contains qa/qc'd discharge data
#sensor4 contains raw water qual and chemistry data
#sensorQraw constains raw discharge data
#sensor could contain qa/qc'd water qual and chem data
#grab NH4 gets converted to NH4-N ON READ, so it's still just NH4 in the database.
#   same with NO3
#sensor NH4 and NO3 are already in N equivalents

dbname = 'hbef'
# setwd('~/git/hbef/shiny/restricted_QAQC/') #only for testing

library(dplyr)
library(RMariaDB)
#library(RMySQL)
#library(reactlog)
library(stringr)
library(DT) #shiny's DataTable functions broke. loading them from here fixed it.
library(shinyjs)
library(tidyr)
library(xts)
# library(readr)

#options(shiny.fullstacktrace=TRUE)

message("hello, I'm in global.R")

source('helpers.R')
pass=readLines('../../RMySQL.config')
note_dest_email = readLines('more_config/compiled_notes_target_email.txt', n = 1)
note_dest_pwd = readLines('more_config/compiled_notes_target_email.txt', n = 2)[2]
#gm_pass=readLines('config.txt')[1]
#gm_addr=readLines('config.txt')[2]

jsCode <- "
$(document).on('shiny:busy', function() {
  $('#loading-icon').show();
});

$(document).on('shiny:idle', function() {
  $('#loading-icon').hide();
});
"

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
                        "Ammonium-N (NH4-N)" = "NH4_N",
                        "Manganese (Mn)" = "Mn",
                        "Iron (Fe)" = "Fe")

# If you add to this list, must update colors_anions list as well
solutes_anions <- list("TOTAL Anion Charge" = "anionCharge",
                       "Sulfate (SO4)" = "SO4",
                       "Nitrate-N (NO3-N)" = "NO3_N",
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
                      "Cumul. Heating Degree Days (balance point 4C)" = "swdd",
                      "Ion Balance" = "ionBalance",
                      "Chlorophyll-a (moss)" = "chla_M",
                      "Chlorophyll-a (tile)" = "chla_T",
                      "Emergence (caddisfly)" = "caddisfly",
                      "Emergence (dipteran)" = "dipteran",
                      "Emergence (mayfly)" = "mayfly",
                      "Emergence (stonefly)" = "stonefly",
                      "Emergence (other)" = "other")

all_factors <- c(unlist(solutes_cations, use.names = FALSE),
                 unlist(solutes_anions, use.names = FALSE),
                 unlist(solutes_other, use.names = FALSE))

codes999.9 <- c("timeEST", "temp", "ANC960", "ANCMet",
                "ionError", "ionBalance","swdd")
codes123 <- c("pH", "pHmetrohm", "spCond", "au254", "au275",
              "au295", "au350", "au400", "Ca", "Mg",
              "K", "Na", "TMAl", "OMAl", "Al_ICP", "NH4",
              "SO4", "NO3", "Cl", "PO4", "DOC", "TDN", "DIC",
              "DON", "SiO2", "Mn", "Fe", "F")

emergence = c('mayfly', 'stonefly', 'caddisfly', 'dipteran', 'other')

# Lists of Sites
#***************
sites_streams <- list("Mainstem" = "HBK",
                      "Watershed 1" = "W1",
                      "Watershed 2" = "W2",
                      "Watershed 3" = "W3",
                      "Watershed 4" = "W4",
                      "Watershed 5" = "W5",
                      "Watershed 6" = "W6",
                      "Watershed 7" = "W7",
                      "Watershed 8" = "W8",
                      "Watershed 9" = "W9",
                      "ML70",
                      "SW",
                      "SP")

#Precipitation sites
# If you update this list, also update conditional panel below
sites_precip <- list("RG1", "RG11", "RG23", "RG22", "N", "S", "RG19",
                     "W7-Precip")

# wateryears ----> see list after data import

# list of solutes that have units other than mg/L for data items
other_units <- c("pH",
                 "DIC",
                 "ANC960",
                 "ANCMet",
                 "cationCharge",
                 "anionCharge",
                 "spCond",
                 "theoryCond",
                 "temp",
                 "swdd",
                 "ionBalance",
                 "chla_M", "chla_T", "chla_MT", "chla_WM",
                 'caddisfly', 'mayfly', 'dipteran', 'stonefly', 'other')

# Functions ----
# ***********************************

## Function to re-classify data class (e.g. numeric, character, etc.) of each variable (column) in a data
## frame. Uses defClasses & defClassesSample data to match data column with its intended
## data class.
standardizeClasses <- function(d) {
   # d:              data.frame to be checked for columns with only NA's
   # ColClasses :    vector of desired class types for the data.frame
   # message(paste("In standardizeClasses for", deparse(substitute(d))))
   r <- nrow(d)
   cc <- ncol(d)
   d$timeEST <- as.character(d$timeEST)
   d$DIC <- as.numeric(d$DIC)
   # is_na_chlaM <- sum(is.na(d$chla_M))
   # is_not_na_ChlaM <-sum(!is.na(d$chla_M))
   # d = rename_all(d, recode, NH4='NH4_N', NO3='NO3_N')
   for (i in 1:cc) {
      # 1. Insert an additional row with a sample value for each column
         ## Find index in defClassesSample that corresponds to column in d, save that index
         current_col_ofData <- colnames(d[i])
         ind_col <- which(current_col_ofData == colnames(defClassesSample), arr.ind = TRUE)
         # Add corresponding sample value to last row of d
         #browser()
         tryCatch({
           d[r+1, i] <- defClassesSample[1, ind_col]
         }, error = function(e) {
           print(paste("error column:", current_col_ofData))
         })
   
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
#y = RMySQL::MySQL()
y = RMariaDB::MariaDB()

con = dbConnect(y,
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = dbname)
tables = dbListTables(con)

# Applying the SWDD calculation to the dataset
dataCurrent <- dbReadTable(con, "current") %>%
  mutate(
    NO3_N = NO3_to_NO3N(NO3),
    NH4_N = NH4_to_NH4N(NH4)
  ) %>%
  select(-NO3, -NH4) %>%
  filter(date >= as.Date('2013-06-01')) %>%
  arrange(site, date, timeEST) %>%
  mutate(timeEST = as.character(timeEST))

dataHistorical <- dbReadTable(con, "historical") %>%
  filter(!(site == 'W6' & date == as.Date('2007-08-06'))) %>%
  mutate(
    NO3_N = NO3_to_NO3N(NO3),
    NH4_N = NH4_to_NH4N(NH4)
  ) %>%
  select(-NO3, -NH4) %>%
  arrange(site, date, timeEST) %>%
  mutate(timeEST = as.character(timeEST))
    
dataSensor <- dbReadTable(con, "sensor2")
sensorvars = dbListFields(con, "sensor4")
sensorvars = sub('S4__', '', sensorvars)
sensorvars[sensorvars == 'Nitrate_mg'] = 'NO3_N_mg'
sensorvars = sensorvars[-which(sensorvars %in% c('datetime', 'id', 'watershedID'))]
sensorvars = c(sensorvars, 'Light_lux')
dataSensor$watershedID = paste0('W', as.character(dataSensor$watershedID))
# dataArchive = tibble(dbReadTable(con, 'archive'))

dataCurrent <- standardizeClasses(dataCurrent)
  # necessary to prevent problems when downloading csv files
  dataCurrent$notes <- gsub(",", ";", dataCurrent$notes)
dataHistorical <- standardizeClasses(dataHistorical)

dataAll = bind_rows(dataCurrent, select(dataHistorical, -canonical))

## build stream water degree days

s4temp <- dbGetQuery(con, 'select datetime, watershedID, S4__TempC from sensor4;')
dbDisconnect(con)

dAtemp <- dataAll %>% 
    select(date, site, temp) %>% 
    filter(grepl('^W[0-9]+$|HBK', site),
           ! is.na(temp))

all_temp <- s4temp %>% 
    mutate(date = as.Date(datetime),
           watershedID = if_else(watershedID == 0, 'HBK', paste0('W', watershedID))) %>% 
    select(date, site = watershedID, temp = S4__TempC) %>% 
    bind_rows(dAtemp)

swdd <- calculate_SWDD(all_temp)

dataAll <- left_join(dataAll, swdd, by = c('site', 'date'))

# ****  END OF DATA IMPORT & PREP ****


# Create water years *list* ----

# Water years list for dataAll
# used in ui.R and server.R for Panels 1-3 (QA/QC graphs)
wy <- levels(as.factor(dataAll$waterYr))
wy1 <- c()
for (i in 1:length(wy)) {
   wy1 <- c(wy1, wy[i])
}
#wy1 <- as.character(sort(as.numeric(wy1), decreasing=TRUE)) # sort so that recent years are first
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
maxDate_historical <- max(dataHistorical$date, na.rm=TRUE)
maxDate_sensor <- max(as.Date(dataSensor$date), na.rm=TRUE)
# maxDate <- maxDate_historical # default value if dataCurrent or dataSensor are empty
# if (maxDate_sensor > maxDate_current) maxDate <- maxDate_sensor
# if (maxDate_sensor < maxDate_current) maxDate <- maxDate_current
maxDate = Sys.Date()

#data for the daterange picker in field note download
fnfiles = list.files('field_notes')
field_note_dates = unique(na.omit(as.Date(substr(fnfiles, 1, 8),
    format='%Y%m%d')))
field_note_daterange = range(field_note_dates, na.rm=TRUE)
field_note_daterange_filled = seq(field_note_daterange[1],
    field_note_daterange[2], by='day')
no_note_days = as.Date(setdiff(field_note_daterange_filled, field_note_dates),
    origin='1970-01-01')
# message(paste(field_note_daterange, collapse = ', '))

#data for the daterange picker in field-and-lab note download
dir.create('field_and_lab_note_collections', showWarnings = FALSE)
# dir.create('field_and_lab_note_collections/part1', showWarnings = FALSE)
# dir.create('field_and_lab_note_collections/complete', showWarnings = FALSE)

fnfiles = list.files('field_and_lab_note_collections')
# fnfiles = list.files('field_and_lab_note_collections/complete')
field_note_dates2 = unique(na.omit(as.Date(substr(fnfiles, 1, 8),
    format='%Y%m%d')))
field_note_daterange2 <- range(field_note_dates2, na.rm=TRUE)
if(any(is.infinite(field_note_daterange2))) field_note_daterange2 <- rep(Sys.Date(), 2)
field_note_daterange_filled2 = seq(field_note_daterange2[1],
    field_note_daterange2[2], by='day')
no_note_days2 = as.Date(setdiff(field_note_daterange_filled2, field_note_dates2),
    origin='1970-01-01')
no_note_days2 <- c(field_note_daterange2[1] - 1, no_note_days2)

# file.create('../logs/email_jeff.log', showWarnings = FALSE)

# merge archive and sample data for new archive perusal tab ####

#find out what "refNo" is. a lot of refNos are identical to barcode numbers, but
#there appears to be no relationship between matches.
# zz = select(dataAll, refNo, site, date, timeEST, fieldCode, notes, archived, uniqueID, waterYr, datetime)
# qq = dataArchive %>%
#     select(-id, -bin, -weight_g, -bottle_type) %>%
#     left_join(zz, by=c('barcode' = 'refNo'))
# range(tibble(dataAll[!is.na(dataAll$archived) & dataAll$archived == 'TRUE',])$refNo)

