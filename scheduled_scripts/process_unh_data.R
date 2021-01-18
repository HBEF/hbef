library(tidyverse)
library(lubridate)
library(RMariaDB)
library(logging)
library(glue)

# config ####
import_static_q_data = FALSE #also commented this section to be safe

# setup ####

#NOTE: this script sources another (process_S.CAN_data.R) at bottom, using
#an absolute path.

# setwd('~/git/hbef/shiny/restricted_QAQC/data/unh_sensor_data')
setwd('/home/mike/shiny/restricted_QAQC/data/unh_sensor_data')

logging::basicConfig()
logging::addHandler(logging::writeToFile, logger='hbef',
    file='../../../logs/hbef_flowdata_retrieval.log')

# pass = readLines('~/git/hbef/RMySQL.config')
pass  = readLines('/home/mike/RMySQL.config')

driver = MariaDB()
con = dbConnect(driver, user='root', password=pass, host='localhost',
    dbname='hbef')
    # dbname='hbef20200415')

# read and process w9 wqual data ####

header = readr::read_csv('CR1000_HBF_W9_WQual.dat',
    skip=1, col_names=FALSE, n_max=1)
wqual9 = readr::read_csv('CR1000_HBF_W9_WQual.dat', skip=4, col_names=FALSE)
colnames(wqual9) = header
w9_colnames = c('Nitrate_mg', 'TempC', 'Conductivity',
                'SpConductivity', 'pH', 'DepthMeter', 'ODOPerCent', 'ODOMGL',
                'TurbidityFNU', 'TurbidityRaw', 'FDOMRFU', 'FDOMQSU',
                'LowEOSCO2_ppm_avg', 'HighEOSCO2_ppm_avg', 'EOSTempC',
                'Chl_RFU', 'BGA_PC_RFU', 'BGA_PE_RFU', 'AqCO2_ppm_avg',
                'AtmCO2_ppm_avg')

wqual9 = wqual9 %>%
    select(datetime=TIMESTAMP, one_of(w9_colnames)) %>%
    mutate(watershedID=9, id=1:nrow(wqual9))

for(cn in w9_colnames){
    if(! cn %in% colnames(wqual9)){
        wqual9[[cn]] = NA
    }
}

wqual9[is.na(wqual9)] = NA

#make config vector for new db table
colnames(wqual9) = paste('S4', colnames(wqual9), sep='__')
wqual9 = rename(wqual9, datetime='S4__datetime',
    id='S4__id', watershedID='S4__watershedID') %>%
    select(datetime, everything()) %>%
    relocate(watershedID, id, .after = last_col())

# tables = RMariaDB::dbListTables(con)
# if(! 'sensor4' %in% tables){

#create sensor4 table
RMariaDB::dbRemoveTable(con, 'sensor4')

fieldnames = colnames(wqual9)
fieldtypes = rep('FLOAT', length(fieldnames))
fieldtypes[fieldnames == 'datetime'] = 'DATETIME'
fieldtypes[fieldnames == 'watershedID'] = 'INT(3)'
fieldtypes[fieldnames == 'id'] = 'INT(11) primary key auto_increment'
names(fieldtypes) = fieldnames

dbCreateTable(con, 'sensor4', fieldtypes)
# }

dbWriteTable(con, 'sensor4', wqual9, append=TRUE)

rm(wqual9)
gc()

# read and process w6 wqual data ####

header = readr::read_csv('CR1000_HBF_W6_WQual.dat',
    skip=1, col_names=FALSE, n_max=1)
wqual6 = readr::read_csv('CR1000_HBF_W6_WQual.dat', skip=4, col_names=FALSE)
colnames(wqual6) = header

w6_colnames = c('TempC', 'Conductivity', 'SpConductivity', 'pH', 'DepthMeter',
                'ODOPerCent', 'ODOMGL', 'TurbidityFNU', 'FDOMRFU', 'FDOMQSU',
                'Nitrate_mg', 'Chl_RFU', 'BGA_PC_RFU', 'BGA_PE_RFU',
                'AqCO2_ppm_avg', 'AtmCO2_ppm_avg', 'TurbidityRaw',
                'LowEOSCO2_ppm_avg', 'HighEOSCO2_ppm_avg', 'EOSTempC')

wqual6 = wqual6 %>%
    select(datetime=TIMESTAMP, one_of(w6_colnames)) %>%
    mutate(watershedID=6, datetime=with_tz(datetime, 'EST')) %>%
    filter(datetime > as.POSIXct('2020-04-23 14:00:00')) #some test records

for(cn in w6_colnames){
    if(! cn %in% colnames(wqual6)){
        wqual6[[cn]] = NA
    }
}

wqual6[is.na(wqual6)] = NA

#make config vector for new db table
colnames(wqual6) = paste('S4', colnames(wqual6), sep='__')
wqual6 = rename(wqual6, datetime='S4__datetime', watershedID='S4__watershedID')

dbWriteTable(con, 'sensor4', wqual6, append=TRUE)

rm(wqual6)
gc()

# read and process w3 wqual data ####

header = readr::read_csv('CR1000_HBF_WQual.dat',
    skip=1, col_names=FALSE, n_max=1)
wqual3 = readr::read_csv('CR1000_HBF_WQual.dat', skip=4, col_names=FALSE)
colnames(wqual3) = header

w3_colnames = c('Nitrate_mg', 'TempC', 'Conductivity', 'SpConductivity',
                'pH', 'DepthMeter', 'ODOPerCent', 'ODOMGL', 'TurbidityFNU',
                'TurbidityRaw', 'FDOMRFU', 'FDOMQSU', 'Chl_RFU', 'BGA_PC_RFU',
                'BGA_PE_RFU', 'AqCO2_ppm_avg', 'AtmCO2_ppm_avg')

wqual3 = wqual3 %>%
    select(datetime=TIMESTAMP, one_of(w3_colnames)) %>%
    mutate(watershedID=3)

for(cn in w3_colnames){
    if(! cn %in% colnames(wqual3)){
        wqual3[[cn]] = NA
    }
}

wqual3[is.na(wqual3)] = NA

#make config vector for new db table
colnames(wqual3) = paste('S4', colnames(wqual3), sep='__')
wqual3 = rename(wqual3, datetime='S4__datetime', watershedID='S4__watershedID')

dbWriteTable(con, 'sensor4', wqual3, append=TRUE)

rm(wqual3)
gc()

# read and process static raw files of flow data for w1-9 (from Nina) ####


#(this assumes there's already a table called 'sensorQraw' that is
#appropriately configured.)
#if(import_static_q_data){
#
#    # tables = RMariaDB::dbListTables(con)
#
#    # if(! 'sensorQraw' %in% tables){
#    #     dbCreateTable(con, 'sensorQraw', fieldtypes)
#    # }
#
#    weirfiles = list.files('static_raw_weirdata/', 'Weir*', full.names=TRUE)
#
#    for(w in weirfiles){
#
#        flowd = readr::read_csv(w,
#                col_types=readr::cols_only(TIMESTAMP='T', Q='d')) %>%
#            rename(datetime=TIMESTAMP, Q_Ls=Q)
#
#        weirno = stringr::str_match(w, '^static_raw_weirdata//Weir_(.)\\.csv$')[,2]
#        flowd$watershedID = weirno
#
#        dbWriteTable(con, 'sensorQraw', flowd, append=TRUE)
#    }
#}

# read and process updating flow data for w1-9 ####
#(this assumes there's already a table called 'sensorQraw'
#that is appropriately configured and populated with static data from Nina
#(unless qa/qc'd data have surpassed those static files, which extend to 20200316)

weirfiles = list.files('.', '^weir[0-9]_Ws_[0-9]b.dat$')

if(length(weirfiles) != 9){
    logging::logerror('weir filenames have changed', logger='hbef.module')
    stop()
}

for(w in weirfiles){

    err = FALSE
    tryCatch({
        header = readr::read_csv(w, skip=1, col_names=FALSE, n_max=1)
        flowd = readr::read_csv(w, skip=4, col_names=FALSE)
        colnames(flowd) = header

        id = stringr::str_match(w, '^weir(.)_Ws_.b\\.dat$')[,2]

        flowd = flowd %>%
            select(datetime=TIMESTAMP, Q_Ls=Q) %>%
            mutate(watershedID=id)
    }, error=function(e){
        logging::logerror('missing Q in weir file', logger='hbef.module')
        err <<- TRUE
    })

    if(err){
        dbDisconnect(con)
        stop()
    }

    # held_data = dbReadTable(con, 'sensorQraw')
    held_datemax = dbGetQuery(con,
        glue('select max(datetime) from sensorQraw where watershedID={id};',
            id=id))[[1]] %>%
        lubridate::with_tz('EST')

    flowd = filter(flowd, datetime > held_datemax)

    dbWriteTable(con, 'sensorQraw', flowd, append=TRUE)
}

dbDisconnect(con)

source('/home/mike/shiny/scheduled_scripts/process_S.CAN_data.R')

# read and process mainstem temperature data ####

header = readr::read_csv('HB%20Mainstem_HB_mainstem.dat',
                         skip=1, col_names=FALSE, n_max=1)
mainstem = readr::read_csv('HB%20Mainstem_HB_mainstem.dat', skip=4,
                           col_names=FALSE)
colnames(mainstem) = header

mainstem = mainstem %>%
    select(datetime=TIMESTAMP, TempC=sensor2tempC) %>%
    mutate(watershedID=0, datetime=with_tz(datetime, 'EST'))

mainstem[is.na(mainstem)] = NA

#make config vector for new db table
colnames(mainstem) = paste('S4', colnames(mainstem), sep='__')
mainstem = rename(mainstem, datetime='S4__datetime', watershedID='S4__watershedID')

dbWriteTable(con, 'sensor4', mainstem, append=TRUE)

rm(mainstem)
gc()
