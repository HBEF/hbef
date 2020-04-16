library(dplyr)
library(RMariaDB)

setwd('/home/hbef/shiny/restricted_QAQC/data/unh_sensor_data')
# setwd('~/git/hbef/shiny/restricted_QAQC/data/unh_sensor_data')

# pass = readLines('~/git/hbef/RMySQL.config')
pass  = readLines('/home/hbef/RMySQL.config')

driver = MariaDB()
con = dbConnect(driver, user='root', password=pass, host='localhost',
    dbname='hbef')
    # dbname='hbef20200415')

#read and process w9 ####

header = readr::read_csv('CR1000_HBF_W9_WQual.dat',
    skip=1, col_names=FALSE, n_max=1)
wqual9 = readr::read_csv('CR1000_HBF_W9_WQual.dat', skip=4, col_names=FALSE)
colnames(wqual9) = header

wqual9 = wqual9 %>%
    select(datetime=TIMESTAMP, Nitrate_mg, TempC, Conductivity, SpConductivity,
        pH, DepthMeter, ODOPerCent, ODOMGL, TurbidityFNU, TurbidityRaw,
        FDOMRFU, FDOMQSU, LowEOSCO2_ppm_avg, HighEOSCO2_ppm_avg, EOSTempC) %>%
    mutate(Chl_RFU=NA, BGA_PC_RFU=NA, BGA_PE_RFU=NA,
        AqCO2_ppm_avg=NA, AtmCO2_ppm_avg=NA, watershedID=9, id=1:nrow(wqual9))

#make config vector for new db table
colnames(wqual9) = paste('S4', colnames(wqual9), sep='__')
wqual9 = rename(wqual9, datetime='S4__datetime',
    id='S4__id', watershedID='S4__watershedID')

tables = RMariaDB::dbListTables(con)
if(! 'sensor4' %in% tables){

    #create sensor4 table
    fieldnames = colnames(wqual9)
    fieldtypes = rep('FLOAT', length(fieldnames))
    fieldtypes[1] = 'DATETIME'
    fieldtypes[length(fieldtypes) - 1] = 'INT(3)'
    fieldtypes[length(fieldtypes)] = 'INT(11) primary key auto_increment'
    names(fieldtypes) = fieldnames

    dbCreateTable(con, 'sensor4', fieldtypes)
} else {
    wqual9 = select(wqual9, -id)
}

dbWriteTable(con, 'sensor4', wqual9, append=TRUE)

rm(wqual9)
gc()

#read and process w3 ####

header = readr::read_csv('CR1000_HBF_WQual.dat',
    skip=1, col_names=FALSE, n_max=1)
wqual3 = readr::read_csv('CR1000_HBF_WQual.dat', skip=4, col_names=FALSE)
colnames(wqual3) = header

wqual3 = wqual3 %>%
    select(datetime=TIMESTAMP, Nitrate_mg, TempC, Conductivity, SpConductivity,
        pH, DepthMeter, ODOPerCent, ODOMGL, TurbidityFNU, TurbidityRaw,
        FDOMRFU, FDOMQSU, Chl_RFU, BGA_PC_RFU, BGA_PE_RFU, AqCO2_ppm_avg,
        AtmCO2_ppm_avg) %>%
    mutate(watershedID=3)

#make config vector for new db table
colnames(wqual3) = paste('S4', colnames(wqual3), sep='__')
wqual3 = rename(wqual3, datetime='S4__datetime', watershedID='S4__watershedID')

dbWriteTable(con, 'sensor4', wqual3, append=TRUE)

dbDisconnect(con)
