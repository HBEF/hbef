library(dplyr)
library(RMariaDB)

# pass = readLines('~/git/hbef/RMySQL.config')
pass  = readLines('/home/hbef/RMySQL.config')

driver = MariaDB()
con = dbConnect(driver, user='root', password=pass, host='localhost',
    dbname='hbef')

x = read.csv('/home/hbef/shiny/restricted_QAQC/data/sensor_data/CR1000_HBF_WQual_W3.csv',
    stringsAsFactors=FALSE)
# x = read.csv('~/Downloads/CR1000_HBF_WQual_W3.csv', stringsAsFactors=FALSE)
x = select(x, -Site, -RECORD, -Year, -Month, -DOM, -Hour, -Minute, -Second,
        -UNH.ID..) %>%
    rename('datetime'='Date')
colnames(x) = gsub('\\.{2}', '_', colnames(x))
colnames(x) = gsub('\\.', '', colnames(x))
x$watershedID = 3
x$id = 1:nrow(x)
x[x == 'NAN'] = NA
x[,-1] = lapply(x[,-1], as.numeric)
x$datetime = as.POSIXct(x$datetime)

#make config vector for new db table
colnames(x) = paste('S3', colnames(x), sep='__')
x = rename(x, datetime='S3__datetime', id='S3__id', watershedID='S3__watershedID')
fieldnames = colnames(x)
fieldtypes = rep('FLOAT', length(fieldnames))
fieldtypes[1] = 'DATETIME'
fieldtypes[length(fieldtypes) - 1] = 'INT(3)'
fieldtypes[length(fieldtypes)] = 'INT(11) primary key auto_increment'
names(fieldtypes) = fieldnames

#create and write table
dbCreateTable(con, 'sensor3', fieldtypes)
dbWriteTable(con, 'sensor3', x, append=TRUE)

dbDisconnect()
