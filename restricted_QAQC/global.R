# Variables that can be accessed by ui.R and server.R

library(RMariaDB)

# Grabbing Data in MySQL ----
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

# Find maximum date
maxDate_initial <- max(dataInitial$date, na.rm=TRUE)
maxDate_current <- max(dataCurrent$date, na.rm=TRUE)
maxDate_historical <- max(dataHistorical$date, na.rm=TRUE)
maxDate_sensor <- max(dataSensor$date, na.rm=TRUE)

maxDate <- maxDate_historical # default value if dataCurrent or dataSensor are empty
if (maxDate_sensor > maxDate_current) maxDate <- maxDate_sensor
if (maxDate_sensor < maxDate_current) maxDate <- maxDate_current


dbDisconnect(con)