#hbef w6 S:CAN data (nitrate and turbidity) processing
#THIS MUST RUN AS A SCHEDULED TASK FOLLOWING process_unh_data.R,
#   which begins by dropping the sensor4 table and rebuilding it.

library(tidyverse)
library(glue)
library(RMariaDB)
library(ggplot2)
library(lubridate)

# runmode = 'test'
runmode = 'live'

#setup ####

if(runmode == 'test'){

    dbname = 'hbef20200415'
    setwd('~/git/hbef/shiny/restricted_QAQC/data/S.CAN_data/')
    pass = readLines('~/git/hbef/RMySQL.config')

} else if(runmode == 'live'){

    dbname = 'hbef'
    setwd('/home/mike/shiny/restricted_QAQC/data/S.CAN_data')
    pass  = readLines('/home/mike/RMySQL.config')
}

driver = MariaDB()
con = dbConnect(driver, user='root', password=pass, host='localhost',
                dbname=dbname)

#read raw sensor data table ####
s4 = dbReadTable(con, 'sensor4') %>%
    as_tibble()

#read "loaner" S:CAN dataset from Lisle ####
dloan = read.csv('Reprocessed_SCAN_Data.csv',
              stringsAsFactors = FALSE,
              skip = 1) %>%
    as_tibble() %>%
    filter(Status == 'Ok') %>%
    select(datetime = New.Date.Time..EST.,
           TurbidityRaw = Turbid...NTUeq.200.00.0.00_2,
           Nitrate_mg = NO3.Neq..mg.l.14.29.0.00_2,
           TurbidityRaw_status = X.Turbid._0.0_1.0_0.0_0.0.,
           Nitrate_mg_status = X.NO3.Neq_0.1_1.0_0.0_0.0.) %>%
    mutate(datetime = as.POSIXct(datetime,
                                 format = '%m/%d/%Y %H:%M:%S',
                                 tz = 'EST')) %>%
    arrange(datetime)

#this weird column associated with turbidity seems to contain nothing useful
plot(dloan$datetime, dloan$TurbidityRaw, type = 'l')
points(dloan$datetime[dloan$TurbidityRaw_status == 1],
       dloan$TurbidityRaw[dloan$TurbidityRaw_status == 1],
       col = 'red')

#the analogous column for no3 seems to represent "below detection limit". leave it
plot(dloan$datetime, dloan$Nitrate_mg, type = 'l')
points(dloan$datetime[dloan$Nitrate_mg_status == 1],
       dloan$Nitrate_mg[dloan$Nitrate_mg_status == 1],
       col = 'red')

#filter bollockery from dataset
dloan = select(dloan,
               -Nitrate_mg_status,
               -TurbidityRaw_status)

dloan[is.na(dloan)] = NA

#read the first "for reals" S:CAN dataset from Tammy ####
d1 = read.csv('spec 20150210 192.168.42.10_par.csv',
              stringsAsFactors = FALSE,
              skip = 4) %>%
    as_tibble() %>%
    rename(datetime = 1, Nitrate_mg = 2, Nitrate_mg_status = 3, TurbidityRaw = 4,
           TurbidityRaw_status = 5) %>%
    mutate(datetime = as.POSIXct(datetime,
                                 tz = 'EST')) %>%
    arrange(datetime)

#turbidity flags are wonky (and so is an unflagged point. ditch em)
ggplot(d1, aes(x = datetime,
               y = TurbidityRaw)) +
    geom_point() +
    geom_point(color = factor(d1$TurbidityRaw_status,
                              labels = c('transparent', 'green')))

#NO3-N flags are chill. leave em? naw. ditch em.
ggplot(d1, aes(x = datetime,
               y = Nitrate_mg)) +
    geom_line() +
    geom_point(color = factor(d1$Nitrate_mg_status,
                              labels = c('transparent', 'green')))

#filter bollockery from dataset
d1 = d1 %>%
    filter(TurbidityRaw_status == '',
           Nitrate_mg_status == '',
           TurbidityRaw < 100) %>%
    select(datetime, Nitrate_mg, TurbidityRaw) %>%
    arrange(datetime)

#bind dsets; get them ready for db; insert them into db ####

insert_df = dloan %>%
    bind_rows(d1) %>%
    arrange(datetime) %>%
    rename_with(.fn = ~ paste('S4', ., sep = '__'),
                .cols = c('TurbidityRaw', 'Nitrate_mg')) %>%
    mutate(watershedID = 6)

dbWriteTable(con,
             'sensor4',
             insert_df,
             append = TRUE)

dbDisconnect(con)
