#hbef w6 S:CAN data (nitrate and turbidity) processing
#THIS MUST RUN AS A SCHEDULED TASK FOLLOWING process_unh_data.R,
#   which begins by dropping the sensor4 table and rebuilding it.

library(tidyverse)
library(glue)
library(RMariaDB)
library(ggplot2)
library(lubridate)
library(readxl)

# runmode = 'test'
runmode = 'live'

# 0. setup ####

if(runmode == 'test'){

    # dbname = 'hbef20200415' #on E550
    dbname = 'hbef' #on BM1
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

# 1. read raw sensor data table ####
s4 = dbReadTable(con, 'sensor4') %>%
    as_tibble()

# 2. read "loaner" S:CAN dataset from Lisle ####

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

# OBSOLETE. read the first "for reals" S:CAN dataset from Tammy ####

#this one is supplanted by the one Tammy sent on 2020-12-04

# d1 = read.csv('spec 20150210 192.168.42.10_par.csv',
#               stringsAsFactors = FALSE,
#               skip = 4) %>%
#     as_tibble() %>%
#     rename(datetime = 1, Nitrate_mg = 2, Nitrate_mg_status = 3, TurbidityRaw = 4,
#            TurbidityRaw_status = 5) %>%
#     mutate(datetime = as.POSIXct(datetime,
#                                  tz = 'EST')) %>%
#     arrange(datetime)
#
# #turbidity flags are wonky (and so is an unflagged point. ditch em)
# ggplot(d1, aes(x = datetime,
#                y = TurbidityRaw)) +
#     geom_point() +
#     geom_point(color = factor(d1$TurbidityRaw_status,
#                               labels = c('transparent', 'green')))
#
# #NO3-N flags are chill. leave em? naw. ditch em.
# ggplot(d1, aes(x = datetime,
#                y = Nitrate_mg)) +
#     geom_line() +
#     geom_point(color = factor(d1$Nitrate_mg_status,
#                               labels = c('transparent', 'green')))
#
# #filter bollockery from dataset
# d1 = d1 %>%
#     filter(TurbidityRaw_status == '',
#            Nitrate_mg_status == '',
#            TurbidityRaw < 100) %>%
#     select(datetime, Nitrate_mg, TurbidityRaw) %>%
#     arrange(datetime)

# 3. read the first and second "for reals" S:CAN datasets from Tammy ####

d1 = read.csv('batches_1_and_2.csv',
              stringsAsFactors = FALSE,
              skip = 4) %>%
    as_tibble() %>%
    select(-(4:5)) %>% #ignore temperature data
    rename(datetime = 1, Nitrate_mg = 2, Nitrate_mg_status = 3, TurbidityRaw = 4,
           TurbidityRaw_status = 5) %>%
    mutate(datetime = as.POSIXct(datetime,
                                 tz = 'EST')) %>%
    arrange(datetime) %>%
    filter(datetime > as.POSIXct('2020-08-06 09:45:07', #last record in dloan
                                 tz = 'EST'))

#visualize flagged turbitidy points
ggplot(d1, aes(x = datetime,
               y = TurbidityRaw)) +
    geom_point() +
    geom_point(color = factor(d1$TurbidityRaw_status,
                              labels = c('transparent', 'green', 'red')))

#same for nitrate
ggplot(d1, aes(x = datetime,
               y = Nitrate_mg)) +
    geom_line() +
    geom_point(color = factor(d1$Nitrate_mg_status,
                              labels = c('transparent', 'green', 'orange')))

#filter bollockery from dataset
d1 = d1 %>%
    filter(TurbidityRaw_status == '',
           Nitrate_mg_status == '',
           TurbidityRaw < 100) %>% #notfy someone?
    select(datetime, Nitrate_mg, TurbidityRaw) %>%
    arrange(datetime)

# OBSOLETE and the third (RESOLVE NTU vs FTU disparity from first two batches) ####

# d2 = read.csv('SCAN 4-22 thru 7-28-2022.csv',
#               stringsAsFactors = FALSE,
#               skip = 4) %>%
#     as_tibble() %>%
#     select(-(4:5)) %>% #ignore temperature data
#     rename(datetime = 1, Nitrate_N_mg = 2, Nitrate_N_mg_status = 3, TurbidityRawNTU = 4,
#            TurbidityRawNTU_status = 5) %>%
#     mutate(datetime = as.POSIXct(datetime,
#                                  tz = 'EST')) %>%
#     arrange(datetime) %>%
#     filter(datetime > as.POSIXct('2020-08-06 09:45:07', #last record in dloan
#                                  tz = 'EST'))
# 
# #visualize flagged turbidity points
# ggplot(d2, aes(x = datetime,
#                y = TurbidityRawNTU)) +
#     geom_point() +
#     geom_point(color = factor(d2$TurbidityRawNTU_status,
#                               labels = c('transparent', 'green', 'red')))
# 
# #same for nitrate
# ggplot(d2, aes(x = datetime,
#                y = Nitrate_N_mg)) +
#     geom_line() +
#     geom_point(color = factor(d2$Nitrate_N_mg_status,
#                               labels = c('transparent', 'green', 'orange')))
# 
# #clean stuff
# # unique(d1$TurbidityRaw_status)
# d2b = d2 %>%
#     filter(TurbidityRawNTU_status == '',
#            Nitrate_N_mg_status == '') %>%
#     filter(Nitrate_N_mg < 10)
# ggplot(d2b, aes(x = datetime,
#                y = Nitrate_N_mg)) +
#     geom_line()
# 
# #filter bollockery from dataset NOT UPDATED
# #NEED CLARITY ON NO3-N, FNU, AND 2021 DATA
# d2 = d2 %>%
#     filter(TurbidityRawNTU_status == '',
#            Nitrate_N_mg_status == '') %>%
#     select(datetime, Nitrate_N_mg, TurbidityRaw = TurbidityRawNTU) %>%
#     arrange(datetime)

# 5. more sets ####

clean_SCAN_data <- function(fpath){
    read_xlsx(fpath, skip = 4, guess_max = 10000, col_names = FALSE) %>%
        select(-(4:5)) %>% #ignore temperature data
        rename(datetime = 1, Nitrate_mg = 2, Nitrate_mg_status = 3, TurbidityRawFTU = 4,
               TurbidityRawFTU_status = 5) %>%
        mutate(datetime = as.POSIXct(datetime, tz = 'EST')) %>%
        arrange(datetime) %>%
        filter(is.na(TurbidityRawFTU_status),
               is.na(Nitrate_mg_status)) %>%
        select(datetime, Nitrate_mg, TurbidityRaw = TurbidityRawFTU) %>%
        arrange(datetime)
}
legit_flags <- c('VAL_ABOVE', 'VAL_BELOW')
clean_SCAN_data_since20230724 <- function(fpath, end_of_previous){
  
  # library(cellranger)
  # 
  # cols <- as.cell_limits(ul = c(NA, 1), lr = c(NA, 12))$col
  # selected_cols <- cols[c(1, 4, 5, 11, 12)]
  # data <- read_xlsx("path_to_your_file.xlsx", range = cell_cols(selected_cols))
  
    read_xlsx(fpath, skip = 4, col_names = FALSE,
              col_types = c('date', 'numeric', 'text', 'numeric', 'text',
                            'numeric', 'text', 'numeric', 'numeric', 'text', 'numeric', 'text')) %>%
        select(-c(2:3, 6:10)) %>% #ignore temperature and nitrate eq data. FTU is no longer
        rename(datetime = 1, Nitrate_mg = 2, Nitrate_mg_status = 3, TurbidityRawNTU = 4,
               TurbidityRawNTU_status = 5) %>%
        mutate(datetime = as.POSIXct(datetime, tz = 'EST')) %>%
        arrange(datetime) %>%
        filter(datetime > end_of_previous,
               is.na(TurbidityRawNTU_status) | TurbidityRawNTU_status %in% legit_flags,
               is.na(Nitrate_mg_status) | Nitrate_mg_status %in% legit_flags) %>%
        select(datetime, Nitrate_mg, TurbidityRawNTU = TurbidityRawNTU) %>%
        arrange(datetime)
}

d2 <- clean_SCAN_data('SCAN 2021-4-30 thru 2022-10-12.xlsx')
d3 <- clean_SCAN_data('SCAN 2022-10-12 thru 2022-12-12.xlsx') %>% 
  slice(-(1:10))
d4 <- clean_SCAN_data('SCAN 2-12-2022 thru 2-27-2023.xlsx')
d5 <- clean_SCAN_data('SCAN 2023-02-27 thru 2023-05-16.xlsx') %>% 
  slice(-(1:14))
d6 <- clean_SCAN_data('SCAN 2023 05-16 thru 07-24.xlsx')
d7 <- clean_SCAN_data_since20230724('SCAN 2023-7-24 thru 2024-3-1.xlsx',
                                    end_of_previous = max(d6$datetime))
#before adding d8, make sure clean_SCAN_data_since20230724 still checks out. 
  #are they recording FTU again?

# plot(zz$datetime, zz$Nitrate_mg, type = 'b')
# lines(d6$datetime, d6$Nitrate_mg, type = 'b')
# plot(zz$datetime, zz$TurbidityRawNTU, type = 'b')
# lines(d6$datetime, d6$TurbidityRaw, type = 'b')

# bind dsets; get them ready for db; insert them into db ####

insert_df = select(dloan, -ends_with('status')) %>%
    bind_rows(select(d1, -ends_with('status'))) %>%
    bind_rows(d2) %>% 
    bind_rows(d3) %>% 
    bind_rows(d4) %>% 
    bind_rows(d5) %>% 
    bind_rows(d6) %>% 
    bind_rows(d7) %>% 
    # bind_rows(d8) %>% 
    arrange(datetime) %>%
    rename_with(.fn = ~ paste('S4', ., sep = '__'),
                .cols = c('TurbidityRaw', 'Nitrate_mg', 'TurbidityRawNTU')) %>%
    mutate(watershedID = 6)

dbWriteTable(con,
             'sensor4',
             insert_df,
             append = TRUE)

dbDisconnect(con)
