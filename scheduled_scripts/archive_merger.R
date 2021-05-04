library(tidyverse)
library(lubridate)
library(RMariaDB)
library(DBI)

# setup ####

# setwd('~/git/hbef/shiny/')
setwd('/home/mike/shiny/')

dbname = 'hbef'
pass = readLines('../RMySQL.config')
con = dbConnect(RMariaDB::MariaDB(),
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = dbname)

# read, munge ####

arch = read_csv('restricted_QAQC/data/archive_data/HB physical archives stream samples.csv',
                skip = 2, col_types = 'cnnnccccccc') %>%
    rename_with(~gsub('\\s+', '_', .))

arch$time_EST[arch$time_EST == -9999] = NA
arch$bottle_type = str_replace(arch$bottle_type,
                               '[nN]algene ?([0-9]+)', 'Nalgene\\1')
arch$bottle_type = str_replace(arch$bottle_type, '([0-9]+)mlNM', 'narrow\\1')

arch = arch %>%
    mutate(watershed = gsub('ws', 'W', watershed),
           sample_date = mdy(sample_date),
           date_weighed = mdy(date_weighed),
           old_date = mdy(old_date),
           time_EST = str_pad(time_EST, 4, 'left', '0'),
           time_EST = paste0(substr(time_EST, 1, 2), ':',
                             substr(time_EST, 3, 4), ':00'))

#identify date discrepancies (looks like they've all been corrected already)
# arch[!is.na(arch$old_date),] %>% select(watershed, sample_date, time_EST, old_date)

arch$old_date = NULL
arch$id = 1:nrow(arch)
arch = select(arch, id, everything())

#convert AM/PM to 24-hour
arch$time_weighed = lubridate::parse_date_time(x = paste(arch$date_weighed,
                                                         arch$time_weighed),
                                               orders = '%Y-%m-%d %I:%M:%S %p',
                                               tz = 'US/Eastern') %>%
    stringr::str_split(' ') %>%
    map_chr(2)

wonky_timeEST_ind = which(sapply(strsplit(arch$time_EST, ':'),
                                 function(x) any(x == 'NA')))
arch$time_EST[wonky_timeEST_ind] = NA

# (over)write archive table in hbef database ####

try(RMariaDB::dbRemoveTable(con, 'archive'),
    silent = TRUE)

fieldnames = colnames(arch)
fieldtypes = c('INT(11) primary key auto_increment', 'VARCHAR(10)', 'FLOAT',
               'INT(5)', 'FLOAT', 'DATE', 'TIME', 'DATE', 'TIME',
               'VARCHAR(15)', 'TINYTEXT')
names(fieldtypes) = fieldnames

dbCreateTable(con, 'archive', fieldtypes)
dbWriteTable(con, 'archive', arch, append=TRUE)
dbDisconnect(con)
