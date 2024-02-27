library(tidyverse)
library(lubridate)
library(RMariaDB)
library(DBI)

#WARNING: run this for reals on server, not local. local test is fine, but
#you'll need to set up a copy of the MySQL database

#if you get more raw xlsx files from Amey, you might have to:
#1. convert to csv
#2. remove the four hidden columns

# setup ####

# setwd('~/git/hbef/shiny/'); misc_dir = 'hbef_misc'
setwd('/home/mike/shiny/'); misc_dir = 'misc'

source('restricted_QAQC/helpers.R')

dbname = 'hbef'
pass = readLines('../RMySQL.config')
con = dbConnect(RMariaDB::MariaDB(),
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = dbname)

# read, munge stream archive data ####

arch = read_csv('restricted_QAQC/data/archive_data/HB physical archives stream samples.csv',
                skip = 2, col_types = 'cnnnccccccc') %>%
    rename_with(~gsub('\\s+', '_', .))

arch$time_EST[arch$time_EST == -9999] = NA
arch$bottle_type = str_replace(arch$bottle_type,
                               '[nN]algene ?([0-9]+)', 'Nalgene\\1')
arch$bottle_type = str_replace(arch$bottle_type, '([0-9]+)mlNM', 'narrow\\1')

arch = arch %>%
    rename(timeEST = time_EST,
           site = watershed,
           notes = notes_sample_condition) %>%
    mutate(site = gsub('ws', 'W', site),
           sample_date = mdy(sample_date),
           date_weighed = mdy(date_weighed),
           old_date = mdy(old_date),
           timeEST = str_pad(timeEST, 4, 'left', '0'),
           timeEST = paste0(substr(timeEST, 1, 2), ':',
                             substr(timeEST, 3, 4), ':00'),
           site_type = 'stream')

#identify date discrepancies (looks like they've all been corrected already)
# arch[!is.na(arch$old_date),] %>% select(site, sample_date, time_EST, old_date)

arch$old_date = NULL
# arch$id = 1:nrow(arch)
# arch = select(arch, id, site, site_type, everything())

#convert AM/PM to 24-hour
arch$time_weighed = lubridate::parse_date_time(x = paste(arch$date_weighed,
                                                         arch$time_weighed),
                                               orders = '%Y-%m-%d %I:%M:%S %p',
                                               tz = 'US/Eastern') %>%
    stringr::str_split(' ') %>%
    map_chr(2)

wonky_timeEST_ind = which(sapply(strsplit(arch$timeEST, ':'),
                                 function(x) any(x == 'NA')))
arch$timeEST[wonky_timeEST_ind] = NA

# read, munge precip archive data ####

arch2 = read_csv('restricted_QAQC/data/archive_data/HB precipitation.csv',
                skip = 2, col_types = 'cnnnccccccc') %>%
    rename_with(~gsub('\\s+', '_', .))

arch2$Time_EST[arch2$Time_EST == '-9999'] = NA
arch2$bottle_type = str_replace(arch2$bottle_type,
                               '[nN]algene ?([0-9]+)', 'Nalgene\\1')
arch2$bottle_type = str_replace(arch2$bottle_type, '([0-9]+)mlNM', 'narrow\\1')

wonky_date_ind = which(nchar(arch2$date_weighed) > 9)
arch2$date_weighed[wonky_date_ind] = str_match(arch2$date_weighed[wonky_date_ind],
                                               '^([^ ]+).*')[, 2]

arch2 = arch2 %>%
    rename(timeEST = Time_EST,
           site = rain_gage) %>%
    mutate(site = gsub('rg ', 'RG', site),
           sample_date = mdy(sample_date),
           date_weighed = mdy(date_weighed),
           timeEST = str_pad(timeEST, 4, 'left', '0'),
           timeEST = paste0(substr(timeEST, 1, 2), ':',
                             substr(timeEST, 3, 4), ':00'),
           site_type = 'precip gauge')

# arch2$id = 1:nrow(arch2)
# arch2 = select(arch2, id, site, site_type, everything())

#convert AM/PM to 24-hour
arch2$time_weighed = lubridate::parse_date_time(x = paste(arch2$date_weighed,
                                                         arch2$time_weighed),
                                               orders = '%Y-%m-%d %I:%M:%S %p',
                                               tz = 'US/Eastern') %>%
    stringr::str_split(' ') %>%
    map_chr(2)

wonky_timeEST_ind = which(sapply(strsplit(arch2$timeEST, ':'),
                                 function(x) any(x == 'NA')))
arch2$timeEST[wonky_timeEST_ind] = NA

stop('have we decided how to deal with duplicates')
if(misc_dir == 'hbef_misc') stop('on local machine? make sure you are using the most recent version of the database and archive dataset')
#this file is used by edi_upload_prep.R
write_csv(bind_rows(arch, arch2),
          file.path('..', misc_dir, 'edi_upload/archive_samples.csv'))

# (over)write archive table in hbef database OBSOLETE ####

# try(RMariaDB::dbRemoveTable(con, 'archive'),
#     silent = TRUE)
#
# fieldnames = colnames(arch)
# fieldtypes = c('INT(11) primary key auto_increment', 'VARCHAR(10)', 'VARCHAR(10)',
#                'INT(5)', 'FLOAT', 'DATE', 'TIME', 'DATE', 'TIME',
#                'VARCHAR(15)', 'TINYTEXT')
# names(fieldtypes) = fieldnames
#
# dbCreateTable(con, 'archive', fieldtypes)
# dbWriteTable(con, 'archive', arch, append=TRUE)
# dbDisconnect(con)

# more munging (bind stream and precip; merge with field data) ####

dataCurrent = dbReadTable(con, "current") %>%
    mutate(
        NO3_N=NO3_to_NO3N(NO3),
        NH4_N=NH4_to_NH4N(NH4)) %>%
    select(-NO3, -NH4) %>%
    filter(date >= as.Date('2013-06-01')) %>%
    arrange(site, date, timeEST) %>%
    mutate(timeEST = as.character(timeEST))

dataHistorical = dbReadTable(con, "historical") %>%
    filter(! (site == 'W6' & date == as.Date('2007-08-06'))) %>%
    mutate(
        NO3_N=NO3_to_NO3N(NO3),
        NH4_N=NH4_to_NH4N(NH4)) %>%
    select(-NO3, -NH4) %>%
    mutate(timeEST = as.character(timeEST))

dbDisconnect(con)

# readr::write_csv(arch, '/tmp/arch1.csv')
# readr::write_csv(dataArchive, '/tmp/arch2.csv')
# arch = readr::read_csv('/tmp/arch1.csv')
#
# defClasses <- read.csv("../data/Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
# defClassesSample <- read.csv("../data/RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
# defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")
# dataCurrent <- standardizeClasses(dataCurrent)
# dataCurrent$notes <- gsub(",", ";", dataCurrent$notes)
# dataHistorical <- standardizeClasses(dataHistorical)

dataAll = bind_rows(dataCurrent, select(dataHistorical, -canonical)) %>%
    as_tibble() %>%
    rename(field_notes = notes)

arch = arch %>%
    bind_rows(arch2) %>%
    # select(-id) %>%
    mutate(time_weighed = as.character(time_weighed)) %>%
           # timeEST = as.difftime(timeEST)) %>%
    left_join(dataAll,
                     # timeEST = as.difftime(timeEST)),
              by = c('site', 'sample_date' = 'date', 'timeEST')) %>%
    rename(date = sample_date) %>%
    arrange(site, date, timeEST) %>%
    select(-archived, -datetime, -uniqueID, -waterYr, -duplicate, -sampleType,
           -precipCatch, -hydroGraph, -gageHt, -fieldCode, -field_notes, -refNo,
           -pHmetrohm, -ionError, -ionBalance, -theoryCond, -flowGageHt) %>%
    select(site, site_type, date, timeEST, barcode, bin, date_weighed,
           time_weighed, weight_g, bottle_type, notes, NO3_N, NH4_N,
           everything()) %>%
    mutate(site = as.factor(site),
           bottle_type = as.factor(bottle_type),
           weight_g = round(weight_g, 2),
           NO3_N = round(NO3_N, 2),
           NH4_N = round(NH4_N, 2))


#embed data in HTML ####

htmlf = read_lines('HTML/restricted/archive_explore/archive_explore.html')
# insert_ind = grep("<div id='archive_hot'></div>", htmlf)
insert_ind_start = grep("<script id='archive_script'>", htmlf)
insert_ind_end = grep("\\s?const container =", htmlf, perl=TRUE)
arch2 = mutate(arch, across(everything(), as.character))
classvec = unname(sapply(arch, class))
enquote = rep(TRUE, length(classvec))
enquote[classvec %in% c('numeric', 'integer')] = FALSE

arch2 = mutate(arch2,
       across(which(enquote), function(x) paste0("'", x, "'")))

arch2_js = c(paste('var header_row =',
                   paste0("['", paste(colnames(arch2), collapse = "','"), "'];")),
             'var hot_data = [',
             apply(arch2, 1, function(x){
                 paste0('[', paste(x, collapse = ','), '],')
             }),
             '];')

arch2_js = gsub("'NA'", 'null', arch2_js)
arch2_js = gsub(',NA,', ',null,', arch2_js)
arch2_js = gsub(',NA,', ',null,', arch2_js)
arch2_js = gsub(',NA]', ',null]', arch2_js)

htmlf = c(htmlf[1:insert_ind_start],
          '',
          arch2_js,
          '',
          htmlf[(insert_ind_end):length(htmlf)])

readr::write_lines(htmlf, 'HTML/restricted/archive_explore/archive_explore.html')
# readr::write_csv(arch, 'restricted_QAQC/data/archive_data/archive_merged.csv')
