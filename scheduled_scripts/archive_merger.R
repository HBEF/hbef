library(tidyverse)
library(lubridate)
library(readxl)
library(RMariaDB)
library(DBI)

#WARNING: run this for reals on server, not local. local test is fine, but
#you'll need to set up a copy of the MySQL database

#stream archive data come from two places:
#1. restricted_QAQC/data/archive_data/HB physical archives stream samples.csv
#   (all watersheds, through 2017-05)
#2. any xlsx files Amey sends, one watershed per file (e.g. "w6 sample archive
#   collection through 2026.xlsx"). drop them, unmodified, into
#   restricted_QAQC/data/archive_data/stream_updates/. each one replaces that
#   watershed's rows from (1). files are applied in alphabetical order.

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

# archive parsing helpers ####

parse_archive_time = function(x){

    #sample times have arrived as HHMM integers (915, 1350), excel day fractions
    #(0.3854 = 09:15), clock text (09:15), and placeholders (-9999, n/a).
    #the numeric forms can't be confused: a fraction is always < 1 and HHMM is
    #always a whole number (0 is midnight either way). anything else is an error.
    #24:00 is kept as recorded.

    x = trimws(as.character(x))
    x[x %in% c('', '-9999', 'n/a', 'N/A', 'NA', 'na')] = NA
    num = suppressWarnings(as.numeric(x))
    clock = ! is.na(x) & grepl('^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$', x)
    frac = ! clock & ! is.na(num) & num >= 0 & num < 1
    hhmm = ! clock & ! is.na(num) & num >= 1 & num == round(num)

    hh = mm = rep(NA_real_, length(x))
    hh[frac] = round(num[frac] * 1440) %/% 60
    mm[frac] = round(num[frac] * 1440) %% 60
    hh[hhmm] = num[hhmm] %/% 100
    mm[hhmm] = num[hhmm] %% 100
    hh[clock] = as.numeric(sub(':.*', '', x[clock]))
    mm[clock] = as.numeric(sub('^[0-9]+:([0-9]{2}).*', '\\1', x[clock]))

    bad = ! is.na(x) & (is.na(hh) | hh > 24 | mm > 59 | (hh == 24 & mm > 0))
    if(any(bad)) stop('unparseable sample time(s): ', paste(unique(x[bad]), collapse = ', '))

    ifelse(is.na(hh), NA_character_, sprintf('%02d:%02d:00', hh, mm))
}

parse_archive_date = function(x){

    #excel serial numbers or m/d/y (or y-m-d) text

    x = trimws(as.character(x))
    num = suppressWarnings(as.numeric(x))
    out = as.Date(rep(NA, length(x)))
    serial = ! is.na(num)
    out[serial] = as.Date(num[serial], origin = '1899-12-30')
    out[! serial] = as.Date(parse_date_time(x[! serial], c('mdy', 'ymd'), quiet = TRUE))

    bad = ! is.na(x) & (is.na(out) | out < as.Date('1950-01-01') | out > Sys.Date())
    if(any(bad)) stop('unparseable or implausible date(s): ', paste(unique(x[bad]), collapse = ', '))

    out
}

parse_weigh_time = function(x){

    #"01:35:48 PM" text or excel day fractions

    x = trimws(as.character(x))
    num = suppressWarnings(as.numeric(x))
    secs = ifelse(! is.na(num), round(num * 86400),
                  period_to_seconds(hms(format(parse_date_time(x, c('IMS p', 'HMS'), quiet = TRUE),
                                               '%H:%M:%S'), quiet = TRUE)))
    ifelse(is.na(secs), NA_character_,
           sprintf('%02d:%02d:%02d', secs %/% 3600, secs %% 3600 %/% 60, secs %% 60))
}

standardize_stream_archive = function(d, site){

    #d: all-character data frame with snake_case column names from either
    #source. date/time weighed are optional (Amey's xlsx files omit them)

    for(cl in c('date_weighed', 'time_weighed')) if(! cl %in% names(d)) d[[cl]] = NA_character_

    d %>%
        transmute(site = site,
                  site_type = 'stream',
                  sample_date = parse_archive_date(sample_date),
                  timeEST = parse_archive_time(time_est),
                  barcode = as.numeric(barcode),
                  bin = as.numeric(bin),
                  date_weighed = parse_archive_date(date_weighed),
                  time_weighed = parse_weigh_time(time_weighed),
                  weight_g = as.numeric(weight_g),
                  bottle_type = str_replace(bottle_type, '[nN]algene ?([0-9]+)', 'Nalgene\\1'),
                  bottle_type = str_replace(bottle_type, '([0-9]+)mlNM', 'narrow\\1'),
                  #typo in Amey's W6 file (56153, 2025-11-24)
                  bottle_type = str_replace(bottle_type, '^Nalgene251$', 'Nalgene250'),
                  #NM = narrow mouth (e.g. "Nalgene500NM" in Amey's W6 file)
                  bottle_type = str_replace(bottle_type, '^Nalgene([0-9]+)NM$', 'narrow\\1'),
                  notes = notes_sample_condition)
}

read_stream_archive_xlsx = function(f){

    #watershed comes from the title rows ("Watershed 6 Streamflow Collection"),
    #or failing that the sheet name ("w6"). header row is the one containing "barcode"

    sheet = excel_sheets(f)[1]
    top = read_excel(f, sheet = sheet, col_names = FALSE, n_max = 10,
                     col_types = 'text', .name_repair = 'minimal')
    site = str_match(paste(na.omit(unlist(top)), collapse = ' '),
                     '[Ww]atershed ([0-9]+)')[, 2]
    if(is.na(site)) site = str_match(sheet, '^[wW][sS]? ?([0-9]+)$')[, 2]
    if(is.na(site)) stop('cannot determine watershed for ', f)
    header_row = which(apply(top, 1, function(r) any(tolower(trimws(r)) == 'barcode')))[1]

    d = read_excel(f, sheet = sheet, skip = header_row - 1, col_types = 'text') %>%
        rename_with(~gsub('\\s+', '_', tolower(trimws(.)))) %>%
        filter(! is.na(barcode))

    missing_cols = setdiff(c('bin', 'barcode', 'weight_g', 'sample_date', 'time_est',
                             'bottle_type', 'notes_sample_condition'), names(d))
    if(length(missing_cols)) stop(f, ' is missing column(s): ', paste(missing_cols, collapse = ', '))
    if(any(duplicated(d$barcode))) stop(f, ' has repeated barcodes')

    standardize_stream_archive(d, paste0('W', site))
}

# read, munge stream archive data ####

arch = read_csv('restricted_QAQC/data/archive_data/HB physical archives stream samples.csv',
                skip = 2, col_types = cols(.default = 'c')) %>%
    rename_with(~gsub('\\s+', '_', tolower(.)))
arch = bind_rows(lapply(split(arch, arch$watershed), function(d){
    standardize_stream_archive(d, gsub('ws', 'W', d$watershed[1]))
}))

update_files = sort(list.files('restricted_QAQC/data/archive_data/stream_updates',
                               pattern = '\\.xlsx$', full.names = TRUE))

for(f in update_files){

    upd = read_stream_archive_xlsx(f)
    upd_site = upd$site[1]
    prev = filter(arch, site == upd_site)

    #updates must be supersets of what they replace
    dropped = setdiff(prev$barcode, upd$barcode)
    if(length(dropped)){
        stop(basename(f), ' omits ', length(dropped), ' ', upd_site,
             ' barcode(s) present in the previous archive, e.g. ',
             paste(head(dropped), collapse = ', '))
    }

    #weigh dates/times aren't in Amey's xlsx files; carry them over by barcode
    upd = upd %>%
        left_join(select(prev, barcode, dw = date_weighed, tw = time_weighed),
                  by = 'barcode') %>%
        mutate(date_weighed = coalesce(date_weighed, dw),
               time_weighed = coalesce(time_weighed, tw)) %>%
        select(-dw, -tw)

    message(basename(f), ': ', upd_site, ' now has ', nrow(upd), ' bottles (',
            nrow(upd) - nrow(prev), ' new)')

    arch = bind_rows(filter(arch, site != upd_site), upd)
}

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

if(misc_dir == 'hbef_misc') stop('on local machine? make sure you are using the most recent version of the database and archive dataset')
#this file is used by edi_upload_prep.R
write_csv(rename(bind_rows(arch, arch2), date = sample_date), na = '',
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
    #many-to-many: two bottles at one site/date/time, with primary + Dup analyses
    #(W6 1999-11-27), can't be paired up from the data alone
    left_join(dataAll,
              by = c('site', 'sample_date' = 'date', 'timeEST'),
              relationship = 'many-to-many') %>%
    rename(date = sample_date) %>%
    #a bottle with lab duplicate analyses appears once per analysis; the
    #duplicate column (Dup, Dup2) distinguishes them. primary analysis first
    mutate(duplicate = na_if(duplicate, '')) %>%
    arrange(site, date, timeEST, barcode, ! is.na(duplicate), duplicate) %>%
    select(-archived, -datetime, -uniqueID, -waterYr, -sampleType,
           -precipCatch, -hydroGraph, -gageHt, -fieldCode, -field_notes, -refNo,
           -pHmetrohm, -ionError, -ionBalance, -theoryCond, -flowGageHt) %>%
    select(site, site_type, date, timeEST, barcode, duplicate, bin, date_weighed,
           time_weighed, weight_g, bottle_type, notes, NO3_N, NH4_N,
           everything()) %>%
    mutate(site = as.factor(site),
           bottle_type = as.factor(bottle_type),
           weight_g = round(weight_g, 2),
           NO3_N = round(NO3_N, 2),
           NH4_N = round(NH4_N, 2))


#embed data in HTML ####

htmlf = read_lines('HTML/archive_explore/archive_explore.html')
# insert_ind = grep("<div id='archive_hot'></div>", htmlf)
insert_ind_start = grep("<script id='archive_script'>", htmlf)
insert_ind_end = grep("\\s?const container =", htmlf, perl=TRUE)
arch2 = mutate(arch, across(everything(), as.character))
classvec = unname(sapply(arch, class))
enquote = rep(TRUE, length(classvec))
enquote[classvec %in% c('numeric', 'integer')] = FALSE

arch2 = mutate(arch2,
       across(which(enquote), function(x) paste0("'", gsub("'", "\\\\'", x), "'")))

#handsontable column types, one per column, so they can't drift out of alignment
col_config = case_when(
    colnames(arch) %in% c('date', 'date_weighed') ~ "{type: 'date', dateFormat: 'YYYY-MM-DD'}",
    colnames(arch) %in% c('timeEST', 'time_weighed') ~ "{type: 'time', timeFormat: 'HH:mm:ss'}",
    ! enquote ~ "{type: 'numeric'}",
    TRUE ~ '{}')

arch2_js = c(paste('var header_row =',
                   paste0("['", paste(colnames(arch2), collapse = "','"), "'];")),
             paste0('var col_config = [', paste(col_config, collapse = ', '), '];'),
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

readr::write_lines(htmlf, 'HTML/archive_explore/archive_explore.html')
# readr::write_csv(arch, 'restricted_QAQC/data/archive_data/archive_merged.csv')
