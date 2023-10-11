library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(glue)

setwd('~/git/hbef/shiny/restricted_QAQC/field_and_lab_note_collections/')
ff = list.files()

# notefile = '20230709.xlsx'

parse_note_collection <- function(notefile){

    sitename_map = tibble(
        sheets123_fmt = c('RG-22', 'RG-1', 'RG-4', 'RG-23',
                          'WS-1', 'WS-2', 'WS-3', 'WS-4', 'WS-5', 'WS-6', 'WS-7',
                          'WS-8', 'WS-9', 'HBK', 'ML70'),
        sheets45_fmt = c('RG-22', 'RG-1', 'RG-4', 'RG-23',
                         'W-1', 'W-2', 'W-3', 'W-4', 'W-5', 'W-6', 'W-7',
                         'W-8', 'W-9', 'HBK', 'ML-70'),
        desired_fmt = c('RG22', 'RG1', 'RG4', 'RG23',
                        'W1', 'W2', 'W3', 'W4', 'W5', 'W6', 'W7',
                        'W8', 'W9', 'HBK', 'ML70')
    )
    
    #precip ####
    
    d = readxl::read_xlsx(notefile,
                          sheet = 1, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    d_date = as.Date(as.numeric(d[5, 4]), origin = '1899-12-30')
    initials_precip = d[[5, 6]]
    addtl_comment = d[[25, 3]]
    
    d_precip = d[11:14, 1:4] %>% 
        as_tibble() %>%
        bind_cols(d[20:23, 8]) %>% 
        rename(site = 1, timeEST = 2, volume_ml = 3, precipCatch = 4,
               fieldCode = 5) %>%
        mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
               volume_ml = as.numeric(volume_ml),
               precipCatch = as.numeric(precipCatch),
               date = d_date,
               fieldCode = as.character(fieldCode),
               notes = addtl_comment) %>% 
        relocate(date, .before = 'timeEST')
    
    d_precip = field_code_handler(d_precip, 1) #see helpers.R
    
    #flow ####
    
    d = readxl::read_xlsx(notefile,
                          sheet = 2, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    d_date = as.Date(as.numeric(d[3, 7]), origin = '1899-12-30')
    initials_flow = d[[3, 9]]
    addtl_comment = d[[20, 3]]
    addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    
    d_flow = d[8:18, 1:9] %>% 
        as_tibble() %>%
        rename(site = 1, timeEST = 2, temp = 3, flowGageHt = 4, hydroGraph = 5,
               color = 6, sediment = 7, notes = 8, fieldCode = 9) %>% 
        mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
               across(c(3, 4, 7), as.numeric),
               date = d_date,
               notes = paste0(notes, addtl_comment),
               notes = sub('^NA -- ', '', notes)) %>% 
        relocate(date, .before = 'timeEST')
    
    d_flow = field_code_handler(d_flow, 2)
    
    #chem ####
    
    d = readxl::read_xlsx(notefile,
                          sheet = 3, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    run_date_chem = as.Date(as.numeric(d[5, 3]), origin = '1899-12-30')
    initials_chem = d[[5, 6]]
    addtl_comment = d[[33, 3]]
    addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    
    d_precip_ = d[12:15, c(1:3, 6:9)] %>% 
        as_tibble() %>%
        rename(site = 1, date = 2, pHmetrohm = 3, pH = 4, spCond = 5, fieldCode = 6,
               archived = 7) %>% 
        mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
               across(3:5, as.numeric),
               archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE))
    
    d_precip_ = field_code_handler(d_precip_, 3)
    
    d_precip = d_precip_ %>% 
        left_join(d_precip, by = c('site', 'date')) %>% 
        mutate(fieldCode = paste(union(fieldCode.x, fieldCode.y), collapse = ' '),
               fieldCode = gsub('NA ?| ?NA', '', fieldCode),
               fieldCode = ifelse(! is.na(fieldCode) & fieldCode == '', NA_character_, fieldCode),
               notes = paste0(notes, addtl_comment),
               notes = sub('^NA -- ', '', notes)) %>% 
        select(-ends_with(c('.x', '.y'))) %>% 
        relocate(timeEST, .after = 'date')
    
    d_flow_ = d[18:28, c(1:4, 6:9)] %>%
        as_tibble() %>%
        rename(site = 1, date = 2, pHmetrohm = 3, ANCMet = 4, pH = 5,
               spCond = 6, fieldCode = 7, archived = 8) %>% 
        mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
               across(3:7, as.numeric),
               archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE))
    
    d_flow_ = field_code_handler(d_flow_, 3)
    
    d_flow = d_flow_ %>% 
        left_join(d_flow, by = c('site', 'date')) %>% 
        mutate(fieldCode = paste(union(fieldCode.x, fieldCode.y), collapse = ' '),
               fieldCode = gsub('NA ?| ?NA', '', fieldCode),
               fieldCode = ifelse(! is.na(fieldCode) & fieldCode == '', NA_character_, fieldCode),
               notes = paste0(notes, addtl_comment),
               notes = sub('^NA -- ', '', notes)) %>% 
        select(-ends_with(c('.x', '.y'))) %>% 
        relocate(timeEST, .after = 'date')
    
    
    #DIC ####
    
    d = readxl::read_xlsx(notefile,
                          sheet = 4, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    d_date = as.Date(as.numeric(d[1, 5]), origin = '1899-12-30')
    run_date_dic = as.Date(as.numeric(d[1, 7]), origin = '1899-12-30')
    
    d_flow = d[4:14, 1:2] %>% 
        as_tibble() %>% 
        rename(site = 1, DIC = 2) %>% 
        mutate(date = d_date,
               DIC = as.numeric(DIC),
               site = toupper(site),
               site = sitename_map$sheets123_fmt[match(site, sitename_map$sheets45_fmt)]) %>%
        left_join(d_flow, by = c('site', 'date')) %>% 
        relocate(DIC, .before = 'notes')
    
    #grab ####
    
    d = readxl::read_xlsx(notefile,
                          sheet = 5, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    addtl_comment = d[[23, 2]]
    addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    
    if(! is.na(d[7, 3])){
        d_grab = d[7:15, 2:16] %>% 
            as_tibble() %>%
            select(site = 1, date = 2, timeEST = 3, pHmetrohm = 4, ANCMet = 5,
                   pH = 7, spCond = 8, DIC = 9, temp = 10, gageHt = 11, hydroGraph = 12,
                   fieldCode = 13, notes = 14, archived = 15) %>% 
            filter(if_any(everything(), ~!is.na(.))) %>% 
            mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
                   across(4:10, as.numeric),
                   site = toupper(site),
                   site = sitename_map$sheets123_fmt[match(site, sitename_map$sheets45_fmt)],
                   date = as.Date(as.numeric(date), origin = '1899-12-30'),
                   notes = paste0(notes, addtl_comment),
                   notes = sub('^NA -- ', '', notes),
                   archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE)) %>% 
            relocate(date, .before = 'timeEST')
        
        d_grab = field_code_handler(d_grab, 5)
    } else {
        d_grab = tibble()
        d_flow$gageHt = NA_real_
    }
    
    
    # resolve, combine ####
    
    #RG-4 data are only used if RG-1 is contaminated (code 923)
    d_precip = split(d_precip, as.factor(d_precip$date)) %>% 
        purrr::map(function(x){
            rg1 = x[x$site == 'RG-1', ]
            rg4 = x[x$site == 'RG-4', ]
            if(! nrow(rg1) || ! nrow(rg4)) return(x)
            if(grepl('923', rg1$fieldCode) | grepl('923', rg4$fieldCode)){
                x = x[! x$site == 'RG-1', ]
                x[x$site == 'RG-4', 'site'] = 'RG-1'
                fcode = pull(x[x$site == 'RG-1', 'fieldCode'])
                x[x$site == 'RG-1', 'fieldCode'] = '923'
                return(x)
            } else {
                return(x)
            }
        }) %>% 
        purrr::reduce(bind_rows)
    
    d_precip = filter(d_precip, site != 'RG-4')
    
    d = d_flow %>% 
        bind_rows(d_grab) %>% 
        bind_rows(d_precip) %>% 
        mutate(site = sub('-', '', site),
               refNo = NA_integer_,
               uniqueID = paste(site, gsub('-', '', date), timeEST, sep = '_'),
               waterYr = if_else(month(date) >= 7, year(date) + 1, year(date)),
               datetime = ymd_hm(paste(date, timeEST))) %>% 
        select(refNo, site, date, timeEST, pH, pHmetrohm, DIC, spCond, temp,
               ANCMet, gageHt, hydroGraph, flowGageHt, precipCatch, fieldCode,
               notes, archived, uniqueID, waterYr, datetime) %>% 
        arrange(site, date, timeEST)
    
    return(d)
}

all = tibble()
for(f in ff){
    print(f)
    out = parse_note_collection(f)
    all = bind_rows(all, out)
    # print(out)
    # readLines(n = 1)
}



all2 = field_code_handler(all)
