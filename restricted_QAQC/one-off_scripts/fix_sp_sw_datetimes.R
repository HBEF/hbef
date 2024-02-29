
library(tidyverse)

setwd('/home/mike/git/hbef/shiny/restricted_QAQC/field_and_lab_note_collections')
fs = list.files()
fs = grep('20231[0-2]..\\.xlsx', fs, value = T)
dir.create('~/Desktop/aa')

field_code_handler <- function(d, sheet){
    
    #sheet is the number corresponding to the index of the data sheet in the note xlsx
    
    d$fieldCode <- sub('[/;,]', ' ', d$fieldCode)
    d$fieldCode <- sub(' {2,}', ' ', d$fieldCode)
    # d$fieldCode <- gsub('([0-9]{3}) \\1', '\\1', d$fieldCode)
    
    # cat('resulting codes:\n', paste(unique(d$fieldCode), collapse = '; '), '\n')
    
    qq <- is.na(d$fieldCode) |
        grepl('^[0-9]{3}$', d$fieldCode) |
        grepl('^[0-9]{3} [0-9]{3}$', d$fieldCode)
    illegal_codes <- unique(d$fieldCode[! qq])
    
    if(any(! qq)) stop(glue('Unrecognized fieldCode(s) in sheet {sheet}: ', paste(illegal_codes, collapse = '; ')))
    
    return(d)
}

scan_for_typos = function(d, sheet, numcol_indices){
    
    if(! nrow(d)) return(d)
    
    errcol = sapply(d[, numcol_indices], function(x) any(! is.na(x) & is.na(as.numeric(x))))
    if(any(errcol)){
        clm = names(which(errcol))
        stop(glue('Detected illegal character in numeric column "{clm}", sheet {sheet}'))
    }
    
    return(d)
}


# sp_sw_corrections <- tibble(site = character(), date = Date(), time = character())
# f = fs[1]

# f = '20231016.xlsx'
# f = '20231010.xlsx'
sp_sw_corrections <- tibble()
for(f in fs){
    
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
    
    d = readxl::read_xlsx(f,
                          sheet = 1, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    d_date = as.Date(as.numeric(d[5, 4]), origin = '1899-12-30')
    initials_precip = d[[5, 6]]
    addtl_comment = d[[25, 3]]
    addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    
    d_precip = d[11:14, 1:4] %>% 
        as_tibble() %>%
        bind_cols(d[20:23, 8:9]) %>% 
        rename(site = 1, timeEST = 2, volume_ml = 3, precipCatch = 4,
               fieldCode = 5, notes = 6) %>%
        scan_for_typos(1, c(3, 4)) %>% 
        mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
               volume_ml = as.numeric(volume_ml),
               precipCatch = as.numeric(precipCatch),
               date = d_date,
               fieldCode = as.character(fieldCode),
               notes = paste0(notes, addtl_comment),
               notes = sub('^NA -- ', '', notes)) %>% 
        relocate(date, .before = 'timeEST')
    
    d_precip = field_code_handler(d_precip, 1)
    
    #flow ####
    
    d = readxl::read_xlsx(f,
                          sheet = 2, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    d_date = as.Date(as.numeric(d[3, 7]), origin = '1899-12-30')
    initials_flow = d[[3, 9]]
    addtl_comment = d[[20, 3]]
    addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    
    d_flow = d[8:18, c(1:5, 8:9)] %>% 
        as_tibble() %>%
        rename(site = 1, timeEST = 2, temp = 3, gageHt = 4, hydroGraph = 5,
               notes = 6, fieldCode = 7) %>% 
        scan_for_typos(2, c(3, 4)) %>% 
        mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
               across(c(3, 4), as.numeric),
               date = d_date,
               fieldCode = as.character(fieldCode),
               notes = paste0(notes, addtl_comment),
               notes = sub('^NA -- ', '', notes)) %>% 
        relocate(date, .before = 'timeEST') %>% 
        mutate(flowGageHt = NA_real_)
    
    d_flow = field_code_handler(d_flow, 2)
    
    
    #chem ####
    
    d = readxl::read_xlsx(f,
                          sheet = 3, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    run_date_chem = as.Date(as.numeric(d[5, 3]), origin = '1899-12-30')
    initials_chem = d[[5, 6]]
    addtl_comment = d[[33, 3]]
    addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    
    d_precip_ = d[12:15, c(1:3, 6:10)] %>% 
        as_tibble() %>%
        rename(site = 1, date = 2, pHmetrohm = 3, pH = 4, spCond = 5, fieldCode = 6,
               notes = 7, archived = 8) %>% 
        scan_for_typos(3, 3:5) %>% 
        mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
               across(3:5, as.numeric),
               archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE))
    
    d_precip_ = field_code_handler(d_precip_, 3)
    
    d_precip = d_precip_ %>% 
        left_join(d_precip, by = c('site', 'date')) %>% 
        mutate(fieldCode = pmap(list(fieldCode.x, fieldCode.y), ~paste(union(...), collapse = ' ')),
               fieldCode = gsub('NA ?| ?NA', '', fieldCode),
               fieldCode = ifelse(! is.na(fieldCode) & fieldCode == '', NA_character_, fieldCode),
               notes = paste0(paste(notes.x, notes.y, sep = ' -- '),
                              addtl_comment),
               notes = sub('^NA -- ', '', notes)) %>% 
        select(-ends_with(c('.x', '.y'))) %>% 
        relocate(timeEST, .after = 'date')
    
    d_flow_ = d[18:32, c(1:4, 6:10)] %>%
        as_tibble() %>%
        rename(site = 1, date = 2, pHmetrohm = 3, ANCMet = 4, pH = 5,
               spCond = 6, fieldCode = 7, notes = 8, archived = 9) %>% 
        filter(if_any(everything(), ~! is.na(.)))
    
    sitenames_verbatim <- d_flow_$site
    
    d_flow_ <- d_flow_ %>% 
        mutate(site = case_when(grepl('sp', site, ignore.case = TRUE) ~ 'SP',
                                grepl('sw', site, ignore.case = TRUE) ~ 'SW',
                                TRUE ~ site),
               date = if_else(is.na(date) & site %in% c('SP', 'SW'),
                              unname(d[18, 2]),
                              date)) %>% 
        scan_for_typos(3, 3:6) %>% 
        mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
               across(3:6, as.numeric),
               archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE))
    
    out <- d_flow_[d_flow_$site %in% c('SP', 'SW'), ]
    out <- mutate(out, incorrect_date = date)
    
    sp_ind <- which(d_flow_$site == 'SP')
    if(length(sp_ind)){
        sw_ind <- which(d_flow_$site == 'SW')
        
        fix_datetime <- function(siten){
            dt_ <- sitenames_verbatim[d_flow_$site == siten]
            dt_elem <- str_extract(dt_, paste0(siten, '_([0-9]{6})_([0-9]{4})'),
                                   group = 1:2) %>% 
                matrix(ncol = 2)
            date_ <- str_replace(dt_,
                                 paste0(siten, '_(\\d{2})(\\d{2})(\\d{2})_'),
                                 '20\\1-\\2-\\3') %>% 
                str_sub(1, 10)
            # time_ <- str_replace(dt_,
            #                      paste0(siten, '_\\d{6}_(\\d{2})(\\d{2})'),
            #                      '\\1:\\2:00')
            
            return(list(date = date_, time = dt_elem[, 2]))
        }
        
        dt_sp <- fix_datetime('SP')
        d_flow_$date[sp_ind] <- dt_sp$date
        dt_sw <- fix_datetime('SW')
        d_flow_$date[sw_ind] <- dt_sw$date
    }
    
    d_flow_ = field_code_handler(d_flow_, 3)
    
    d_flow = d_flow_ %>% 
        left_join(d_flow, by = c('site', 'date')) %>% 
        mutate(fieldCode = pmap(list(fieldCode.x, fieldCode.y), ~paste(union(...), collapse = ' ')),
               fieldCode = gsub('NA ?| ?NA', '', fieldCode),
               fieldCode = ifelse(! is.na(fieldCode) & fieldCode == '', NA_character_, fieldCode),
               notes = paste0(paste(notes.x, notes.y, sep = ' -- '),
                              addtl_comment),
               notes = sub('^NA -- ', '', notes)) %>% 
        select(-ends_with(c('.x', '.y'))) %>% 
        relocate(timeEST, .after = 'date')
    
    if(length(sp_ind)){
        d_flow$timeEST[sp_ind] <- dt_sp$time
        d_flow$timeEST[sw_ind] <- dt_sw$time
    }
    
    if(any(is.na(out$pHmetrohm) | is.na(out$spCond))){
        warning(f)
    }
    
    d_flow = d_flow %>% 
        left_join(select(out, site, incorrect_date, pHmetrohm, spCond),
                  by = c('site', 'pHmetrohm', 'spCond'))
        
    # out %>% 
    #     select(-ANCMet, -pH, -fieldCode, -notes, -archived) %>% 
    #     left_join(d_flow, by = c('site', 'pHmetrohm', 'spCond')) %>% 
    #     rename(correct_date = date) %>% 
    
    #DIC ####
    
    d = readxl::read_xlsx(f,
                          sheet = 4, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    d_date = as.Date(as.numeric(d[1, 5]), origin = '1899-12-30')
    run_date_dic = as.Date(as.numeric(d[1, 7]), origin = '1899-12-30')
    
    d_flow = d[4:14, 1:2] %>% 
        as_tibble() %>% 
        rename(site = 1, DIC = 2) %>%
        scan_for_typos(4, 2) %>% 
        mutate(date = d_date,
               DIC = as.numeric(DIC),
               site = toupper(site),
               site = sitename_map$sheets123_fmt[match(site, sitename_map$sheets45_fmt)]) %>%
        full_join(d_flow, by = c('site', 'date')) %>% 
        relocate(DIC, .before = 'notes')
    
    #grab ####
    
    d = readxl::read_xlsx(f,
                          sheet = 5, col_names = FALSE, col_types = 'text') %>% 
        as.matrix()
    
    addtl_comment = d[[23, 2]]
    addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    
    if(! is.na(d[7, 3])){
        d_grab = d[7:15, 2:16] %>% 
            as_tibble() %>%
            select(site = 1, date = 2, timeEST = 3, pHmetrohm = 4, ANCMet = 5,
                   pH = 6, spCond = 8, DIC = 9, temp = 10, gageHt = 11, hydroGraph = 12,
                   fieldCode = 13, notes = 14, archived = 15) %>%
            scan_for_typos(5, 4:10) %>% 
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
        # d_flow$gageHt = NA_real_
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
        select(refNo, site, date, incorrect_date, timeEST, pH, pHmetrohm, DIC, spCond, temp,
               ANCMet, gageHt, hydroGraph, flowGageHt, precipCatch, fieldCode,
               notes, archived, uniqueID, waterYr, datetime) %>% 
        arrange(site, date, timeEST)
    
    #nvm, actually it should be set up like this:
    d = d %>% 
        mutate(HBEFLabDataSource = basename(f)) %>% 
        select(Site = site, SampleDate = date, incorrect_date, MilitaryTime = timeEST,
               Temperature = temp, GageHt = gageHt, HydroGraph = hydroGraph,
               precipCatch, phMet = pHmetrohm, ANCMet, `3StarpH` = pH,
               SpCond = spCond, DICRaw = DIC, Remarks = notes,
               Archived = archived, FieldCode = fieldCode, HBEFLabDataSource) %>% 
        mutate(Site = case_when(grepl('^WS[0-9]', Site) ~ sub('WS', 'W', Site),
                                grepl('^RG22', Site) ~ 'STA/22',
                                TRUE ~ Site),
               Archived = if_else(Archived, 'yes', ''),
               Remarks = if_else(Remarks == 'NA', NA_character_, Remarks),
               SampleDate = format(SampleDate, '%m/%d/%Y')) %>% 
        mutate(incorrect_date = format(incorrect_date, '%m/%d/%Y')) %>% 
        filter(Site %in% c('SP', 'SW'))
    
    sp_sw_corrections <- bind_rows(sp_sw_corrections, d)
    
    # 
    # #flow
    # d = readxl::read_xlsx(f,
    #                       sheet = 2, col_names = FALSE, col_types = 'text') %>% 
    #     as.matrix()
    # 
    # d_date = as.Date(as.numeric(d[3, 7]), origin = '1899-12-30')
    # initials_flow = d[[3, 9]]
    # addtl_comment = d[[20, 3]]
    # addtl_comment = ifelse(is.na(addtl_comment), '', paste(' --', addtl_comment))
    # 
    # d_flow = d[8:18, c(1:5, 8:9)] %>% 
    #     as_tibble() %>%
    #     rename(site = 1, timeEST = 2, temp = 3, gageHt = 4, hydroGraph = 5,
    #            notes = 6, fieldCode = 7) %>% 
    #     scan_for_typos(2, c(3, 4)) %>% 
    #     mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
    #            across(c(3, 4), as.numeric),
    #            date = d_date,
    #            fieldCode = as.character(fieldCode),
    #            notes = paste0(notes, addtl_comment),
    #            notes = sub('^NA -- ', '', notes)) %>% 
    #     relocate(date, .before = 'timeEST') %>% 
    #     mutate(flowGageHt = NA_real_)
    # 
    # d_flow = field_code_handler(d_flow, 2)
    # 
    # #chem
    # d = readxl::read_xlsx(f, sheet = 3, col_names = FALSE, col_types = 'text',
    #                       .name_repair = "unique_quiet") %>%
    #     as.matrix()
    # 
    # d_flow_ = d[18:32, c(1:4, 6:10)] %>%
    #     as_tibble() %>%
    #     rename(site = 1, date = 2, pHmetrohm = 3, ANCMet = 4, pH = 5,
    #            spCond = 6, fieldCode = 7, notes = 8, archived = 9) %>%
    #     filter(if_any(everything(), ~! is.na(.)))
    # 
    # sitenames_verbatim <- d_flow_$site
    # 
    # d_flow_ <- d_flow_ %>% 
    #     mutate(site = case_when(grepl('sp', site, ignore.case = TRUE) ~ 'SP',
    #                             grepl('sw', site, ignore.case = TRUE) ~ 'SW',
    #                             TRUE ~ site),
    #            date = if_else(is.na(date) & site %in% c('SP', 'SW'),
    #                           unname(d[18, 2]),
    #                           date)) %>% 
    #     scan_for_typos(3, 3:6) %>%
    #     mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
    #            across(3:6, as.numeric),
    #            archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE))
    # 
    # out <- d_flow_[d_flow_$site %in% c('SP', 'SW'), ]
    # out <- rename(out, incorrect_date = date)
    # 
    # sp_ind <- which(d_flow_$site == 'SP')
    # if(length(sp_ind)){
    #     sw_ind <- which(d_flow_$site == 'SW')
    #     
    #     fix_datetime <- function(siten){
    #         dt_ <- sitenames_verbatim[d_flow_$site == siten]
    #         dt_elem <- str_extract(dt_, paste0(siten, '_([0-9]{6})_([0-9]{4})'),
    #                                group = 1:2) %>% 
    #             matrix(ncol = 2)
    #         date_ <- str_replace(dt_,
    #                              paste0(siten, '_(\\d{2})(\\d{2})(\\d{2})_'),
    #                              '20\\1-\\2-\\3') %>% 
    #             str_sub(1, 10)
    #         # time_ <- str_replace(dt_,
    #         #                      paste0(siten, '_\\d{6}_(\\d{2})(\\d{2})'),
    #         #                      '\\1:\\2:00')
    #         
    #         return(list(date = date_, time = dt_elem[, 2]))
    #     }
    #     
    #     dt_sp <- fix_datetime('SP')
    #     d_flow_$date[sp_ind] <- dt_sp$date
    #     dt_sw <- fix_datetime('SW')
    #     d_flow_$date[sw_ind] <- dt_sw$date
    # }
    # 
    # d_flow_ = field_code_handler(d_flow_, 3)
    # 
    # d_flow = d_flow_ %>% 
    #     left_join(d_flow, by = c('site', 'date')) %>% 
    #     mutate(fieldCode = pmap(list(fieldCode.x, fieldCode.y), ~paste(union(...), collapse = ' ')),
    #            fieldCode = gsub('NA ?| ?NA', '', fieldCode),
    #            fieldCode = ifelse(! is.na(fieldCode) & fieldCode == '', NA_character_, fieldCode),
    #            notes = paste0(paste(notes.x, notes.y, sep = ' -- '),
    #                           addtl_comment),
    #            notes = sub('^NA -- ', '', notes)) %>% 
    #     select(-ends_with(c('.x', '.y'))) %>% 
    #     relocate(timeEST, .after = 'date')
    # 
    # if(length(sp_ind)){
    #     d_flow$timeEST[sp_ind] <- dt_sp$time
    #     d_flow$timeEST[sw_ind] <- dt_sw$time
    # }
    # 
    # if(any(is.na(out$pHmetrohm) | is.na(out$spCond)){
    #     stop('oi')
    # }
    # 
    # out %>% 
    #     select(-ANCMet, -pH, -fieldCode, -notes, -archived) %>% 
    #     left_join(d_flow, by = c('site', 'pHmetrohm', 'spCond')) %>% 
    #     rename(correct_date = date) %>% 
    #     # select(site, incorrect_date, correct_date, timeEST, ) %>% 
    #     # select(refNo, site, date, timeEST, pH, pHmetrohm, DIC, spCond, temp,
    #     #        ANCMet, gageHt, hydroGraph, flowGageHt, precipCatch, fieldCode,
    #     #        notes, archived, uniqueID, waterYr, datetime) %>% 
    #     select(Site = site, incorrect_date, correct_date, MilitaryTime = timeEST,
    #            Temperature = temp, GageHt = gageHt, HydroGraph = hydroGraph,
    #            precipCatch, phMet = pHmetrohm, ANCMet, `3StarpH` = pH,
    #            SpCond = spCond, DICRaw = DIC, Remarks = notes,
    #            Archived = archived, FieldCode = fieldCode, HBEFLabDataSource)
        
    
    # cat(f)
    # cat('\n')
    # cat(grep('S[PW]', d_flow_$site, value = T), sep = '\n')
    # readLines(n=1)
}
sp_sw_corrections = rename(sp_sw_corrections, correct_date = SampleDate)
write_csv(sp_sw_corrections, '/tmp/sp_sw_datetime_corrections.csv')
