library(readxl)
library(dplyr)
library(stringr)

setwd('~/git/hbef/shiny/restricted_QAQC/field_and_lab_note_collections/')

notefile = '20230709.xlsx'

sitename_map = tibble(
    sheets123_fmt = c('RG-22', 'RG-1', 'RG-4', 'RG-23',
                      'WS-1', 'WS-2', 'WS-3', 'WS-4', 'WS-5', 'WS-6', 'WS-7',
                      'WS-8', 'WS-9', 'HBK', 'ML70'),
    sheets45_fmt = c('RG-22', 'RG-1', 'RG-4', 'RG-23',
                     'W-1', 'W-2', 'W-3', 'W-4', 'W-5', 'W-6', 'W-7',
                     'W-8', 'W-9', 'HBk', 'ML-70'),
    desired_fmt = c('RG22', 'RG1', 'RG4', 'RG23', #what is RG4?
                    'W1', 'W2', 'W3', 'W4', 'W5', 'W6', 'W7',
                    'W8', 'W9', 'HBK', 'ML70')
)

#precip ####

d = readxl::read_xlsx(notefile,
                      sheet = 1, col_names = FALSE, col_types = 'text') %>% 
    as.matrix()

d_date = as.Date(as.numeric(d[5, 4]), origin = '1899-12-30')
initials_precip = d[[5, 6]]

d_precip = d[11:14, 1:4] %>% 
    as_tibble() %>%
    bind_cols(d[20:23, 8]) %>% 
    rename(site = 1, timeEST = 2, volume_ml = 3, depth_mm = 4, #one of these is precipCatch. edi was down when i checked
           fieldCode = 5) %>%
    mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
           volume_ml = as.numeric(volume_ml),
           depth_mm = as.numeric(depth_mm),
           date = d_date,
           fieldCode = as.character(fieldCode)) %>% 
    relocate(date, .before = 'timeEST')

#flow ####

d = readxl::read_xlsx(notefile,
                      sheet = 2, col_names = FALSE, col_types = 'text') %>% 
    as.matrix()

d_date = as.Date(as.numeric(d[3, 7]), origin = '1899-12-30')
initials_flow = d[[3, 9]]

d_flow = d[8:18, 1:9] %>% 
    as_tibble() %>%
    rename(site = 1, timeEST = 2, temp = 3, flowGageHt = 4, hydroGraph = 5,
           color = 6, sediment = 7, notes = 8, fieldCode = 9) %>% 
    mutate(timeEST = str_pad(timeEST, 4, 'left', '0'),
           across(c(3, 4, 7), as.numeric),
           date = d_date) %>% 
    relocate(date, .before = 'timeEST')

#chem ####

d = readxl::read_xlsx(notefile,
                      sheet = 3, col_names = FALSE, col_types = 'text') %>% 
    as.matrix()

run_date_chem = as.Date(as.numeric(d[5, 3]), origin = '1899-12-30')
initials_chem = d[[5, 6]]

d_precip = d[12:15, c(1:3, 6:9)] %>% 
    as_tibble() %>%
    rename(site = 1, date = 2, pHmetrohm = 3, pH = 4, spCond = 5, fieldCode = 6,
           archived = 7) %>% 
    mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(3:5, as.numeric),
           archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE)) %>% 
    left_join(d_precip, by = c('site', 'date')) %>% 
    mutate(fieldCode = paste(fieldCode.x, fieldCode.y),
           fieldCode = gsub('NA ?| ?NA', '', fieldCode),
           fieldCode = ifelse(! is.na(fieldCode) & fieldCode == '', NA_character_, fieldCode)) %>% 
    select(-ends_with(c('.x', '.y'))) %>% 
    relocate(timeEST, .after = 'date')

d_flow = d[18:28, c(1:9)] %>% 
    as_tibble() %>%
    rename(site = 1, date = 2, pHmetrohm = 3, ANCMet = 4, ANC960 = 5, pH = 6,
           spCond = 7, fieldCode = 8, archived = 9) %>% 
    mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(3:7, as.numeric),
           archived = ifelse(toupper(archived) == 'Y', TRUE, FALSE)) %>% 
    left_join(d_flow, by = c('site', 'date')) %>% 
    mutate(fieldCode = paste(fieldCode.x, fieldCode.y),
           fieldCode = gsub('NA ?| ?NA', '', fieldCode),
           fieldCode = ifelse(! is.na(fieldCode) & fieldCode == '', NA_character_, fieldCode)) %>% 
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
           site = sitename_map$sheets123_fmt[match(site, sitename_map$sheets45_fmt)]) %>%
    left_join(d_flow, by = c('site', 'date')) %>% 
    relocate(DIC, .before = 'notes')

#grab ####
