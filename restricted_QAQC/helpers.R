merge_sensor_data = function(d, svar, ssite, sdate){
   # input = list(SENSORVAR1='Nitrate_mg', SITES1='W3', WATERYEAR1='2016', SOLUTES1='Ca')
   # print(input$SENSORVAR4)

   SENSORVAR4_S4 = paste('S4', svar, sep='__')
   # print(SENSORVAR4_S4)
   # yrstart = as.POSIXct(paste0(input$WATERYEAR, '-06-01'))
   # yrend = as.POSIXct(paste0(as.numeric(input$WATERYEAR1) + 1, '-05-31'))

   #y = RMySQL::MySQL()
   y = RMariaDB::MariaDB()
   con = dbConnect(y, user='root', password=pass, host='localhost',
      dbname=dbname)

   res = dbSendQuery(con, paste0("select datetime, ", SENSORVAR4_S4,
      " from sensor4 WHERE watershedID = '",
      substr(ssite, 2, nchar(ssite)), "' and datetime > '",
      sdate[1], "' and datetime < '", sdate[2], "';"))
   dsens = dbFetch(res)

   dbClearResult(res)
   dbDisconnect(con)

   d = mutate(d, date=with_tz(as.POSIXct(date), 'UTC'))
   d = dsens %>%
      full_join(d, by=c('datetime'='date')) %>%
      rename(date=datetime, sensorvar=SENSORVAR4_S4) %>%

      # filter(waterYr %in% input$WATERYEAR1) %>%
      # filter(site %in% input$SITES1) %>%
      # select(-flowGageHt) %>%
      # mutate(datetime=as.POSIXct(paste(as.character(date),
      #    as.character(timeEST)))) %>%
      # full_join(datasensor4, by=c('datetime'='datetime')) %>%
      # select(-date) %>%
      # rename(date=datetime) %>%
      # select(one_of("date", input$SOLUTES1, SENSORVAR1_S4))
   # colnames(dataAllQ1)[which(colnames(dataAllQ1) == SENSORVAR1_S4)] = input$SENSORVAR1
   return(d)
}

# svar=sv; ssite=ss; sdate=dd
get_sensor_data = function(svar, ssite, sdate){#, placeholder1, placeholder2){

   if(svar == 'NO3_N_mg') svar = 'Nitrate_mg'

   #y = RMySQL::MySQL()
   y = RMariaDB::MariaDB()
   con = dbConnect(y, user='root', password=pass, host='localhost',
      dbname=dbname)

   if(svar == 'Light_lux'){
       res = dbSendQuery(con, paste0("select datetime, light_lux",
          " from sensor5_light WHERE site = '",
          ssite, "' and location = 'WEIR' and datetime >= '",
          sdate[1], "' and datetime <= '", sdate[2], "';"))
   # } else if(svar == 'Temp_C'){
   #     res = dbSendQuery(con, paste0("select datetime, temp_C",
   #        " from sensor6_temp WHERE site = '",
   #        ssite, "' and datetime >= '",
   #        sdate[1], "' and datetime <= '", sdate[2], "';"))
   } else {
       SENSORVAR_S4 = paste('S4', svar, sep='__')
       res = dbSendQuery(con, paste0("select datetime, ", SENSORVAR_S4,
          " from sensor4 WHERE watershedID = '",
          substr(ssite, 2, nchar(ssite)), "' and datetime >= '",
          sdate[1], "' and datetime <= '", sdate[2], "';"))
   }
   
   dsens = dbFetch(res)

   colnames(dsens) = gsub('S4__', '', colnames(dsens))

   if(svar == 'Nitrate_mg'){
       dsens = dsens %>% 
           slice(seq(1, n(), 8)) %>% 
           mutate(NO3_N_mg=NO3_to_NO3N(Nitrate_mg)) %>%
           select(-Nitrate_mg)
   }
   
   if(svar == 'TurbidityRaw'){
      dsens = slice(dsens, seq(1, n(), 8))
   }

   dbClearResult(res)
   dbDisconnect(con)

   return(dsens)
}

plot_empty_dygraph = function(datelims, plotgroup, ylab){

   datelims = as.POSIXct(datelims)
   dateseq = seq(datelims[1], datelims[2], by='day')
   emptydat = xts(rep(0, length.out=length(dateseq)),
      order.by=dateseq, tzone='UTC')
   dg = dygraph(emptydat, group=plotgroup) %>%
      dyOptions(useDataTimezone=TRUE, drawPoints=FALSE,
         colors='transparent', retainDateWindow=TRUE) %>%
      dyAxis('y', label=ylab, labelWidth=16, labelHeight=10, rangePad=10)

   return(dg)
}

pad_ts = function(tsdf, datebounds){

   datebounds = as.POSIXct(datebounds)
   row1 = data.frame(datebounds[1], NA)
   colnames(row1) = colnames(tsdf)
   rown = data.frame(datebounds[2], NA)
   colnames(rown) = colnames(tsdf)
   df_padded = bind_rows(row1, tsdf, rown)

   return(df_padded)
}

parse_molecular_formulae = function(formulae){

    #`formulae` is a vector

    # formulae = c('C', 'C4', 'Cl', 'Cl2', 'CCl', 'C2Cl', 'C2Cl2', 'C2Cl2B2')
    # formulae = 'BCH10He10PLi2'
    # formulae='Mn'

    conc_vars = str_match(formulae, '^(?:OM|TM|DO|TD|UT|UTK|TK)?([A-Za-z0-9]+)_?')[,2]
    two_let_symb_num = str_extract_all(conc_vars, '([A-Z][a-z][0-9]+)')
    conc_vars = str_remove_all(conc_vars, '([A-Z][a-z][0-9]+)')
    one_let_symb_num = str_extract_all(conc_vars, '([A-Z][0-9]+)')
    conc_vars = str_remove_all(conc_vars, '([A-Z][0-9]+)')
    two_let_symb = str_extract_all(conc_vars, '([A-Z][a-z])')
    conc_vars = str_remove_all(conc_vars, '([A-Z][a-z])')
    one_let_symb = str_extract_all(conc_vars, '([A-Z])')

    constituents = mapply(c, SIMPLIFY=FALSE,
        two_let_symb_num, one_let_symb_num, two_let_symb, one_let_symb)

    return(constituents) # a list of vectors
}

combine_atomic_masses = function(molecular_constituents){

    #`molecular_constituents` is a vector

    xmat = str_match(molecular_constituents,
        '([A-Z][a-z]?)([0-9]+)?')[, -1, drop=FALSE]
    elems = xmat[,1]
    mults = as.numeric(xmat[,2])
    mults[is.na(mults)] = 1
    molecular_mass = sum(PeriodicTable::mass(elems) * mults)

    return(molecular_mass) #a scalar
}

NO3_to_NO3N = function(no3_mg){
    no3_mg * 14.0067 / 62.0049
}

NH4_to_NH4N = function(nh4_mg){
    nh4_mg * 14.0067 / 18.03846
}

NO3N_to_NO3 = function(no3n_mg){
    no3n_mg * 62.0049 / 14.0067
}

NH4N_to_NH4 = function(nh4n_mg){
    nh4n_mg * 18.03846 / 14.0067
}

# abs.Date = function(x){x}

email_msg <- function(subject, text_body, addr, pw){
    
    mailout = tryCatch({
            
        email = emayili::envelope() %>%
            emayili::from('grdouser@gmail.com') %>%
            emayili::to(addr) %>%
            emayili::subject(subject) %>%
            emayili::text(text_body)
        
        smtp = emayili::server(host='smtp.gmail.com',
                               port=587, #or 465 for SMTPS
                               username='grdouser@gmail.com',
                               password=pw)
        
        smtp(email, verbose=FALSE)
        
    }, error=function(e){
        
        #not sure if class "error" is always returned by tryCatch,
        #so creating custom class
        errout = 'err'
        class(errout) = 'err'
        return(errout)
    })
    
    if('err' %in% class(mailout)){
        writeLines(paste('failed to email', addr, 'on', Sys.time()), '../logs/email_jeff.log')
    }
}

prep_stickytrap_data = function(input, graphnum){
  
  iwyr = input[[paste0('WATERYEAR', graphnum)]]
  idate = input[[paste0('DATE', graphnum)]]
  isite = input[[paste0('SITES', graphnum)]]
  ibug = intersect(input[[paste0('SOLUTES', graphnum)]], emergence)
  
  con = dbConnect(MariaDB(),
                  user = 'root',
                  password = pass,
                  host = 'localhost',
                  dbname = dbname)
  stky = as_tibble(dbReadTable(con, 'stickytrap'))
  dbDisconnect(con)
  
  stky = stky %>%
    rename(site = watershed) %>%
    mutate(waterYr = if_else(month(date) %in% 1:5, year(date) + 1, year(date)),
           site = paste0('W', site))
  
  date_unchanged = is.null(idate) || (idate[2] == maxDate_current && idate[1] == maxDate_current-365)
  wyear_unchanged = is.null(iwyr) || iwyr == 1963
  if(date_unchanged && wyear_unchanged){
    stky = filter(stky, waterYr %in% iwyr)
  } else if(wyear_unchanged) {
    stky = filter(stky, date >= idate[1] & date <= idate[2])
  } else {
    stky = filter(stky, waterYr %in% iwyr)
  }
  
  maybe_site = ifelse(graphnum == 4, 'site', 'not gonna match')
  
  stky = stky %>% 
    filter(site %in% isite) %>%
    select(date, starts_with(ibug), any_of(maybe_site)) %>% 
    rowwise()
  
  for(b in ibug){
    stky = mutate(stky, !!b := sum(c_across(starts_with(b)), na.rm = TRUE))
  }
  
  stky = ungroup(stky) %>% 
    select(date, !!ibug, any_of(maybe_site))
  
  return(stky)
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
    
    d = readxl::read_xlsx(notefile,
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
    
    d = readxl::read_xlsx(notefile,
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
    
    #DIC ####
    
    d = readxl::read_xlsx(notefile,
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
    
    d = readxl::read_xlsx(notefile,
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
        select(refNo, site, date, timeEST, pH, pHmetrohm, DIC, spCond, temp,
               ANCMet, gageHt, hydroGraph, flowGageHt, precipCatch, fieldCode,
               notes, archived, uniqueID, waterYr, datetime) %>% 
        arrange(site, date, timeEST)
    
    #nvm, actually it should be set up like this:
    d = d %>% 
        mutate(HBEFLabDataSource = basename(notefile)) %>% 
        select(Site = site, SampleDate = date, MilitaryTime = timeEST,
               Temperature = temp, GageHt = gageHt, HydroGraph = hydroGraph,
               precipCatch, phMet = pHmetrohm, ANCMet, `3StarpH` = pH,
               SpCond = spCond, DICRaw = DIC, Remarks = notes,
               Archived = archived, FieldCode = fieldCode, HBEFLabDataSource) %>% 
        mutate(Site = case_when(grepl('^WS[0-9]', Site) ~ sub('WS', 'W', Site),
                                grepl('^RG22', Site) ~ 'STA/22',
                                TRUE ~ Site),
               Archived = if_else(Archived, 'yes', ''),
               Remarks = if_else(Remarks == 'NA', NA_character_, Remarks),
               SampleDate = format(SampleDate, '%m/%d/%Y'))
               
    allowed_fieldcodes = as.character(c(319, 888, 905, 907, 920, 911, 912, 955, 960, 966, 969, 970))
    fc_split = strsplit(d$FieldCode, ' ')
    illegal_codes = lapply(fc_split, function(x){
        x[! is.na(x) & ! x %in% allowed_fieldcodes]
    }) %>% 
        unlist()
    if(length(illegal_codes)){
        stop('Unrecognized fieldCodes:\n', paste(illegal_codes, collapse = '; '))
    }
    
    return(d)
}

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

email_data <- function(df, orig_file, orig_name, msgs, addrs, pw){
    
    #fails on server because emayili renders the smtp server address as
    #smtp://smtp.gmail.com:587/ instead of just smtp.gmail.com, so
    #the guts of emayili are reused in email_data2()
    
    tmpcsv = tempfile(fileext = ".csv")
    write.csv(df, tmpcsv, row.names = FALSE, na = '')
    
    new_name = sub('xlsx', 'csv', orig_name, ignore.case = TRUE)
    if(length(new_name) > 1){
        name_range = sort(new_name)
        new_name = paste(sub('\\.csv', '', name_range[1]),
                         new_name[length(new_name)],
                         sep = '-')
    }
    
    if(is.list(msgs)){
        msgs = Reduce(function(x, y) paste(x, y, sep='\n---\n'), msgs)
    }
        
    for(a in addrs){
        
        email = emayili::envelope() %>%
            emayili::from('grdouser@gmail.com') %>%
            emayili::to(a) %>%
            emayili::subject('HBEF data from Tammy') %>%
            emayili::text(msgs) %>% 
            emayili::attachment(tmpcsv, name = new_name)
        
        for(att in orig_file){
            email = email %>% 
                emayili::attachment(att, name = att,
                                    type = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
        }
        
        smtp = emayili::server(host='smtp.gmail.com',
                               port=587, #or 465 for SMTPS
                               username='grdouser@gmail.com',
                               password=pw)
        
        smtp(email, verbose=FALSE)
    }
    
    file.remove(tmpcsv)
}

email_data2 <- function(df, orig_file, orig_name, msgs, addrs, pw){
    
    tmpcsv = tempfile(fileext = ".csv")
    write.csv(df, tmpcsv, row.names = FALSE, na = '')
    
    new_name = sub('xlsx', 'csv', orig_name, ignore.case = TRUE)
    if(length(new_name) > 1){
        name_range = sort(new_name)
        new_name = paste(sub('\\.csv', '', name_range[1]),
                         new_name[length(new_name)],
                         sep = '-')
    }
    
    if(is.list(msgs)){
        msgs = Reduce(function(x, y) paste(x, y, sep='\n---\n'), msgs)
    }
        
    for(a in addrs){
        
        email = emayili::envelope() %>%
            emayili::from('grdouser@gmail.com') %>%
            emayili::to(a) %>%
            emayili::subject('HBEF data from Tammy') %>%
            emayili::text(msgs) %>% 
            emayili::attachment(tmpcsv, name = new_name)
        
        for(att in orig_file){
            email = emayili::attachment(email, att, name = basename(att),
                                        type = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
        }
        
        host = 'smtp.gmail.com'
        port = 587 #or 465 for SMTPS
        username = 'grdouser@gmail.com'
        password = pw
        
        insecure = FALSE
        reuse = TRUE
        helo = NA
        protocol = NA
        test = FALSE
        pause_base = 1
        max_times = 1
        verbose = FALSE
        
        ssl_verifypeer = TRUE
        
        port <- as.integer(port)
        if (port %in% c(465, 587)) {
            use_ssl <- 1
        } else {
            use_ssl <- 0
        }
        
        # smtp_server <- emayili:::smtp_url(host, port, protocol, helo)
        smtp_server <- host
        
        debugfunction <- if (verbose) function(type, email) cat(readBin(email, character()), file = stderr()) # nocov
        
        recipients <- c(emayili::to(email), emayili::cc(email), emayili::bcc(email))
        
        curl::send_mail(
            mail_from = emayili::raw(emayili::from(email)),
            mail_rcpt = emayili::raw(recipients),
            message = as.character(email, encode = TRUE),
            smtp_server = smtp_server,
            username = username,
            password = password,
            verbose = verbose,
            debugfunction = debugfunction,
            ssl_verifypeer = ssl_verifypeer,
            use_ssl = use_ssl,
            forbid_reuse = !reuse
        )
    }
    
    file.remove(tmpcsv)
}
