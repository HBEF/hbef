merge_sensor_data = function(d, svar, ssite, sdate){
   # input = list(SENSORVAR1='Nitrate_mg', SITES1='W3', WATERYEAR1='2016', SOLUTES1='Ca')
   # print(input$SENSORVAR4)
   SENSORVAR4_S4 = paste('S4', svar, sep='__')
   # print(SENSORVAR4_S4)
   # yrstart = as.POSIXct(paste0(input$WATERYEAR, '-06-01'))
   # yrend = as.POSIXct(paste0(as.numeric(input$WATERYEAR1) + 1, '-05-31'))

   y = RMariaDB::MariaDB()
   con = dbConnect(y, user='root', password=pass, host='localhost',
      # dbname='hbef')
      dbname='hbef20200415')

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

   SENSORVAR_S4 = paste('S4', svar, sep='__')

   y = RMariaDB::MariaDB()
   con = dbConnect(y, user='root', password=pass, host='localhost',
      # dbname='hbef')
      dbname='hbef20200415')

   res = dbSendQuery(con, paste0("select datetime, ", SENSORVAR_S4,
      " from sensor4 WHERE watershedID = '",
      substr(ssite, 2, nchar(ssite)), "' and datetime >= '",
      sdate[1], "' and datetime <= '", sdate[2], "';"))
   dsens = dbFetch(res)

   colnames(dsens) = gsub('S4__', '', colnames(dsens))

   if(svar == 'Nitrate_mg'){
       dsens = mutate(dsens, NO3_N_mg=NO3_to_NO3N(Nitrate_mg)) %>%
           select(-Nitrate_mg)
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

# abs.Date = function(x){x}
