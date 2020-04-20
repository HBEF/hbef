merge_sensor_data = function(d, svar, ssite, sdate){
   # input = list(SENSORVAR1='Nitrate_mg', SITES1='W3', WATERYEAR1='2016', SOLUTES1='Ca')
   # print(input$SENSORVAR4)
   SENSORVAR4_S4 = paste('S4', svar, sep='__')
   # print(SENSORVAR4_S4)
   # yrstart = as.POSIXct(paste0(input$WATERYEAR, '-06-01'))
   # yrend = as.POSIXct(paste0(as.numeric(input$WATERYEAR1) + 1, '-05-31'))

   y = RMariaDB::MariaDB()
   con = dbConnect(y, user='root', password=pass, host='localhost',
      dbname='hbef')

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

get_sensor_data = function(svar, ssite, sdate){#, placeholder1, placeholder2){

   SENSORVAR4_S4 = paste('S4', svar, sep='__')

   y = RMariaDB::MariaDB()
   con = dbConnect(y, user='root', password=pass, host='localhost',
      dbname='hbef')

   res = dbSendQuery(con, paste0("select datetime, ", SENSORVAR4_S4,
      " from sensor4 WHERE watershedID = '",
      substr(ssite, 2, nchar(ssite)), "' and datetime >= '",
      sdate[1], "' and datetime <= '", sdate[2], "';"))
   dsens = dbFetch(res)

   colnames(dsens) = gsub('S4__', '', colnames(dsens))

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

# abs.Date = function(x){x}
