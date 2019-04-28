library(plyr)
library(dplyr)
library(stringr)
library(lubridate)

setwd('/home/lter/data')
weirfiles = list.files('.', pattern='weir')

recent = data.frame()
for(i in 1:length(weirfiles)){
    watershed_id = str_match(weirfiles[i], 'weir([0-9]+)_')[,2]
    x = read.csv(weirfiles[i], skip=1, stringsAsFactors=FALSE)
    x = x[-c(1:2), c('TIMESTAMP', 'Q')]
    colnames(x)[1:2] = c('datetime', 'Q_Ls')
    x$Q_Ls = as.numeric(x$Q_Ls) * 28.31685
    x = x[! is.na(x$Q_Ls),]
    x['watershed_id'] = watershed_id
    x = mutate(x, datetime=as.POSIXct(datetime))
    recent = rbind.fill(recent, x)
}

setwd('/home/lter/data/lter_historical')
histfiles = list.files('.', pattern='stmflow')

hist = data.frame()
for(i in 1:length(histfiles)){
   watershed_id = str_match(histfiles[i], 'w([0-9]+)_')[,2]
   x = read.csv(histfiles[i], stringsAsFactors=FALSE)
   x = select(x, datetime=DATETIME, Q_Ls=Discharge_ls, watershed_id=WS) %>%
      mutate(datetime=as.POSIXct(datetime))
   if(grepl('5min', histfiles[i])){
      x = filter(x, minute(datetime) %% 30 == 0)
   }
   x = x[! is.na(x$Q_Ls),]
   hist = rbind.fill(hist, x)
}

out = rbind.fill(recent, hist)
out = out[order(out$watershed_id, out$datetime),]
write.csv(out, '/var/lib/mysql-files/lter_Qdata.csv', row.names=TRUE)
