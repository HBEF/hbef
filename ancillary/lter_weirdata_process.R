library(plyr)
library(stringr)

setwd('/home/lter/data')
weirfiles = list.files('.', pattern='weir')

out = data.frame()
for(i in 1:length(weirfiles)){

    watershed_id = str_match(weirfiles[i], 'weir([0-9]+)_')[,2]
    x = read.csv(weirfiles[i], skip=1, stringsAsFactors=FALSE)
    x = x[-c(1:2), c('TIMESTAMP', 'Q')]
    colnames(x)[2] = 'Q_Ls'
    x$Q_Ls = as.numeric(x$Q_Ls) * 28.31685
    x = x[! is.na(x$Q_Ls),]
    x['watershed_id'] = watershed_id
    out = rbind.fill(out, x)
}

out = out[order(out$watershed_id, out$TIMESTAMP),]
write.csv(out, '/var/lib/mysql-files/lter_Qdata.csv', row.names=TRUE)
