library(RMariaDB)
library(DBI)
library(tidyverse)

setwd('~/shiny/restricted_QAQC/data/manual_upload')
pass=readLines('/home/mike/RMySQL.config')

defClasses <- read.csv("../Rclasses.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample <- read.csv("../RclassesSample.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
defClassesSample$date <- as.Date(defClassesSample$date, "%m/%d/%y")

fs = list.files('.')

con = dbConnect(MariaDB(),
                user = 'root',
                password = pass,
                host = 'localhost',
                dbname = 'hbef')
                # dbname = 'hbef20200415')

standardizeClasses <- function(d) {
    r <- nrow(d)
    c <- ncol(d)
    for (i in 1:c) {
        current_col_ofData <- colnames(d[i])
        ind_col <- which(current_col_ofData == colnames(defClassesSample), arr.ind = TRUE)
        d[r+1,i] <- defClassesSample[1,ind_col]
        ind_row <- which(current_col_ofData == defClasses$VariableName, arr.ind = TRUE)
        switch(defClasses$Class[ind_row],
               integer=as.integer(d[[i]]),
               character=as.character(d[[i]]),
               numeric=as.numeric(d[[i]]),
               Date=as.Date(d[[i]]), #, origin='1970-01-01'
               factor=as.factor(d[[i]])
        )
    }
    d <- d[-(r+1),]
    d
}

for(f in fs){

    print(f)

    dataNew <-read.csv(f, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
    dataNew <- dataNew[rowSums(is.na(dataNew)) !=ncol(dataNew),] # remove rows with all NA's
    if ("date" %in% names(dataNew)) {
        dataNew$date <- as.Date(dataNew$date, "%m/%d/%y")
    }

    # make needed data type changes to data before uploading
    dataNew <- standardizeClasses(dataNew)
    dataNew = dataNew %>%
        group_by(uniqueID) %>%
        summarize_each(list(~if(is.numeric(.)) mean(., na.rm=TRUE) else first(.))) %>%
        ungroup()

    uid = unname(unlist(dbGetQuery(con, 'select uniqueID from historical;')))
    nrecords_submit = nrow(dataNew)
    dataNew = dataNew[! dataNew$uniqueID %in% uid, ]
    nomits = nrecords_submit - nrow(dataNew)
    nsubmitted = nrecords_submit - nomits
    dbWriteTable(con, "historical", dataNew, append=TRUE, row.names=FALSE)

    # print(paste("Submit Complete. Omitted", nomits, "already held uniqueIDs."))
    print(paste(nsubmitted, "submitted"))
}

dbDisconnect(con)
