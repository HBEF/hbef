
#Cleaning vegetation data######################

#load data
fine_litter_data <- readRDS("fine_litter_data.rds")

#Replacing missing data values with NA
fine_litter_data[fine_litter_data == -9999] <- NA
fine_litter_data[fine_litter_data == -9999.9] <- NA
fine_litter_data[fine_litter_data == -9999.99] <- NA

#Average all leaf count columns by year
by_year <- fine_litter_data %>% group_by(YEAR, SITE)
sugarm_count_mean<-by_year %>% summarise(sugarm_count_mean = mean(M_COUNT, na.rm=T))
redm_count_mean<-by_year %>% summarise(redm_count_mean = mean(f_COUNT, na.rm=T))
stripedm_count_mean<-by_year %>% summarise(stripedm_count_mean = mean(t_COUNT, na.rm=T))
ash_count_mean<-by_year %>% summarise(ash_count_mean = mean(Q_COUNT, na.rm=T))
beech_count_mean<-by_year %>% summarise(beech_count_mean = mean(B_COUNT, na.rm=T))
whiteb_count_mean<-by_year %>% summarise(whiteb_count_mean = mean(W_COUNT, na.rm=T))
yellowb_count_mean<-by_year %>% summarise(yellowb_count_mean = mean(Y_COUNT, na.rm=T))
pcherry_count_mean<-by_year %>% summarise(pcherry_count_mean = mean(P_COUNT, na.rm=T))
aspen_count_mean<-by_year %>% summarise(aspen_count_mean = mean(a_COUNT, na.rm=T))

#paste a new column called species in each df
sugarm_count_mean$species <- rep("Sugar Maple", nrow(sugarm_count_mean))
redm_count_mean$species <- rep("Red Maple", nrow(redm_count_mean))
stripedm_count_mean$species <- rep("Striped Maple", nrow(stripedm_count_mean))
ash_count_mean$species <- rep("Ash", nrow(ash_count_mean))
beech_count_mean$species <- rep("Beech", nrow(beech_count_mean))
whiteb_count_mean$species <- rep("White Birch", nrow(whiteb_count_mean))
yellowb_count_mean$species <- rep("Yellow Birch", nrow(yellowb_count_mean))
pcherry_count_mean$species <- rep("Pin Cherry", nrow(pcherry_count_mean))
aspen_count_mean$species <- rep("Aspen", nrow(aspen_count_mean))

#rename columns to be just "count" in order to merge into long df rather than wide
sugarm_count_mean<-plyr::rename(sugarm_count_mean, c("sugarm_count_mean"="count"))
redm_count_mean<-plyr::rename(redm_count_mean, c("redm_count_mean"="count"))
stripedm_count_mean<-plyr::rename(stripedm_count_mean, c("stripedm_count_mean"="count"))
ash_count_mean<-plyr::rename(ash_count_mean, c("ash_count_mean"="count"))
beech_count_mean<-plyr::rename(beech_count_mean, c("beech_count_mean"="count"))
whiteb_count_mean<-plyr::rename(whiteb_count_mean, c("whiteb_count_mean"="count"))
yellowb_count_mean<-plyr::rename(yellowb_count_mean, c("yellowb_count_mean"="count"))
pcherry_count_mean<-plyr::rename(pcherry_count_mean, c("pcherry_count_mean"="count"))
aspen_count_mean<-plyr::rename(aspen_count_mean, c("aspen_count_mean"="count"))

#Merge all count column averages into one df with three total columns (year, species, count)
count_means <- Reduce(function(...) merge(..., all=T), 
                      list(sugarm_count_mean, redm_count_mean, stripedm_count_mean, 
                           ash_count_mean, beech_count_mean, whiteb_count_mean, 
                           yellowb_count_mean, pcherry_count_mean, aspen_count_mean))

#save as RDS file
saveRDS(count_means, file="yearly_count_means.rds")
yearly_count_means <- readRDS("yearly_count_means.rds")
#End vegetation data cleaning##########################

#Normalization of flux by subtracting ws6 values as a baseline##################
library(magrittr)
library(dplyr)
library(reshape2)

#load data
load("precip_streamflow_dfs.RData")
imported_data <- precip_streamflow_long

#YEARLY
ws_cast_year <- imported_data %>%
  filter(granularity=='year') %>% 
  filter(source=='streamflow') %>%
  filter(solute=='NO3') %>%
  dcast(.,date+water_year+solute~ws,value.var='flux')

#normalize data by subtracting ws6 from each
ws_cast_year$"n2" <- ws_cast_year$"2"-ws_cast_year$"6"
ws_cast_year$"n4" <- ws_cast_year$"4"-ws_cast_year$"6"
ws_cast_year$"n5" <- ws_cast_year$"5"-ws_cast_year$"6"

#rename ws columns so I can name normalized columns just numbers to make merging easier
names(ws_cast_year)[names(ws_cast_year) == "2"] <- "ws2"
names(ws_cast_year)[names(ws_cast_year) == "4"] <- "ws4"
names(ws_cast_year)[names(ws_cast_year) == "5"] <- "ws5"
names(ws_cast_year)[names(ws_cast_year) == "n2"] <- "2"
names(ws_cast_year)[names(ws_cast_year) == "n4"] <- "4"
names(ws_cast_year)[names(ws_cast_year) == "n5"] <- "5"

#melt function to get them all back together (new tidyr version is spread)
ws_cast_year <- melt(ws_cast_year, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
                     variable.name = "ws", value.name = "normalized_flux")

#add granularity column with "year"
ws_cast_year$granularity <- rep("year", nrow(ws_cast_year))

#repeat for month and week
#MONTHLY
ws_cast_month <- imported_data %>%
  filter(granularity=='month') %>% 
  filter(source=='streamflow') %>%
  filter(solute=='NO3') %>%
  dcast(.,date+water_year+solute~ws,value.var='flux')

#normalize data by subtracting ws6 from each
ws_cast_month$"n2" <- ws_cast_month$"2"-ws_cast_month$"6"
ws_cast_month$"n4" <- ws_cast_month$"4"-ws_cast_month$"6"
ws_cast_month$"n5" <- ws_cast_month$"5"-ws_cast_month$"6"

#rename ws columns so I can name normalized columns just numbers to make merging easier
names(ws_cast_month)[names(ws_cast_month) == "2"] <- "ws2"
names(ws_cast_month)[names(ws_cast_month) == "4"] <- "ws4"
names(ws_cast_month)[names(ws_cast_month) == "5"] <- "ws5"
names(ws_cast_month)[names(ws_cast_month) == "n2"] <- "2"
names(ws_cast_month)[names(ws_cast_month) == "n4"] <- "4"
names(ws_cast_month)[names(ws_cast_month) == "n5"] <- "5"

#melt function to get them all back together (new tidyr version is spread)
ws_cast_month <- melt(ws_cast_month, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
                      variable.name = "ws", value.name = "normalized_flux")

#add granularity column with "month"
ws_cast_month$granularity <- rep("month", nrow(ws_cast_month))

#WEEKLY
ws_cast_week <- imported_data %>%
  filter(granularity=='week') %>% 
  filter(source=='streamflow') %>%
  filter(solute=='NO3') %>%
  dcast(.,date+water_year+solute~ws,value.var='flux')

#normalize data by subtracting ws6 from each
ws_cast_week$"n2" <- ws_cast_week$"2"-ws_cast_week$"6"
ws_cast_week$"n4" <- ws_cast_week$"4"-ws_cast_week$"6"
ws_cast_week$"n5" <- ws_cast_week$"5"-ws_cast_week$"6"

#rename ws columns so I can name normalized columns just numbers to make merging easier
names(ws_cast_week)[names(ws_cast_week) == "2"] <- "ws2"
names(ws_cast_week)[names(ws_cast_week) == "4"] <- "ws4"
names(ws_cast_week)[names(ws_cast_week) == "5"] <- "ws5"
names(ws_cast_week)[names(ws_cast_week) == "n2"] <- "2"
names(ws_cast_week)[names(ws_cast_week) == "n4"] <- "4"
names(ws_cast_week)[names(ws_cast_week) == "n5"] <- "5"

#melt function to get them all back together (new tidyr version is spread)
ws_cast_week <- melt(ws_cast_week, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
                     variable.name = "ws", value.name = "normalized_flux")

#add granularity column with "week"
ws_cast_week$granularity <- rep("week", nrow(ws_cast_week))

#####MERGE
#merge all ws_casts together
ws_cast <- Reduce(function(...) merge(..., all=T), list(ws_cast_month, ws_cast_year, ws_cast_week))
data_norm <- merge(imported_data, ws_cast, all = T)

#save as RDS file
saveRDS(data_norm, file="normalized_flux.rds")
data_norm <- readRDS("normalized_flux.rds")

#End normalization data cleaning##########################
