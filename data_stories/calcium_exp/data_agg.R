library(shinydashboard)
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)

#Load and assign data
load("precip_streamflow_dfs.RData")
streamflow = precip_streamflow_long

#Load streamflow data
stream = read_csv("swd.txt")

#Set NA codes equal to NA
stream[stream == -99] = NA

#Seperate DATE column into Year, Month, and Day columns for 
#aggregating later by month and by week
stream = separate(stream, col = DATE, into = c("Year", "Month", "Day"), 
                  sep = "-", remove = FALSE)

#Create a Year.month column that gives only the year and the month but 
#not the specific day, for aggregating by month
stream$Year.month = paste(stream$Year, stream$Month, "01", sep = "-")

#Create a column called Water.month that gives the water date by month
stream$Water.month = as.Date(stream$Year.month) %m-% months(5)

#Seperate Water.month into Water.year, the water year, 
#w.m, and w.d; we will only keep Water.year
stream = separate(stream, col = Water.month, 
                  into = c("Water.year", "w.m", "w.d"),
                  sep = "-", remove = FALSE)

#Remove w.m and w.d columns that aren't needed
stream$w.m = NULL
stream$w.d = NULL

#Create a column that designates weeks 
stream$Week <- as.numeric(as.Date(stream$DATE) -
                            as.Date(stream[[1, "DATE"]])) %/% 7

#Sum up the streamflow by week 
weekly.streamflow <- stream %>%
  select(Week, WS_1, WS_2, WS_3, WS_4, WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Week) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),NA,sum(.,na.rm = TRUE))))
#If the data is all NAs, the ifelse statement gives the sum to be NA

#Get rid of NA row
weekly.streamflow <- weekly.streamflow[-3080, ]

#Add 1 to all of the Week designators to start weeks at 1 instead of 0
weekly.streamflow$Week <- weekly.streamflow$Week + 1

#Save weekly.streamflow as weekly_streamflow, a .Rdata file
save(weekly.streamflow, file = "weekly_streamflow.Rdata")

#Sum up the streamflow by month
monthly.streamflow = stream %>%
  select(Year.month, WS_1, WS_2, WS_3, WS_4, 
         WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Year.month) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),
                             NA,sum(.,na.rm = TRUE))))

#Get rid of the row with NAs for the date
monthly.streamflow <- monthly.streamflow[-709,]

#save monthly.streamflow as monthly_streamflow, a .Rdata file
save(monthly.streamflow, file = "monthly_streamflow.Rdata")

#Sum up the streamflow by year
yearly.streamflow = stream %>%
  select(Water.year, WS_1, WS_2, WS_3, WS_4, WS_5, 
         WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Water.year) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),
                             NA,sum(.,na.rm = TRUE))))

#Get rid of the row with NAs for the date
yearly.streamflow <- yearly.streamflow[-61, ]

#Save yearly.streamflow as yearly_streamflow, an .Rdata file
save(yearly.streamflow, file = "yearly_streamflow.Rdata")

#Load and format precipitation data
precip = read_csv("pwd.txt")

#Set the cells coded as NAs equal to NA
precip[precip == -99.9] = NA

#Separate the Date column into Year, Month, and Day, for aggregating
#by year, month, and week later
precip = separate(precip, col = Date, into = c("Year", "Month", "Day"), 
                  sep = "-", remove = FALSE)

#Create a Year.month column for aggregating by month
precip$Year.month <- paste(precip$Year, precip$Month, "01", sep = "-")

#Create a Water.month column that gives the water year and month
precip$Water.month <- as.Date(precip$Year.month) %m-% months(5)

#Separate the Water.month column into a Water.year column, for the water year,
#and into w.m and w.d columns, which we will delete
precip <- separate(precip, col = Water.month, 
                   into = c("Water.year", "w.m", "w.d"),
                   sep = "-", remove = FALSE)

#Delete the w.m and w.d columns; we only need the water year
precip$w.m <- NULL
precip$w.d <- NULL

#Create a column with a designator for the week
precip$Week <- as.numeric(as.Date(precip$Date) - 
                            as.Date(precip[[1, "Date"]])) %/% 7

#Sum up precipitation by week
weekly.precip <- precip %>%
  select(Week, WS_1, WS_2, WS_3, WS_4, 
         WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Week) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),
                             NA,sum(.,na.rm = TRUE))))

#Create a column which gives the date of the end of each week
weekly.precip$Date <- as.Date("1956-01-01") + weekly.precip$Week*7

#Add one to all the week designators so they start at one instead of zero
weekly.precip$Week <- weekly.precip$Week + 1

#Save weekly.precip as weekly_precip, a .Rdata file
save(weekly.precip, file = "weekly_precip.Rdata")

#Sum up the precipitation by month
monthly.precip = precip %>%
  select(Year.month, WS_1, WS_2, WS_3, WS_4, 
         WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Year.month) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),
                             NA,sum(.,na.rm = TRUE))))

#Save monthly.precip as monthly_precip, a .Rdata file
save(monthly.precip, file = "monthly_precip.Rdata")

#Sum up the precipitation by year
yearly.precip = precip %>%
  select(Water.year, WS_1, WS_2, WS_3, WS_4, 
         WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Water.year) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),
                             NA,sum(.,na.rm = TRUE))))

#Save yearly.precip as yearly_precip, a .Rdata file
save(yearly.precip, file = "yearly_precip.Rdata")

#Find the monthly evapotranspiration, precipitation minus streamflow
monthly.et <- data.frame(Date = monthly.precip$Year.month,
                         WS_1 = monthly.precip$WS_1 - monthly.streamflow$WS_1,
                         WS_2 = monthly.precip$WS_2 - monthly.streamflow$WS_2,
                         WS_3 = monthly.precip$WS_3 - monthly.streamflow$WS_3,
                         WS_4 = monthly.precip$WS_4 - monthly.streamflow$WS_4,
                         WS_5 = monthly.precip$WS_5 - monthly.streamflow$WS_5,
                         WS_6 = monthly.precip$WS_6 - monthly.streamflow$WS_6,
                         WS_7 = monthly.precip$WS_7 - monthly.streamflow$WS_7,
                         WS_8 = monthly.precip$WS_8 - monthly.streamflow$WS_8,
                         WS_9 = monthly.precip$WS_9 - monthly.streamflow$WS_9)

#Find the weekly evapotranspiration
weekly.et <- data.frame(Date = weekly.precip$Date,
                        WS_1 = weekly.precip$WS_1 - weekly.streamflow$WS_1,
                        WS_2 = weekly.precip$WS_2 - weekly.streamflow$WS_2,
                        WS_3 = weekly.precip$WS_3 - weekly.streamflow$WS_3,
                        WS_4 = weekly.precip$WS_4 - weekly.streamflow$WS_4,
                        WS_5 = weekly.precip$WS_5 - weekly.streamflow$WS_5,
                        WS_6 = weekly.precip$WS_6 - weekly.streamflow$WS_6,
                        WS_7 = weekly.precip$WS_7 - weekly.streamflow$WS_7,
                        WS_8 = weekly.precip$WS_8 - weekly.streamflow$WS_8,
                        WS_9 = weekly.precip$WS_9 - weekly.streamflow$WS_9)


#find the yearly evapotranspiration for each watershed
yearly.et <- data.frame(Date = yearly.streamflow$Water.year,
                        WS_1 = yearly.precip$WS_1 - yearly.streamflow$WS_1,
                        WS_2 = yearly.precip$WS_2 - yearly.streamflow$WS_2,
                        WS_3 = yearly.precip$WS_3 - yearly.streamflow$WS_3,
                        WS_4 = yearly.precip$WS_4 - yearly.streamflow$WS_4,
                        WS_5 = yearly.precip$WS_5 - yearly.streamflow$WS_5,
                        WS_6 = yearly.precip$WS_6 - yearly.streamflow$WS_6,
                        WS_7 = yearly.precip$WS_7 - yearly.streamflow$WS_7,
                        WS_8 = yearly.precip$WS_8 - yearly.streamflow$WS_8,
                        WS_9 = yearly.precip$WS_9 - yearly.streamflow$WS_9)

#Change the column names of the watersheds to be more descriptive 
#for graphing
colnames(yearly.et) = c("Date", "Watershed 1", "Watershed 2",
                        "Watershed 3", "Watershed 4", "Watershed 5",
                        "Watershed 6", "Watershed 7", "Watershed 8",
                        "Watershed 9")
#Save yearly.et as yearly_et, a .Rdata file
save(yearly.et, file = "yearly_et.Rdata")

#Change to a long format
et.by.year <- yearly.et %>%
  gather(Watershed, Value, -Date)

#Save et.by.year as et_by_year_long, a .Rdata file
save(et.by.year, file = "et_by_year_long.Rdata")

#Filter so that only Watershed 1 and Watershed 3 are included,
#the calcium experiment watershed and the hydrological reference
yearly <- et.by.year %>%
  filter(Watershed == "Watershed 1"|Watershed == "Watershed 3")

#Add -01-01 to the year for putting into a date format later
yearly$Date <- paste(yearly$Date, "01-01", sep = "-")

#Create a data frame with the watershed areas in hectares to give 
#evapotranspiration in terms of area
ws_area <- data.frame(Watershed = c("Watershed 1", "Watershed 3"),
                      Area = c(11.8, 42.4))

#Merge ws_area and yearly data frames to create an Area column in yearly
yearly <- merge(yearly, ws_area, by = "Watershed")

#Create an ET column that gives ET per hectare
yearly$ET <- yearly$Value/yearly$Area

#Change the column names of monthly.et to give more descriptive watershed
#names for graphing
colnames(monthly.et) = c("Date", "Watershed 1", "Watershed 2",
                         "Watershed 3", "Watershed 4", "Watershed 5",
                         "Watershed 6", "Watershed 7", "Watershed 8",
                         "Watershed 9")

#Save montlhy.et as monthly_et, a .Rdata file
save(monthly.et, file = "monthly_et.Rdata")

#change to a long format 
et.by.month <- monthly.et %>%
  gather(Watershed, Value, -Date) 

#Save et.by.month as et_by_month_long, a .Rdata file
save(et.by.month, file = "et_by_month_long.Rdata")

#Filter only Watersheds 1 and 3 out
et.by.month <- et.by.month %>%
  filter(Watershed == "Watershed 1"|Watershed == "Watershed 3")

#Create an Area column by merging
et.by.month <- merge(et.by.month, ws_area, by = "Watershed")

#Create an ET column that gives monthly evapotranspiration per hectare
et.by.month$ET <- et.by.month$Value/et.by.month$Area

#Change the column names of the watersheds to be more descriptive for graphing
#later
colnames(weekly.et) <-c("Date", "Watershed 1", "Watershed 2",
                        "Watershed 3", "Watershed 4", "Watershed 5",
                        "Watershed 6", "Watershed 7", "Watershed 8",
                        "Watershed 9")

#Save weekly.et as weekly_et, a .Rdata file
save(weekly.et, file = "weekly_et.Rdata")

#Change to a long format
et.by.week <- weekly.et %>%
  gather(Watershed, Value, -Date)

#Save et.by.week as et_by_week_long, a .Rdata file
save(et.by.week, file = "et_by_week_long.Rdata")

#Filter by watershed, keeping only watersheds 1 and 3
et.by.week <- et.by.week %>%
  filter(Watershed == "Watershed 1"|Watershed == "Watershed 3")

#Merge to create an Area column in et.by.week
et.by.week <- merge(et.by.week, ws_area, by="Watershed")

#Create a column for evapotranspiration by hectare
et.by.week$ET <- et.by.week$Value/et.by.week$Area

#Read in elevation data
w1_coords <- read.csv("w1_coords.csv")
w6_coords <- read.csv("w6_coords_revised.csv")
#Change the column name of the first column to Plot in each data set
#for easier data wrangling later
colnames(w6_coords)[1] <- "Plot"
colnames(w1_coords)[1] <- "Plot"

#Read in yearly data
w1_1996 <- read_csv("w1_1996veg.txt")
w6_1997 <- read_csv("w6_1997veg.txt")
w1_2001 <- read_csv("w1_2001veg.txt")
w6_2002 <- read_csv("w6_2002veg.txt")
w1_2006 <- read_csv("w1_2006veg.txt")
w6_2007 <- read.csv("w6_2007veg.csv")
w1_2011 <- read_csv("w1_2011veg.txt")
w6_2012 <- read.csv("w6_2012veg.csv")

#Add elevation data to yearly data, then format to combine 
#data frames
w1_1996 <- merge(w1_1996, w1_coords, by = "Plot")
#Added elevation data to w1_1996, watershed 1, 1996 data
w1_1996 <- w1_1996 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh, Vigor)
#Selected the relevant columns for filtering and plotting

w6_1997 <- merge(w6_1997, w6_coords, by = "Plot")
#Added elevation data to w6_1997

#Add information about the year and the watershed for better 
#combination of data frames later
Year <- rep(1997, nrow(w6_1997))
Watershed <- rep(6, nrow(w6_1997))
w6_1997 <- cbind(w6_1997, Year, Watershed)


#Selecting the relevant columns for filtering and plotting
w6_1997 <- w6_1997 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh, Vigor)

#Add elevation data to w1_2001 by merging
w1_2001 <- merge(w1_2001, w1_coords, by = "Plot")

#Selecting the relevant columns for filtering and plotting
w1_2001 <- w1_2001 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh, Vigor)

#Merging to add elevation data to w6_2002
w6_2002 <- merge(w6_2002, w6_coords, by = "Plot")

#Adding Year and Watershed information to w6_2002 for improved 
#combination of data frames later
Year <- rep(2002, nrow(w6_2002))
Watershed <- rep(6, nrow(w6_2002))
w6_2002 <- cbind(w6_2002, Year, Watershed)

#Selecting relevant columns for filtering and plotting later
w6_2002 <- w6_2002 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh, Vigor)

#Adding elevation data to w1_2006 though a merge
w1_2006 <- merge(w1_2006, w1_coords, by = "Plot")

#Selecting relevant columns
w1_2006 <- w1_2006 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh, Vigor)

#Merging to add elevation data to w6_2007
w6_2007 <- merge(w6_2007, w6_coords, by = "Plot")

#Adding Year and Watershed columns to w6_2007 for easy combination of data
#frames
Year <- rep(2007, nrow(w6_2007))
Watershed <- rep(6, nrow(w6_2007))
w6_2007 <- cbind(w6_2007, Year, Watershed)

#selecting relevant columns
w6_2007 <- w6_2007 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh, Vigor)

#Adding elevation data to w1_2011 through merge
w1_2011 <- merge(w1_2011, w1_coords, by = "Plot")

#selecting relevant columns
w1_2011 <- w1_2011 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh, Vigor)

#Merging to add elevation data to w6_2012
w6_2012 <- merge(w6_2012, w6_coords, by = "Plot")

#Adding year and watershed information to w6_2012
Year <- rep(2012, nrow(w6_2012))
Watershed <- rep(6, nrow(w6_2012))
w6_2012 <- cbind(w6_2012, Year, Watershed)

#Selecting relevant columns
w6_2012 <- w6_2012 %>%
  select(Plot, Year, Watershed, Species, AbvBmss, BlwBmss, m, Dbh,  Vigor)

#combinine all the data frames together for all the years and watersheds
df = rbind(w1_1996, w6_1997,
           w1_2001, w6_2002,
           w1_2006, w6_2007,
           w1_2011, w6_2012)

#save df as biomass_w1_w6, a .Rdata file
save(df, file = "biomass_w1_w6.Rdata")