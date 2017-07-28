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

#Seperate Water.month into Water.year, the water year, w.m, and w.d; we will only 
#keep Water.year
stream = separate(stream, col = Water.month, into = c("Water.year", "w.m", "w.d"),
                  sep = "-", remove = FALSE)

#Remove w.m and w.d columns that aren't needed
stream$w.m = NULL
stream$w.d = NULL

#Create a column that designates weeks 
stream$Week <- as.numeric(as.Date(stream$DATE) - as.Date(stream[[1, "DATE"]])) %/% 7

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

#Sum up the streamflow by month
monthly.streamflow = stream %>%
  select(Year.month, WS_1, WS_2, WS_3, WS_4, WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Year.month) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),NA,sum(.,na.rm = TRUE))))

#Get rid of the row with NAs for the date
monthly.streamflow <- monthly.streamflow[-709,]


#Sum up the streamflow by year
yearly.streamflow = stream %>%
  select(Water.year, WS_1, WS_2, WS_3, WS_4, WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Water.year) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),NA,sum(.,na.rm = TRUE))))

#Get rid of the row with NAs for the date
yearly.streamflow <- yearly.streamflow[-61, ]

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
precip <- separate(precip, col = Water.month, into = c("Water.year", "w.m", "w.d"),
                  sep = "-", remove = FALSE)

#Delete the w.m and w.d columns; we only need the water year
precip$w.m <- NULL
precip$w.d <- NULL

#Create a column with a designator for the week
precip$Week <- as.numeric(as.Date(precip$Date) - 
                            as.Date(precip[[1, "Date"]])) %/% 7

#Sum up precipitation by week
weekly.precip <- precip %>%
  select(Week, WS_1, WS_2, WS_3, WS_4, WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Week) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),NA,sum(.,na.rm = TRUE))))

#Create a column which gives the date of the end of each week
weekly.precip$Date <- as.Date("1956-01-01") + weekly.precip$Week*7

#Add one to all the week designators so they start at one instead of zero
weekly.precip$Week <- weekly.precip$Week + 1

#Sum up the precipitation by month
monthly.precip = precip %>%
  select(Year.month, WS_1, WS_2, WS_3, WS_4, WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Year.month) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),NA,sum(.,na.rm = TRUE))))

#Sum up the precipitation by year
yearly.precip = precip %>%
  select(Water.year, WS_1, WS_2, WS_3, WS_4, WS_5, WS_6, WS_7, WS_8, WS_9) %>%
  group_by(Water.year) %>%
  summarise_each(funs(ifelse(sum(is.na(.))==length(.),NA,sum(.,na.rm = TRUE))))

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
#Change to a long format
et.by.year <- yearly.et %>%
  gather(Watershed, Value, -Date)

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

#change to a long format and filter to only include Watersheds 1 and 3
et.by.month <- monthly.et %>%
  gather(Watershed, Value, -Date) %>%
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

#Change to a long format and filter by watershed, keeping only
#watersheds 1 and 3
et.by.week <- weekly.et %>%
  gather(Watershed, Value, -Date) %>%
  filter(Watershed == "Watershed 1"|Watershed == "Watershed 3")

#Merge to create an Area column in et.by.week
et.by.week <- merge(et.by.week, ws_area, by="Watershed")

#Create a column for evapotranspiration by hectare
et.by.week$ET <- et.by.week$Value/et.by.week$Area

#Function to plot the evapotranspiration graphs
ET.plot <- function(data, date.range,gran){
  ggplotly(ggplot(data = data, aes(x = as.Date(Date), y = ET, 
                                   color = Watershed))+
             #color lines by watershed
             geom_line(aes(group = Watershed)) + 
             #have to assign Watershed to group so that geom_line
             #connects dots correctly
             geom_point(aes(text = paste("Date: ", Date, "<br>",
                                         "Value:", round(ET, 2), "<br>", 
                                         "Watershed: ", Watershed))) +
             #Create a text variable in aes for hover information
             coord_cartesian(xlim = c(as.Date(date.range[1]),
                                      as.Date(date.range[2])))+
             #Set the x limits to the date range inputed by the date slider
             scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
             #Set the breaks to every 10 years, output only years on the 
             #x axis
             labs(x = ifelse(gran == "year", "Water Year", "Date"),
                  y = "Evapotranspiration in mm/ha",
                  title = "Evapotranspiration Per Unit Area")+
             #label the x-axis as the water year if the granularity is by year
             #and as Date otherwise, months and weeks are not given in terms of 
             #water year
             geom_vline(xintercept = 10865) +
             #Create a verticle line showing when the calcium was applied
             theme(legend.title = element_blank()), tooltip = "text",width = 500, height = 400)%>%
    add_annotations( text="Watershed", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.8, yanchor="bottom",    # Same y as legend below
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    #Move the legend title
    config(autosize=TRUE) %>%
    #Move the legend
    layout(legend=list(y=0.8, yanchor="top")) 
}


#Read in data including SOC information
w1_soil <- read_csv("w1ffexchem.txt")

#Assign NAs to numbers designating missingness
w1_soil[w1_soil == -8888.88] = NA
w1_soil[w1_soil == -9999.88] = NA
w1_soil[w1_soil == -8888.8888] = NA
w1_soil[w1_soil == -9999.9999] = NA
w1_soil[w1_soil == -9999.99] = NA
w1_soil[w1_soil == -8888.8887] = NA
w1_soil[w1_soil == -10000.0000] = NA

#Find average percent soil carbon by year
w1_C = w1_soil %>%
  group_by(Year) %>%
  summarize(mean_C = mean(PercentC, na.rm = T))

#Function for plotting the mean percent carbon over time for 
#Watershed 1.
carbon_ggplot = function(data){
  v.line = data.frame(val = 1999)
  ggplotly(ggplot(data = data, aes(x = Year, y = mean_C)) +
             geom_bar(stat = "identity",
                      aes(text = paste("Date: ", Year, "<br>",
                            "Percent Carbon:", round(mean_C, 2))))+
             #text gives the hover information
             geom_vline(data = v.line, 
                        aes(xintercept = val),
                        linetype = 1,
                        show.legend = F)+
             #plot a vertical line for when the calcium was applied
             labs(x = "Year", y = "Mean Percent Carbon in Soil", title =
                    "Average Percent Soil Carbon for Watershed 1"), width = 700, height = 400, 
           tooltip = "text")%>%
    layout(margin = list(b = 90)) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE) %>%
    config(autosize = TRUE)
  #Turn off the plotly toolbar^
}


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

#combinint all the data frames together for all the years and watersheds
df = rbind(w1_1996, w6_1997,
           w1_2001, w6_2002,
           w1_2006, w6_2007,
           w1_2011, w6_2012)
#Add the area of the watersheds in hectares
ws.vals = data.frame(Watershed = c(1,6), Area = c(11.8, 13.2))
df = merge(df, ws.vals, by = "Watershed")

#Find biomass per area totals by year, watershed, species, and elevation range
data = df %>%
  filter(Vigor == 0| Vigor == 1 | Vigor == 2 | Vigor ==3) %>%
  filter(Species != "UNKN") %>%
  mutate(elev_range = (m %/% 100)*100) %>%
  group_by(Year, Watershed, Species, elev_range) %>%
  summarize(Biomass = (sum(AbvBmss) + sum(BlwBmss))/(mean(Area)*1000))

#Function to change the watershed number to a string such as "Watershed 6" for 6
watershed_change <- function(df){
  df$Watershed <- paste("Watershed", df$Watershed, sep = " ")
  return(df)
}
watershed_change2 <- function(df){
  df$ws <- paste("Watershed", df$ws, sep = " ")
  return(df)
}

#Function for plotting streamflow data
streamflow_plot = function(data, date.range, x.v, gran){
  #if statements set dot size and x-axis label
  if (gran == "week"){
    s = 1
    date = "Date"
  }else if (gran == "month"){
    s = 1
    date = "Water Date"
  }else{
    s = 1.5
    date = "Water Year"
  }
  #v.line gives value for plotting the calcium application line
  v.line = data.frame(val = 10865)
  ws_palette <- c("Watershed 1" = "#fae550", "Watershed 2" = "#a8db40", 
                  "Watershed 3" = "#62c74a", "Watershed 4" = "#408b77", 
                  "Watershed 5" = "#27517b", "Watershed 6" = "#303475", 
                  "Watershed 7" = "#351042", "Watershed 8" = "#79276e", 
                  "Watershed 9" = "#b63462")
  #colors for the watersheds^
  ggplotly(ggplot(data = data, aes(x = get(x.v), y = water_mm, color = ws)) +
             #x.v gives the x-variable to plot: water_date, water_year or date
             geom_line() +
             geom_point(size = s, #size of point set from above ifelse statement
                        aes(text = paste("Value:", round(water_mm, 2), "<br>",
                                         "Date: ", get(x.v), "<br>",
                                         "Watershed:", ws))) +
             #text set for hover information
             coord_cartesian(xlim = c(date.range[1], date.range[2])) +
             #date range given by date slider, inputted here^
             geom_vline(data = v.line, 
                        aes(xintercept = val),
                        linetype = 1,
                        show.legend = T) +
             #line for the calcium application date
             scale_color_manual(values = ws_palette) +
             #colors of watersheds set here^
             labs(x = date, y = "Streamflow in mm", title = "Streamflow Over Time") +
             #labels for the date set here from ifelse statement
             theme(legend.title = element_blank()),
           tooltip = "text", width = 500, height = 400) %>%
    add_annotations( text="Watershed", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.8, yanchor="bottom",    # Same y as legend below
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    #add_annotations moves the legend title
    config(autosize=TRUE) %>%
    layout(legend=list(y=0.8, yanchor="top" ))
  #layout moves the legend
}
#Function for plotting bar graphs of the biomass data 
ggplot_function = function(data, elev.name, sp.name){
  v.line = data.frame(val = 1999)
  ggplotly(ggplot(data = data, aes(x = Year, y = Biomass, fill = Watershed)) +
             geom_bar(stat = "identity")+
             geom_vline(data = v.line, 
                        aes(xintercept = val),
                        linetype = 1,
                        show.legend = T)+
             #geom_vline sets a line for the calcium application date
             
             #Below command reduces the y limits to the top region of the bars
             coord_cartesian(ylim = c(min(data$Biomass) - ((max(data$Biomass) -min(data$Biomass))/10),
                                      max(data$Biomass) + ((max(data$Biomass) -min(data$Biomass))/10))) +
             #labs gives the species name of the selected species in the title
             labs(x = "Year", y = "Live Tree Biomass (Mg/ha)",
                  title = paste(sp.name, "Biomass Over Area At", elev.name, sep = " "))+
             theme(legend.title = element_blank()),
           width = 500, height = 400)%>%
    add_annotations( text="Watershed", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.8, yanchor="bottom",    # Same y as legend below
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    #above command moves the legend title
    config(autosize=TRUE) %>%
    layout(legend=list(y=0.8, yanchor="top" ) )
  #above command moves the legend
}
shinyServer(function(input, output) {
  
#Create reactive ET dataframe with data granularity set by inputs
 ET.df <- reactive({
   if (input$granularity.et == "week"){
     et.by.week
   }else if (input$granularity.et == "month"){
     et.by.month
   }else{
     yearly
   }
 }) 

#Create reactive biomass data frame with data present according to inputs
 biomass.df <- reactive({
   if (input$species != "all"){
     data = data[data$Species== input$species,]
   }
   if (input$elevation != "all"){
     data = data[data$elev_range ==input$elevation, ]
   }
   data = data %>%
     group_by(Year, Watershed)%>%
     summarize(Biomass = sum(Biomass))
   data = watershed_change(data)
 })

 #Create reactive streamflow data set with data present according to inputs
 streamflow.df <- reactive({
   streamflow = streamflow[streamflow$granularity == input$granularity, ]
   streamflow = streamflow[streamflow$solute == "Ca", ]
   streamflow = streamflow[streamflow$source == "streamflow", ]
   streamflow = streamflow[streamflow$ws == c(1, 3, 6), ]
   streamflow = watershed_change2(streamflow)
   
 })
 
 #Create reactive x-variable for streamflow graph
 x <- reactive({
   if (input$granularity == "week"){
     "date"
   }else if (input$granularity == "month"){
     "water_date"
   }else{
     "water_year"
   }
 })
 #Create a reactive variable for the species name
 species.name <- reactive({
   if (input$species == "ACSA"){
     "Sugar Maple"
   }else if (input$species == "FAGR"){
     "American Beech"
   }else if (input$species == "BEAL"){
     "Yellow Birch"
   }else if (input$species == "FRAM"){
     "White Ash"
   }else if (input$species == "ACSP"){
     "Mountain Maple"
   }else if (input$species == "ACPE"){
     "Striped Maple/Moose Wood"
   }else if (input$species == "PRPE"){
     "Pin/Fire Chery"
   }else if (input$species == "PRVI"){
     "Choke Cherry"
   }else if (input$species == "ABBA"){
     "Balsam Fir"
   }else if (input$species == "PIRU"){
     "Red Spruce"
   }else if (input$species == "BEPA"){
     "White/Paper Birch"
   }else if (input$species == "SOAM"){
     "Mountain Ash"
   }else if (input$species == "ACRU"){
     "Red Maple"
   }else if (input$species == "TSCA"){
     "Eastern Hemlock"
   }else if (input$species == "POTR"){
     "Quaking Aspen"
   }else if (input$species == "PRSE"){
     "Black Cherry"
   }else if (input$species == "AMSP"){
     "Shadbush"
   }else if (input$species == "POGR"){
     "Big-Tooth Aspen"
   }else if (input$species == "SASP"){
     "Willow"
   }else if (input$species == "COAL"){
     "Alternate-Leaved Dogwood"
   }else if (input$species == "PRSP"){
     "Cherry (Unspecified)"
   }else if (input$species == "SARA"){
     "Red Elderberry"
   }else{
     "Total"
   }
 })
 
 #Creates a reactive value for the elevation
 elev <- reactive({
   if (input$elevation == 400){
     "400m - 500m"
   }else if (input$elevation == 500){
     "500m - 600m"
   }else if (input$elevation == 600){
     "600m - 700m"
   }else if (input$elevation == 700){
     "700m - 800m"
   }else{
     "All Elevations"
   }
 })
 
 #Plot output
  output$bm.plot = renderPlotly({theplot = ggplot_function(data = biomass.df(), elev(), species.name())%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
  output$c.plot = renderPlotly({theplot = carbon_ggplot(w1_C)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
  output$s.plot = renderPlotly({theplot = streamflow_plot(data = streamflow.df(),
                                                date.range = input$date_range,
                                                x.v = x(),
                                                gran = input$granularity)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
  output$et.plot <- renderPlotly({theplot = ET.plot(data = ET.df(),
                                          date.range = input$date_range_et,
                                          gran = input$granularity.et)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
})

