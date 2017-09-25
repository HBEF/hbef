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

load("weekly_streamflow.Rdata")
load("monthly_streamflow.Rdata")
load("yearly_streamflow.Rdata")
load("weekly_precip.Rdata")
load("monthly_precip.Rdata")
load("yearly_precip.Rdata")
load("weekly_et.Rdata")
load("monthly_et.Rdata")
load("yearly_et.Rdata")
load("et_by_week_long.Rdata")
load("et_by_month_long.Rdata")
load("et_by_year_long.Rdata")
load("biomass_w1_w6.Rdata")


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

#Filter only Watersheds 1 and 3 out
et.by.month <- et.by.month %>%
  filter(Watershed == "Watershed 1"|Watershed == "Watershed 3")

#Create an Area column by merging
et.by.month <- merge(et.by.month, ws_area, by = "Watershed")

#Create an ET column that gives monthly evapotranspiration per hectare
et.by.month$ET <- et.by.month$Value/et.by.month$Area

#Filter by watershed, keeping only watersheds 1 and 3
et.by.week <- et.by.week %>%
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
             theme(legend.title = element_blank()), tooltip = "text", width = 500, height = 400)%>%
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
 
 #Graphs the biomass plot
  output$bm.plot = renderPlotly({theplot = ggplot_function(data = biomass.df(), 
                                                           elev(), species.name())%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  #the code below fixes an issue where the plotly width 
  #argument doesn't adjust automatically.
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
  
  #Graphs the soil organic carbon plot
  output$c.plot = renderPlotly({theplot = carbon_ggplot(w1_C)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  
  #the code below fixes an issue where the plotly width 
  #argument doesn't adjust automatically.
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
  
  #Plots the streamflow graph
  output$s.plot = renderPlotly({theplot = 
    streamflow_plot(data = streamflow.df(),
    date.range = input$date_range,
    x.v = x(),
    gran = input$granularity)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  #the code below fixes an issue where the plotly width 
  #argument doesn't adjust automatically.
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
  
  #plots the evapotranspiration graph
  output$et.plot <- renderPlotly({theplot = ET.plot(data = ET.df(),
                                          date.range = input$date_range_et,
                                          gran = input$granularity.et)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  #the code below fixes an issue where the plotly width 
  #argument doesn't adjust automatically.
  theplot$x$layout$width <- NULL
  theplot$y$layout$height <- NULL
  theplot$width <- NULL
  theplot$height <- NULL
  theplot %>%
    layout(margin = list(b = 90))})
})

