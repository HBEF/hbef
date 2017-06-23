library(ggplot2)
library(lubridate)
library(gridExtra)
library(readr)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(utils)
library(ggiraph)
library(grid)
library(ggthemes)



cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")

solute_palette <- c(cation, anion, hydro)
source_shapes <- c("Discharge" = 16, "Precipitation"= 21)

#read in the data
precip_dis <- readRDS("precip_stream_data_long.rds")

#Add columns of the natural logarithm of relevant values
ln_concentration_ueq <- as.data.frame(log(precip_dis$concentration_ueq))
ln_ueq_weighted_average <- as.data.frame(log(precip_dis$ueq_weighted_average))
ln_flux <- as.data.frame(log(precip_dis$flux))
ln_flux_sum <- as.data.frame(log(precip_dis$flux_sum))

precip_dis <- cbind(as.data.frame(precip_dis), ln_concentration_ueq,
                    ln_ueq_weighted_average, ln_flux,
                    ln_flux_sum)
colnames(precip_dis) <- c("ws", "date", "water_date", "water_year", "solute",
                          "concentration_mg", "source", "water_mm_pm", "MW", "z",
                          "concentration_ueq", "concentration_umol", "flux",
                          "mg_weighted_average", "umol_weighted_average",
                          "ueq_weighted_average","flux_sum",
                          "ln_concentration_ueq", "ln_ueq_weighted_average",
                          "ln_flux", "ln_flux_sum")


#Write a function that converts the Source code from precip and 
#flow to Precipitation and Streamwater Discharge
source_change <- function(df){
  df$source <- ifelse(df$source == "precip", "Precipitation",
                      "Discharge")
  return(df)
}

# a function that formats the data to use in the plot 
#based on the user input parameters
format_data <- function(df, watersheds, ion, precipitation, 
                        c.units, log, t.scale){
  df <- as.data.frame(df)
  df1 <- source_change(df)
  df2 <- df1[df1$ws %in% watersheds,]
  df3 <- filter(df2, solute == ion)
  if (precipitation == "precip"){
    df4 <- df3
  }else{
    df4 <- filter(df3, source == "Discharge") 
  }
  if (log == FALSE){
    if (c.units == "ueq/L"){
      if (t.scale == "year"){
        df4$value = df4$ueq_weighted_average
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = df4$concentration_ueq
        df4$date.st = paste(df4$water_date)
      }
    }else if (c.units == "mg/L"){
      if (t.scale == "year"){
        df4$value = df4$mg_weighted_average
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = df4$concentration_mg
        df4$date.st = paste(df4$water_date)
      }
    }else{
      if (t.scale == "year"){
        df4$value = df4$umol_weighted_average
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = df4$concentration_umol
        df4$date.st = paste(df4$water_date)
      }
    }
  }else{
    if (c.units == "ueq/L"){
      if (t.scale == "year"){
        df4$value = log(df4$ueq_weighted_average)
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = log(df4$concentration_ueq)
        df4$date.st = paste(df4$water_date)
      }
    }else if (c.units == "mg/L"){
      if (t.scale == "year"){
        df4$value = log(df4$mg_weighted_average)
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = log(df4$concentration_mg)
        df4$date.st = paste(df4$water_date)
      }
    }else{
      if (t.scale == "year"){
        df4$value = log(df4$umol_weighted_average)
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = log(df4$concentration_umol)
        df4$date.st = paste(df4$water_date)
      }
    }
  }
 df5 <- select(df4, ws, water_date, water_year,
               date.st, source, solute, value) 
 colnames(df5) = c("ws","water.date", "water.year",
                   "date", "Source", "solute",
                   "value")
 df5$ws = paste("Watershed", df5$ws, sep = " ")
 return(df5)
}

#a function used to generate the ggplot grob based on the user inputs
df_ggplot <- function(df, timescale, date.input, sc, y.lab, title.lab, addprecip, ws){
  if (length(ws) %in% c(1,2,3)){
    col = 1
  }else if (length(ws) == 4){
    col = 2
  }else if (length(ws) == 5){
    col = 1
  }else if (length(ws) == 6){
    col = 2
  }else if (length(ws) == 7){
    col = 1
  }else if (length(ws) == 8){
    col = 2
  }else if (length(ws) == 9){
    col = 3
  }
  if (addprecip == "precip"){
    if (timescale == "month"){
      p = ggplot(df, aes(x = water.date, y = value,
                         shape = Source, label = date)) +
        geom_line(color = sc) +
        geom_point(color = sc, size = 1.5, fill = "white", stroke = 0.5) +
        geom_smooth(color = "green", method = "lm", se = FALSE) 
        
    }else{
      p = ggplot(df, aes(x = water.year, y = value, color = solute,
                         shape = Source,label = date)) +
        geom_line(color = sc) + 
        geom_point(color = sc, size = 1.5, fill = "white", stroke = 0.5)+
        geom_smooth(color = "green", method = "lm", se = FALSE)
        
        
    }
  }else{
    if (timescale == "month"){
      p = ggplot(df, aes(x = water.date, y = value,
                         label = date)) +
        geom_line(color = sc) +
        geom_point(color = sc, size = 1.5, fill = "white", stroke = 0.5) +
        geom_smooth(color = "green", method = "lm", se = FALSE)
    }else{
      p = ggplot(df, aes(x = water.year, y = value,
                         label = date)) +
        geom_line(color = sc) +
        geom_point(color = sc, size = 1.5, fill = "white", stroke = 0.5) +
        geom_smooth(color = "green", method = "lm", se = FALSE)
    }
  }
  if (length(ws) > 1){
    plot.df <- p +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      facet_wrap(~ws, ncol = col) +
      scale_shape_manual(values = source_shapes) +
      guides(color = FALSE)
  
  }else{
    plot.df <- p +
    coord_cartesian(xlim = c(as.Date(date.input[1]), 
                             as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab) +
      scale_shape_manual(values = source_shapes)+
      guides(color = FALSE)

  }
  return(plot.df)
}


shinyServer(function(input, output) {
  
  #a reactive data set for plotting
  solute.data2 <- reactive({
    format_data(df = precip_dis,
                watersheds = input$watershed,
                ion = input$solute,
                log = input$ln,
                precipitation = input$p,
                c.units = input$units,
                t.scale = input$scale)
  })
  
  #Reactive value for the type of inputted solute
  sol2 <- reactive({
    if (input$solute == "Na"){
      "Sodium"
    }else if (input$solute == "Ca"){
      "Calcium"
    }else if (input$solute == "Mg"){
      "Magnesium"
    }else if (input$solute == "K"){
      "Postassium"
    }else if (input$solute == "SO4"){
      "Sulfate"
    }else if (input$solute == "NO3"){
      "Nitrate"
    }else if (input$solute == "Cl"){
      "Chlorine"
    }else{
      "Hydrogen Ion"
    }
  })
  
  #the plot output
  output$splot <- renderPlotly({
    len <- length(input$watershed)
    if (len == 1){
      l = 300
      w = 500
    }else if (len == 2){
      l = 500
      w = 500
    }else if (len == 3){
      l = 600
      w = 500
    }else if (len == 4){
      l = 600
      w = 500
    }else if (len == 5){
      l = 800
      w = 500
    }else if (len == 6){
      l = 800
      w = 500
    }else if (len == 7){
      l = 1200
      w = 500
    }else if (len == 8){
      l = 1200
      w = 500
    }else{
      l = 1200
      w = 700
    }
    if (input$solute == "K"){
       c = "#95AFDD"
    }else if (input$solute == "Na"){
      c = "#7195D2"
    }else if (input$solute == "NH4"){
      c = "#4E7AC7"
    }else if (input$solute == "Ca"){
      c = "#3B5C95"
    }else if (input$solute == "Mg"){
      c = "#273D64"
    }else if (input$solute == "Al"){
      c = "#162338"
    }else if (input$solute == "PO4"){
      c = "#600B0B"
    }else if (input$solute == "SO4"){
      c = "#8F1010"
    }else if (input$solute == "NO3"){
      c = "#BF1616"
    }else if (input$solute == "SiO2"){
      c = "#CC4545"
    }else if (input$solute == "Cl"){
      c = "#D97373"
    }else if (input$solute == "HCO3"){
      c = "#E5A2A2"
    }else{
      c = "#FFE79C"
    }
    if (input$ln == FALSE){
      title = paste(sol2(), "Concentration", sep = " ")
      y = input$units
    }else{
      title = paste("Natural Log of", sol2(), "Concentration", sep = " ")
      y = paste("ln(", input$units, ")", sep = "")
    }
    if (input$scale == "month"){
      ggplotly(
        df_ggplot(solute.data2(), timescale = input$scale,
                  date.input = input$dates,
                  y.lab = y, sc = c,
                  title.lab = title,
                  addprecip = input$p,
                  ws = input$watershed), tooltip = c("y", "label"),
        height = l, width = w
      ) %>% layout(margin = list(b = 90))
    }else{
      ggplotly(
        df_ggplot(solute.data2(), timescale = input$scale,
                  date.input = input$dates,
                  y.lab = y, sc = c,
                  title.lab = title,
                  addprecip = input$p,
                  ws = input$watershed), tooltip = c("y", "label"),
        height = l, width = w
      ) %>% layout(margin = list(b = 90))
    }
  })
})