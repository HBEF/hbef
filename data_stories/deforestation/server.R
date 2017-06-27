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
library(grid)

source_shapes <- c("Discharge" = 16, "Precipitation"= 21)

#read in the data
precip_dis <- readRDS("precip_stream_data_long.rds")


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
    }else if (c.units == "umol/L"){
      if (t.scale == "year"){
        df4$value = df4$umol_weighted_average
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = df4$concentration_umol
        df4$date.st = paste(df4$water_date)
      }
    }else {
      if (t.scale == "year"){
        df4$value = df4$flux_sum
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = df4$flux
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
    }else if (c.units == "umol/L"){
      if (t.scale == "year"){
        df4$value = log(df4$umol_weighted_average)
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = log(df4$concentration_umol)
        df4$date.st = paste(df4$water_date)
      }
    }else {
      if (t.scale == "year"){
        df4$value = log(df4$flux_sum)
        df4$date.st = paste(df4$water_year)
      }else{
        df4$value = log(df4$flux)
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

#Function to produce a formatted data frame of the quantities of 
#precipitation and discharge on the correct scales
quantity_format <- function(df, time.scale, log.q) {
  if (time.scale == "monthly"){
    if (log.q == FALSE){
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2 | ws == 4 |
               ws == 5, solute == "Ca")
      timely <- source_change(timely)
      colnames(timely) = c("ws", "water.year", "water.date",
                            "solute", "value", "Source")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.date)
    }else{
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2 | ws == 4 |
                 ws == 5, solute == "Ca")
      timely$ln_water_mm_pm <- log(timely$water_mm_pm)
      timely <- source_change(timely)
      colnames(timely) = c("ws", "water.year", "water.date",
                           "solute", "water_mm_pm", "Source", "value")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.date)
    }
  }else{
    if (log.q == FALSE){
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2 | ws == 4 |
                 ws == 5, solute == "Ca") %>%
        group_by(ws, source, water_year) %>%
        summarize(value = sum(water_mm_pm))
      timely <- source_change(timely)
      colnames(timely) <- c("ws", "Source", "water.year", "value")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.year)
    }else{
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2 | ws == 4 |
                 ws == 5, solute == "Ca") %>%
        group_by(ws, source, water_year) %>%
        summarize(value = log(sum(water_mm_pm)))
      timely <- source_change(timely)
      colnames(timely) <- c("ws", "Source", "water.year", "value")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.year)
    
    }
    
  }
  return(as.data.frame(timely))
}


#Function to plot the formatted data frame in ggplot2
plot.formatted.df <- function(df, timescale, sc, date.input, y.lab, title.lab, addprecip){
  m <- max(df$value, na.rm = TRUE)
  v.line <- data.frame(ws = c("Watershed 2", "Watershed 4",
                                 "Watershed 5", "Watershed 6"), 
                       vals = c(-1675, 92, 4926, NA))
  if (addprecip == "precip"){
    p <- ggplot(df,aes(x= get(timescale),y=value, shape =Source, label=date)) +
      geom_line(color = 
                  ifelse(length(sc) ==1, sc, sc[1])) +
      geom_point(color = 
                   ifelse(length(sc) == 1, sc, sc[2]), fill = "white") +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = T) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      scale_shape_manual(values = source_shapes) +
      facet_wrap(~ws, ncol = 1) 
  }else{
    p <- ggplot(df,aes(x= get(timescale),y=value,label=date)) +
      geom_line(color = 
                  ifelse(length(sc) == 1, sc, sc[1])) +
      geom_point(color = 
                   ifelse(length(sc) ==1, sc, sc[2]), fill = "white") +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = T) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      facet_wrap(~ws, ncol = 1) 
  }
  return(p)
}


shinyServer(function(input, output) {
  
  #Reactive value for the units, either in
  #concentration or flux
  unit <- reactive({input$units})
  
  #Reactive value for the type of inputted solute
  sol <- reactive({
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
  
  #Reactive value selecting the discharge quantity data set by the time scale,
  #annually or monthly
  discharge.data <- reactive({
    if (input$p.dis == "precip"){
      if (input$ln.dis == FALSE){
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = FALSE)
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = FALSE)
        }
      }else{
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = TRUE)
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = TRUE)
        }
      }
    }else{
      if (input$ln.dis == FALSE){
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = FALSE) %>%
            filter(Source == "Discharge")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = FALSE) %>%
            filter(Source == "Discharge")
        }
      }else{
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = TRUE) %>%
            filter(Source == "Discharge")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = TRUE) %>%
            filter(Source == "Discharge")
        }
      }
    }
    
  })
  
  #Reactive value selecting the data set for discharge chemistry
  #by time scale, unit and solute
  solute.data <- reactive({
    format_data(df = precip_dis,
                watersheds = c(2, 6, 4, 5),
                ion = input$solute,
                precipitation = input$p,
                c.units = input$units,
                log = input$ln,
                t.scale = input$scale)
  })
  
  #Plot output for water chemistry
  output$s.plot <- renderPlotly({
    if (input$ln == FALSE){
      title = paste(sol(), "Concentration", sep = " ")
      y = input$units
    }else{
      title = paste("Natural Log of", sol(), "Concentration", sep = " ")
      y = paste("ln(", input$units, ")", sep = "")
    }
    if (input$scale == "month"){
      s <- "water.date"
    }else{
      s <- "water.year"
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
    ggplotly( 
      plot.formatted.df(df = solute.data(),
                        timescale = s, sc = c,
                        date.input = input$dates, y.lab = y, 
                        title.lab = title,
                        addprecip = input$p),tooltip = c("y", "label"),
      width = 600, height = 600)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) 
  })
  
  #Plot output for discharge quantity
  output$d.plot <- renderPlotly({
    if (input$p.dis == "precip"){
      if (input$ln.dis == FALSE){
        y <- "mm"
        title <- "Discharge and Precipitation Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Discharge and Precipitation Quantities"
      }
    }else{
      if (input$ln.dis == FALSE){
        y <- "mm"
        title <- "Discharge Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Discharge Quantities"
      }
    }
    
    if (input$scale.dis == "month"){
      s2 <- "water.date"
    }else{
      s2 <- "water.year"
    }
    ggplotly(
      plot.formatted.df(df = discharge.data(), 
                        timescale = s2, sc = c("red", "blue"),
                        date.input = input$dates.dis,
                        y.lab = y, title.lab = title,
                        addprecip = input$p.dis), tooltip = c("y", "label"),
      height = 600, width = 600)%>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) 
    
    
    
  })
  options(warn = -1)
})
