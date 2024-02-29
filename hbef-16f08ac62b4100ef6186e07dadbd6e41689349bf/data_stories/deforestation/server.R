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


watershed_change <- function(df){
  df$ws2 <- paste("Watershed", df$ws, sep = " ")
  return(df)
}

#Write a function that converts the solute labels to their full written name
solute_change <- function(df){
  df[df$solute == "K", "solute"] = "Potassium"
  df[df$solute == "Na", "solute"] = "Sodium"
  df[df$solute == "Ca", "solute"] = "Calcium"
  df[df$solute == "Mg", "solute"] = "Magnesium"
  df[df$solute == "Al", "solute"] = "Aluminum"
  df[df$solute == "SO4", "solute"] = "Sulfate"
  df[df$solute == "NO3", "solute"] = "Nitrate"
  df[df$solute == "Cl", "solute"] = "Chloride"
  df[df$solute == "H", "solute"] = "Hydrogen Ion"
  return(df)
  
}

source_change <- function(df){
  df[df$source == "flow", "source"] = "Streamflow (Q)"
  df[df$source == "precip", "source"] = "Precipitation (P)"
  return(df)
}

# a function that formats the data to use in the plot 
#based on the user input parameters
format_data <- function(df, watersheds, ion, precipitation, 
                        c.units, log, t.scale){
  df <- as.data.frame(df)
  df <- solute_change(df)
  df1 <- source_change(df)
  df2 <- df1[df1$ws %in% watersheds,]
  df3 <- df2[df2$solute %in% ion, ]
  if (precipitation == "precip"){
    df4 <- df3
  }else{
    df4 <- filter(df3, source == "Streamflow (Q)") 
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
                    "date", "source", "solute",
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
                            "solute", "value", "source")
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
                           "solute", "water_mm_pm", "source", "value")
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
      colnames(timely) <- c("ws", "source", "water.year", "value")
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
      colnames(timely) <- c("ws", "source", "water.year", "value")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.year)
    
    }
    
  }
  return(as.data.frame(timely))
}


#Function to plot the formatted data frame in ggplot2
plot.formatted.df <- function(df, timescale, date.input, y.lab, title.lab, addprecip){
  theme <- theme(legend.position = "none", legend.direction = "vertical", legend.title = element_blank())
  
  color_cation <- c("Potassium" = "#95AFDD", "Sodium" = "#7195D2", 
                    "Ammonium" = "#4E7AC7" , "Calcium" = "#3B5C95",
                    "Magnesium" = "#273D64", "Aluminum" = "#162338")
  color_anion <- c("Phosphate" = "#600B0B", "Sulfate" = "#8F1010", 
                   "Nitrate" = "#BF1616", "Silicon Dioxide"= "#CC4545",
                   "Chloride" = "#D97373", "Bicarbonate" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "Hydrogen Ion" = "#FFE79C")
  
  solutes_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("Streamflow (Q)" = 16, "Precipitation (P)"= 21)
  m <- max(df$value, na.rm = TRUE)
  v.line <- data.frame(ws = c("Watershed 2", "Watershed 4",
                                 "Watershed 5", "Watershed 6"), 
                       vals = c(-1675, 92, 4926, NA))
  if (addprecip == "precip"){
    p <- ggplot(df,aes(x= get(timescale),y=value, shape =source, 
                       color = solute, label=date)) +
      geom_line() +
      geom_point(fill = "white",
                 aes(text = paste("Solute: ", solute, "<br>", 
                                  "Water Source: ", source, "<br>",
                                  "Value:", round(value, 2), "<br>", 
                                  "Date: ", get(timescale)))) +
      theme+
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = T) +
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solutes_palette) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      scale_shape_manual(values = source_shapes) +
      facet_wrap(~ws, ncol = 1) 
  }else{
    p <- ggplot(df,aes(x= get(timescale),y=value,color = solute, label=date)) +
      geom_line() +
      geom_point(fill = "white",
                 aes(text = paste("Solute: ", solute, "<br>", 
                                  "Water Source: ", source, "<br>",
                                  "Value:", round(value, 2), "<br>", 
                                  "Date: ", get(timescale)))) +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = T) +
      theme+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solutes_palette) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      facet_wrap(~ws, ncol = 1) 
  }
  plot <- ggplotly(p,tooltip = "text",
  width = 400, height = 600) %>%
  layout(margin = list(b = 90)) %>%
  config(displayModeBar = FALSE) %>%
  config(showLink = FALSE)
  
  return(plot)
}

plot.dis.df <- function(df, timescale, date.input, y.lab, title.lab, addprecip){
 theme <- theme(legend.position = "none", legend.direction = "vertical", legend.title = element_blank())
  
  color_cation <- c("Potassium" = "#95AFDD", "Sodium" = "#7195D2", 
                    "Ammonium" = "#4E7AC7" , "Calcium" = "#3B5C95",
                    "Magnesium" = "#273D64", "Aluminum" = "#162338")
  color_anion <- c("Phosphate" = "#600B0B", "Sulfate" = "#8F1010", 
                   "Nitrate" = "#BF1616", "Silicon Dioxide"= "#CC4545",
                   "Chloride" = "#D97373", "Bicarbonate" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "Hydrogen Ion" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("Streamflow (Q)" = 16, "Precipitation (P)"= 21)
  
  v.line <- data.frame(ws = c("Watershed 2", "Watershed 4",
                              "Watershed 5", "Watershed 6"), 
                       vals = c(-1675, 92, 4926, NA))
  if (addprecip == "precip"){
    p <- ggplot(df,aes(x= get(timescale),y=value, shape =source, label=date)) +
      geom_line(color = "red") +
      geom_point(color = "blue", fill = "white",
                 aes(text = paste("Water Source: ", source, "<br>",
                                  "Value:", round(value, 2), "<br>", 
                                  "Date: ", get(timescale)))) +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = T) +
      theme+
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      scale_shape_manual(values = source_shapes) +
      facet_wrap(~ws, ncol = 1) 
  }else{
    p <- ggplot(df,aes(x= get(timescale),y=value,label=date)) +
      geom_line(color = "red") +
      geom_point(color = "blue", fill = "white",
                 aes(text = paste("Water Source: ", source, "<br>",
                                  "Value:", round(value, 2), "<br>", 
                                  "Date: ", get(timescale)))) +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = T) +
      theme+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      facet_wrap(~ws, ncol = 1) 
  }
  plot <- ggplotly(p, tooltip = "text",
                   height = 600, width = 400)%>%
    layout(margin = list(b = 90)) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE) 
  return(plot)
}


shinyServer(function(session, input, output) {
  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  allow 'select all' interactivity
  solutes_cations <- list("Potassium (K)" = "Potassium",
                          "Sodium (Na)" = "Sodium",
                          "Calcium (Ca)" = "Calcium",
                          "Magnesium (Mg)" = "Magnesium",
                          "Aluminum (Al)" = "Aluminum")
  
  solutes_anions <- list("Sulfate (SO4)" = "Sulfate",
                         "Nitrate (NO3)" = "Nitrate",
                         "Chloride (Cl)" = "Chloride")
  solutes_H <- c("Hydrogen (H)" = "Hydrogen Ion")
  
  observeEvent(input$select_all_ions, {
    if(input$select_all_ions == 0) {}
    else if (input$select_all_ions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations", selected = "Sodium")}
    else{
      updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)
      updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)}
  })
  
  observeEvent(input$select_all_anions, {
    if(input$select_all_anions == 0) {}
    else if (input$select_all_anions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "Phosphate")}
    else{updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)}
  })
  
  observeEvent(input$select_all_cations, {
    if(input$select_all_cations == 0) {}
    else if (input$select_all_cations%%2 == 0){updateCheckboxGroupInput(session, "solutes_cations", selected = "Sodium")}
    else{updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)}
  })
  
  
  solutes <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
  
  #Reactive value for the units, either in
  #concentration or flux
  unit <- reactive({input$units})
  
  #Reactive value selecting the discharge quantity data set by the time scale,
  #annually or monthly
  discharge.data <- reactive({
    if (input$p.dis == "precip"){
      if (input$ln.dis == FALSE){
        if (input$granularity2 == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = FALSE)
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = FALSE)
        }
      }else{
        if (input$granularity2 == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = TRUE)
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = TRUE)
        }
      }
    }else{
      if (input$ln.dis == FALSE){
        if (input$granularity2 == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = FALSE) %>%
            filter(source == "Streamflow (Q)")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = FALSE) %>%
            filter(source == "Streamflow (Q)")
        }
      }else{
        if (input$granularity2 == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", log.q = TRUE) %>%
            filter(source == "Streamflow (Q)")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", log.q = TRUE) %>%
            filter(source == "Streamflow (Q)")
        }
      }
    }
    
  })
  
  #Reactive value selecting the data set for discharge chemistry
  #by time scale, unit and solute
  solute.data <- reactive({
    format_data(df = precip_dis,
                watersheds = c(2, 6, 4, 5),
                ion = solutes(),
                precipitation = input$p,
                c.units = input$units,
                log = input$ln,
                t.scale = input$granularity)
  })
  
  #Plot output for water chemistry
  output$s.plot <- renderPlotly({
    if (length(solutes()) <= 1){
      if (input$ln == FALSE){
        title = paste(solutes(), "Concentration", sep = " ")
        y = input$units
      }else{
        title = paste("Natural Log of", solutes(), "Concentration", sep = " ")
        y = paste("ln(", input$units, ")", sep = "")
      }
    }else{
      if (input$ln == FALSE){
        title = "Solute Concentations"
        y = input$units
      }else{
        title = "Natural Log of Solute Concentrations"
        y = paste("ln(", input$units, ")", sep = "")
      }
    }
    if (input$granularity == "month"){
      s <- "water.date"
    }else{
      s <- "water.year"
    }

    plot.formatted.df(df = solute.data(),
                      timescale = s,
                      date.input = input$dates,
                      y.lab = y,
                      title.lab = title,
                      addprecip = input$p)
  })
  
  #Plot output for discharge quantity
  output$d.plot <- renderPlotly({
    if (input$p.dis == "precip"){
      if (input$ln.dis == FALSE){
        y <- "mm"
        title <- "Discharge and Precipitation Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Water Quantities"
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
    
    if (input$granularity2 == "month"){
      s2 <- "water.date"
    }else{
      s2 <- "water.year"
    }
    
      plot.dis.df(df = discharge.data(), 
                        timescale = s2,
                        date.input = input$dates.dis,
                        y.lab = y, title.lab = title,
                        addprecip = input$p.dis)
    
    
    
  })
  options(warn = -1)
})
