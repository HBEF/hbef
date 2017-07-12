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
load("precip_streamflow_dfs.RData")
imported_data <- precip_streamflow_long

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
  df[df$source == "streamflow", "source"] = "Streamflow (Q)"
  df[df$source == "precipitation", "source"] = "Precipitation (P)"
  return(df)
}


#Function to plot the formatted data frame in ggplot2
plot.solute.df <- function(df, timescale, x.r, y.r, date.input, y.lab, title.lab, logarithm){
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
  v.line <- data.frame(ws2 = c("Watershed 2", "Watershed 4",
                               "Watershed 5", "Watershed 6"), 
                       vals = c(-1675, 92, 4926, NA))
  if (logarithm == TRUE) {
    gg <- ggplot(df,aes(x= get(x.r),y= log(get(y.r)), shape =source, 
                        color = solute, label=date))
  }else {
    gg <- ggplot(df,aes(x= get(x.r),y= get(y.r), shape =source, 
                        color = solute, label=date))
  }
  p <- gg +
    geom_line() +
    geom_point(fill = "white",
               aes(text = paste("Solute: ", solute, "<br>", 
                                "Water Source: ", source, "<br>",
                                "Value:", round(get(y.r), 2), "<br>", 
                                "Date: ", get(x.r)))) +
    scale_shape_manual(values = source_shapes) +
    scale_color_manual(values = solutes_palette) +
    coord_cartesian(xlim = c(as.Date(date.input[1]), 
                             as.Date(date.input[2])))+
    labs(x = "Date (In Water Years)", 
         y = y.lab,
         title = title.lab)+
    scale_shape_manual(values = source_shapes) +
    facet_wrap(~ws2, ncol = 1) +
    geom_vline(data = v.line,
               aes(xintercept = vals),
               linetype = 1,
               show.legend = T) +
    theme_bw() + theme
  plot <- ggplotly(p,tooltip = "text",
                   width = 400, height = 600) %>%
    layout(margin = list(b = 90)) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE)
  
  return(plot)
}

plot.solute.eco <- function(df, x.r, y.r, date.input, y.lab, title.lab){
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
  p <- ggplot(df,aes(x= get(x.r),y= get(y.r), shape =source, 
                     color = solute, label=date)) +
    theme
}
plot.dis.df <- function(df, x.r, date.input, y.lab, title.lab, logarithm){
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
  
  v.line <- data.frame(ws2 = c("Watershed 2", "Watershed 4",
                               "Watershed 5", "Watershed 6"), 
                       vals = c(-1675, 92, 4926, NA))
  if (logarithm == TRUE){
    gg <- ggplot(df,aes(x= get(x.r),y= log(water_mm), shape =source, label=date))
  }else{
    gg <- ggplot(df,aes(x= get(x.r),y= water_mm, shape =source, label=date))
  }
  p <- gg +
    geom_line(color = "red") +
    geom_point(color = "blue", fill = "white",
               aes(text = paste("Water Source: ", source, "<br>",
                                "Value:", round(water_mm, 2), "<br>", 
                                "Date: ", get(x.r))))  +
    coord_cartesian(xlim = c(as.Date(date.input[1]), 
                             as.Date(date.input[2])))+
    scale_shape_manual(values = source_shapes) +
    scale_color_manual(values = solute_palette) +
    labs(x = "Date (In Water Years)", 
         y = y.lab,
         title = title.lab)+
    scale_shape_manual(values = source_shapes) +
    facet_wrap(~ws2, ncol = 1) +
    geom_vline(data = v.line,
               aes(xintercept = vals),
               linetype = 1,
               show.legend = T) +
    theme_bw() + theme
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
  
  sources <- reactive({
    if (input$p == "precip"){
      c("Precipitation (P)", "Streamflow (Q)")
    }else{
      c("Streamflow (Q)")
    }
  })
  
  solute_data <- reactive({
    data <- imported_data
    data <- watershed_change(data)
    data <- solute_change(data)
    data <- source_change(data)
    data <- data[data$source %in% sources(),]
    data <- data[data$solute %in% solutes(),] 
    data <- data[data$ws %in% c(2,4,5,6),]
    data <- data[data$granularity %in% input$granularity,]
    
  })
  
  solute_data_eco <- reactive({
    data <- imported_data
    data <- watershed_change(data)
    data <- solute_change(data)
    data <- source_change(data)
    data <- data[data$solute %in% solutes(),] 
    data <- data[data$ws %in% c(2,4,5,6),]
    data <- data[data$granularity %in% input$granularity,]
    
  })
  
  x <- reactive({
    if(input$granularity == "week"){"water_date"}
    else if(input$granularity == "month"){"water_date"}
    else if(input$granularity == "year"){"water_year"}
  })
  
  y <- reactive({
    if(input$units =="umg/L"){"concentration_mg"}
    else if(input$units =="ueq/L"){"concentration_ueq"}
    else if(input$units =="umol/L"){"concentration_umol"}
    else if(input$units =="Eq/ha-yr"){"flux"}
  })
  
  sources.dis <- reactive({
    if (input$p.dis == "precip"){
      c("Precipitation (P)", "Streamflow (Q)")
    }else{
      c("Streamflow (Q)")
    }
  })
  discharge_data <- reactive({
    data <- imported_data
    data <- watershed_change(data)
    data <- solute_change(data)
    data <- source_change(data)
    data <- data[data$source %in% sources.dis(),]
    data <- data[data$solute %in% c("Calcium"),] 
    data <- data[data$ws %in% c(2,4,5,6),]
    data <- data[data$granularity %in% input$granularity2,]
  })
  
  discharge_data_eco <- reactive({
    data <- imported_data
    data <- watershed_change(data)
    data <- solute_change(data)
    data <- source_change(data)
    data <- data[data$solute %in% c("Calcium"),] 
    data <- data[data$ws %in% c(2,4,5,6),]
    data <- data[data$granularity %in% input$granularity2,]
  })
  
  x2 <- reactive({
    if(input$granularity2 == "week"){"water_date"}
    else if(input$granularity2 == "month"){"water_date"}
    else if(input$granularity2 == "year"){"water_year"}
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
    
    plot.solute.df(df = solute_data(),
                   x.r = x(),
                   y.r = y(),
                   date.input = input$dates,
                   y.lab = y,
                   title.lab = title,
                   logarithm = input$ln)
  })
  
  #Plot output for discharge quantity
  output$d.plot <- renderPlotly({
    if (input$p.dis == "precip"){
      if (input$ln.dis == FALSE){
        y <- "mm"
        title <- "Streamflow and Precipitation Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Water Quantities"
      }
    }else{
      if (input$ln.dis == FALSE){
        y <- "mm"
        title <- "Streamflow Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Streamflow Quantities"
      }
    }
    
    
    plot.dis.df(df = discharge_data(), 
                x.r = x2(),
                date.input = input$dates.dis,
                y.lab = y, title.lab = title,
                logarithm = input$ln.dis)
    
    
    
  })
  options(warn = -1)
})
