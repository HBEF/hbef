library(shiny)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(rio)
library(stringr)
library(plotly)
library(ggthemes)
library(devtools)
library(directlabels)
library(ggiraph)


function(input, output) {
  
  #DATA
  
  imported_data <- readRDS("precip_stream_data_long.rds")
  imported_diff_data <- readRDS("precip_stream_diff_data_long.rds")
  
  my_theme <- theme_fivethirtyeight() + 
    theme(rect = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "#dddddd"), 
          text = element_text(family = "Helvetica", size = 12), 
          legend.position = "none", legend.direction = "vertical", legend.title = element_blank(),
          strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
          axis.title= element_text(NULL), axis.title.x= element_blank(), 
          axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
  
  cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
  anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
  hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")
  
  solute_palette <- c(cation, anion, hydro)
  source_shapes <- c("flow" = 16, "precip"= 21)
  
  
  
  filter <- eventReactive(input$update, {input$filter})
  
  #Reactive Data Normal
  reactivedata <- eventReactive(input$update,{
    data <- imported_data
    if(input$filter == "Watershed"){
      data <- data[data$source %in% input$source,]
      data <- data[data$solute %in% input$solute,]
      data <- data[data$ws %in% input$filterws,]
      data
    }
    else if(input$filter == "Water Source"){
      data <- data[data$source %in% input$filtersource,]
      data <- data[data$solute %in% input$solute,]
      data <- data[data$ws %in% input$ws,]
      data
    }
    else{
      data <- data[data$source %in% input$source,]
      data <- data[data$solute%in% input$filtersolute,]
      data <- data[data$ws %in% input$ws,]
      data
    }
  })
  
  
  #Reactive Data Difference
  reactive_diff_data <- eventReactive(input$update,{
    data_diff <- imported_diff_data
    if(input$filter == "Watershed"){
      data_diff <- data_diff[data_diff$solute %in% input$solute,]
      data_diff <- data_diff[data_diff$ws %in% input$filterws,]
      data_diff
    }
    else{
      data_diff <- data_diff[data_diff$solute%in% input$filtersolute,]
      data_diff <- data_diff[data_diff$ws %in% input$ws,]
      data_diff
    }
  })
  
  units <- eventReactive(input$update,{
    input$units})
  
  x <- eventReactive(input$update,{
    if(input$granularity == "Month"){"water_date"}
    else if(input$granularity == "Year"){"water_year"}
  })
  
  y <- eventReactive(input$update,{
    if(input$granularity == "Month" & input$units =="uMg/L"){"concentration_mg"}
    else if(input$granularity == "Year" & input$units =="uMg/L"){"mg_weighted_average"}
    else if(input$granularity == "Month" & input$units =="uEquivalent/L"){"concentration_ueq"}
    else if(input$granularity == "Year" & input$units =="uEquivalent/L"){"ueq_weighted_average"}
    else if(input$granularity == "Month"& input$units =="uMole/L"){"concentration_umol"}
    else if(input$granularity == "Year"& input$units =="uMole/L"){"umol_weighted_average"}
    else if(input$granularity == "Month"& input$units =="flux"){"flux"}
    else if(input$granularity == "Year"& input$units =="flux"){"flux_sum"}
  })
  
  variable_a <- reactive({
    if(filter() == "Watershed"){"source"}
    else if(filter() == "Solute"){"source"}
    else{"solute"}
  })
  
  variable_b <- reactive({
    if(filter() == "Watershed"){"solute"}
    else if(filter() == "Solute"){"ws"}
    else{"ws"}
  })
  
  
  ## GGPLOT TIME FUNCTIONS
  ggplot_function <- function(data, x, y, color, facet, ncol = NULL, nrow = NULL){
    ggplotly(  
      (ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws)) + my_theme +
         geom_line(size = 1)+ 
         geom_point(size = 1.5, fill = "white", stroke = 0.5) + 
         facet_wrap(~get(facet) , ncol = ncol)+ 
         xlim(min(input$timeframe[1]), max(input$timeframe[2]))+ 
         labs(x = "Water Year", y = units())+ 
         scale_shape_manual(values = source_shapes) +
         scale_color_manual(values = solute_palette) +
         scale_alpha_discrete(range = c(0.9, 0.5))), 
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)  
  }
  
  ggplot_log_function <- function(data, x, y, color, facet, ncol = NULL, nrow = NULL){
    ggplotly(
      ggplot(data=data, aes(x = get(x), y = log(get(y)) ,color = solute, alpha = ws)) + my_theme +
        geom_line(size = 1)+ 
        geom_point(size = 3) + 
        facet_wrap(~get(facet) , ncol = 1)+ 
        xlim(min(input$timeframe[1]), max(input$timeframe[2]))+ 
        labs(x = "Water Year", y = paste("log", "(",units(), ")"))+ 
        scale_shape_manual(values = source_shapes) +
        scale_color_manual(values = solute_palette) + 
        scale_alpha_discrete(range = c(0.9, 0.5)), 
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  }
  
  ggplot_diff_function <- function(data, x, y, facet, ncol = NULL, nrow = NULL){
    ggplotly(
      (ggplot(data=data, aes(x = get(x), y = get(y)))+ my_theme +
         geom_bar(stat= "identity", aes(fill = solute))+
         facet_wrap(~get(facet) , ncol = ncol)+
         scale_fill_manual(values = solute_palette)), 
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)}
  
  output$view1a <- renderPlotly({
    
    if(input$logscale){
      ggplot_log_function(reactivedata(), x(), y(), variable_a(), variable_b(), ncol = 1)
    }
    
    else{
      ggplot_function(reactivedata(), x(), y(), variable_a(), variable_b(), ncol = 1)
    }  })
  
  
  output$view1b <- renderPlotly({
    if(input$logscale){
    }
    else{ggplot_diff_function(reactive_diff_data(), x(), y(), variable_b(), ncol = 1)
      
    }
    
  })
  
  
  output$view2 <- renderPlotly({
    if(input$logscale){
      ggplot_log_function(reactivedata(), x(), y(), variable_b(), variable_a(), ncol = 1)
    }
    
    else{
      ggplot_function(reactivedata(), x(), y(), variable_b(), variable_a(), ncol = 1)
    } })
  
}