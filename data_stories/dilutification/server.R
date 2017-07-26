library(ggplot2)
library(lubridate)
library(gridExtra)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(utils)
library(grid)
library(ggthemes)
library(directlabels)



#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################
#Write a function that converts the Source code from precip and 
#flow to Precipitation and Streamwater Discharge
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
  df[df$solute == "H", "solute"] = "Hydrogen"
  return(df)
  
}

source_change <- function(df){
  df[df$source == "flow", "source"] = "Streamflow (Q)"
  df[df$source == "precip", "source"] = "Precipitation (P)"
  return(df)
}

shinyServer(function(session, input, output) {
  
  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  Theme  ################
  my_theme <-theme(rect = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "#dddddd"),
          strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
          axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
  
  color_cation <- c("Potassium" = "#95AFDD", "Sodium" = "#7195D2", "Calcium" = "#3B5C95",
                    "Magnesium" = "#273D64", "Aluminum" = "#162338")
  color_anion <- c("Sulfate" = "#8F1010", "Nitrate" = "#BF1616", "Chloride" = "#D97373")
  color_hydro <- c("Hydrogen" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("Streamflow (Q)" = 16, "Precipitation (P)"= 21)
  
  ### End of Theme ################
  
  ###  Lists for the sidebar  ######
  #Edit if there are values that do not appear or are not relevant to your data. 
  #Should be the same as the lists in the UI file.
  
  watersheds <- list("Watershed 1" = 1,
                     "Watershed 2" = 2, 
                     "Watershed 3" = 3,
                     "Watershed 4" = 4,
                     "Watershed 5" = 5,
                     "Watershed 6" = 6,
                     "Watershed 7" = 7,
                     "Watershed 8" = 8,
                     "Watershed 9" = 9)
  
  solutes_cations <- list("Potassium (K)" = "K",
                          "Sodium (Na)" = "Na",
                          "Calcium (Ca)" = "Ca",
                          "Magnesium (Mg)" = "Mg",
                          "Aluminum (Al)" = "Al")
  
  solutes_anions <- list("Sulfate (SO4)" = "SO4",
                         "Nitrate (NO3)" = "NO3",
                         "Chloride (Cl)" = "Cl")
  
  ########### END OF IMPORTANT PRELIMINARY INFO #############################################
  
  
  
  
  ########### SIDEBAR FUNCTIONS ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  observeEvent(input$select_all_ions, {
    if(input$select_all_ions == 0) {}
    else if (input$select_all_ions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations", selected = "K")
      updateCheckboxGroupInput(session, "solutes_H", selected = "H")}
    else{
      updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)
      updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)
      updateCheckboxGroupInput(session, "solutes_H", selected = solutes_H)}
  })
  
  observeEvent(input$select_all_anions, {
    if(input$select_all_anions == 0) {}
    else if (input$select_all_anions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "PO4")}
    else{updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)}
  })
  
  observeEvent(input$select_all_cations, {
    if(input$select_all_cations == 0) {}
    else if (input$select_all_cations%%2 == 0){updateCheckboxGroupInput(session, "solutes_cations", selected = "K")}
    else{updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)}
  })
  
  observeEvent(input$select_all_H, {
    if(input$select_all_H == 0) {}
    else if (input$select_all_H%%2 == 0){updateCheckboxGroupInput(session, "solutes_H", selected = "H")}
    else{updateCheckboxGroupInput(session, "solutes_H", selected = solutes_H)}
  })
  
  observeEvent(input$select_all_ws, {
    if(input$select_all_ws == 0) {updateSelectInput(session, "watersheds", selected = 1)}
    else if (input$select_all_ws%%2 == 0){updateSelectInput(session, "watersheds", selected =  1)}
    else{updateSelectInput(session, "watersheds", selected = watersheds)}
  })
  
  
  
  solutes2 <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################
  
  
  
  
  ########### DATA IMPORT ####################################################
  
  imported_data <- readRDS("precip_stream_data_long.rds")
  imported_data <- watershed_change(imported_data)
  
  
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y  #########################################
  #Reactive Data Normal
  add_precip <- reactive({
    if (input$water_sources == "precip"){
      c("precip", "flow")
    }else{
      "flow"
    }
  })
  
  add_precip2 <- reactive({
    if (input$water_sources2 == "precip"){
      c("precip", "flow")
    }else{
      "flow"
    }
  })
  reactive_data <- reactive({
    data <- imported_data
    data <- data[data$source %in% add_precip(),]
    data <- data[data$solute %in% input$sol,] 
    data <- data[data$ws %in% input$watersheds,]
    data <- solute_change(data)
    data <- source_change(data)
  })
  
  reactive_data2 <- reactive({
    data <- imported_data
    data <- data[data$source %in% add_precip2(),]
    data <- data[data$solute %in% solutes2(),] 
    #note that solutes2 is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
    data <- solute_change(data)
    data <- source_change(data)
  })
  
  x <- reactive({
    if(input$granularity == "month"){"water_date"}
    else if(input$granularity == "year"){"water_year"}
  })
  
  x2 <- reactive({
    if(input$granularity2 == "month"){"water_date"}
    else if(input$granularity2 == "year"){"water_year"}
  })
  
  y <- reactive({
    if(input$granularity == "month" & input$units =="mg/L"){"concentration_mg"}
    else if(input$granularity == "year" & input$units =="mg/L"){"mg_weighted_average"}
    else if(input$granularity == "month" & input$units =="ueq/L"){"concentration_ueq"}
    else if(input$granularity == "year" & input$units =="ueq/L"){"ueq_weighted_average"}
    else if(input$granularity == "month"& input$units =="umol/L"){"concentration_umol"}
    else if(input$granularity == "year"& input$units =="umol/L"){"umol_weighted_average"}
  })
  
  y2 <- reactive({
    if(input$granularity2 == "month" & input$units2 =="mg/L"){"concentration_mg"}
    else if(input$granularity2 == "year" & input$units2 =="mg/L"){"mg_weighted_average"}
    else if(input$granularity2 == "month" & input$units2 =="ueq/L"){"concentration_ueq"}
    else if(input$granularity2 == "year" & input$units2 =="ueq/L"){"ueq_weighted_average"}
    else if(input$granularity2 == "month"& input$units2 =="umol/L"){"concentration_umol"}
    else if(input$granularity2 == "year"& input$units2 =="umol/L"){"umol_weighted_average"}
  })
  

  
  ########### PLOT FUNCTIONS #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function <- function(data, x, y,facet, w.s, ion, log,
                              units, date.input, source_shapes, source_palette){
    if (length(ion) == 1){
      if (length(w.s) %in% c(1,2,3)){
        col1 = 1
        h = 400
      }else if (length(w.s) == 4){
        col1 = 2
        h = 400
      }else if (length(w.s) == 5){
        col1 = 1
        h = 600
      }else if (length(w.s) == 6){
        col1 = 2
        h = 600
      }else if (length(w.s) == 7){
        col1 = 1
        h = 800
      }else if (length(w.s) == 8){
        col1 = 2
        h = 800
      }else if (length(w.s) == 9){
        col1 = 3
        h = 800
      }
    }
    if (length(w.s) == 1){
      if (length(ion) %in% c(1,2, 3)){
        col2 = 1
        h = 400
      }else if (length(ion) == 4){
        col2 = 2
        h = 400
      }else if (length(ion) == 5){
        col2 = 1
        h = 600
      }else if (length(ion) == 6){
        col2 = 2
        h = 600
      }else if (length(ion) == 7){
        col2 = 1
        h = 800
      }else if (length(ion) == 8){
        col2 = 2
        h = 800
      }else if (length(ion) == 9){
        col2 = 3
        h = 800
      }else{
        col2 = 1
        h = 800
      }
    }
    
    if(log == "log") {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), 
                                   shape = source, color = solute))+
        labs(x = "Water Year", y = paste("log", "(",units, ")", sep = ""))
    
    }else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), 
                                    shape = source, color = solute))+
        labs(x = "Water Year", y = units)}
    
    if (facet == "w.s"){
      if (length(w.s) <= 1) {
        final <- plot+ geom_line(size = 1,aes(color = solute)) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute, shape = source,
                       text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                       "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          labs(title = paste(data$solute, "Concentration", sep = " ")) +
          coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                   as.Date(date.input[2])))+
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5)) +
          theme_bw() +
          scale_x_date(date_breaks = "10 years", date_labels = "%Y")
        
      }else{
        final <- plot+ geom_line(size = 1,aes(color = solute)) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute, shape = source,
                         text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                      "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
          coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                   as.Date(date.input[2])))+
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          facet_wrap(~ws2, ncol = col1, scales = "free") +
          labs(title = paste(data$solute, "Concentrations", sep = " ")) +
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5))+
          theme_bw() +
          scale_x_date(date_breaks = ifelse(col1 %in% c(2,3), "20 years", 
                              "10 years"),
                       date_labels = "%Y")
        
      }
    }else{
      if (length(ion) <= 1){
        final <- plot+ geom_line(size = 1,aes(color = solute)) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute, shape = source,
                         text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                      "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) +
          coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                   as.Date(date.input[2]))) +
          labs(title = paste(data$solute, "Concentration", sep = " ")) +
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5)) +
          theme_bw() +
          scale_x_date(date_breaks = "10 years", date_labels = "%Y")
      }else{
        final <- plot+ geom_line(size = 1,aes(color = solute)) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute, shape = source,
                         text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                      "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                   as.Date(date.input[2])))+
          labs(title = "Solute Concentrations") +
          facet_wrap(~solute, ncol = col2, scales = "free") +
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5))+
          theme_bw() +
          scale_x_date(date_breaks = ifelse(col2 %in% c(2,3), "20 years", 
                              "10 years"),
                       date_labels = "%Y")
      }
    }
    
    p = hide_guides(ggplotly(  
      final, tooltip = "text",
      width = 1000, height = h))
  
    return(p)
    
  }
  
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  output$plot1 <- renderPlotly({
    theplot <- ggplot_function(reactive_data(), x(), y(), facet = "w.s",
                               w.s = input$watersheds, ion = input$sol,
                               log = input$log1,
                               units = input$units,
                               date.input = input$date_range,
                               source_palette = source_palette,
                               source_shapes = source_shapes)
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
    layout(margin = list(b = 90))
  })
  
  output$plot2 <- renderPlotly({
    theplot <- ggplot_function(reactive_data2(), x2(), y2(), facet = "solutes",
                               w.s = input$watersheds2, ion = solutes2(),
                               log = input$log2,
                               units = input$units2,
                               date.input = input$date_range2,
                               source_palette = source_palette,
                               source_shapes = source_shapes)
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
    layout(margin = list(b = 90))
  })
  
  
  
})
