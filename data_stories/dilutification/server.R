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


shinyServer(function(session, input, output) {
  
  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  Theme  ################
  my_theme <-theme(rect = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "#dddddd"),
          strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
          axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
  
  color_cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
  color_anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("flow" = 16, "precip"= 21)
  
  ### End of Theme ################
  
  ###  Lists for the sidebar  ######
  #Edit if there are values that do not appear or are not relevant to your data. 
  #Should be the same as the lists in the UI file.
  
  watersheds <- list("Watershed 1" = "Watershed 1",
                     "Watershed 2" = "Watershed 2", 
                     "Watershed 3" = "Watershed 3",
                     "Watershed 4" = "Watershed 4",
                     "Watershed 5" = "Watershed 5",
                     "Watershed 6" = "Watershed 6",
                     "Watershed 7" = "Watershed 7",
                     "Watershed 8" = "Watershed 8",
                     "Watershed 9" = "Watershed 9")
  
  solutes_cations <- list("Potassium (K)" = "K",
                          "Sodium (Na)" = "Na",
                          "Calcium (Ca)" = "Ca",
                          "Magnesium (Mg)" = "Mg",
                          "Aluminum (Al)" = "Al")
  
  solutes_anions <- list("Sulfate (SO4)" = "SO4",
                         "Nitrate (NO3)" = "NO3",
                         "Chlorine (Cl)" = "Cl")
  
  ########### END OF IMPORTANT PRELIMINARY INFO #############################################
  
  
  
  
  ########### SIDEBAR FUNCTIONS ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  observeEvent(input$select_all_ions2, {
    if(input$select_all_ions2 == 0) {
    }else if (input$select_all_ions2%%2 == 0){
      updateCheckboxGroupInput(session, "solutes_anions2", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations2", selected = "K")
    }else{
      updateCheckboxGroupInput(session, "solutes_anions2", selected = solutes_anions2)
      updateCheckboxGroupInput(session, "solutes_cations2", selected = solutes_cations2)
    }
  })
  
  observeEvent(input$select_all_anions2, {
    if(input$select_all_anions2 == 0) {
    }else if (input$select_all_anions2%%2 == 0){
      updateCheckboxGroupInput(session, "solutes_anions2", selected = "PO4")
    }else{
      updateCheckboxGroupInput(session, "solutes_anions2", selected = solutes_anions2)
    }
  })
  
  observeEvent(input$select_all_cations2, {
    if(input$select_all_cations2 == 0) {
    }else if (input$select_all_cations2%%2 == 0){
      updateCheckboxGroupInput(session, "solutes_cations2", selected = "K")
    }else{
      updateCheckboxGroupInput(session, "solutes_cations2", selected = solutes_cations2)
    }
  })
  
  observeEvent(input$select_all_ws, {
    if(input$select_all_ws == 0) {
      updateCheckboxGroupInput(session, "watersheds", selected = "ws1")
    }else if (input$select_all_ws%%2 == 0){
      updateCheckboxGroupInput(session, "watersheds", selected = "ws1")
    }else{
      updateCheckboxGroupInput(session, "watersheds", selected = watersheds)
    }
  })
  
  
  solutes2 <- reactive({c(input$solutes_cations2, input$solutes_anions2, input$solutes_H2)})
  
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
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds,]
  })
  
  reactive_data2 <- reactive({
    data <- imported_data
    data <- data[data$source %in% add_precip2(),]
    data <- data[data$solute %in% solutes2(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
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
    if(input$granularity == "month" & input$units =="umg/L"){"concentration_mg"}
    else if(input$granularity == "year" & input$units =="umg/L"){"mg_weighted_average"}
    else if(input$granularity == "month" & input$units =="ueq/L"){"concentration_ueq"}
    else if(input$granularity == "year" & input$units =="ueq/L"){"ueq_weighted_average"}
    else if(input$granularity == "month"& input$units =="umol/L"){"concentration_umol"}
    else if(input$granularity == "year"& input$units =="umol/L"){"umol_weighted_average"}
  })
  
  y2 <- reactive({
    if(input$granularity2 == "month" & input$units2 =="umg/L"){"concentration_mg"}
    else if(input$granularity2 == "year" & input$units2 =="umg/L"){"mg_weighted_average"}
    else if(input$granularity2 == "month" & input$units2 =="ueq/L"){"concentration_ueq"}
    else if(input$granularity2 == "year" & input$units2 =="ueq/L"){"ueq_weighted_average"}
    else if(input$granularity2 == "month"& input$units2 =="umol/L"){"concentration_umol"}
    else if(input$granularity2 == "year"& input$units2 =="umol/L"){"umol_weighted_average"}
  })
  
  log_transform <- reactive({
    if(input$log == "ln"){"transform"}
    else{"no_transform"}
  })
  
  log_transform2 <- reactive({
    if(input$log2 == "ln"){"transform"}
    else{"no_transform"}
  })
  
  ########### PLOT FUNCTIONS #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function <- function(data, x, y, ncol = NULL, nrow = NULL, facet, w.s, ion, log,
                              units, date_range, source_shapes, source_palette){
    if (length(ion) == 1){
      if (length(w.s) %in% c(1,2,3)){
        col1 = 1
        h = 400
      }else if (length(w.s) == 4){
        col1 = 2
        h = 400
      }else if (length(w.s) == 5){
        col1 = 1
        h = 500
      }else if (length(w.s) == 6){
        col1 = 2
        h = 500
      }else if (length(w.s) == 7){
        col1 = 1
        h = 600
      }else if (length(w.s) == 8){
        col1 = 2
        h = 600
      }else if (length(w.s) == 9){
        col1 = 3
        h = 600
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
        h = 500
      }else if (length(ion) == 6){
        col2 = 2
        h = 500
      }else{
        col2 = 1
        h = 500
      }
    }
    
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), 
                                   shape = source, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",units, ")", sep = ""),
             title = "Solute Concentrations")}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), 
                                    shape = source, alpha = ws))+
        labs(x = "Water Year", y = units,
             title = "Solute Concentrations")}
    
    if (facet == "w.s"){
      if (length(w.s) <= 1) {
        final <- plot+ geom_line(size = 1,aes(color = solute)) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute,
                       text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                       "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          xlim(min(date_range[1]), max(date_range[2]))+ 
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5)) +
          guides(color = FALSE, alpha = FALSE) 
        
      }else{
        final <- plot+ geom_line(size = 1,aes(color = solute)) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute,
                         text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                      "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
          xlim(min(date_range[1]), max(date_range[2]))+ 
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          facet_wrap(~ws2, ncol = col1) +
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5))+
          guides(color = FALSE, alpha = FALSE)
        
      }
    }else{
      if (length(ion) <= 1){
        final <- plot+ geom_line(size = 1,aes(color = solute), guide = FALSE) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute,
                         text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                      "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) +
          xlim(min(date_range[1]), max(date_range[2]))+ 
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5)) +
          guides(color = FALSE, alpha = FALSE) 
      }else{
        final <- plot+ geom_line(size = 1,aes(color = solute), guide = FALSE) + 
          geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                     aes(color = solute,
                         text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                      "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
          geom_smooth(method = "lm", color = "green", se = FALSE) +
          xlim(min(date_range[1]), max(date_range[2]))+ 
          facet_wrap(~solute, ncol = col2) +
          scale_shape_manual(values = source_shapes) +
          scale_color_manual(values = solute_palette) +
          scale_alpha_discrete(range = c(0.9, 0.5))+
          guides(color = FALSE, alpha = FALSE)
      }
    }
    
    p = hide_guides(ggplotly(  
      final, tooltip = "text",
      width = 900, height = h) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE))
  
    return(p)
    
  }
  
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  output$plot1 <- renderPlotly({
    theplot <- ggplot_function(reactive_data(), x(), y(), facet = "w.s",
                               w.s = input$watersheds, ion = input$sol,
                               ncol = 1, log = input$log,
                               units = input$units,
                               date_range = input$date_range,
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
                               ncol = 1, log = input$log2,
                               units = input$units,
                               date_range = input$date_range,
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
