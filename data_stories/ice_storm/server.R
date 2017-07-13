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
library(reshape2)

#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################


shinyServer(function(session, input, output) {
  
  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  Theme  ################
  my_theme <- theme_fivethirtyeight() + 
    theme(rect = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "#dddddd"), 
          text = element_text(family = "Helvetica", size = 12), 
          legend.position = "none", legend.direction = "vertical", legend.title = element_blank(),
          strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
          axis.title= element_text(NULL), axis.title.x= element_blank(), 
          axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
  
  color_cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
  color_anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("streamflow" = 16, "precipitation"= 21)
  watershed_shapes <- c("1"= 5, "2"= 2, "3"= 16, "4"= 1, "5"= 0, "6"= 17)
  
  ### End of Theme ################
  
  ###  Lists for the sidebar  ######
  #Edit if there are values that do not appear or are not relevant to your data. 
  #Should be the same as the lists in the UI file.
  
  watersheds <- list("Watershed 1" = "1",
                     "Watershed 2" = "2", 
                     "Watershed 3" = "3",
                     "Watershed 4" = "4",
                     "Watershed 5" = "5",
                     "Watershed 6" = "6",
                     "Watershed 7" = "7",
                     "Watershed 8" = "8",
                     "Watershed 9" = "9")
  
  solutes_cations <- list("Aluminum (Al)" = "Al",
                          "Magnesium (Mg)" = "Mg",
                          "Calcium (Ca)" = "Ca",
                          "Sodium (Na)" = "Na",
                          "Potassium (K)" = "K")
  
  solutes_anions <- list("Phosphate (PO4)" = "PO4",
                         "Sulfate (SO4)" = "SO4",
                         "Nitrate (NO3)" = "NO3",
                         "Silicon Dioxide (SiO2)" = "SiO2",
                         "Chloride (Cl)" = "Cl",
                         "Bicarbonate (HCO3)" = "HCO3")
  
  solutes_H <- list("Hydrogen (H)" = "H",
                    "pH" = "pH")
  
  all_solutes <- c(solutes_cations, solutes_anions, solutes_H)
  
  ########### END OF IMPORTANT PRELIMINARY INFO #############################################
  
  
  
  
  ########### SIDEBAR FUNCTIONS ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  observeEvent(input$select_all_ions, {
    if(input$select_all_ions == 0) {}
    else if (input$select_all_ions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations", selected = "K")}
    else{
      updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)
      updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)}
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
  
  observeEvent(input$select_all_ws, {
    if(input$select_all_ws == 0) {updateCheckboxGroupInput(session, "watersheds", selected = "ws1")}
    else if (input$select_all_ws%%2 == 0){updateCheckboxGroupInput(session, "watersheds", selected = "ws1")}
    else{updateCheckboxGroupInput(session, "watersheds", selected = watersheds)}
  })
  
  observeEvent(input$select_all_ws2, {
    if(input$select_all_ws2 == 0) {updateCheckboxGroupInput(session, "watersheds2", selected = "ws1")}
    else if (input$select_all_ws2%%2 == 0){updateCheckboxGroupInput(session, "watersheds2", selected = "ws1")}
    else{updateCheckboxGroupInput(session, "watersheds2", selected = watersheds)}
  })
  
  solutes_NO3 <- reactive({c(input$solutes_NO3)})
  solutes_NO33 <- reactive({c(input$solutes_NO33)})
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################
  
  
  
  
  ########### DATA IMPORT ####################################################
  
  lai_data <- read_csv("lai.txt")
  # lai_data <- read_csv("D:/Duke/Work(Environ)/Programming/hbef/data_stories/ice_storm/lai.txt")
  load("D:/Duke/Work(Environ)/Programming/hbef/data_stories/ice_storm/precip_streamflow_dfs.RData")
  load("precip_streamflow_dfs.RData")
  imported_data <- precip_streamflow_long
  
  #Matt normalization####################
  #####YEARLY#####
  ws_cast_year <- imported_data %>%
    filter(granularity=='year') %>% 
    filter(source=='streamflow') %>%
    filter(solute=='NO3') %>%
    dcast(.,date+water_year+solute~ws,value.var='flux')
  
  #normalize data by subtracting ws6 from each
  ws_cast_year$"n2" <- ws_cast_year$"2"-ws_cast_year$"6"
  ws_cast_year$"n4" <- ws_cast_year$"4"-ws_cast_year$"6"
  ws_cast_year$"n5" <- ws_cast_year$"5"-ws_cast_year$"6"
  
  #rename ws columns so I can name normalized columns just numbers to make merging easier
  names(ws_cast_year)[names(ws_cast_year) == "2"] <- "ws2"
  names(ws_cast_year)[names(ws_cast_year) == "4"] <- "ws4"
  names(ws_cast_year)[names(ws_cast_year) == "5"] <- "ws5"
  names(ws_cast_year)[names(ws_cast_year) == "n2"] <- "2"
  names(ws_cast_year)[names(ws_cast_year) == "n4"] <- "4"
  names(ws_cast_year)[names(ws_cast_year) == "n5"] <- "5"
  
  #melt function to get them all back together (new tidyr version is spread)
  ws_cast_year <- melt(ws_cast_year, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
       variable.name = "ws", value.name = "normalized_flux")
  
  #add granularity column with "year"
  ws_cast_year$granularity <- rep("year", nrow(ws_cast_year))
  
  #repeat for month and week
  #####MONTHLY#####
  ws_cast_month <- imported_data %>%
    filter(granularity=='month') %>% 
    filter(source=='streamflow') %>%
    filter(solute=='NO3') %>%
    dcast(.,date+water_year+solute~ws,value.var='flux')
  
  #normalize data by subtracting ws6 from each
  ws_cast_month$"n2" <- ws_cast_month$"2"-ws_cast_month$"6"
  ws_cast_month$"n4" <- ws_cast_month$"4"-ws_cast_month$"6"
  ws_cast_month$"n5" <- ws_cast_month$"5"-ws_cast_month$"6"
  
  #rename ws columns so I can name normalized columns just numbers to make merging easier
  names(ws_cast_month)[names(ws_cast_month) == "2"] <- "ws2"
  names(ws_cast_month)[names(ws_cast_month) == "4"] <- "ws4"
  names(ws_cast_month)[names(ws_cast_month) == "5"] <- "ws5"
  names(ws_cast_month)[names(ws_cast_month) == "n2"] <- "2"
  names(ws_cast_month)[names(ws_cast_month) == "n4"] <- "4"
  names(ws_cast_month)[names(ws_cast_month) == "n5"] <- "5"
  
  #melt function to get them all back together (new tidyr version is spread)
  ws_cast_month <- melt(ws_cast_month, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
                       variable.name = "ws", value.name = "normalized_flux")
  
  #add granularity column with "month"
  ws_cast_month$granularity <- rep("month", nrow(ws_cast_month))
  
  #####WEEKLY#####
  ws_cast_week <- imported_data %>%
    filter(granularity=='week') %>% 
    filter(source=='streamflow') %>%
    filter(solute=='NO3') %>%
    dcast(.,date+water_year+solute~ws,value.var='flux')
  
  #normalize data by subtracting ws6 from each
  ws_cast_week$"n2" <- ws_cast_week$"2"-ws_cast_week$"6"
  ws_cast_week$"n4" <- ws_cast_week$"4"-ws_cast_week$"6"
  ws_cast_week$"n5" <- ws_cast_week$"5"-ws_cast_week$"6"
  
  #rename ws columns so I can name normalized columns just numbers to make merging easier
  names(ws_cast_week)[names(ws_cast_week) == "2"] <- "ws2"
  names(ws_cast_week)[names(ws_cast_week) == "4"] <- "ws4"
  names(ws_cast_week)[names(ws_cast_week) == "5"] <- "ws5"
  names(ws_cast_week)[names(ws_cast_week) == "n2"] <- "2"
  names(ws_cast_week)[names(ws_cast_week) == "n4"] <- "4"
  names(ws_cast_week)[names(ws_cast_week) == "n5"] <- "5"
  
  #melt function to get them all back together (new tidyr version is spread)
  ws_cast_week <- melt(ws_cast_week, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
                        variable.name = "ws", value.name = "normalized_flux")
  
  #add granularity column with "week"
  ws_cast_week$granularity <- rep("week", nrow(ws_cast_week))
  #####
  #merge all ws_cast s together
  ws_cast <- merge(ws_cast_month, ws_cast_year, all = T)
  ws_cast <- merge(ws_cast, ws_cast_week, all = T)

  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y 2 #########################################
  #Reactive Data Normal
  
  reactive_data2 <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity,]
    data <- data[data$source %in% input$water_sources2,]
    data <- data[data$solute %in% solutes_NO3(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
  })
  
  
  x <- reactive({
    if(input$granularity == "week"){"water_date"}
    else if(input$granularity == "month"){"water_date"}
    else if(input$granularity == "year"){"water_year"}
  })
  
  y <- reactive({
    if(input$units =="mg/L"){"concentration_mg"}
    else if(input$units =="uEquivalent/L"){"concentration_ueq"}
    else if(input$units =="uMole/L"){"concentration_umol"}
    else if(input$units =="flux"){"flux"}
  })
  
  log_transform <- reactive({
    if(input$log == "ln"){"transform"}
    else{"no_transform"}
  })

  ########### REACTIVE DATA AND X Y 3 #########################################
  #Reactive Data Normal
  
  reactive_data3 <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity,]
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% solutes_NO33(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds3,]
  })
  
  reactive_data_norm <- reactive({
    data_norm <- merge(imported_data, ws_cast, all = T)
    data_norm <- data_norm[data_norm$granularity %in% input$granularity3,]
    data_norm <- data_norm[data_norm$source %in% c("streamflow"),]
    data_norm <- data_norm[data_norm$ws %in% c("2", "4", "5"),]
    data_norm <- data_norm[data_norm$solute %in% c("NO3"),]
  })
  
  x3 <- reactive({
    if(input$granularity3 == "week"){"water_date"}
    else if(input$granularity3 == "month"){"water_date"}
    else if(input$granularity3 == "year"){"water_year"}
  })
  
  y3 <- reactive({
    if(input$units3 =="mg/L"){"concentration_mg"}
    else if(input$units3 =="uEquivalent/L"){"concentration_ueq"}
    else if(input$units3 =="uMole/L"){"concentration_umol"}
    else if(input$units3 =="flux"){"flux"}
    else if (input$units3=="normalized_flux"){"normalized_flux"}
  })
  
  log_transform <- reactive({
    if(input$log3 == "ln"){"transform"}
    else{"no_transform"}
  })
  
  ########### PLOT FUNCTIONS 2 #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function <- function(data, x, y, ncol = NULL, nrow = NULL, log){
    
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",input$units, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = input$units)}
    
    final <- plot+ my_theme + geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes( text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range2[1]), max(input$date_range2[2]))+ 
      geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = 22, color = "black")+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }

  ########### PLOT FUNCTIONS 3 #########################################
  
  ## GGPLOT TIME FUNCTION 3.1
  ggplot_function3.1 <- function(data, x, y, ncol = NULL, nrow = NULL, log){
    
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = ws, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",input$units3, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = imported_data$flux, color = solute, shape = ws, alpha = ws))+
        labs(x = "Water Year", y = input$units3)}
    
    final <- plot+ my_theme + geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes( text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range3[1]), max(input$date_range3[2]))+ 
      geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = -8, color = "black")+
      scale_shape_manual(values = watershed_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }


  ## GGPLOT TIME FUNCTION _excess
  ggplot_function_excess <- function(data, x, y, ncol = NULL, nrow = NULL, log){
    
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",input$units3, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = input$units3)}
    
    final <- plot+ my_theme + geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes( text = paste("Solute: ", solute, "<br>", "Watershed: ", ws, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range3[1]), max(input$date_range3[2]))+ 
      geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = -8, color = "black")+
      scale_shape_manual(values = source_shapes)+
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }
  
    
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  #ggplotly that shows most plots increase in lai following the ice storm
  output$lai_plot <- renderPlotly({
    lai_plot <- ggplot(lai_data[lai_data$WS == input$watersheds1,], aes(x = YEAR, y = LAIT, color = ELEVATION_M))+
      geom_point(aes(text = paste("Year: ", YEAR, "<br>", "LAI: ", LAIT)))+
      geom_smooth(method = "lm", se = F, size = 0.5)+
      xlab(" ")+
      ylab("Leaf Area Index T")+
      facet_wrap(~PLOT)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    lai_plot <- ggplotly(lai_plot, tooltip = "text",
                         width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
    lai_plot$x$layout$width <- NULL
    lai_plot$y$layout$height <- NULL
    lai_plot$width <- NULL
    lai_plot$height <- NULL
    lai_plot %>%
      layout(autosize = TRUE, height = 600)
  })
  
  #plot to generally show how the ice storm affected NO3 (conc or flux?)
  output$NO3_plot <- renderPlotly({
    NO3_plot <- ggplot_function(reactive_data2(), x(), y(), ncol = 1, log = input$log) #probs should use Camila's theme
    
    NO3_plot$x$layout$width <- NULL
    NO3_plot$y$layout$height <- NULL
    NO3_plot$width <- NULL
    NO3_plot$height <- NULL
    NO3_plot %>%
      layout(autosize = TRUE, height = 600)
  })
  
  #make a plot of nitrates like in the 2003 paper
  #(moles/ha-yr (flux) vs water year, faceted into output for ws1,6 and excess (norm) for ws2,4,5)
  #have line for ws1 and ws6 show up on same graph... write an if statement when weekly data is figured out
  output$NO3_output <- renderPlotly({
    NO3_output <- ggplot_function3.1(reactive_data3(), x3(), y, ncol = 1, log = input$log3)
    NO3_output$x$layout$width <- NULL
    NO3_output$y$layout$height <- NULL
    NO3_output$width <- NULL
    NO3_output$height <- NULL
    NO3_output %>%
      layout(autosize = TRUE, height = 600)
  })
  #simple version of NO3 output to help see what should be interactive, etc
  output$static_NO3_output <- renderPlotly({
    streamflow_NO3_data <- imported_data
    streamflow_NO3_data <- streamflow_NO3_data[streamflow_NO3_data$source %in% c("streamflow"),]
    streamflow_NO3_data <- streamflow_NO3_data[streamflow_NO3_data$solute %in% c("NO3"),]
    streamflow_NO3_data <- streamflow_NO3_data[streamflow_NO3_data$granularity %in% input$granularity3,]
    
    static_NO3_output <- ggplot(NULL, aes(get(x3()), y = flux, color = "#BF1616", shape = ws))+ my_theme+
      geom_line(data = streamflow_NO3_data[streamflow_NO3_data$ws == "1",])+
      geom_point(data = streamflow_NO3_data[streamflow_NO3_data$ws =="1",])+
      geom_line(data = streamflow_NO3_data[streamflow_NO3_data$ws == "6",])+
      geom_point(data = streamflow_NO3_data[streamflow_NO3_data$ws == "6",])+
      scale_shape_manual(values = watershed_shapes)

    static_NO3_output <- ggplotly(
      static_NO3_output, #tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)

    static_NO3_output$x$layout$width <- NULL
    static_NO3_output$y$layout$height <- NULL
    static_NO3_output$width <- NULL
    static_NO3_output$height <- NULL
    static_NO3_output %>%
      layout(autosize = TRUE, height = 600)
  })
  
  #NO3 excess
  output$NO3_excess <- renderPlotly({
    NO3_excess <- ggplot_function_excess(reactive_data_norm(), x3(), y3(), ncol = 1, log = input$log3)
    NO3_excess$x$layout$width <- NULL
    NO3_excess$y$layout$height <- NULL
    NO3_excess$width <- NULL
    NO3_excess$height <- NULL
    NO3_excess %>%
      layout(autosize = TRUE, height = 600)
    })
})
