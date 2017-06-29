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
library(magrittr)
library(timevis)


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
  source_shapes <- c("flow" = 16, "precip"= 21)
  
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
  
  solutes_cations <- list("Potassium (K)" = "K",
                          "Sodium (Na)" = "Na",
                          "Calcium (Ca)" = "Ca",
                          "Magnesium (Mg)" = "Mg",
                          "Aluminum (Al)" = "Al")
  
  solutes_anions <- list("Phosphate (PO4)" = "PO4",
                         "Sulfate (SO4)" = "SO4",
                         "Nitrate (NO3)" = "NO3",
                         "Silicon Dioxide (SiO2)" = "SiO2",
                         "Chlorine (Cl)" = "Cl",
                         "Bicarbonate (HCO3)" = "HCO3")
  
  ########### END OF IMPORTANT PRELIMINARY INFO #############################################
  
  ########### SIDEBAR FUNCTIONS 1 ##############################################################
  ###  allow 'select all' interactivity, do not edit
  

  ########### END OF SIDEBAR FUNCTIONS 1 ####################################################
  
  
  ########### SIDEBAR FUNCTIONS 2 ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  observeEvent(input$select_all_ions2, {
    if(input$select_all_ions2 == 0) {}
    else if (input$select_all_ions2%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions2", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations2", selected = "K")}
    else{
      updateCheckboxGroupInput(session, "solutes_anions2", selected = solutes_anions)
      updateCheckboxGroupInput(session, "solutes_cations2", selected = solutes_cations)}
  })
  
  observeEvent(input$select_all_anions2, {
    if(input$select_all_anions2 == 0) {}
    else if (input$select_all_anions2%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions2", selected = "PO4")}
    else{updateCheckboxGroupInput(session, "solutes_anions2", selected = solutes_anions)}
  })
  
  observeEvent(input$select_all_cations2, {
    if(input$select_all_cations2 == 0) {}
    else if (input$select_all_cations2%%2 == 0){updateCheckboxGroupInput(session, "solutes_cations2", selected = "K")}
    else{updateCheckboxGroupInput(session, "solutes_cations2", selected = solutes_cations)}
  })
  
  observeEvent(input$select_all_ws2, {
    if(input$select_all_ws2 == 0) {updateCheckboxGroupInput(session, "watersheds2", selected = "ws1")}
    else if (input$select_all_ws2%%2 == 0){updateCheckboxGroupInput(session, "watersheds2", selected = "ws1")}
    else{updateCheckboxGroupInput(session, "watersheds2", selected = watersheds)}
  })
  
  solutes2 <- reactive({c(input$solutes_cations2, input$solutes_anions2, input$solutes_H2)})
  
  ########### END OF SIDEBAR FUNCTIONS 2 ####################################################
  
  
  ########### SIDEBAR FUNCTIONS 3 ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  # observeEvent(input$select_all_ions3, {
  #   if(input$select_all_ions3 == 0) {}
  #   else if (input$select_all_ions3%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions3", selected = "PO4")
  #     updateCheckboxGroupInput(session, "solutes_cations3", selected = "K")}
  #   else{
  #     updateCheckboxGroupInput(session, "solutes_anions3", selected = solutes_anions)
  #     updateCheckboxGroupInput(session, "solutes_cations3", selected = solutes_cations)}
  # })

  observeEvent(input$select_all_anions3, {
    if(input$select_all_anions3 == 0) {}
    else if (input$select_all_anions3%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions3", selected = "PO4")}
    else{updateCheckboxGroupInput(session, "solutes_anions3", selected = solutes_anions)}
  })

  observeEvent(input$select_all_cations3, {
    if(input$select_all_cations3 == 0) {}
    else if (input$select_all_cations3%%2 == 0){updateCheckboxGroupInput(session, "solutes_cations3", selected = "K")}
    else{updateCheckboxGroupInput(session, "solutes_cations3", selected = solutes_cations)}
  })

  observeEvent(input$select_all_ws3, {
    if(input$select_all_ws3 == 0) {updateCheckboxGroupInput(session, "watersheds3", selected = "ws1")}
    else if (input$select_all_ws3%%2 == 0){updateCheckboxGroupInput(session, "watersheds3", selected = "ws1")}
    else{updateCheckboxGroupInput(session, "watersheds3", selected = watersheds)}
  })

   solutes3 <- reactive({c(input$solutes_cations3, input$solutes_anions3)})
   
   anions3 <- reactive({input$solutes_anions3})
  
  ########### END OF SIDEBAR FUNCTIONS 3 ####################################################
  
  
  ########### SIDEBAR FUNCTIONS 4 ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  observeEvent(input$select_all_ions4, {
    if(input$select_all_ions4 == 0) {}
    else if (input$select_all_ions4%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions4", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations4", selected = "K")}
    else{
      updateCheckboxGroupInput(session, "solutes_anions4", selected = solutes_anions)
      updateCheckboxGroupInput(session, "solutes_cations4", selected = solutes_cations)}
  })
  
  observeEvent(input$select_all_anions4, {
    if(input$select_all_anions4 == 0) {}
    else if (input$select_all_anions4%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions4", selected = "PO4")}
    else{updateCheckboxGroupInput(session, "solutes_anions4", selected = solutes_anions)}
  })
  
  observeEvent(input$select_all_cations4, {
    if(input$select_all_cations4 == 0) {}
    else if (input$select_all_cations4%%2 == 0){updateCheckboxGroupInput(session, "solutes_cations4", selected = "K")}
    else{updateCheckboxGroupInput(session, "solutes_cations4", selected = solutes_cations)}
  })
  
  observeEvent(input$select_all_ws4, {
    if(input$select_all_ws4 == 0) {updateCheckboxGroupInput(session, "watersheds4", selected = "ws1")}
    else if (input$select_all_ws4%%2 == 0){updateCheckboxGroupInput(session, "watersheds4", selected = "ws1")}
    else{updateCheckboxGroupInput(session, "watersheds4", selected = watersheds)}
  })
  
  solutes4 <- reactive({c(input$solutes_cations4, input$solutes_anions4, input$solutes_H4)})
  
  ########### END OF SIDEBAR FUNCTIONS 4 ####################################################
  
  
  ########### DATA IMPORT ####################################################
  
  imported_data1 <- readRDS("precip_stream_data_long.rds")
  #load in all the data from Camila download.. fix so the imported_data will actually load
  imported_data <- readRDS("D:/Duke/Work(Environ)/Programming/AcidRainStory/DataCleaning/precip_stream_data.rds")
  
  #make a df of acid rain history dates (CAA, etc.) #https://daattali.com/shiny/timevis-demo/
  historyData <- data.frame(
    id = 1:6,
    content = c("Majority of HBEF dataset",
                "The Air Pollution Control Act",
                "EPA founded", 
                "Clean Air Act",
                "Clean Air Act Amendment",
                "Today"),
    title = c("Watershed 6 is displayed in this story, as it is the control",
              "",
              "The EPA was founded to enforce the Clean Air Act",
              "The Clean Air Act also has important amendments",
              "Amendment that more specifically addressed acid rain",
              "Today isn't really today"),
    start = c("1963-06-01",
              "1955-01-01",
              "1970-12-02", 
              "1970-06-01",
              "1990-06-01",
              "2017-06-19"), #FIND THE REAL DATE OF CAA ENACTMENT!
    end = c("2014-05-01",
            NA,
            NA, 
            NA,
            NA,
            NA)
  )
  #making data frames to use with rb selection of cmpd ##OPTIMIZE
  CaData <- imported_data[imported_data$solute == "Ca",]
  CaData <- CaData[CaData$ws == "6",]
  
  SO4Data <- imported_data[imported_data$solute == "SO4",]
  SO4Data <- SO4Data[SO4Data$ws == "6",]
  
  MgData <- imported_data[imported_data$solute == "Mg",]
  MgData <- MgData[MgData$ws == "6",]
  
  KData <- imported_data[imported_data$solute == "K",]
  KData <- KData[KData$ws == "6",]
  
  NaData <- imported_data[imported_data$solute == "Na",]
  NaData <- NaData[NaData$ws == "6",]
  
  AlData <- imported_data[imported_data$solute == "Al",]
  AlData <- AlData[AlData$ws == "6",]
  
  ClData <- imported_data[imported_data$solute == "Cl",]
  ClData <- ClData[ClData$ws == "6",]
  
  NH4Data <- imported_data[imported_data$solute == "NH4",]
  NH4Data <- NH4Data[NH4Data$ws == "6",]
  
  NO3Data <- imported_data[imported_data$solute == "NO3",]
  NO3Data <- NO3Data[NO3Data$ws == "6",]
  
  PO4Data <- imported_data[imported_data$solute == "PO4",]
  PO4Data <- PO4Data[PO4Data$ws == "6",]
  
  SiO2Data <- imported_data[imported_data$solute == "SiO2",]
  SiO2Data <- SiO2Data[SiO2Data$ws == "6",]
  
  HData <- imported_data[imported_data$solute == "H",]
  HData <- HData[HData$ws == "6",]
  
  pHData <- imported_data[imported_data$solute == "pH",]
  pHData <- pHData[pHData$ws == "6",]
  pHData <- pHData[,c(1:4,14,5:13,15:16)]
  
  pHData_precip <- pHData[pHData$source == "precip",]
  
  #watershed 6 dataframe
  imported_data_ws6 <- imported_data[imported_data$ws == "6",]
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y 1 #########################################
  #Reactive Data Normal
  reactive_data1 <- reactive({
    data <- pHData_precip
  })
  
  
  x1 <- reactive({
    if(input$granularity1 == "month"){"water_date"}
    else if(input$granularity1 == "year"){"water_year"}
  })
  
  y1 <- reactive({
    if(input$granularity1 == "month" & input$units1 =="uMg/L"){"concentration_mg"}
    else if(input$granularity1 == "year" & input$units1 =="uMg/L"){"mg_weighted_average"}
  })

  ########### END REACTIVE DATA AND X Y 1 #########################################
  
  
  ########### PLOT FUNCTIONS 1 #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function1 <- function(data, x, y, ncol = NULL, nrow = NULL){
    
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source))+
        geom_ribbon(aes(ymin=4.2, ymax= 5), fill = "grey", alpha = 0.2)+ #set this as the critical lower bound?
        geom_ribbon(aes(ymin=4, ymax= 4.2), fill = "black", alpha = 0.4)+
        labs(x = "Water Year", y = input$units1)
    
    final <- plot+ my_theme + 
      geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes( text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range1[1]), max(input$date_range1[2]))+ 
      geom_vline(size = 0.5, xintercept = -5)+
      geom_vline(size = 0.5, xintercept = 7300, alpha = 0.7)+
      geom_ribbon(aes(ymin=5,ymax=5.1), fill="blue", alpha=0.3)+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }
  
  ########### END PLOT FUNCTIONS 1 #########################################
  
  
  ########### REACTIVE DATA AND X Y 2 #########################################
  #Reactive Data Normal
  ##sidebar_number_function <- function(number){  #try this out later on for optimization purposes
  reactive_data2 <- reactive({
    data <- imported_data
    data <- data[data$source %in% input$water_sources2,]
    data <- data[data$solute %in% solutes2(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
  })
  
  
  x2 <- reactive({
    if(input$granularity2 == "month"){"water_date"}
    else if(input$granularity2 == "year"){"water_year"}
  })
  
  y2 <- reactive({
    if(input$granularity2 == "month" & input$units2 =="uMg/L"){"concentration_mg"}
    else if(input$granularity2 == "year" & input$units2 =="uMg/L"){"mg_weighted_average"}
    else if(input$granularity2 == "month" & input$units2 =="uEquivalent/L"){"concentration_ueq"}
    else if(input$granularity2 == "year" & input$units2 =="uEquivalent/L"){"ueq_weighted_average"}
    else if(input$granularity2 == "month"& input$units2 =="uMole/L"){"concentration_umol"}
    else if(input$granularity2 == "year"& input$units2 =="uMole/L"){"umol_weighted_average"}
    else if(input$granularity2 == "month"& input$units2 =="flux"){"flux"}
    else if(input$granularity2 == "year"& input$units2 =="flux"){"flux_sum"}
  })
  
  log_transform2 <- reactive({
    if(input$log2 == "ln"){"transform"}
    else{"no_transform"}
  })
  
  ########### END REACTIVE DATA AND X Y 2 #########################################
  
  
  ########### PLOT FUNCTIONS 2 #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function2 <- function(data, x, y, ncol = NULL, nrow = NULL, log){
    
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",input$units2, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = input$units2)}
    
    final <- plot+ my_theme + geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes( text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range2[1]), max(input$date_range2[2]))+ 
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }
  
  ## }#end sidebar number function
  
  ########### END PLOT FUNCTIONS 2 #########################################
  
  
  ########### REACTIVE DATA AND X Y 3 #########################################
  #Reactive Data Normal
  reactive_data3_Al <- reactive({
    data <- subset(imported_data[imported_data$ws == 6,], solute %in% "Al")
    data <- data[data$source %in% input$water_sources3,]
   # data <- data[data$solute %in% solutes3(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds3,]
  })
  
  reactive_data3 <- reactive({
    data <- imported_data
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% anions3(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds3,]
  })
  
  x3 <- reactive({
    if(input$granularity3 == "month"){"water_date"}
    else if(input$granularity3 == "year"){"water_year"}
  })
  
  y3 <- reactive({
    if(input$granularity3 == "month" & input$units3 =="uMg/L"){"concentration_mg"}
    else if(input$granularity3 == "year" & input$units3 =="uMg/L"){"mg_weighted_average"}
    else if(input$granularity3 == "month" & input$units3 =="uEquivalent/L"){"concentration_ueq"}
    else if(input$granularity3 == "year" & input$units3 =="uEquivalent/L"){"ueq_weighted_average"}
    else if(input$granularity3 == "month"& input$units3 =="uMole/L"){"concentration_umol"}
    else if(input$granularity3 == "year"& input$units3 =="uMole/L"){"umol_weighted_average"}
    else if(input$granularity3 == "month"& input$units3 =="flux"){"flux"}
    else if(input$granularity3 == "year"& input$units3 =="flux"){"flux_sum"}
  })
  
  log_transform3 <- reactive({
    if(input$log3 == "ln"){"transform"}
    else{"no_transform"}
  })
  
  ########### END REACTIVE DATA AND X Y 3 #########################################
  
  
  ########### PLOT FUNCTIONS 3 #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function3 <- function(data, x, y, ncol = NULL, nrow = NULL, log){
    
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",input$units3, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = input$units3)}
    
    final <- plot+ my_theme + geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes( text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range3[1]), max(input$date_range3[2]))+ 
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }
  
  ########### END PLOT FUNCTIONS 3 #########################################
  
    
  ########### REACTIVE DATA AND X Y 4 #########################################
  #Reactive Data Normal
                    ##sidebar_number_function <- function(number){  #try this out later on for optimization purposes
  reactive_data4 <- reactive({
    data <- imported_data
    data <- data[data$source %in% input$water_sources4,]
    data <- data[data$solute %in% solutes4(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds4,]
  })
  
  
  x4 <- reactive({
    if(input$granularity4 == "month"){"water_date"}
    else if(input$granularity4 == "year"){"water_year"}
  })
  
  y4 <- reactive({
    if(input$granularity4 == "month" & input$units4 =="uMg/L"){"concentration_mg"}
    else if(input$granularity4 == "year" & input$units4 =="uMg/L"){"mg_weighted_average"}
    else if(input$granularity4 == "month" & input$units4 =="uEquivalent/L"){"concentration_ueq"}
    else if(input$granularity4 == "year" & input$units4 =="uEquivalent/L"){"ueq_weighted_average"}
    else if(input$granularity4 == "month"& input$units4 =="uMole/L"){"concentration_umol"}
    else if(input$granularity4 == "year"& input$units4 =="uMole/L"){"umol_weighted_average"}
    else if(input$granularity4 == "month"& input$units4 =="flux"){"flux"}
    else if(input$granularity4 == "year"& input$units4 =="flux"){"flux_sum"}
  })
  
  log_transform4 <- reactive({
    if(input$log4 == "ln"){"transform"}
    else{"no_transform"}
  })
  
  ########### END REACTIVE DATA AND X Y 4 #########################################
  
  
  ########### PLOT FUNCTIONS 4 #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function4 <- function(data, x, y, ncol = NULL, nrow = NULL, log){
    
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",input$units4, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = input$units4)}
    
    final <- plot+ my_theme + geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes( text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range4[1]), max(input$date_range4[2]))+ 
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }
  
 ## }#end sidebar number function
  
  ########### END PLOT FUNCTIONS 4 #########################################
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  output$plot1a <- renderPlotly({
    theplot <- ggplot_function(reactive_data(), x(), y(), ncol = 1, log = input$log)
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, height = 600)
  })
  
  output$plot1b <- renderPlotly({
    theplot <- ggplot_function(reactive_data(), x(), y(), ncol = 1, log = input$log)
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, height = 600)
  })
  
  #successfully interactive/integrated intro pH plot with only precip
  output$pH_intro <- renderPlotly({
    pH_intro <- ggplot_function1(reactive_data1(), x1(), y1(), ncol = 1, nrow = NULL)
    pH_intro$x$layout$width <- NULL
    pH_intro$y$layout$height <- NULL
    pH_intro$width <- NULL
    pH_intro$height <- NULL
    pH_intro %>%
      layout(autosize = TRUE, height = 600)
    })
  
  #pH aes reference plot.. delete eventually
  output$pHtheme <- renderPlotly({
    pHtheme <- ggplot(pHData_precip, aes(x = water_year, y = mg_weighted_average, 
                                         shape = source)) + my_theme +
      geom_ribbon(aes(ymin=4.2, ymax= 5), fill = "grey", alpha = 0.2)+ #set this as the critical lower bound?
      geom_ribbon(aes(ymin=4, ymax= 4.2), fill = "black", alpha = 0.4)+
      geom_line(size = 1, aes(color = solute))+
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes(color = solute, 
                     text = paste("pH value: ", mg_weighted_average, "<br>", "Date: ", date)))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("Precipitation de-acidifying in response to acid rain mitigation")+
      labs(x = "Year", y = "pH")+
      coord_cartesian(ylim = c(4, 5.05))+
      geom_vline(size = 0.5, xintercept = -5)+
      geom_vline(size = 0.5, xintercept = 7300, alpha = 0.7)+
      geom_ribbon(aes(ymin=5,ymax=5.5), fill="blue", alpha=0.3)

    ggplotly(pHtheme, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #pH plot with P and Q to show acid in, more neutralized out
  output$pHPandQ <- renderPlotly({
    pHPandQ <- ggplot(pHData, aes(x = water_year, y = mg_weighted_average, 
                                  shape = source, color = solute, alpha = ws))+ my_theme +
      geom_ribbon(aes(ymin=4.2, ymax= 5), fill = "grey", alpha = 0.2)+ #set this as the critical lower bound?
      geom_ribbon(aes(ymin=4, ymax= 4.2), fill = "black", alpha = 0.4)+
      geom_ribbon(aes(ymin=5,ymax=5.5), fill="blue", alpha=0.3)+
      geom_line(size = 1)+
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes(text = paste("pH value: ", mg_weighted_average, "<br>", "Date: ", date)))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("De-acidification of P and Q in response to reducing SOx and NOx emissions")+
      labs(x = "Year", y = "pH")+
      coord_cartesian(ylim = c(4, 5.4))+
      geom_vline(size = 0.5, xintercept = -5)+
      geom_vline(size = 0.5, xintercept = 7300, alpha = 0.7)
    ggplotly(pHPandQ, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #in progress interactive/integrated plot of SO4 and NO3 to complement pH increase - shows decreasing trend
  output$policy_SO4_NO3 <- renderPlotly({
    policy_SO4_NO3 <- ggplot_function3(reactive_data3(), x3(), y3(), ncol = 1, nrow = NULL, log = input$log3)
    policy_SO4_NO3$x$layout$width <- NULL
    policy_SO4_NO3$y$layout$height <- NULL
    policy_SO4_NO3$width <- NULL
    policy_SO4_NO3$height <- NULL
    policy_SO4_NO3%>%
      layout(autosize = TRUE, height = 600)
  })
  
  #plot of SO4 and NO3 to complement pH increase - shows decreasing trend
  output$SO4NO3reductions <- renderPlotly({
    SO4NO3reductions <- ggplot(NULL, aes(shape = source, color = solute, alpha = ws))+
      geom_line(data = NO3Data, aes(x = water_year, y = ueq_weighted_average), size = 1)+
      geom_point(data = NO3Data, aes(x = water_year, y = ueq_weighted_average,
                                     text = paste("NO3 Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      geom_line(data = SO4Data, aes(x = water_year, y = ueq_weighted_average), size = 1)+
      geom_point(data = SO4Data, aes(x = water_year, y = ueq_weighted_average,
                                     text = paste("SO4 Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("SOx and NOx concentrations lowering as policy is implemented")+
      labs(x = "Year", y = "ueq/L")+ my_theme+
      coord_cartesian(ylim = c(0, 130))
    ggplotly(SO4NO3reductions, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #base cations trend plot to read next to decreasing so4/no3 and increasing pH
  output$baseCations <- renderPlotly({
    baseCations <- ggplot(NULL, aes(shape = source, color = solute, alpha = ws))+ my_theme+
      geom_line(data = CaData, aes(x = water_year, y = ueq_weighted_average), size = 1)+ #alter Camila function to create one more custom to this style? (ie less input based, more to see trend)
      geom_point(data = CaData, aes(x = water_year, y = ueq_weighted_average,
                                    text = paste("Ca Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)),
                 size = 1.5, fill = "white", stroke = 0.5)+
      #NOTE all of these extra geom_line and _point are to graph additional base cations rather than creating yet another df
      geom_line(data = MgData, aes(x = water_year, y = ueq_weighted_average), size = 1)+
      geom_point(data = MgData, aes(x = water_year, y = ueq_weighted_average,
                                    text = paste("Mg Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)),
                 size = 1.5, fill = "white", stroke = 0.5)+
      geom_line(data = KData, aes(x = water_year, y = ueq_weighted_average), size = 1)+
      geom_point(data = KData, aes(x = water_year, y = ueq_weighted_average,
                                   text = paste("K Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)),
                 size = 1.5, fill = "white", stroke = 0.5)+
      geom_line(data = NaData, aes(x = water_year, y = ueq_weighted_average), size = 1)+
      geom_point(data = NaData, aes(x = water_year, y = ueq_weighted_average,
                                    text = paste("Na Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)),
                 size = 1.5, fill = "white", stroke = 0.5)+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("Decrease in Base cations leaving the soil")+
      labs(x = "Year", y = "ueq/L")+
      coord_cartesian(ylim = c(0, 130))
    ggplotly(baseCations, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #Successfully interactive/integrated Al plot to show decrease in acids mean less Al released from soil
  output$policy_Al <- renderPlotly({
    policy_Al <- ggplot_function3(reactive_data3_Al(), x3(), y3(), ncol = 1, nrow = NULL, log = input$log3)
    policy_Al$x$layout$width <- NULL
    policy_Al$y$layout$height <- NULL
    policy_Al$width <- NULL
    policy_Al$height <- NULL
    policy_Al%>%
      layout(autosize = TRUE, height = 600)
    })
  
  #Al plot ...delete eventually
  output$Al <- renderPlotly({
    Al <- ggplot(subset(imported_data[imported_data$ws == 6,], solute %in% "Al"), aes(x = water_year, y = ueq_weighted_average,
                                                                                      shape = source, color = solute, alpha = ws))+ my_theme+
      geom_line(size = 1)+
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes(text = paste("Al Concentration: ", ueq_weighted_average, "<br>", "Date: ", date)))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("Decrease in toxic Al discharge as SOx and NOx decrease")+
      labs(x = "Year", y = "ueq/L")+
      coord_cartesian(ylim = c(0, 130))
    
    ggplotly(Al, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #plot of any compound conc (reactively chosen) over rective time
  output$cTime <- renderPlotly({
    cTime <- ggplot(get(input$selComp), 
                    aes(x = as.Date(get(input$selDate)), y = ueq_weighted_average,
                        shape = source, color = solute, alpha = ws))+ my_theme+
      geom_line(size = 1) +
      geom_point(size = 1.5, fill = "white", stroke = 0.5,
                 aes(text = paste("Concentration:", concentration_ueq, "<br>", "Date:", date)))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      labs(colour = "Source", x = "Year", y = "(ueq/L)")+
      coord_cartesian(ylim = c(0, 130))+
      xlim(min(input$date_range2[1]), max(input$date_range2[2]))+
      ggtitle(as.character(input$selComp), "affected by acid rain") #possibly rename 'CaData' to be 'Calcium'
    ggplotly(cTime, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #plot of Al flux and acid flux to show acids release Al from the soil ###Not sure how to interpret and/or make faster
  #also try to make this yearly by creating a yearly flux... but would that defeat the purpose? 
  # output$fluxAlAcids <- renderPlotly({
  #   fluxAlAcids <- ggplot(subset(imported_data_ws6, solute %in% c("Al", "SO4")),#######################################################
  #                         aes(x = date, y = flux,
  #                             shape = source, color = solute, alpha = ws))+ my_theme+
  #     geom_line(size = 1) +
  #     geom_point(size = 1.5, fill = "white", stroke = 0.5,
  #                aes(text = paste("Solute: ", solute, "<br>", "Flux in ___ units:", flux, "<br>", "Date:", date)))+
  #     scale_shape_manual(values = source_shapes) +
  #     scale_color_manual(values = solute_palette) +
  #     scale_alpha_discrete(range = c(0.9, 0.5))+
  #     labs(colour = "Source", x = "Year", y = "(units)")+
  #     coord_cartesian(ylim = c(0, 130))+
  #     xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
  #     ggtitle("Increasing acid inflow increases Aluminum outflow")
  #   ggplotly(fluxAlAcids, tooltip = "text", width = 1900)%>%
  #     config(displayModeBar = F)%>%
  #     config(showLink = F)
  # }) ################################
  
  #output an interactive timeline for the history of acid rain
  output$CAAetc <- renderTimevis({
    timevis(historyData) #possibly use groups in order to contextualize (ie disney movie years)
  })
  
  #output all compounds plot using the ggplot_function (possibly new sidebar in each tab and delete all compound options except desired)
  output$practice <- renderPlotly({
    practice <- ggplot_function4(reactive_data4(), x4(), y4(), ncol = 1, nrow = NULL, log = input$log4)
    practice$x$layout$width <- NULL
    practice$y$layout$height <- NULL
    practice$width <- NULL
    practice$height <- NULL
    practice %>%
      layout(autosize = TRUE, height = 600)
  })
  
})


