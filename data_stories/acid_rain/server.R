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
library(shinydashboard)
library(grid)


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
                         "Chloride (Cl)" = "Cl",
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
   
   Al_anions3 <- reactive({input$solutes_Al_anions3})
   
   cations3 <- reactive({input$solutes_cations3})
  
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
  
  #load("D:/Duke/Work(Environ)/Programming/hbef/data_stories/acid_rain/precip_streamflow_dfs.RData")
  load("precip_streamflow_dfs.RData")
  imported_data <- precip_streamflow_long
  
  #make a df of acid rain history dates (CAA, etc.) #https://daattali.com/shiny/timevis-demo/
  historyData <- data.frame(
    id = 1:9,
    content = c("Span of HBEF dataset",
                "Air Pollution Control Act",
                "Clean Air Act of 1963",
                "Air Quality Act of 1967",
                "EPA founded", 
                "Clean Air Act",
                "1977 Clean Air Act Amendments",
                "1990 Clean Air Act Amendments",
                "Today"),
    title = c("1 Watershed 6 is displayed in this story, as it is the control",
              "2 Research funding, first federal legislation on air pollution",
              "3 Research developing, national program made",
              "4 Expanded research, interstate pollution policies",
              "5 The EPA was founded to enforce the Clean Air Act",
              "6 The Clean Air Act of 1970",
              "7 National Ambient Air Quality standards improvement",
              "8 Amendment that more specifically addressed acid rain",
              "9 Today isn't really today"),
    start = c("1957-06-01",
              "1955-01-01",
              "1963-01-01",
              "1967-01-01",
              "1970-12-02", 
              "1970-06-01",
              "1977-01-01",
              "1990-06-01",
              "2017-06-19"), #FIND THE REAL DAY OF CAA ENACTMENT!
    end = c("2014-05-01",
            NA,
            NA,
            NA, 
            NA,
            NA,
            NA,
            NA,
            NA)
  )
  ########### END OF DATA IMPORT #############################################
  
  
  ########### PLOT FUNCTIONS  #########################################
  
  ## GGPLOT pH FUNCTION -- This builds the pH graph
  ggplot_function1 <- function(data, x, y, ncol = NULL, nrow = NULL){
    
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source))+
        geom_ribbon(aes(ymin=4.2, ymax= 5), fill = "grey", alpha = 0.2)+
        geom_ribbon(aes(ymin=4, ymax= 4.2), fill = "black", alpha = 0.4)+
        geom_ribbon(aes(ymin=5,ymax=5.4), fill="blue", alpha=0.3)+
        labs(x = "Water Year", y = "pH")
    
    final <- plot+ my_theme + 
      geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes(text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range1[1]), max(input$date_range1[2]))+ 
      geom_vline(size = 0.5, xintercept = -5, alpha = 0.5)+
      annotate("text", label = "Clean Air Act", x = as.Date("1970-01-01"), y = 4.02, color = "black")+
      geom_vline(size = 0.5, xintercept = 7300, alpha = 0.5)+
      annotate("text", label = "Clean Air Act Amendment  ", x = as.Date("1990-01-01"), y = 4.02, color = "black")+
      annotate("text", label = "Average pH of acid rain", x = as.Date("2005-01-01"), y = 4.21, alpha = 0.7, color = "black")+
      annotate("text", label = "Normal (clean) rain pH", x = as.Date("1979-01-01"), y = 5.01, alpha = 0.7, color = "black")+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(  
      final, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }
  
  
  ## GGPLOT TIME FUNCTION
  #### --- GGPLOT TIME FUNCTION
  ggplot_function2 <- function(data, x, y, log, y_label, date_range){
    
    if(log == "log") {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = paste("log", "(",y_label, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
        labs(x = "Water Year", y = y_label)}
    
    plot <- plot+ my_theme + geom_line(size = 0.5) + 
      geom_point(size = 1.3, fill = "white", stroke = 0.2, aes(text = paste(solute,":", round(get(y), 4)))) +
      xlim(min(date_range[1]), max(date_range[2]))+ 
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>% 
      layout(hovermode = "x")
  }
  
  ########### END PLOT FUNCTIONS #########################################
  

  ########### REACTIVE DATA #########################################
  
  ########## Reactive Data 1
  #Reactive Data Normal
  reactive_data1 <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity1,]
    data <- data[data$ws %in% c("6"),]
    data <- data[data$solute %in% c("pH"),]
    data <- data[data$source %in% input$water_sources1,]
  })
  
  
  x1 <- reactive({
    if(input$granularity1 == "week"){"water_date"}
    else if(input$granularity1 == "month"){"water_date"}
    else if(input$granularity1 == "year"){"water_year"}
  })
  
  y1 <- reactive({
    {"concentration_mg"}
  })
  
  ########## Reactive Data 2
  #Reactive Data Normal
  ##sidebar_number_function <- function(number){  #try this out later on for optimization purposes
  reactive_data2 <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity2,]
    data <- data[data$source %in% input$water_sources2,]
    data <- data[data$solute %in% solutes2(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
  })
  
  
   x2 <- reactive({
     if(input$granularity2 == "week"){"water_date"}
     else if(input$granularity2 == "month"){"water_date"}
     else if(input$granularity2 == "year"){"water_year"}
   })
   
   y2 <- reactive({
     if(input$units2 =="mg/L"){"concentration_mg"}
     else if(input$units2 =="uEquivalent/L"){"concentration_ueq"}
     else if(input$units2 =="uMole/L"){"concentration_umol"}
     else if(input$units2 =="flux"){"flux"}
   })
   
  ########## Reactive Data 3
  #Reactive Data Normal
  reactive_data3_Al <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity5,]
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% Al_anions3(),] 
    data <- data[data$ws %in% input$watersheds3,]
  })
  
  reactive_data3_anions <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity3,]
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% anions3(),] 
    #note that anions is a function, that's because the inputs come from input$anions
    data <- data[data$ws %in% input$watersheds3,]
  })
  
  reactive_data3_cations <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity4,]
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% cations3(),] 
    #note that cations is a function, that's because the inputs come from input$cations
    data <- data[data$ws %in% input$watersheds3,]
  })
  
  x3 <- reactive({
    if(input$granularity3 == "week"){"water_date"}
    else if(input$granularity3 == "month"){"water_date"}
    else if(input$granularity3 == "year"){"water_year"}
  })
  
  
  x4 <- reactive({
    if(input$granularity4 == "week"){"water_date"}
    else if(input$granularity4 == "month"){"water_date"}
    else if(input$granularity4 == "year"){"water_year"}
  })
  
  x5 <- reactive({
    if(input$granularity5 == "week"){"water_date"}
    else if(input$granularity5 == "month"){"water_date"}
    else if(input$granularity5 == "year"){"water_year"}
  })
  
  y3 <- reactive({
    if(input$units3 =="mg/L"){"concentration_mg"}
    else if(input$units3 =="uEquivalent/L"){"concentration_ueq"}
    else if(input$units3 =="uMole/L"){"concentration_umol"}
    else if(input$units3 =="flux"){"flux"}
  })
 
  
  ########### END REACTIVE DATA #########################################
  
  

  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  

  #successfully interactive/integrated intro pH plot
  output$pH_intro <- renderPlotly({
    pH_intro <- ggplot_function1(reactive_data1(), x1(), y1(), ncol = 1, nrow = NULL)
    pH_intro$x$layout$width <- NULL
    pH_intro$y$layout$height <- NULL
    pH_intro$width <- NULL
    pH_intro$height <- NULL
    pH_intro %>%
      layout(autosize = TRUE)
    })
  
  #Successfully interactive/integrated plot of any compound conc
  output$chemistry <- renderPlotly({
    chemistry <- ggplot_function2(reactive_data2(), x2(), y2(), log = input$log2, input$units2, input$date_range2)
    chemistry$x$layout$width <- NULL
    chemistry$y$layout$height <- NULL
    chemistry$width <- NULL
    chemistry$height <- NULL
    chemistry %>%
      layout(autosize = TRUE)
  })
  
#   #plot pH vs Q to see if there are any trends##################
#   output$pH_streamflow <- renderPlotly({
# #    pH_streamflow <- ggplot_function2(reactive_data2(), x2(), y2(), ncol = 1, nrow = NULL, log = input$log2)
#     pH_streamflow_data <- imported_data
#     pH_streamflow_data <- pH_streamflow_data[pH_streamflow_data$granularity %in% c("month"),]
#     pH_streamflow_data <- pH_streamflow_data[pH_streamflow_data$source %in% c("streamflow"),]
#     pH_streamflow_data <- pH_streamflow_data[pH_streamflow_data$ws %in% c("6"),]
#     pH_streamflow_data <- pH_streamflow_data[pH_streamflow_data$solute %in% c("pH"),]
#     pH_streamflow_data <- pH_streamflow_data %>% filter(date <"1990-06-01")
# 
#     pH_streamflow <- ggplot(pH_streamflow_data, aes(x = water_mm, y=concentration_mg, color = water_date))+
#       geom_point(aes(text=paste("Date: ", date)))+
#       geom_smooth(method="lm",se=F)+
#       coord_cartesian(ylim = c(4.5, 6))
#     
#     pH_streamflow <- ggplotly(pH_streamflow, tooltip = "text",
#       width = 900) %>%
#         config(displayModeBar = FALSE) %>%
#         config(showLink = FALSE)
#     
#     pH_streamflow$x$layout$width <- NULL
#     pH_streamflow$y$layout$height <- NULL
#     pH_streamflow$width <- NULL
#     pH_streamflow$height <- NULL
#     pH_streamflow %>%
#       layout(autosize = TRUE, height = 600)
#   })
#   #second half
#   #plot pH vs Q to see if there are any trends
#   output$pH_streamflow1990 <- renderPlotly({
#     pH_streamflow_data1990 <- pH_streamflow_data %>% filter(date > "1990-06-01")
#     
#     pH_streamflow1990 <- ggplot(pH_streamflow_data1990, aes(x = water_mm, y=concentration_mg, color = water_date))+
#       geom_point(aes(text=paste("Date: ", date)))+
#       geom_smooth(method="lm",se=F)+
#       coord_cartesian(ylim = c(4.5, 6))
#     
#     pH_streamflow1990 <- ggplotly(pH_streamflow1990, tooltip = "text",
#                               width = 900) %>%
#       config(displayModeBar = FALSE) %>%
#       config(showLink = FALSE)
#     
#     pH_streamflow1990$x$layout$width <- NULL
#     pH_streamflow1990$y$layout$height <- NULL
#     pH_streamflow1990$width <- NULL
#     pH_streamflow1990$height <- NULL
#     pH_streamflow1990 %>%
#       layout(autosize = TRUE, height = 600)
#   })
#   #################
  #Successfully interactive/integrated plot of SO4 and NO3 to complement pH increase - shows decreasing trend
  output$policy_SO4_NO3 <- renderPlotly({
    policy_SO4_NO3 <- ggplot_function2(reactive_data3_anions(), x3(), y3(), log = input$log3, input$units3, input$date_range3)
    policy_SO4_NO3$x$layout$width <- NULL
    policy_SO4_NO3$y$layout$height <- NULL
    policy_SO4_NO3$width <- NULL
    policy_SO4_NO3$height <- NULL
    policy_SO4_NO3%>%
      layout(autosize = TRUE)
  })
  
  #Successfully interactive/integrated base cation trends plot 
  output$policy_base_cations <- renderPlotly({
    policy_base_cations <- ggplot_function2(reactive_data3_cations(), x4(), y3(), log = input$log4, input$units3, input$date_range3)
    policy_base_cations$x$layout$width <- NULL
    policy_base_cations$y$layout$height <- NULL
    policy_base_cations$width <- NULL
    policy_base_cations$height <- NULL
    policy_base_cations%>%
      layout(autosize = TRUE)
  })
  
  #Successfully interactive/integrated Al plot to show decrease in acids mean less Al released from soil
  output$policy_Al <- renderPlotly({
    policy_Al <- ggplot_function2(reactive_data3_Al(), x5(), y3(), log = input$log5, input$units3, input$date_range3)
    policy_Al$x$layout$width <- NULL
    policy_Al$y$layout$height <- NULL
    policy_Al$width <- NULL
    policy_Al$height <- NULL
    policy_Al%>%
      layout(autosize = TRUE)
    })

  #in progress plot of Al and acid flux and conc... shoudl be on seperate tab so has a diff sidebar!
  output$chemistry_flux <- renderPlotly({
    chemistry_flux <- ggplot_function2(reactive_data2(), x2(), y2(), log = input$log2, input$units2, input$date_range2)
    chemistry_flux$x$layout$width <- NULL
    chemistry_flux$y$layout$height <- NULL
    chemistry_flux$width <- NULL
    chemistry_flux$height <- NULL
    chemistry_flux %>%
      layout(autosize = TRUE)
    
  })
  
  #output an interactive timeline for the history of acid rain
  output$timeline <- renderTimevis({
    timevis(historyData) #possibly use groups in order to contextualize (ie disney movie years)
  })
  

  
})


