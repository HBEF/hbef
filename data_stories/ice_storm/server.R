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
  
  color_cation <- c("Al" = "#162338", "Mg" = "#273D64", "Ca" = "#3B5C95", "NH4" = "#4E7AC7" , "Na" = "#7195D2", "K" = "#95AFDD")
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
  
  solutes_cations <- list("Aluminum (Al)" = "Al",
                          "Magnesium (Mg)" = "Mg",
                          "Calcium (Ca)" = "Ca",
                          "Sodium (Na)" = "Na",
                          "Potassium (K)" = "K")
  
  solutes_anions <- list("Phosphate (PO4)" = "PO4",
                         "Sulfate (SO4)" = "SO4",
                         "Nitrate (NO3)" = "NO3",
                         "Silicon Dioxide (SiO2)" = "SiO2",
                         "Chlorine (Cl)" = "Cl",
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
  
  imported_data <- readRDS("precip_stream_data_long.rds")
#  lai_data <- read.csv("lai.txt")
  lai_data <- read_csv("D:/Duke/Work(Environ)/Programming/hbef/data_stories/ice_storm/lai.txt")

  # lai_data[lai_data=="6"]<-"six"
  # lai_data[lai_data=="1"]<-"one"

  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y 2 #########################################
  #Reactive Data Normal
  
  reactive_data2 <- reactive({
    data <- imported_data
    data <- data[data$source %in% input$water_sources2,]
    data <- data[data$solute %in% solutes_NO3(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
  })
  
  
  x <- reactive({
    if(input$granularity == "month"){"water_date"}
    else if(input$granularity == "year"){"water_year"}
  })
  
  y <- reactive({
    if(input$granularity == "month" & input$units =="uMg/L"){"concentration_mg"}
    else if(input$granularity == "year" & input$units =="uMg/L"){"mg_weighted_average"}
    else if(input$granularity == "month" & input$units =="uEquivalent/L"){"concentration_ueq"}
    else if(input$granularity == "year" & input$units =="uEquivalent/L"){"ueq_weighted_average"}
    else if(input$granularity == "month"& input$units =="uMole/L"){"concentration_umol"}
    else if(input$granularity == "year"& input$units =="uMole/L"){"umol_weighted_average"}
    else if(input$granularity == "month"& input$units =="flux"){"flux"}
    else if(input$granularity == "year"& input$units =="flux"){"flux_sum"}
  })
  
  log_transform <- reactive({
    if(input$log == "ln"){"transform"}
    else{"no_transform"}
  })

  ########### REACTIVE DATA AND X Y 3 #########################################
  #Reactive Data Normal
  
  reactive_data3 <- reactive({
    data <- imported_data
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% solutes_NO33(),] 
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
      geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = -8, color = "black")+
      scale_shape_manual(values = source_shapes) +
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
  
  #make a plot of nitrates like in the 2003 paper
  #(moles/ha-yr (flux) vs water year, faceted into output (excess) for ws1,6 and diff for ws2,4,5)
  #have line for ws1 and ws6 show up on same graph... write an if statement when weekly data is figured out
  output$NO3_output <- renderPlotly({
    NO3_output <- ggplot_function3(reactive_data3(), x3(), y3(), ncol = 1, log = input$log3)
    NO3_output$x$layout$width <- NULL
    NO3_output$y$layout$height <- NULL
    NO3_output$width <- NULL
    NO3_output$height <- NULL
    NO3_output %>%
      layout(autosize = TRUE, height = 600)
  })
  output$NO3_difference <- renderPlotly({
    
  })

  #ggplotly that shows most plots increase in lai following the ice storm
  output$lai_plot <- renderPlotly({
    lai_plot <- ggplot(lai_data[lai_data$WS == input$watersheds1,], aes(x = YEAR, y = LAIT, color = ELEVATION_M))+
      geom_point(aes(text = paste("Year: ", YEAR, "<br>", "LAI: ", LAIT)))+
      geom_smooth(method = "lm", se = F, size = 0.5)+
      facet_wrap(~PLOT)
    
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

})
