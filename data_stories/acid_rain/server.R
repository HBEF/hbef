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


########### DATA IMPORT ####################################################

load("precip_streamflow_dfs.RData")
imported_data <- precip_streamflow_long
timeline_data <- readRDS("timeline_data.rds")

########### END OF DATA IMPORT #############################################

########### IMPORTANT PRELIMINARY INFO #############################################

###  Theme  ################
my_theme <- 
  theme(rect = element_rect(fill = NA),
        panel.background = element_rect("transparent", colour = NA),
        panel.grid.major = element_line(colour = "#dddddd"), 
        panel.grid.major.x = element_line(colour = NA),
        text = element_text(family = "Helvetica", size = 15), 
        legend.position="none", #hides legend
        legend.direction = "horizontal", legend.title = element_blank(),
        axis.title = element_text(size = 17, margin = unit(c(3, 3, 3, 3), "cm")),
        axis.text = element_text(size = 15),
        plot.margin = margin(1, 1, 1, 1, "cm"))

color_cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
color_anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
color_hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")

solute_palette <- c(color_cation, color_anion, color_hydro)
#palette used for pH plot
grey_palette <- c("#505050", "#CCCDD9")
source_shapes <- c("streamflow" = 16, "precipitation"= 21)

### End of Theme ################

###  Lists for the sidebar  ######
#Edit if there are values that do not appear or are not relevant to your data. 

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


#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################

shinyServer(function(session, input, output) {

  ########### SIDEBAR FUNCTIONS (chemistry tab) ############################################
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
  
  #make variable to use for filtering data
  solutes <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################


  ########### PLOT FUNCTIONS  #########################################
  
  ## GGPLOT pH FUNCTION -- This builds the pH graph
  ggplot_function_pH <- function(data, x, y, log){
    source_shapes <- c("streamflow" = 16, "precipitation"= 21)
    if(log == "log") {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source))+
        labs(x = " ", y = paste("log pH")) +
        scale_shape_manual(values = source_shapes)}
    
    else{plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source))+
        labs(x = " ", y = "pH") +
        scale_shape_manual(values = source_shapes)}
    
    final <- plot+ my_theme + 
      #geom_hline annotations for acid/clean rain cutoffs on graph
      geom_hline(size = 0.5, yintercept = 4.2, alpha = 0.5, linetype="dashed", color = "red")+
      geom_hline(size = 0.5, yintercept = 5.1, alpha = 0.5, linetype="dashed", color = "red")+
      geom_line(size = 0.5) + 
      geom_point(size = 1.3, fill = "white", stroke = 0.5, 
                 #plotly hover tooltip text and variables
                 aes(text = paste("Water Source: ", source, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(input$date_range_pH[1]), max(input$date_range_pH[2]))+ 
      geom_vline(size = 0.5, xintercept = -5, alpha = 0.2)+
      annotate("text", label = "Clean Air Act", x = as.Date("1970-01-01"), y = 4, alpha = 0.7, color = "black", angle = 20, size = 4)+
      geom_vline(size = 0.5, xintercept = 7300, alpha = 0.2)+
      annotate("text", label = "Clean Air Act Amendment  ", x = as.Date("1990-01-01"), y = 4, alpha = 0.7, color = "black", angle = 20, size = 4)+
      annotate("text", label = "Average pH of acid rain", x = as.Date("2005-01-01"), y = 4.22, alpha = 0.7, color = "black", hjust=2, size = 4)+
      annotate("text", label = "Normal (clean) rain pH", x = as.Date("2005-01-01"), y = 5.12, alpha = 0.7, color = "black", angle = 20, hjust=2, size = 4)+
      scale_color_manual(values = grey_palette)
      #Efforts to make it a gradient color scale
      #scale_colour_gradient2(low = "red", mid = "orange" , high = "green", midpoint = 4)
    
    ggplotly(final, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
  }
  
  
  ## GGPLOT TIME FUNCTION
  #### --- Generalized ggplot for time functions in chemistry and policy tabs
  ggplot_function <- function(data, x, y, log, y_label, date_range, color){
    
    #if statment to take log of Y axis and define plot basics
    if(log == "log") {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
        labs(x = " ", y = paste("log", "(",y_label, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
        labs(x = " ", y = y_label)}
    
    #define rest of plot specifics
    plot <- plot+ my_theme + geom_line(size = 0.5) + 
      geom_point(size = 1.3, fill = "white", stroke = 0.2, aes(text = paste(solute,":", round(get(y), 4)))) +
      xlim(min(date_range[1]), max(date_range[2]))+ 
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = color) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>% 
      layout(hovermode = "x")
  }
  
  ########### END PLOT FUNCTIONS #########################################
  

  ########### REACTIVE DATA #########################################
  
  ########## Reactive Data 1 (pH)
  #Reactive Data for intro pH plot
  reactive_data_pH <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity1,]
    data <- data[data$ws %in% c("6"),]
    data <- data[data$solute %in% c("H"),]
    data <- data[data$source %in% input$water_sources1,]
  })
  #x for intro pH plot
  x1 <- reactive({
    if(input$granularity1 == "week"){"water_date"}
    else if(input$granularity1 == "month"){"water_date"}
    else if(input$granularity1 == "year"){"water_year"}
  })
  #y for intro pH plot
  y1 <- reactive({
    {"pH"}
  })
  
  ########## Reactive Data 2 (chemistry)
  #Reactive Data for Water Chemistry plot
  reactive_data_chem <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity2,]
    data <- data[data$source %in% input$water_sources2,]
    data <- data[data$solute %in% solutes(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% c("6"),]
  })
  
  #x for chemistry plot
   x2 <- reactive({
     if(input$granularity2 == "week"){"water_date"}
     else if(input$granularity2 == "month"){"water_date"}
     else if(input$granularity2 == "year"){"water_year"}
   })
   #y for chemistry plot
   y2 <- reactive({
     if(input$units2 =="mg/L"){"concentration_mg"}
     else if(input$units2 =="uEquivalent/L"){"concentration_ueq"}
     else if(input$units2 =="uMole/L"){"concentration_umol"}
     else if(input$units2 =="flux"){"flux"}
   })
   
  ########## Reactive Data 3 (policy tab)
   
   #reactive variables for the SO4 NO3 plot, Al plot, and base cations plot
   anions3 <- reactive({input$solutes_anions3})
   Al_anions3 <- reactive({input$solutes_anions_Al})
   cations3 <- reactive({input$solutes_cations3})
   
  #Reactive Data for Al plot
  reactive_data3_Al <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity5,]
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% Al_anions3(),] 
    data <- data[data$ws %in% c("6"),]
  })
  #Reactive Data for SO4 NO3 plot
  reactive_data3_anions <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity3,]
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% anions3(),] 
    #note that anions is a function, that's because the inputs come from input$anions
    data <- data[data$ws %in% c("6"),]
  })
  #Reactive Data for base cations plot
  reactive_data3_cations <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity4,]
    data <- data[data$source %in% input$water_sources3,]
    data <- data[data$solute %in% cations3(),] 
    #note that cations is a function, that's because the inputs come from input$cations
    data <- data[data$ws %in% c("6"),]
  })
  #x for SO4 NO3 plot
  x3 <- reactive({
    if(input$granularity3 == "week"){"water_date"}
    else if(input$granularity3 == "month"){"water_date"}
    else if(input$granularity3 == "year"){"water_year"}
  })
  #x for base cations plot
  x4 <- reactive({
    if(input$granularity4 == "week"){"water_date"}
    else if(input$granularity4 == "month"){"water_date"}
    else if(input$granularity4 == "year"){"water_year"}
  })
  #x for Al plot
  x5 <- reactive({
    if(input$granularity5 == "week"){"water_date"}
    else if(input$granularity5 == "month"){"water_date"}
    else if(input$granularity5 == "year"){"water_year"}
  })
  #y for policy graphs
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
  

  #intro pH plot with annotations
  output$pH <- renderPlotly({

    pH <- ggplot_function_pH(reactive_data_pH(), x1(), y1(), log = input$log1)
    #code to make the plot readjust to consistently fit the screen well
    pH$x$layout$width <- NULL
    pH$y$layout$height <- NULL
    pH$width <- NULL
    pH$height <- NULL
    pH 
    })
  
  #plot of any solute concentration over time
  output$chemistry <- renderPlotly({
    chemistry <- ggplot_function(reactive_data_chem(), x2(), y2(), log = input$log2, input$units2, input$date_range_chem, solute_palette)
    #code to make the plot readjust to consistently fit the screen well
    chemistry$x$layout$width <- NULL
    chemistry$y$layout$height <- NULL
    chemistry$width <- NULL
    chemistry$height <- NULL
    chemistry %>%
      layout(autosize = TRUE, height = 500)
  })

  #plot of SO4 and NO3 to complement pH increase - shows decreasing trend
  output$policy_SO4_NO3 <- renderPlotly({
    policy_SO4_NO3 <- ggplot_function(reactive_data3_anions(), x3(), y3(), log = input$log3, input$units3, input$date_range_policy, solute_palette)
    #code to make the plot readjust to consistently fit the screen well
    policy_SO4_NO3$x$layout$width <- NULL
    policy_SO4_NO3$y$layout$height <- NULL
    policy_SO4_NO3$width <- NULL
    policy_SO4_NO3$height <- NULL
    policy_SO4_NO3%>%
      layout(autosize = TRUE)
  })
  
  #base cation trends plot 
  output$policy_base_cations <- renderPlotly({
    policy_base_cations <- ggplot_function(reactive_data3_cations(), x4(), y3(), log = input$log4, input$units3, input$date_range_policy, solute_palette)
    #code to make the plot readjust to consistently fit the screen well
    policy_base_cations$x$layout$width <- NULL
    policy_base_cations$y$layout$height <- NULL
    policy_base_cations$width <- NULL
    policy_base_cations$height <- NULL
    policy_base_cations%>%
      layout(autosize = TRUE)
  })
  
  #Al plot to show decrease in acids mean less Al released from soil
  output$policy_Al <- renderPlotly({
    policy_Al <- ggplot_function(reactive_data3_Al(), x5(), y3(), log = input$log5, input$units3, input$date_range_policy, solute_palette)
    #code to make the plot readjust to consistently fit the screen well
    policy_Al$x$layout$width <- NULL
    policy_Al$y$layout$height <- NULL
    policy_Al$width <- NULL
    policy_Al$height <- NULL
    policy_Al%>%
      layout(autosize = TRUE)
    })

  #output an interactive timeline for the history of acid rain
  output$timeline <- renderTimevis({
    timevis(timeline_data) #possibly create/use groups in order to contextualize
  })
  
})
