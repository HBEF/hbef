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
library(lattice)
library(directlabels)


#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################


shinyServer(function(session, input, output) {

  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  Theme  ################
  my_theme <- theme_fivethirtyeight() + 
    theme(rect = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "#dddddd"), 
          text = element_text(family = "Helvetica", size = 14), 
          legend.position = "none", legend.direction = "vertical", legend.title = element_blank(),
          strip.text = element_text(hjust = 1, size = 12, face = "bold"), 
          axis.title= element_text(NULL), axis.title.x= element_blank(), 
          axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
  
  color_cation <- c("Al" = "#162338", "Mg" = "#273D64", "Ca" = "#3B5C95", "NH4" = "#4E7AC7" , "Na" = "#7195D2", "K" = "#95AFDD")
  color_anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("flow" = 16, "precip"= 21)
  source_color <- c("flow" = "#505050", "precip"= "#CCCDD9")
  
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
  
  solutes <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
  solutes2 <- reactive({c(input$solutes_cations2, input$solutes_anions2, input$solutes_H2)})
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################

  
  
  
  ########### DATA IMPORT ####################################################
  
  load("precip_stream_dfs.RData")
  
  imported_data <- precip_stream_data_wide
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y  #########################################
  #Reactive Data Normal
  
  reactive_data <- reactive({
    data <- imported_data
      data <- data[data$ws %in% input$ws,]
      data <- data[data$source %in% input$source,]
      unit_columns <-colnames(imported_data[,(grep(input$units, colnames(data))), with = FALSE])
      basic_columns <- c("ws","date","water_date","water_year","source","water_mm_pm")
      needed_columns <- c(unit_columns,basic_columns)
      data <- data[,needed_columns, with = FALSE]
      #data <- data[water_date %between% c(input$timeframe[1], input$timeframe[2])]
  })
  
  
  x <- reactive({
    colnames(reactive_data()[,grep(solutes(), names(reactive_data())), with = FALSE])
  })
  
  y <- reactive({
    colnames(reactive_data()[,grep(solutes2(), names(reactive_data())), with = FALSE])
  })
  
  log_transform <- reactive({
    if(input$log == "ln"){"transform"}
    else{"no_transform"}
  })
  
  ########### PLOT FUNCTIONS #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function <- function(data, x, y, ncol = NULL, nrow = NULL, log){
    
  
    if(log) {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
      labs(x = "Water Year", y = paste("log", "(",input$units, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
      labs(x = "Water Year", y = input$units)}
    
      plot <- plot+ my_theme + geom_line(size = 1) + 
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes(text = paste("Solute: ", solute, "<br>", 
                                   "Water Source: ", source, "<br>", 
                                   "Watershed: ", ws, "<br>", 
                                   "Date: ", get(x), "<br>", 
                                    "Value:", get(y)))) + 
      xlim(min(input$date_range[1]), max(input$date_range[2]))+ 
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    plot1 <- ggplotly(plot, tooltip = "text",
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
    plot2 <- ggplot(data = reactive_data_PQ(), aes(x = get(x()), y = get(y_PQ()))) + my_theme+
      geom_bar(aes(alpha = ws, fill = source),stat = "identity", position="dodge")+
      labs(x = "Water Year", y = "mm")+
      facet_grid(source ~.)+
      xlim(min(input$date_range[1]), max(input$date_range[2]))+
      scale_fill_manual(values = source_color)+
      scale_alpha_discrete(range = c(0.9, 0.5))
        
    
    plot2 <- ggplotly(  
      plot2,
      width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
    
    subplot(plot1, plot2, nrows = 2, shareX = TRUE, heights = c(0.8, 0.2), titleY = TRUE)
    
  }
  
  ## GGPLOT DIFF FUNCTION
  ggplot_diff_function <- function(data, x, y, ncol = NULL, nrow = NULL){
    final <- ggplot(data=data, aes(x = get(x), y = get(y), fill = solute, text = paste("Solute: ", solute, "<br>",
                                                                                        "Watershed: ", ws, "<br>", 
                                                                                        "Date: ", get(x), "<br>", 
                                                                                         "Value:", get(y)))) + 
      my_theme +
      geom_bar(stat= "identity") +
      facet_grid(ws~solute) +
      xlim(min(input$date_range[1]), max(input$date_range[2]))+ 
      labs(x = "Water Year", y = input$units) +
      scale_fill_manual(values = solute_palette)
  
  ggplotly(  
    final, tooltip = "text",
    width = 900) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE)  
    
  }
  
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  output$plot1a <- renderPlot({
    ggplot(data=reactive_data()) + 
      geom_point(aes(x = get(x()), y = get(y()), shape = source, size = water_mm_pm, color = ws), stroke = 2)+
      scale_shape_manual(values= c(1, 16))
     })


  
  
  

  
})
