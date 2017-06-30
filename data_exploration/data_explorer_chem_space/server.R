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
  
  units <- list("uEquivalent/L" = "^concentration_ueq_",
                "uMole/L" = "^concentration_umol_", 
                "uMg/L" = "^concentration_mg_", 
                "flux" = "^flux_")

  ########### END OF IMPORTANT PRELIMINARY INFO #############################################
  
  
  
  
  ########### SIDEBAR FUNCTIONS ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  observeEvent(input$select_all_ws, {
    if(input$select_all_ws == 0) {updateCheckboxGroupInput(session, "watersheds", selected = "ws1")}
    else if (input$select_all_ws%%2 == 0){updateCheckboxGroupInput(session, "watersheds", selected = "ws1")}
    else{updateCheckboxGroupInput(session, "watersheds", selected = watersheds)}
  })
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################

  
  
  
  ########### DATA IMPORT ####################################################
  
  load("precip_stream_dfs.RData")
  
  imported_data <- precip_stream_data_wide
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y  #########################################
  #Reactive Data Normal
  
  reactive_data <- reactive({
    data <- imported_data
    data <- data[data$ws %in% input$watersheds,]
    data <- data[data$source %in% input$water_sources,]
    unit_columns <- colnames(imported_data[,(grep(input$units, colnames(data))), with = FALSE])
    basic_columns <- c("ws","date","water_date","water_year","source","water_mm_pm")
    needed_columns <- c(unit_columns, basic_columns)
    data <- data[,needed_columns, with = FALSE]
    
    
    
    data <- imported_data
    data <- data[data$ws %in% c("6"),]
    data <- data[data$source %in% c("precip"),]
    str(data)
    unit_columns <- colnames(imported_data[,(grep("^concentration_ueq_", colnames(data))), with = FALSE])
    basic_columns <- c("ws","date","water_date","water_year","source","water_mm_pm")
    needed_columns <- c(unit_columns, basic_columns)
    data <- data[,needed_columns, with = FALSE]
    
    
    
    
    #data <- data[water_date %between% c(input$timeframe[1], input$timeframe[2])]
  })
  
  
  x <- reactive({
    colnames(reactive_data()[,grep(input$solutesx, names(reactive_data())), with = FALSE])
  })
  
  y <- reactive({
    colnames(reactive_data()[,grep(input$solutesy, names(reactive_data())), with = FALSE])
  })
  
  log_transform <- reactive({
    if(input$log == "ln"){"transform"}
    else{"no_transform"}
  })
  
  ########### PLOT FUNCTIONS #########################################
  
  ggplot_bubble_function <- function(data, x, y, ncol = NULL, nrow = NULL){
    plot <- ggplot(data=data) + my_theme + 
      geom_point(aes(x = get(x), y = get(y), shape = source, 
                     size = water_mm_pm, color = water_year), stroke= 1, alpha = 0.8) +
      scale_shape_manual(values= c(1, 16))
    ggplotly(plot, tooltip = "text",
             width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) 
  }
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  output$bubblePlot <- renderPlotly({
    theplot <- ggplot_bubble_function(reactive_data(), x(), y())
    
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE)
    
     })

  
  
})
