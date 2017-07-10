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
library(data.table)


#######################################################################################
########### SHINY SERVER FOR EXPLORATORY IN CHEMICAL SPACE ############################
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
  source_shapes <- c("streamflow" = 16, "precipitation"= 21)
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

  ############
  
  ###### function #######
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
    
  
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
  
  load("precip_streamflow_dfs.RData")
  
  imported_data <- precip_streamflow_wide
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y  #########################################
  #Reactive Data Normal
  
  reactive_data <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity,]
    data <- data[data$ws %in% input$watersheds,]
    data <- data[data$source %in% input$water_sources,]
    #data <- data[data$date >= input$date_range[1] & data$date <= input$date_range[2]]
    unit_columns <- colnames(imported_data[,(grep(input$units, colnames(data)))])

    # if(input$granularity == "month"){
    #   date_range_columns <- colnames(data[,(grep("month", colnames(data))), with = FALSE])
    #   final_columns <-intersect(unit_columns, date_range_columns)
    #   }
    # else{
    #   date_range_columns <- colnames(data[,(grep("year", colnames(data))), with = FALSE])
    #   final_columns <-intersect(unit_columns, date_range_columns)
    # }
  
    basic_columns <- c("ws","date","water_date","water_year","source","water_mm","framey")
    needed_columns <- c(basic_columns,unit_columns)
    data <- data[,needed_columns]
    
    # if(input$granularity == "year"){
    #   data <- data[!duplicated(data[,c("water_year","source", "ws", "framey")]),]}
    # 
    # else{data}
    
    if(length(input$solutesx) == 1){data}
    else{ solutes_to_add <- colnames(data[,grep(paste(input$solutesx, collapse="|"), names(data))])
    data$sum_temporary_x = rowSums(data[,  solutes_to_add, with = FALSE], na.rm=TRUE)
    }
    
    if(length(input$solutesy) == 1){data}
    else{ solutes_to_add <- colnames(data[,grep(paste(input$solutesy, collapse="|"), names(data))])
    data$sum_temporary_y = rowSums(data[,  solutes_to_add, with = FALSE], na.rm=TRUE)
    }
    
    if(input$trace){data <- accumulate_by(data, ~framey)}
    else{data}
  
  })
  
  x <- reactive({
    if(length(input$solutesx) == 1) {colnames(reactive_data()[grep(input$solutesx, names(reactive_data()))])}
    else{
      "sum_temporary_x"
    }
  })
  
  y <- reactive({
    if(length(input$solutesy) == 1) {colnames(reactive_data()[grep(input$solutesy, names(reactive_data()))])}
    else{"sum_temporary_y"}
  })
  
  
  log_transform <- reactive({
    if(input$log == "ln"){"transform"}
    else{"no_transform"}
  })
  
  animation_speed <- reactive({
    (80)*(1/(input$animation_speed))^2
  })
  
  ########### PLOT FUNCTIONS #########################################
  

  ggplot_bubble_function <- function(data, x, y, ncol = NULL, nrow = NULL){
    if(input$trace){
      plot <- ggplot(data=data) + my_theme + 
      geom_point(aes(x = get(x), y = get(y), shape = source, 
                     size = water_mm, color = water_year, frame = frame), stroke= 1, alpha = 0.8) +
      scale_shape_manual(values= source_shapes)+ 
      labs(y = "")}
    
    else{
    plot <- ggplot(data=data) + my_theme + 
      geom_point(aes(x = get(x), y = get(y), shape = source, 
                       size = water_mm, color = water_year, frame = framey), stroke= 1, alpha = 0.8) +
      scale_shape_manual(values= source_shapes)+ 
      labs(y = "")
      
    }
    
    ggplotly(plot, tooltip = "text",
             width = 900) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>% 
      animation_opts(frame = animation_speed(), transition = 0, redraw = FALSE)
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
