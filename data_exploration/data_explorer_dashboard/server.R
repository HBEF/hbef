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
library(stringr)

#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################


shinyServer(function(session, input, output) {

  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  Theme  ################
  my_theme <- 
    theme(rect = element_rect(fill = NA),
          panel.background = element_rect("transparent", colour = NA),
          panel.grid.major = element_line(colour = "#dddddd"), 
          panel.grid.major.x = element_line(colour = NA),
          text = element_text(family = "Helvetica", size = 12), 
          legend.position="none", legend.direction = "horizontal", legend.title = element_blank(),
          strip.text = element_text(margin = margin(20)),
          axis.title= element_text(size = 10, margin = margin(20)), 
          plot.margin = margin(1, 1, 0, 1, "cm"))
  
  color_cation <- c("Al" = "#240085", "Mg" = "#1D267A", "Ca" = "#164C6F", "NH4" = "#0F7364" , "Na" = "#089959", "K" = "#02C04E")
  color_anion <- c("PO4" = "#BB1D4C", "SO4" = "#BB1D4C", "NO3" = "#C83239", "SiO2"= "#D54726", "Cl" = "#E25C13", "HCO3" = "#F07100")
  
  
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
                         "Chloride (Cl)" = "Cl",
                         "Bicarbonate (HCO3)" = "HCO3")
  
  solutes_H <- list("Hydrogen (H)" = "H",
                    "pH" = "pH")
  
  all_solutes <- c(solutes_cations, solutes_anions, solutes_H)

  ###### function #######
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  formula_function_x <- function(df, s){
    q = quote(mutate(df, temporary_x = s))
    eval(parse(text=sub("s", s, deparse(q))))}
  
  formula_function_y <- function(df, s){
    q = quote(mutate(df, temporary_y = s))
    eval(parse(text=sub("s", s, deparse(q))))}

  
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
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################

  
  
  
  ########### DATA IMPORT ####################################################
  
  load("precip_streamflow_dfs.RData")
  
  imported_data <- precip_streamflow_long
  imported_data_wide<- precip_streamflow_wide
  
  names(imported_data)
  levels(as.factor(imported_data$solute))
  
  names(imported_data_wide)
  levels(as.factor(imported_data$solute))
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y  #########################################
  #Reactive Data Normal
  
  reactive_data_time <- reactive({
    data<- imported_data
    data <- data[data$granularity %in% input$granularity_time,]
    data <- data[data$solute %in% solutes(),] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
  })
  
  reactive_data_pq <- reactive({
    data<- imported_data
    data <- data[data$granularity %in% input$granularity,]
    data <- data[data$solute %in% "Ca",] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
  })
  
  reactive_data_cq <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity_cq,] 
    data <- data[data$solute %in% solutes(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds,]
    data <- data[data$source %in% "streamflow",]
  
    if(input$trace_cq){data <- accumulate_by(data, ~framey)}
    else{data}
    
  })
  
  
  solutesx_formula <- reactive({
    capitalized<-"^[[:upper:]][[:alpha:]]"
    strip_spaces <- gsub(" ", "", input$solutesx_formula, fixed = TRUE)
    include_space <- gsub("([^[:alnum:]])", " \\1 ", strip_spaces)
    trim <- str_trim(include_space)
    split_each <- str_split(trim, " ")
    include_units <- sapply(split_each, function(x) ifelse(str_detect(x, capitalized), paste(paste(input$units_bubble, "_", sep=""), x , sep=""), x))
    p_q_replace <- sapply(include_units, function(x) gsub("^[P|Q]", "water_mm", x, fixed = FALSE))
    formula <- paste(p_q_replace, collapse=' ' )
    formula
  })
  
  
  solutesy_formula <- reactive({
    capitalized<-"^[[:upper:]][[:alpha:]]"
    strip_spaces <- gsub(" ", "", input$solutesy_formula, fixed = TRUE)
    include_space <- gsub("([^[:alnum:]])", " \\1 ", strip_spaces)
    trim <- str_trim(include_space)
    split_each <- str_split(trim, " ")
    include_units <- sapply(split_each, function(x) ifelse(str_detect(x, capitalized), paste(paste(input$units_bubble, "_", sep=""), x , sep=""), x))
    p_q_replace <- sapply(include_units, function(x) gsub("^[P|Q]", "water_mm", x, fixed = FALSE))
    formula <- paste(p_q_replace, collapse=' ' )
    formula
  })
  
  reactive_data_bubble <- reactive({
    data <- imported_data_wide
    data <- data[data$granularity %in% input$granularity_bubble,]
    data <- data[data$ws %in% input$watersheds_bubble,]
    data <- data[data$source %in% input$water_sources_bubble,]
    #data <- data[data$date >= input$date_range[1] & data$date <= input$date_range[2]]
    unit_columns <- colnames(imported_data_wide[,(grep(input$units_bubble, colnames(data)))])
    
    basic_columns <- c("ws","date","water_date","water_year","source","water_mm", "pH", "anc", "dic","temp", "spcond", "framey")
    needed_columns <- c(basic_columns,unit_columns)
    data <- data[,needed_columns]
  
    data <- formula_function_x(data, solutesx_formula())
    data <- formula_function_y(data, solutesy_formula())
    
    if(input$trace_bubble){data <- accumulate_by(data, ~framey)}
    else{data}
    
  })
  
  

  x_time <- reactive({
    if(input$granularity_time == "week"){"date"}
    else if(input$granularity_time == "month"){"date"}
    else if(input$granularity_time == "year"){"water_year"}
  })
  
  
  y_time <- reactive({
    if(input$yaxis_time == "concentration"){as.character(input$units)}
    else {as.character(input$yaxis_time)}
  })
  
  animation_speed_cq <- reactive({
    (80)*(1/(input$animation_speed_cq))^2
  })
  
  animation_speed_bubble <- reactive({
    (80)*(1/(input$animation_speed_bubble))^2
  })
  
  
  ########### PLOT FUNCTIONS #########################################
  
  #### ---  GGPLOT BASIC PLOT FUNCTION
  basic_ggplot_function <- function(data, x, y, log){
    
    data_stream <- data[data$source %in% c("streamflow"),]
    data_precip <- data[data$source %in% c("precipitation"),]
    
    if(log == "log") {
      streamflow <- ggplot(data=data_stream, aes(x = as.POSIXct(date), y = logb(water_mm, base=exp(1)), color = ws)) + my_theme +
        geom_line()+
        geom_point(size = 1.3, fill = "white", stroke = 0.2) +
        labs(x = "Date", y = "log(Q (mm))")+
        scale_colour_grey()}
    
    else{
      streamflow <- ggplot(data=data_stream, aes(x = as.POSIXct(date), y = water_mm, color = ws)) + my_theme +
        geom_line()+
        geom_point(size = 1.3, fill = "white", stroke = 0.2)+
        labs(x = "Date", y = "Q (mm)")+
        scale_colour_grey()}
    
    precipitation <- ggplot()+ my_theme + 
                                geom_bar(data=data_precip, aes(x = as.POSIXct(date), y = water_mm, fill = ws), stat = "identity", position = "dodge", key = date)+ 
                                scale_y_reverse() + labs(x = "Date", y = "P (mm)")+
                                scale_fill_grey()
    
    streamflow <- ggplotly(streamflow, showlegend = FALSE)
    precipitation <- ggplotly(precipitation)
    
    plot <- subplot(precipitation, streamflow, nrows = 2, shareX = TRUE, heights = c(0.5, 0.5), titleY = TRUE)%>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
    plot
  }
  
  
  #### --- GGPLOT TIME FUNCTION
  ggplot_time_function <- function(data, x, y, log, y_label){
    
    if(log == "log") {
      plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute, shape = source, alpha = ws))+
      labs(x = "Water Year", y = paste("log", "(",y_label, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws))+
      labs(x = "Water Year", y = y_label)}
    
      plot <- plot+ my_theme + geom_line(size = 0.5) + 
      geom_point(size = 1.3, fill = "white", stroke = 0.2) +  
      #xlim(min(as.Date(event_data("plotly_relayout")[1])), as.Date(max(event_data("plotly_relayout")[2])))+
        
      #xlim(min(input$date_range[1]), max(input$date_range[2]))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(plot) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }

  
  #### --- GGPLOT BUBBLE PLOT
  
  ggplot_bubble_function <- function(data, x, y, log_x, log_y, speed, trace){
    
    if(trace){
      plot <- ggplot(data=data, aes(shape = source, frame = frame, alpha = ws)) + my_theme + 
        scale_shape_manual(values= source_shapes)+
        scale_color_manual(values = solute_palette)}
    
    else{
      plot <- ggplot(data=data, aes(shape = source, frame = framey, alpha = ws)) + my_theme + 
        scale_shape_manual(values= source_shapes)+
        scale_color_manual(values = solute_palette)}
      
    
    if(log_y == "log"){
      
      if(log_x == "log"){
      plot <- plot + geom_point(aes(x = logb(get(x), base=exp(1)), y = logb(get(y), base=exp(1)), color = solute), stroke= 0.2) +
        labs(y = "")}
      else{
      plot <- plot + geom_point(aes(x = get(x), y = logb(get(y), base=exp(1)), color = solute), stroke= 0.2) +
        labs(y = "")}
    }
    
    else{
      
      if(log_x == "log"){
        plot <- plot + geom_point(aes(x = logb(get(x), base=exp(1)), y = get(y), color = solute), stroke= 0.2) +
          labs(y = "")}
      else{
        plot <- plot + geom_point(aes(x = get(x), y = get(y), color = solute), stroke= 0.2) +
          labs(y = "")}
    }
    
    plot <- plot + 
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>% 
      animation_opts(frame = speed, transition = 0, redraw = FALSE) %>% 
      animation_slider(currentvalue = list(prefix = "Water Year ", font = list(size = 15))) %>% 
      animation_button(font = list(size = 12))
  }
  
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  output$plot_pq <- renderPlotly({
    theplot <- basic_ggplot_function(reactive_data_pq(), x(), y(), log = input$log_pq) 
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, legend = list(orientation = 'h', x = 0, y = 1.2))
     })
  
  output$plot_time <- renderPlotly({
    theplot <- ggplot_time_function(reactive_data_time(), x_time(), y_time(),log = input$log_time, y_time())
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, legend = list(orientation = 'h', x = 0, y = 1.2))
  })
  
  output$plot_cq <- renderPlotly({
    theplot <- ggplot_bubble_function(reactive_data_cq(), "water_mm", input$units, input$log_cq_x,input$log_cq_y, animation_speed_cq(), input$trace_cq)
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE)
    
  })
  
  
  #input log was an input cause the function thing. 
  
  output$plot_flux <- renderPlotly({
    theplot <- ggplot_time_function(reactive_data_time(), x_time(), "flux", log = input$log_flux, "flux")
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE)
  })
  
  output$bubblePlot <- renderPlotly({
    theplot <- ggplot_bubble_function(reactive_data_bubble(), "temporary_x", "temporary_y", input$log_bubble_x, input$log_bubble_y, animation_speed_bubble(), input$trace_bubble)
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, height = 600)
    
  })
  

  output$zoom <- renderPrint({
    d <- event_data("plotly_relayout")
    d
  })
  
  
})
