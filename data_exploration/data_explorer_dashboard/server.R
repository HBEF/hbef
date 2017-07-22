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
library(magrittr)

#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################


shinyServer(function(input, output, session) {

  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  Theme  ################
  my_theme <- 
    theme(rect = element_rect(fill = NA),
          panel.background = element_rect("transparent", colour = NA),
          panel.grid.major = element_line(colour = "#dddddd"), 
          panel.grid.major.x = element_line(colour = NA),
          text = element_text(family = "Helvetica", size = 12), 
          legend.position="none", legend.direction = "horizontal", legend.title = element_blank(),
          #strip.text = element_text(margin = margin(20)),
          axis.title = element_text(size = 10, margin = unit(c(3, 3, 3, 3), "cm")),
          plot.margin = margin(1, 1, 1, 1, "cm"))
  
  color_cation <- c("Al" = "#240085", "Mg" = "#1D267A", "Ca" = "#164C6F", "NH4" = "#0F7364" , "Na" = "#089959", "K" = "#02C04E")
  color_anion <- c("PO4" = "#BB1D4C", "SO4" = "#BB1D4C", "NO3" = "#C83239", "SiO2"= "#D54726", "Cl" = "#E25C13", "HCO3" = "#F07100")
  
  
  color_cation <- c("Al" = "#162338", "Mg" = "#273D64", "Ca" = "#3B5C95", "NH4" = "#4E7AC7" , "Na" = "#7195D2", "K" = "#95AFDD")
  color_anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  ws_palette <- c("1" = "#fae550", "2" = "#a8db40", "3" = "#62c74a", "4" = "#408b77", 
                  "5" = "#27517b", "6" = "#303475", "7" = "#351042", "8" = "#79276e", "9" = "#b63462")
  source_shapes <- c("streamflow" = 16, "precipitation"= 21)
  source_color_solutemd <- c("streamflow_Al" = "#162338", "streamflow_Mg" = "#273D64", "streamflow_Ca" = "#3B5C95", "streamflow_NH4" = "#4E7AC7" , 
                    "streamflow_Na" = "#7195D2", "streamflow_K" = "#95AFDD","streamflow_PO4" = "#600B0B", "streamflow_SO4" = "#8F1010", 
                    "streamflow_NO3" = "#BF1616", "streamflow_SiO2"= "#CC4545", "streamflow_Cl" = "#D97373", "streamflow_HCO3" = "#E5A2A2",
                     "streamflow_H" = "#FFE79C", 
                    "precipitation_Al" = "#FFFFFF", "precipitation_Mg" = "#FFFFFF", "precipitation_Ca" = "#FFFFFF", "precipitation_NH4" = "#FFFFFF" , 
                    "precipitation_Na" = "#FFFFFF", "precipitation_K" = "#FFFFFF","precipitation_PO4" = "#FFFFFF", "precipitation_SO4" = "#FFFFFF", 
                    "precipitation_NO3" = "#FFFFFF", "precipitation_SiO2"= "#FFFFFF", "precipitation_Cl" = "#FFFFFF", "precipitation_HCO3" = "#FFFFFF",
                    "precipitation_H" = "#FFFFFF",
                    "precipitation"= "#FFFFFF")
  
  source_color_wsmd <- c("streamflow_1" = "#fae550", "streamflow_2" = "#a8db40", "streamflow_3" = "#62c74a", "streamflow_4" = "#408b77", 
  "streamflow_5" = "#27517b", "streamflow_6" = "#303475", "streamflow_7" = "#351042", "streamflow_8" = "#79276e", "streamflow_9" = "#b63462",
  "precipitation_1" = "#FFFFFF", "precipitation_2" = "#FFFFFF", "precipitation_3" = "#FFFFFF", "precipitation_4" = "#FFFFFF", 
  "precipitation_5" = "#FFFFFF", "precipitation_6" = "#FFFFFF", "precipitation_7" = "#FFFFFF", "precipitation_8" = "#FFFFFF", "precipitation_9" = "#FFFFFF")

  
  grey_palette <- c("#222222", "#999999", "#555555","#777777","#333333", "#888888",  "#444444", "#666666")
  
  grey_palette_2 <- c("streamflow_Al" = "#222222", "streamflow_Mg" = "#222222", "streamflow_Ca" = "#222222", "streamflow_NH4" = "#222222" , 
                      "streamflow_Na" = "#222222", "streamflow_K" = "#222222","streamflow_PO4" = "#222222", "streamflow_SO4" = "#222222", 
                      "streamflow_NO3" = "#222222", "streamflow_SiO2"= "#222222", "streamflow_Cl" = "#222222", "streamflow_HCO3" = "#222222",
                      "streamflow_H" = "#222222", 
                      "precipitation_Al" = "#FFFFFF", "precipitation_Mg" = "#FFFFFF", "precipitation_Ca" = "#FFFFFF", "precipitation_NH4" = "#FFFFFF" , 
                      "precipitation_Na" = "#FFFFFF", "precipitation_K" = "#FFFFFF","precipitation_PO4" = "#FFFFFF", "precipitation_SO4" = "#FFFFFF", 
                      "precipitation_NO3" = "#FFFFFF", "precipitation_SiO2"= "#FFFFFF", "precipitation_Cl" = "#FFFFFF", "precipitation_HCO3" = "#FFFFFF",
                      "precipitation_H" = "#FFFFFF",
                      "precipitation"= "#FFFFFF")
  
  water_sources <- list("Streamflow (Q)" = "streamflow", 
                        "Precipitation (P)" = "precipitation")
  
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
  
  solutes_H <- list("Hydrogen (H)" = "H", "Dissolved Inorganic Carbon" = "DIC")
  
  
  all_solutes <- c(solutes_cations, solutes_anions, solutes_H)
  

  ############ FUNCTIONS ##################################
  
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

  #Solutesx_formula and solutesy_formula parse the formula inputed 
  #by the user in the bubble plot to match column names
  

  solutesx_formula <- reactive({
    capitalized <-"(^[[:upper:]][[:alpha:]])|^H"
    strip_spaces <- gsub(" ", "", input$solutesx_formula, fixed = TRUE)
    include_space <- gsub("([^[:alnum:]])", " \\1 ", strip_spaces)
    trim <- str_trim(include_space)
    split_each <- str_split(trim, " ")
    include_units <- sapply(split_each, function(x) ifelse(str_detect(x, capitalized), paste(paste(input$units_bubble, "_", sep=""), x , sep=""), x))
    p_q_replace <- sapply(include_units, function(x) gsub("^[P|Q]", "water_mm", x, fixed = FALSE))
    add_source <- sapply(p_q_replace, function(x) ifelse(str_detect(x, "date"), x, paste(x, paste("_", input$solutesx_source, sep=""), sep="")))    
    formula <- paste(add_source, collapse=' ' )
    formula
  })
  
  
  solutesy_formula <- reactive({
    capitalized<-"^[[:upper:]][[:alpha:]]|^H"
    strip_spaces <- gsub(" ", "", input$solutesy_formula, fixed = TRUE)
    include_space <- gsub("([^[:alnum:]])", " \\1 ", strip_spaces)
    trim <- str_trim(include_space)
    split_each <- str_split(trim, " ")
    include_units <- sapply(split_each, function(x) ifelse(str_detect(x, capitalized), paste(paste(input$units_bubble, "_", sep=""), x , sep=""), x))
    p_q_replace <- sapply(include_units, function(x) gsub("^[P|Q]", "water_mm", x, fixed = FALSE))
    add_source <- sapply(p_q_replace, function(x) ifelse(str_detect(x, "[[:alpha::]]"), paste(x, paste("_", input$solutesy_source, sep=""), sep=""), x))    
    formula <- paste(add_source, collapse=' ' )
    formula
  })
  
  ########### END OF IMPORTANT PRELIMINARY INFO #############################################
  
  
  
  
  ########### SIDEBAR FUNCTIONS ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  
  observeEvent(input$granularity_global, {
    choices <- input$granularity_global
    updateSelectInput(session, "granularity", selected = choices)
    updateSelectInput(session, "granularity_time", selected = choices)
    updateSelectInput(session, "granularity_cq", selected = choices)
    updateSelectInput(session, "granularity_flux", selected = choices)
  })
  
  observeEvent(input$log_global_y, {
    choices <- input$log_global_y
    updateSelectInput(session, "log_pq", selected = choices)
    updateSelectInput(session, "log_time", selected = choices)
    updateSelectInput(session, "log_cq_x", selected = choices)
    updateSelectInput(session, "log_flux", selected = choices)
  })
  
  observeEvent(input$animate_global, {
    value <- input$animate_global
    updateSelectInput(session, "animate_cq", selected = value)
    updateSelectInput(session, "animate_time", selected = value)
    updateSelectInput(session, "animate_flux", selected = value)
  })

  observeEvent(input$animation_speed_global, {
    value <- input$animation_speed_global 
    updateSelectInput(session, "animation_speed_cq", selected = value)
    updateSelectInput(session, "animation_speed_time", selected = value)
    updateSelectInput(session, "animation_speed_flux", selected = value)
  })
  
  observeEvent(input$trace_global, {
    value <- input$trace_global
    updateSelectInput(session, "trace_cq", selected = value)
    updateSelectInput(session, "trace_time", selected = value)
    updateSelectInput(session, "trace_flux", selected = value)
  })
  
  
  observeEvent(input$colormode_global, {
    if(input$colormode_global == "ws") {
      updateSelectizeInput(session, "watersheds", options = list(maxItems = 9))
    }
    else{
      updateSelectizeInput(session, "watersheds", options = list(maxItems = 5))
    }
  })
  
  observe({
    if(input$colormode_global == "ws" & length(input$solutes_cations) + length(input$solutes_anions) + length(input$solutes_H)> 5){
      updateCheckboxGroupInput(session, "solutes_cations", selected=head(input$solutes_cations,length(input$solutes_cations)))
      updateCheckboxGroupInput(session, "solutes_anions", selected= head(input$solutes_anions,5-length(input$solutes_cations)))
      updateCheckboxGroupInput(session, "solutes_H", selected= head(input$solutes_H,5-length(input$solutes_cations)-length(input$solutes_anions)))}
      })
  
  observeEvent(input$select_all_ions, {
    if(input$select_all_ions == 0) {}
    else if (input$select_all_ions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations", selected = "K")}
    else{
      updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)
      updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)
      updateCheckboxGroupInput(session, "solutes_H", selected = solutes_H)}
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
  
  solutes <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
  
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################

  
  
  
  ########### DATA IMPORT ####################################################
  
  load("precip_streamflow_dfs.RData")
  
  imported_data <- precip_streamflow_long
  imported_data %<>% 
    mutate(fill_color_solutemd = paste(source, solute, sep = '_')) %>% 
    mutate(fill_color_wsmd = paste(source, ws, sep = '_'))
  imported_data_wide<- precip_streamflow_wide
  imported_data_super_wide<- precip_streamflow_super_wide
  levels(as.factor(imported_data$solute))
  levels(as.factor(imported_data$solute))
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA FOR EACH PLOT #########################################
  
  ###### >>>>>>> Reactive Time Plot <<<<<<<<<< #####
  eventReactive_data_time <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    data$date<- as.POSIXct(data$date)
    data$water_year<- as.POSIXct(data$water_year)
    #data <- data[data$granularity %in% input$granularity_time,]
    data <- data[data$source %in% input$water_sources,]
    data <- data[data$solute %in% solutes(),] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
    
    if(input$trace_time == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}
  })
  
  #This separation is done so that people can change granularity when zoomed in without replotting and zooming out. 
  reactive_data_time <- reactive({
    data <- eventReactive_data_time()[eventReactive_data_time()$granularity %in% input$granularity_time,]
  })
  
  # X axis for Time Plot
  x_time <- reactive({
    if(input$granularity_time == "week"){"date"}
    else if(input$granularity_time == "month"){"date"}
    else if(input$granularity_time == "year"){"water_year"}
  })
  
  # Y axis for Time Plot
  y_time <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    if(input$yaxis_time == "concentration"){as.character(input$units)}
    else if(input$yaxis_time == "chargebalance"){as.character(input$units)}
    else {as.character(input$yaxis_time)}
  })
  
  y_labs <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    if(input$yaxis_time == "concentration"){
      if(input$units == "concentration_ueq"){"uEquivalent/L"}
      else if(input$units == "concentration_mg"){"mg/L"}
      else if(input$units == "concentration_umol"){"uMole/L"}}
    else{""
    }
    
  })
  
  #Animation Speed Bubble Plot
  animation_speed_time <- reactive({
    (80)*(1/(input$animation_speed_time))^2
  })
  
  
  #Coloring for Time pLot
  line_coloring <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    if(input$yaxis_time %in% c("concentration", "chargebalance") & input$colormode_global == "solute"){solute_palette}
    else if(input$colormode_global == "ws"){ws_palette}
    else{grey_palette}    
  })
  
  fill_coloring <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
        if(input$yaxis_time %in% c("concentration", "chargebalance") & input$colormode_global == "solute"){source_color_solutemd}
        else if(input$colormode_global == "ws"){source_color_wsmd}
        else{grey_palette_2}   
      })
  
  observeEvent({input$go_exploratory |
      input$lastkeypresscode},{if(!(input$yaxis_time %in% c("concentration", "pH", "chargebalance"))){
    updateSelectInput(session, "granularity_time",
                      selected = "week")}})
    
  observe({if(!(input$yaxis_time %in% c("concentration", "pH"))){
    updateSelectInput(session, "water_sources",
                      selected = "streamflow")}
    else{
    updateSelectInput(session, "water_sources",
                      selected = c("streamflow", "precipitation"))
    }})
  
  ###### >>>>>>> Reactive Charge Plot <<<<<<<<<< #####
  
  eventReactive_data_charge <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    #data <- data[data$granularity %in% input$granularity_time,]
    data <- data[data$source %in% "precipitation",]
    data <- data[data$solute %in% solutes(),] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
    data %<>% 
      mutate(charge_ueq = ifelse(solute %in% solutes_anions, -1*(concentration_ueq), concentration_ueq))
  
  })
  
  #This separation is done so that people can change granularity when zoomed in without replotting and zooming out. 
  reactive_data_charge <- reactive({
    data <- eventReactive_data_charge()[eventReactive_data_charge()$granularity %in% input$granularity_time,]
  })

  ###### >>>>>>> End of Reactive Charge Plot <<<<<<<<<< #####
  
  
  
  ###### >>>>>>> Reactive PQ Plot <<<<<<<<<< #####
  eventReactive_data_pq <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    #data <- data[data$granularity %in% input$granularity,]
    data <- data[data$solute %in% "Ca",] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
  })
  
  #This separation is done so that people can change granularity when zoomed in without replotting and zooming out. 
  reactive_data_pq <- reactive({
    data <- eventReactive_data_pq()[eventReactive_data_pq()$granularity %in% input$granularity,]
  })
  
  
  coloring_pq <- reactive({
    if(input$colormode_global == "solute"){grey_palette}
    else{ws_palette}
  })
  
  
  ###### >>>>>>> Reactive cQ Plot <<<<<<<<<< #####
  eventReactive_data_cq <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data <- imported_data
    d <- event_data("plotly_selected")
    #data <- data[data$granularity %in% input$granularity_cq,] 
    data <- data[data$solute %in% solutes(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds,]
    data <- data[data$source %in% "streamflow",]
    
    if(input$trace_cq == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}
    
  })
  
  #This separation is done so that people can change granularity when zoomed in without replotting and zooming out. 
  reactive_data_cq <- reactive({
    d <- event_data("plotly_selected")
    data <- eventReactive_data_cq()
    if (!is.null(d$x)){
      data <- data[eventReactive_data_cq()$granularity %in% input$granularity_cq,]
      data <- data[as.numeric(as.POSIXct(data$date)) >= min(d$x) & as.numeric(as.POSIXct(data$date)) <= max(d$x),]
      }
    else{
      data <- eventReactive_data_cq()[eventReactive_data_cq()$granularity %in% input$granularity_cq,]
    }
  })
  
  #Animation Speed cQ Plot
  animation_speed_cq <- reactive({
    (80)*(1/(input$animation_speed_cq))^2
  })

  
  ###### >>>>>>> End of Reactive cQ Plot <<<<<<<<<< #####
  
  
  
  ###### >>>>>>> Reactive Flux Plot <<<<<<<<<< #####
  eventReactive_data_flux <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    data$date<- as.POSIXct(data$date)
    data$water_year<- as.POSIXct(data$water_year)
    #data <- data[data$granularity %in% input$granularity_flux,]
    data <- data[data$source %in% input$water_sources,]
    data <- data[data$solute %in% solutes(),] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
    
    if(input$trace_flux == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}

  })
  
  #This separation is done so that people can change granularity when zoomed in without replotting and zooming out. 
  reactive_data_flux <- reactive({
    data <- eventReactive_data_flux()[eventReactive_data_flux()$granularity %in% input$granularity_flux,]
  })
  
  ## X axis for Flux Plot
  x_flux <- reactive({
    if(input$granularity_flux == "week"){"date"}
    else if(input$granularity_flux == "month"){"date"}
    else if(input$granularity_flux == "year"){"water_year"}
  })
  
  #Animation Speed Flux
  animation_speed_flux <- reactive({
    (80)*(1/(input$animation_speed_flux))^2
  })
  
  
  ###### >>>>>>> End of Reactive Flux Plot <<<<<<<<<< #####
  
  
  
  ###### >>>>>>> Reactive Bubble Plot <<<<<<<<<< #####
  reactive_data_bubble <- eventReactive(input$go_bubble, {
    data <- imported_data_super_wide
    data$framey <- as.numeric(data$framey)
    data <- data[data$granularity %in% input$granularity_bubble,]
    data <- data[data$ws %in% input$watersheds_bubble,]
    data <- data[data$date >= input$date_range_bubble[1] & data$date <= input$date_range_bubble[2]]
    
    #code below creates temporary_x and temporary_y
    #there are the columns that have the appropriate data 
    #given the column
    
    text_no_x <- "PLEASE TYPE A FORMULA FOR THE X AXIS IN THE BOX BELOW.

    Try any of the following possibilities:
    
    Solutes: 
    • Al, Ca, Cl, H, HCO3, K, Mg, Na, NO3, PO4, SiO2, SO4
    
    Hydrologic Flux:
    • P for precipitation in mm
    • Q for streamflow in mm
    
    Others: 
    • pH for pH
    • anc for acid neutralizing capacity (only available for streamflow)
    • spcond for specific conductivity (only available for streamflow)
    • temp for temperature (only available for streamflow)
    
    You can also perform arithmetic operations on the solutes using: +, - , *, /, ^
    
    For example, try typing: Na + Mg"
    
    text_no_y <- "PLEASE TYPE A FORMULA FOR THE Y AXIS IN THE BOX ABOVE.

    Try any of the following possibilities:
    
    Solutes: 
    • Al, Ca, Cl, H, HCO3, K, Mg, Na, NO3, PO4, SiO2, SO4
    
    Hydrologic Flux:
    • P for precipitation in mm
    • Q for streamflow in mm
    
    Others: 
    • pH for pH
    • anc for acid neutralizing capacity (only available for streamflow)
    • spcond for specific conductivity (only available for streamflow)
    • temp for temperature (only available for streamflow)
    
    You can also perform arithmetic operations on the solutes using: +, - , *, /, ^
    
    For example, try typing: Na + Mg"
    
    text_pq_conflict <- "When looking at precipitation (P) you can't see streamflow (Q) in mm. 
When looking at streamflow (Q) you can't see precipitation (P) in mm. 

Try typing P and matching the dropdown menu by selecting P or
try typing Q and matching the dropdown menu by selecting Q"
    
    #X axis validation
    if(input$solutesx_source == "precipitation") {
      validate(
        need(input$solutesx_formula != "", text_no_x),
        need(input$solutesx_formula != "Q", text_pq_conflict), 
        need(input$solutesx_formula != "q", text_pq_conflict))}
    if(input$solutesx_source == "streamflow") {
      validate(
        need(input$solutesx_formula != "", text_no_x),
        need(input$solutesx_formula != "P",text_pq_conflict),
        need(input$solutesx_formula != "p", text_pq_conflict))}
    
    #Y axis validation
    if(input$solutesy_source == "precipitation") {
      validate(
        need(input$solutesy_formula != "", text_no_y),
        need(input$solutesy_formula != "Q", text_pq_conflict),
        need(input$solutesx_formula != "q", text_pq_conflict))}
    if(input$solutesy_source == "streamflow") {
      validate(
        need(input$solutesy_formula != "", text_no_y),
        need(input$solutesy_formula != "P", text_pq_conflict),
        need(input$solutesx_formula != "p", text_pq_conflict))}
    
    
    data <- formula_function_x(data, solutesx_formula())
    data <- formula_function_y(data, solutesy_formula())
    
    if(input$trace_bubble == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}
    
  })

  #Animation Speed Bubble Plot
  animation_speed_bubble <- reactive({
    (80)*(1/(input$animation_speed_bubble))^2
  })
  
  #sizing for Bubble plot
  sizing <- reactive({
    if(input$sizing_bubble == 1){c(2,2)}
    else{c(1,6)}
  })
  
  ###### >>>>>>> End of Reactive Bubble Plot <<<<<<<<<< #####
  
  ########### END OF REACTIVE DATA FOR EACH PLOT #########################################
  
  
  
  ########### PLOT FUNCTIONS #########################################
  
  #### ---------  GGPLOT BASIC PQ PLOT FUNCTION-----------------------
  basic_ggplot_function <- function(data, x, y, log, color_scale){
    
    data_stream <- data[data$source %in% c("streamflow"),]
    data_precip <- data[data$source %in% c("precipitation"),]
    
    
    #Streamflow Plot
    streamflow <- ggplot(data=data_stream, aes(x = as.POSIXct(date), y = water_mm, color = ws)) + 
      my_theme +
      geom_line()+
      geom_point(size = 1.3, fill = "white", stroke = 0.2, aes(text= paste(framey, ":",water_mm)))+
      labs(x = "", y = "Q (mm)")+
      scale_color_manual(values = color_scale)+
      scale_fill_manual(values = color_scale)
      
    
    if(log == "log"){
      streamflow <- streamflow + scale_y_continuous(trans='log2')+
        labs(x = "", y = "log(Q (mm))")
    }
    
    #Precipitation Plot
    precipitation <- ggplot()+ 
      my_theme + 
      geom_bar(data=data_precip, aes(x = as.POSIXct(date), y = water_mm, fill = ws, text= paste(framey, ":", water_mm)), 
               stat = "identity", position = "dodge", key = date)+ 
      scale_y_reverse() + labs(x = "", y = "P (mm)")+
      scale_fill_manual(values = color_scale)
    
    
    streamflow <- ggplotly(streamflow, showlegend = FALSE, tooltip = "text")
    precipitation <- ggplotly(precipitation, tooltip = "text")
    
    #Subplot (Join Precip and Streamflow)
    plot <- subplot(precipitation, streamflow, nrows = 2, shareX = TRUE, heights = c(0.5, 0.5), titleY = TRUE)%>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)%>% 
      layout(hovermode = "x") %>%
      layout(dragmode = "select")

  }
  
  
  #### ---------  GGPLOT TIME FUNCTION-----------------------
  ggplot_time_function <- function(data, x, y, log, y_label, animate, speed, trace, color, line_color_scale, fill_color_scale, granularity = "year"){
    
    if(animate == "Animate"){
      if(trace == "Leave Trace"){
        plot <- ggplot(data=data, aes_string(x = x, y = y, frame = "frame", color = color))}

      else{
        plot <- ggplot(data=data, aes_string(x = x, y = y, frame = "framey", color = color))}
      }
    
    else{
      plot <- ggplot(data=data, aes_string(x = x, y = y, color = color))
    } 
    
    plot <- plot +
      my_theme + 
      scale_shape_manual(values = c(21, 22, 23, 24, 25)) + 
      scale_color_manual(values = line_color_scale) +
      scale_fill_manual(values = fill_color_scale)+
      scale_alpha_discrete(range = c(0.9, 0.5))+
      labs(x = "", y = y_label)
    
    if(color == "ws"){
      plot <- plot + 
        geom_line(size = 0.5, aes_string(shape = "solute", group = "source")) + 
        geom_point(size = 1.3, stroke = 0.2, aes(shape = get("solute"),fill = get("fill_color_wsmd"), 
                                                 text = paste(solute,":", round(get(y), 4))))
    }
    else{
      plot <- plot + 
        geom_line(size = 0.5, aes_string(fill = "source", shape = "ws", group = "solute")) + 
        geom_point(size = 1.3, stroke = 0.2, aes(shape = get("ws"), fill = get("fill_color_solutemd"), text = paste(solute,":", round(get(y), 4))))
    }

      
      if(log == "log"){
        plot <- plot + scale_y_continuous(trans='log2')+
        labs(x = "", y = paste("log", "(",y_label, ")"))
      }
      
    if(animate == "Animate"){
      ggplotly(plot, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE) %>%
        layout(hovermode = "x") %>% 
        layout(dragmode = "select") %>% 
        animation_opts(frame = speed, transition = 0, redraw = FALSE) %>%
        animation_slider(currentvalue = list(prefix = "Water Year ", font = list(size = 15))) %>%
        animation_button(font = list(size = 12))}
    
    else{
      ggplotly(plot, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE) %>% 
        layout(hovermode = "x")%>%
        layout(dragmode = "select")
    }
  }

  
  #### ---------  GGPLOT CHARGE BALANCE FUNCTION-----------------------
  ggplot_charge_function <- function(data, x, y, log, y_label, color_scale){
    plot <- ggplot(data=data, aes(x = as.POSIXct(get(x)), y = get(y), color = solute, fill = solute, shape = source, alpha = ws))+
      my_theme + 
      geom_area() + 
      facet_grid(ws ~ .)+
      scale_color_manual(values = color_scale) +
      scale_fill_manual(values = color_scale) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      labs(x = "Water Year", y = y_label)
    
    if(log == "log"){
      plot <- plot + scale_y_continuous(trans='log2')+
        labs(x = "Water Year", y = paste("log", "(",y_label, ")"))
    }
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>% 
      layout(hovermode = "x") %>%
      layout(dragmode = "select")
  }
  
  
  #### ---------  GGPLOT BUBBLE FUNCTION-----------------------
  
  ggplot_bubble_function <- function(data, x, y, log_x, log_y, x_label, y_label,
                                     animate, speed, trace, color = NA, color_scale = NA, size = NA, size_range, text = NA, shape = NA){
    
    if(animate == "Animate"){
      if(trace == "Leave Trace"){
        plot <- ggplot(data=data, aes(frame = frame, text = paste(frame, ": (", round(get(x)), ", ",round(get(y)), ")", sep = ""))) + my_theme +
          scale_size(range = size_range)}
      
      else{
        plot <- ggplot(data=data, aes(frame = framey, text = paste(framey, ": (", round(get(x)), ", ", round(get(y)), ")", sep = ""))) + my_theme+
          scale_size(range = size_range)}}
    
    else{
      plot <- ggplot(data=data, aes(text = paste(framey, ": (", round(get(x)), ", ",round(get(y)), ")", sep = ""))) + my_theme +
        scale_size(range = size_range)}

    if(is.na(color)){
    plot <- plot + geom_point(aes_string(x = x, y = y, size = size), stroke= 0.2, alpha = 0.8)}
    
    else{
    plot <- plot + geom_point(aes_string(x = x, y = y, size = size, color = color, fill = color, shape = (c("ws", "solute")[c("ws", "solute") != color])), stroke= 0.2, alpha = 0.8)}
    
    if(color == input$colormode_global){
      plot <- plot + 
        scale_color_manual(values = color_scale)+
        scale_fill_manual(values = color_scale)
    }
    else{
      plot <- plot+ 
        scale_color_gradient()
    }
    
    if(log_x == "log"){
      plot <- plot + scale_x_continuous(trans='log2')
    }
    
    if(log_y == "log"){
      plot <- plot + scale_y_continuous(trans='log2')
    }
    
    plot <- plot + 
      scale_alpha_discrete(range = c(0.9, 0.5))+
      scale_shape_manual(values = c(21, 22, 23, 24, 25)) + 
      labs(x= x_label, y = y_label)
      
    if(animate == "Animate"){
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>%
      layout(hovermode = "closest") %>% 
      animation_opts(frame = speed, transition = 0, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Water Year ", font = list(size = 15))) %>%
      animation_button(font = list(size = 12))}
    else{
      ggplotly(plot, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE) %>% 
        layout(hovermode = "closest")
    }
  }
  
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  output$plot_pq <- renderPlotly({
    d <- event_data("plotly_selected")
    theplot <- basic_ggplot_function(reactive_data_pq(), x, y, 
                                     log = input$log_pq, coloring_pq()) 
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, legend = list(orientation = 'h', x = 0, y = 1.2))
    if(!is.null(d$x)){
      theplot %>%
        layout(xaxis = list(range = c(min(d$x) - 4000000, max(d$x) + 4000000)))} 
    else{theplot} 
    
    })
  
  output$plot_time <- renderPlotly({
    d <- event_data("plotly_selected")
    theplot <- ggplot_time_function(reactive_data_time(), x_time(), y_time(),
                                    log = input$log_time, y_labs(), input$animate_time, animation_speed_time(), 
                                    input$trace_time, input$colormode_global, line_coloring(), fill_coloring(), 
                                    input$granularity_time)
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, legend = list(orientation = 'h', x = 0, y = 1.2))
    if(!is.null(d$x)){
      theplot %>%
        layout(xaxis = list(range = c(min(d$x)- 4000000, max(d$x)+ 4000000)))} 
        #must convert from POSIXct (s) to JavaScript equivalent (ms) 
    else{theplot}
  }) 
  
  output$plot_charge <- renderPlotly({
    d <- event_data("plotly_selected")
    theplot <- ggplot_charge_function(reactive_data_charge(), x_time(), "charge_ueq",
                                    log = input$log_time, "uEquivalent/L", solute_palette)
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, legend = list(orientation = 'h', x = 0, y = 1.2))
    
    if(!is.null(d$x)){
      theplot %>%
        layout(xaxis = list(range = c(min(d$x)- 4000000, max(d$x)+ 4000000)))} 
    #must convert from POSIXct (s) to JavaScript equivalent (ms) 
    else{theplot}
  
  })
  
  
  
  output$plot_cq <- renderPlotly({
    theplot <- ggplot_bubble_function(reactive_data_cq(), "water_mm", input$units, 
                                      input$log_cq_x,input$log_cq_y, "mm", y_labs(), input$animate_cq, animation_speed_cq(), 
                                      input$trace_cq, input$colormode_global, line_coloring(), 1, c(1, 2), shape = "ws")
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot 
  })
  
  
  
  
  output$plot_flux <- renderPlotly({
    d <- event_data("plotly_selected")
    theplot <- ggplot_time_function(reactive_data_flux(), x_flux(), "flux", 
                                    log = input$log_flux, "Equivalent / Hectare", input$animate_flux, animation_speed_flux(), input$trace_flux, 
                                    input$colormode_global, line_coloring(), fill_coloring())
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE)
    
    if(!is.null(d$x)){
      theplot %>%
        layout(xaxis = list(range = c(min(d$x)- 4000000, max(d$x)+ 4000000)))} 
    #must convert from POSIXct (s) to JavaScript equivalent (ms) 
    else{theplot}
  })
  

  output$bubblePlot <- renderPlotly({
    input$go_bubble
    input$lastkeypresscode
    theplot <- isolate(ggplot_bubble_function(reactive_data_bubble(), "temporary_x", "temporary_y", 
                                      input$log_bubble_x, input$log_bubble_y, "", "",
                                      input$animate_bubble, animation_speed_bubble(), 
                                      input$trace_bubble, "framey", NA, input$sizing_bubble, sizing()))
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE)
    
  })

  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else{d}
  })
  
})
