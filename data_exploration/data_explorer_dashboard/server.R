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

 
  #######################################################################################
  ############## THEME  ##################################################################
  my_theme <- 
    theme(rect = element_rect(fill = NA),
          panel.background = element_rect("transparent", colour = NA),
          panel.grid.major = element_line(colour = "#dddddd"), 
          panel.grid.major.x = element_line(colour = NA),
          text = element_text(family = "Helvetica", size = 12), 
          legend.position="none", legend.direction = "horizontal", legend.title = element_blank(),
          axis.title = element_text(size = 10, margin = unit(c(3, 3, 3, 3), "cm")),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          strip.background = element_rect(colour = NA, fill = NA),
          strip.text = element_text(hjust = 1, size = 10, face = "bold", lineheight = 20))
  
  
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
  
 
  ############################################################################################## 
  ###  SIDEBAR LISTS  ##########################################################################
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

  # Changing legend depending on the color mode.
  # If colormode is solutes, the checkboxes for solutes will have solute colors if selected 
  # If colormode is ws, the checkboxes for solutes will be grey if selected
  
  observe({
    if(input$colormode_global == "ws"){
      addClass("solutes_col", "solutes_wsmd")
      removeClass("solutes_col", "solutes_solutesmd")}
    else{
      removeClass("solutes_col", "solutes_wsmd")
      addClass("solutes_col", "solutes_solutesmd")
    }
  })
  

  ###########################################################################################
  ############ FUNCTIONS ####################################################################
  
  ## This function is used to duplicate rows and allow for cummulative animations
  ## See https://plot.ly/r/cumulative-animations/
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  
  ## These functions are used to allow for formulas as input in the exploratory bubble graph
  ## See https://stackoverflow.com/questions/22908050/formula-evaluation-with-mutate
  
  # creates column with name temporary_x
  formula_function_x <- function(df, s){
    q = quote(mutate(df, temporary_x = s))
    eval(parse(text=sub("s", s, deparse(q))))}
  
  # creates column with name temporary_y
  formula_function_y <- function(df, s){
    q = quote(mutate(df, temporary_y = s))
    eval(parse(text=sub("s", s, deparse(q))))}

  ############ END OF FUNCTIONS ##############################################################
  

  
  
  ############################################################################################
  ########### GENERAL SIDEBAR REACTIVITY ######################################################
  
  # Global input reactivity. 
  # The following reactive functions allow the user to modify 
  # settings for all plots in the dashboard from the general settings tab. 
  
  #Global granularity
  observeEvent(input$granularity_global, {
    choices <- input$granularity_global
    updateSelectInput(session, "granularity_pq", selected = choices)
    updateSelectInput(session, "granularity_time", selected = choices)
    updateSelectInput(session, "granularity_cq", selected = choices)
    updateSelectInput(session, "granularity_flux", selected = choices)
  })
  
  #Global log transformation
  observeEvent(input$log_global_y, {
    choices <- input$log_global_y
    updateSelectInput(session, "log_pq", selected = choices)
    updateSelectInput(session, "log_time", selected = choices)
    updateSelectInput(session, "log_cq_x", selected = choices)
    updateSelectInput(session, "log_flux", selected = choices)
  })
  
  #Global animation
  observeEvent(input$animate_global, {
    value <- input$animate_global
    updateSelectInput(session, "animate_cq", selected = value)
    updateSelectInput(session, "animate_time", selected = value)
    updateSelectInput(session, "animate_flux", selected = value)
  })

  #Global animation speed
  observeEvent(input$animation_speed_global, {
    value <- input$animation_speed_global 
    updateSelectInput(session, "animation_speed_cq", selected = value)
    updateSelectInput(session, "animation_speed_time", selected = value)
    updateSelectInput(session, "animation_speed_flux", selected = value)
  })
  
  #Global animation trace
  observeEvent(input$trace_global, {
    value <- input$trace_global
    updateSelectInput(session, "trace_cq", selected = value)
    updateSelectInput(session, "trace_time", selected = value)
    updateSelectInput(session, "trace_flux", selected = value)
  })
  
  # The following reactivity prevents the user from selecting more than 5 ws when viewing the
  # data in solute mode. This is because there are only 5 shapes that the program can assign to differentiate ws. 

  observeEvent(input$colormode_global, {
    if(input$colormode_global == "ws") {
      updateSelectizeInput(session, "watersheds", options = list(maxItems = 9))
    }
    else{
      updateSelectizeInput(session, "watersheds", options = list(maxItems = 5))
    }
  })
  
  # The following reactivity prevents the user from selecting more than 5 solutes when viewing the
  # data in ws mode. This is because there are only 5 shapes that the program can assign to differentiate solutes. 
  observe({
    if(input$colormode_global == "ws" & length(input$solutes_cations) + length(input$solutes_anions) + length(input$solutes_H)> 5){
      updateCheckboxGroupInput(session, "solutes_cations", selected=head(input$solutes_cations,length(input$solutes_cations)))
      updateCheckboxGroupInput(session, "solutes_anions", selected= head(input$solutes_anions,5-length(input$solutes_cations)))
      updateCheckboxGroupInput(session, "solutes_H", selected= head(input$solutes_H,5-length(input$solutes_cations)-length(input$solutes_anions)))}
      })
  
  
  ###  The following function allow 'select all' interactivity 
  
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
  
  solutes <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
  
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################

  
  
  
  ###########################################################################################
  ############## DATA IMPORT ################################################################
  
  load("precip_streamflow_dfs.RData")
  
  # LONG DATA
  imported_data <- precip_streamflow_long
  # Add fill_color_solutemd and fill_color_wsmd columns so that fill color 
  # can depend on both source and solute and source and ws. 
  imported_data %<>% 
    mutate(fill_color_solutemd = paste(source, solute, sep = '_')) %>% 
    mutate(fill_color_wsmd = paste(source, ws, sep = '_'))
  
  # WIDE DATA
  imported_data_wide<- precip_streamflow_wide
  
  # SUPER WIDE DATA
  imported_data_super_wide<- precip_streamflow_super_wide
  
  
  
  ###########################################################################################
  ############ REACTIVE FUNCTIONS AND REACTIVE DATA FOR EACH PLOT ###########################
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Reactive Time Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  #Reactive data filtering for time plot. Even reactive to update (go_exploratory) button
  # Even reactive so that the app is not as slow.
  
  eventReactive_data_time <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    data$date<- as.POSIXct(data$date) + (24*60*60) #Add one day because Posix counts January 1 as DOY 0
    data$water_year<- as.POSIXct(data$water_year) + (24*60*60) #Add one day because Posix counts January 1 as DOY 0
    data <- data[data$source %in% input$water_sources,]
    data <- data[data$solute %in% solutes(),] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
  })
  
  # Data is reactive to granularity and animation/trace, not event reactive to those
  # This separation is done so that people can change granularity when zoomed in 
  # without replotting and zooming out.
  
  reactive_data_time <- reactive({
    data <- eventReactive_data_time()[eventReactive_data_time()$granularity %in% input$granularity_time,]
    if(input$animate_time == "Animate" & input$trace_time == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}
  })
  
  ## X axis for Time Plot reactive to granularity. 
  #This is done because We want o plot water_year when it's yearly data 
  # e.g. have Jan. 2013 as part of 2012 water year 
  
  x_time <- reactive({
    if(input$granularity_time == "week"){"date"}
    else if(input$granularity_time == "month"){"date"}
    else if(input$granularity_time == "year"){"water_year"}
  })
  
  ## Y axis for Flux Plot reactive input where the person can choose what kind 
  # of graph they want to look at, either concentration, or  
  # temperature, or pH etc. 
  
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
  
  
  #Coloring for Time Plot
  #Line coloring
  line_coloring <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    if(input$yaxis_time %in% c("concentration", "chargebalance") & input$colormode_global == "solute"){solute_palette}
    else if(input$colormode_global == "ws"){ws_palette}
    else{grey_palette}    
  })
  
  #Line coloring
  fill_coloring <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
        if(input$yaxis_time %in% c("concentration", "chargebalance") & input$colormode_global == "solute"){source_color_solutemd}
        else if(input$colormode_global == "ws"){source_color_wsmd}
        else{grey_palette_2}   
      })
  
  # Change granularity to week if user is looking at temperature, anc, or spcond
  # Since these only exist in weekly format. 
  observeEvent({input$go_exploratory |
      input$lastkeypresscode},{if(!(input$yaxis_time %in% c("concentration", "pH", "chargebalance"))){
    updateSelectInput(session, "granularity_time",
                      selected = "week")}})
  
  
  # Change water sources to streamflow only if user is looking at charge balance,
  # temperature, anc, or spcond since these only exist for streamflow
  
  observe({if(!(input$yaxis_time %in% c("concentration", "pH"))){
    updateSelectInput(session, "water_sources",
                      selected = "streamflow")}
    else{
    updateSelectInput(session, "water_sources",
                      selected = c("streamflow", "precipitation"))
    }})
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> End of Reactive Time Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Reactive Charge Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  #Reactive data filtering for charge plot. Even reactive to update (go_exploratory) button
  # Even reactive so that the app is not as slow.
  
  eventReactive_data_charge <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    data <- data[data$source %in% "precipitation",] #Charge plot is only done for precip data. 
    data <- data[data$solute %in% solutes(),] 
    data <- data[data$ws %in% input$watersheds,]
    data %<>% 
      mutate(charge_ueq = ifelse(solute %in% solutes_anions, -1*(concentration_ueq), concentration_ueq))
  
  })
  
  # Data is reactie to granularity, not event reactive to granularity 
  # This separation is done so that people can change granularity when zoomed in 
  # without replotting and zooming out. 
  
  reactive_data_charge <- reactive({
    data <- eventReactive_data_charge()[eventReactive_data_charge()$granularity %in% input$granularity_time,]
  })
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> End of Reactive Charge Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Reactive PQ Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  #Reactive data filtering for PQ plot. Even reactive to update (go_exploratory) button
  # Even reactive so that the app is not as slow.
  
  eventReactive_data_pq <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    data$date<- as.POSIXct(data$date)+ (24*60*60) #Add one day because Posix counts January 1 as DOY 0
    data$water_year<- as.POSIXct(data$water_year)+ (24*60*60) #Add one day because Posix counts January 1 as DOY 0
    data <- data[data$solute %in% "Ca",] #filter so that they only appear once. Df structure has same P and Q data for all solutes.  
    data <- data[data$ws %in% input$watersheds,]
  })
  
  # Data is reactie to granularity, not event reactive to granularity 
  # This separation is done so that people can change granularity when zoomed in 
  # without replotting and zooming out. 
  
  reactive_data_pq <- reactive({
    data <- eventReactive_data_pq()[eventReactive_data_pq()$granularity %in% input$granularity_pq,]
  })
  
  ## X axis for PQ Plot
  x_pq <- reactive({
    if(input$granularity_pq == "week"){"date"}
    else if(input$granularity_pq == "month"){"date"}
    else if(input$granularity_pq == "year"){"water_year"}
  })
  
  # Reactive coloring for pQ plot depending on global colormode. 
  # Data should be grey scale if looking at solute mode, but colored by 
  # watershed if looking at ws mode. 
  
  coloring_pq <- reactive({
    if(input$colormode_global == "solute"){grey_palette}
    else{ws_palette}
  })
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> End of Reactive PQ Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Reactive cQ Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  #Reactive data filtering for cQ plot. Even reactive to update (go_exploratory) button
  # Even reactive so that the app is not as slow.
  
  eventReactive_data_cq <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data <- imported_data
    data <- data[data$solute %in% solutes(),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds,]
    data <- data[data$source %in% "streamflow",]
  })
  
  # Data is reactive to granularity and animation/trace, not event reactive to these
  # This separation is done so that people can change granularity when zoomed in 
  # without replotting and zooming out. Also, so that animation happens without having to replot. 
  # In cQ plot, the data also has to be reactively updated 
  # to match the date range selected when brushing in other plots. 

  reactive_data_cq <- reactive({
    d <- event_data("plotly_selected")
    data <- eventReactive_data_cq()
    if (!is.null(d$x)){
      data <- data[data$granularity %in% input$granularity_cq,]
      data <- data[as.numeric(as.POSIXct(data$date)) >= min(d$x) & as.numeric(as.POSIXct(data$date)) <= max(d$x),]
      }
    else{
      data <- data[data$granularity %in% input$granularity_cq,]
    }
    if(input$animate_cq == "Animate" & input$trace_cq == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}
  })
  
  #Animation Speed cQ Plot
  animation_speed_cq <- reactive({
    (80)*(1/(input$animation_speed_cq))^2
  })

  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> End of Reactive cQ Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Reactive Flux Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  #Reactive data filtering for flux plot. Even reactive to update (go_exploratory) button
  # Even reactive so that the app is not as slow. 
  
  eventReactive_data_flux <- eventReactive({input$go_exploratory |
      input$lastkeypresscode},{
    data<- imported_data
    data$date<- as.POSIXct(data$date) + (24*60*60) #Add one day because Posix counts January 1 as DOY 0 
                                                  # See for more info: http://www.javacms.tech/questions/731962/r-as-posixctsys-date-returns-date-a-day-early
    data$water_year<- as.POSIXct(data$water_year) + (24*60*60) #Add one day because Posix counts January 1 as DOY 0
    #data <- data[data$granularity %in% input$granularity_flux,]
    data <- data[data$source %in% input$water_sources,]
    data <- data[data$solute %in% solutes(),] #filter so that they only appear once. 
    data <- data[data$ws %in% input$watersheds,]
    
    if(input$animate_flux == "Animate" & input$trace_flux == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}

  })
  
  # Data is reactie to granularity, not event reactive to granularity 
  #This separation is done so that people can change granularity when zoomed in 
  #without replotting and zooming out. 
  
  reactive_data_flux <- reactive({
    data <- eventReactive_data_flux()[eventReactive_data_flux()$granularity %in% input$granularity_flux,]
  })
  
  ## X axis for Flux Plot reactive to granularity. 
  #This is done because We want o plot water_year when it's yearly data 
  # e.g. have Jan. 2013 as part of 2012 water year 
  
  x_flux <- reactive({
    if(input$granularity_flux == "week"){"date"}
    else if(input$granularity_flux == "month"){"date"}
    else if(input$granularity_flux == "year"){"water_year"}
  })
  
  #Animation Speed for Flux
  animation_speed_flux <- reactive({
    (80)*(1/(input$animation_speed_flux))^2
  })
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> End of Reactive Flux Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Reactive Bubble Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  
  ## solutesx_formula and solutesy_formula parse the formula inputed into the 
  #by the user in the bubble plot to match column names
  
  solutesx_formula <- reactive({
    capitalized <-"(^[[:upper:]][[:alpha:]])|^H|^K"
    strip_spaces <- gsub(" ", "", input$solutesx_formula, fixed = TRUE)
    include_space <- gsub("([^[:alnum:]])", " \\1 ", strip_spaces)
    trim <- str_trim(include_space)
    split_each <- str_split(trim, " ")
    include_units <- sapply(split_each, function(x) ifelse(str_detect(x, capitalized), paste(paste(input$units_bubble, "_", sep=""), x , sep=""), x))
    p_q_replace <- sapply(include_units, function(x) gsub("^[P|Q]", "water_mm", x, fixed = FALSE))
    add_source <- sapply(p_q_replace, function(x) ifelse(str_detect(x, "date"), x, (ifelse(str_detect(x, "[[:alpha:]]"), paste(x, paste("_", input$solutesx_source, sep=""), sep = ""), x))))    
    formula <- paste(add_source, collapse=' ' )
    formula
  })
  
  solutesy_formula <- reactive({
    capitalized<-"^[[:upper:]][[:alpha:]]|^H|^K"
    strip_spaces <- gsub(" ", "", input$solutesy_formula, fixed = TRUE)
    include_space <- gsub("([^[:alnum:]])", " \\1 ", strip_spaces)
    trim <- str_trim(include_space)
    split_each <- str_split(trim, " ")
    include_units <- sapply(split_each, function(x) ifelse(str_detect(x, capitalized), paste(paste(input$units_bubble, "_", sep=""), x, sep=""), x))
    p_q_replace <- sapply(include_units, function(x) gsub("^[P|Q]", "water_mm", x, fixed = FALSE))
    add_source <- sapply(p_q_replace, function(x) ifelse(str_detect(x, "[[:alpha::]]"), paste(x, paste("_", input$solutesy_source, sep=""), sep=""), x))    
    formula <- paste(add_source, collapse=' ' )
    formula
  })
  
  
  #Reactive data filtering according to inputs for bubble plot
  
  reactive_data_bubble <- eventReactive(input$go_bubble, {
    data <- imported_data_super_wide
    data$framey <- as.numeric(data$framey)
    data <- data[data$granularity %in% input$granularity_bubble,]
    data <- data[data$ws %in% input$watersheds_bubble,]
    data <- data[data$date >= input$date_range_bubble[1] & data$date <= input$date_range_bubble[2]]
    data <- formula_function_x(data, solutesx_formula())
    data <- formula_function_y(data, solutesy_formula())
    
    if(input$animate_bubble == "Animate" & input$trace_bubble == "Leave Trace"){data <- accumulate_by(data, ~framey)}
    else{data}
    
  })

  # Animation Speed for Bubble Plot
  animation_speed_bubble <- reactive({
    (80)*(1/(input$animation_speed_bubble))^2
  })
  
  # Sizing for Bubble plot
  sizing <- reactive({
    if(input$sizing_bubble == 1){c(2,2)}
    else{c(1,6)}
  })
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> End of Reactive Bubble Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  ########### END OF REACTIVE DATA FOR EACH PLOT #########################################
  
  
  
  
  ######################################################################################################
  ########### PLOTTING  FUNCTIONS ###########################################################################
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> BASIC PQ PLOT FUNCTION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  basic_ggplot_function <- function(data, x, y, log, color_scale, date_breaks, date_labels){
    
    data_stream <- data[data$source %in% c("streamflow"),]
    data_precip <- data[data$source %in% c("precipitation"),]
    
    
    #Streamflow Plot
    streamflow <- ggplot(data=data_stream, aes(x = get(x), y = water_mm, color = ws)) + 
      my_theme +
      geom_line()+
      geom_point(size = 1.3, fill = "white", stroke = 0.2, 
                 aes(text= {
                   if(input$granularity_pq == "year"){
                     paste(ws, " • ", "(",strftime(get(x), "%Y"), ", ",water_mm, ")", sep="")}
                   else if(input$granularity_pq == "month"){
                     paste(ws, " • ", "(",strftime(get(x), "%Y-%b"), ", ",water_mm, ")", sep="")}
                   else{
                     paste(ws, " • ", "(",strftime(get(x), "%Y-%b-%d"), ", ",water_mm, ")", sep="")}}))+
      labs(x = "", y = "Q (mm)")+
      scale_color_manual(values = color_scale)+
      scale_fill_manual(values = color_scale)+
      scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
      
    
    if(log == "log"){
      streamflow <- streamflow + scale_y_continuous(trans='log2')+
        labs(x = "", y = "log(Q (mm))")
    }
    
    #Precipitation Plot
    precipitation <- ggplot()+ 
      my_theme + 
      geom_bar(data=data_precip, aes(x = get(x), y = water_mm, fill = ws, 
                                     text={
                                       if(input$granularity_pq == "year"){paste(ws, " • ", "(",strftime(get(x), "%Y"), ", ",water_mm, ")", sep="")}
                                       else if(input$granularity_pq == "month"){paste(ws, " • ", "(",strftime(get(x), "%Y-%b"), ", ",water_mm, ")", sep="")}
                                       else{paste(ws, " • ", "(",strftime(get(x), "%Y-%b-%d"), ", ",water_mm, ")", sep="")}}), 
               stat = "identity", position = "dodge", key = date)+ 
      scale_y_reverse() + labs(x = "", y = "P (mm)")+
      scale_fill_manual(values = color_scale)+
      scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
    
    
    streamflow <- ggplotly(streamflow, showlegend = FALSE, tooltip = "text")
    precipitation <- ggplotly(precipitation, tooltip = "text")
    
    #Subplot (Join Precip and Streamflow)
    plot <- subplot(precipitation, streamflow, nrows = 2, shareX = TRUE, heights = c(0.5, 0.5), titleY = TRUE)%>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)%>% 
      layout(hovermode = "x") %>%
      layout(dragmode = "select")

  }
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> BASIC TIME PLOT FUNCTION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  ggplot_time_function <- function(data, x, y, 
                                   log, y_label, 
                                   animate, speed, trace, 
                                   colormode, line_color_scale, fill_color_scale,
                                   date_breaks = "5 years", date_labels = "%Y", granularity){
    
    # Animation 
    # options where frame is the column with the year and framey is a column with the data point's year. 
    # And frame is a column with the year after accumulating -- so each data point is duplicated various times, once
    # for every year after they have appeared in the graph. 
    
    if(animate == "Animate"){
      plot <- ggplot(data=data, aes_string(x = x, y = y, 
                                           frame = ifelse(trace == "Leave Trace","frame", "framey"), color = colormode))}

    else{
      plot <- ggplot(data=data, aes_string(x = x, y = y, color = colormode))
    } 
    
    # Color:
    # If colormode is watershed, the line and point should be grouped according to source and solute
    # Shape of the point reflects solute
    # Line color reflects solute
    # Fill color reflects both source. If precipitation, then white, 
    # if stream water then the same color as the line
    # Text in the tooltip also changes
    
    
    if(colormode == "ws"){
      plot <- plot + 
        geom_line(size = 0.5, aes_string(shape = "solute", group = "source")) + 
        geom_point(size = 1.3, stroke = 0.2, aes(shape = get("solute"), fill = get("fill_color_wsmd"), group = "source",
                                                 text={
                                                   if(granularity == "year"){paste(ws, ", ", solute, " • ", "(", strftime(get(x), "%Y"), ", ", round(get(y),2), ")", sep="")}
                                                   else if(granularity == "month"){paste(ws, " • ", strftime(get(x), "%Y-%b"), " • ", solute, " • ", round(get(y),2), sep="")}
                                                   else{paste(ws, " • ", strftime(get(x), "%Y-%b-%d"), " • ", solute, " • ", round(get(y),2), sep="")}}))
    }
    else{
      plot <- plot + 
        geom_line(size = 0.5, aes_string(shape = "ws", fill = "solute", group = "source")) + 
        geom_point(size = 1.3, stroke = 0.2, aes(shape = get("ws"), fill = get("fill_color_solutemd"), group = "source",
                                                 text={
                                                   if(granularity == "year"){paste(ws, ", ", solute, " • ", "(", strftime(get(x), "%Y"), ", ", round(get(y),2), ")", sep="")}
                                                   else if(granularity == "month"){paste(ws, ", ", solute, " • ", "(", strftime(get(x), "%Y-%b"), ", ", round(get(y),2), ")", sep="")}
                                                   else{paste(ws, ", ", solute, " • ", "(", strftime(get(x), "%Y-%b-%d"), ", ", round(get(y),2), ")", sep="")}}))
       
    }

    
    plot <- plot +
      my_theme + 
      scale_shape_manual(values = c(21, 22, 23, 24, 25)) + 
      scale_color_manual(values = line_color_scale) +
      scale_fill_manual(values = fill_color_scale)+
      scale_alpha_discrete(range = c(0.9, 0.5))+
      labs(x = "", y = y_label)+
      scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
    
    
    # y axis transformation 
    
    if(log == "log"){
      plot <- plot + scale_y_continuous(trans='log2')+
        labs(x = "", y = paste("log", "(",y_label, ")"))
    }
    
    
    # ggplotly
      
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
        layout(hovermode = "closest")%>%
        layout(dragmode = "select")
    }
  }

  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> CHARGE BALANCE PLOT FUNCTION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  ggplot_charge_function <- function(data, x, y, log, y_label, color_scale){
    plot <- ggplot(data=data, aes(x = as.POSIXct(get(x)), y = get(y), color = solute, fill = solute, shape = source)) +
      my_theme + 
      geom_area() + 
      facet_grid(ws ~ .)+
      scale_color_manual(values = color_scale) +
      scale_fill_manual(values = color_scale) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      labs(x = "", y = y_label)
    
    # y axis transformation 
    
    if(log == "log"){
      plot <- plot + scale_y_continuous(trans='log2')+
        labs(x = "Water Year", y = paste("log", "(",y_label, ")"))
    }
    
    
    #ggplotly
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>% 
      layout(hovermode = "x") %>%
      layout(dragmode = "select")
  }
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> CHARGE BUBBLE PLOT FUNCTION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  ggplot_bubble_function <- function(data, x, y, 
                                     log_x, log_y, x_label, y_label,
                                     animate, speed, trace, 
                                     color = NA, color_scale = NA, 
                                     size = NA, size_range, 
                                     text = NA, cq = "no", granularity){
  
    # Animation 
    # options where frame is the column with the year and framey is a column with the data point's year. 
    # And frame is a column with the year after accumulating -- so each data point is duplicated various times, once
    # for every year after they have appeared in the graph. 
    
    if(animate == "Animate"){
      if(trace == "Leave Trace"){
        plot <- ggplot(data=data, aes(frame = frame, alpha = ws, text = paste(frame, ": (", round(get(x)), ", ",round(get(y)), ")", sep = "")))}
      
      else{
        plot <- ggplot(data=data, aes(frame = framey, alpha = ws, text = paste(framey, ": (", round(get(x)), ", ", round(get(y)), ")", sep = "")))}}
    
    else{
      plot <- ggplot(data=data, aes(alpha = ws, text = paste(framey, ": (", round(get(x)), ", ",round(get(y)), ")", sep = "")))}
    
    
    #Some additional options for bubble graph if it is the cq plot
    
    if(cq == "yes"){
      plot <- plot + 
        geom_point(aes_string(x = x, y = y, size = size, color = color, fill = color, shape = ifelse(color == "ws","solute", "ws")), stroke= 0.2, alpha = 0.8)+
        scale_color_manual(values = color_scale) +
        scale_fill_manual(values = color_scale)
    }
   
    else{
      plot <- plot+ 
        geom_point(aes_string(x = x, y = y, size = size, color = color, fill = color, shape = NA), stroke= 0.2, alpha = 0.8)
        scale_colour_brewer()
    }
    
    
    # x and y axis transformations
    if(log_x == "log"){
      plot <- plot + scale_x_continuous(trans='log2')
    }
    
    if(log_y == "log"){
      plot <- plot + scale_y_continuous(trans='log2')
    }
    
    plot <- plot + my_theme + 
      scale_size(range = size_range)+
      scale_alpha_discrete(range = c(0.9, 0.5))+
      scale_shape_manual(values = c(21, 22, 23, 24, 25)) + 
      labs(x= x_label, y = y_label)
      
    #ggplotly
    
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
  
  
  
  ###############################################################################################
  ########### OUTPUTS ###########################################################################
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> PQ <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  output$plot_pq <- renderPlotly({
    d <- event_data("plotly_selected")
    # Make x axis reactive to the user's selection and to granularity
    if(is.null(d)){
      breaks<- "5 years"
      ldates<- "%Y"}
    else if(input$granularity_pq== "year"){
      breaks<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"5 years","1 year")
      ldates<-ifelse(max(d$x) - min(d$x) > 1*(31557600),"%Y","%Y")}
    else{
      breaks<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"5 years","4 months")
      ldates<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"%Y","%b%y")
    }
    
    # Plot
    theplot <- basic_ggplot_function(reactive_data_pq(), x_pq(), y, 
                                     log = input$log_pq, coloring_pq(), breaks, ldates) 
    
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, legend = list(orientation = 'h'))
    if(!is.null(d$x)){
      theplot %>%
        layout(xaxis = list(range = c(min(d$x) - 4000000, max(d$x) + 4000000)))} 
    else{theplot} 
    
    })
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Time Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  output$plot_time <- renderPlotly({
    d <- event_data("plotly_selected")
    
    # Make x axis reactive to the user's selection and to granularity
    if(is.null(d)){
    breaks<- "5 years"
    ldates<- "%Y"}
    else if(input$granularity_time == "year"){
      breaks<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"5 years","1 year")
      ldates<-ifelse(max(d$x) - min(d$x) > 1*(31557600),"%Y","%Y")}
    else{
      breaks<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"5 years","4 months")
      ldates<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"%Y","%b%y")
    }
    
    # Plot 
    theplot <- ggplot_time_function(reactive_data_time(), x_time(), y_time(),
                                    log = input$log_time, y_labs(), 
                                    input$animate_time, animation_speed_time(), input$trace_time, 
                                    input$colormode_global, line_coloring(), fill_coloring(), 
                                    breaks, ldates, granularity = input$granularity_time)
    
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
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Charge Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  output$plot_charge <- renderPlotly({
    d <- event_data("plotly_selected")
    # Plot
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
    # Plot
    theplot <- ggplot_bubble_function(reactive_data_cq(), "water_mm", input$units, 
                                      input$log_cq_x,input$log_cq_y, "mm", y_labs(), 
                                      input$animate_cq, animation_speed_cq(), input$trace_cq, 
                                      input$colormode_global, line_coloring(), 1, c(1, 2), cq = "yes")
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot 
  })
  
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Flux Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  output$plot_flux <- renderPlotly({
    d <- event_data("plotly_selected")
    
    # Make x axis reactive to the user's selection and to granularity
    if(is.null(d)){
      breaks<- "5 years"
      ldates<- "'%y"}
    else if(input$granularity_flux == "year"){
      breaks<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"5 years","1 year")
      ldates<-ifelse(max(d$x) - min(d$x) > 1*(31557600),"'%y","'%y")}
    else{
      breaks<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"5 years","4 months")
      ldates<-ifelse(max(d$x) - min(d$x) > 5*(31557600),"'%y","%b%y")
    }
    
    # Plot
    theplot <- ggplot_time_function(reactive_data_flux(), x_flux(), "flux", 
                                    log = input$log_flux, "Equivalent / Hectare", 
                                    input$animate_flux, animation_speed_flux(), input$trace_flux, 
                                    input$colormode_global, line_coloring(), fill_coloring(), 
                                    breaks, ldates, granularity = input$granularity_flux)
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
  
  ###### >>>>>>>>>>>>>>>>>>>>>>>>>>>> Bubble Plot <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #####
  
  output$bubblePlot <- renderPlotly({
    input$go_bubble
    input$lastkeypresscode
    # Plot
    theplot <- isolate(ggplot_bubble_function(reactive_data_bubble(), "temporary_x", "temporary_y", 
                                      input$log_bubble_x, input$log_bubble_y, "", "",
                                      input$animate_bubble, animation_speed_bubble(), input$trace_bubble, 
                                      input$coloring_bubble, NA, 
                                      input$sizing_bubble, sizing()))
    #the code below fixes an issue where the plotly width argument doesn't adjust automatically.
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(autosize = TRUE, height = 500)
    
  })
  
})
