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
library(reshape2)

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
          legend.position = "right", legend.direction = "vertical", legend.title = element_blank(),
          strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
          axis.title= element_text(NULL), axis.title.x= element_blank(), 
          axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
  
  color_cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
  color_anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")
  ws_palette <- c("1" = "#fae550", "2" = "#a8db40", "3" = "#62c74a", "4" = "#408b77", 
                  "5" = "#27517b", "6" = "#303475", "7" = "#351042", "8" = "#79276e", "9" = "#b63462")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("streamflow" = 16, "precipitation"= 21)
  watershed_linetypes <- c("1"= 2,"2"= 1,"3"= 3,"4"= 4,"5"= 5,"6"= 6,"7"= 1,"8"= 1,"9"= 1)
  
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
  
  #load vegetation data
  lai_data<- readRDS("lai_data.rds")
  fine_litter_data <- readRDS("fine_litter_data.rds")
  
  #load pq data
  load("precip_streamflow_dfs.RData")
  imported_data <- precip_streamflow_long
  
  #Cleaning vegetation data######################
  
  #Replacing missing data values with NA
  fine_litter_data[fine_litter_data == -9999] <- NA
  fine_litter_data[fine_litter_data == -9999.9] <- NA
  fine_litter_data[fine_litter_data == -9999.99] <- NA
  
  #Average all count columns by year
  by_year <- fine_litter_data %>% group_by(YEAR, SITE)
  sugarm_count_mean<-by_year %>% summarise(sugarm_count_mean = mean(M_COUNT, na.rm=T))
  redm_count_mean<-by_year %>% summarise(redm_count_mean = mean(f_COUNT, na.rm=T))
  stripedm_count_mean<-by_year %>% summarise(stripedm_count_mean = mean(t_COUNT, na.rm=T))
  ash_count_mean<-by_year %>% summarise(ash_count_mean = mean(Q_COUNT, na.rm=T))
  beech_count_mean<-by_year %>% summarise(beech_count_mean = mean(B_COUNT, na.rm=T))
  whiteb_count_mean<-by_year %>% summarise(whiteb_count_mean = mean(W_COUNT, na.rm=T))
  yellowb_count_mean<-by_year %>% summarise(yellowb_count_mean = mean(Y_COUNT, na.rm=T))
  pcherry_count_mean<-by_year %>% summarise(pcherry_count_mean = mean(P_COUNT, na.rm=T))
  aspen_count_mean<-by_year %>% summarise(aspen_count_mean = mean(a_COUNT, na.rm=T))
  
  #paste a new column called species in each df
  sugarm_count_mean$species <- rep("Sugar Maple", nrow(sugarm_count_mean))
  redm_count_mean$species <- rep("Red Maple", nrow(redm_count_mean))
  stripedm_count_mean$species <- rep("Striped Maple", nrow(stripedm_count_mean))
  ash_count_mean$species <- rep("Ash", nrow(ash_count_mean))
  beech_count_mean$species <- rep("Beech", nrow(beech_count_mean))
  whiteb_count_mean$species <- rep("White Birch", nrow(whiteb_count_mean))
  yellowb_count_mean$species <- rep("Yellow Birch", nrow(yellowb_count_mean))
  pcherry_count_mean$species <- rep("Pin Cherry", nrow(pcherry_count_mean))
  aspen_count_mean$species <- rep("Aspen", nrow(aspen_count_mean))
  
  #rename columns to be just "count" in order to merge into long df rather than wide
  sugarm_count_mean<-plyr::rename(sugarm_count_mean, c("sugarm_count_mean"="count"))
  redm_count_mean<-plyr::rename(redm_count_mean, c("redm_count_mean"="count"))
  stripedm_count_mean<-plyr::rename(stripedm_count_mean, c("stripedm_count_mean"="count"))
  ash_count_mean<-plyr::rename(ash_count_mean, c("ash_count_mean"="count"))
  beech_count_mean<-plyr::rename(beech_count_mean, c("beech_count_mean"="count"))
  whiteb_count_mean<-plyr::rename(whiteb_count_mean, c("whiteb_count_mean"="count"))
  yellowb_count_mean<-plyr::rename(yellowb_count_mean, c("yellowb_count_mean"="count"))
  pcherry_count_mean<-plyr::rename(pcherry_count_mean, c("pcherry_count_mean"="count"))
  aspen_count_mean<-plyr::rename(aspen_count_mean, c("aspen_count_mean"="count"))
  
  #Merge all count column averages into one df with three total columns (year, species, count)
  count_means <- Reduce(function(...) merge(..., all=T), 
                        list(sugarm_count_mean, redm_count_mean, stripedm_count_mean, 
                             ash_count_mean, beech_count_mean, whiteb_count_mean, 
                             yellowb_count_mean, pcherry_count_mean, aspen_count_mean))
  site_names <- c(
    'BB'="Bear Brook Watershed site",
    'TF'="Throughfall site",
    'W1'="Watershed 1",
    'W5'="Watershed 5"
  )

  #Matt normalization of flux####################
  #YEARLY
  ws_cast_year <- imported_data %>%
    filter(granularity=='year') %>% 
    filter(source=='streamflow') %>%
    filter(solute=='NO3') %>%
    dcast(.,date+water_year+solute~ws,value.var='flux')
  
  #normalize data by subtracting ws6 from each
  ws_cast_year$"n2" <- ws_cast_year$"2"-ws_cast_year$"6"
  ws_cast_year$"n4" <- ws_cast_year$"4"-ws_cast_year$"6"
  ws_cast_year$"n5" <- ws_cast_year$"5"-ws_cast_year$"6"
  
  #rename ws columns so I can name normalized columns just numbers to make merging easier
  names(ws_cast_year)[names(ws_cast_year) == "2"] <- "ws2"
  names(ws_cast_year)[names(ws_cast_year) == "4"] <- "ws4"
  names(ws_cast_year)[names(ws_cast_year) == "5"] <- "ws5"
  names(ws_cast_year)[names(ws_cast_year) == "n2"] <- "2"
  names(ws_cast_year)[names(ws_cast_year) == "n4"] <- "4"
  names(ws_cast_year)[names(ws_cast_year) == "n5"] <- "5"
  
  #melt function to get them all back together (new tidyr version is spread)
  ws_cast_year <- melt(ws_cast_year, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
       variable.name = "ws", value.name = "normalized_flux")
  
  #add granularity column with "year"
  ws_cast_year$granularity <- rep("year", nrow(ws_cast_year))
  
  #repeat for month and week
  #MONTHLY
  ws_cast_month <- imported_data %>%
    filter(granularity=='month') %>% 
    filter(source=='streamflow') %>%
    filter(solute=='NO3') %>%
    dcast(.,date+water_year+solute~ws,value.var='flux')
  
  #normalize data by subtracting ws6 from each
  ws_cast_month$"n2" <- ws_cast_month$"2"-ws_cast_month$"6"
  ws_cast_month$"n4" <- ws_cast_month$"4"-ws_cast_month$"6"
  ws_cast_month$"n5" <- ws_cast_month$"5"-ws_cast_month$"6"
  
  #rename ws columns so I can name normalized columns just numbers to make merging easier
  names(ws_cast_month)[names(ws_cast_month) == "2"] <- "ws2"
  names(ws_cast_month)[names(ws_cast_month) == "4"] <- "ws4"
  names(ws_cast_month)[names(ws_cast_month) == "5"] <- "ws5"
  names(ws_cast_month)[names(ws_cast_month) == "n2"] <- "2"
  names(ws_cast_month)[names(ws_cast_month) == "n4"] <- "4"
  names(ws_cast_month)[names(ws_cast_month) == "n5"] <- "5"
  
  #melt function to get them all back together (new tidyr version is spread)
  ws_cast_month <- melt(ws_cast_month, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
                       variable.name = "ws", value.name = "normalized_flux")
  
  #add granularity column with "month"
  ws_cast_month$granularity <- rep("month", nrow(ws_cast_month))
  
  #WEEKLY
  ws_cast_week <- imported_data %>%
    filter(granularity=='week') %>% 
    filter(source=='streamflow') %>%
    filter(solute=='NO3') %>%
    dcast(.,date+water_year+solute~ws,value.var='flux')
  
  #normalize data by subtracting ws6 from each
  ws_cast_week$"n2" <- ws_cast_week$"2"-ws_cast_week$"6"
  ws_cast_week$"n4" <- ws_cast_week$"4"-ws_cast_week$"6"
  ws_cast_week$"n5" <- ws_cast_week$"5"-ws_cast_week$"6"
  
  #rename ws columns so I can name normalized columns just numbers to make merging easier
  names(ws_cast_week)[names(ws_cast_week) == "2"] <- "ws2"
  names(ws_cast_week)[names(ws_cast_week) == "4"] <- "ws4"
  names(ws_cast_week)[names(ws_cast_week) == "5"] <- "ws5"
  names(ws_cast_week)[names(ws_cast_week) == "n2"] <- "2"
  names(ws_cast_week)[names(ws_cast_week) == "n4"] <- "4"
  names(ws_cast_week)[names(ws_cast_week) == "n5"] <- "5"
  
  #melt function to get them all back together (new tidyr version is spread)
  ws_cast_week <- melt(ws_cast_week, id.vars = c("date","water_year","solute"), measure.vars = c("2","4","5"),
                        variable.name = "ws", value.name = "normalized_flux")
  
  #add granularity column with "week"
  ws_cast_week$granularity <- rep("week", nrow(ws_cast_week))
  #####MERGE
  #merge all ws_casts together
  ws_cast <- Reduce(function(...) merge(..., all=T), list(ws_cast_month, ws_cast_year, ws_cast_week))
  data_norm <- merge(imported_data, ws_cast, all = T)
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y #########################################
  
  #Reactive Data for NO3 trends
  reactive_data <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity,]
    data <- data[data$source %in% input$water_sources2,]
    data <- data[data$solute %in% c("NO3"),] 
    #note that solutes is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
  })
  
  
  x <- reactive({
    if(input$granularity == "week"){"water_date"}
    else if(input$granularity == "month"){"water_date"}
    else if(input$granularity == "year"){"water_year"}
  })
  
  y <- reactive({
    if(input$units =="mg/L"){"concentration_mg"}
    else if(input$units =="uEquivalent/L"){"concentration_ueq"}
    else if(input$units =="uMole/L"){"concentration_umol"}
    else if(input$units =="flux"){"flux"}
  })

  #Reactive flux data  
  reactive_data_flux <- reactive({
    data <- imported_data
    data <- data[data$granularity %in% input$granularity3,]
    data <- data[data$source %in% c("streamflow"),]
    data <- data[data$ws %in% c("1", "6"),]
    data <- data[data$solute %in% c("NO3"),] 
  })

  yflux <- reactive({
    {"flux"}
  })
  
    #Reactive normalized flux data
    reactive_data_norm <- reactive({
    data_norm <- data_norm[data_norm$granularity %in% input$granularity3,]
    data_norm <- data_norm[data_norm$source %in% c("streamflow"),]
    data_norm <- data_norm[data_norm$ws %in% c("2", "4", "5"),]
    data_norm <- data_norm[data_norm$solute %in% c("NO3"),]
  })
  
  xflux <- reactive({
    if(input$granularity3 == "week"){"water_date"}
    else if(input$granularity3 == "month"){"water_date"}
    else if(input$granularity3 == "year"){"water_year"}
  })
  
  ynorm <- reactive({
    {"normalized_flux"}
  })

  ########### PLOT FUNCTION #########################################
  
  ## GGPLOT TIME FUNCTION
  ggplot_function <- function(data, x, y, log, units, date_range){
    
    if(log == "log") {
      plot <- ggplot(data=data, aes(x= get(x), y= logb(get(y), base=exp(1)), color= solute, shape= source, linetype= ws))+
        labs(x = "Water Year", y = paste("log", "(",units, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x= get(x), y= get(y), color= solute, shape= source, linetype= ws))+
        labs(x = "Water Year", y = units)}
    
    final <- plot+ my_theme + geom_line(size = 0.5) + 
      geom_point(size = 1.3, fill = "white", stroke = 0.5, 
                 aes( text = paste("Watershed: ", ws, "<br>",
                                   "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(date_range[1]), max(date_range[2]))+ 
      geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = 5, color = "black")+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_linetype_manual(values = watershed_linetypes)

    ggplotly(  
      final, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
  }

  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  #ggplotly that shows most plots increase in lai following the ice storm
  output$lai_plot <- renderPlotly({
    lai_plot <- ggplot(lai_data[lai_data$WS == input$watersheds1,], ####THIS PLOT SHOULD BE DONE WITH fine_litter.txt HBEF DATA!!
                       aes(x = YEAR, y = LAIT, color = ELEVATION_M))+ my_theme+ ###ALSO make more plots from said data
      theme(legend.title = element_text("Plot Elevation", family = "Helvetica"),
            strip.text = element_text(size = 10))+
      geom_point(aes(text = paste("Year: ", YEAR, "<br>", "LAI: ", LAIT)))+
      geom_smooth(method = "lm", se = F, size = 0.5)+
      labs(color="Plot Elevation")+
      xlab(" ")+
      ylab("LAI (meter-squared per meter-squared)")+ 
      facet_wrap(~PLOT)+
      coord_cartesian(ylim = c(2,11))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    lai_plot <- ggplotly(lai_plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
    lai_plot$x$layout$width <- NULL
    lai_plot$y$layout$height <- NULL
    lai_plot$width <- NULL
    lai_plot$height <- NULL
    lai_plot %>%
      layout(autosize = TRUE)
  })
  
  #plot of paper birch and sugar maple decline after ice storm
  #also there's a weirdly large spike in 2011...
  output$leaf_count <- renderPlotly({
    count_means<-count_means[count_means$SITE != "W5",]
    leaf_count <- ggplot(count_means, aes(x=YEAR, y=count, color = species))+
      geom_line()+ my_theme+
     facet_wrap(~SITE, ncol = 1, labeller= as_labeller(site_names))+
      theme(legend.title = element_text("Tree Species", family = "Helvetica"),
            strip.text = element_text(size = 10))+
      xlab(" ")+
      ylab("\n Leaf counts")+ 
      geom_vline(size = 0.5, xintercept = 1998, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = 1998, y = 120, color = "black")
      
    #Wrap in plotly and hide unnecessary plotly toolbar
    leaf_count <- ggplotly(leaf_count, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    
    #manually adjust margins so labs aren't cut off
    leaf_count$x$layout$margin$l <- leaf_count$x$layout$margin$l + 30
    leaf_count$x$layout$margin$b <- leaf_count$x$layout$margin$b + 30
    
    #makes plot readjust correctly to window
    leaf_count$x$layout$width <- NULL
    leaf_count$y$layout$height <- NULL
    leaf_count$width <- NULL
    leaf_count$height <- NULL
    leaf_count %>%
      layout(autosize = TRUE)
  })
  
  #plot to generally show how the ice storm affected NO3 (conc or flux?)
  output$NO3_plot <- renderPlotly({
    NO3_plot <- ggplot_function(reactive_data(), x(), y(), log=input$log, units=input$units, date_range=input$date_range2)
    NO3_plot$x$layout$width <- NULL
    NO3_plot$y$layout$height <- NULL
    NO3_plot$width <- NULL
    NO3_plot$height <- NULL
    NO3_plot %>%
      layout(autosize = TRUE)
  })
  
  #make a plot of nitrates like in the 2003 paper
  #(moles/ha-yr (flux) vs water year, faceted into output for ws1,6 and excess (norm) for ws2,4,5)
  output$NO3_output <- renderPlotly({
    NO3_output <- ggplot_function(reactive_data_flux(), xflux(), yflux(), log=input$log_flux, units="moles/ha-yr", date_range=input$date_range3)
    NO3_output$x$layout$width <- NULL
    NO3_output$y$layout$height <- NULL
    NO3_output$width <- NULL
    NO3_output$height <- NULL
    NO3_output %>%
      layout(autosize = TRUE#, showlegend = T
             )
  })

  #NO3 excess
  output$NO3_excess <- renderPlotly({
    NO3_excess <- ggplot_function(reactive_data_norm(), xflux(), ynorm(), log="linear", units="moles/ha-yr", date_range=input$date_range3)
    NO3_excess$x$layout$width <- NULL
    NO3_excess$y$layout$height <- NULL
    NO3_excess$width <- NULL
    NO3_excess$height <- NULL
    NO3_excess %>%
      layout(autosize = TRUE#, showlegend=T
      )
    })
})
