library(ggplot2)
#library(lubridate)
library(gridExtra)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
#library(utils)
library(grid)
library(ggthemes)
library(reshape2)

#detach("package:<packageName>", unload=TRUE)

########### DATA IMPORT ####################################################

#load vegetation data
lai_data<- readRDS("lai_data.rds")
yearly_count_means <- readRDS("yearly_count_means.rds")

#load pq data
load("precip_streamflow_dfs.RData")
imported_data <- precip_streamflow_long
data_norm <- readRDS("normalized_flux.rds")

#create vector to rename facets in leaf count plot
site_names <- c(
  'BB'="Bear Brook Watershed site",
  'TF'="Throughfall site",
  'W1'="Watershed 1",
  'W5'="Watershed 5"
  )
########### END OF DATA IMPORT #############################################


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
  
  color_anion <- c("NO3" = "#BF1616")
  
  source_shapes <- c("streamflow" = 16, "precipitation"= 21)
  watershed_linetypes <- c("1"= 2,"2"= 1,"3"= 3,"4"= 4,"5"= 5,"6"= 6,"7"= 1,"8"= 1,"9"= 1)
  
  ### End of Theme ################
  
  ########### END OF IMPORTANT PRELIMINARY INFO #############################################

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
      plot <- ggplot(data=data, aes(x= get(x), y= get(y), linetype= ws, color= solute, shape= source))+
        labs(x = "Water Year", y = units)}
    
    final <- plot+ my_theme + geom_line(size = 0.5) + 
      geom_point(size = 1.3, fill = "white", stroke = 0.5, 
                 aes(text = paste("Watershed: ", ws, "<br>", "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(date_range[1]), max(date_range[2]))+ 
      geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = 5, color = "black")+
      scale_shape_manual(labels=NULL,values = source_shapes) +
      scale_color_manual(labels = NULL, values = color_anion) +
      scale_linetype_manual(labels = c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), values = watershed_linetypes)
    
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
    yearly_count_means<-yearly_count_means[yearly_count_means$SITE != "W5",]
    leaf_count <- ggplot(yearly_count_means, aes(x=YEAR, y=count, color = species))+
      geom_line(size=0.5)+ my_theme+
      geom_point(size=1.3, aes(text = paste("Species: ", species, "<br>", 
                                            "Leaf count: ", count, "<br>", "Year: ", YEAR)))+
      facet_wrap(~SITE, ncol = 1, labeller= as_labeller(site_names))+
      theme(legend.title = element_text("Tree Species", family = "Helvetica"),
            strip.text = element_text(size = 10))+
      xlim(min(input$date_range_count[1]), max(input$date_range_count[2]))+
      xlab(" ")+ ylab("\n Leaf counts")+
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
      layout(autosize = TRUE, legend = guides(shape = F, color = F)
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


#  my_theme <- theme_fivethirtyeight() +
#    theme(rect = element_rect(fill = NA), panel.grid.major = element_line(colour = "#dddddd"), text = element_text(family = "Helvetica", size = 12),
#        legend.position = "right", legend.direction = "vertical", legend.title = element_blank(),
#          strip.text = element_text(hjust = 1, size = 20, face = "bold"),
#        axis.title= element_text(NULL), axis.title.x= element_blank(),
#          axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))
# 
#  color_anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
#  source_shapes <- c("streamflow" = 16, "precipitation"= 21)
#  watershed_linetypes <- c("1"= 2,"2"= 1,"3"= 3,"4"= 4,"5"= 5,"6"= 6,"7"= 1,"8"= 1,"9"= 1)
# 
#  dataa <- imported_data
#  dataa <- dataa[dataa$granularity == "year",]
#  dataa <- dataa[dataa$source %in% c("streamflow"),]
#  dataa <- dataa[dataa$ws %in% c("1", "6"),]
#  dataa <- dataa[dataa$solute %in% c("NO3"),]
# 
# p<-ggplot(dataa, aes(x= water_year, y= flux, linetype= ws, color= solute, shape= source))+
#    labs(x = "Water Year", y = "flux")+    geom_line(size = 0.5) + my_theme + 
#    geom_point(size = 1.3, fill = "white", stroke = 0.5,
#               aes(text = paste("Watershed: ", ws, "<br>", "Value:", flux, "<br>", "Date: ", water_year))) +
#    geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
#    annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = 5, color = "black")+
#    scale_shape_manual(values = source_shapes) +
#    scale_color_manual(values = color_anion)+ guides(shape = F, color = F)
#    scale_linetype_manual(labels = c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), values = watershed_linetypes)
# ggplotly(p)