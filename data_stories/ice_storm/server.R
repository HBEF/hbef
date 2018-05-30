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


########### DATA IMPORT ####################################################

#load vegetation data - LAI and leaf counts
lai_data<- readRDS("lai_data.rds")
yearly_count_means <- readRDS("yearly_count_means.rds")

#load water chemistry data
water_chem_data <- readRDS("normalized_flux.rds")

#create vector to rename facets in leaf count plot
site_names <- c(
  'BB'="Bear Brook Watershed site",
  'TF'="Throughfall site",
  'W1'="Watershed 1",
  'W5'="Watershed 5"
  )
########### END OF DATA IMPORT #############################################


########### IMPORTANT PRELIMINARY INFO #############################################

###  Theme  ################
my_theme <- theme_fivethirtyeight() + 
  theme(rect = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#dddddd"), 
        text = element_text(family = "Helvetica", size = 12), 
        legend.position = "right", legend.direction = "vertical", legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title= element_text(NULL), axis.title.x= element_blank(), 
        axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

color_anion <- c("NO3" = "#BF1616")
source_shapes <- c("streamflow" = 16, "precipitation"= 21)
watershed_linetypes <- c("1"= 2,"2"= 1,"3"= 3,"4"= 5,"5"= 4,"6"= 6,"7"= 1,"8"= 1,"9"= 1)

### End of Theme ################

########### END OF IMPORTANT PRELIMINARY INFO #########################################


#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################

shinyServer(function(session, input, output) {

  ########### REACTIVE DATA AND X Y #########################################
  
  #Reactive Data for NO3 trends
  reactive_data <- reactive({
    data <- water_chem_data
    data <- data[data$granularity %in% input$granularity,]
    data <- data[data$source %in% c("streamflow"),]
    data <- data[data$solute %in% c("NO3"),] 
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
    data <- water_chem_data
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
    data <- water_chem_data
    data <- data[data$granularity %in% input$granularity3,]
    data <- data[data$source %in% c("streamflow"),]
    data <- data[data$ws %in% c("2", "4", "5"),]
    data <- data[data$solute %in% c("NO3"),]
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
    
    #if statement to make log Y axis and define plot basics
    if(log == "log") {
      plot <- ggplot(data=data, aes(x= get(x), y= logb(get(y), base=exp(1)), linetype= ws, color = solute, shape = source))+
        labs(x = "Water Year", y = paste("log", "(",units, ")"))}
    
    else{
      plot <- ggplot(data=data, aes(x= get(x), y= get(y), color = solute, shape = source, linetype= ws))+
        labs(x = "Water Year", y = units)}
    
    #create rest of basic plot format
    final <- plot+ my_theme + geom_line(size = 0.5) + 
      geom_point(size = 1.3, fill = "white", stroke = 0.5, 
                 aes(text = paste("Watershed: ", ws, "<br>", "Value:", get(y), "<br>", "Date: ", get(x)))) + 
      xlim(min(date_range[1]), max(date_range[2]))+ 
      geom_vline(size = 0.5, xintercept = 10235, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = as.Date("1998-01-07"), y = 5, color = "black")+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = color_anion) +
      scale_linetype_manual(values = watershed_linetypes)
    #wrap in plotly
    ggplotly(final, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) 
    }
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  #ggplotly that shows most plots increase in tree lai following the ice storm
  output$lai_plot <- renderPlotly({
    lai_plot <- ggplot(lai_data[lai_data$WS == input$watersheds1,], #filtered by WS1 becuase it showed trends more clearly
                       aes(x = YEAR, y = LAIT, color = ELEVATION_M))+ my_theme+
      geom_point(aes(text = paste("Year: ", YEAR, "<br>", "LAI: ", LAIT)))+
      geom_smooth(method = "lm", se = F, size = 0.5)+
      labs(color="Plot Elevation", x=" ", y="LAIT (meter-squared per meter-squared)")+ #LAIT = LAI of only trees
      facet_wrap(~PLOT)+ coord_cartesian(ylim = c(2,11))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1), 
            legend.title = element_text("Plot Elevation", family = "Helvetica"))
    #wrap in plotly
    lai_plot <- ggplotly(lai_plot, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
    #make plot readjust consistently to fill window
    lai_plot$x$layout$width <- NULL
    lai_plot$y$layout$height <- NULL
    lai_plot$width <- NULL
    lai_plot$height <- NULL
    lai_plot %>%
      layout(autosize = TRUE)
  })
  
  #plot of beech, sugar maple, etc. leaf count decline due to ice storm
  output$leaf_count <- renderPlotly({
    yearly_count_means<-yearly_count_means[yearly_count_means$SITE != "W5",] #filter for all but w5 since w5 has so much missing data
    #filter for all but Aspen and Pin Cherry since they are always zero
    yearly_count_means<-yearly_count_means[yearly_count_means$species != "Aspen",]
    yearly_count_means<-yearly_count_means[yearly_count_means$species != "Pin Cherry",]
    
    #if statement for log Y axis
    if(input$log_counts == "log") {
      plot <- ggplot(yearly_count_means, aes(x=YEAR, y=logb(count, base=exp(1)), color = species))+
        labs(x = " ", y = paste("log", "(Leaf counts)"))+
        coord_cartesian(ylim = c(-1,5))}
    
    else{
      plot <- ggplot(yearly_count_means, aes(x=YEAR, y=count, color = species))+
        labs(x = " ", y = "Leaf Counts")}
    
    #rest of plot
    leaf_count <- plot + geom_line(size=0.5)+ my_theme+
      geom_point(size=1.3, 
                 #text/variables to display in the plotly tooltip hover
                 aes(text = paste("Species: ", species, "<br>", "Leaf count: ", count, "<br>", "Year: ", YEAR)))+
      #facet by site and rename facets to be full site names
      facet_wrap(~SITE, ncol = 1, labeller= as_labeller(site_names))+
      xlim(min(input$date_range_count[1]), max(input$date_range_count[2]))+
      #Ice storm annotation
      geom_vline(size = 0.5, xintercept = 1998, alpha = 0.5)+
      annotate("text", label = "   Ice storm", x = 1998, y = 120, color = "black")
    
    #Wrap in plotly and hide unnecessary plotly toolbar
    leaf_count <- ggplotly(leaf_count, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE) %>%
      #annotate legend title so that it's connected to the legend itself
      add_annotations(text="Species", xref="paper", yref="paper",
                      x=1, xanchor="left",
                      y=0.8, yanchor="bottom",    # Same y as legend below
                      legendtitle=TRUE, showarrow=FALSE) %>%
      layout(legend=list(y=0.8, yanchor="top"))
    
    #manually adjust margins so axis labs aren't cut off
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
    #readjusts plot size to consistently fill window
    NO3_plot$x$layout$width <- NULL
    NO3_plot$y$layout$height <- NULL
    NO3_plot$width <- NULL
    NO3_plot$height <- NULL
    NO3_plot %>%
      layout(autosize = TRUE, showlegend = FALSE)
  })
  
  #recreate plots of nitrate fluxes (Bernhardt et. al, 2003)
  #(moles/ha-yr (flux) vs water year, faceted into NO3 streamflow for ws1,6 and excess (normalized) for ws2,4,5)
  output$NO3_output <- renderPlotly({
    NO3_output <- ggplot_function(reactive_data_flux(), xflux(), yflux(), log=input$log_flux, units="moles/ha-yr", date_range=input$date_range3)%>%
      #annotate legend title so that it is connected to the legend itself
      add_annotations(text="Watershed", xref="paper", yref="paper",
                       x=1, xanchor="left",
                       y=0.8, yanchor="bottom",    # Same y as legend below
                       legendtitle=TRUE, showarrow=FALSE) %>%
      layout(legend=list(y=0.8, yanchor="top"))
    #readjusts plot size to consistently fill window
    NO3_output$x$layout$width <- NULL
    NO3_output$y$layout$height <- NULL
    NO3_output$width <- NULL
    NO3_output$height <- NULL
    NO3_output %>%
      layout(autosize = TRUE)
    #manually code what shows in the legend
    NO3_output[['x']][['data']][[1]][['name']] <- '1'
    NO3_output[['x']][['data']][[2]][['name']] <- '6'
    NO3_output
  })
  
  #NO3 excess plot
  output$NO3_excess <- renderPlotly({
    NO3_excess <- ggplot_function(reactive_data_norm(), xflux(), ynorm(), log="linear", units="moles/ha-yr", date_range=input$date_range3)%>%
      #annotate legend title so that it is connected to the legend itself
      add_annotations(text="Watershed", xref="paper", yref="paper",
                      x=1, xanchor="left",
                      y=0.8, yanchor="bottom",    # Same y as legend below
                      legendtitle=TRUE, showarrow=FALSE) %>%
      layout(legend=list(y=0.8, yanchor="top"))
    
    #readjusts plot size to consistently fill window
    NO3_excess$x$layout$width <- NULL
    NO3_excess$y$layout$height <- NULL
    NO3_excess$width <- NULL
    NO3_excess$height <- NULL
    NO3_excess %>%
      layout(autosize = TRUE)
    #manually code what shows in the legend
    NO3_excess[['x']][['data']][[1]][['name']] <- '2'
    NO3_excess[['x']][['data']][[2]][['name']] <- '4'
    NO3_excess[['x']][['data']][[3]][['name']] <- '5'
    NO3_excess
  })
})
