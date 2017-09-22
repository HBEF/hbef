library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(utils)
library(shinydashboard)




#######################################################################################
########### SHINY SERVER ##############################################################
#######################################################################################





#Function that creates a column of descriptive watershed labels, 
#such as "Watershed 1"
watershed_change <- function(df){
  df$ws2 <- paste("Watershed", df$ws, sep = " ")
  return(df)
}

#list of cations to enable select all interactivity later
solutes_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Aluminum (Al)" = "Al")

#list of anions to enable select all interactivity later
solutes_anions <- list("Sulfate (SO4)" = "SO4",
                       "Nitrate (NO3)" = "NO3",
                       "Chloride (Cl)" = "Cl")

#Hydrogen designation for select all interactivity later
solutes_H <- c("Hydrogen (H)" = "H")

#Function that converts the solute labels to their full written name
solute_change <- function(df){
  df[df$solute == "K", "solute"] = "Potassium"
  df[df$solute == "Na", "solute"] = "Sodium"
  df[df$solute == "Ca", "solute"] = "Calcium"
  df[df$solute == "Mg", "solute"] = "Magnesium"
  df[df$solute == "Al", "solute"] = "Aluminum"
  df[df$solute == "SO4", "solute"] = "Sulfate"
  df[df$solute == "NO3", "solute"] = "Nitrate"
  df[df$solute == "Cl", "solute"] = "Chloride"
  df[df$solute == "H", "solute"] = "Hydrogen Ion"
  return(df)
  
}

#Function that converts the source labels to more descriptive strings
#Used for hover information later
source_change <- function(df){
  df[df$source == "streamflow", "source"] = "Streamflow (Q)"
  df[df$source == "precipitation", "source"] = "Precipitation (P)"
  return(df)
}

########### PLOT FUNCTIONS #########################################

##ggplot2 function for dilutification graphs, allows facetting by solute and
#watershed
ggplot_function <- function(data, x, y,facet, w.s, ion, log,
                            units, date.input, source_shapes, 
                            solute_palette, my_theme){
  #if statements determine how many columns there are as well as the height of
  #the graphs by how many solutes or watersheds are presented in the facets
  if (length(ion) == 1){
    if (length(w.s) %in% c(1,2,3)){
      col1 = 1
      h = 400
    }else if (length(w.s) == 4){
      col1 = 2
      h = 400
    }else if (length(w.s) == 5){
      col1 = 1
      h = 600
    }else if (length(w.s) == 6){
      col1 = 2
      h = 600
    }else if (length(w.s) == 7){
      col1 = 1
      h = 800
    }else if (length(w.s) == 8){
      col1 = 2
      h = 800
    }else if (length(w.s) == 9){
      col1 = 3
      h = 800
    }
  }
  if (length(w.s) == 1){
    if (length(ion) %in% c(1,2, 3)){
      col2 = 1
      h = 400
    }else if (length(ion) == 4){
      col2 = 2
      h = 400
    }else if (length(ion) == 5){
      col2 = 1
      h = 600
    }else if (length(ion) == 6){
      col2 = 2
      h = 600
    }else if (length(ion) == 7){
      col2 = 1
      h = 800
    }else if (length(ion) == 8){
      col2 = 2
      h = 800
    }else if (length(ion) == 9){
      col2 = 3
      h = 800
    }else{
      col2 = 1
      h = 800
    }
  }
  
  #Below if statement determines if the y-values will be presented on a log scale or 
  #linear scale
  if(log == "log") {
    plot <- ggplot(data=data, aes(x = get(x), y = logb(get(y), base=exp(1)), 
                                  shape = source, color = solute))+ 
      labs(x = "Water Year", y = paste("log", "(",units, ")", sep = ""))
    
  }else{
    plot <- ggplot(data=data, aes(x = get(x), y = get(y), 
                                  shape = source, color = solute))+
      labs(x = "Water Year", y = units)}
  
  #If statements used to determine plots for different facetting variables,
  #watershed or solute
  if (facet == "w.s"){
    #Below if statement used if there is only one watershed shown if the facetting
    #variable is also by watershed
    if (length(w.s) <= 1) {
      final <- plot+ geom_line(size = 1,aes(color = solute)) + 
        geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                   aes(color = solute, shape = source,
                       text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                    "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
        #text gives hover information
        geom_smooth(method = "lm", color = "green", se = FALSE) +
        labs(title = paste(data$solute, "Concentration", sep = " ")) +
        #title reactive to solute, gives solute name in title
        coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                 as.Date(date.input[2])))+
        #above command sets the x limits to that given by the date slider
        scale_shape_manual(values = source_shapes) +
        #above command sets the water source shapes
        scale_color_manual(values = solute_palette) +
        #above command sets the colors of the solutes
        scale_alpha_discrete(range = c(0.9, 0.5)) +
        scale_x_date(date_breaks = "10 years", date_labels = "%Y")
      #above command sets the breaks in the x scale
      
    }else{
      #This statement plots graphs facetted by watershed
      final <- plot+ geom_line(size = 1,aes(color = solute)) + 
        geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                   aes(color = solute, shape = source,
                       text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                    "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) + 
        #text gives hover information
        coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                 as.Date(date.input[2])))+
        #above command sets x limits according to the date slider
        geom_smooth(method = "lm", color = "green", se = FALSE) +
        facet_wrap(~ws2, ncol = col1, scales = "free") +
        #above command facets by watershed, setting the number of columns depending on
        #the number of watersheds shown, and allowing the scales to change for each
        #watershed
        labs(title = paste(data$solute, "Concentrations", sep = " ")) +
        #title gives the solute name
        scale_shape_manual(values = source_shapes) +
        #sets the source shapes
        scale_color_manual(values = solute_palette) +
        #above command sets the solute colors
        scale_alpha_discrete(range = c(0.9, 0.5))+
        scale_x_date(date_breaks = ifelse(col1 %in% c(2,3), "20 years", 
                                          "10 years"),
                     date_labels = "%Y")
      #above command sets the date breaks to 20 years instead of 10 years if there is
      #more than one column displayed
      
    }
  }else{
    #this else statement allows facetting by solute
    if (length(ion) <= 1){
      #Creates a plot for if there is only one solute selected
      final <- plot+ geom_line(size = 1,aes(color = solute)) +
        geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                   aes(color = solute, shape = source,
                       text = paste("Solute: ", solute, "<br>", "Water Source: ", source, "<br>",
                                    "Value:", round(get(y), 2), "<br>", "Date: ", get(x)))) +
        #text provides hover information
        coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                 as.Date(date.input[2]))) +
        #above command allows for the date slider to change x-limits
        labs(title = paste(data$solute, "Concentration", sep = " ")) +
        #above command changes the title to reflect the solute
        geom_smooth(method = "lm", color = "green", se = FALSE) +
        scale_shape_manual(values = source_shapes) +
        #sets the source shapes
        scale_color_manual(values = solute_palette) +
        #sets consistent solute colors
        scale_alpha_discrete(range = c(0.9, 0.5)) +
        scale_x_date(date_breaks = "10 years", date_labels = "%Y")
      #sets the date break to 10 years on the x-axis 
      #if there is only one solute selected
    }else{
      #creates a plot for when more than one solute is selected
      final <- plot+ geom_line(size = 1,aes(color = solute)) + 
        #coloring by solute, although only one will be shown 
        #per graph
        geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                   aes(color = solute, shape = source,
                       text = paste("Solute: ", solute, "<br>", 
                                    "Water Source: ", source, 
                                    "<br>",
                                    "Value:", round(get(y), 2), 
                                    "<br>", "Date: ", get(x)))) +
        #"text" provides hover information
        geom_smooth(method = "lm", color = "green", se = FALSE) +
        #above command creates a green best-fit line
        coord_cartesian(xlim = c(as.Date(date.input[1]), 
                                 as.Date(date.input[2])))+
        #above command allows the date slider to change the
        #x-limits
        labs(title = "Solute Concentrations") +
        facet_wrap(~solute, ncol = col2, scales = "free") +
        #wrapping by solute with # of columns set by an if 
        #statement above that is based on the number of 
        #solutes included. Scales adjust to the data
        #for each facet graph
        scale_shape_manual(values = source_shapes) +
        #sets the source shapes
        scale_color_manual(values = solute_palette) +
        #sets consistent solute colors
        scale_alpha_discrete(range = c(0.9, 0.5))+
        scale_x_date(date_breaks = ifelse(col2 %in% c(2,3), "20 years", 
                                          "10 years"),
                     date_labels = "%Y")
      #above command sets the date breaks to 20 years if 
      #there are 2 or 3 columns and to 10 years if there
      #is one column. Date labels are shown as years.
    }
  }
  
  #below: adding the theme commands
  final <- final + my_theme
  p = hide_guides(ggplotly(  
    final, tooltip = "text",
    width = 1000, height = h))%>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE)
  #height of ggplotly object set by if statement
  return(p)
  
}

shinyServer(function(session, input, output) {
  
  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  Theme  ################
  #my theme
  
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
          strip.text = element_text(hjust = 1, vjust = 0, size = 10, face = "bold", lineheight = 30))
  
  #Solute colors for plotting
  color_cation <- c("Potassium" = "#95AFDD", "Sodium" = "#7195D2", "Calcium" = "#3B5C95",
                    "Magnesium" = "#273D64", "Aluminum" = "#162338")
  color_anion <- c("Sulfate" = "#8F1010", "Nitrate" = "#BF1616", "Chloride" = "#D97373")
  color_hydro <- c("Hydrogen Ion" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  
  #Water source shapes for plotting
  source_shapes <- c("Streamflow (Q)" = 16, "Precipitation (P)"= 21)
  
  ### End of Theme ################
  
  ###  Lists for the sidebar  ######
  #Edit if there are values that do not appear or are not relevant to your data. 
  #Should be the same as the lists in the UI file.
  
  watersheds <- list("Watershed 1" = 1,
                     "Watershed 2" = 2, 
                     "Watershed 3" = 3,
                     "Watershed 4" = 4,
                     "Watershed 5" = 5,
                     "Watershed 6" = 6,
                     "Watershed 7" = 7,
                     "Watershed 8" = 8,
                     "Watershed 9" = 9)
  
  solutes_cations <- list("Potassium (K)" = "K",
                          "Sodium (Na)" = "Na",
                          "Calcium (Ca)" = "Ca",
                          "Magnesium (Mg)" = "Mg",
                          "Aluminum (Al)" = "Al")
  
  solutes_anions <- list("Sulfate (SO4)" = "SO4",
                         "Nitrate (NO3)" = "NO3",
                         "Chloride (Cl)" = "Cl")
  
  ########### END OF IMPORTANT PRELIMINARY INFO #############################################
  
  
  
  
  ########### SIDEBAR FUNCTIONS ##############################################################
  ###  allow 'select all' interactivity, do not edit
  
  observeEvent(input$select_all_ions, {
    if(input$select_all_ions == 0) {}
    else if (input$select_all_ions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations", selected = "Potassium")
      updateCheckboxGroupInput(session, "solutes_H", selected = "Hydrogen Ion")}
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
  
  observeEvent(input$select_all_H, {
    if(input$select_all_H == 0) {}
    else if (input$select_all_H%%2 == 0){updateCheckboxGroupInput(session, "solutes_H", selected = "H")}
    else{updateCheckboxGroupInput(session, "solutes_H", selected = solutes_H)}
  })
  
  observeEvent(input$select_all_ws, {
    if(input$select_all_ws == 0) {updateSelectInput(session, "watersheds", selected = 1)}
    else if (input$select_all_ws%%2 == 0){updateSelectInput(session, "watersheds", selected =  1)}
    else{updateSelectInput(session, "watersheds", selected = watersheds)}
  })
  
  
  # The following reactivity prevents the user from deselecting streamflow. 
  #The can add precipitation but not remove streamflow
  observe({
    if((length(input$water_sources) < 2) & "precip" %in% input$water_sources){
      updateCheckboxGroupInput(session, "water_sources", selected = c("streamflow", "precipitation"))}
    else if(length(input$water_sources) < 2){
      updateCheckboxGroupInput(session, "water_sources", selected = c("streamflow")) 
    }
  })
  
  observe({
    if((length(input$water_sources2) < 2) & "precip" %in% input$input$water_sources2){
      updateCheckboxGroupInput(session, "water_sources2", selected = c("streamflow", "precipitation"))}
    else if(length(input$water_sources2) < 2){
      updateCheckboxGroupInput(session, "water_sources2", selected = c("streamflow")) 
    }
  })
  
  #Groups selected cations, anions, and Hydrogen together into a a reactive solute vector
  solutes2 <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
  
  ########### END OF SIDEBAR FUNCTIONS ####################################################
  
  
  
  
  ########### DATA IMPORT ####################################################
  load("precip_streamflow_dfs.RData")
  imported_data <- precip_streamflow_long
  imported_data <- watershed_change(imported_data)
  
  
  
  ########### END OF DATA IMPORT #############################################
  
  
  ########### REACTIVE DATA AND X Y  #########################################
 #Reactive data for facetting by watershed
  reactive_data <- reactive({
    data <- imported_data
    data <- data[data$source %in% input$water_sources,]
    data <- data[data$solute %in% input$sol,] 
    data <- data[data$ws %in% input$watersheds,]
    data <- data[data$granularity %in% input$granularity,]
    data <- solute_change(data)
    data <- source_change(data)
  })
  
  #Reactive data for facetting by solute
  reactive_data2 <- reactive({
    data <- imported_data
    data <- data[data$source %in% input$water_sources2,]
    data <- data[data$solute %in% solutes2(),] 
    #note that solutes2 is a function, that's because the inputs for solutes come from input$cations and input$anions
    data <- data[data$ws %in% input$watersheds2,]
    data <- data[data$granularity %in% input$granularity2, ]
    data <- solute_change(data)
    data <- source_change(data)
  })
  
  #Reactive values for x for facetting by watershed
  x <- reactive({
    if(input$granularity == "month"){"water_date"}
    else if(input$granularity == "year"){"water_year"}
  })
  
  #Reactive values for x for facetting by solute
  x2 <- reactive({
    if(input$granularity2 == "month"){"water_date"}
    else if(input$granularity2 == "year"){"water_year"}
  })
  
  #Reactive y values for facetting by watershed
  y <- reactive({
    if(input$units =="mg/L"){"concentration_mg"}
    else if(input$units =="ueq/L"){"concentration_ueq"}
    else if(input$units =="umol/L"){"concentration_umol"}
  })
  
  #Reactive y values for facetting by solute
  y2 <- reactive({
    if(input$units2 =="mg/L"){"concentration_mg"}
    else if(input$units2 =="ueq/L"){"concentration_ueq"}
    else if(input$units2 =="umol/L"){"concentration_umol"}
  })
  

  
 
  
  
  
  #############################################################
  ########### OUTPUTS #########################################
  #############################################################
  
  #below: outputs the graphs which are facetted by watershed
  output$plot1 <- renderPlotly({
    theplot <- ggplot_function(reactive_data(), x(), y(), facet = "w.s",
                               w.s = input$watersheds, ion = input$sol,
                               log = input$log1,
                               units = input$units,
                               date.input = input$date_range,
                               solute_palette = solute_palette,
                               source_shapes = source_shapes,
                               my_theme = my_theme)
    #the code below fixes an issue where the plotly width 
    #argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
    layout(margin = list(b = 90))
  })
  
  #below: outputs the plots that are facetted by solute
  output$plot2 <- renderPlotly({
    theplot <- ggplot_function(reactive_data2(), x2(), y2(), facet = "solutes",
                               w.s = input$watersheds2, ion = solutes2(),
                               log = input$log2,
                               units = input$units2,
                               date.input = input$date_range2,
                               solute_palette = solute_palette,
                               source_shapes = source_shapes,
                               my_theme = my_theme)
    #the code below fixes an issue where the plotly width 
    #argument doesn't adjust automatically. 
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
    layout(margin = list(b = 90))
  })
  
  
  
})
