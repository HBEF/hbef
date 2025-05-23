library(ggplot2)
library(lubridate)
library(gridExtra)
library(readr)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(utils)
library(grid)
library(shinydashboard)

#Establish shapes for the water sources for graphing later
source_shapes <- c("Streamflow (Q)" = 16, "Precipitation (P)"= 21)


#my theme

my_theme <- 
  theme(rect = element_rect(fill = NA),
        panel.background = element_rect("transparent", colour = NA),
        panel.grid.major = element_line(colour = "#dddddd"), 
        panel.grid.major.x = element_line(colour = NA),
        text = element_text(family = "Helvetica", size = 16), 
        legend.position="none", legend.direction = "horizontal", legend.title = element_blank(),
        axis.title = element_text(size = 16, margin = unit(c(3, 3, 3, 3), "cm")),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(hjust = 1, vjust = 0, size = 16, face = "bold", lineheight = 20))

#read in the data
load("precip_streamflow_dfs.RData")
imported_data <- precip_streamflow_long

#Function that creates a column of watershed data written in words, such as
#"Watershed 1"
watershed_change <- function(df){
  df$ws2 <- paste("Watershed", df$ws, sep = " ")
  return(df)
}

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

#Function that converts the water source designators to more descriptive
#labels
source_change <- function(df){
  df[df$source == "streamflow", "source"] = "Streamflow (Q)"
  df[df$source == "precipitation", "source"] = "Precipitation (P)"
  return(df)
}


#Function to plot the formatted solute concentration data frame in ggplot2
plot.solute.df <- function(df, timescale, x.r, y.r, date.input, y.lab, title.lab, logarithm, ecological){
  
  #Vectors of colors for plotting lines and points associated with solutes
  color_cation <- c("Potassium" = "#95AFDD", "Sodium" = "#7195D2", 
                    "Ammonium" = "#4E7AC7" , "Calcium" = "#3B5C95",
                    "Magnesium" = "#273D64", "Aluminum" = "#162338")
  color_anion <- c("Phosphate" = "#600B0B", "Sulfate" = "#8F1010", 
                   "Nitrate" = "#BF1616", "Silicon Dioxide"= "#CC4545",
                   "Chloride" = "#D97373", "Bicarbonate" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "Hydrogen Ion" = "#FFE79C")
  
  #Solute palette that groups together above vectors
  solutes_palette <- c(color_cation, color_anion, color_hydro)
  
  #Establish shapes for the water sources for graphing 
  source_shapes <- c("Streamflow (Q)" = 16, "Precipitation (P)"= 21)
  
  #Information for plotting a vertical line in the different watershed 
  #graphs at the time of cutting
  v.line <- data.frame(ws2 = c("Watershed 2", "Watershed 4",
                               "Watershed 5", "Watershed 6"), 
                       vals = c(-1675, 92, 4926, NA))

  
    if (logarithm == "log") {
      gg <- ggplot(df,aes(x= get(x.r),y= log(get(y.r)), shape =source, 
                          color = solute))
    }else {
      gg <- ggplot(df,aes(x= get(x.r),y= get(y.r), shape =source, 
                          color = solute))
    }
    p <- gg +
      geom_line() +
      geom_point(fill = "white",
                 aes(text = paste("Solute: ", solute, "<br>", 
                                  "Water Source: ", source, "<br>",
                                  "Value:", round(get(y.r), 2), "<br>", 
                                  "Date: ", get(x.r)))) +
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solutes_palette) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      scale_shape_manual(values = source_shapes) +
      facet_wrap(~ws2, ncol = 1, scales = "free_y") +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = T) + my_theme
    plot <- ggplotly(p,tooltip = "text",
                     width = 400, height = 900) %>%
      layout(margin = list(b = 90)) %>%
      config(displayModeBar = FALSE) %>%
      config(showLink = FALSE)
 
  return(plot)
}


plot.dis.df <- function(df, x.r, date.input, y.lab, title.lab, logarithm){

  color_cation <- c("Potassium" = "#95AFDD", "Sodium" = "#7195D2", 
                    "Ammonium" = "#4E7AC7" , "Calcium" = "#3B5C95",
                    "Magnesium" = "#273D64", "Aluminum" = "#162338")
  color_anion <- c("Phosphate" = "#600B0B", "Sulfate" = "#8F1010", 
                   "Nitrate" = "#BF1616", "Silicon Dioxide"= "#CC4545",
                   "Chloride" = "#D97373", "Bicarbonate" = "#E5A2A2")
  color_hydro <- c("pH" = "#FFC408", "Hydrogen Ion" = "#FFE79C")
  
  solute_palette <- c(color_cation, color_anion, color_hydro)
  source_shapes <- c("Streamflow (Q)" = 16, "Precipitation (P)"= 21)
  
  v.line <- data.frame(ws2 = c("Watershed 2", "Watershed 4",
                               "Watershed 5", "Watershed 6"), 
                       vals = c(-1675, 92, 4926, NA))
  if (logarithm == "log"){
    gg <- ggplot(df,aes(x= get(x.r),y= log(water_mm), shape =source, label=date))
  }else{
    gg <- ggplot(df,aes(x= get(x.r),y= water_mm, shape =source, label=date))
  }
  p <- gg +
    geom_line(color = "#171A1E") +
    geom_point(color = "#30363F", fill = "white",
               aes(text = paste("Water Source: ", source, "<br>",
                                "Value:", round(water_mm, 2), "<br>", 
                                "Date: ", get(x.r))))  +
    coord_cartesian(xlim = c(as.Date(date.input[1]), 
                             as.Date(date.input[2])))+
    scale_shape_manual(values = source_shapes) +
    scale_color_manual(values = solute_palette) +
    labs(x = "Date (In Water Years)", 
         y = y.lab,
         title = title.lab)+
    scale_shape_manual(values = source_shapes) +
    facet_wrap(~ws2, ncol = 1, scales = "free_y") +
    geom_vline(data = v.line,
               aes(xintercept = vals),
               linetype = 1,
               show.legend = T) + my_theme
  plot <- ggplotly(p, tooltip = "text",
                   height = 900, width = 400)%>%
    layout(margin = list(b = 90)) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE) 
  return(plot)
}


shinyServer(function(session, input, output) {
  ########### IMPORTANT PRELIMINARY INFO #############################################
  
  ###  allow 'select all' interactivity
  solutes_cations <- list("Potassium (K)" = "Potassium",
                          "Sodium (Na)" = "Sodium",
                          "Calcium (Ca)" = "Calcium",
                          "Magnesium (Mg)" = "Magnesium",
                          "Aluminum (Al)" = "Aluminum")
  
  solutes_anions <- list("Sulfate (SO4)" = "Sulfate",
                         "Nitrate (NO3)" = "Nitrate",
                         "Chloride (Cl)" = "Chloride")
  solutes_H <- c("Hydrogen (H)" = "Hydrogen Ion")
  
  observeEvent(input$select_all_ions, {
    if(input$select_all_ions == 0) {}
    else if (input$select_all_ions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "PO4")
      updateCheckboxGroupInput(session, "solutes_cations", selected = "Sodium")
      updateCheckboxGroupInput(session, "solutes_H", selected = "Hydrogen")}
    else{
      updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)
      updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)
      updateCheckboxGroupInput(session, "solutes_H", selected = solutes_H)}
  })
  
  observeEvent(input$select_all_anions, {
    if(input$select_all_anions == 0) {}
    else if (input$select_all_anions%%2 == 0){updateCheckboxGroupInput(session, "solutes_anions", selected = "Phosphate")}
    else{updateCheckboxGroupInput(session, "solutes_anions", selected = solutes_anions)}
  })
  
  observeEvent(input$select_all_cations, {
    if(input$select_all_cations == 0) {}
    else if (input$select_all_cations%%2 == 0){updateCheckboxGroupInput(session, "solutes_cations", selected = "Sodium")}
    else{updateCheckboxGroupInput(session, "solutes_cations", selected = solutes_cations)}
  })
  
  observeEvent(input$select_all_H, {
    if(input$select_all_H == 0) {}
    else if (input$select_all_H%%2 == 0){updateCheckboxGroupInput(session, "solutes_H", selected = "Hydrogen")}
    else{updateCheckboxGroupInput(session, "solutes_H", selected = solutes_H)}
  })
  
  
  
  solutes <- reactive({c(input$solutes_cations, input$solutes_anions, input$solutes_H)})
 
  
  # The following reactivity prevents the user from deselecting streamflow. 
  #The can add precipitation but not remove streamflow
  observe({
    if((length(input$p.dis) < 2) & "precipitation" %in% input$p.dis){
      updateCheckboxGroupInput(session, "p.dis", selected = c("streamflow", "precipitation"))}
    else if(length(input$p.dis) < 2){
      updateCheckboxGroupInput(session, "p.dis", selected = c("streamflow")) 
    }
  })
  
  observe({
    if((length(input$p) < 2) & "precipitation" %in% input$p){
      updateCheckboxGroupInput(session, "p", selected = c("streamflow", "precipitation"))}
    else if(length(input$p) < 2){
      updateCheckboxGroupInput(session, "p", selected = c("streamflow")) 
    }
  })
  
   
  sources <- reactive({
    if (("precipitation" %in% input$p)){
      c("Precipitation (P)", "Streamflow (Q)")
    }else{
      c("Streamflow (Q)")}
  })
  
  solute_data <- reactive({
    data <- imported_data
    data <- watershed_change(data)
    data <- solute_change(data)
    data <- source_change(data)
    data <- data[data$source %in% sources(),]
    data <- data[data$solute %in% solutes(),] 
    data <- data[data$ws %in% c(2,4,5,6),]
    data <- data[data$granularity %in% input$granularity,]
    
  })
  

  x <- reactive({
    if(input$granularity == "week"){"water_date"}
    else if(input$granularity == "month"){"water_date"}
    else if(input$granularity == "year"){"water_year"}
  })
  
  y <- reactive({
    if(input$units =="mg/L"){"concentration_mg"}
    else if(input$units =="ueq/L"){"concentration_ueq"}
    else if(input$units =="umol/L"){"concentration_umol"}
    else if(input$units =="Eq/ha-yr"){"flux"}
  })
  
  sources.dis <- reactive({
    if (("precipitation" %in% input$p.dis)){
      c("Precipitation (P)", "Streamflow (Q)")
    }else{
      c("Streamflow (Q)")
    }
  })
  discharge_data <- reactive({
    data <- imported_data
    data <- watershed_change(data)
    data <- solute_change(data)
    data <- source_change(data)
    data <- data[data$source %in% sources.dis(),]
    data <- data[data$solute %in% c("Calcium"),] 
    data <- data[data$ws %in% c(2,4,5,6),]
    data <- data[data$granularity %in% input$granularity2,]
  })
  
  x2 <- reactive({
    if(input$granularity2 == "week"){"water_date"}
    else if(input$granularity2 == "month"){"water_date"}
    else if(input$granularity2 == "year"){"water_year"}
  })
  
  
  #Plot output for water chemistry
  output$s.plot <- renderPlotly({
    if (length(solutes()) <= 1){
      if (input$log1 == "linear"){
        title = paste(solutes(), "Concentration", sep = " ")
        y = input$units
      }else{
        title = paste("Natural Log of", solutes(), "Concentration", sep = " ")
        y = paste("ln(", input$units, ")", sep = "")
      }
    }else{
      if (input$log1 == "linear"){
        title = "Solute Concentations"
        y = input$units
      }else{
        title = "Natural Log of Solute Concentrations"
        y = paste("ln(", input$units, ")", sep = "")
      }
    }
    
    theplot = plot.solute.df(df = solute_data(),
                   x.r = x(),
                   y.r = y(),
                   date.input = input$dates,
                   y.lab = y,
                   title.lab = title,
                   logarithm = input$log1)
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(margin = list(b = 90))
  })
  
  #Plot output for discharge quantity
  output$d.plot <- renderPlotly({
    if ("precipitation" %in% input$p.dis){
      if (input$ln.dis == "linear"){
        y <- "mm"
        title <- "Streamflow and Precipitation Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Water Quantities"
      }
    }else{
      if (input$ln.dis == "linear"){
        y <- "mm"
        title <- "Streamflow Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Streamflow Quantities"
      }
    }
    
    
    theplot = plot.dis.df(df = discharge_data(), 
                x.r = x2(),
                date.input = input$dates.dis,
                y.lab = y, title.lab = title,
                logarithm = input$ln.dis)
    theplot$x$layout$width <- NULL
    theplot$y$layout$height <- NULL
    theplot$width <- NULL
    theplot$height <- NULL
    theplot %>%
      layout(margin = list(b = 90))
    
    
  })
  options(warn = -1)
})
