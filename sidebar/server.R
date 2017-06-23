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



#Theme
my_theme <- theme_fivethirtyeight() + 
  theme(rect = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#dddddd"), 
        text = element_text(family = "Helvetica", size = 12), 
        legend.position = "none", legend.direction = "vertical", legend.title = element_blank(),
        strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
        axis.title= element_text(NULL), axis.title.x= element_blank(), 
        axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")

solute_palette <- c(cation, anion, hydro)
source_shapes <- c("flow" = 16, "precip"= 21)

#read in the data
precip_dis <- 
  readRDS("precip_stream_data.rds")

#Add columns of the natural logarithm of relevant values
ln_concentration_ueq <- as.data.frame(log(precip_dis$concentration_ueq))
ln_ueq_weighted_average <- as.data.frame(log(precip_dis$ueq_weighted_average))
ln_flux <- as.data.frame(log(precip_dis$flux))
ln_flux_sum <- as.data.frame(log(precip_dis$flux_sum))

precip_dis <- cbind(as.data.frame(precip_dis), ln_concentration_ueq,
                    ln_ueq_weighted_average, ln_flux,
                    ln_flux_sum)
colnames(precip_dis) <- c("ws", "date", "water_date", "water_year", "solute",
                          "concentration_mg", "source", "water_mm_pm", "MW", "z",
                          "concentration_ueq", "concentration_umol", "flux",
                          "mg_weighted_average", "umol_weighted_average",
                          "ueq_weighted_average","flux_sum",
                          "ln_concentration_ueq", "ln_ueq_weighted_average",
                          "ln_flux", "ln_flux_sum")


#Write a function that converts the Source code from precip and 
#flow to Precipitation and Streamwater Discharge
source_change <- function(df){
  df$Source <- ifelse(df$Source == "precip", "Precipitation",
                       "Discharge")
  return(df)
}

df_format <- function(df, column, s, date_scale){
  formatted <- df %>%
    filter(ws == 2| ws==6, solute == s) %>%
    select(ws, water_date, water_year, source, solute, get(column))
  colnames(formatted) = c("ws", "water.date", "water.year",
                          "Source", "solute", "value")
  if (date_scale == "yearly"){
    formatted$date = paste(formatted$water.year)
  }else{
    formatted$date = paste(formatted$water.date)
  }
  formatted$ws = paste("Watershed", formatted$ws, sep = " ")
  formatted <- source_change(formatted)
  return(formatted)
}

#Function to produce a formatted data frame of the quantities of 
#precipitation and discharge on the correct scales
quantity_format <- function(df, time.scale, q_unit) {
  if (time.scale == "monthly"){
    if (q_unit == "mm"){
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2, solute == "Ca")
      colnames(timely) = c("ws", "water.year", "water.date",
                            "solute", "value", "Source")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.date)
      timely <- source_change(timely)
    }else{
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2, solute == "Ca")
      timely$ln_water_mm_pm <- log(timely$water_mm_pm)
      colnames(timely) = c("ws", "water.year", "water.date",
                           "solute", "water_mm_pm", "Source", "value")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.date)
      timely <- source_change(timely)
    }
  }else{
    if (q_unit == "mm"){
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2, solute == "Ca") %>%
        group_by(ws, source, water_year) %>%
        summarize(value = sum(water_mm_pm))
      colnames(timely) <- c("ws", "Source", "water.year", "value")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.year)
      timely <- source_change(timely)
    }else{
      timely <- df %>%
        select(ws, water_year, water_date, 
               solute, water_mm_pm, source)%>%
        filter(ws == 6 | ws == 2, solute == "Ca") %>%
        group_by(ws, source, water_year) %>%
        summarize(value = log(sum(water_mm_pm)))
      colnames(timely) <- c("ws", "Source", "water.year", "value")
      timely$ws <- paste("Watershed", timely$ws, sep = " ")
      timely$date <- paste(timely$water.year)
      timely <- source_change(timely)
    }
    
  }
  return(as.data.frame(timely))
}



#Function to filter and format original data frame to
#make it work as a reactive value for a specific ion and 
#watershed
format2 <- function(df, unit, ion, date.scale){
  if (date.scale == "month"){
    if (unit == "ueq/L"){
      df_format(df = precip_dis, column = "concentration_ueq", s = ion,
                date_scale = "monthly")
    }else if (unit == "Eq/ha-yr"){
      df_format(df = precip_dis, column = "flux", s = ion,
                date_scale = "monthly")
    }else if (unit == "ln(Eq/ha-yr)"){
      df_format(df = precip_dis, column = "ln_flux", s = ion,
                date_scale = "monthly")
    }else{
      df_format(df = precip_dis, column = "ln_concentration_ueq", s = ion,
                date_scale = "monthly")
    }
  }else{
    if (unit == "ueq/L"){
      df_format(df = precip_dis, column = "ueq_weighted_average", s = ion,
                date_scale = "yearly")
    }else if (unit == "Eq/ha-yr"){
      df_format(df = precip_dis, column = "flux_sum", s = ion,
                date_scale = "yearly")
    }else if (unit == "ln(Eq/ha-yr)"){
      df_format(df = precip_dis, column = "ln_flux_sum", s= ion,
                date_scale = "yearly")
    }else{
      df_format(df = precip_dis, column = "ln_ueq_weighted_average", s = ion,
                date_scale = "yearly")
    }
  }
}

#Function to plot the formatted data frame in ggplot2
plot.formatted.df <- function(df, timescale, date.input, y.lab, title.lab, addprecip){
  m <- max(df$value, na.rm = TRUE)
  v.line <- data.frame(Event = "WS2 Cutting", vals = -1675)
  if (addprecip == "precip"){
    p <- ggplot(df,aes(x= get(timescale),y=value, label=date)) +
      geom_line() +
      geom_point() +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = F) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      facet_wrap(~ws, ncol = 1)+ 
      my_theme
  }else{
    p <- ggplot(df,aes(x= get(timescale),y=value,label=date)) +
      geom_line() +
      geom_point() +
      geom_vline(data = v.line,
                 aes(xintercept = vals),
                 linetype = 1,
                 show.legend = F) +
      coord_cartesian(xlim = c(as.Date(date.input[1]), 
                               as.Date(date.input[2])))+
      labs(x = "Date (In Water Years)", 
           y = y.lab,
           title = title.lab)+
      facet_wrap(~ws, ncol = 1)+ 
      my_theme
  }
  return(p)
}


watersheds <- list("Watershed 1" = "ws1",
                   "Watershed 2" = "ws2", 
                   "Watershed 3" = "ws3",
                   "Watershed 4" = "ws4",
                   "Watershed 5" = "ws5",
                   "Watershed 6" = "ws6",
                   "Watershed 7" = "ws7",
                   "Watershed 8" = "ws8",
                   "Watershed 9" = "ws9")

solutes_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Aluminum (Al)" = "Al")

solutes_anions <- list("Phosphate (PO4)" = "PO4",
                       "Sulfate (SO4)" = "SO4",
                       "Nitrate (NO3)" = "NO3",
                       "Silicon Dioxide (SiO2)" = "SiO2",
                       "Chlorine (Cl)" = "Cl",
                       "Bicarbonate (HCO3)" = "HCO3")

shinyServer(function(session, input, output) {
  
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

  
  
  #Reactive value for the units, either in
  #concentration or flux
  unit <- reactive({input$units})
  
  #Reactive value for the type of inputted solute
  sol <- reactive({
    if (input$solute == "Na"){
      "Sodium"
    }else if (input$solute == "Ca"){
      "Calcium"
    }else if (input$solute == "Mg"){
      "Magnesium"
    }else if (input$solute == "K"){
      "Postassium"
    }else if (input$solute == "SO4"){
      "Sulfate"
    }else if (input$solute == "NO3"){
      "Nitrate"
    }else if (input$solute == "Cl"){
      "Chlorine"
    }else{
      "Hydrogen Ion"
    }
    
  })
  
  #Reactive value selecting the discharge quantity data set by the time scale,
  #annually or monthly
  discharge.data <- reactive({
    if (input$p.dis == "precip"){
      if (input$units.dis == "mm"){
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", q_unit = "mm")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", q_unit = "mm")
        }
      }else{
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", q_unit = "ln(mm)")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", q_unit = "ln(mm)")
        }
      }
    }else{
      if (input$units.dis == "mm"){
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", q_unit = "mm") %>%
            filter(Source == "Discharge")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", q_unit = "mm") %>%
            filter(Source == "Discharge")
        }
      }else{
        if (input$scale.dis == "month"){
          quantity_format(df = precip_dis, time.scale = "monthly", q_unit = "ln(mm)") %>%
            filter(Source == "Discharge")
        }else{
          quantity_format(df = precip_dis, time.scale = "yearly", q_unit = "ln(mm)") %>%
            filter(Source == "Discharge")
        }
      }
    }
    
  })
  
  #Reactive value selecting the data set for discharge chemistry
  #by time scale, unit and solute
  solute.data <- reactive({
    if (input$p == "precip"){
      format2(df = precip_dis, unit = input$units,
             ion = input$solute, date.scale = input$scale)
    }else{
      format2(df = precip_dis, unit = input$units,
              ion = input$solute, date.scale = input$scale) %>%
        filter(Source == "Discharge")
    }
  })
  
  #Plot output for water chemistry
  output$s.plot <- renderPlotly({
    y = input$units
    if (input$units == "ueq/L"){
      title = paste(sol(), "Concentration", sep = " ")
    }else if (input$units == "ln(ueq/L)"){
      title = paste("Natural Log of ", sol(), "Concentration", sep = " ")
    }else if (input$units == "Eq/ha-yr"){
      title = paste(sol(), "Flux", sep = " ")
    }else{
      title = paste("Natural Log of", sol(), "Flux", sep = " ")
    }
    if (input$scale == "month"){
      s <- "water.date"
    }else{
      s <- "water.year"
    }
    ggplotly( 
      plot.formatted.df(df = solute.data(),
                        timescale = s,
                        date.input = input$dates, y.lab = y, 
                        title.lab = title,
                        addprecip = input$p),tooltip = c("y", "label"), width = "100vh")%>% 
      config(displayModeBar = F)
  })
  
  #Plot output for discharge quantity
  output$d.plot <- renderPlotly({
    if (input$p.dis == "precip"){
      if (input$units.dis == "mm"){
        y <- "mm"
        title <- "Discharge and Precipitation Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Discharge and Precipitation Quantities"
      }
    }else{
      if (input$units.dis == "mm"){
        y <- "mm"
        title <- "Discharge Quantities"
      }else{
        y <- "ln(mm)"
        title <- "Natural Log of Discharge Quantities"
      }
    }
    
    if (input$scale.dis == "month"){
      s2 <- "water.date"
    }else{
      s2 <- "water.year"
    }
    
    ggplotly(plot.formatted.df(df = discharge.data(), 
                                    timescale = s2,
                                    date.input = input$dates.dis,
                                    y.lab = y, title.lab = title,
                                    addprecip = input$p.dis), tooltip = c("y", "label"), width = "auto")%>% 
      config(displayModeBar = F)
  
    
  })
  options(warn = -1)
})
