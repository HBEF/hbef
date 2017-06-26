library(magrittr)
library(timevis)
library(shiny)
library(readr)
library(ggplot2)
library(plotly)
library(ggthemes)

#DATA and themes
imported_data <- readRDS("precip_stream_data_long.rds")

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

ggplot_function <- function(data, x, y, color, title, facet, ncol = NULL, nrow = NULL){
  #  ggplotly(  
  (ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws)) + my_theme +
     geom_line(size = 1)+ 
     geom_point(size = 1.5, fill = "white", stroke = 0.5) + 
     ggtitle(title) +
     #     facet_wrap(~get(facet) , ncol = ncol)+ 
     #       xlim(min(input$timeframe[1]), max(input$timeframe[2]))+ 
     #      labs(x = "Water Year", y = units())+ 
     scale_shape_manual(values = source_shapes) +
     scale_color_manual(values = solute_palette) +
     scale_alpha_discrete(range = c(0.9, 0.5))) 
  #    width = 900) %>%
  #   config(displayModeBar = FALSE) %>%
  #  config(showLink = FALSE)  
}


#load in all the data from Camila download
precip_stream_data <- readRDS("D:/Duke/Work(Environ)/Programming/AcidRainStory/DataCleaning/precip_stream_data.rds")

#make a df of acid rain history dates (CAA, etc.) #https://daattali.com/shiny/timevis-demo/
historyData <- data.frame(
  id = 1:6,
  content = c("Majority of HBEF dataset",
              "The Air Pollution Control Act",
              "EPA founded", 
              "Clean Air Act",
              "Clean Air Act Amendment",
              "Today"),
  title = c("Watershed 6 is displayed in this story, as it is the control",
            "",
            "The EPA was founded to enforce the Clean Air Act",
            "The Clean Air Act also has important amendments",
            "Amendment that more specifically addressed acid rain",
            "Today isn't really today"),
  start = c("1963-06-01",
            "1955-01-01",
            "1970-12-02", 
            "1970-06-01",
            "1990-06-01",
            "2017-06-19"), #FIND THE REAL DATE OF CAA ENACTMENT!
  end = c("2014-05-01",
          NA,
          NA, 
          NA,
          NA,
          NA)
)
#making data frames to use with rb selection of cmpd ##OPTIMIZE
CaData <- precip_stream_data[precip_stream_data$solute == "Ca",]
CaData <- CaData[CaData$ws == "6",]

SO4Data <- precip_stream_data[precip_stream_data$solute == "SO4",]
SO4Data <- SO4Data[SO4Data$ws == "6",]

MgData <- precip_stream_data[precip_stream_data$solute == "Mg",]
MgData <- MgData[MgData$ws == "6",]

KData <- precip_stream_data[precip_stream_data$solute == "K",]
KData <- KData[KData$ws == "6",]

NaData <- precip_stream_data[precip_stream_data$solute == "Na",]
NaData <- NaData[NaData$ws == "6",]

AlData <- precip_stream_data[precip_stream_data$solute == "Al",]
AlData <- AlData[AlData$ws == "6",]

ClData <- precip_stream_data[precip_stream_data$solute == "Cl",]
ClData <- ClData[ClData$ws == "6",]

NH4Data <- precip_stream_data[precip_stream_data$solute == "NH4",]
NH4Data <- NH4Data[NH4Data$ws == "6",]

NO3Data <- precip_stream_data[precip_stream_data$solute == "NO3",]
NO3Data <- NO3Data[NO3Data$ws == "6",]

PO4Data <- precip_stream_data[precip_stream_data$solute == "PO4",]
PO4Data <- PO4Data[PO4Data$ws == "6",]

SiO2Data <- precip_stream_data[precip_stream_data$solute == "SiO2",]
SiO2Data <- SiO2Data[SiO2Data$ws == "6",]

HData <- precip_stream_data[precip_stream_data$solute == "H",]
HData <- HData[HData$ws == "6",]

pHData <- precip_stream_data[precip_stream_data$solute == "pH",]
pHData <- pHData[pHData$ws == "6",]
pHData <- pHData[,c(1:4,14,5:13,15:16)]

pHData_precip <- pHData[pHData$source == "precip",]

#make df of SO4 NO3 for graph of mitigation effects
SO4NO3Data <- precip_stream_data[precip_stream_data$solute == c("SO4", "NO3"),]
SO4NO3Data <- SO4NO3Data[SO4NO3Data$ws == "6",]

shinyServer(function(input, output){
  #intro pH plot with only precip
  output$pHtheme <- renderPlotly({
    pHtheme <- ggplot(pHData_precip, aes(x = water_year, y = mg_weighted_average, 
                                         shape = source)) + my_theme +
      geom_ribbon(aes(ymin=4.2, ymax= 5), fill = "black", alpha = 0.1)+ #set this as the critical lower bound?
      geom_ribbon(aes(ymin=4, ymax= 4.2), fill = "black", alpha = 0.4)+
      geom_line(size = 1, aes(color = solute))+
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes(color = solute, 
                     text = paste("pH value: ", mg_weighted_average, "<br>", "Date: ", date)))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("Precipitation de-acidifying in response to acid rain mitigation")+
      labs(x = "Year", y = "pH")+
      coord_cartesian(ylim = c(4, 5.05))+
      geom_vline(size = 0.5, xintercept = 1970-01-01)+
      geom_vline(size = 0.5, xintercept = 1990-01-01)+
      geom_ribbon(aes(ymin=5,ymax=5.5), fill="blue", alpha=0.3)
   #   geom_ribbon(aes(xmin=as.Date("1970-01-01"), xmax=as.Date("1990-01-01")), fill="green", alpha=0.1)+ coord_flip()
    ggplotly(pHtheme, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  #practice with Camila's theme
  output$pH <- renderPlotly({
    annotation <- list(yref = 'paper', xref = "x", y = 0.5, x = 1960, text = "annotation")
    
    pHtheme <- ggplot_function(data = pHData_precip, x = "water_year", y = "mg_weighted_average")
    layout(annotations= list(annotation))
  })
  
  #pH plot with P and Q to show acid in, more neutralized out
  output$pHPandQ <- renderPlotly({
    pHPandQ <- ggplot(pHData, aes(x = water_year, y = mg_weighted_average, 
                                  shape = source, color = solute, alpha = ws))+ my_theme +
      geom_line(size = 1)+
      geom_point(size = 1.5, fill = "white", stroke = 0.5, 
                 aes(text = paste("pH value: ", mg_weighted_average, "<br>", "Date: ", date)))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("De-acidification of P and Q in response to reducing SOx and NOx emissions")+
      labs(x = "Year", y = "pH")+
      coord_cartesian(ylim = c(4, 5.4))
    ggplotly(pHPandQ, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  #plot of SO4 and NO3 to complement pH increase - shows decreasing trend
  output$SO4NO3reductions <- renderPlotly({
    SO4NO3reductions <- ggplot(NULL, aes(shape = source, color = solute, alpha = ws))+
      geom_line(data = NO3Data, aes(x = water_year, y = ueq_weighted_average), size = 1)+
      geom_point(data = NO3Data, aes(x = water_year, y = ueq_weighted_average,
                              text = paste("NO3 Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      geom_line(data = SO4Data, aes(x = water_year, y = ueq_weighted_average), size = 1)+
      geom_point(data = SO4Data, aes(x = water_year, y = ueq_weighted_average,
                              text = paste("SO4 Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("SOx and NOx concentrations lowering as policy is implemented")+
      labs(x = "Year", y = "ueq/L")+ my_theme
    ggplotly(SO4NO3reductions, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #base cations trend plot to read next to decreasing so4/no3 and increasing pH
  output$baseCations <- renderPlotly({
    baseCations <- ggplot(NULL, aes(shape = source, color = solute, alpha = ws))+ my_theme+
      geom_line(data = CaData, aes(x = water_year, y = ueq_weighted_average), size = 1)+ #alter Camila function to create one more custom to this style? (ie less input based, more to see trend)
      geom_point(data = CaData, aes(x = water_year, y = ueq_weighted_average,
                                     text = paste("Ca Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      #NOTE all of these extra geom_line and _point are to graph additional base cations rather than creating yet another df
      geom_line(data = MgData, aes(x = water_year, y = ueq_weighted_average), size = 1)+ 
      geom_point(data = MgData, aes(x = water_year, y = ueq_weighted_average,
                                    text = paste("Mg Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      geom_line(data = KData, aes(x = water_year, y = ueq_weighted_average), size = 1)+ 
      geom_point(data = KData, aes(x = water_year, y = ueq_weighted_average,
                                    text = paste("K Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      geom_line(data = NaData, aes(x = water_year, y = ueq_weighted_average), size = 1)+ 
      geom_point(data = NaData, aes(x = water_year, y = ueq_weighted_average,
                                   text = paste("Na Concentration: ", ueq_weighted_average, "<br>", "Date: ", water_year)), 
                 size = 1.5, fill = "white", stroke = 0.5)+
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      ggtitle("Decrease in Base cations leaving the soil")+
      labs(x = "Year", y = "ueq/L")
    ggplotly(baseCations, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })################## CHANGE FROM TRIANGLES SOMEHOW
  
  #plot of any compound conc (reactively chosen) over rective time
  output$cTime <- renderPlotly({
    cTime <- ggplot(get(input$selComp), 
                    aes(x = as.Date(get(input$selDate)), y = ueq_weighted_average,
                        shape = source, color = solute, alpha = ws))+ my_theme+
      geom_line(size = 1) +
      geom_point(size = 1.5, fill = "white", stroke = 0.5,
                 aes(text = paste("Concentration:", concentration_ueq, "<br>", "Date:", date)))+
      scale_shape_manual(values = source_shapes) +
      scale_color_manual(values = solute_palette) +
      scale_alpha_discrete(range = c(0.9, 0.5))+
      labs(colour = "Source", x = "Year", y = "(ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
      ggtitle(as.character(input$selComp), "affected by acid rain") #possibly rename 'CaData' to be 'Calcium'
    ggplotly(cTime, tooltip = "text", width = 900)%>%
      config(displayModeBar = F)%>%
      config(showLink = F)
  })
  
  #output an interactive timeline for the history of acid rain
  output$CAAetc <- renderTimevis({
    timevis(historyData) #possibly use groups in order to contextualize (ie disney movie years)
  })
  
})
