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


#load in all the data from Camila download
precip_stream_data <- readRDS("D:/Duke/Work(Environ)/Programming/AcidRainStory/DataCleaning/precip_stream_data.rds")

#make a df of acid rain history dates (CAA, etc.) #https://daattali.com/shiny/timevis-demo/
historyData <- data.frame(
  id = 1:4,
  content = c("Majority of HBEF dataset", 
              "EPA founded", 
              "Clean Air Act", 
              "Today"),
  title = c("Watershed 6 is displayed in this story, as it is the control",
            "The EPA was founded to enforce the Clean Air Act",
            "The Clean Air Act also has important amendments",
            "Today isn't really today"),
  start = c("1963-06-01", 
            "1970-12-02", 
            "1970-06-01", 
            "2017-06-19"), #FIND THE REAL DATE OF CAA ENACTMENT!
  end = c("2014-05-01",
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

shinyServer( function(input, output){
  
  #plot of any compound conc (reactively chosen) over rective time
  output$cTime <- renderPlotly({
    cTime <- ggplot(get(input$selComp), aes(x = as.Date(date)))+
      geom_line(aes(y = concentration_ueq, group = source, color = source))+ 
      labs(colour = "Source", x = "Year", y = "(ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
      ggtitle(as.character(input$selComp), "affected by acid rain")+
      theme(plot.background = element_rect(fill = 'gray', colour = 'gray'))
    #      theme(panel.background = element_rect(fill = 'black'))
    
    ggplotly(cTime)
  })
  
  
  #output an interactive timeline for the history of acid rain
  output$CAAetc <- renderTimevis({
    timevis(historyData) #possibly use groups in order to contextualize (ie disney movie years)
  })
  
})
