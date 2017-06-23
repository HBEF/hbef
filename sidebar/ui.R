library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)

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
solutes_H <- list("Hydrogen (H)" = "H",
                  "pH" = "pH")

watersheds <- list("Watershed 1" = "ws1",
                   "Watershed 2" = "ws2", 
                   "Watershed 3" = "ws3",
                   "Watershed 4" = "ws4",
                   "Watershed 5" = "ws5",
                   "Watershed 6" = "ws6",
                   "Watershed 7" = "ws7",
                   "Watershed 8" = "ws8",
                   "Watershed 9" = "ws9")

watersheds2 <- list("1" = "ws1",
                   "2" = "ws2", 
                   "3" = "ws3",
                   "4" = "ws4",
                   "5" = "ws5",
                   "6" = "ws6",
                   "7" = "ws7",
                   "8" = "ws8",
                   "9" = "ws9")

water_sources <- list("Precipitation (P)" = "precip",
                     "Discharge (Q)" = "flow")

granularity <- list("Year" = "year",
                    "Month" = "month",
                    "Week" = "week")

units <- list("uEquivalent/L","uMole/L", "uMg/L", "flux")

  # Application title
shinyUI(fluidPage(theme = "hubbard.css",
  tags$head(includeScript(system.file('www', 'ajax.js'))),
  tags$head(includeScript(system.file('www', 'hubbard.js'))),
  tags$head(tags$style(HTML(
    "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
  
  fluidRow(
    tags$h3("What happens to discharge when the entire forest is cut?")
  ),
  
  fluidRow(
    
    sidebarLayout(
      ### Side Bar
      sidebarPanel(
        
        #Solutes
        fluidRow(
          column(12, actionLink("select_all_ions", h4("Ions"))),
         
          #Cations
           column(6,
                    actionLink("select_all_cations", h5("Cations")),
                    checkboxGroupInput("solutes_cations", label = "",
                    choices = solutes_cations,
                    selected = "Sodium (Na)")),
          
          #Anions
          
          column(6, actionLink("select_all_anions", h5("Anions")),
                    checkboxGroupInput("solutes_anions", label = "",
                    choices = solutes_anions,
                    selected = "Sulfate (SO4)"))),
          #Hydrogen  
        
        fluidRow(
          column(12, checkboxGroupInput("solutes_H", label = h4(""),
                                       choices = solutes_H,
                                       selected = ""))),
        
        #Watersheds
        fluidRow(
          column(12, actionLink("select_all_ws", h4("Watersheds")), 
                 selectInput("watersheds", label = "",
                                       choices = watersheds, multiple = TRUE,
                                       selected = "ws6"))),
        
        #Water Sources
        fluidRow(
          column(12, checkboxGroupInput("water_sources", label = h4("Water Sources"),
                                        choices = water_sources,
                                        selected = "precip",
                                        inline = TRUE))),
        
        #Units  
        fluidRow(
          column(12, selectInput("units", label = h4("Units"),
                    choices = units,
                    selected = "mm")),
          column(12, checkboxGroupInput("log", label = h4(""),
                     choices = "ln"))),
        #Granularity
        fluidRow(
          column(12, selectInput("granularity", label = h4("Granularity"),
                    choices = granularity,
                    selected = "year"))),
        
        #Date Range
        sliderInput("date_range", label = h4("Date Range"),
                    min = as.Date("1962-01-01"),
                    max = as.Date("2014-01-01"),
                    value = c(as.Date("1965-01-01"), as.Date("2013-01-01")))),
      
      mainPanel(plotlyOutput("d.plot")), 
      position = "right"
    )
  ),
  
    fluidRow(
      tags$p("Deforestation, the removal of forest trees,is harmful to the environment for a number of reasons, 
            some of which are less obvious than others.
             Deforestation results in habitat loss for woodland-
             dwelling species, causing die-offs and concurrent declines
             in biodiversity. Around eighty percent of land animals
             and plants on Earth reside in forests (National Geographic Society
             2017). It follows that the impact of widespread deforestation
             on wildlife is not insignificant. Deforestation also 
             accelerates climate change, as the loss of forests
             that absorb carbon dioxide tips the balance 
             so that more of this greenhouse gas enters 
             that atmosphere, causing global warming. On
             the smaller scale, deforestation can trigger
             regional climate change because the ground 
             cover from the trees is eliminated, allowing sun
             rays to penetrate where they were previously 
             blocked. This causes soils to dry out, which can
             transform once forested land into deserts. 
             Without canopy cover to block sunlight during
             the day and retain heat during the night, 
             temperature fluctuations become more severe
             and harmful to wildlife. Deforestation also
             leads to drier climates because less water 
             is transpired, or released into the air, by
             trees. This negatively impacts the water cycle
             (National Geographic Society 2017)."),
      tags$p("Using our data set, one can visualize two more
             consequences of deforestation- leaching 
             of nutrients from the environment and 
             increases in water runoff.  Trees prevent 
             erosion by fixing soil in place with their
             roots and creating natural dams with fallen
             leaves and branches. When trees are removed,
             erosion increases, carrying away the nutrients
             in the soil. At the same time, deforestation 
             inhibits water uptake by trees, resulting in
             heightened runoff and more water moving through
             the soil, dissolving and washing away more 
             nutrients. Thus, the concentration of nutrients
             in streamwater rises following 
             deforestation (Likens et al. 1970)."),
      tags$p("At Hubbard Brook, an experiment was conducted 
             in November and December 1965 in which all the
             trees in Watershed 2 were cut, left in place,
             and limbed so that no branches were more that 
             1.5 meters high. Herbicide was applied 
             periodically to Watershed 2 for two years 
             after the deforestation event, so that forest
             regrowth was prevented. Chemical and quantity
             measurements of precipitation and streamwater
             discharge were recorded. At the same time, these
             measurements were taken at other Hubbard Brook 
             watersheds, particullarly Watershed 6, which 
             was left undisturbed (Likens et al. 1970). 
             These graphs compare data from the deforested
             Watershed 2 and the undisturbed Watershed 6, 
             so you can explore the effects of deforestation yourself.")
    ),
    
    
    fluidRow(
      tags$h3("What happens to the solutes when the entire forest is cut?")
    ),
    
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("solute", label = h4("Solute"),
                    choices = list("Sodium (Na)" = "Na",
                                   "Calcium (Ca)" = "Ca",
                                   "Magnesium (Mg)" = "Mg",
                                   "Potassium (K)" = "K",
                                   "Sulfate (SO4)" = "SO4",
                                   "Nitrate (NO3)" = "NO3",
                                   "Chlorine (Cl)" = "Cl",
                                   "Hydrogen Ion (H)" = "H"),
                    selected = "Na"),
        
        selectInput("units", label = h4("Units"),
                    choices = list("Eq/ha-yr" = "Eq/ha-yr",
                                   "ueq/L" = "ueq/L",
                                   "ln(ueq/L)" = "ln(ueq/L)",
                                   "ln(Eq/ha-yr)" = "ln(Eq/ha-yr)"),
                    selected = "ueq/L"),
        selectInput("scale", label = h4("Time Scale"),
                    choices = list("By month" = "month",
                                   "By year" = "year"),
                    selected = "year"),
        selectInput("p", label = h4("Adding Precipitation"),
                    choices = list("Without Precipitation" = "noprecip",
                                   "With Precipitation" = "precip"),
                    selected = "noprecip"),
        sliderInput("dates", label = h4("Date Range"),
                    min = as.Date("1962-01-01"),
                    max = as.Date("2014-01-01"),
                    value = c(as.Date("1965-01-01"), as.Date("1971-01-01")))),
      
      mainPanel(plotlyOutput("s.plot", width = 500, height = 250)), 
      position = "right"
    )
  )
  
  
    
    
))

  