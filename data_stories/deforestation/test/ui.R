library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(dygraphs)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggiraph)

solutes_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Aluminum (Al)" = "Al")

solutes_anions <- list("Sulfate (SO4)" = "SO4",
                       "Nitrate (NO3)" = "NO3",
                       "Chloride (Cl)" = "Cl")
solutes_H <- c("Hydrogen (H)" = "H")

  # Application title
shinyUI(fluidPage(
  titlePanel("Effects of Deforestation on Streamwater"),
  
  tabsetPanel(id = "top", type = "pills",
    tabPanel("Solute Concentrations",
             fluidRow( 
               tags$h3("What effect does deforestation have on 
                       solute concentrations in streamwater?")
               ),
             sidebarLayout(position = "right",
               sidebarPanel(
                 #Solutes
                 fluidRow(
                   column(12, actionLink("select_all_ions", h4("Solutes"))),
                   
                   #Cations
                   column(6,
                          actionLink("select_all_cations", h5("Cations")),
                          checkboxGroupInput("solutes_cations", label = "",
                                             choices = solutes_cations,
                                             selected = "Na")),
                   
                   #Anions
                   
                   column(6, actionLink("select_all_anions", h5("Anions")),
                          checkboxGroupInput("solutes_anions", label = "",
                                             choices = solutes_anions,
                                             selected = "SO4)"))),
                 #Hydrogen  
                 
                 fluidRow(
                   column(12, checkboxGroupInput("solutes_H", 
                                                 label = h5("Hydrogen"),
                                                 choices = solutes_H,
                                                 selected = ""))),
                 fluidRow(column(12,
                 selectInput("units", label = h3("Units"),
                             choices = list("Eq/ha-yr" = "Eq/ha-yr",
                                            "ueq/L" = "ueq/L",
                                            "mg/L" = "mg/L",
                                            "umol/L" = "umol/L"),
                             selected = "ueq/L"))),
                 #checkbox input for selecting whether to apply the logarithm
                 fluidRow(column(12,
                 h3("Applying the Natural Logarithm"),
                 checkboxInput("ln", label = "Natural Log",
                               value = FALSE))),
                 fluidRow(column(12,
                 selectInput("granularity", label = h3("Granularity"),
                             choices = list("By month" = "month",
                                            "By year" = "year"),
                             selected = "year"))),
                 fluidRow(column(12,
                 selectInput("p", label = h3("Adding Precipitation"),
                             choices = list("Without Precipitation" = "noprecip",
                                            "With Precipitation" = "precip"),
                             selected = "noprecip"))),
                 fluidRow(column(12, 
                 sliderInput("dates", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1985-01-01")))))),
               mainPanel(plotlyOutput("s.plot", width = "100%", height = "100%"))),
               fluidRow(column(10, offset = 1,
                         p("*The black horizontal lines on the graph represent the 
                           starting date of deforestation at the different watersheds."),
                         h4("Consequences of Deforestation"),
                         p("Deforestation, the removal of forest trees,
                           is harmful to the environment for a number of 
                           reasons, some of which are less obvious than others.
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
                         p("Using our data set, one can visualize two more
                           consequences of deforestation- leaching 
                           of nutrients from the environment and 
                           increases in water runoff. Deforestation 
                           inhibits water uptake by trees, resulting in
                           heightened runoff and more water moving through
                           the soil, increasingly dissolving and washing away 
                           nutrients. Thus, the concentration of nutrients
                           in streamwater rises following extensive
                           deforestation, especially when the prevailing 
                           conditions prevent regrowth (Likens et al. 1970)."),
                         p("At Hubbard Brook, several deforestation
                           experiments were implemented at different times.  
                           One such experiment was conducted 
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
                           was left undisturbed (Likens et al. 1970). In
                           a separate experiment, Watershed 4 was divided 
                           into 49 strips directed from east to west, each 
                           25 meters wide. This area was then strip cut according
                           to the following timeline: in the fall of 1970, every 
                           third strip was deforested, then in 1972, the second 
                           set of strips were cut, and finally the last series of strips 
                           were cut in 1974. Timber of value was removed by logging 
                           companies, and the watershed was left to regrow.
                           In a third experiment, Watershed 5 was clear-cut, the 
                           trees removed, and the watershed was allowed to regrow
                           (HB Watersheds Tour 2017). Precipitation and discharge data
                           were taken for all experimental watersheds.
                           These graphs compare data from the deforested
                           Watersheds 2, 4, and 5, and the undisturbed Watershed 6, 
                           so you can explore the effects of deforestation yourself."),
                         h4("References"),
                         p("\"Deforestation and Its Effect on the Planet.\"", 
                           em("National Geographic."), 
                           "National Geographic Society, 24 May 2017. Web. 16 June 2017.
                           <http://www.nationalgeographic.com/environment/global-warming/deforestation/>."),
                         p("\"HB Watersheds Tour.\"", em("Hubbard Brook."),
                           "Hubbard Brook Experimental Forest, n.d. Web. 
                           26 June 2017. 
                           <http://www.hubbardbrook.org/6-12_education/Introduction/Intro9C.htm>."),
                         p("Likens, Gene E., F. Herbert Bormann, Noye M. Johnson, D. W. Fisher,
                           and Robert S. Pierce. \"Effects of Forest Cutting and Herbicide Treatment 
                           on Nutrient Budgets in the Hubbard Brook Watershed-Ecosystem.\"", 
                           em("Ecological Monographs")," 40.1 (1970): 23-47. Web. 16 June 2017.")
          
                         ), column(2))
             ), #end of tabPanel
    tabPanel("Discharge and Precipitation Quantities",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(12,
                 selectInput("granularity2", label = h3("Time Scale"),
                             choices = list("By month" = "month",
                                            "By year" = "year"),
                             selected = "year"))),
                 #checkbox input for selecting whether to apply the logarithm
                 fluidRow(column(12, 
                 h3("Applying the Natural Logarithm"),
                 checkboxInput("ln.dis", label = "Natural Log",
                               value = FALSE))),
                 fluidRow(column(12, 
                 selectInput("p.dis", label = h3("Adding Precipitation"),
                             choices = list("Without Precipitation" = "noprecip",
                                            "With Precipitation" = "precip"),
                             selected = "noprecip"))),
                 fluidRow(column(12, 
                 sliderInput("dates.dis", label = h3("Date Range"),
                             min = as.Date("1962-01-01"),
                             max = as.Date("2014-01-01"),
                             value = c(as.Date("1965-01-01"), as.Date("1985-01-01")))))),
               
               mainPanel(plotlyOutput("d.plot", width = "100%", height = "100%"))),
               fluidRow(column(10, offset = 1,
                         p("*The black horizontal lines on the graph represent the 
                           starting date of deforestation at the different watersheds."),
                         h4("Consequences of Deforestation"),
                         p("Deforestation, the removal of forest trees,
                           is harmful to the environment for a number of 
                           reasons, some of which are less obvious than others.
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
                         p("Using our data set, one can visualize two more
                           consequences of deforestation- leaching 
                           of nutrients from the environment and 
                           increases in water runoff. Deforestation 
                           inhibits water uptake by trees, resulting in
                           heightened runoff and more water moving through
                           the soil, increasingly dissolving and washing away 
                           nutrients. Thus, the concentration of nutrients
                           in streamwater rises following extensive
                           deforestation, especially when the prevailing 
                           conditions prevent regrowth (Likens et al. 1970)."),
                         p("At Hubbard Brook, several deforestation
                           experiments were implemented at different times.  
                           One such experiment was conducted 
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
                           was left undisturbed (Likens et al. 1970). In
                           a separate experiment, Watershed 4 was divided 
                           into 49 strips directed from east to west, each 
                           25 meters wide. This area was then strip cut according
                           to the following timeline: in the fall of 1970, every 
                           third strip was deforested, then in 1972, the second 
                           set of strips were cut, and finally the last series of strips 
                           were cut in 1974. Timber of value was removed by logging 
                           companies, and the watershed was left to regrow.
                           In a third experiment, Watershed 5 was clear-cut, the 
                           trees removed, and the watershed was allowed to regrow
                           (HB Watersheds Tour 2017). Precipitation and discharge data
                           were taken for all experimental watersheds.
                           These graphs compare data from the deforested
                           Watersheds 2, 4, and 5, and the undisturbed Watershed 6, 
                           so you can explore the effects of deforestation yourself."),
                         h4("References"),
                         p("\"Deforestation and Its Effect on the Planet.\"", 
                           em("National Geographic."), 
                           "National Geographic Society, 24 May 2017. Web. 16 June 2017.
                           <http://www.nationalgeographic.com/environment/global-warming/deforestation/>."),
                         p("\"HB Watersheds Tour.\"", em("Hubbard Brook."),
                          "Hubbard Brook Experimental Forest, n.d. Web. 
                           26 June 2017. 
                          <http://www.hubbardbrook.org/6-12_education/Introduction/Intro9C.htm>."),
                         p("Likens, Gene E., F. Herbert Bormann, Noye M. Johnson, D. W. Fisher, 
                           and Robert S. Pierce. \"Effects of Forest Cutting and Herbicide Treatment 
                           on Nutrient Budgets in the Hubbard Brook Watershed-Ecosystem.\"", 
                           em("Ecological Monographs")," 40.1 (1970): 23-47. Web. 16 June 2017.")),
                        column(2))
             
             
             
    ))
 )#end of fluidPage
)#end of ShinyUI 
  