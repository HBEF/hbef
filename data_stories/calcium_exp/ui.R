library(plotly)
library(ggplot2)

shinyUI(fluidPage(
  tabsetPanel(id = "top", type = "pills",
    tabPanel("Biomass Maps",
             fluidRow(
               tags$div(class = "container_question", 
                        tags$h3("What happens to biomass when calcium is added 
                                to the environment?"))
             ),
             fluidRow(
               sidebarLayout(position = "right",
                 sidebarPanel(
                   fluidRow(
                     column(12, selectInput("species", 
                                            label = h4("Total Biomass or By Species"),
                                            choices = list("Total Biomass" = "none",
                                                           "Sugar maple" = "ACSA",
                                                           "American beech" = "FAGR",
                                                           "Yellow birch" = "BEAL",
                                                           "White ash" = "FRAM",
                                                           "Mountain maple" = "ACSP",
                                                           "Striped maple/Moose wood"
                                                           = "ACPE",
                                                           "Pin/fire cherry" = "PRPE",
                                                           "Choke cherry" = "PRVI",
                                                           "Balsam fir" = "ABBA",
                                                           "Red spruce" = "PIRU",
                                                           "White/paper birch" = "BEPA",
                                                           "Mountain ash" = "SOAM",
                                                           "Red maple" = "ACRU",
                                                           "Eastern hemlock" = "TSCA",
                                                           "Quaking aspen" = "POTR",
                                                           "Black cherry" = "PRSE",
                                                           "Shadbush" = "AMSP",
                                                           "Big-tooth aspen" = "POGR",
                                                           "Willow" = "SASP",
                                                           "Alternate-leaved dogwood" =
                                                             "COAL",
                                                           "Cherry (unspecified)" =
                                                             "PRSP",
                                                           "Red elderberry" = "SARA"),
                                            selected = "ACSA"))
                            
                            )
                     
                   ),
                 mainPanel(plotlyOutput("map.plot", width = "100%", height = "100%"))
                 )
               )
             )
    )   
              
              
              
              
  
  
))