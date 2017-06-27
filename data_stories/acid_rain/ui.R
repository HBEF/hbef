library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(timevis)
library(ggthemes)

shinyUI(
  fluidPage(#theme = "Solar_bootstrap.min.css",
    fluidRow(column(width = 12, offset = 1, h1(strong(em("Acid Rain & HBEF"))))),
    fluidRow(column(width = 10, offset = 1,
                    tags$img(src = "DeadForest.jpg", width = '1270px', height = '650px')),
             column(1)),
    fluidRow(column(width = 8, offset = 1,
                    h3("Experiencing nature for many people means traveling to see awe-inspiring 
                               views and wildlife in national parks or forests. Perhaps the place pictured above used to be one of
                               those nature hubs. These national sites, as well as anywhere in nature, are composed of many diverse 
                               ecosystems that maintain an important balance. 
                               ")),
             column(width = 2,
                    p(strong("Ecosystem:"), "a network of animals, plants, and the physical features 
                              of where they live")),
             column(1)),
    fluidRow(column(width = 11, offset = 1,
                    plotlyOutput("pHtheme", height = "auto", width = "auto"))),
    fluidRow(column(width = 8, offset = 1,
                    h3("Starting in the early 1950s (soon after Disney first released Cinderella 
                               and Peter Pan) this balance within ecosystems everywhere began to tip.  
                               What caused this shift in so many ecosystems?  Well, the weather did 
                               believe it or not.  More specifically, the precipitation that fell 
                               on the ecosystems."),
                    h3('“But don’t plants and animals need the rain and snowmelt to survive” you 
                               ask?  Yes, point for you.  Though the precipitation at this time was 
                               acid rain, and had become polluted to a point of concern.  Many 
                               plants and aquatic creatures specifically were harmed by the increasing 
                               acidity of the water, which began to disrupt the flow of the ecosystems.'),
                    h3('Acid rain hasn’t always been around to harm ecosystems though.  It became 
                               an issue as humans increasingly emitted sulfur dioxide (SO2) and 
                               nitrogen oxides (NOx).  These chemicals came mostly from burning 
                               fossil fuels (namely coal) to produce electricity, and from car 
                               emissions.  They then rise into the atmosphere to react with water, 
                               oxygen, etc. and are carried quite far from where they originated.  
                               When they fall back to earth, in rain, snow, or even fog, it is called acid rain.
                               ')),
             column(3)),
    fluidRow(column(width = 8, offset = 1,
                    #insert widget that links to a quizlet or something here
                    h3("Let's see how much you know offhand about acid rain... Click", 
                       tags$a(href = "https://www.surveymonkey.com/r/RGNNTMH", "here"),
                       "to take the acid rain quiz.")
    ),
    column(3)),
    fluidRow(column(width = 12, offset = 1,
                    h2(strong("Chemistry")))),
    fluidRow(column(width = 8, offset = 1,
                    h3(" Though sulfur dioxide and nitrogen oxides have different effects on their 
                               own, when combined in acid rain they do a number on nature.  One way they harm 
                               ecosystems is by wearing down the natural soil buffer.")),
             column(width = 2, #make a text box
                    p(strong("Soil buffer:"), "chemicals naturally present in the soil, which neutralize the 
                              strong acidity of acid rain at the expense of losing base cations in the 
                              neutralizing reactions")),
             column(1)
    ),
    fluidRow(column(width = 8, offset = 1,
                    h3("The acid rain reacts with the base cations in the soil, causing them to be 
                               washed out of the ecosystem.  Try exploring this pattern using the graph 
                               below.  You can see that calcium (Ca) discharge increases even though the Ca 
                               precipitation remains relatively stable.")),     
             column(width = 2, 
                    p(strong("Base cations:"), "positively charged elements present in the soil that help
                              neutralize acid rain (ie. Ca, Mg, K)")
             ),
             column(1)),
    fluidRow(column(width = 10, offset = 2,
                    #use slider to view data in specific time ranges
                    sliderInput("dateSlide", label = "Input date range",
                                min = as.Date("1963/06/01"), 
                                max = as.Date("2013/06/01"),
                                value = c(as.Date("1963/06/01"), as.Date("2013/06/01")),
                                timeFormat="%b %Y"),
                    selectInput("selComp", label = "Choose a compound to graph",
                                choices = c("Ca" = "CaData", "Mg" = "MgData", "K" = "KData", 
                                            "Na" = "NaData", "SO4" = "SO4Data", "NO3" = "NO3Data", 
                                            "Al" = "AlData", "NH4" = "NH4Data", 
                                            "Cl" = "ClData", "H" = "HData"),
                                selected = "Ca"),
                    #switch between monthly and yearly data
                    selectInput("selDate", label = "Timescale granularity",
                                ###############make an if statement to change x data to avg data when yearly is selected??
                                choices = c("Yearly" = "water_year", "Monthly" = "date"))
    )
    ),
    fluidRow(column(width = 11, offset = 1,
                    plotlyOutput("cTime", height = "auto", width = "100%")
    )),
    fluidRow(column(width = 8, offset = 1,
                    h3("One effect of the base cation loss was the poor growth of Sugar Maples, 
                               which rely heavily on Ca to grow.  Another danger to the ecosystem balance 
                               was caused by acid rain reacting to release aluminum from the soil.  Aluminum 
                               is toxic once released from its stable soil state, and makes it hard for trees 
                               to take up water.  Taking the pH of the precipitation and streamflow also show 
                               these effects of acid rain, because the inflow is acidic when the outflow is 
                               much less so.  Acid is coming in, reacting, and staying.  Sounds like an 
                               unwelcome house guest.")),
             column(3)),
    fluidRow(column(width = 11, offset = 1
    )),
    fluidRow(column(width = 11, offset = 1,
                    h2(strong("History/Policy")))),
    fluidRow(column(width = 8, offset = 1,
                    h3("The hydrologic dataset doesn’t begin until 1963 (which was after the onset 
                               of acid rain) but it still captures the story of an increasing dilemma, 
                               actions taken to mitigate it, and the rebalancing of the ecosystem.  Up 
                               until this time, the United States government was just beginning to fund 
                               research and small policies around air pollution.  In 1967 they began to 
                               expand their monitoring and control, until the enactment of the Clean Air
                               Act in 1970.")),
             column(3)),
    fluidRow(column(width = 11, offset = 1,
                    timevisOutput("CAAetc"))),
    fluidRow(column(width = 8, offset = 1,
                    h3("The Clean Air Act was made to regulate emissions from both stationary sources 
                               (like power plants) and mobile ones (like cars).  The EPA was also founded in
                               1970 in order to enforce the new act.  There have since been amendments, in 
                               1977 and 1990, with the 1990 ones specifically addressing the control of acid
                               rain.  Since then, SO2 emissions have been declining because of mandatory 
                               reductions, though NOx has been tougher to reduce.  Ecosystems aren’t 
                               recovering as quickly as hoped, and efforts continue to both reduce emissions
                               and mitigate ecological distress.")),
             column(3)),
    fluidRow(column(width = 11, offset = 1,
                    tabsetPanel(
                      tabPanel(title = "SO4 & NO3",
                               plotlyOutput("SO4NO3reductions", height = "auto")),
                      tabPanel(title = "Base Cations",
                        plotlyOutput("baseCations", height = "auto")),
                      tabPanel(title = "Al",
                               plotlyOutput("Al", height = "auto")),
                      tabPanel(title = "pH",
                               plotlyOutput("pHPandQ", width = "100%", height = "auto"))
                    ))
    ),
    fluidRow(column(width = 12, offset = 1,
                    h2(strong("Take Action")))),
    fluidRow(column(width = 8, offset = 1,
                    h3("	Feeling stressed about nature?  Slightly overwhelmed?  If you’re a farmer, 
                               you can reduce your nitrogen oxide in a number of ways, like timing the 
                               nitrogen fertilization to crop demand.  For the rest of us?  The overemphasized
                               carpooling, biking, or walking actually does help to reduce both nitrogen oxide
                               and sulfur dioxide emissions from your vehicle.  Even switching over to more 
                               energy efficient lightbulbs and appliances helps, because electricity is 
                               produced in large part by burning fossil fuels.  If you’re feeling super 
                               energized, you could even get a solar panel to produce some of your own 
                               electricity use.  Now there’s a bright idea."),
                    h3("")),
             column(3))
  )
)
