library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggthemes)
library(directlabels)
library(magrittr)
library(timevis)
library(shinydashboard)
library(grid)


########### IMPORTANT LISTS #############


###  Lists for the sidebar  ###
#Edit if there are values that do not appear or are not relevant to your data. 

solutes_cations <- list("Potassium (K)" = "K",
                        "Sodium (Na)" = "Na",
                        "Calcium (Ca)" = "Ca",
                        "Magnesium (Mg)" = "Mg",
                        "Aluminum (Al)" = "Al")

#solutes list specifically for the base cations plot in the policy tab
solutes_base_cations <- list("Potassium (K)" = "K",
                             "Sodium (Na)" = "Na",
                             "Calcium (Ca)" = "Ca",
                             "Magnesium (Mg)" = "Mg")

solutes_anions <- list("Phosphate (PO4)" = "PO4",
                       "Sulfate (SO4)" = "SO4",
                       "Nitrate (NO3)" = "NO3",
                       "Silicon Dioxide (SiO2)" = "SiO2",
                       "Chloride (Cl)" = "Cl",
                       "Bicarbonate (HCO3)" = "HCO3")

#solutes list specifically for the SO4 NO3 graph in the policy tab
solutes_anions3 <- list("Sulfate (SO4)" = "SO4",
                        "Nitrate (NO3)" = "NO3")

#solutes list specifically for the Al graph in the policy tab
solutes_anions_Al <- list("Sulfate (SO4)" = "SO4",
                          "Nitrate (NO3)" = "NO3",
                          "Aluminum (Al)" = "Al")

solutes_H <- list("Hydrogen (H)" = "H")

watersheds <- list("Watershed 1" = "1",
                   "Watershed 2" = "2", 
                   "Watershed 3" = "3",
                   "Watershed 4" = "4",
                   "Watershed 5" = "5",
                   "Watershed 6" = "6",
                   "Watershed 7" = "7",
                   "Watershed 8" = "8",
                   "Watershed 9" = "9")

water_sources <- list("Precipitation (P)" = "precipitation",
                      "Streamflow (Q)" = "streamflow")

granularity <- list("Year (VWC)" = "year",
                    "Month (VWC)" = "month",
                    "Week" = "week")

units <- list("ueq/L","umol/L", "mg/L", "flux")

#######################################################################################
########### APPLICATION UI ############################################################
########################################################################################

shinyUI(
  dashboardPage(skin = "black",
                dashboardHeader(title = tags$a(href="http://hbef.streampulse.org/","HB-WER Viz"), titleWidth = 200),
                dashboardSidebar(
                  width = 200,
                  sidebarMenu(
                    menuItem("Intro", tabName = "intro", icon = icon("tint")),
                    menuItem("pH", tabName = "pH", icon = icon("list-alt")),
                    menuItem("Chemistry", tabName = "chemistry", icon = icon("flask")),
                    menuItem("Policy", tabName = "policy", icon = icon("book")),
                    # footer here
                    tags$div(class = "footer",tags$ul(
                      tags$li(tags$a(href="http://hbef.streampulse.org/#menu", "HOME")),
                      tags$li(tags$a(href="http://hbef.streampulse.org/#datastories","DATA STORIES")),
                      tags$li(tags$a(href="http://hbef.streampulse.org/#exploratory","EXPLORATORY TOOLS")),
                      tags$li(tags$a(href="http://hbef.streampulse.org/#aboutus","ABOUT US")))
                    ))
                  
                ),
                dashboardBody(
                  ########### HEAD - DO NOT EDIT ################################################
                  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
                  tags$head(includeScript(system.file('www', 'ajax.js'))),
                  tags$head(includeScript(system.file('www', 'hubbard.js'))),
                  tags$head(includeScript(system.file('www','google_analytics_1.js'))),
                  tags$head(includeScript(system.file('www','google_analytics_2.js'))),
                  tags$head(includeScript(system.file('www','google_analytics_3.js'))),
                  tags$head(tags$style(HTML(
                    "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
                  ###############################################################################
                  
                  tabItems(
                    
                    ###############################################################################
                    #### ------------  Intro  Tab ----------------------------------------- #######
                    ###############################################################################
                    
                    #If you change this, make sure to change it above in the menuItem
                    tabItem(tabName = "intro",
                            
                            #############################################
                            
                            ########### TEXT ###########
                            
                            #title and text to overlay intro image
                            fluidRow(tags$div(class = "intro-text",
                                              h1("understanding acid rain"),
                                              hr(),
<<<<<<< HEAD
                                              h2(tags$div(HTML(paste("When Gene Likens discovered that rainwater collected in the Hubbard Brook Experimental Forest 
=======
                                              h5(tags$div(HTML(paste("When Gene Likens discovered that rainwater collected in the Hubbard Brook Experimental Forest 
>>>>>>> bd6cb36562dcc3a765368c6100e3967e95e6b34d
                                                 in 1963 had a pH of 3.7, this began a long series of efforts to understand
                                                 why this rain was so acidic, how this acidity compared to other places on earth,
                                                 and what the source of this acidity was. It took more than a decade for researchers
                                                 to establish that this rainwater acidity was not natural (rain in the most remote regions
                                                 of the Earth typically is 100x less acidic than samples collected from Hubbard Brook in 
                                                 the 1960s) and that the source of this acidity in the rain falling in central New Hampshire
                                                 was from the smokestacks of coal fired power plants in the Ohio River valley.
                                                 (read more about this discovery ", a(href = "https://www.caryinstitute.org/science-program/our-scientists/dr-gene-e-likens/acid-rain",
                                                                                    "here"), ")", sep = ""))), 
                                                 p(div(img(src = "acid_rain_us.png", width = "600px", height = "200px"),
                                                                                               style="text-align: center;")),
                                                 div(p(class = "thick","FIGURE 1 – Hydrogen ion concentrations as pH from measurements made by 
                                                 the National Atmospheric Deposition Program in a) 1985 and b) 2015. Note that each 
                                                 dot represents a precipitation monitoring station. 
                                                 Maps for all years available at http://nadp.isws.illinois.edu/maplib"), 
                                                       style = "text-align: left;"),
                                                 p(div(h4(em("Have we fixed the problem of acid rain?")), style = "text-align: left;")),
                                                 p(div("The answer is not everywhere and not quite.", style = "text-align: left;")),
                                                 tags$div(p(class = "thick", "Not Everywhere:"), style = "text-align: left;"), 
                                                 tags$div(HTML(paste("While environmental regulations to reduce the production 
                                                     of volatiles from fossil fuel combustion have significantly
                                                     reduced the problem of acid rain in North American and eastern Europe,
                                                     the problem of acid rain is on the rise in other parts of the world (", 
                                                     a(href = "http://iopscience.iop.org/article/10.1088/1748-9326/8/1/014003/pdf","Klimont et al. 2013"), ") 
                                                     where fossil fuel combustion is increasing without
                                                     corresponding regulatory protections for air quality (Figure 2).", sep = ""))),
                                                 p(div(img(src = "so2_emissions.png", width = "500px", height = "400px"), style = "text-align: center;")),
                                                 div(p(class = "thick", "FIGURE 2. A map of the change in global sulfur dioxide", tags$span(HTML(paste("(SO", tags$sub(2),")", sep = "")), style = "font-weight:bold;" ),
                                                       tags$span(HTML(paste("emissions between 2005 and 2010. Note the decline throughout North American
                                                       and Europe and the widespread increase in southeast Asia (this figure is from
                                                       Klimont et al. 2013 and is available at this",
                                                       a(href = "https://www.researchgate.net/profile/Janusz_Cofala/publication/255812633/figure/fig3/AS:297935989428229@1448044883976/Figure-3-Change-in-regional-distribution-of-anthropogenic-land-based-SO-2-emissions.png",
                                                         " link"), ".", sep = "")), style = "font-weight:bold;"),
                                                     style = "text-align: left;")),
                                                 div(p(class = "thick", "Not Quite:"), style = "text-align: left;"),
                                                 "Even in the places, like Hubbard Brook, where the acidity of rain has returned to a more natural range (pH ~ 5, Figure 1B),
                                                 the forests and the streams have been altered by decades of acid rain. 
                                                 As sulfuric acid rain fell onto soils, it acidified the soil and leached important
                                                 plant nutrients such as calcium and magnesium and toxic metals (such as Al) into receiving 
                                                 streams. Over time, these important nutrients and trace elements have been depleted from
                                                 the soil, limiting forest growth and contributing to toxicity issues in affected rivers
                                                 and lakes. It will take many decades for rock weathering to replenish the supply of these
                                                 critical elements for forest growth and aquatic food webs. You can see what happens when
                                                 these acid rain impacted forests were experimentally fertilized with calcium by going to the",
                                                 a(href = "http://hbef.streampulse.org:3838/data_stories/calcium_exp/", "Calcium Experiment data story"),
                                                 "on our webpage. There is encouraging news from a recent study by Lawrence et al.",
                                                 p(div(p(class = "thick", "What to look for in this data story"), style = "text-align: left;")),
                                                 p(div("You can see the change over time in the pH and sulfur content of rain and the corresponding changes
                                                       in the chemistry of receiving streams. As soil nutrients were depleted, 
                                                       fewer and fewer soil minerals were available to be leached into streams 
                                                       and thus  streamwater grew increasingly dilute over time.", style = "text-align: left;")),
                                                 p(tags$div(HTML(paste("Was there an effect of the Clean Air Act of 1970 or the Clean Air Act
                                                       Amendments of 1990 on the amount of H", tags$sup("+"), " or SO", tags$sub(4), " (the constituents of
                                                       sulfuric acid) in precipitation?", sep = "")), style = "text-align: left;")),
                                                 p(div("What is more acidic, rain or stream water? How have both changed over time?",
                                                       style = "text-align: left;")),
                                                 p(div("How much calcium and magnesium enters these forests in rain? How much is
                                                       leaving in streams? How has this changed over time?", style = "text-align: left;")),
                                                 p(div(h4(p(class = "thick","RESOURCES TO LEARN MORE")), 
                                                       style = "text-align: left;")),
                                                 p(em("Free internet resources")),
                                                 p(div("From the Hubbard Brook Research Foundation,",
                                                   a(href = "https://www.caryinstitute.org/sites/default/files/public/downloads/page/AcidRain_revisited.pdf",
                                                     "Acid Rain Revisted: Advances in scientific understanding since the 
                                                     passage of the 1970 and 1990 Clean Air Act Amendments"), style = "text-align: left;")),
                                                 p(div("From The Nature Conservancy and the Institute for Ecosystem Studies,",
                                                   a(href = "https://www.caryinstitute.org/sites/default/files/public/downloads/page/AcidRain_threats_from_above.pdf",
                                                     "Threats from above: Air pollution impacts on Ecosystems and 
                                                     Biological Diversity in the Eastern United States"), style = "text-align: left;")),
                                                 p(div("From US EPA,", 
                                                   a(href = "https://www.epa.gov/acidrain/what-acid-rain", "What is Acid Rain?"),
                                                   style = "text-align: left;")),
                                                 p(div("From US EPA,", 
                                                   a(href = "https://www3.epa.gov/acidrain/education/teachersguide.pdf",
                                                     "Learning About Acid Rain: a teacher’s guide for grades 6-8"), 
                                                   style = "text-align: left;")),
                                                 p(em("Books")),
                                                 p(tags$div("Holmes, R. T., & Likens, G. E. (2016).", a(href = "https://www.amazon.com/Hubbard-Brook-Story-Forest-Ecosystem/dp/0300203640",
                                                         em(span("Hubbard Brook: The story of a forest ecosystem.", style = "text-decoration:underline;"))),
                                                       "New Haven: Yale University Press.", style = "text-align: left;")),
                                                 p(em("Key Papers")),
                                                 p(div("Likens, G.E., R.F. Wright, J.N. Galloway, and T.J. Butler.",
                                                       a(href = "http://www.jstor.org/stable/pdf/24965312.pdf?refreqid=excelsior%3A647a0616162fb49242ddcc984ce38c2a&seq=1#page_scan_tab_contents",
                                                         span("Acid Rain", style = "text-decoration:underline;")), em("Scientific American 241:"), "43-51"),
                                                   style = "text-align: left;"),
                                                 p(div("Likens, G.E., C.T. Driscoll and D.C. Buso. 1996.",
                                                       a(href = "http://www.esf.edu/efb/mitchell/Class%20Readings/Sci.272.244.246.pdf",
                                                         span("Long-term effects of Acid Rain: Response and Recovery of a Forest Ecosystem.",
                                                              style = "text-decoration:underline;")),
                                                       em("Science 272"),"244-246", style = "text-align: left;")),
                                                 p(div("Stoddard, J.L. et al. 1999.",
                                                       a(href = "https://www.researchgate.net/publication/228803203_Regional_trends_in_aquatic_recovery_from_acidification_in_North_America_and_Europe",
                                                         span("Regional trends in aquatic recovery from acidification in North American and Europe.", 
                                                              style = "text-decoration:underline;")), em("Nature 401:"), "575-578",
                                                       style = "text-align: left;"))
                                                 )
                                              ))
                            
                            ########### END ###########
                    ),# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------ End of Intro Tab ------------------------------------ #######
                    ###############################################################################  
                    
                    
                    
                    ###############################################################################
                    #### ------------  pH  Tab -------------------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "pH",
                            
                            ########### TITLE ####################
                            fluidRow(column(9,tags$h1("How does pH change when acid rain is mitigated?"))
                            ),
                            
                            #############################################
                            
                            ########### GRAPH FOR QUESTION #1 ##########
                            
                            fluidRow(
                              column(9,
                                     tabBox(width = 12, height = "600px", side="right", selected = shiny::icon("circle"), 
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log1", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(5, tags$h2(" ")),
                                                                                      ##Granularity
                                                                                      column(3,  offset = 4, selectInput("granularity1", label = "",
                                                                                                                         choices = granularity,
                                                                                                                         selected = "year")))
                                                     ),
                                                     ## pH plot over time
                                                     plotlyOutput("pH")
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "600px", id = "sidebar",
                                         ##Water Sources
                                         fluidRow(
                                           column(12, checkboxGroupInput("water_sources1", label = h4("Select Water Sources"),
                                                                         choices = water_sources,
                                                                         selected = "precipitation"))),
                                         
                                         ##Date Range
                                         fluidRow(
                                           sliderInput("date_range_pH", label = h4("Select Date Range"),
                                                       min = as.Date("1962-01-01"),
                                                       max = as.Date("2014-01-01"),
                                                       value = c(as.Date("1962-01-01"), as.Date("2013-01-01")), timeFormat = "%b %Y")))
                                     
                              )#Closes the column
                              
                            )#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
                            
                    ), # Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of pH Tab ---------------------------- #######
                    ###############################################################################                     
                    
                    
                    ###############################################################################
                    #### ------------  Chemistry  Tab ---------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "chemistry",
                            
                            ########### TITLE ####################
                            fluidRow(column(9,tags$h1("How does acid rain affect solute concentrations?"))
                            ),
                            
                            #############################################
                            
                            ########### GRAPH FOR QUESTION #1 ##########
                            
                            fluidRow(
                              column(9,
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log2", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(5, tags$h2(" ")),
                                                                                      ##Granularity
                                                                                      column(3,  offset = 4, selectInput("granularity2", label = "",
                                                                                                                         choices = granularity,
                                                                                                                         selected = "year")))),
                                                     ## plot of solute conc in streamflow and precipitation over time in watershed 6
                                                     fluidRow(plotlyOutput("chemistry"))
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         #Solutes
                                         fluidRow(
                                           actionLink("select_all_ions", h4("Select Solutes")),
                                           div(class = "scrollbox",
                                               #Cations
                                               actionLink("select_all_cations", h5("Cations")),
                                               checkboxGroupInput("solutes_cations", label = p(""),
                                                                  choices = solutes_cations,
                                                                  selected = "Ca"),
                                               
                                               #Hydrogen  
                                               checkboxGroupInput("solutes_H", label = h4(""),
                                                                  choices = solutes_H,
                                                                  selected = ""),
                                               
                                               #Anions
                                               actionLink("select_all_anions", h5("Anions")),
                                               checkboxGroupInput("solutes_anions", label = p(""),
                                                                  choices = solutes_anions,
                                                                  selected = ""))),
                                         
                                         
                                         ##Water Sources
                                         fluidRow(
                                           column(12, checkboxGroupInput("water_sources2", label = h4("Select Water Sources"),
                                                                         choices = water_sources,
                                                                         selected = c("precipitation", "streamflow")))),
                                         
                                         ##Units  
                                         fluidRow(
                                           column(12, selectInput("units2", label = h4("Select Units"),
                                                                  choices = units,
                                                                  selected = "mg/L"))),
                                         
                                         ##Date Range
                                         sliderInput("date_range_chem", label = h4("Select Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1965-01-01"), as.Date("2013-01-01")),
                                                     timeFormat = "%b %Y"))
                                     
                              )#Closes the column
                              
                            )#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            
                            ########### TEXT FOR QUESTION #1 ##########
                            
        
                            
                            ########### END OF QUESTION #1 ##########
                    ),# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of Chemistry Tab ------------------------------- #######
                    ###############################################################################  
                    
                    
                    ###############################################################################
                    #### ------------  Policy Tab  ---------------------------------------- #######
                    ###############################################################################
                    
                    tabItem(tabName = "policy",
                            
                            ########### TITLE ####################
                            fluidRow(column(9, tags$h1("How have policies altered the effects of acid rain? (As seen in ws6)")) 
                            ),
                            
                            #############################################
                            
                            ####Policy Timeline#####
                            tags$div(class = "container_question",
                                fluidRow(column(9, h4("Acid Rain Key Events Timeline"))),
                                fluidRow(column(9, timevisOutput("timeline")))),
                            
                            
                            ########### GRAPH FOR QUESTION #1 ##########
                            
                            fluidRow(
                              column(9,
                                     #------ Box 1 --------#
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log3", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Decrease in SO4 and NO3")),
                                                                                      ##Granularity
                                                                                      column(3, selectInput("granularity3", label = "",
                                                                                                            choices = granularity,
                                                                                                            selected = "year")))),
                                                     #Solutes
                                                     fluidRow(column(12, checkboxGroupInput("solutes_anions3", label = "",
                                                                                            choices = solutes_anions3,
                                                                                            selected = c("SO4", "NO3"), 
                                                                                            inline = TRUE))),
                                                     
                                                     ## Time Plot
                                                     fluidRow(column(12,plotlyOutput("policy_SO4_NO3")))
                                            ) #Closes tabpanel
                                            
                                     ),# Closes tab Box
                                     
                                     #------ End of Box 1 --------#
                                     
                                     #------ Box 2 --------#
                                     
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log4", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Decrease in loss of base cations")),
                                                                                      ##Granularity
                                                                                      column(3, selectInput("granularity4", label = "",
                                                                                                            choices = granularity, selected = "year")))),
                                                     #Solutes
                                                     fluidRow(checkboxGroupInput("solutes_cations3", label = "",
                                                                                 choices = solutes_base_cations,
                                                                                 selected = c("K", "Na", "Ca", "Mg"), 
                                                                                 inline = TRUE)), 
                                                     ## Time Plot
                                                     fluidRow(plotlyOutput("policy_base_cations"))
                                            ) #Closes tabpanel
                                            
                                     ), # Closes tab Box
                                     
                                     #------ End of Box 2 --------#
                                     
                                     #------ Box 3 --------#
                                     
                                     tabBox(width = 12, height = "700px", side="right", selected = shiny::icon("circle"),
                                            ######## OPTIONS
                                            ###Units - Axis Log
                                            tabPanel(shiny::icon("gear"),
                                                     fluidRow(
                                                       column(6, offset = 6, box(width = 12, title = "X and Y", collapsible = TRUE, collapsed = FALSE, 
                                                                                 
                                                                                 ##Units - Y Axis Log
                                                                                 column(5, selectInput("log5", label = "Y Axis",
                                                                                                       choices = c("linear", "log"), 
                                                                                                       selected = "linear")))))),
                                            ######## PLOT 
                                            tabPanel(shiny::icon("circle"),
                                                     div(class = "titleRow", fluidRow(column(9, tags$h2("Decrease in toxic Al streamflow")),
                                                                                      ##Granularity
                                                                                      column(3, selectInput("granularity5", label = "",
                                                                                                            choices = granularity,
                                                                                                            selected = "year")))),
                                                     #Solutes
                                                     fluidRow(column(9, offset = 6, checkboxGroupInput("solutes_anions_Al", label = "",
                                                                                                       choices = solutes_anions_Al,
                                                                                                       selected = c("Al"), 
                                                                                                       inline = TRUE))),
                                                     ## Time Plot
                                                     fluidRow(plotlyOutput("policy_Al"))
                                            ) #Closes tabpanel
                                            
                                     )# Closes tab Box
                                     
                                     #------ End of Box 3 --------#
                                     
                              ), #Closes the column
                              
                              ######## SIDEBAR
                              column(3, 
                                     box(width = 13, height = "700px", id = "sidebar",
                                         ##Water Sources
                                         fluidRow(
                                           column(12, checkboxGroupInput("water_sources3", label = h4("Water Sources"),
                                                                         choices = water_sources,
                                                                         selected = c("precipitation", "streamflow")))),
                                         
                                         ##Units  
                                         fluidRow(
                                           column(12, selectInput("units3", label = h4("Units"),
                                                                  choices = units,
                                                                  selected = "mg/L"))),
                                         
                                         ##Date Range
                                         sliderInput("date_range_policy", label = h4("Date Range"),
                                                     min = as.Date("1962-01-01"),
                                                     max = as.Date("2014-01-01"),
                                                     value = c(as.Date("1965-01-01"), as.Date("2013-01-01")),
                                                     timeFormat = "%b %Y"))
                                     
                              )#Closes the column
                              
                            )#Closes graph row
                            
                            ########### END OF GRAPH FOR QUESTION #1 ##########
                            
                            ########### END OF QUESTION #1 ##########
                    )# Closes Intro Tab
                    
                    ###############################################################################
                    #### ------------  End of Policy Tab ------------------------------- #######
                    ###############################################################################    
                    
                    
                    
                  )# Closes Tabset Panel for Main Tabs
                )#Closes Dashboard Body
  )#Closes Dashboard Page
) #closes ShinyUI


