library(shiny)
library(plotly)
library(ggplot2)


shinyUI(fluidPage(
  plotOutput("plot1"),
  plotlyOutput("plot2")
))