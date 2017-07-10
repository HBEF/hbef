library(shiny)

shinyServer(function(session, input, output) {
  
  x <- as.Date(c("1963-06-01", "1964-06-01", "1965-06-01","1966-06-01"))
  y <- c(1162.7, 975.4, 1280.3, 1380.0)
  data<- data.frame(x, y)

  output$plot1 <- renderPlot(ggplot(data = data, aes(x = x, y = y)) +
  geom_bar(stat = "identity"))

  output$plot2 <- renderPlotly(ggplotly(ggplot(data = data, aes(x = x, y = y)) +
           geom_bar(stat = "identity")))
           
           })
  
  