# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(grid)

source("scripts/loader.R")
source("scripts/plot.R")

shinyServer(function(input, output) {
  plotHeight <- reactive({
    h <- 600
    tryCatch({
      h <- as.numeric(input$h)
    }, warning=function(w) {
      h <- 600
    }, error=function(e) {
      h <- 600
    })
    return(h)
  })
  
  output$distPlot <- renderPlot({
    p <- createPlot(input, full2014)
    print(p)
  }, height=plotHeight, units="px")
})
