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

shinyServer(function(input, output, session) {
  # Get the user-specified height of the plot
  plotHeight <- reactive({
    h <- tryFail(as.numeric(input$h), 600)
    updateTextInput(session, "pheight", value=as.character(h))
    h
  })
  
  
  output$distPlot <- renderPlot({
    p <- createPlot(input, full2014)
    print(p)
  }, height=plotHeight, units="px")
  
  output$saveImage <- downloadHandler(
    filename = input$fname,
    content = function(file) {
      device <- function(..., width, height) {
        pw <- tryFail(as.numeric(input$pwidth), 1200)
        ph <- tryFail(as.numeric(input$pheight), 600)
        grDevices::png(..., width = pw, height = ph, units = "px")
      }
      ggsave(file, plot=createPlot(input, full2014), device=device)  
    })
})
