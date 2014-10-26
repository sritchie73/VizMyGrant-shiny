# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source("loader.R")
source("main-plot.R")

shinyServer(function(input, output) {
  
  getPlotHeight <- function() {
    y.var <- input$y.cat
    y.type <- getType(y.var)
    y.query <- getDTQ(y.var)
    y.table <- getTable(y.var)
    
    min_height <- 400
    if (y.type == "categorical") {
      x.var <- input$x.cat
      x.type <- getType(x.var)
      x.query <- getDTQ(x.var)
      x.table <- getTable(x.var)
      
      filter <- input$filter
      f.type <- getType(filter)
      f.query <- getDTQ(filter)
      f.table <- getTable(filter)
      
      # Join the tables for x and y if necessary
      if (x.table == y.table) {
        eval(parse(
          text=paste0("joined <- ", x.table)
        ))
      } else {
        stop("not implemented yet!")
      }
      
      # Subset the data based on the filter
      if (!is.na(f.type)) {
        eval(parse(
          text=paste0("subset <- joined[", f.query, "]")
        ))
      } else {
        subset <- joined
      }
      
      # Get the Y categories
      eval(parse(
        text = paste0("ycats <- subset[, unique(", y.query, ")]")
      ))
      ycats <- ycats[!is.na(ycats)]
      nycats <- length(ycats)
      
      dyn_height <- nycats * 30
      return(max(min_height, dyn_height))
    } else {
      return(min_height)
    }
  }
  
  output$distPlot <- renderPlot({
    plotData(input)
  },  height=getPlotHeight, units="px")
  
  output$Height <- renderText(getPlotHeight)
})
