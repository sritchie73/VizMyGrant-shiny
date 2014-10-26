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
    
    x.var <- input$x.cat
    x.type <- getType(x.var)
    x.query <- getDTQ(x.var)
    x.table <- getTable(x.var)
    
    filter <- input$filter
    f.type <- getType(filter)
    f.query <- getDTQ(filter)
    f.table <- getTable(filter)
    
    group <- input$group
    g.type <- getType(group)
    g.query <- getDTQ(group)
    g.table <- getTable(group)
    
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
    
    if (!is.na(g.type)) {
      eval(parse(
        text=paste0("ngroups <- subset[!is.na(", g.query, "), length(unique(", g.query, "))]")
      ))
    } else {
      ngroups <- 1
    }
    
    min_height <- 400
    if (y.type == "categorical") {
      # Get the Y categories
      eval(parse(
        text = paste0("ycats <- subset[, unique(", y.query, ")]")
      ))
      ycats <- ycats[!is.na(ycats)]
      nycats <- length(ycats)
      
      if (x.type != "categorical" & ngroups <= 2) {
        dyn_height <- nycats * 30
      } else {
        dyn_height <- (nycats * 50 + 30) * ngroups
      }
      
      return(max(min_height, dyn_height))
    } 
    else if (x.type == "categorical") {
      # Get the x categories
      eval(parse(
        text = paste0("xcats <- subset[, unique(", x.query, ")]")
      ))
      xcats <- xcats[!is.na(xcats)]
      nxcats <- length(xcats)
      
      margSize <- max(sapply(xcats, nchar))
      
            
      if (y.type != "categorical" & ngroups <= 2) {
        dyn_height <- margSize + 500
      } else {
        dyn_height <- (margSize + 500) * ngroups
      }
      
      return(max(min_height, dyn_height))
    } 
    else {
      return(min_height)
    }
  }
  
  output$distPlot <- renderPlot({
    tryCatch({
      plotData(input)
    }, error=function(e) {
      if (grepl("figure margins", e$message)) {
        par(mar=c(0,0,6,0))
        nullPlot(c(0,1), c(0,1))
        mtext(
          "Window is too small for plot.\nPlease resize or choose a different category to view.",
          side=3, cex=1.8
        )
      }
      
    })
    
  },  height=getPlotHeight, units="px")
  
  output$Height <- renderText(getPlotHeight)
})
