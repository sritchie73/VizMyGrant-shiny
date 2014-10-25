
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    input$go
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    col <- isolate(sprintf("#%02X%02X%02X%02X", input$red, input$green, input$blue, input$trans))
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = col, border = 'white', main="Old Faithful data")

  })

})
