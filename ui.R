
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("What does the funding landscape for NHRMC look like?"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min=1, max=50, value=30),
      sliderInput("red", "Red:", min=0, max=255, value=10),
      sliderInput("blue", "Blue:", min=0, max=255, value=10),
      sliderInput("green", "Green:", min=0, max=255, value=10),
      sliderInput("trans", "Transparency:", min=0, max=255, value=255),
      actionButton("go", "Update Colors")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
