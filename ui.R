
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("scripts/loader.R")


shinyUI(fluidPage(

  # Application title
  titlePanel("What does the funding landscape for NHMRC look like in 2014?"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3(strong("Basic plot options")),
      selectInput(
        "x", "Data to show on the x-axis:", 
        choices=c(
          "Sex", "Career Stage", "Grant Type", "Grant Sub Type", "State", 
          "Institution", "Broad Research Area", "Field of Research",
          "Field of Research Category", "Title"
        ),
        selected="Sex"
      ),
      selectInput(
        "y", "Data to show on the y-axis:", 
        choices=c(
          "Amount awarded", "Total amount awarded", "Total number funded"
        ),
        selected="Amount awarded"
      ),
      textInput(
        "h", "Plot height (pixels)",
        value="600"
      ),
      h3(strong("Groups to show")),
      selectInput(
        "g", "Color within each plot:", 
        choices=c(
          "-", "Sex", "Career Stage", "Grant Type", "Grant Sub Type", "State", 
          "Institution", "Broad Research Area", "Field of Research",
          "Field of Research Category", "Title"
        ),
        selected="Sex"
      ),
      selectInput(
        "p1", "Create a plot for each of:",
        choices=c(
          "-", "Sex", "Career Stage", "Grant Type", "Grant Sub Type", "State", 
          "Institution", "Broad Research Area", "Field of Research",
          "Field of Research Category", "Title"
        ),
        selected="Career Stage"
      ),
      selectInput(
        "p2", "Compare to each of:",
        choices=c(
          "-", "Sex", "Career Stage", "Grant Type", "Grant Sub Type", "State", 
          "Institution", "Broad Research Area", "Field of Research",
          "Field of Research Category", "Title"
        ),
        selected="-"
      ),
      h3("Filter data:"),
      selectInput(
        "fS", "Grant Type:",
        choices = c("Fellowship grant", "Non-fellowship grant"),
        selected = c("Fellowship grant", "Non-fellowship grant"),
        multiple=TRUE
      ),
      selectInput(
        "fGT", "Specific Grant:",
        choices = sort(na.omit(unique(full2014[["GrantType"]]))),
        selected = sort(na.omit(unique(full2014[["GrantType"]]))), 
        multiple=TRUE
      ),
      selectInput(
        "fI", "Institute:",
        choices = sort(na.omit(unique(full2014[["Institution"]]))),
        selected = sort(na.omit(unique(full2014[["Institution"]]))),
        multiple=TRUE
      )
    ),

    
    mainPanel(
      plotOutput("distPlot", height="auto"),
      hr(),
      fluidRow(
        column(3, textInput("fname", "Save as...", value="VizMyGrant.png")),
        column(3, textInput("pwidth", "Width (pixels)", value="1200")),
        column(3, textInput("pheight", "Height (pixels)", value="600")),
        column(2, 
          # clungy hack to make the save image button appear in the same 
          # vertical location as the other inputs
          HTML('<label for="saveImage"><font color="white">Button</font></label>'), 
          downloadButton("saveImage", label = "Save image")
        )
      ),
      hr(),
      includeHTML("inference-info.html"),
      includeHTML("copyright.html")
    )
  )
))
