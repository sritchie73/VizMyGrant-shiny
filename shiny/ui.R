
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("What does the funding landscape for NHRMC look like in 2014?"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "x.cat", "X axis", 
        choices=c("Number Funded", "Total Amount Awarded"),
        selected="Number Funded"
      ),
      selectInput(
        "y.cat", "Y axis", 
        choices=c(
          "Career Stage", "Grant Type", "Grant Sub Type", "State", "Institution",
          "Broad Research Area", "Field of Research",
          "Field of Research Category"
        ),
        selected="Career Stage"
      ),
      selectInput(
        "group", "Group By", 
        choices=c("Sex", "None"),
        selected="Sex"
      ),
      selectInput(
        "filter", "Filter By", 
        choices=c("Salary Grants", "Non Salary Grants", "None"),
        selected="Salary Grants"
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height="auto"),
      br(),
      p(
        strong("Note:"), "Gender was inferred by title or using historical data",
        "from the U.S. Social Security Administration baby name database from",
        "the years 1932 through 2012 using the", 
        a("R Gender package", href="https://github.com/ropensci/gender"), "."
      ),
      p(
        "Career stage was inferred based on title, then by fellowship type as follows:",
        tags$ul(
          tags$li(
            strong("Early Career:"), "where the individual's title is Mr, Mrs,",
            'Miss, Ms, or Dr; or a Grant Type containing "Early Career Fellowship"',
            '; or a Grant Sub Type ending with "ECF"'
          ),
          tags$li(
            strong("Mid Career:"), "where the individual's title Associate Professor;", 
            'or a Grant Type containing "Career Development Fellowship"; or a',
            'Grant Sub Type ending with "CDF"'
          ),
          tags$li(
            strong("Senior:"), "where the individual's title is Professor, or",
            'Emiritus Professor; a Grant Type of "NHMRC Partnership"; or a', 
            'Grant Sub Type containing "Research Fellowship"'
          )
        )
      ),
      p(
        "Original data was downloaded from the",
        a("NHMRC website", href="https://www.nhmrc.gov.au/grants/outcomes-funding-rounds"),
        ". Summary files for 2014 were as follows:", br(),
        a(
          "Summary details of the 17 Oct 2014 Announcement (XLS, 241KB)", 
          href="https://www.nhmrc.gov.au/_files_nhmrc/file/grants/funding/2014/summary_data_17_october_2014_announcement_141020.xlsx"
        ),
        br(),
        a(
          "Summary details of the 29 August 2014 Announcement (XLS, 49KB)",
          href="https://www.nhmrc.gov.au/_files_nhmrc/file/grants/funding/2014/summary_data_august_2014_announcement_140919.xlsx"
        ),
        br(),
        br(),
        includeHTML("copyright.html")
        
      )
    )
    
  )
))
