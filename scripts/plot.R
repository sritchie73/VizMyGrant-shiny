createPlot <- function(input, data) {
  # Map from human readable to column names
  map <- c(
    "Amount awarded"="TotalAmount",
    "Total amount awarded"="TotalAmount",
    "Total number funded"="TotalAmount",
    "Career Stage"="CareerStage",
    "Sex"="Gender",
    "Title"="Title",
    "State"="State",
    "Institution"="Institution",
    "Grant Type"="GrantType",
    "Grant Sub Type"="GrantSubType",
    "Broad Research Area"="BroadArea",
    "Field of Research Category"="Category",
    "Field of Research"="FoR"
  )
  
  x <- input$x
  y <- input$y
  g <- input$g
  p1 <- input$p1
  p2 <- input$p2
  
  # First filter the data that will be shown for missingness
  data <- data[
    !is.na(data[[map[x]]]) & !is.na(data[[map[y]]])
  ]
  if (g != "None") {
    data <- data[!is.na(data[[map[g]]])]
  }
  if (p1 != "None") {
    data <- data[!is.na(data[[map[p1]]])]
  }
  if (p2 != "None") {
    data <- data[!is.na(data[[map[p2]]])]
  }
  
  # Filter data based on user specified filters:
  data <- data[
    is.na(data[["isSalaryGrant"]]) | 
    (data[["isSalaryGrant"]] & "Fellowship grant" %in% input$fS) | 
    (!data[["isSalaryGrant"]] & "Non-fellowship grant" %in% input$fS)
  ]
  data <- data[
    is.na(data[["GrantType"]]) | data[["GrantType"]] %in% input$fGT
  ]
  
  # Add geometry layer based on data to show on the y axis
  if (y == "Amount awarded") {
    p <- ggplot(data, aes_string(x=map[x], y=map[y]))
    if (g == "None") {
      p <- p + geom_boxplot()
    } else {
      p <- p + geom_boxplot(aes_string(fill=map[g]))
    }
  } else if (y == "Total number funded") {
    p <- ggplot(data, aes_string(x=map[x]))
    if (g == "None") {
      p <- p + geom_bar(stat="bin")
    } else {
      p <- p + geom_bar(stat="bin", position="dodge", aes_string(fill=map[g]))
    }
  } else if (y == "Total amount awarded") {
    p <- ggplot(data, aes_string(x=map[x], y=map[y]))
    if (g == "None") {
      p <- p + geom_bar(stat="sum")
    } else {
      p <- p + geom_bar(stat="sum", position="dodge", aes_string(fill=map[g]))
    }
  }
  
  
  # Add facets
  if (p1 != "None" & p2 == "None") {
    p <- p + facet_wrap(as.formula(paste("~", map[p1])))
  } else if (p1 == "None" & p2 != "None") {
    p <- p + facet_wrap(as.formula(paste(map[p2], "~")))
  } else if (p1 != "None" & p2 != "None") {
    p <- p + facet_grid(as.formula(paste(map[p2], "~", map[p1])))
  }
  
  # Change color mapping to be gender neutral
  fill.ys <- c("Amount awarded", "Total number funded", "Total amount awarded")
  if (g == "Sex" & y %in% fill.ys) {
    p <- p + scale_fill_brewer(palette="PuOr")
  } else if (g == "Sex" & y %in% col.ys) {
    p <- p + scale_color_brewer(palette="PuOr")
  }
  
  p <- p + xlab(x)
  p <- p + ylab(y)
}
