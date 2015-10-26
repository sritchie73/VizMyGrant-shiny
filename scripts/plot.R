library(scales)
source("scripts/utils.R")

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
  if (g != "-") {
    data <- data[!is.na(data[[map[g]]])]
  }
  if (p1 != "-") {
    data <- data[!is.na(data[[map[p1]]])]
  }
  if (p2 != "-") {
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
  data <- data[
    is.na(data[["Institution"]]) | data[["Institution"]] %in% input$fI
  ]
  
  # Add geometry layer based on data to show on the y axis
  if (y == "Amount awarded") {
    p <- ggplot(data, aes_string(x=map[x], y=map[y]))
    if (g == "-") {
      p <- p + geom_boxplot()
    } else {
      p <- p + geom_boxplot(aes_string(fill=map[g]))
    }
  } else if (y == "Total number funded") {
    p <- ggplot(data, aes_string(x=map[x]))
    if (g == "-") {
      p <- p + geom_bar(stat="bin")
    } else {
      p <- p + geom_bar(stat="bin", position="dodge", aes_string(fill=map[g]))
    }
  } else if (y == "Total amount awarded") {
    # We need to create a summary table based on the groupings and panels 
    # because ggplot2 won't let us do this manually
    data <- data[,
      list(TotalAmount=sum(TotalAmount)), 
      # The na.omit allows us to dynamically handle panels: if they exist.
      by=eval(na.omit(c(map[x], map[g], map[p1], map[p2]))) 
    ]
    
    p <- ggplot(data, aes_string(x=map[x], y=map[y]))
    if (g == "-") {
      p <- p + geom_bar(stat="identity")
    } else {
      p <- p + geom_bar(
        stat="identity", position="dodge", 
        aes_string(fill=map[g])
      )
    }
  }
  
  
  # Add facets
  if (p1 != "-" & p2 == "-") {
    p <- p + facet_wrap(as.formula(paste("~", map[p1])))
  } else if (p1 == "-" & p2 != "-") {
    p <- p + facet_grid(as.formula(paste(map[p2], "~ .")))
  } else if (p1 != "-" & p2 != "-") {
    p <- p + facet_grid(as.formula(paste(map[p2], "~", map[p1])))
  }
  
  # Change color mapping to be gender neutral
  fill.ys <- c("Amount awarded", "Total number funded", "Total amount awarded")
  col.ys <- c()
  if (g == "Sex" & y %in% fill.ys) {
    p <- p + scale_fill_manual(values=c("#d8b365", "#5ab4ac"), label=capitalize)
  }
  if (g == "Sex" & y %in% col.ys) {
    p <- p + scale_color_manual(values=c("#d8b365", "#5ab4ac"), label=capitalize)
  }
  
  # Use human readable axes titles
  p <- p + xlab(paste0("\n",x))
  p <- p + ylab(paste0(y, "\n"))
  
  # Human readable legend title
  if (y %in% fill.ys) {
    p <- p + labs(color = g)
  } 
  if (y %in% col.ys) {
    p <- p + labs(fill = g)
  }
  
  # Human readable axis text
  if (y %in% c("Amount awarded", "Total amount awarded")) {
    p <- p + scale_y_continuous(label=dollar)
  }
  p <- p + scale_x_discrete(label=capitalize)
  
  # Modify the theme of the plot
  p <- p + 
    theme(
      legend.title = element_text(size = 18, face="bold"),
      strip.text = element_text(size = 20, face="bold"),
      axis.text = element_text(size = 16),
      axis.text.x = element_text(angle=90),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      legend.key.height = unit(1, "cm")
    )
  
  return(p)
}
