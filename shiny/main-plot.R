library(RColorBrewer)

plotData <- function(input) {
  x.var <- input$x.cat
  y.var <- input$y.cat
  group <- input$group
  filter <- input$filter
  
  x.type <- getType(x.var)
  y.type <- getType(y.var)
  g.type <- getType(group)
  f.type <- getType(filter)
  
  x.query <- getDTQ(x.var)
  y.query <- getDTQ(y.var)
  g.query <- getDTQ(group)
  f.query <- getDTQ(filter)
  
  x.table <- getTable(x.var)
  y.table <- getTable(y.var)
  g.table <- getTable(group)
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
  
  if (grepl("(discrete)|(continuous)", x.type) && y.type == "categorical") {
    # Get the Y categories
    eval(parse(
      text = paste0("ycats <- subset[, unique(", y.query, ")]")
    ))
    ycats <- ycats[!is.na(ycats)]
    nycats <- length(ycats)
    
    
    # What are the groups, and how many are there?
    if (!is.na(g.type)) {
      eval(parse(
        text=paste0(
          "groups <- subset[!is.na(", g.query, "), unique(", g.query, ")]"
        )
      ))
      nGroups <- length(groups)
    } else {
      nGroups <- 0
    }
    
    # If there are only two groups, x is single value per category, and always
    # positive or negative, we can flip over the y axis
    if (grepl("(positive)", x.type) && grepl("(point)", x.type) && nGroups == 2) {
      ylim = getLimsForManualUsr(c(0, nycats+0.1))
      
      # Determine the x limits
      xmax <- 0
      for (ycat in ycats) {
        eval(parse(
          text=paste0(
            "xdat.g1 <- subset[", g.query, " == '", groups[1], "' & ",
            y.query, " == '", ycat, "', ", x.query, "]"
          ) 
        ))
        xmax <- max(c(xmax, xdat.g1))
        eval(parse(
          text=paste0(
            "xdat.g2 <- subset[", g.query, " == '", groups[2], "' & ",
            y.query, " == '", ycat, "', ", x.query, "]"
          )  
        ))
        xmax <- max(c(xmax, xdat.g2)) 
      }
      xlim = c(-1*(xmax*1.1), xmax*1.1)
      
      
      par(mar=c(5,15,4,8))
      nullPlot(xlim=xlim, ylim=ylim)
      mtext(x.var, side=1, line=3, cex=1.4)
      axisTicks <- seq(xlim[1], xlim[2], length=7)
      axisTicks[4] <- 0
      axis(
        side=1, at=axisTicks,
        labels=HumanReadable(abs(axisTicks))
      )
      
      # Make sure the y categories are ordered, for consistency
      if (!is.null(lookup[[y.var]][["order"]])) {
        ycats <- lookup[[y.var]][["order"]]
      }
      # Make sure the groups are ordered, for consistency
      if (!is.null(lookup[[group]][["order"]])) {
        groups <- lookup[[group]][["order"]]
      }
      # For each category and group
      for (ii in seq_along(ycats)) {
        mtext(ycats[[ii]], side=2, at=ii-0.5, las=2)
        for (jj in seq_along(groups)) {
          eval(parse(
            text=paste0(
              "x.val <- subset[", g.query, " == '", groups[jj], "' & ",
              y.query, " == '", ycats[[ii]], "', ", x.query, "]"  
            )  
          ))
          
          color <- lookup[[group]][["colors"]][[groups[jj]]]
          
          # Render the bars for each y-axis category
          # plot on the left for group 1
          if (jj == 1) {
            rect(
              xleft=-1*x.val,
              ybottom=ii-1,
              xright=0,
              ytop=ii,
              col=color
            )
          } else { # plot on the right for group 2
            rect(
              xleft=0,
              ybottom=ii-1,
              xright=x.val,
              ytop=ii,
              col=color
            )
          }
        }
      }
      legend(
        x=xmax*1.2, y=nycats/1.5, legend=names(lookup[[group]][["colors"]]), 
        fill=unlist(lookup[[group]][["colors"]]), xpd=TRUE
      ) 
    }
    
    # Create bar plots for without groups
    else if (grepl("(point)|(positive)", x.type)) {
      ylim = getLimsForManualUsr(c(0, nycats+0.1))
      
      # Determine the x limits
      xmax <- 0
      xmin <- 0
      for (ycat in ycats) {
        fullQ <- paste0(
          "xdat <- subset[", y.query, " == '", ycat, "', ", x.query, "]"
        ) 
        eval(parse(
          text=paste0(
            "xdat <- subset[", y.query, " == '", ycat, "', ", x.query, "]"
          ) 
        ))
        xmax <- max(c(xmax, xdat)) 
        xmin <- min(c(xmin, xdat))
      }
      xlim = c(
        ifelse(xmin == 0, 0, xmin*1.1), 
        ifelse(xmax == 0, 0, xmax*1.1)
      )
      
      # Create the empty plot with the appropriate axes
      par(mar=c(5,8,4,8))
      nullPlot(xlim=xlim, ylim=ylim)
      mtext(x.var, side=1, line=3, cex=1.4)
      axis(
        side=1, at=seq(xlim[1], xlim[2], length=7),
        labels=HumanReadable(seq(xlim[1], xlim[2], length=7))
      )
      
      # Make sure the y categories are ordered, if ordering makes sense!
      if (!is.null(lookup[[y.var]][["order"]])) {
        ycats <- lookup[[y.var]][["order"]]
      }
      # For each category and group
      for (ii in seq_along(ycats)) {
        mtext(ycats[[ii]], side=2, at=ii-0.5, las=2)
       
        eval(parse(
          text=paste0(
            "x.val <- subset[", y.query, " == '", ycats[[ii]], "', ", 
            x.query, "]"  
          )  
        ))
          
        # Render the bars for each y-axis category
        color <- "#e6550d"
        if (xlim[1] == 0) { # if x is negative 
          rect(
            xleft=0,
            ybottom=ii-1,
            xright=x.val,
            ytop=ii,
            col=color
          )
        } else { # if x is positive
          rect(
            xleft=x.al,
            ybottom=ii-1,
            xright=0,
            ytop=ii,
            col=color
          )
        }
      }
    }
  }
}