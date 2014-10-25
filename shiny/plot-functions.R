library(RColorBrewer)

#' Get the plot limits to set for the desired plot window
#'
#' \code{plot} manually adds an extra region to the plot on top of the given
#' 'xlim' and 'ylim', amounting to 4% of the plot region in either direction.
#' This function tells you what limits to set so that the boundaries of the plot
#' are precisely the min and max you want.
#' 
#' @param dlim the limits you want to set
#' @return
#'  the min and max to set for the desired plot limits  
getLimsForManualUsr <- function(dlim) {
  A = matrix(c(1.04, -0.04, -0.04, 1.04), nrow=2, ncol=2)
  B = matrix(dlim, nrow=2, ncol=1)
  as.vector(solve(A, B))
}

# Create an empty plot
nullPlot <- function(xlim=c(0,0), ylim=c(0,0), xlab="", ylab="", cex.lab=1) {
  plot(
    0, type='n', xaxt='n', yaxt='n', xlim=xlim, ylim=ylim, xlab="", ylab="", 
    frame.plot = FALSE, cex.lab=cex.lab
  )
  mtext(xlab, side=1, cex=cex.lab)
  mtext(ylab, side=2, cex=cex.lab)
}

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

  print(f.query)
  print(g.query)
  print("---")
  
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
      
      
      par(mar=c(5,8,4,8))
      nullPlot(xlim=xlim, ylim=ylim)
      mtext(x.var, side=1, line=3, cex=1.4)
      axis(
        side=1, at=seq(xlim[1], xlim[2], length=7),
        labels=round(abs(seq(xlim[1], xlim[2], length=7)), digits=0)
      )
      
      if (!is.null(lookup[[y.var]][["order"]])) {
        ycats <- lookup[[y.var]][["order"]]
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
          
          # plot on the left
          if (jj == 1) {
            rect(
              xleft=-1*x.val,
              ybottom=ii-1,
              xright=0,
              ytop=ii,
              col=color
            )
          } else { # plot on the right
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
    
    else if (grepl("(point)|(positive)", x.type)) {
      ylim = getLimsForManualUsr(c(0, nycats+0.1))
      
      # Determine the x limits
      xmax <- 0
      xmin <- 0
      for (ycat in ycats) {
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
      
      par(mar=c(5,8,4,8))
      nullPlot(xlim=xlim, ylim=ylim)
      mtext(x.var, side=1, line=3, cex=1.4)
      axis(
        side=1, at=seq(xlim[1], xlim[2], length=7),
        labels=round(abs(seq(xlim[1], xlim[2], length=7)), digits=0)
      )
      
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
          
        color <- "#e6550d"
        if (xlim[1] == 0) {
          rect(
            xleft=0,
            ybottom=ii-1,
            xright=x.val,
            ytop=ii,
            col=color
          )
        } else { 
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