library(stringr)

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

# Make numbers human readable for axis text
HumanReadable <- function(nums) {
  max_num <- max(nums)
  min_num <- min(nums)
  diff <- max_num - min_num
  
  # split numbers over decimal point
  nums <- as.character(nums)
  left <- sapply(str_split(nums, "\\."), `[[`, 1)
  right <- NULL
  tryCatch({ 
    right <- sapply(str_split(nums, "\\."), `[[`, 2)
  }, error = function(e) {
    right <- NULL
  })
  # how many powers of ten are the numbers?
  power <- max(nchar(gsub("-", "", left)))
  
  if (power == 1) { # probably grant length
    if(is.null(right)) {
      return(left)
    } else {
      return(nums)
    }
  } else if (power == 2) { # probably a percentage?
    return(paste0(left))
  } else if (power == 3) { # probably a total amount?
    return(left)
  } else { # probably $$
    sapply(left, function(nn) {
      pow <- nchar(gsub("-", "", nn))
      nn <- format(round(as.numeric(nn), digits=-1*(power-2)), scientific=FALSE)
      
      if (grepl("-", nn)) {
        first <- str_sub(nn, 1, 2)
        second <- str_sub(nn, 3, 3)
        third <- str_sub(nn, 4, 4)
      } else {
        first <- str_sub(nn, 1, 1)
        second <- str_sub(nn, 2, 2)
        third <- str_sub(nn, 3, 3)
      }
      
      if (pow == 1)
        return(nn)
      if (pow == 2)
        return(nn)
      if (pow == 3)
        return(nn)
      if (pow == 4)
        return(paste0(first, ".", second, "K"))
      if (pow == 5)
        return(paste0(first, second, "K"))
      if (pow == 6)
        return(paste0(first, second, third, "K"))
      if (pow == 7)
        return(paste0(first, ".", second, "M"))
      if (pow == 8)
        return(paste0(first, second, "M"))
      if (pow == 9)
        return(paste0(first, second, third, "M"))
    })
  }
} 

# Determine margin size based on the length of the labels
marginSize <- function(labels, multipanel=FALSE) {
  # No idea why this is different
  if (multipanel) {
    max_len <- max(sapply(labels, strwidth))
    max_len * 100
  } else {
    max_len <- max(sapply(labels, strwidth))
    max_len * 60
  }
}

# show 0 if NA
cleanMedian <- function(vec, type="integer") {
  val <- median(vec, na.rm=TRUE)
  if (is.na(val)) {
    val <- 0
  }
  if (type == "integer") {
    val <- as.integer(val)
  } else {
    val <- as.double(val)
  }
  val
}
