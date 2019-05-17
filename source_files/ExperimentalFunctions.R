# ExperimentalFunctions
# Authors: Daroc Alden, Jeremy Walker, & Samantha Piatt

# Creates a column containg the average difference between this data point and 
#   the past and future n datapoints, given by the width parameter.
#
# Returns a data.frame containing each column of the original data frame, plus 
#   one column for each with the suffix ".Past" and ".Future"
series_metric <- function(data, width) {
  shift_metric <- function(column, width, prefix){
    n <- length(column)[1]
    tmp <- data.frame(past = 0, current = column, future = 0)
    for(i in 1:n){
      future = c(0)
      past = c(0)
      for(j in 1:width){
        if((i - j) >= 1) past[j] <- column[i] - column[i-j]
        if((i + j) <= n) future[j] <- column[i] - column[i+j]
      }
      tmp$past[i] <- mean(past)
      tmp$future[i] <- mean(future)
    }
    colnames(tmp) <- c(paste(prefix, "Past"), prefix, paste(prefix, "Future"))
    return(tmp)
  }
  
  names <- colnames(data)
  out <- shift_metric(data[, 1], width, names[1])
  for(i in 1:(length(data)-1)){
    tmp <- shift_metric(data[, i + 1], width, names[i + 1])
    out <- data.frame(out, tmp)
  }
  return(out)
}