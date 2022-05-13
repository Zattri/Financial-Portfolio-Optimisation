findminmax <- function(data, minimise = TRUE){
  # Need to fix dimensions again because 
  minmax <- NA
  if (minimise) minmax <- min(data[,2])
  else minmax <- max(data[,2])
  
  rownum <- which(data[,2] == minmax)
  if (length(rownum) > 1) 
    rownum <- rownum[1]
  
  if (minimise)
    return (minmax - data [rownum,3])
  else return (minmax + data [rownum,3])
}

plotbars <- function(data1, data2, data3, 
                     cap1 = "GA1", cap2 = "GA2", cap3 = "GA3"){
  data = data1
  hues = c("black","blue","green")
  
  min1 = findminmax(data1)   #min(data1) - data1 [which(data1 == min(data1))+2*nrow(data1)]
  min2 = findminmax(data2)   #min(data2) - data2 [which(data2 == min(data2))+nrow(data2)]
  min3 = findminmax(data3)   #min(data3) - data3 [which(data3 == min(data3))+nrow(data3)]
  
  max1 = findminmax(data1, FALSE)   #max(data1) + data1 [which(data1 == max(data1))+nrow(data1)]
  max2 = findminmax(data2, FALSE)   #max(data2) + data2 [which(data2 == max(data2))+nrow(data2)]
  max3 = findminmax(data3, FALSE)   #max(data3) + data3 [which(data3 == max(data3))+nrow(data3)]
  
  minn = min(min1, min2, min3)
  maxx = max(max1, max2, max3)
  
  
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  plot(df$x, df$y, type = "l", col = hues[1],  ylim=c(minn, maxx), #choose ylim CAREFULLY as per your data ranges
       main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[1]);    #plot the error bars mean-errorbar, mean+errorbar
  
  data = data2
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[2])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[2]); 
  
  data = data3
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[3])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[3]); 
  
  legend("topleft", legend = c(cap1, cap2, cap3), col = hues, lwd = 1,
         cex = 0.5)
}


singlePlot <- function(data1, cap1 = "GA1", mopso=FALSE){
  data = data1
  hues = c("black","blue","green")
  
  #min1 = findminmax(data1, minimise=TRUE)   #min(data1) - data1 [which(data1 == min(data1))+2*nrow(data1)]
  #max1 = findminmax(data1, minimise=FALSE)   #max(data1) + data1 [which(data1 == max(data1))+nrow(data1)]
  
  minn = min(data1)
  maxx = max(data1)
  
  if (mopso) {
    df <- data.frame(x=1:length(data), y=data, dy = 0)
  }
  else {
    df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  }
  
  plot(df$x, df$y, type = "l", col = hues[1],  ylim=c(minn, maxx), #choose ylim CAREFULLY as per your data ranges
       main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[1]);    #plot the error bars mean-errorbar, mean+errorbar
  
  legend("topleft", legend = c(cap1), col = "black", lwd = 1, cex = 0.5)
}

comparisonPlot <- function(nsga_2, mopso, nsga_3, 
                     cap1 = "NSGA-2", cap2 = "MOPSO", cap3 = "NSGA-3"){
  data = nsga_2
  hues = c("black","blue","green")
  
  min1 = findminmax(nsga_2)   #min(data1) - data1 [which(data1 == min(data1))+2*nrow(data1)]
  min2 = min(mopso)   #min(data2) - data2 [which(data2 == min(data2))+nrow(data2)]
  min3 = min(nsga_3)   #min(data3) - data3 [which(data3 == min(data3))+nrow(data3)]
  
  max1 = findminmax(nsga_2, FALSE)   #max(data1) + data1 [which(data1 == max(data1))+nrow(data1)]
  max2 = max(mopso, FALSE)   #max(data2) + data2 [which(data2 == max(data2))+nrow(data2)]
  max3 = max(nsga_3, FALSE)   #max(data3) + data3 [which(data3 == max(data3))+nrow(data3)]
    
  minn = min(min1, min2, min3)
  maxx = max(max1, max2, max3)
  
  
  
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  plot(df$x, df$y, type = "l", col = hues[1],  ylim=c(minn, maxx), #choose ylim CAREFULLY as per your data ranges
       main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[1]);    #plot the error bars mean-errorbar, mean+errorbar
  
  data = mopso
  df <- data.frame(x=1:length(data), y=data, dy = 0)  #MOPSO has no error bar  
  lines(df$x, df$y, col = hues[2])
  #segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[2]); # There are no error bars for MOPSO
  
  data = nsga_3
  df <- data.frame(x=1:length(data), y=data, dy = 0)  #dy = length of error bar  
  lines(df$x, df$y, col = hues[3])
  #segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[3]); 
  
  
  legend("topleft", legend = c(cap1, cap2, cap3), col = hues, lwd = 1,
         cex = 0.5)
}

convertNSGA2data = function(data, objectiveF=1, maximise=FALSE) {
  df <<- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df) = c("generation", "mean", "error")
  for (i in 1:length(data)) {
    fitnessValues = data[[i]]$value[,objectiveF] # [,1] is f1, [,2] is f2
    if (maximise) {
      fitnessValues = fitnessValues * -1
    }
    df[nrow(df) + 1,] = c(i, mean(fitnessValues), (max(fitnessValues) - min(fitnessValues)/2)) 
    # Dividing error bar by two because it's added on to each side of the mean
  }
  
  return(df)
}

convertMOPSOdata = function(data, objectiveF=1, maximise=FALSE) {
  fitnessValues = data$objfnvalues[,objectiveF] # [,1] is f1, [,2] is f2
  if (maximise) {
    fitnessValues = fitnessValues * -1
  }
  return(fitnessValues)
}


convertNSGA3data = function(data, objectiveF=1, maximise=FALSE) {
  fitnessValues = data@fitness[,objectiveF] # [,1] is f1, [,2] is f2
  if (maximise) {
    fitnessValues = fitnessValues * -1
  }
  return(fitnessValues)
}


