setwd("C:/Users/Zattri/Desktop/modern_opt/lab7")

library("mco")
source("Bag-Prices.R")

eval=function(x) {
  return(c(-profit(x),produced(x))) # Two dimensional evaluation function - minimising both profit and units produced
}

runNSGA <- function(){
  m = 2 #2 objectives: maximise profit, minimise production
  D=5 # 5 bag prices

  cat("NSGA-II begins:\n")

  #minimum value of prices is 1,1,1,1,1, and max is 1000,1000,1000,1000,1000
  # - this specifies the range of weightings to change
  
  G=nsga2(fn=eval,idim=5,odim=m,lower.bounds=rep(1,D),upper.bounds=rep(1000,D),popsize=20,generations=1:100)

  # show best individuals:
  I=which(G[[100]]$pareto.optimal)
  for(i in I)
  {
    x=round(G[[100]]$par[i,])
    cat(x," f=(",profit(x),",",produced(x),")","\n",sep=" ")
  }

  return (G)
}

plotNSGA <- function(G){
  I=1:100
  for(i in I) { 
    P=G[[i]]$value # objectives f1 and f2
    P[,1]=-1*P[,1] # show positive f1 values
    # color from light gray (75) to dark (1):
    COL=paste("gray",round(76-i*0.75),sep="")
    if(i==1) {
      plot(P,xlim=c(-500,44000),ylim=c(0,140),xlab="Profit",ylab="Production",cex=0.5,col=COL)
    }
    Pareto=P[G[[i]]$pareto.optimal,]
    # sort Pareto according to x axis:
    I=sort.int(Pareto[,1],index.return=TRUE)
    Pareto=Pareto[I$ix,]
    points(P,type="p",pch=1,cex=0.5,col=COL)
    lines(Pareto,type="l",cex=0.5,col=COL)
  }
  
}

# Test runs
my_ga = runNSGA()
plotNSGA(my_ga)