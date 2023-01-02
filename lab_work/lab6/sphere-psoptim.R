library(pso) # load pso 


getfitness <- function(x){
  sphere=sum(x^2)             #the objective function is the sphere function with a minimum at x= (0,0) if D=2
  return (sphere)
}

getPSOParams <- function(){
  #problem specific parameters
  D=2;  #2 dimensional solutions 
  lower=rep(-5.2,D)  #lower bound on the solution variables
  upper=rep(5.2,D)   #upper bound on the solution variables
  # type '?psoptim' to understand why we are setting up the parameters below.
  #C=list(trace=1,maxit=10,REPORT=1,trace.stats=1,s=5)  #Control parameters for PSO; see "Details" after typing ?psoptim
  C=list(trace=1,maxit=30,REPORT=1,trace.stats=1,s=20)
  p <- list(D=D,lower=lower, upper=upper,C=C)    
  return (p)
}

runPSO <- function(){
  #set.seed(12345) # set for replicability 
  p <- getPSOParams()
  # perform the optimization:
  PSO=psoptim(rep(NA,p$D),fn=getfitness,lower=p$lower, upper=p$upper,control=p$C)
  #print the best solution and its fitness
  cat("best:",PSO$par,"f:",PSO$value,"\n") 
  return (PSO)                    
}


# result:
plotResults <-function(PSO,j=1){        #supply a PSO object, and the index of the solution variable to plot. 
  p <- getPSOParams()                   #get the PSO parameters set up as above
  s <- p$C$s                            #extract the required parameters out of p
  maxit <- p$C$maxit
  pdf("psoptim1.pdf",width=5,height=5)  #set up an output PDF file
  #plot the value of the jth solution variable as iterations go on. 
  plot(xlim=c(1,maxit),rep(1,s),PSO$stats$x[[1]][j,],pch=19,
     xlab="iterations",ylab=paste("s_",j," value",sep=""))
  for(i in 2:maxit) points(rep(i,s),PSO$stats$x[[i]][j,],pch=19)
  dev.off()
  pdf("psoptim2.pdf",width=5,height=5)
  plot(PSO$stats$error,type="l",lwd=2,xlab="iterations",
     ylab="best fitness")
  dev.off()
}

plotFitScores <-function(PSO,j=1){        #supply a PSO object, and the index of the solution variable to plot. 
  p <- getPSOParams()                   #get the PSO parameters set up as above
  s <- p$C$s                            #extract the required parameters out of p
  maxit <- p$C$maxit
  plot(xlim=c(1,maxit),rep(1,s),PSO$stats$x[[1]][j,],pch=19,
       xlab="iterations",ylab=paste("s_",j," value",sep=""))
  for(i in 2:maxit) points(rep(i,s),PSO$stats$x[[i]][j,],pch=19)
}

plotSwarmBest <-function(PSO) {
  plot(PSO$stats$error,type="l",lwd=2,xlab="iterations", ylab="best fitness")
}