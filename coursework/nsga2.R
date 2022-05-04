library("mco")

run_nsga2 = function(obj = 2, dim = 11, pop = 20, gen = 100, cprob=0.7, mprob=0.2){
  cat("Running NSGA-2 \n")
  return(
    mco::nsga2(
      constraints = budget_new, 
      cdim = 1:dim, 
      fn = eval,
      idim = dim,
      odim = obj,
      cprob=cprob,
      mprob=mprob,
      lower.bounds = rep(1,dim), # Might want to make a minimum investment percentage or something?
      upper.bounds = rep(100,dim),
      popsize = pop,
      generations = 1:gen)
    )
}

print_best = function(ga, generation) {
  # show best individuals:
  I=which(ga[[generation]]$pareto.optimal) 
  for(i in I)
  {
    x=round(ga[[generation]]$par[i,])
    cat(x," f=(",price_earnings_ratio(df, x),",",value_at_risk(df, x),")","\n",sep=" ")
  }
}

# --------------------------------------
# PLOTTING 
plotNSGA <- function(ga, gen = 100, title="NSGA-II", xlab="F1", ylab="F2", colour=FALSE) {
  cat("Plotting NSGA \n")
  I=1:gen
  for(i in I)
  { 
    P = ga[[i]]$value # objectives f1 and f2
    P[,1] = -1 * P[,1] # show positive f1 values
    # color from light gray (75) to dark (1):
    COL="black"
    if (colour == TRUE) {
      COL=paste("gray",round(76-(i/2)*0.75),sep="") #i/2 for 200 gen
    }
    if(i==1) {
      plot(P,main=title,xlab=xlab,ylab=ylab,cex=0.5,col=COL)
    }
    # Only includes pareto optimal values
    Pareto=P[ga[[i]]$pareto.optimal,]
    # sort Pareto according to x axis:
    # Trying to remove plotter bug with pareto length checker - seems to work but might block out certain results
    if (length(Pareto) > 2){
      I=sort.int(Pareto[,1],index.return=TRUE)
      Pareto=Pareto[I$ix,]
    }
    lines(Pareto,type="l",cex=0.5,col=COL)
    points(P,type="p",pch=1,cex=0.5,col=COL)
  }
  
}

# --------------------------------------