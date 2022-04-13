library("mco")

run_NSGA = function(obj = 2, dim = 11, pop = 20, gen = 100){
  return(
    nsga2(
      constraints=budget_constraint, 
      cdim=1:dim, 
      fn=eval,
      idim=dim,
      odim=obj,
      lower.bounds=rep(1,dim), # Might want to make a minimum investment percentage or something?
      upper.bounds=rep(100,dim),
      popsize=pop,
      generations=1:gen)
    )
}

print_best = function(ga, generation) {
  # show best individuals:
  I=which(ga[[generation]]$pareto.optimal) 
  for(i in I)
  {
    x=round(ga[[generation]]$par[i,])
    cat(x," f=(",price_per_earnings(df, x),",",value_at_risk(df, x),")","\n",sep=" ")
  }
}

# --------------------------------------
# PLOTTING 
plotNSGA <- function(ga, gen = 100, xlab="F1", ylab="F2"){
  I=1:gen
  for(i in I)
  { 
    P = ga[[i]]$value # objectives f1 and f2
    P[,1] = -1 * P[,1] # show positive f1 values
    # color from light gray (75) to dark (1):
    COL=paste("gray",round(76-i*0.75),sep="")
    if(i==1) {
      plot(P,xlab=xlab,ylab=ylab,cex=0.5,col=COL)
    }
    # Only includes pareto optimal values
    Pareto=P[ga[[i]]$pareto.optimal,]
    # sort Pareto according to x axis:
    # Tweaked this to only sort if there is more than 1 Pareto element
    if (length(Pareto) > 2) {
      I=sort.int(Pareto[,1],index.return=TRUE)
      Pareto=Pareto[I$ix,]
    }
    
    points(P,type="p",pch=1,cex=0.5,col=COL)
    lines(Pareto,type="l",cex=0.5,col=COL)
  }
  
}

# --------------------------------------