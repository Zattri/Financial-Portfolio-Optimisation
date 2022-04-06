library("mco")

run_NSGA = function(obj = 2, dim = 11, pop = 20, gen = 100){
  return(
    nsga2(
      constraints=budget_constraint, 
      cdim=1:dim, 
      fn=eval,
      idim=D,
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
    cat(x," f=(",sum_price_per_earnings(df, x),",",sum_value_at_risk(df, x),")","\n",sep=" ")
  }
}

# --------------------------------------
# PLOTTING 
plotNSGA <- function(G){
  I=1:100
  for(i in I)
  { 
    P=G[[i]]$value # objectives f1 and f2
    P[,1]=-1*P[,1] # show positive f1 values
    # color from light gray (75) to dark (1):
    COL=paste("gray",round(76-i*0.75),sep="")
    if(i==1) 
      plot(P,xlim=c(-500,44000),ylim=c(0,140),xlab="Profit",ylab="Production",cex=0.5,col=COL)
    Pareto=P[G[[i]]$pareto.optimal,]
    # sort Pareto according to x axis:
    I=sort.int(Pareto[,1],index.return=TRUE)
    Pareto=Pareto[I$ix,]
    points(P,type="p",pch=1,cex=0.5,col=COL)
    lines(Pareto,type="l",cex=0.5,col=COL)
  }
  
}

# --------------------------------------